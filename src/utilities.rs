use std::alloc::{alloc, dealloc, Layout};
use std::ops::{Deref, DerefMut};
use std::ptr::NonNull;
use std::sync::atomic::{AtomicUsize, Ordering};

// Die innere Struktur, die gezählt wird
struct RcBoxInner<T> {
    // Anzahl der starken Referenzen
    strong: AtomicUsize,
    // Der eigentliche Wert
    value: T,
}

// Trait für eigene Allocator-Implementierungen
pub trait CustomAllocator {
    unsafe fn allocate(&self, layout: Layout) -> *mut u8;
    unsafe fn deallocate(&self, ptr: *mut u8, layout: Layout);
}

// Implementierung des Standard-Allocators
#[derive(Clone, Copy)]
pub struct GlobalAllocator;

impl CustomAllocator for GlobalAllocator {
    unsafe fn allocate(&self, layout: Layout) -> *mut u8 {
        alloc(layout)
    }

    unsafe fn deallocate(&self, ptr: *mut u8, layout: Layout) {
        dealloc(ptr, layout)
    }
}

// Unser öffentlicher Rc-Typ
pub struct Rc<T, A: CustomAllocator = GlobalAllocator> {
    // Pointer zur inneren Struktur
    ptr: NonNull<RcBoxInner<T>>,
    // Speichere den Allocator direkt
    allocator: A,
}
// Implementierung mit GlobalAllocator als Standard
impl<T> Rc<T, GlobalAllocator> {
    // Standardkonstruktor
    pub fn new(value: T) -> Self {
        Self::new_in(value, GlobalAllocator)
    }
}

// Implementierung für alle Allocator-Typen
impl<T, A: CustomAllocator> Rc<T, A> {
    // Konstruktor mit spezifischem Allocator
    pub fn new_in(value: T, allocator: A) -> Self {
        // Layout für den inneren Typ berechnen
        let layout = Layout::new::<RcBoxInner<T>>();

        // Speicher allozieren
        let ptr = unsafe {
            let mem = allocator.allocate(layout);
            if mem.is_null() {
                std::alloc::handle_alloc_error(layout);
            }

            let ptr = mem as *mut RcBoxInner<T>;

            // Innere Struktur im allozierten Speicher initialisieren
            ptr.write(RcBoxInner {
                strong: AtomicUsize::new(1),
                value,
            });

            NonNull::new_unchecked(ptr)
        };

        Self { ptr, allocator }
    }
    // Aktuelle Anzahl der starken Referenzen
    pub fn strong_count(&self) -> usize {
        unsafe { (*self.ptr.as_ptr()).strong.load(Ordering::SeqCst) }
    }
}

// Eine einzige Clone-Implementierung für MyRc mit Copy-Trait-Bound
impl<T, A: CustomAllocator + Copy> Clone for Rc<T, A> {
    fn clone(&self) -> Self {
        unsafe {
            // Erhöhe den Referenzzähler
            (*self.ptr.as_ptr()).strong.fetch_add(1, Ordering::SeqCst);
        }

        Self {
            ptr: self.ptr,
            allocator: self.allocator,
        }
    }
}

// Implementierung von Deref für einfachen Zugriff auf den Wert
impl<T, A: CustomAllocator> Deref for Rc<T, A> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { &(*self.ptr.as_ptr()).value }
    }
}
impl<T, A: CustomAllocator> DerefMut for Rc<T, A> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut (*self.ptr.as_ptr()).value }
    }
}

// Implementierung von Drop für die Speicherfreigabe
impl<T, A: CustomAllocator> Drop for Rc<T, A> {
    fn drop(&mut self) {
        unsafe {
            // Referenzzähler verringern
            let strong = (*self.ptr.as_ptr()).strong.fetch_sub(1, Ordering::SeqCst);

            // Wenn dies die letzte Referenz war, gebe den Speicher frei
            if strong == 1 {
                // Layout berechnen (muss dem Allokationslayout entsprechen)
                let layout = Layout::new::<RcBoxInner<T>>();

                // Manuell den Destruktor für den inneren Wert aufrufen
                std::ptr::drop_in_place(&mut (*self.ptr.as_ptr()).value);

                // Speicher freigeben mit dem gespeicherten Allocator
                self.allocator
                    .deallocate(self.ptr.as_ptr() as *mut u8, layout);
            }
        }
    }
}
impl<T: PartialEq, A: CustomAllocator> PartialEq for Rc<T, A> {
    fn eq(&self, other: &Self) -> bool {
        unsafe { self.ptr.as_ref().value == other.ptr.as_ref().value }
    }
}
use std::fmt;
impl<T: fmt::Debug, A: CustomAllocator> fmt::Debug for Rc<T, A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", unsafe { &self.ptr.as_ref().value })
    }
}

#[macro_export]
macro_rules! pop_as_long_as {
    ($arr:expr => $pattern:pat => $code:expr) => {
        while let Some($pattern) = $arr.last() {
            if let $pattern = $arr.pop().unwrap() {
                $code
            }
        }
    };
}
