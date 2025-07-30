use std::{
    cmp::Ordering,
    fmt::Display,
    ops::{Add, Index, Mul, Neg, Sub},
};
#[derive(Clone, PartialEq, PartialOrd, Eq, Default, Debug)]
pub struct Number {
    pub sign: Sign,
    pub buffer: Vec<usize>, // least significant first
    pub surrounding_material: bool,
}
#[derive(Clone, PartialEq, PartialOrd, Eq, Copy, Default, Debug)]
pub enum Sign {
    Neg,
    #[default]
    Pos,
}
impl Neg for Sign {
    type Output = Sign;
    fn neg(self) -> Self::Output {
        match self {
            Sign::Neg => Sign::Pos,
            Sign::Pos => Sign::Neg,
        }
    }
}
use Sign::*;

use crate::parser::tokenizing::binary_op::BinaryOp;
#[macro_export]
macro_rules! num {
    () => {{
        Number { buffer: vec![] }
    }};
    ($num:literal) => {{
        // Konvertiere das Literal in einen String zur Kompilierzeit
        let num_str = stringify!($num);
        // Parse den String zur Laufzeit in dein Number-Struct
        crate::parser::tokenizing::num::parse_to_num(num_str, &crate::error::Position::default())
            .expect("not a valid number")
            .expect("not a number")
    }};
}
impl From<usize> for Number {
    fn from(value: usize) -> Self {
        Number {
            sign: Sign::Pos,
            buffer: vec![value],
            surrounding_material: false,
        }
    }
}
impl From<i32> for Number {
    fn from(value: i32) -> Self {
        match value < 0 {
            true => Number {
                sign: Sign::Neg,
                buffer: vec![-value as usize],
                surrounding_material: false,
            },
            false => Number {
                sign: Sign::Pos,
                buffer: vec![value as usize],
                surrounding_material: false,
            },
        }
    }
}
impl Index<usize> for Number {
    type Output = usize;
    /// if the index is out of range 0 is returned
    fn index(&self, index: usize) -> &Self::Output {
        if index >= self.buffer.len() {
            if self.surrounding_material {
                &1
            } else {
                &0
            }
        } else {
            &self.buffer[index]
        }
    }
}
impl Ord for Number {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.sign {
            Neg => match other.sign {
                Neg => self.cmp_abs(other).reverse(),
                Pos => match self.is_zero() {
                    true => self.cmp_abs(other),
                    false => Ordering::Less,
                },
            },
            Pos => match other.sign {
                Neg => match other.is_zero() {
                    true => self.cmp_abs(other),
                    false => Ordering::Greater,
                },
                Pos => self.cmp_abs(other),
            },
        }
    }
}
impl Add<Self> for &Number {
    type Output = Number;
    fn add(self, rhs: Self) -> Self::Output {
        match self.sign {
            Neg => match rhs.sign {
                Neg => -&self.raw_add(rhs), // -a + -b
                Pos => rhs - self,          // -a + b
            },
            Pos => match rhs.sign {
                Neg => self - rhs,        // a + -b
                Pos => rhs.raw_add(self), // a + b
            },
        }
    }
}
impl Add<Self> for Number {
    type Output = Number;
    fn add(self, rhs: Self) -> Self::Output {
        match self.sign {
            Neg => match rhs.sign {
                Neg => -&self.raw_add(&rhs), // -a + -b
                Pos => rhs - self,           // -a + b
            },
            Pos => match rhs.sign {
                Neg => self - rhs,         // a + -b
                Pos => rhs.raw_add(&self), // a + b
            },
        }
    }
}
impl Sub<Self> for &Number {
    type Output = Number;
    fn sub(self, rhs: Self) -> Self::Output {
        match self.sign {
            Neg => match rhs.sign {
                Neg => match self.cmp_abs(rhs) {
                    Ordering::Less => rhs.raw_sub(self),
                    Ordering::Greater => -self.raw_sub(rhs),
                    Ordering::Equal => Number::default(),
                }, // -a - -b
                Pos => -&self.raw_add(rhs), // -a - b
            },
            Pos => match rhs.sign {
                Neg => self.raw_add(rhs), // a - -b
                Pos => match self.cmp_abs(rhs) {
                    Ordering::Less => -rhs.raw_sub(self),
                    Ordering::Greater => self.raw_sub(rhs),
                    Ordering::Equal => Number::default(),
                }, // a - b
            },
        }
    }
}
impl Sub<Self> for Number {
    type Output = Number;
    fn sub(self, rhs: Self) -> Self::Output {
        match self.sign {
            Neg => match rhs.sign {
                Neg => match self.cmp_abs(&rhs) {
                    Ordering::Less => rhs.raw_sub(&self),
                    Ordering::Greater => -self.raw_sub(&rhs),
                    Ordering::Equal => Number::default(),
                }, // -a - -b
                Pos => -&self.raw_add(&rhs), // -a - b
            },
            Pos => match rhs.sign {
                Neg => self.raw_add(&rhs), // a - -b
                Pos => match self.cmp_abs(&rhs) {
                    Ordering::Less => -rhs.raw_sub(&self),
                    Ordering::Greater => self.raw_sub(&rhs),
                    Ordering::Equal => Number::default(),
                }, // a - b
            },
        }
    }
}
impl Mul<Self> for &Number {
    type Output = Number;
    fn mul(self, rhs: Self) -> Self::Output {
        match self.sign == rhs.sign {
            true => self.raw_mul(rhs),
            false => -self.raw_mul(rhs),
        }
    }
}
impl Mul<Self> for Number {
    type Output = Number;
    fn mul(self, rhs: Self) -> Self::Output {
        match self.sign == rhs.sign {
            true => self.raw_mul(&rhs),
            false => -self.raw_mul(&rhs),
        }
    }
}
impl Neg for &Number {
    type Output = Number;
    fn neg(self) -> Self::Output {
        Number {
            sign: -self.sign,
            buffer: self.buffer.clone(),
            surrounding_material: false,
        }
    }
}
impl Neg for Number {
    type Output = Number;
    fn neg(self) -> Self::Output {
        Number {
            sign: -self.sign,
            buffer: self.buffer,
            surrounding_material: false,
        }
    }
}
impl Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{}]",
            self.buffer
                .iter()
                .enumerate()
                .map(|(i, chunk)| (*chunk as u128) << size_of::<usize>() * 8 * i)
                .fold(0_u128, |acc, val| acc + val)
        )
    }
}
impl Number {
    #[inline]
    fn is_zero(&self) -> bool {
        self.buffer.iter().all(|chunk| *chunk == 0)
    }
    fn cmp_abs(&self, other: &Self) -> Ordering {
        for i in self.buffer.len().max(other.buffer.len()) - 1..=0 {
            let ordering = self[i].cmp(&other[i]);
            if let Ordering::Greater | Ordering::Less = ordering {
                return ordering;
            }
        }
        return Ordering::Equal;
    }
    fn raw_sub(&self, rhs: &Number) -> Number {
        let mut carry_in = 0;
        let mut result = Vec::new();

        for i in 0..=self.buffer.len().max(rhs.buffer.len()) {
            let subtractor = rhs[i] + carry_in;
            if self[i] < subtractor {
                result.push(self[i].overflowing_sub(subtractor).0);
                carry_in = 1
            } else {
                result.push(self[i] - subtractor)
            }
        }
        // cleanup
        result.clone().iter().rev().for_each(|chunk| {
            if *chunk == 0 {
                result.pop();
            }
        });
        Number {
            sign: Pos,
            buffer: result,
            surrounding_material: false,
        }
    }
    fn raw_add(&self, rhs: &Number) -> Number {
        let (mut longer, shorter) = match self.buffer.len() > rhs.buffer.len() {
            true => (self.buffer.iter(), rhs.buffer.iter()),
            false => (rhs.buffer.iter(), self.buffer.iter()),
        }; // two iterators from the least significant to the most significant
        let mut carry_out = false;
        let mut result = Vec::new();
        for part in shorter {
            let result_and_carry_out =
                part.overflowing_add(*longer.next().unwrap() + carry_out as usize);
            result.push(result_and_carry_out.0);
            carry_out = result_and_carry_out.1;
        }
        if carry_out {
            for part in longer {
                let result_and_carry_out = part.overflowing_add(carry_out as usize);
                result.push(result_and_carry_out.0);
                carry_out = result_and_carry_out.1;
            }
            if carry_out {
                result.push(1)
            }
        } else {
            longer.for_each(|part| result.push(*part))
        }
        Number {
            sign: Pos,
            buffer: result,
            surrounding_material: false,
        }
    }
    fn raw_mul(&self, rhs: &Number) -> Number {
        let (mut longer, shorter) = match self.buffer.len() > rhs.buffer.len() {
            true => (self.buffer.iter(), rhs.buffer.iter()),
            false => (rhs.buffer.iter(), self.buffer.iter()),
        }; // two iterators from the least significant to the most significant
        let mut carry_out = 0;
        let mut result = Vec::new();
        for part in shorter {
            let result_and_carry_out =
                *part as u128 * (*longer.next().unwrap() as u128 + carry_out as u128);
            result.push((result_and_carry_out & (u128::MAX >> size_of::<usize>() * 8)) as usize);
            carry_out = (result_and_carry_out & (u128::MAX << size_of::<usize>() * 8)) as usize;
        }
        if carry_out != 0 {
            for part in longer {
                let result_and_carry_out = *part as u128 * (*part as u128 + carry_out as u128);
                result
                    .push((result_and_carry_out & (u128::MAX >> size_of::<usize>() * 8)) as usize);
                carry_out = (result_and_carry_out & (u128::MAX << size_of::<usize>() * 8)) as usize;
            }
            if carry_out != 0 {
                result.push(carry_out)
            }
        } else {
            longer.for_each(|part| result.push(*part))
        }
        Number {
            sign: Pos,
            buffer: result,
            surrounding_material: false,
        }
    }
    pub fn apply(&self, op: BinaryOp, rhs: &Self) -> Number {
        match op {
            BinaryOp::Add => self + rhs,
            BinaryOp::Mul => self * rhs,
            BinaryOp::Sub => self - rhs,
            _ => todo!(),
        }
    }
}
