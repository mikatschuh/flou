#[test]
fn test() {
    use crate::{
        comp,
        error::Errors,
        parser::{
            binary_op::BinaryOp, intern::Internalizer, tokenizing::Tokenizer, unary_op::UnaryOp,
            Parser,
        },
        tree::{HeapNode, NodeWrapper},
        utilities::Rc,
    };
    use std::path::Path;

    macro_rules! node {
        // Statements
        [$inter:expr, (stmts, $($arg:tt), *)] => {
            Box::new(HeapNode::Statements(comp::Vec::new([$(node![$inter, $arg]), *])))
        };

        // Conditionals
        [$inter:expr, (loop, $cond:tt, $body:tt)] => {
            Box::new(HeapNode::Conditional {
                condition: node![$inter, $cond],
                looping: true,
                then_body: node![$inter, $body],
                else_body: None,
            })
        };
        // Binary Ops
        [$inter:expr, ($lhs:tt, add, $rhs:tt)] => {
            Box::new(HeapNode::Binary {
                op: BinaryOp::Add,
                left: node![$inter, $lhs],
                right: node![$inter, $rhs],
            })
        };

        [$inter:expr, ($lhs:tt, mul, $rhs:tt)] => {
            Box::new(HeapNode::Binary {
                op: BinaryOp::Mul,
                left: node![$inter, $lhs],
                right: node![$inter, $rhs],
            })
        };

        [$inter:expr, ($lhs:tt, equals, $rhs:tt)] => {
            Box::new(HeapNode::Binary {
                op: BinaryOp::Equation,
                left: node![$inter, $lhs],
                right: node![$inter, $rhs],
            })
        };

        [$inter:expr, ($lhs:tt, dot, $rhs:tt)] => {
            Box::new(HeapNode::Binary {
                op: BinaryOp::Dot,
                left: node![$inter, $lhs],
                right: node![$inter, $rhs],
            })
        };

        [$inter:expr, ($lhs:tt, $op:tt, $rhs:tt)] => {
            Box::new(HeapNode::Binary {
                op: BinaryOp::$op,
                left: node![$inter, $lhs],
                right: node![$inter, $rhs],
            })
        };

        // Unary Ops
        [$inter:expr, (neg, $lhs:tt)] => {
            Box::new(HeapNode::Unary {
                op: UnaryOp::Neg,
                operand: node![$inter, $lhs]
            })
        };

        [$inter:expr, (inc, $lhs:tt)] => {
            Box::new(HeapNode::Unary {
                op: UnaryOp::Increment,
                operand: node![$inter, $lhs]
            })
        };

        [$inter:expr, ($op:tt, $lhs:tt)] => {
            Box::new(HeapNode::Unary {
                op: UnaryOp::$op,
                operand: node![$inter, $lhs]
            })
        };

        // Ident
        [$inter:expr, $name:expr] => {
            Box::new(HeapNode::Ident($inter.get($name)))
        };

        // Placeholder
        [$inter:expr, ..] => {
            Box::new(HeapNode::PlaceHolder)
        };


    }

    let errors = Rc::new(Errors::empty(Path::new("example.flou")));
    let mut internalizer = Rc::new(Internalizer::new());

    macro_rules! parse {
        ($text:expr) => {{
            let (root, tree) = Parser::<NodeWrapper>::new(
                Tokenizer::new($text, errors.clone()),
                internalizer.clone(),
                errors.clone(),
            )
            .parse();
            tree.clone().build_graph(root)
        }};
    }
    assert_eq!(
        parse!("a + b * c"),
        node![internalizer, ("a", add, ("b", mul, "c"))],
    );

    assert_eq!(
        parse!("a + b * c++"),
        node![internalizer, (inc, ("a", add, ("b", mul, "c")))],
    );

    assert_eq!(
        parse!("a + b * -c * d"),
        node![internalizer, ("a", add, (("b", mul, (neg, "c")), mul, "d"))],
    );

    assert_eq!(
        parse!("a = b * -c · d"),
        node![
            internalizer,
            ("a", equals, ("b", mul, (neg, ("c", dot, "d"))))
        ],
    );

    assert_eq!(
        parse!(
            "a := 1 \
            b := 0
            "
        ),
        node!(internalizer, (stmts, ("a", Write, "1"), ("b", Write, "0")))
    );
}
