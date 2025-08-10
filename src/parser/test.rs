#[test]
fn test() {
    use crate::{
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

        // Ident
        [$inter:expr, $name:expr] => {
            Box::new(HeapNode::Ident($inter.get($name)))
        };
    }

    let errors = Rc::new(Errors::empty(Path::new("example.flou")));
    let mut internalizer = Rc::new(Internalizer::new());

    macro_rules! parse {
        ($text:expr) => {{
            let mut parser = Parser::<NodeWrapper>::new(
                Tokenizer::new($text, errors.clone()),
                internalizer.clone(),
                errors.clone(),
            );
            let root = parser.parse_expr(0).unwrap();
            parser.tree().build_graph(root)
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
}
