use crate::{parser::pratt_parser::PrattParser, tree::Node};

use std::fmt;
impl fmt::Display for PrattParser {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut string = String::new();
        let mut all_levels = vec![&self.0]; // collect all the levels
        while let Some(ref higher) = all_levels.last().unwrap().higher {
            all_levels.push(&higher.0);
        }
        all_levels = all_levels.into_iter().rev().collect(); // all the levels from uppest to lowest

        let mut pointer_height = None;
        for (i, level) in all_levels.iter().enumerate() {
            if level.output.len() == 0 {
                (0..pointer_height.unwrap_or(1) - 1).for_each(|_| string += " ");
                string += "[ ]";
                break;
            } else {
                let (mut starting_at, mut next_pointer_height, look) = level
                    .output
                    .last()
                    .unwrap()
                    .as_string_without_indentation(i == all_levels.len() - 1);

                next_pointer_height += 1;
                starting_at += 1; // the "[" has to be accounted for
                if i != 0 {
                    (0..pointer_height.unwrap_or(2) - starting_at).for_each(|_| {
                        string += " ";
                        next_pointer_height += 1;
                    })
                }
                string += "[";
                if level.output.len() > 1 {
                    string += ".., "; // represents previous nodes
                    next_pointer_height += 4;
                }
                string += &look;
                string += "]";
                if i < all_levels.len() - 1 {
                    let mut content_height = String::new();
                    (0..next_pointer_height).for_each(|_| content_height += " ");
                    string = string + "\n" + &content_height + "|\n";
                    pointer_height = Some(next_pointer_height)
                }
            }
        }
        write!(f, "{}", string)
    }
}
impl Node {
    fn as_string_without_indentation(&self, last: bool) -> (usize, usize, String) {
        match self {
            Self::None => unreachable!("the variant [None] should never show up here"),
            Self::Quote(quote) => (1, 1, format!(" \"{}\" ", quote.clone())),
            Self::Id { name, .. } => (1, 1, format!(" {} ", name.clone())),
            Self::Num(num) => (1, 1, format!(" {} ", num)),
            Self::UnaryOp { kind, operand } => (
                1,
                6,
                format!(
                    " {} {{{}}}",
                    kind.to_string(),
                    if last {
                        operand.as_string_without_indentation(true).2
                    } else {
                        String::from(" • ")
                    }
                ),
            ),
            Self::Com(content) => (
                0,
                2,
                if last {
                    format!("({})", handle_multiple_nodes(content))
                } else {
                    String::from("( • )")
                },
            ),
            Self::Sum(content) => (
                0,
                2,
                if last {
                    format!("[{}]", handle_multiple_nodes(content))
                } else {
                    String::from("[ • ]")
                },
            ),
            Self::List(content) => (
                0,
                6,
                match content.last() {
                    Some(content) => format!(
                        "<[.., {}]>",
                        if last {
                            content.as_string_without_indentation(true).2
                        } else {
                            String::from("• ")
                        }
                    ),

                    None => String::from("<[]>"),
                },
            ),
            Self::ChainedOp { first, additions } => match last {
                true => {
                    let mut string = format!("{{{}", first.as_string_without_indentation(true).2);
                    for addition in additions.iter() {
                        string += &format!(
                            " {} {}",
                            addition.0,
                            addition.1.as_string_without_indentation(true).2
                        )
                    }
                    (0, 0, string + "}")
                }
                false => {
                    let mut string = String::from("{..");
                    for (i, addition) in additions.iter().enumerate() {
                        string += &if i == additions.len() - 1 {
                            format!(" {} • ", addition.0)
                        } else {
                            format!(" {} ..", addition.0)
                        }
                    }
                    (0, string.len() - 4, string + "}")
                }
            },
            Self::BinaryOp { kind, right, .. } => (
                1,
                8,
                format!(
                    " {} {{.., {}}}",
                    kind.to_string(),
                    if last {
                        right.as_string_without_indentation(true).2
                    } else {
                        String::from("• ")
                    }
                ),
            ),
            Self::Call { called, input } => (
                1,
                5,
                format!(
                    "{}({})",
                    called.as_string_without_indentation(true).2,
                    if last {
                        handle_multiple_nodes(input)
                    } else {
                        String::from(" • ")
                    }
                ),
            ),
            Self::Index { indexed, index } => (
                1,
                5,
                format!(
                    "{}[{}]",
                    indexed.as_string_without_indentation(true).2,
                    if last {
                        handle_multiple_nodes(index)
                    } else {
                        String::from(" • ")
                    }
                ),
            ),
            Self::Conditional { .. } => todo!(),
        }
    }
}
fn handle_multiple_nodes(list: &Vec<Node>) -> String {
    if list.len() == 0 {
        String::from(" ")
    } else if list.len() == 1 {
        format!(
            " {} ",
            list.first().unwrap().as_string_without_indentation(false).2
        )
    } else {
        let mut string = String::from(" ");
        for node in list.iter().rev().skip(1).rev() {
            string += &node.as_string_without_indentation(false).2;
            string += ", "
        }
        string += &list.last().unwrap().as_string_without_indentation(false).2;
        string + " "
    }
}
fn handle_multiple_nodes_in_brackets(list: &Vec<Node>) -> String {
    if list.len() == 0 {
        String::from("()")
    } else if list.len() == 1 {
        format!(
            " {} ",
            list.last().unwrap().as_string_without_indentation(false).2
        )
    } else {
        let mut string = String::from("(");
        for node in list.iter().rev().skip(1).rev() {
            string += &node.as_string_without_indentation(false).2;
            string += ", "
        }
        string += &list.last().unwrap().as_string_without_indentation(false).2;
        string + ")"
    }
}
