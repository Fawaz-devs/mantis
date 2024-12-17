use std::ops::Deref;

use mantis_tokens::MantisLexerTokens;
use node::{BinaryOperation, Node};

pub mod node;

/*

Example expressions

let a = 10;
let b = (a + 10) * 3;
let c = add(a, b);
let f = malloc(6);
f.a.b();
f.a.c = f.a.c + 1;

*/

#[test]
fn test_ast_parsing() {
    use logos::Logos;
    let s = "(a + b) * c = 1 + (t.d as int)";
    // let s = "(a + b)";
    let tokens = MantisLexerTokens::lexer(s)
        .collect::<Result<Vec<_>, _>>()
        .unwrap();

    let node = parse(&tokens).unwrap();

    dbg!(node);
    panic!();
}

impl Node {
    pub fn parse_expression(tokens: &[MantisLexerTokens]) -> anyhow::Result<Self> {
        let (node, _) = parse(tokens)?;
        Ok(node)
    }
}

fn parse(tokens: &[MantisLexerTokens]) -> anyhow::Result<(Node, usize)> {
    let mut current_node = Node::None;
    let mut i = 0;
    loop {
        if i >= tokens.len() {
            break;
        }

        let c = &tokens[i];

        match c {
            MantisLexerTokens::Add
            | MantisLexerTokens::Sub
            | MantisLexerTokens::Multiply
            | MantisLexerTokens::Divide
            | MantisLexerTokens::Assign
            | MantisLexerTokens::As
            | MantisLexerTokens::Dot
            | MantisLexerTokens::GreaterThan
            | MantisLexerTokens::GreaterThanOrEqualTo
            | MantisLexerTokens::EqualTo
            | MantisLexerTokens::NotEqualTo
            | MantisLexerTokens::LessThan
            | MantisLexerTokens::LessThanOrEqualTo

             => {
                let operation = BinaryOperation::try_from(c)?;

                match current_node {
                    Node::Binary(_, _, _) => {
                        todo!()
                    }
                    Node::Var(ref lhs) => {
                        let (rhs, consumed) = parse(&tokens[i + 1..])?;
                        i += consumed + 1;

                        match rhs {
                            Node::Binary(rhs_op, rhs_lhs, rhs_rhs) => {
                                if operation == BinaryOperation::Access
                                    && rhs_op == BinaryOperation::Access
                                {
                                    let mut new_rhs = Node::Binary(rhs_op, rhs_lhs, rhs_rhs);
                                    add_node_to_left_most_child(
                                        &mut new_rhs,
                                        current_node,
                                        operation,
                                    );
                                    current_node = new_rhs;
                                } else {
                                    let prioriy = get_priority(operation, rhs_op);
                                    if prioriy > 0 {
                                        let mut new_rhs = Node::Binary(rhs_op, rhs_lhs, rhs_rhs);
                                        add_node_to_left_most_child(
                                            &mut new_rhs,
                                            current_node,
                                            operation,
                                        );
                                        current_node = new_rhs;
                                    } else {
                                        current_node = Node::Binary(
                                            operation,
                                            Box::new(Node::Var(lhs.clone())),
                                            Box::new(Node::Binary(rhs_op, rhs_lhs, rhs_rhs)),
                                        );
                                    }
                                }

                                return Ok((current_node, i));
                            }
                            _ => {}
                        };

                        current_node =
                            Node::Binary(operation, Box::new(Node::Var(lhs.clone())), Box::new(rhs));

                        return Ok((current_node, i));
                    }
                    Node::None => return Err(anyhow::anyhow!("No LHS")),
                    Node::Expr(lhs) => {
                        let (rhs, consumed) = parse(&tokens[i + 1..])?;
                        i += consumed + 1;

                        match rhs {
                            Node::Binary(rhs_op, rhs_lhs, rhs_rhs) => {
                                if operation == BinaryOperation::Access
                                    && rhs_op == BinaryOperation::Access
                                {
                                    let mut new_rhs = Node::Binary(rhs_op, rhs_lhs, rhs_rhs);
                                    add_node_to_left_most_child(
                                        &mut new_rhs,
                                        Node::Expr(lhs),
                                        operation,
                                    );
                                    current_node = new_rhs;
                                } else {
                                    let prioriy = get_priority(operation, rhs_op);
                                    if prioriy > 0 {
                                        let mut new_rhs = Node::Binary(rhs_op, rhs_lhs, rhs_rhs);
                                        add_node_to_left_most_child(
                                            &mut new_rhs,
                                            Node::Expr(lhs),
                                            operation,
                                        );
                                        current_node = new_rhs;
                                    } else {
                                        current_node = Node::Binary(
                                            operation,
                                            Box::new(Node::Expr(lhs)),
                                            Box::new(Node::Binary(rhs_op, rhs_lhs, rhs_rhs)),
                                        );
                                    }
                                }

                                return Ok((current_node, i));
                            }
                            _ => {}
                        };

                        current_node =
                            Node::Binary(operation, Box::new(Node::Expr(lhs)), Box::new(rhs));
                        return Ok((current_node, i));
                    }
                    Node::Tuple(_) => todo!(),
                };
            }

            MantisLexerTokens::BracketOpen => match current_node {
                Node::Binary(op, lhs, rhs) => match rhs.deref() {
                    Node::None => {
                        let (node, consumed) = parse(&tokens[i + 1..])?;
                        i += consumed + 1;
                        current_node = Node::Binary(op, lhs, Box::new(node));
                    }
                    _ => return Err(anyhow::anyhow!("invalid character after operation")),
                },
                Node::None => {
                    let (node, consumed) = parse(&tokens[i + 1..])?;
                    i += consumed;

                    current_node = Node::Expr(Box::new(node));
                }
                Node::Tuple(_) => todo!(),
                Node::Expr(_) => todo!(),
                Node::Var(lhs) => {
                    let (args, consumed) = collect_call_args(&tokens[i + 1..])?;

                    i += consumed;
                    current_node = Node::Binary(
                        BinaryOperation::Call,
                        Box::new(Node::Var(lhs)),
                        Box::new(Node::Tuple(args)),
                    );
                    current_node = Node::Expr(Box::new(current_node));
                }
            },

            MantisLexerTokens::Comma => {
                return Ok((current_node, i + 1));
            }

            MantisLexerTokens::BracketClose => {
                return Ok((current_node, i + 1));
            }

            _ => match current_node {
                Node::Binary(op, lhs, rhs) => match rhs.deref() {
                    Node::None => {
                        current_node = Node::Binary(op, lhs, Box::new(Node::Var(c.clone())));
                    }
                    _ => panic!("invalid character after operation"),
                },
                Node::Var(_) => panic!("variable after variable current_node {:?}, current_token {:?}", current_node, c),
                Node::None => current_node = Node::Var(c.clone()),
                Node::Expr(_) => {
                   return Err(anyhow::anyhow!("Node::EXpr Not implemented, the current token {:?} and index {i} of {:?} = {:?}, current_node: {:?}", c, tokens, tokens[i], current_node))
                }
                Node::Tuple(_) => todo!(),
            },
        }
        i += 1;
    }

    Ok((current_node, i))
}

fn collect_call_args(tokens: &[MantisLexerTokens]) -> anyhow::Result<(Vec<Node>, usize)> {
    let mut args = Vec::new();
    let mut i = 0;
    loop {
        let (arg, consumed) = parse(&tokens[i..])?;
        match arg {
            Node::None => break,
            _ => args.push(arg),
        }

        i += consumed;
        if tokens[i - 1] == MantisLexerTokens::BracketClose {
            break;
        }
    }

    Ok((args, i))
}

fn add_node_to_left_most_child(parent: &mut Node, lhs: Node, operator: BinaryOperation) {
    match parent {
        Node::Binary(_, plhs, _) => {
            add_node_to_left_most_child(plhs, lhs, operator);
        }
        _ => {
            match parent {
                Node::None => {
                    *parent = lhs;
                }

                Node::Var(_) | Node::Expr(_) => {
                    *parent = Node::Binary(operator, Box::new(lhs), Box::new(parent.clone()));
                    // can we avoid this clone?
                }

                _ => {}
            };
        }
    };
}

fn get_priority(a: BinaryOperation, b: BinaryOperation) -> isize {
    use BinaryOperation::*;

    let priority_order = [
        Call,
        Access,
        Div,
        Mult,
        Sub,
        Add,
        GreaterThan,
        GreaterThanOrEqualTo,
        EqualTo,
        NotEqualTo,
        LessThan,
        LessThanOrEqualTo,
        Cast,
        Assign,
    ];
    let mut ad = -1;
    let mut bd = -1;
    for (i, &c) in priority_order.iter().enumerate() {
        if a == c {
            ad = i as isize;
        }
        if b == c {
            bd = i as isize;
        }
    }

    bd - ad
}
