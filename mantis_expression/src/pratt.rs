use std::collections::{BTreeSet, HashMap};

use pest::{
    iterators::{Pair, Pairs},
    pratt_parser::{Assoc, Op, PrattParser},
    Parser,
};
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "./grammer.pest"]
pub struct ExpressionParser;

#[derive(Debug)]
pub enum Term {
    Ident(Box<str>),
    I64(i64),
    F64(f64),
}

#[derive(Debug)]
pub enum Node {
    Binary(Rule, Box<Node>, Box<Node>),
    Unary(Rule, Box<Node>),
    Expr(Box<Node>),
    FnCall(Box<Node>, Vec<Node>),
    Term(Term),
    None,
}

#[derive(Debug)]
pub enum Statement {
    Let(Box<str>, Node),
    Return(Node),
    Break(Box<str>),
    Continue(Box<str>),
    Expr(Node),
}

#[derive(Debug)]
pub enum Block {
    Scoped(Vec<Statement>),
    If(Node, Box<Block>),
    Elif(Node, Box<Block>),
    Else(Box<Block>),
    Loop(Box<Block>),
}

#[derive(Debug)]
pub enum Type {
    Struct { fields: HashMap<Box<str>, Type> },
    WithGenerics(Box<str>, Vec<Type>),
    Word(Box<str>),
}

pub struct TypeDecl {
    name: Type,
    ty: Type,
}

#[derive(Debug)]
pub enum Declaration {
    Function(Box<str>, Block),
    Type(Type, Type),
    // Struct(Box<str>, HashMap<Box<str>, Type>),
}

fn get_pratt_parser() -> PrattParser<Rule> {
    let pratt = PrattParser::new()
        .op(Op::infix(Rule::assign, Assoc::Right))
        .op(Op::infix(Rule::add, Assoc::Left) | Op::infix(Rule::sub, Assoc::Left))
        .op(Op::infix(Rule::mul, Assoc::Left) | Op::infix(Rule::div, Assoc::Left))
        .op(Op::infix(Rule::pow, Assoc::Right))
        .op(Op::postfix(Rule::expr_call))
        .op(Op::prefix(Rule::neg))
        .op(Op::infix(Rule::cast, Assoc::Right))
        .op(Op::prefix(Rule::at))
        .op(Op::postfix(Rule::fac))
        .op(Op::infix(Rule::dot, Assoc::Left));

    pratt
}

fn pratt_parse(input: &str) -> anyhow::Result<Node> {
    let pairs = ExpressionParser::parse(Rule::expr, input).unwrap();
    let pratt = get_pratt_parser();

    let node = parse_expr(pairs, &pratt);
    Ok(node)
}

fn parse_expr(pairs: Pairs<Rule>, pratt: &PrattParser<Rule>) -> Node {
    pratt
        .map_primary(|primary| match primary.as_rule() {
            Rule::expr => parse_expr(primary.into_inner(), pratt),
            Rule::int => Node::Term(Term::I64(primary.as_str().parse::<i64>().unwrap())),
            Rule::float => Node::Term(Term::F64(primary.as_str().parse::<f64>().unwrap())),
            Rule::word => Node::Term(Term::Ident(primary.as_str().into())),
            _ => unreachable!(
                "Unhandled Rule {:?} {:?}",
                primary.as_rule(),
                primary.as_str()
            ),
        })
        .map_prefix(|op, rhs| match op.as_rule() {
            Rule::at | Rule::neg => Node::Unary(op.as_rule(), rhs.into()),
            _ => unreachable!(),
        })
        .map_infix(|lhs, op, rhs| match op.as_rule() {
            Rule::add
            | Rule::sub
            | Rule::mul
            | Rule::div
            | Rule::pow
            | Rule::dot
            | Rule::assign
            | Rule::cast => Node::Binary(op.as_rule(), lhs.into(), rhs.into()),
            _ => unreachable!(),
        })
        .map_postfix(|lhs, op| match op.as_rule() {
            Rule::fac => Node::Unary(Rule::fac, lhs.into()),
            Rule::expr_call => {
                let args = if let Some(pair) = op.into_inner().into_iter().next() {
                    parse_expr_list(pair, pratt)
                } else {
                    Vec::new()
                };
                Node::FnCall(lhs.into(), args)
            }
            _ => unreachable!(),
        })
        .parse(pairs)
}
fn parse_decls(pairs: Pairs<Rule>, pratt: &PrattParser<Rule>) -> Declaration {
    pratt
        .map_primary(|primary| match primary.as_rule() {
            Rule::declarations => {
                let decl = primary.into_inner();
                parse_decls(decl, pratt)
            }
            Rule::type_decl => {
                let mut decl = primary.into_inner().into_iter();
                let name = decl.next().unwrap();
                let name = parse_type(Pairs::single(name), pratt);
                let ty = decl.next().unwrap();
                let ty = parse_type(Pairs::single(ty), pratt);

                Declaration::Type(name, ty)

                // match next.as_rule() {
                //     Rule::struct_decl => {
                //         let map = parse_struct_declaration(Pairs::single(next), pratt);
                //         Declaration::Type(Type::Word(name.into()), Type::Struct { fields: map })
                //     }
                //     Rule::type_name => {
                //         let ty = parse_type(Pairs::single(next), pratt);
                //         Declaration::Type(Type::Word(name.into()), ty)
                //     }
                //     _ => unreachable!(),
                // }

                // let type_name = parse_type(Pairs::single(next), pratt);
            }
            _ => unreachable!(
                "Unhandled Rule {:?} {:?}",
                primary.as_rule(),
                primary.as_str()
            ),
        })
        .parse(pairs)
}

// fn parse_struct_declaration(
//     pairs: Pairs<Rule>,
//     pratt: &PrattParser<Rule>,
// ) -> HashMap<Box<str>, Type> {
//     pratt
//         .map_primary(|primary| match primary.as_rule() {
//             Rule::struct_decl => {
//                 let mut iter = primary.into_inner().into_iter();
//                 let next = iter.next().unwrap();

//                 if !matches!(next.as_rule(), Rule::typed_args_list) {
//                     panic!("got {:?} instead of typed args list", next);
//                 }

//                 // if matches!(next.as_rule(), Rule::type_list) {
//                 //     // generics
//                 //     let mut generics = Vec::new();
//                 // }

//                 // let next = iter.next().unwrap();
//                 assert_eq!(next.as_rule(), Rule::typed_args_list);
//                 let typed_args_list = next;
//                 let mut map = HashMap::<Box<str>, Type>::new();
//                 for pair in typed_args_list.into_inner() {
//                     let mut typed_arg = pair.into_inner().into_iter();
//                     let arg_name = typed_arg.next().unwrap();
//                     let arg_type = typed_arg.next().unwrap();
//                     map.insert(
//                         arg_name.as_str().into(),
//                         parse_type(arg_type.into_inner(), pratt),
//                     );
//                 }

//                 map
//             }
//             _ => unreachable!(
//                 "Unhandled Rule {:?} {:?}",
//                 primary.as_rule(),
//                 primary.as_str()
//             ),
//         })
//         .parse(pairs)
// }

fn parse_type(pairs: Pairs<Rule>, pratt: &PrattParser<Rule>) -> Type {
    pratt
        .map_primary(|primary| match primary.as_rule() {
            Rule::type_name => {
                let mut iter = primary.into_inner().into_iter();
                let word = iter.next().unwrap();
                if let Some(type_list) = iter.next() {
                    let mut generics = Vec::new();
                    for ty in type_list.into_inner().into_iter() {
                        generics.push(parse_type(Pairs::single(ty), pratt));
                    }

                    return Type::WithGenerics(word.as_str().into(), generics);
                }
                parse_type(Pairs::single(word), pratt)
            }
            Rule::word => Type::Word(primary.as_str().into()),
            Rule::struct_decl => {
                let mut iter = primary.into_inner().into_iter();
                let typed_args_list = iter.next().unwrap();
                let mut map = HashMap::<Box<str>, Type>::new();
                for pair in typed_args_list.into_inner() {
                    let mut typed_arg = pair.into_inner().into_iter();
                    let arg_name = typed_arg.next().unwrap();
                    let arg_type = typed_arg.next().unwrap();
                    let arg_type = parse_type(Pairs::single(arg_type), pratt);
                    map.insert(arg_name.as_str().into(), arg_type);
                }

                Type::Struct { fields: map }
            }
            _ => unreachable!(
                "Unhandled Rule {:?} {:?}",
                primary.as_rule(),
                primary.as_str()
            ),
        })
        .parse(pairs)
}

fn parse_expr_list(pair: Pair<Rule>, pratt: &PrattParser<Rule>) -> Vec<Node> {
    // let expressions = expr_list.into_inner();
    let pairs = pair.into_inner();

    pairs
        .into_iter()
        .map(|x| parse_expr(Pairs::single(x), pratt))
        .collect::<Vec<_>>()
}

fn parse_block(input: &str) -> anyhow::Result<()> {
    let pratt = get_pratt_parser();
    let pairs = ExpressionParser::parse(Rule::declarations, input)?;
    // let pairs = pairs.into_iter().next().unwrap().into_inner();
    for pair in pairs.into_iter() {
        let inner_pair = pair.into_inner();

        for pair in inner_pair.into_iter() {
            let inner_pair = Pairs::single(pair);
            let node = parse_decls(inner_pair, &pratt);
            dbg!(node);
        }
    }

    Ok(())
}

#[test]
fn test_pratt() {
    // let inputs = [
    //     // "g = a+1.2*70-c.d.e(0, 2).to_int().unwrap(7) *@f * foo(a+2.3, b as f64)",
    //     "g = a as i32",
    //     "g = a + b",
    //     "g = @a.b as f64 + g() + f(a, b )",
    // ];
    // for input in inputs {
    //     let node = pratt_parse(input).unwrap();
    //     dbg!(node);
    // }

    let mut code = String::new();
    code += "type foo = struct { a i32, b f32 }\n";
    code += "type gen[T] = struct { a T, b ptr[T] }\n";
    code += "type doo = i32\n";
    code += "type u32_ptr = ptr[u32]\n";
    code += "type Node[T] = enum {
        None,
        Binary(Op, T, T),
        Unary(Op, T),
    }\n";

    parse_block(code.as_str()).unwrap();

    panic!()
}
