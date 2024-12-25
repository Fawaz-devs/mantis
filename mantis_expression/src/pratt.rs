use linear_map::LinearMap;
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
    Char(char),
    String(Box<str>),
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
    Closed(Vec<Statement>),
    Opened(Vec<Statement>),
    Continuos(Vec<Block>),
    If(Node, Box<Block>),
    Elif(Node, Box<Block>),
    Else(Box<Block>),
    Loop(Box<str>, Box<Block>),
}

#[derive(Debug)]
pub enum Type {
    Enum {
        fields: LinearMap<Box<str>, Vec<Type>>,
    },
    Struct {
        fields: LinearMap<Box<str>, Type>,
    },
    WithGenerics(Box<str>, Vec<Type>),
    Word(Box<str>),
}

pub struct TypeDecl {
    name: Type,
    ty: Type,
}

#[derive(Debug)]
pub enum Declaration {
    Function(Type, LinearMap<Box<str>, Type>, Type, Block),
    Type(Type, Type),
    // Struct(Box<str>, HashMap<Box<str>, Type>),
}

fn get_pratt_parser() -> PrattParser<Rule> {
    let pratt = PrattParser::new()
        .op(Op::infix(Rule::assign, Assoc::Right))
        .op(Op::infix(Rule::eq, Assoc::Left)
            | Op::infix(Rule::eq, Assoc::Left)
            | Op::infix(Rule::not_eq, Assoc::Left)
            | Op::infix(Rule::ge, Assoc::Left)
            | Op::infix(Rule::le, Assoc::Left)
            | Op::infix(Rule::le_eq, Assoc::Left)
            | Op::infix(Rule::gt_eq, Assoc::Left))
        .op(Op::infix(Rule::add, Assoc::Left) | Op::infix(Rule::sub, Assoc::Left))
        .op(Op::infix(Rule::mul, Assoc::Left) | Op::infix(Rule::div, Assoc::Left))
        .op(Op::infix(Rule::pow, Assoc::Right))
        .op(Op::postfix(Rule::expr_call))
        .op(Op::prefix(Rule::neg))
        .op(Op::infix(Rule::cast, Assoc::Right))
        .op(Op::prefix(Rule::at))
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
            Rule::string_literal => Node::Term(Term::String(primary.as_str().into())),
            Rule::char => {
                let c = primary.as_str();
                let c = &c[1..c.len() - 1];
                let c = c.chars().next().unwrap();

                Node::Term(Term::Char(c))
            }
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
            | Rule::eq
            | Rule::not_eq
            | Rule::gt_eq
            | Rule::ge
            | Rule::le_eq
            | Rule::le
            | Rule::cast => Node::Binary(op.as_rule(), lhs.into(), rhs.into()),
            _ => unreachable!(),
        })
        .map_postfix(|lhs, op| match op.as_rule() {
            // Rule::fac => Node::Unary(Rule::fac, lhs.into()),
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
            }

            Rule::fn_decl => {
                let mut iter = primary.into_inner().into_iter();
                let mut next = iter.next().unwrap();
                let fn_name = parse_type(Pairs::single(next), pratt);

                next = iter.next().unwrap();

                let args = if matches!(next.as_rule(), Rule::typed_args_list) {
                    let args = next;
                    next = iter.next().unwrap();
                    parse_typed_args(args, pratt)
                } else {
                    LinearMap::new()
                };

                let return_ty = if matches!(next.as_rule(), Rule::type_name) {
                    let ret_ty = next;
                    next = iter.next().unwrap();
                    parse_type(Pairs::single(ret_ty), pratt)
                } else {
                    Type::Word("void".into())
                };

                // let mut blocks = Vec::new();
                let mut block = Block::Opened(Vec::new());

                if matches!(next.as_rule(), Rule::block) {
                    loop {
                        block = parse_block(next, pratt);
                        if let Some(next_block) = iter.next() {
                            next = next_block;
                        } else {
                            break;
                        }
                    }
                };

                Declaration::Function(fn_name, args, return_ty, block)
            }

            _ => unreachable!(
                "Unhandled Rule {:?} {:?}",
                primary.as_rule(),
                primary.as_str()
            ),
        })
        .parse(pairs)
}

fn parse_block(pair: Pair<Rule>, pratt: &PrattParser<Rule>) -> Block {
    let mut blocks = Vec::new();
    for pair in pair.into_inner().into_iter() {
        match pair.as_rule() {
            Rule::stmts => {
                let mut statements = Vec::new();
                for pair in pair.into_inner().into_iter() {
                    statements.push(parse_statement(pair, pratt))
                }

                blocks.push(Block::Opened(statements));
            }
            Rule::if_block => {
                let mut iter = pair.into_inner().into_iter();
                let expr = parse_expr(Pairs::single(iter.next().unwrap()), pratt);
                let block = parse_block(iter.next().unwrap(), pratt);
                blocks.push(Block::If(expr, block.into()));
            }
            Rule::elif_block => {
                let mut iter = pair.into_inner().into_iter();
                let expr = parse_expr(Pairs::single(iter.next().unwrap()), pratt);
                let block = parse_block(iter.next().unwrap(), pratt);
                blocks.push(Block::Elif(expr, block.into()));
            }
            Rule::else_block => {
                let mut iter = pair.into_inner().into_iter();
                let block = parse_block(iter.next().unwrap(), pratt);
                blocks.push(Block::Else(block.into()));
            }

            Rule::loop_block => {
                let mut iter = pair.into_inner().into_iter();
                let mut next = iter.next().unwrap();
                let loop_name = if matches!(next.as_rule(), Rule::word) {
                    let ln = next.as_str().into();
                    next = iter.next().unwrap();
                    ln
                } else {
                    "".into()
                };

                let block = parse_block(next, pratt);
                blocks.push(Block::Loop(loop_name, block.into()));
            }

            _ => unreachable!("{:#?}", pair),
        }
    }

    if blocks.len() == 1 {
        return blocks.pop().unwrap();
    } else {
        return Block::Continuos(blocks);
    }
}

fn parse_statement(pair: Pair<Rule>, pratt: &PrattParser<Rule>) -> Statement {
    let inner = pair.into_inner().next().unwrap();
    let result = match inner.as_rule() {
        Rule::let_stmt => {
            let mut iter = inner.into_inner().into_iter();
            let lhs = iter.next().unwrap().as_str();
            let mut next = iter.next().unwrap();
            let mut ty = Type::Word("unknown".into());
            if matches!(next.as_rule(), Rule::type_name) {
                ty = parse_type(Pairs::single(next), pratt);
                next = iter.next().unwrap();
            }
            let rhs = parse_expr(Pairs::single(next), pratt);
            _ = ty; // TODO: use this for casting
            Statement::Let(lhs.into(), rhs)
        }
        Rule::ret_stmt => {
            let expr = inner.into_inner().next().unwrap();
            let expr = parse_expr(Pairs::single(expr), pratt);
            Statement::Return(expr)
        }
        Rule::break_stmt => {
            let word = inner
                .into_inner()
                .into_iter()
                .next()
                .map(|x| x.as_str().into())
                .unwrap_or("".into());

            Statement::Break(word)
        }
        Rule::continue_stmt => {
            let word = inner
                .into_inner()
                .into_iter()
                .next()
                .map(|x| x.as_str().into())
                .unwrap_or("".into());

            Statement::Continue(word)
        }
        Rule::expr => Statement::Expr(parse_expr(Pairs::single(inner), pratt)),
        _ => unreachable!(),
    };
    result
}

fn parse_typed_args(pair: Pair<Rule>, pratt: &PrattParser<Rule>) -> LinearMap<Box<str>, Type> {
    if !(matches!(pair.as_rule(), Rule::typed_args_list)) {
        panic!("{:#?}", pair);
    }

    let mut map = LinearMap::new();

    for pair in pair.into_inner() {
        let mut typed_arg = pair.into_inner().into_iter();
        let arg_name = typed_arg.next().unwrap();
        let arg_type = typed_arg.next().unwrap();
        let arg_type = parse_type(Pairs::single(arg_type), pratt);
        map.insert(arg_name.as_str().into(), arg_type);
    }

    map
}

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
                } else {
                    parse_type(Pairs::single(word), pratt)
                }
            }
            Rule::word => Type::Word(primary.as_str().into()),
            Rule::struct_decl => {
                let mut iter = primary.into_inner().into_iter();
                let typed_args_list = iter.next().unwrap();
                let map = parse_typed_args(typed_args_list, pratt);

                Type::Struct { fields: map }
            }

            Rule::enum_decl => {
                let enum_variants = primary
                    .into_inner()
                    .into_iter()
                    .next()
                    .unwrap()
                    .into_inner();

                let mut map = LinearMap::<Box<str>, Vec<Type>>::new();

                for enum_variant in enum_variants {
                    let mut iter = enum_variant.into_inner().into_iter();
                    let word = iter.next().unwrap().as_str();
                    let mut args = Vec::new();
                    if let Some(type_list) = iter.next() {
                        for type_name in type_list.into_inner().into_iter() {
                            args.push(parse_type(Pairs::single(type_name), pratt));
                        }
                    }
                    map.insert(word.into(), args);
                }

                Type::Enum { fields: map }
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

fn parse_blocks(input: &str) -> anyhow::Result<()> {
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
    let inputs = [
        // "g = a+1.2*70-c.d.e(0, 2).to_int().unwrap(7) *@f * foo(a+2.3, b as f64)",
        // "g = a as i32",
        // "g = a + b",
        // "g = @a.b as f64 + g() + f(a, b == 0)",
        "a != b",
        "g = a == b * 'd'",
        r#"g = "hello wor\nld""#,
    ];
    for input in inputs {
        let node = pratt_parse(input).unwrap();
        dbg!(node);
    }

    panic!();

    let mut code = String::new();
    code += "type foo = struct { a i32, b f32 }\n";
    code += "type gen[T] = struct { a T, b ptr[T] }\n";
    code += "type doo = i32\n";
    code += "type u32_ptr = ptr[u32]\n";
    code += "type Node[T] = enum { None, Binary(Op, T, ptr[T]), Unary(Op, T) }\n";

    code += "fn malloc(size i64) i64 extern\n";
    code += "fn foo() i32 { return 74; }\n";
    code += "fn main(argc i32, argv i64) i32 { let a = 29; mut b = 2 + a * 6; return a + b; }\n";

    code += r#"
        fn fibonacci(n i32) i32 {
            let i = n;
            if n == 0  {
                return 0;
            } elif n == 1 { 
                return 1;
            } else {
                return fibonacci(n-1) + fibonacci(n - 2);
            }
        }
    "#;

    code += r#"
    fn loop_test(n i32) {
        let i = n;
        loop my_loop {
            println("{}", i);
            i = i - 1;
            if i < 0 {
                break;
            }
        }
    }

        
    "#;

    println!("{code}");

    parse_blocks(code.as_str()).unwrap();

    panic!()
}
