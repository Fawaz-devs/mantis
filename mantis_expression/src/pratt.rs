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
    Ident(String),
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

fn pratt_parse(input: &str) -> anyhow::Result<Node> {
    let input = "a+1.2*70-c.d.e(0, 2)*@f * foo(a+2.3, b)";
    let pairs = ExpressionParser::parse(Rule::expr, input).unwrap();
    let pratt = PrattParser::new()
        .op(Op::infix(Rule::add, Assoc::Left) | Op::infix(Rule::sub, Assoc::Left))
        .op(Op::infix(Rule::mul, Assoc::Left) | Op::infix(Rule::div, Assoc::Left))
        .op(Op::infix(Rule::pow, Assoc::Right))
        .op(Op::postfix(Rule::expr_call))
        .op(Op::infix(Rule::dot, Assoc::Left))
        .op(Op::prefix(Rule::neg))
        .op(Op::prefix(Rule::at))
        .op(Op::postfix(Rule::fac));

    let node = parse_expr(pairs, &pratt);
    Ok(node)
}

fn parse_expr(pairs: Pairs<Rule>, pratt: &PrattParser<Rule>) -> Node {
    pratt
        .map_primary(|primary| match primary.as_rule() {
            Rule::expr => parse_expr(primary.into_inner(), pratt),
            Rule::int => Node::Term(Term::I64(primary.as_str().parse::<i64>().unwrap())),
            Rule::float => Node::Term(Term::F64(primary.as_str().parse::<f64>().unwrap())),
            Rule::word => Node::Term(Term::Ident(primary.as_str().to_string())),
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
            Rule::add | Rule::sub | Rule::mul | Rule::div | Rule::pow | Rule::dot => {
                Node::Binary(op.as_rule(), lhs.into(), rhs.into())
            }
            _ => unreachable!(),
        })
        .map_postfix(|lhs, op| match op.as_rule() {
            Rule::fac => Node::Unary(Rule::fac, lhs.into()),
            Rule::expr_call => {
                let args = parse_expr_list(op.into_inner().into_iter().next().unwrap(), pratt);
                Node::FnCall(lhs.into(), args)
            }
            _ => unreachable!(),
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
