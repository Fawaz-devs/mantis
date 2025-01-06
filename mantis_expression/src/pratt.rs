use std::{
    fmt::{Debug, Display},
    hash::Hash,
    ops::{Deref, Range},
    rc::Rc,
};

use linear_map::LinearMap;
use pest::{
    iterators::{Pair, Pairs},
    pratt_parser::{Assoc, Op, PrattParser},
    Parser, Span,
};
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "./grammer.pest"]
pub struct ExpressionParser;

#[derive(Clone)]
pub struct WordSpan {
    content: Rc<str>,
    range: Range<usize>,
}

impl WordSpan {
    pub fn new(content: Rc<str>, range: Range<usize>) -> Self {
        assert!(content.len() >= range.end);
        Self {
            content: content.into(),
            range,
        }
    }

    pub fn from_span(span: Span, src: &Rc<str>) -> Self {
        Self::new(src.clone(), span.start()..span.end())
    }

    pub fn as_str(&self) -> &str {
        &self
    }

    pub fn range(&self) -> Range<usize> {
        self.range.clone()
    }
}

impl Debug for WordSpan {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s: &str = &self;
        write!(f, "{}@{}..{}", s, self.range.start, self.range.end)
    }
}

impl Deref for WordSpan {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.content[self.range.clone()]
    }
}

impl<'a> Into<&'a str> for &'a WordSpan {
    fn into(self) -> &'a str {
        self.deref()
    }
}

impl From<&str> for WordSpan {
    fn from(value: &str) -> Self {
        Self {
            content: value.into(),
            range: 0..value.len(),
        }
    }
}

impl PartialEq for WordSpan {
    fn eq(&self, other: &Self) -> bool {
        let s: &str = &self;
        let other: &str = other;

        s == other
    }
}

impl Eq for WordSpan {}

impl Hash for WordSpan {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write(self.as_bytes());
    }
}

impl Display for WordSpan {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s: &str = &self;
        write!(f, "{}@{}..{}", s, self.range.start, self.range.end)
    }
}

#[derive(Debug)]
pub enum Term {
    Array(Vec<Node>),
    Function(Box<FunctionDecl>),
    String(WordSpan),
    Type(Type),
    I64(i64),
    F64(f64),
    Char(char),
    Struct(Type, LinearMap<WordSpan, Node>),
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
    Let(WordSpan, Node, Type, bool), // var, expr, expected_type, is_mutable
    Return(Node),
    Break(Option<WordSpan>),
    Continue(Option<WordSpan>),
    Expr(Node),
}

#[derive(Debug)]
pub struct ConditionalBlock {
    pub condition: Node,
    pub block: Block,
}

#[derive(Debug)]
pub struct IfElseChain {
    pub if_block: ConditionalBlock,
    pub elif_blocks: Vec<ConditionalBlock>,
    pub else_block: Option<Block>,
}

#[derive(Debug)]
pub enum Block {
    Closed(Box<Block>),
    Statements(Vec<Statement>),
    Continuos(Vec<Block>),
    IfElseChain(Box<IfElseChain>),
    Loop(Option<WordSpan>, Box<Block>),
    Match(Node, Vec<MatchCase>),
    Empty,
}

#[derive(Debug)]
pub struct MatchCase {
    pub condition: Node,
    pub block: Block,
}

#[derive(Debug, Clone)]
pub enum Type {
    Enum {
        fields: LinearMap<WordSpan, Vec<Type>>,
    },
    Struct {
        fields: LinearMap<WordSpan, Type>,
    },
    WithGenerics(Box<Type>, Vec<Type>),
    Word(WordSpan),
    Nested(Box<Type>, Box<Type>),

    Ref(Box<Type>, bool), // (type, is_mutable)
    Unknown,
}

impl Type {
    pub fn to_string(&self) -> String {
        match self {
            Self::Word(word) => word.as_str().into(),
            Self::WithGenerics(ty, generics) => {
                let mut s = String::new();
                s.push_str(&ty.to_string());

                if !generics.is_empty() {
                    s.push_str("_[");
                    for gen in generics {
                        s.push_str(&gen.to_string());
                        s.push(',');
                    }
                    s.pop();
                    s.push_str("]");
                }
                s
            }
            _ => todo!(),
        }
    }

    pub fn word(&self) -> Option<&str> {
        match self {
            Type::Word(word_span) => Some(word_span.as_str()),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub struct FunctionDecl {
    pub name: Type,
    pub arguments: LinearMap<WordSpan, Type>,
    pub return_type: Type,
    pub block: Block,
    pub is_extern: bool,
}

#[derive(Debug)]
pub struct UseDecl {
    pub alias: Option<WordSpan>,
    pub path: Vec<WordSpan>,
}

#[derive(Debug)]
pub enum Declaration {
    Function(FunctionDecl),
    Type(Type, Type),
    Use(UseDecl),
    Trait(TraitDecl),
    TraitImpl(TraitDecl, Type),
}

#[derive(Debug)]
pub struct TraitDecl {
    pub name: Type,
    pub functions: Vec<FunctionDecl>,
}

fn get_pratt_parser() -> PrattParser<Rule> {
    PrattParser::new()
        .op(Op::infix(Rule::assign, Assoc::Right))
        .op(Op::infix(Rule::eq, Assoc::Left)
            | Op::infix(Rule::eq, Assoc::Left)
            | Op::infix(Rule::not_eq, Assoc::Left)
            | Op::infix(Rule::ge, Assoc::Left)
            | Op::infix(Rule::le, Assoc::Left)
            | Op::infix(Rule::le_eq, Assoc::Left)
            | Op::infix(Rule::gt_eq, Assoc::Left))
        .op(Op::infix(Rule::modulus, Assoc::Left))
        .op(Op::infix(Rule::add, Assoc::Left) | Op::infix(Rule::sub, Assoc::Left))
        .op(Op::infix(Rule::mul, Assoc::Left) | Op::infix(Rule::div, Assoc::Left))
        .op(Op::infix(Rule::pow, Assoc::Right))
        .op(Op::postfix(Rule::expr_call))
        .op(Op::postfix(Rule::propogate))
        .op(Op::prefix(Rule::neg))
        .op(Op::prefix(Rule::deref))
        .op(Op::infix(Rule::cast, Assoc::Right))
        .op(Op::prefix(Rule::at))
        .op(Op::infix(Rule::dot, Assoc::Left))
}

fn parse_expr(pairs: Pairs<Rule>, pratt: &PrattParser<Rule>, src: &Rc<str>) -> Node {
    pratt
        .map_primary(|primary| match primary.as_rule() {
            Rule::expr => parse_expr(primary.into_inner(), pratt, src),
            Rule::int => Node::Term(Term::I64(primary.as_str().parse::<i64>().unwrap())),
            Rule::float => Node::Term(Term::F64(primary.as_str().parse::<f64>().unwrap())),
            // Rule::word => Node::Term(Term::Ident(primary.as_str().into())),
            Rule::string_literal => {
                Node::Term(Term::String(WordSpan::from_span(primary.as_span(), src)))
            }
            Rule::char => {
                let c = primary.as_str();
                let c = &c[1..c.len() - 1];
                let c = match c {
                    "\\n" => '\n',
                    "\\r" => '\r',
                    "\\t" => '\t',
                    "\\0" => '\0',
                    "\\\\" => '\\',
                    _ => {
                        let mut iter = c.chars();
                        let c = iter.next().unwrap();
                        assert_eq!(iter.next(), None);
                        c
                    }
                };

                Node::Term(Term::Char(c))
            }

            Rule::type_name => {
                Node::Term(Term::Type(parse_type(Pairs::single(primary), pratt, src)))
            }

            Rule::fn_decl => Node::Term(Term::Function(parse_fn_decl(primary, pratt, src).into())),
            Rule::struct_initialization => Node::Term(parse_fn_struct_initialzation(
                primary.into_inner(),
                pratt,
                src,
            )),
            Rule::word => Node::Term(Term::Type(Type::Word(WordSpan::from_span(
                primary.as_span(),
                src,
            )))),

            Rule::array_initialization => Node::Term(Term::Array(parse_expr_list(
                primary.into_inner().next().unwrap(),
                pratt,
                src,
            ))),
            _ => unreachable!(
                "Unhandled Rule {:?} {:?}",
                primary.as_rule(),
                primary.as_str()
            ),
        })
        .map_prefix(|op, rhs| match op.as_rule() {
            Rule::at | Rule::neg | Rule::deref => Node::Unary(op.as_rule(), rhs.into()),
            _ => unreachable!(),
        })
        .map_infix(|lhs, op, rhs| match op.as_rule() {
            Rule::add
            | Rule::modulus
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
                    parse_expr_list(pair, pratt, src)
                } else {
                    Vec::new()
                };
                Node::FnCall(lhs.into(), args)
            }

            _ => unreachable!("Unhandled Rule {:?} {:?}", op.as_rule(), op.as_span()),
        })
        .parse(pairs)
}

fn parse_fn_struct_initialzation(
    pairs: Pairs<'_, Rule>,
    pratt: &PrattParser<Rule>,
    src: &Rc<str>,
) -> Term {
    let mut iter = pairs.into_iter();
    let type_name = parse_type(Pairs::single(iter.next().unwrap()), pratt, src);
    let assignment_list = iter.next().unwrap().into_inner().into_iter();
    let mut map = LinearMap::new();
    for single_assignment in assignment_list {
        let mut iter = single_assignment.into_inner().into_iter();
        let next = iter.next().unwrap();
        assert_eq!(next.as_rule(), Rule::word);
        let key_word = next.as_span();
        let key = WordSpan::from_span(key_word, src);
        let next = iter.next().unwrap();
        let assignment = parse_expr(Pairs::single(next), pratt, src);
        map.insert(key, assignment);
    }

    Term::Struct(type_name, map)
}
fn parse_decls(pairs: Pairs<Rule>, pratt: &PrattParser<Rule>, src: &Rc<str>) -> Declaration {
    pratt
        .map_primary(|primary| match primary.as_rule() {
            Rule::declarations => {
                let decl = primary.into_inner();
                parse_decls(decl, pratt, src)
            }
            Rule::type_decl => {
                let mut decl = primary.into_inner().into_iter();
                let name = decl.next().unwrap();
                let name = parse_type(Pairs::single(name), pratt, src);
                let ty = decl.next().unwrap();
                let ty = parse_type(Pairs::single(ty), pratt, src);

                Declaration::Type(name, ty)
            }

            Rule::fn_decl => Declaration::Function(parse_fn_decl(primary, pratt, src)),
            Rule::use_decl => Declaration::Use(parse_use_decl(primary, src)),
            Rule::trait_decl => Declaration::Trait(parse_trait_decl(primary, pratt, src)),

            Rule::trait_impl => {
                let (for_type, trait_decl) = parse_trait_impl(primary, pratt, src);
                Declaration::TraitImpl(trait_decl, for_type)
            }
            _ => unreachable!(
                "Unhandled Rule {:?} {:?}",
                primary.as_rule(),
                primary.as_str()
            ),
        })
        .parse(pairs)
}

fn parse_trait_impl(
    primary: Pair<'_, Rule>,
    pratt: &PrattParser<Rule>,
    src: &Rc<str>,
) -> (Type, TraitDecl) {
    let mut iter = primary.into_inner().into_iter();
    let trait_name = parse_type(Pairs::single(iter.next().unwrap()), pratt, src);
    let next = iter.next().unwrap();

    let mut functions = Vec::new();
    let for_type = if matches!(next.as_rule(), Rule::type_name) {
        parse_type(Pairs::single(next), pratt, src)
    } else {
        if matches!(next.as_rule(), Rule::fn_decl) {
            functions.push(parse_fn_decl(next, pratt, src));
        }
        trait_name.clone()
    };
    functions.extend(iter.map(|x| parse_fn_decl(x, pratt, src)));

    (
        for_type,
        TraitDecl {
            name: trait_name,
            functions,
        },
    )
}

fn parse_trait_decl(
    primary: Pair<'_, Rule>,
    pratt: &PrattParser<Rule>,
    src: &Rc<str>,
) -> TraitDecl {
    let mut iter = primary.into_inner().into_iter();
    let trait_name = parse_type(Pairs::single(iter.next().unwrap()), pratt, src);

    let functions = iter.map(|x| parse_fn_decl(x, pratt, src)).collect();

    TraitDecl {
        name: trait_name,
        functions,
    }
}

fn parse_use_decl(primary: Pair<Rule>, src: &Rc<str>) -> UseDecl {
    let mut path = Vec::new();
    let mut next_is_alias = false;
    let mut alias = None;
    for token in primary.into_inner().into_iter() {
        match token.as_rule() {
            Rule::word => {
                if next_is_alias {
                    let token = token.as_span();
                    alias = Some(WordSpan::from_span(token, src));
                    next_is_alias = false;
                } else {
                    let token = token.as_span();
                    path.push(WordSpan::from_span(token, src));
                }
            }
            Rule::cast => {
                next_is_alias = true;
            }

            _ => unreachable!(),
        }
    }

    UseDecl { alias, path }
}

fn parse_fn_decl(pair: Pair<Rule>, pratt: &PrattParser<Rule>, src: &Rc<str>) -> FunctionDecl {
    let mut iter = pair.into_inner().into_iter();
    let mut next = iter.next().unwrap();

    let fn_name = if matches!(next.as_rule(), Rule::type_name) {
        let fn_name = parse_type(Pairs::single(next), pratt, src);
        next = iter.next().unwrap();
        fn_name
    } else {
        let span = next.as_span();
        let fn_name = format!("closure_{}__{}", span.start(), span.end());
        Type::Word(WordSpan::from(fn_name.as_str()))
    };

    // let fn_name = parse_type(Pairs::single(next), pratt);
    // next = iter.next().unwrap();

    let args = if matches!(next.as_rule(), Rule::typed_args_list) {
        let args = next;
        next = iter.next().unwrap();
        parse_typed_args(args, pratt, src)
    } else {
        LinearMap::new()
    };

    let mut block = Block::Empty;
    let mut is_extern = false;
    let return_ty = if matches!(next.as_rule(), Rule::type_name) {
        let ret_ty = next;
        let ret_ty = parse_type(Pairs::single(ret_ty), pratt, src);
        if let Some(next_item) = iter.next() {
            next = next_item;
        } else {
            return FunctionDecl {
                name: fn_name,
                arguments: args,
                return_type: ret_ty,
                block,
                is_extern,
            };
        }
        ret_ty
    } else {
        Type::Unknown
    };

    if matches!(next.as_rule(), Rule::block) {
        loop {
            block = parse_block(next, pratt, src);
            if let Some(next_block) = iter.next() {
                next = next_block;
            } else {
                break;
            }
        }
    } else if matches!(next.as_rule(), Rule::extern_word) {
        is_extern = true;
    }

    FunctionDecl {
        name: fn_name,
        arguments: args,
        return_type: return_ty,
        block,
        is_extern,
    }
}

fn parse_block(pair: Pair<Rule>, pratt: &PrattParser<Rule>, src: &Rc<str>) -> Block {
    let mut blocks = Vec::new();
    for pair in pair.into_inner().into_iter() {
        match pair.as_rule() {
            Rule::stmts => {
                let mut statements = Vec::new();
                for pair in pair.into_inner().into_iter() {
                    statements.push(parse_statement(pair, pratt, src))
                }

                blocks.push(Block::Statements(statements));
            }
            Rule::if_else_chain => {
                let mut iter = pair.into_inner().into_iter();
                let if_block = iter.next().unwrap();
                let if_block = {
                    let mut if_iter = if_block.into_inner().into_iter();
                    let condition = parse_expr(Pairs::single(if_iter.next().unwrap()), pratt, src);
                    let block = parse_block(if_iter.next().unwrap(), pratt, src);
                    ConditionalBlock { condition, block }
                };

                let mut elif_blocks = Vec::new();
                let mut else_block = None;

                while let Some(else_or_else_if_block) = iter.next() {
                    if matches!(else_or_else_if_block.as_rule(), Rule::elif_block) {
                        let mut if_iter = else_or_else_if_block.into_inner().into_iter();
                        let condition =
                            parse_expr(Pairs::single(if_iter.next().unwrap()), pratt, src);
                        let block = parse_block(if_iter.next().unwrap(), pratt, src);
                        elif_blocks.push(ConditionalBlock { condition, block });
                    } else if matches!(else_or_else_if_block.as_rule(), Rule::else_block) {
                        else_block = Some(parse_block(
                            else_or_else_if_block.into_inner().next().unwrap(),
                            pratt,
                            src,
                        ));
                    }
                }

                blocks.push(Block::IfElseChain(
                    IfElseChain {
                        if_block,
                        elif_blocks,
                        else_block,
                    }
                    .into(),
                ));
            }

            Rule::loop_block => {
                let mut iter = pair.into_inner().into_iter();
                let mut next = iter.next().unwrap();
                let loop_name = if matches!(next.as_rule(), Rule::word) {
                    let ln = next.as_span();
                    next = iter.next().unwrap();
                    Some(WordSpan::from_span(ln, src))
                } else {
                    None
                };

                let block = parse_block(next, pratt, src);
                blocks.push(Block::Loop(loop_name, block.into()));
            }
            Rule::block => {
                let block = Block::Closed(parse_block(pair, pratt, src).into());
                blocks.push(block);
            }
            Rule::match_block => {
                let block = parse_match_block(pair, pratt, src);
                blocks.push(block);
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

fn parse_match_block(pair: Pair<Rule>, pratt: &PrattParser<Rule>, src: &Rc<str>) -> Block {
    let mut iter = pair.into_inner().into_iter();
    let expr = iter.next().unwrap();
    let expr = parse_expr(Pairs::single(expr), pratt, src);
    let mut match_cases = Vec::new();
    for case in iter {
        match_cases.push(parse_match_case(case, pratt, src));
    }

    Block::Match(expr, match_cases)
}

fn parse_match_case(pair: Pair<Rule>, pratt: &PrattParser<Rule>, src: &Rc<str>) -> MatchCase {
    let mut iter = pair.into_inner().into_iter();
    let expr = iter.next().unwrap();
    let expr = parse_expr(Pairs::single(expr), pratt, src);

    let block = iter.next().unwrap();
    let block = parse_block(block, pratt, src);

    MatchCase {
        condition: expr,
        block,
    }
}

fn parse_statement(pair: Pair<Rule>, pratt: &PrattParser<Rule>, src: &Rc<str>) -> Statement {
    let inner = pair.into_inner().next().unwrap();
    let result = match inner.as_rule() {
        Rule::let_stmt => {
            let mut iter = inner.into_inner().into_iter();
            let keyword = iter.next().unwrap();

            let is_mutable = matches!(keyword.as_rule(), Rule::mut_word);

            let lhs = iter.next().unwrap().as_str();
            let mut next = iter.next().unwrap();
            let ty = if matches!(next.as_rule(), Rule::type_name) {
                let ty = parse_type(Pairs::single(next), pratt, src);
                next = iter.next().unwrap();
                ty
            } else {
                Type::Unknown
            };

            let rhs = parse_expr(Pairs::single(next), pratt, src);
            Statement::Let(lhs.into(), rhs, ty, is_mutable)
        }
        Rule::ret_stmt => {
            let expr = inner.into_inner().next().unwrap();
            let expr = parse_expr(Pairs::single(expr), pratt, src);
            Statement::Return(expr)
        }
        Rule::break_stmt => {
            if let Some(word) = inner.into_inner().into_iter().next() {
                let word = word.as_span();
                Statement::Break(Some(WordSpan::from_span(word, src)))
            } else {
                Statement::Break(None)
            }
        }
        Rule::continue_stmt => {
            if let Some(word) = inner.into_inner().into_iter().next() {
                let word = word.as_span();
                Statement::Continue(Some(WordSpan::from_span(word, src)))
            } else {
                Statement::Continue(None)
            }
        }
        Rule::expr => Statement::Expr(parse_expr(Pairs::single(inner), pratt, src)),
        _ => unreachable!(),
    };
    result
}

fn parse_typed_args(
    pair: Pair<Rule>,
    pratt: &PrattParser<Rule>,
    src: &Rc<str>,
) -> LinearMap<WordSpan, Type> {
    if !(matches!(pair.as_rule(), Rule::typed_args_list)) {
        panic!("{:#?}", pair);
    }

    let mut map = LinearMap::new();

    for pair in pair.into_inner() {
        let mut typed_arg = pair.into_inner().into_iter();
        let arg_name = typed_arg.next().unwrap().as_span();
        let arg_type = typed_arg.next().unwrap();
        let arg_type = parse_type(Pairs::single(arg_type), pratt, src);

        map.insert(WordSpan::from_span(arg_name, src), arg_type);
    }

    map
}

fn parse_type(pairs: Pairs<Rule>, pratt: &PrattParser<Rule>, src: &Rc<str>) -> Type {
    pratt
        .map_primary(|primary| match primary.as_rule() {
            Rule::type_name => {
                let mut iter = primary.into_inner().into_iter();
                let mut word = iter.next().unwrap();

                let mut is_reference = false;

                let mut reference_mutables = Vec::new();

                loop {
                    let mut is_mutable = false;
                    if matches!(word.as_rule(), Rule::at) {
                        word = iter.next().unwrap();
                        is_reference = true;
                    }
                    if matches!(word.as_rule(), Rule::mut_word) {
                        word = iter.next().unwrap();
                        is_mutable = true;
                    }

                    if is_reference {
                        reference_mutables.push(is_mutable);
                    }
                    if matches!(word.as_rule(), Rule::nested_type_name) {
                        break;
                    }
                }

                let word = parse_type(Pairs::single(word), pratt, src);

                let ty = if let Some(type_list) = iter.next() {
                    let mut generics = Vec::new();
                    for ty in type_list.into_inner().into_iter() {
                        generics.push(parse_type(Pairs::single(ty), pratt, src));
                    }

                    Type::WithGenerics(word.into(), generics)
                } else {
                    word
                };
                if is_reference {
                    let mut final_ty = ty;
                    for mutable in reference_mutables.into_iter().rev() {
                        final_ty = Type::Ref(final_ty.into(), mutable);
                    }

                    // Type::Ref(ty.into(), is_mutable)
                    final_ty
                } else {
                    ty
                }
            }
            Rule::word => Type::Word(WordSpan::from_span(primary.as_span(), src)),
            Rule::struct_decl => {
                let mut iter = primary.into_inner().into_iter();
                let typed_args_list = iter.next().unwrap();
                let map = parse_typed_args(typed_args_list, pratt, src);

                Type::Struct { fields: map }
            }

            Rule::enum_decl => {
                let enum_variants = primary
                    .into_inner()
                    .into_iter()
                    .next()
                    .unwrap()
                    .into_inner();

                let mut map = LinearMap::new();

                for enum_variant in enum_variants {
                    let mut iter = enum_variant.into_inner().into_iter();
                    let key_word = iter.next().unwrap().as_span();
                    let mut args = Vec::new();
                    if let Some(type_list) = iter.next() {
                        for type_name in type_list.into_inner().into_iter() {
                            args.push(parse_type(Pairs::single(type_name), pratt, src));
                        }
                    }
                    map.insert(WordSpan::from_span(key_word, src), args);
                }

                Type::Enum { fields: map }
            }

            Rule::nested_type_name => {
                let iter = primary.into_inner().into_iter();
                let mut types = Vec::new();
                for word in iter {
                    types.push(parse_type(Pairs::single(word), pratt, src));
                }

                let mut rev_iter = types.into_iter().rev();
                let mut ty = rev_iter.next().unwrap_or(Type::Unknown);
                for nty in rev_iter {
                    ty = Type::Nested(nty.into(), ty.into());
                }

                ty
            }

            _ => unreachable!(
                "Unhandled Rule {:?} {:?}",
                primary.as_rule(),
                primary.as_str()
            ),
        })
        .parse(pairs)
}

fn parse_expr_list(pair: Pair<Rule>, pratt: &PrattParser<Rule>, src: &Rc<str>) -> Vec<Node> {
    let pairs = pair.into_inner();

    pairs
        .into_iter()
        .map(|x| parse_expr(Pairs::single(x), pratt, src))
        .collect::<Vec<_>>()
}

pub fn parse_blocks(input: &Rc<str>) -> Result<Vec<Declaration>, pest::error::Error<Rule>> {
    let pratt = get_pratt_parser();
    let pairs = ExpressionParser::parse(Rule::declarations, input)?;
    let mut declarataions = Vec::new();
    for pair in pairs.into_iter() {
        let inner_pair = pair.into_inner();

        for pair in inner_pair.into_iter() {
            let inner_pair = Pairs::single(pair);
            let declaration = parse_decls(inner_pair, &pratt, input);
            declarataions.push(declaration)
        }
    }

    Ok(declarataions)
}

#[test]
fn test_pratt() {
    let code = std::fs::read_to_string("test.ms").unwrap();
    let src = code.into();

    let decls = parse_blocks(&src).unwrap();

    let count = Rc::strong_count(&src);
    std::fs::write(
        "/tmp/declarations",
        format!("{:#?}\nrc count: {}", decls, count),
    )
    .unwrap();
}
