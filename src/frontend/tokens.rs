use std::collections::BTreeMap;

use cranelift::{
    codegen::ir::{types, AbiParam, Type},
    frontend::FunctionBuilder,
    prelude::*,
};
use logos::Logos;

#[derive(Logos, Clone, Debug)]
#[logos(skip r"[ \t\n\f]+")]
pub enum Keyword {
    Let,
    Mut,
    Const,
    Fn,
}

#[derive(Logos, Clone, PartialEq, PartialOrd)]
#[logos(skip r"[ \t\r\n\f]+")]
pub enum MantisLexerTokens {
    #[token("false", |_| false)]
    #[token("true", |_| true)]
    Bool(bool),

    #[token("{")]
    BraceOpen,

    #[token("}")]
    BraceClose,

    #[token("(")]
    BracketOpen,

    #[token(")")]
    BracketClose,

    #[token(":")]
    Colon,

    #[token("=")]
    Assign,

    #[token("+")]
    Add,

    #[token("-")]
    Sub,

    #[token("*")]
    Multiply,

    #[token("/")]
    Divide,

    #[token(",")]
    Comma,

    #[token(";")]
    SemiColon,

    #[token(".")]
    Dot,

    #[regex(r#""-?\d+""#, |lex| lex.slice().parse::<i64>().unwrap(), priority= 4)]
    Integer(i64),

    #[regex(r#"-?(\d+(\.\d*)?|\.\d+)"#, |lex| lex.slice().parse::<f64>().unwrap(), priority = 3)]
    Float(f64),

    #[regex(r#""([^"\\]|\\["\\bnfrt]|u[a-fA-F0-9]{4})*""#, |lex| lex.slice().to_owned(), priority= 3)]
    String(String),

    #[token("fn")]
    FunctionDecl,

    #[token("use")]
    Use,

    #[token("let")]
    Let,

    #[token("mut")]
    Mut,

    #[token("loop")]
    Loop,

    #[token("break")]
    Break,

    #[token("continue")]
    Continue,

    #[regex(r"\w+", |lex| lex.slice().to_owned())]
    Word(String),

    #[token("extern")]
    Extern,
}

#[derive(Clone, Debug)]
pub enum Token {
    Use(String),
    Keyword(Keyword),
    VarIdentifier(Variable),
    VarValue(Variable),
    Symbol(char),
    ConstLiteral(ConstLiteral),
}

#[derive(Clone, Debug, Default)]
pub struct VariableType(pub String);

#[derive(Clone, Debug, Default)]
pub struct Variable {
    pub name: String,
    pub var_type: VariableType,
}

impl Variable {
    pub fn new(name: impl Into<String>, var_type: VariableType) -> Self {
        Self {
            name: name.into(),
            var_type,
        }
    }

    pub fn const_i64() -> Self {
        let val = rand::random::<u32>();
        Self {
            name: format!("ci64_{val}"),
            var_type: VariableType("i64".into()),
        }
    }
    pub fn const_f64() -> Self {
        let val = rand::random::<u32>();
        Self {
            name: format!("fi64_{val}"),
            var_type: VariableType("f64".into()),
        }
    }
    pub fn const_bool() -> Self {
        let val = rand::random::<u32>();
        Self {
            name: format!("bool_{val}"),
            var_type: VariableType("bool".into()),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct ConstLiteral(Variable);

#[derive(Clone, Debug)]
pub enum Expression {
    Assign(Variable, Box<Expression>),
    ConstLiteral(Variable, String), // Variable and Value
    Variable(Variable),             // Variable and Value
    Declare(Variable, Box<Expression>),
    Add(Variable, Box<Expression>),
    Subtract(Variable, Box<Expression>),
    Multiply(Variable, Box<Expression>),
    Divide(Variable, Box<Expression>),
    Call(Variable, Vec<Expression>),
    Cast(Variable, Variable),
    Nil,
}

#[derive(Clone, Debug)]
pub struct FunctionDeclaration {
    pub name: String,
    pub return_type: VariableType,
    pub arguments: Vec<Variable>,
    pub body: Option<Vec<Expression>>,
}

pub fn ms_type_to_native_type(var_type: &VariableType) -> Type {
    match var_type.0.as_str() {
        "string" => types::I64,
        "f64" => types::F64,
        _ => types::I64,
    }
}

impl FunctionDeclaration {
    pub(crate) fn declare(
        &self,
        ctx: &mut cranelift::prelude::codegen::Context,
        fbx: &mut cranelift::prelude::FunctionBuilderContext,
    ) {
        ctx.func.signature.params = self
            .arguments
            .iter()
            .map(|x| AbiParam::new(ms_type_to_native_type(&x.var_type)))
            .collect();

        if !self.return_type.0.is_empty() && self.return_type.0 != "void" {
            ctx.func
                .signature
                .returns
                .push(AbiParam::new(ms_type_to_native_type(&self.return_type)));
        }

        let mut builder = FunctionBuilder::new(&mut ctx.func, fbx);

        let entry_block = builder.create_block();

        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);

        builder.seal_all_blocks();

        let mut variables = BTreeMap::new();

        let values = builder.block_params(entry_block).to_vec();
        for (value, variable) in std::iter::zip(values, self.arguments.iter()) {
            let var = cranelift_frontend::Variable::new();

            variables.insert(variable.name.clone(), value);
        }

        todo!()
    }
}

// #[derive(Clone, Debug)]
// pub enum Syntax {
//     Scope(Vec<Expression>),
//     FunctionDeclaration(FunctionDeclaration),
// }

#[derive(Clone, Debug)]
pub enum Operator {
    Add,
    Assign,
    Sub,
    Divide,
    Multiply,
}

#[derive(Clone, Debug)]
pub enum Node {
    Variable(Variable),
    UnaryExpr {
        op: Operator,
        child: Box<Node>,
    },
    BinaryExpr {
        op: Operator,
        lhs: Box<Node>,
        rhs: Box<Node>,
    },
}
