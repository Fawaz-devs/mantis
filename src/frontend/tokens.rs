use logos::Logos;

#[derive(Logos, Clone, Debug)]
#[logos(skip r"[ \t\n\f]+")]
pub enum Keyword {
    Let,
    Mut,
    Const,
    Fn,
}

#[derive(Logos, Clone, Debug)]
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

#[derive(Clone, Debug, Default)]
pub struct ConstLiteral(Variable);

#[derive(Clone, Debug)]
pub enum Expression {
    Assign(Variable, Variable),
    Declare(Variable, ConstLiteral),
    Add(Variable, Variable),
    Subtract(Variable, Variable),
    Multiply(Variable, Variable),
    Divide(Variable, Variable),
    Call(Variable, Vec<Variable>),
    Cast(Variable, Variable),
    Nil,
}

#[derive(Clone, Debug)]
pub struct FunctionDeclaration {
    name: String,
    return_type: VariableType,
    arguments: Vec<VariableType>,
    is_external: bool,
}

#[derive(Clone, Debug)]
pub enum Syntax {
    Scope(Vec<Expression>),
    FunctionDeclaration(FunctionDeclaration),
}

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
