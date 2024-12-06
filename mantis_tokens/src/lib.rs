use logos::Logos;

#[derive(Logos, Clone, PartialEq, PartialOrd, Debug)]
#[logos(skip r"[ \t\r\n\f]+")]
pub enum MantisLexerTokens {
    #[regex(r#"-?\d+"#, |lex| lex.slice().parse::<i64>().unwrap())]
    Integer(i64),

    // #[regex(r#"\w+"#, |lex| lex.slice().to_owned())]
    #[regex(r#"[a-zA-Z_][a-zA-Z0-9_]*"#, |lex| lex.slice().to_owned())]
    Word(String),

    #[regex(r#"\d+\.\d+"#, |lex| lex.slice().parse::<f64>().unwrap())]
    Float(f64),

    #[token(".")]
    Dot,

    #[regex(r"//[^\n]*", logos::skip)]
    Comment,

    #[token("return")]
    Return,

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

    #[token("==")]
    EqualTo,

    #[token(">")]
    GreaterThan,

    #[token("<")]
    LessThan,

    #[token(">=")]
    GreaterThanOrEqualTo,

    #[token("<=")]
    LessThanOrEqualTo,

    #[token("!=")]
    NotEqualTo,

    #[regex(r#""([^"\\]|\\["\\bnfrt]|u[a-fA-F0-9]{4})*""#, |lex| {
        let s = lex.slice();
        if s.len() > 2 {
            s[1..s.len() -1].to_owned()
        } else {
            s.to_owned()
        }
    })]
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

    #[token("extern")]
    Extern,

    #[token("as")]
    As,

    #[token("if")]
    If,

    #[token("else")]
    Else,

    #[token("elif")]
    ElseIf,

    #[token("struct")]
    Struct,

    #[token("match")]
    Match,
}
