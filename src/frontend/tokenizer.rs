use logos::{Lexer, Logos};

use crate::frontend::tokens::MantisLexerTokens;

use super::tokens::{
    ConstLiteral, Expression, FunctionDeclaration, Keyword, Token, Variable, VariableType,
};

pub fn read_to_tokens(input: String) -> Vec<Token> {
    let mut tokens = Vec::new();

    let mut lexer = MantisLexerTokens::lexer(&input);

    while let Some(token) = lexer.next() {
        match token {
            Ok(MantisLexerTokens::FunctionDecl) => {
                let decl = parse_fn_declaration(&mut lexer);
                println!("{:?}", decl);
            }

            _ => print!("Unsupported token {:?}\n", lexer.span()),
        };
    }

    return tokens;
}

pub fn parse_fn_declaration(lexer: &mut Lexer<'_, MantisLexerTokens>) -> FunctionDeclaration {
    let mut fn_name = String::new();
    let mut arguments = Vec::new();
    let mut is_external = false;
    let mut fn_scope = Vec::new();
    let mut return_type = VariableType(String::from("void"));
    while let Some(token) = lexer.next() {
        match token {
            Ok(MantisLexerTokens::Word(value)) => {
                if fn_name.is_empty() {
                    fn_name = value;
                }
            }
            Ok(MantisLexerTokens::BracketOpen) => {
                arguments = parse_arguments(lexer);
            }

            Ok(MantisLexerTokens::Extern) => {
                is_external = true;
            }
            Ok(MantisLexerTokens::SemiColon) => {
                break;
            }

            Ok(MantisLexerTokens::BraceOpen) => {
                if !is_external {
                    fn_scope = parse_scope(lexer);
                    break;
                } else {
                    panic!("External Function can't have function scope");
                }
            }
            _ => print!("Unsupported token {:?}\n", lexer.span()),
        }
    }

    return FunctionDeclaration {
        name: fn_name,
        arguments,
        body: Some(fn_scope),
        return_type,
    };
}

pub fn parse_scope(lexer: &mut Lexer<'_, MantisLexerTokens>) -> Vec<Expression> {
    let mut expressions = Vec::<Expression>::new();

    loop {
        let expression = parse_expression(lexer);
        if matches!(expression, Expression::Nil) {
            break;
        }
        expressions.push(expression);
    }

    expressions
}

pub fn parse_expression(lexer: &mut Lexer<'_, MantisLexerTokens>) -> Expression {
    let mut expression = Expression::Nil;
    let mut tokens = Vec::new();
    while let Some(token) = lexer.next() {
        match token {
            Ok(MantisLexerTokens::SemiColon) => {
                expression = build_ast(tokens);
                break;
            }
            Ok(token) => {
                tokens.push(token);
            }
            _ => panic!("Unsupported token {:?}, {}\n", lexer.span(), lexer.slice()),
        }
    }

    expression
}

pub fn build_ast(tokens: Vec<MantisLexerTokens>) -> Expression {
    match tokens.as_slice() {
        [MantisLexerTokens::Let, MantisLexerTokens::Word(var_name), MantisLexerTokens::Assign, MantisLexerTokens::Integer(val)] =>
        {
            return Expression::Declare(
                Variable::new(var_name, VariableType("i64".into())),
                Box::new(Expression::ConstLiteral(
                    Variable::const_i64(),
                    val.to_string(),
                )),
            );
        }
        [MantisLexerTokens::Let, MantisLexerTokens::Word(var_name), MantisLexerTokens::Assign, MantisLexerTokens::Float(val)] =>
        {
            return Expression::Declare(
                Variable::new(var_name, VariableType("i64".into())),
                Box::new(Expression::ConstLiteral(
                    Variable::const_f64(),
                    val.to_string(),
                )),
            );
        }
        [MantisLexerTokens::Let, MantisLexerTokens::Word(var_name), MantisLexerTokens::Assign, MantisLexerTokens::Word(val_name)] =>
        {
            return Expression::Declare(
                Variable::new(var_name, VariableType("i64".into())),
                Box::new(Expression::Variable(Variable::new(
                    val_name,
                    VariableType::default(),
                ))),
            );
        }

        [MantisLexerTokens::Word(fn_name), MantisLexerTokens::BracketOpen, MantisLexerTokens::String(value), MantisLexerTokens::BracketClose] => {
            return Expression::Call(
                Variable::new(fn_name, VariableType("fn".into())),
                vec![Expression::Variable(Variable::new(
                    value,
                    VariableType("string".into()),
                ))],
            )
        }
        _ => {}
    }
    panic!("Invalid expression not supported");
}

pub fn parse_arguments(lexer: &mut Lexer<'_, MantisLexerTokens>) -> Vec<Variable> {
    let mut variables = Vec::new();

    let mut next_is_type = false;
    let mut next_is_variable = true;

    while let Some(token) = lexer.next() {
        match token {
            Ok(MantisLexerTokens::Word(value)) => {
                if next_is_variable {
                    let variable = Variable {
                        name: value,
                        var_type: VariableType(String::new()),
                    };
                    variables.push(variable);
                    next_is_variable = false;
                } else if next_is_type {
                    variables.last_mut().unwrap().var_type = VariableType(value);
                    next_is_type = false;
                }
            }

            Ok(MantisLexerTokens::Colon) => {
                next_is_type = true;
            }
            Ok(MantisLexerTokens::Comma) => {
                next_is_variable = true;
            }

            Ok(MantisLexerTokens::BracketClose) => {
                break;
            }
            _ => print!("Unsupported token {:?}\n", lexer.span()),
        }
    }

    return variables;
}
