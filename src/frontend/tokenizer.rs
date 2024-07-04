use logos::{Lexer, Logos};

use crate::frontend::tokens::MantisLexerTokens;

use super::tokens::{ConstLiteral, Expression, Keyword, Token, Variable, VariableType};

pub fn read_to_tokens(input: String) -> Vec<Token> {
    let mut tokens = Vec::new();

    let mut lexer = MantisLexerTokens::lexer(&input);

    while let Some(token) = lexer.next() {
        match token {
            Ok(MantisLexerTokens::FunctionDecl) => {}

            _ => print!("Unsupported token {:?}\n", lexer.span()),
        };
    }

    return tokens;
}

pub fn parse_fn_declaration(lexer: &mut Lexer<'_, MantisLexerTokens>) {
    let mut fn_name = String::new();
    let mut arguments = Vec::new();
    let mut is_external = false;
    let mut fn_scope = Vec::new();
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
}

pub fn parse_scope(lexer: &mut Lexer<'_, MantisLexerTokens>) -> Vec<Expression> {
    let mut expressions = Vec::<Expression>::new();
    while let Some(token) = lexer.next() {
        match token {
            Ok(MantisLexerTokens::BraceOpen) => {
                expressions.extend(parse_scope(lexer));
            }

            Ok(MantisLexerTokens::BraceClose) => {
                break;
            }

            Ok(t) => {
                expressions.push(parse_expression(lexer));
            }

            _ => panic!("Unsupported token {:?}\n", lexer.span()),
        }
    }
    expressions
}

pub fn parse_expression(lexer: &mut Lexer<'_, MantisLexerTokens>) -> Expression {
    let mut expression = Expression::Nil;
    let mut next_is_type = false;
    while let Some(token) = lexer.next() {
        match token {
            Ok(MantisLexerTokens::Let) => {
                expression = Expression::Declare(Variable::default(), ConstLiteral::default());
            }

            Ok(MantisLexerTokens::Word(value)) => {
                if let Expression::Declare(var, con) = &mut expression {
                    if next_is_type {
                        var.var_type.0 = value;
                    } else {
                        var.name = value;
                    }
                }
            }

            Ok(MantisLexerTokens::Colon) => next_is_type = true,

            Ok(MantisLexerTokens::SemiColon) => break,
            _ => panic!("Unsupported token {:?}\n", lexer.span()),
        }
    }

    expression
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
