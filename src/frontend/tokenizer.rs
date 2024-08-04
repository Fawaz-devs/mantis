use cranelift::codegen::ir::{types, Type};
use logos::{Lexer, Logos};

use crate::frontend::tokens::MantisLexerTokens;

use super::tokens::{
    BuiltInType, ConstLiteral, Expression, FunctionDeclaration, Keyword, MsVariable, Node,
    StructMapBuilder, StructRegistry, Token, VariableType,
};

pub fn collect_to_tokens(input: &str) {
    let mut lexer = MantisLexerTokens::lexer(&input);

    while let Some(token) = lexer.next() {
        match token {
            Ok(token) => log::info!("{:?}, {:?} {}", token, lexer.span(), lexer.slice()),
            Err(err) => log::error!("unknown token {:?} {}", lexer.span(), lexer.slice()),
        }
    }
}

pub fn read_to_tokens(input: String) -> (Vec<FunctionDeclaration>, StructRegistry) {
    let mut lexer = MantisLexerTokens::lexer(&input);

    let mut functions = Vec::new();

    let mut struct_registry = StructRegistry::new();

    while let Some(token) = lexer.next() {
        match token {
            Ok(MantisLexerTokens::FunctionDecl) => {
                let decl = parse_fn_declaration(&mut lexer, &struct_registry);
                functions.push(decl);
            }

            Ok(MantisLexerTokens::Struct) => {
                parse_struct_scope(&mut lexer, &mut struct_registry);
            }

            _ => log::error!("Unsupported token {:?} {}\n", lexer.span(), lexer.slice()),
        };
    }

    return (functions, struct_registry);
}

// pub fn map_type_to_native(val: &str) -> Option<VariableType> {
//     let t = match val {
//         "i32" => VariableType::Native(types::I32),
//         "i64" => VariableType::Native(types::I64),
//         "f64" => VariableType::Native(types::F64),
//         "f32" => VariableType::Native(types::F32),
//         _ => {
//             return None;
//         }
//     };
//     Some(t)
// }

pub fn parse_fn_declaration(
    lexer: &mut Lexer<'_, MantisLexerTokens>,
    struct_registry: &StructRegistry,
) -> FunctionDeclaration {
    let mut fn_name = String::new();
    let mut arguments = Vec::new();
    let mut is_external = false;
    let mut fn_scope = None;
    let mut return_type = None;
    while let Some(token) = lexer.next() {
        match token {
            Ok(MantisLexerTokens::Word(value)) => {
                if fn_name.is_empty() {
                    fn_name = value;
                } else {
                    return_type = Some(VariableType::BuiltIn(
                        BuiltInType::from_str(&MantisLexerTokens::Word(value)).unwrap(),
                    ))
                    // return_type = Some(
                    //     map_type_to_native(&value).expect(&format!("Invalid Return Type {value}")),
                    // );
                }
            }
            Ok(MantisLexerTokens::BracketOpen) => {
                arguments = parse_fn_declared_arguments(lexer);
            }

            Ok(MantisLexerTokens::Extern) => {
                is_external = true;
            }
            Ok(MantisLexerTokens::SemiColon) => {
                break;
            }

            Ok(MantisLexerTokens::BraceOpen) => {
                if !is_external {
                    fn_scope = Some(parse_scope(lexer, struct_registry));
                    break;
                } else {
                    panic!("External Function can't have function scope");
                }
            }
            _ => log::error!("Unsupported token {:?} {}\n", lexer.span(), lexer.slice()),
        }
    }

    return FunctionDeclaration {
        name: fn_name,
        arguments,
        body: fn_scope,
        return_type,
    };
}

pub fn parse_scope(
    lexer: &mut Lexer<'_, MantisLexerTokens>,
    struct_registry: &StructRegistry,
) -> Vec<Expression> {
    let mut expressions = Vec::<Expression>::new();

    loop {
        let expression = parse_expression(lexer, &struct_registry);
        if matches!(expression, Expression::Nil) {
            break;
        }
        expressions.push(expression);
    }

    expressions
}

pub fn parse_expression(
    lexer: &mut Lexer<'_, MantisLexerTokens>,
    struct_registry: &StructRegistry,
) -> Expression {
    let mut expression = Expression::Nil;
    let mut tokens = Vec::new();

    let mut if_begun = false;
    let mut else_begun = false;
    let mut elseif_begun = false;
    let mut loop_begun = false;
    while let Some(token) = lexer.next() {
        match token {
            Ok(MantisLexerTokens::SemiColon) => {
                match tokens.as_slice() {
                    &[MantisLexerTokens::Break] => {
                        expression = Expression::Break;
                    }
                    &[MantisLexerTokens::Continue] => {
                        expression = Expression::Continue;
                    }
                    _ => {
                        expression = parse_line_expression(tokens);
                    }
                }

                break;
            }
            Ok(MantisLexerTokens::BraceClose) => {
                expression = parse_line_expression(tokens);
                break;
            }
            Ok(MantisLexerTokens::Loop) => {
                loop_begun = true;
            }
            Ok(MantisLexerTokens::If) => {
                if_begun = true;
            }
            Ok(MantisLexerTokens::Else) => {
                else_begun = true;
            }
            Ok(MantisLexerTokens::ElseIf) => {
                elseif_begun = true;
            }
            Ok(MantisLexerTokens::BraceOpen) => {
                let scope = parse_scope(lexer, &struct_registry);
                if loop_begun {
                    expression = Expression::Scope(super::tokens::ScopeType::Loop, scope);
                } else if else_begun {
                    expression = Expression::Scope(super::tokens::ScopeType::Else, scope);
                } else if if_begun {
                    log::info!("if scope with condition {:?}", tokens);
                    let node = Node::parse(&tokens).unwrap();
                    expression = Expression::Scope(super::tokens::ScopeType::If(node), scope);
                } else if elseif_begun {
                    log::info!("if scope with condition {:?}", tokens);
                    let node = Node::parse(&tokens).unwrap();
                    expression = Expression::Scope(super::tokens::ScopeType::ElseIf(node), scope);
                }

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

pub fn parse_let_expression(tokens: &[MantisLexerTokens]) -> Option<Expression> {
    if let (Some(MantisLexerTokens::Word(var_name)), Some(MantisLexerTokens::Assign)) =
        (tokens.get(0), tokens.get(1))
    {
        return Some(Expression::Declare(
            MsVariable::new(var_name, VariableType::BuiltIn(BuiltInType::I64)),
            Node::parse(&tokens[2..])
                .expect(&format!("Node Parse Failed with Tokens {:?}", tokens)),
        ));
    }

    None
}

pub fn parse_line_expression(tokens: Vec<MantisLexerTokens>) -> Expression {
    if let Some(token) = tokens.first() {
        match token {
            MantisLexerTokens::Let => return parse_let_expression(&tokens[1..]).unwrap(),
            MantisLexerTokens::Return => {
                return Expression::Return(Node::parse(&tokens[1..]).unwrap());
            }
            _ => {
                return Expression::Operation(Node::parse(&tokens).unwrap());
            }
        }
    } else {
        return Expression::Nil;
    }
    panic!("Invalid expression not supported");
}

pub fn parse_fn_call_args(tokens: &[MantisLexerTokens]) -> Vec<Node> {
    let mut args = Vec::new();

    for chunk in tokens.split(|x| *x == MantisLexerTokens::Comma) {
        if let Ok(arg) = (Node::parse(chunk)) {
            args.push(arg);
        } else {
            break;
        }
    }

    return args;
}

pub fn parse_fn_declared_arguments(tokens: &mut Lexer<MantisLexerTokens>) -> Vec<MsVariable> {
    let mut variables = Vec::new();
    let mut next_is_type = false;
    let mut next_is_variable = true;

    let mut lexer = tokens;

    while let Some(Ok(token)) = lexer.next() {
        match token {
            MantisLexerTokens::Word(value) => {
                if next_is_variable {
                    let variable = MsVariable::new(value, VariableType::Native(types::I64));
                    variables.push(variable);
                    next_is_variable = false;
                } else if next_is_type {
                    let vtype = BuiltInType::from_str(&MantisLexerTokens::Word(value)).unwrap();
                    variables.last_mut().unwrap().var_type = VariableType::BuiltIn(vtype);
                    next_is_type = false;
                }
            }

            MantisLexerTokens::Colon => {
                next_is_type = true;
            }
            MantisLexerTokens::Comma => {
                next_is_variable = true;
            }

            MantisLexerTokens::BracketClose => {
                break;
            }
            _ => log::error!("Unsupported token {:?}\n", token),
        }
    }

    return variables;
}

pub fn parse_struct_scope(tokens: &mut Lexer<MantisLexerTokens>, registry: &mut StructRegistry) {
    let Some(Ok(MantisLexerTokens::Word(name))) = tokens.next() else {
        panic!("No name after struct declaration");
    };

    let Some(Ok(MantisLexerTokens::BraceOpen)) = tokens.next() else {
        panic!("Struct Declaration, No Brace Open");
    };

    let mut builder = StructMapBuilder::new();
    let mut next_is_variable = true;
    let mut next_is_type = false;

    let mut field: Option<(String, VariableType)> = None;

    while let Some(Ok(token)) = tokens.next() {
        match token {
            MantisLexerTokens::Word(field_name) => {
                if next_is_variable {
                    field = Some((field_name, VariableType::BuiltIn(BuiltInType::I64)));
                    next_is_variable = false;
                } else if next_is_type {
                    let f = field.take().unwrap();
                    if let Ok(t) =
                        BuiltInType::from_str(&MantisLexerTokens::Word(field_name.clone()))
                    {
                        builder.add_field(&f.0, VariableType::BuiltIn(t), &registry);
                    } else {
                        builder.add_field(&f.0, VariableType::Custom(field_name), &registry);
                    }

                    next_is_type = false;
                }
            }
            MantisLexerTokens::Comma => {
                next_is_variable = true;
            }
            MantisLexerTokens::Colon => {
                next_is_type = true;
            }
            MantisLexerTokens::BraceClose => {
                break;
            }

            _ => {
                panic!(
                    "Undefined token in struct declaration {:?} {:?}",
                    token,
                    tokens.slice()
                );
            }
        }
    }

    registry.add_struct(name, builder);
}
