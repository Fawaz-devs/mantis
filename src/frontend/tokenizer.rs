use std::{ops::Add, rc::Rc};

use cranelift::codegen::{
    gimli::ReaderOffset,
    ir::{types, Type},
};
use logos::{Lexer, Logos};
use mantis_tokens::MantisLexerTokens;

use crate::{
    frontend::tokens::{MsExpression, MsNode},
    registries::{
        functions::FunctionType,
        structs::MsStructType,
        types::{MsNativeType, MsType, MsTypeRegistry},
        MsRegistry,
    },
};

use super::tokens::{
    BuiltInType, ConstLiteral, Expression, FunctionDeclaration, MsFunctionDeclaration,
    MsTypedVariable, MsVariable, Node, StructMapBuilder, StructRegistry, Token, VariableType,
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

pub fn read_to_tokens(input: String) -> (Vec<FunctionDeclaration>, MsTypeRegistry) {
    let mut lexer = MantisLexerTokens::lexer(&input);

    let mut functions = Vec::new();

    let mut type_registry = MsTypeRegistry::default();

    while let Some(token) = lexer.next() {
        match token {
            Ok(MantisLexerTokens::FunctionDecl) => {
                let decl = parse_fn_declaration(&mut lexer, &type_registry);
                functions.push(decl);
            }

            Ok(MantisLexerTokens::Struct) => {
                parse_struct_scope(&mut lexer, &mut type_registry);
            }

            Ok(MantisLexerTokens::TypeDecl) => {
                parse_type_decl(&mut lexer, &mut type_registry);
            }

            _ => log::error!("Unsupported token {:?} {}\n", lexer.span(), lexer.slice()),
        };
    }

    return (functions, type_registry);
}

pub fn collect_functions(input: String) -> (Vec<MsFunctionDeclaration>, MsTypeRegistry) {
    let mut lexer = MantisLexerTokens::lexer(&input);

    let mut functions = Vec::new();

    let mut type_registry = MsTypeRegistry::default();

    while let Some(token) = lexer.next() {
        match token {
            Ok(MantisLexerTokens::FunctionDecl) => {
                let decl = parse_ms_fn_declaration(&mut lexer, &type_registry);
                functions.push(decl);
            }

            Ok(MantisLexerTokens::Struct) => {
                parse_struct_scope(&mut lexer, &mut type_registry);
            }

            Ok(MantisLexerTokens::TypeDecl) => {
                parse_type_decl(&mut lexer, &mut type_registry);
            }

            _ => log::error!("Unsupported token {:?} {}\n", lexer.span(), lexer.slice()),
        };
    }

    return (functions, type_registry);
}

fn parse_type(lexer: &mut Lexer<'_, MantisLexerTokens>) {
    match lexer.next().unwrap().unwrap() {
        MantisLexerTokens::Word(word) => {}
        MantisLexerTokens::SqBracketOpen => {}
        MantisLexerTokens::SqBracketClose => {}
        MantisLexerTokens::Comma => {}
        _ => {}
    };
}

fn parse_type_decl(lexer: &mut Lexer<'_, MantisLexerTokens>, type_registry: &mut MsTypeRegistry) {
    let type_name = lexer.next().unwrap();
    if let Some(Ok(MantisLexerTokens::Assign)) = lexer.next() {
    } else {
        panic!("Expected type NewType = ExistingType ; missing = operator");
    }
}

pub fn parse_fn_declaration(
    lexer: &mut Lexer<'_, MantisLexerTokens>,
    type_registry: &MsTypeRegistry,
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
                    fn_scope = Some(parse_scope(lexer, type_registry));
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
pub fn parse_ms_fn_declaration(
    lexer: &mut Lexer<'_, MantisLexerTokens>,
    type_registry: &MsTypeRegistry,
) -> MsFunctionDeclaration {
    let mut fn_name = String::new();
    let mut arguments = Vec::new();
    let mut fn_scope = Vec::<MsExpression>::new();
    let mut return_type = MsType::Native(MsNativeType::Void);
    let mut fn_type = FunctionType::Public;
    while let Some(token) = lexer.next() {
        match token {
            Ok(MantisLexerTokens::Word(value)) => {
                if fn_name.is_empty() {
                    fn_name = value;
                    log::info!("Found a Fn Declaration {}", fn_name);
                } else {
                    log::info!("Getting Return Type for {}", fn_name);
                    return_type = type_registry
                        .get_registry()
                        .get(value.as_str())
                        .expect(format_lexer_state(lexer).as_str())
                        .clone();
                    // return_type = Some(VariableType::BuiltIn(
                    //     BuiltInType::from_str(&MantisLexerTokens::Word(value)).unwrap(),
                    // ))
                }
            }
            Ok(MantisLexerTokens::BracketOpen) => {
                arguments = parse_fn_ms_declared_arguments(lexer, type_registry);
                log::info!("Got Arguments for {} {:?}", fn_name, arguments);
            }

            Ok(MantisLexerTokens::Extern) => {
                fn_type = FunctionType::Extern;
            }
            Ok(MantisLexerTokens::SemiColon) => {
                break;
            }

            Ok(MantisLexerTokens::BraceOpen) => {
                fn_scope = parse_ms_scope(lexer, type_registry);
                break;
                // if !is_external {
                //     break;
                // } else {
                //     panic!("External Function can't have function scope");
                // }
            }
            _ => log::error!("Unsupported token {:?} {}\n", lexer.span(), lexer.slice()),
        }
    }

    return MsFunctionDeclaration {
        name: fn_name,
        arguments,
        body: fn_scope,
        return_type,
        fn_type,
    };
}
pub fn parse_fn_ms_declaration(
    lexer: &mut Lexer<'_, MantisLexerTokens>,
    type_registry: &MsTypeRegistry,
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
                    fn_scope = Some(parse_scope(lexer, type_registry));
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
    type_registry: &MsTypeRegistry,
) -> Vec<Expression> {
    let mut expressions = Vec::<Expression>::new();

    loop {
        let expression = parse_expression(lexer, &type_registry);
        if matches!(expression, Expression::Nil) {
            break;
        }
        expressions.push(expression);
    }

    expressions
}

pub fn parse_ms_scope(
    lexer: &mut Lexer<'_, MantisLexerTokens>,
    type_registry: &MsTypeRegistry,
) -> Vec<MsExpression> {
    let mut expressions = Vec::<MsExpression>::new();

    loop {
        let size = parse_ms_expression_and_append(lexer, &type_registry, &mut expressions);
        if size == 0 {
            break;
        }

        if let Some(&MsExpression::Operation(MsNode::None)) = expressions.last() {
            break;
        }
    }

    expressions
}

pub fn parse_expression(
    lexer: &mut Lexer<'_, MantisLexerTokens>,
    type_registry: &MsTypeRegistry,
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
                let scope = parse_scope(lexer, &type_registry);
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

pub fn parse_ms_expression_and_append(
    lexer: &mut Lexer<'_, MantisLexerTokens>,
    type_registry: &MsTypeRegistry,
    output: &mut Vec<MsExpression>,
) -> usize {
    let mut size = 1;
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
                        output.push(MsExpression::Break);
                        size = 1;
                    }
                    &[MantisLexerTokens::Continue] => {
                        output.push(MsExpression::Continue);
                        size = 1;
                    }
                    _ => {
                        size = parse_line_and_append(tokens, type_registry, output);
                    }
                }

                break;
            }
            Ok(MantisLexerTokens::BraceClose) => {
                size = parse_line_and_append(tokens, type_registry, output);
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
                let scope = parse_ms_scope(lexer, &type_registry);
                if loop_begun {
                    output.push(MsExpression::Scope(super::tokens::ScopeType::Loop, scope));
                    size = 1;
                } else if else_begun {
                    output.push(MsExpression::Scope(super::tokens::ScopeType::Else, scope));
                    size = 1;
                } else if if_begun {
                    log::info!("if scope with condition {:?}", tokens);
                    let node = Node::parse(&tokens).unwrap();
                    output.push(MsExpression::Scope(
                        super::tokens::ScopeType::If(node),
                        scope,
                    ));
                    size = 1;
                } else if elseif_begun {
                    log::info!("if scope with condition {:?}", tokens);
                    let node = Node::parse(&tokens).unwrap();
                    output.push(MsExpression::Scope(
                        super::tokens::ScopeType::ElseIf(node),
                        scope,
                    ));
                    size = 1;
                }

                break;
            }
            Ok(token) => {
                tokens.push(token);
            }
            _ => panic!("Unsupported token {:?}, {}\n", lexer.span(), lexer.slice()),
        }
    }

    log::info!("parsing expressions, adding 0 expressions",);

    size
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
    } else if let (
        Some(MantisLexerTokens::Word(var_name)),
        Some(MantisLexerTokens::Colon),
        Some(MantisLexerTokens::Word(type_name)),
        Some(MantisLexerTokens::Assign),
    ) = (tokens.get(0), tokens.get(1), tokens.get(2), tokens.get(3))
    {
        return Some(Expression::Declare(
            MsVariable::new(var_name, VariableType::BuiltIn(BuiltInType::I64)),
            Node::parse(&tokens[4..])
                .expect(&format!("Node Parse Failed with Tokens {:?}", tokens)),
        ));
    }
    None
}

pub fn parse_let_to_ms_expression(
    tokens: &[MantisLexerTokens],
    type_registry: &MsTypeRegistry,
) -> Option<(MsExpression, MsExpression)> {
    if let (Some(MantisLexerTokens::Word(var_name)), Some(MantisLexerTokens::Assign)) =
        (tokens.get(0), tokens.get(1))
    {
        return Some((
            MsExpression::Declare(var_name.clone(), MsType::Native(MsNativeType::Void)),
            MsExpression::Operation(MsNode::parse_expression(&tokens[0..]).unwrap()),
        ));
    } else if let (
        Some(MantisLexerTokens::Word(var_name)),
        Some(MantisLexerTokens::Colon),
        Some(MantisLexerTokens::Word(type_name)),
        Some(MantisLexerTokens::Assign),
    ) = (tokens.get(0), tokens.get(1), tokens.get(2), tokens.get(3))
    {
        let ty = type_registry
            .get_registry()
            .get(type_name)
            .expect("Invalid type name");
        let let_expr = MsExpression::Declare(var_name.clone(), ty.clone());

        let rhs = MsNode::parse_expression(&tokens[4..]).unwrap();
        let assign_expr = MsExpression::Operation(MsNode::Binary(
            mantis_expression::node::BinaryOperation::Assign,
            Box::new(MsNode::Var(tokens[0].clone())),
            Box::new(rhs),
        ));

        return Some((let_expr, assign_expr));
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

// returns how many expressions were appended
pub fn parse_line_and_append(
    tokens: Vec<MantisLexerTokens>,
    type_registry: &MsTypeRegistry,

    output: &mut Vec<MsExpression>,
) -> usize {
    if let Some(token) = tokens.first() {
        match token {
            MantisLexerTokens::Let => {
                let (decl, ass) = parse_let_to_ms_expression(&tokens[1..], type_registry).unwrap();
                output.push(decl);
                output.push(ass);

                return 2;
            }
            MantisLexerTokens::Return => {
                let ret = MsExpression::Return(MsNode::parse_expression(&tokens[1..]).unwrap());
                output.push(ret);
                return 1;
            }
            _ => {
                let op = MsExpression::Operation(MsNode::parse_expression(&tokens).unwrap());
                output.push(op);
                return 1;
            }
        }
    } else {
        return 0;
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
pub fn parse_fn_ms_declared_arguments(
    tokens: &mut Lexer<MantisLexerTokens>,
    type_registry: &MsTypeRegistry,
) -> Vec<MsTypedVariable> {
    let mut variables = Vec::new();
    let mut next_is_type = false;
    let mut next_is_variable = true;

    let mut lexer = tokens;

    while let Some(Ok(token)) = lexer.next() {
        match token {
            MantisLexerTokens::Word(value) => {
                if next_is_variable {
                    let variable = MsTypedVariable {
                        name: value,
                        var_type: MsType::Native(MsNativeType::Void),
                    };

                    // let variable = MsVariable::new(value, VariableType::Native(types::I64));
                    variables.push(variable);
                    next_is_variable = false;
                } else if next_is_type {
                    // let vtype = BuiltInType::from_str(&MantisLexerTokens::Word(value)).unwrap();
                    variables.last_mut().unwrap().var_type = type_registry
                        .get_registry()
                        .get(value.as_str())
                        .expect("Undefined type")
                        .clone();
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

pub fn parse_struct_scope(tokens: &mut Lexer<MantisLexerTokens>, registry: &mut MsTypeRegistry) {
    let Some(Ok(MantisLexerTokens::Word(name))) = tokens.next() else {
        panic!("No name after struct declaration");
    };

    let Some(Ok(MantisLexerTokens::BraceOpen)) = tokens.next() else {
        panic!("Struct Declaration, No Brace Open");
    };

    // let mut builder = StructMapBuilder::new();
    let mut ms_struct = MsStructType::default();
    let mut next_is_variable = true;
    let mut next_is_type = false;

    let mut field: Option<(String, MsType)> = None;

    while let Some(Ok(token)) = tokens.next() {
        match token {
            MantisLexerTokens::Word(field_name) => {
                if next_is_variable {
                    // field = Some((field_name, VariableType::BuiltIn(BuiltInType::I64)));
                    field = Some((field_name, MsType::Native(MsNativeType::Void)));
                    next_is_variable = false;
                } else if next_is_type {
                    let f = field.take().unwrap();
                    let ty = registry
                        .get_registry()
                        .get(field_name.as_str())
                        .expect("Undefined Type");
                    ms_struct.add_field(f.0, ty.clone());

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

    // registry.add_struct(name, builder);
    registry
        .get_registry_mut()
        .insert(name, MsType::Struct(Rc::new(ms_struct)));
}

pub fn format_lexer_state(lexer: &Lexer<MantisLexerTokens>) -> String {
    let range = lexer.span();
    let source = lexer.source();

    let prefix = &source[(range.start.checked_sub(30).unwrap_or(0))..range.start];
    let suffix = &source[range.end..range.end.add(30).min(source.len())];
    format!("{prefix}\n>{}\n<{suffix}", lexer.slice())
}
