use cranelift_module::FuncId;
use std::{collections::BTreeMap, rc::Rc};

use cranelift::{
    codegen::ir::{condcodes, types, AbiParam, Type},
    frontend::FunctionBuilder,
    prelude::*,
};
use cranelift_module::{DataDescription, FuncOrDataId, Linkage, Module};
use cranelift_object::ObjectModule;
use logos::Logos;

use crate::libc::libc::declare_external_function;

use super::tokenizer::parse_fn_call_args;

#[derive(Logos, Clone, Debug)]
#[logos(skip r"[ \t\n\f]+")]
pub enum Keyword {
    Let,
    Mut,
    Const,
    Fn,
}

#[derive(Logos, Clone, PartialEq, PartialOrd, Debug)]
#[logos(skip r"[ \t\r\n\f]+")]
pub enum MantisLexerTokens {
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

    #[token(".")]
    Dot,

    #[token("==")]
    EqualTo,

    #[token(">")]
    GreaterThan,

    #[token("<")]
    LessThan,

    #[token("!=")]
    NotEqualTo,

    #[regex(r#"\w+"#, |lex| lex.slice().to_owned())]
    Word(String),

    #[regex(r#""-?\d+""#, |lex| lex.slice().parse::<i64>().unwrap())]
    Integer(i64),

    #[regex(r#""-?\d+\.\d+""#, |lex| lex.slice().parse::<f64>().unwrap())]
    Float(f64),

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
pub struct CustomType {
    name: String,
    fields: BTreeMap<String, VariableType>,
}

#[derive(Clone, Debug)]
pub enum VariableType {
    Native(Type),
    Custom(Rc<CustomType>),
    BuiltIn(BuiltInType),
    Constant(ConstantType),
}

#[derive(Clone, Debug)]
pub enum ConstantType {
    BuiltIn(BuiltInType),
    Custom(Rc<CustomType>),
    Native(Type),
}

#[derive(Clone, Copy, Debug)]
pub enum BuiltInType {
    String,
    Function,
}

#[derive(Clone, Debug)]
pub struct MsVariable {
    pub name: String,
    pub var_type: VariableType,
}

impl MsVariable {
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
            var_type: VariableType::Native(types::I64),
        }
    }
    pub fn const_f64() -> Self {
        let val = rand::random::<u32>();
        Self {
            name: format!("fi64_{val}"),
            var_type: VariableType::Native(types::F64),
        }
    }
    pub fn const_bool() -> Self {
        let val = rand::random::<u32>();
        Self {
            name: format!("bool_{val}"),
            var_type: VariableType::Native(types::I8),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ConstLiteral(MsVariable);

#[derive(Clone, Debug)]
pub enum Expression {
    Assign(MsVariable, Node),
    ConstLiteral(MsVariable, String), // MsVariable and Value
    Declare(MsVariable, Node),
    Call(MsVariable, Vec<Node>),
    Cast(MsVariable, MsVariable),
    Operation(Node),
    Return(Node),
    Nil,
}

pub struct CraneliftVariable {
    pub var: Variable,
}

pub struct MsContext {
    local_variables: BTreeMap<String, CraneliftVariable>,
    variable_index: usize,
}

impl MsContext {
    pub fn new(offset: usize) -> Self {
        Self {
            local_variables: BTreeMap::new(),
            variable_index: offset,
        }
    }

    pub fn new_variable(&mut self) -> Variable {
        self.variable_index += 1;
        Variable::new(self.variable_index)
    }
}

impl Expression {
    pub fn translate(
        &self,
        ms_ctx: &mut MsContext,
        fbx: &mut FunctionBuilder<'_>,
        module: &mut ObjectModule,
    ) -> Option<Value> {
        match self {
            Expression::Assign(var, val) => {
                let value = val.translate(ms_ctx, fbx, module);
                let variable = ms_ctx
                    .local_variables
                    .get(&var.name)
                    .expect("Undeclared variable");
                fbx.def_var(variable.var.clone(), value);
            }
            Expression::ConstLiteral(_, _) => todo!(),
            Expression::Declare(var, val) => {
                let value = val.translate(ms_ctx, fbx, module);
                if let Some(variable) = ms_ctx.local_variables.get(&var.name) {
                    fbx.def_var(variable.var.clone(), value);
                } else {
                    if let VariableType::Native(t) = var.var_type {
                        let variable = ms_ctx.new_variable();
                        fbx.declare_var(variable, t);
                        // let value = val.translate(ms_ctx, fbx, module);
                        fbx.def_var(variable, value);
                        ms_ctx
                            .local_variables
                            .insert(var.name.clone(), CraneliftVariable { var: variable });
                    } else {
                        panic!("Unsupported type {:?}, {:?}", var, val);
                    }
                }
            }
            Expression::Call(var, args) => {
                todo!();
            }
            Expression::Cast(_, _) => todo!(),
            Expression::Operation(val) => {
                val.translate(ms_ctx, fbx, module);
            }
            Expression::Return(val) => {
                if let Some(return_type) = fbx.func.signature.returns.first().cloned() {
                    let value = val.translate(ms_ctx, fbx, module);
                    let value = fbx.ins().ireduce(return_type.value_type, value);
                    fbx.ins().return_(&[value]);
                } else {
                    panic!("Function doesn't support return type");
                }
            }
            Expression::Nil => {}
        }
        None
    }
}

#[derive(Clone, Debug)]
pub struct FunctionDeclaration {
    pub name: String,
    pub return_type: Option<VariableType>,
    pub arguments: Vec<MsVariable>,
    pub body: Option<Vec<Expression>>,
}

impl FunctionDeclaration {
    pub(crate) fn declare(
        &self,
        ctx: &mut cranelift::prelude::codegen::Context,
        fbx: &mut cranelift::prelude::FunctionBuilderContext,
        module: &mut ObjectModule,
        offset: usize,
    ) -> FuncId {
        ctx.func.signature.returns.clear();
        ctx.func.signature.params = self
            .arguments
            .iter()
            .map(|x| {
                if let VariableType::Native(t) = x.var_type {
                    AbiParam::new(t)
                } else {
                    panic!("Unsupported custom struct");
                }
            })
            .collect();
        if let Some(VariableType::Native(return_type)) = self.return_type {
            ctx.func.signature.returns.push(AbiParam::new(return_type));
        }

        if let Some(expressions) = &self.body {
            let mut builder = FunctionBuilder::new(&mut ctx.func, fbx);

            let entry_block = builder.create_block();

            builder.append_block_params_for_function_params(entry_block);
            builder.switch_to_block(entry_block);
            let mut ms_ctx = MsContext::new(offset);

            let values = builder.block_params(entry_block).to_vec();
            for (value, variable) in std::iter::zip(values, self.arguments.iter()) {
                let var = ms_ctx.new_variable();
                // variables.insert(variable.name.clone(), value);
                if let VariableType::Native(t) = variable.var_type {
                    builder.try_declare_var(var, t).unwrap();
                    builder.try_def_var(var, value).unwrap();
                    ms_ctx
                        .local_variables
                        .insert(variable.name.clone(), CraneliftVariable { var });
                } else {
                    log::error!("Unsupported variable type {:?}", variable);
                }
            }
            for expression in expressions {
                expression.translate(&mut ms_ctx, &mut builder, module);
            }
            builder.seal_all_blocks();
            builder.finalize();

            let func_id = module
                .declare_function(&self.name, Linkage::Preemptible, &ctx.func.signature)
                .unwrap();

            module.define_function(func_id, ctx);
            module.clear_context(ctx);

            return func_id;
        } else {
            let func_id = declare_external_function(
                &self.name,
                &format!("ext_{}", self.name),
                // &self.name,
                ctx.func.signature.clone(),
                module,
                fbx,
                ctx,
            )
            .unwrap();

            return func_id;
        }
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

    EqualTo,
    GreaterThan,
    NotEqualTo,
    LessThan,

    Call,
}

impl Operator {
    fn try_from(value: MantisLexerTokens) -> Result<Self, ()> {
        Ok(match value {
            MantisLexerTokens::Add => Self::Add,
            MantisLexerTokens::Sub => Self::Sub,
            MantisLexerTokens::Multiply => Self::Multiply,
            MantisLexerTokens::Divide => Self::Divide,
            MantisLexerTokens::Assign => Self::Assign,
            MantisLexerTokens::EqualTo => Self::EqualTo,
            MantisLexerTokens::NotEqualTo => Self::NotEqualTo,
            MantisLexerTokens::GreaterThan => Self::GreaterThan,
            MantisLexerTokens::LessThan => Self::LessThan,

            _ => return Err(()),
        })
    }
}

#[derive(Clone, Debug)]
pub enum Node {
    Variable(MsVariable),
    FuncExpr {
        lhs: MsVariable,
        args: Vec<Node>,
    },
    BinaryExpr {
        op: Operator,
        lhs: Box<Node>,
        rhs: Box<Node>,
    },
}

impl Node {
    pub fn parse(tokens: &[MantisLexerTokens]) -> Result<Self, ()> {
        let mut node = None;

        for i in 0..tokens.len() {
            let token = &tokens[i];

            match token {
                MantisLexerTokens::Add
                | MantisLexerTokens::Sub
                | MantisLexerTokens::Multiply
                | MantisLexerTokens::Divide
                | MantisLexerTokens::Assign => {
                    node = Some(Node::BinaryExpr {
                        op: Operator::try_from(token.clone())?,
                        lhs: Box::new(Self::parse(&tokens[0..i])?),
                        rhs: Box::new(Self::parse(&tokens[i + 1..])?),
                    });

                    break;
                }

                MantisLexerTokens::Word(var_name) => {
                    if var_name.chars().all(|x| x.is_numeric()) {
                        node = Some(Node::Variable(MsVariable::new(
                            var_name,
                            VariableType::Constant((ConstantType::Native(types::I64))),
                        )));
                    } else {
                        node = Some(Node::Variable(MsVariable::new(
                            var_name,
                            VariableType::Native(types::I64),
                        )));
                    }
                }

                MantisLexerTokens::Integer(value) => {
                    node = Some(Node::Variable(MsVariable::new(
                        value.to_string(),
                        VariableType::Constant((ConstantType::Native(types::I64))),
                    )))
                }

                MantisLexerTokens::String(value) => {
                    node = Some(Node::Variable(MsVariable::new(
                        value.to_string(),
                        VariableType::Constant((ConstantType::BuiltIn(BuiltInType::String))),
                    )))
                }

                MantisLexerTokens::BracketOpen => {
                    if let Some(Node::Variable(mut var)) = node.take() {
                        var.var_type = VariableType::BuiltIn(BuiltInType::Function);
                        let args = parse_fn_call_args(&tokens[i..]);
                        node = Some(Node::FuncExpr { lhs: var, args });
                        break;
                    }
                }
                MantisLexerTokens::BracketClose => {}

                _ => {
                    log::error!("unknown token {:?}", tokens);
                    return Err(());
                }
            }
        }

        node.ok_or(())
    }

    pub fn translate(
        &self,
        ms_ctx: &mut MsContext,
        fbx: &mut FunctionBuilder<'_>,
        module: &mut ObjectModule,
    ) -> Value {
        let mut local_variables = &mut ms_ctx.local_variables;
        match self {
            Node::Variable(var) => {
                if let Some(val) = local_variables.get(&var.name) {
                    return fbx.use_var(val.var);
                } else {
                    if let VariableType::Constant(vtype) = &var.var_type {
                        if let ConstantType::Native(t) = vtype {
                            let val = fbx
                                .ins()
                                .iconst(t.clone(), var.name.parse::<i64>().unwrap());
                            return val;
                        } else if let ConstantType::BuiltIn(BuiltInType::String) = vtype {
                            let data_id = if let Some(FuncOrDataId::Data(data_id)) =
                                module.get_name(&var.name)
                            {
                                log::warn!(
                                    "already assigned global data is being used {}",
                                    var.name
                                );
                                data_id
                            } else {
                                let data_id = module
                                    .declare_data(
                                        &var.name,
                                        cranelift_module::Linkage::Local,
                                        false,
                                        false,
                                    )
                                    .unwrap();
                                let mut const_str = String::with_capacity(var.name.len() + 1);
                                const_str.push_str(&var.name);
                                const_str.push('\0');

                                let mut data_description = DataDescription::new();
                                data_description.define(const_str.as_bytes().into());
                                module.define_data(data_id, &data_description).unwrap();

                                data_id
                            };

                            let string_ptr = module.declare_data_in_func(data_id, fbx.func);
                            return fbx.ins().global_value(types::I64, string_ptr);
                        }
                    } else {
                        panic!("Undeclared Variable {:?}", var);
                    }
                }
            }
            Node::FuncExpr { lhs, args } => {
                if let Some(FuncOrDataId::Func(fn_id)) = module.get_name(&lhs.name) {
                    let fn_ref = module.declare_func_in_func(fn_id, fbx.func);

                    let fn_args: Vec<Value> = args
                        .iter()
                        .map(|arg| arg.translate(ms_ctx, fbx, module))
                        .collect();

                    let inst = fbx.ins().call(fn_ref, &fn_args);
                    let results = fbx.inst_results(inst);
                    return results[0];
                } else {
                    panic!("Undeclared Function {:?}", lhs.name);
                }
            }
            Node::BinaryExpr { op, lhs, rhs } => match op {
                Operator::Add => {
                    let lhs_val = lhs.translate(ms_ctx, fbx, module);
                    let rhs_val = rhs.translate(ms_ctx, fbx, module);
                    return fbx.ins().iadd(lhs_val, rhs_val);
                }
                Operator::Assign => {
                    let rhs_val = rhs.translate(ms_ctx, fbx, module);
                    if let Node::Variable(var) = lhs.as_ref() {
                        if let Some(variable) = ms_ctx.local_variables.get(&var.name) {
                            fbx.def_var(variable.var, rhs_val);
                            return rhs_val;
                        } else {
                            panic!("undeclared variable {:?}", var.name);
                        }
                    } else {
                        panic!("LHS can't be an expression when assigning");
                    }
                }
                Operator::Sub => {
                    let lhs_val = lhs.translate(ms_ctx, fbx, module);
                    let rhs_val = rhs.translate(ms_ctx, fbx, module);
                    return fbx.ins().isub(lhs_val, rhs_val);
                }
                Operator::Divide => {
                    let lhs_val = lhs.translate(ms_ctx, fbx, module);
                    let rhs_val = rhs.translate(ms_ctx, fbx, module);
                    return fbx.ins().sdiv(lhs_val, rhs_val);
                }
                Operator::Multiply => {
                    let lhs_val = lhs.translate(ms_ctx, fbx, module);
                    let rhs_val = rhs.translate(ms_ctx, fbx, module);
                    return fbx.ins().imul(lhs_val, rhs_val);
                }

                Operator::EqualTo => {
                    let lhs_val = lhs.translate(ms_ctx, fbx, module);
                    let rhs_val = rhs.translate(ms_ctx, fbx, module);
                    return fbx.ins().icmp(condcodes::IntCC::Equal, lhs_val, rhs_val);
                }
                Operator::NotEqualTo => {
                    let lhs_val = lhs.translate(ms_ctx, fbx, module);
                    let rhs_val = rhs.translate(ms_ctx, fbx, module);
                    return fbx.ins().icmp(condcodes::IntCC::NotEqual, lhs_val, rhs_val);
                }
                Operator::GreaterThan => {
                    let lhs_val = lhs.translate(ms_ctx, fbx, module);
                    let rhs_val = rhs.translate(ms_ctx, fbx, module);
                    return fbx
                        .ins()
                        .icmp(condcodes::IntCC::SignedGreaterThan, lhs_val, rhs_val);
                }
                Operator::LessThan => {
                    let lhs_val = lhs.translate(ms_ctx, fbx, module);
                    let rhs_val = rhs.translate(ms_ctx, fbx, module);
                    return fbx
                        .ins()
                        .icmp(condcodes::IntCC::SignedLessThan, lhs_val, rhs_val);
                }

                _ => todo!(),
            },
        };

        todo!()
    }
}
