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
    // Native(Type),
}

#[derive(Clone, Copy, Debug)]
pub enum BuiltInType {
    Bool,
    Void,
    String,
    Function,
    I64,
    I32,
    F32,
    F64,
    U32,
    U64,
    I128,
    U128,
    I8,
    U8,
    I16,
    U16,
    Pointer,
}

impl BuiltInType {
    pub fn to_cranelift_type(&self) -> Option<Type> {
        Some(match self {
            BuiltInType::Bool | BuiltInType::I8 | BuiltInType::U8 => types::I8,
            BuiltInType::I16 | BuiltInType::U16 => types::I16,
            BuiltInType::I32 | BuiltInType::U32 => types::I32,
            BuiltInType::Pointer | BuiltInType::I64 | BuiltInType::U64 => types::I64,
            BuiltInType::I128 | BuiltInType::U128 => types::I128,
            BuiltInType::F32 => types::F32,
            BuiltInType::F64 => types::F64,
            _ => {
                panic!("Unhandled Type {:?}", self);
                return None;
            }
        })
    }

    pub fn size(&self) -> isize {
        match self {
            BuiltInType::Bool | BuiltInType::I8 | BuiltInType::U8 => 1,
            BuiltInType::I16 | BuiltInType::U16 => 2,
            BuiltInType::I32 | BuiltInType::U32 | BuiltInType::F32 => 4,
            BuiltInType::Pointer | BuiltInType::I64 | BuiltInType::U64 | BuiltInType::F64 => 8,
            BuiltInType::I128 | BuiltInType::U128 => 16,
            _ => {
                panic!("unhandled type {:?}", self);
                return 0;
            }
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            Self::F32 | Self::F64 => true,
            _ => false,
        }
    }

    pub fn is_signed_int(&self) -> bool {
        match self {
            Self::I128 | Self::I64 | Self::I32 | Self::I16 | Self::I8 => true,
            _ => false,
        }
    }

    pub fn is_unsigned_int(&self) -> bool {
        match self {
            Self::U128 | Self::U64 | Self::U32 | Self::U16 | Self::U8 => true,
            _ => false,
        }
    }

    fn from_str(i: &MantisLexerTokens) -> Result<Self, ()> {
        if let MantisLexerTokens::Word(value) = i {
            let t = match value.as_str() {
                "i8" => BuiltInType::I8,
                "i16" => BuiltInType::I16,
                "i32" => BuiltInType::I32,
                "i64" => BuiltInType::I64,
                "i128" => BuiltInType::I128,
                "u8" => BuiltInType::U8,
                "u16" => BuiltInType::U16,
                "u32" => BuiltInType::U32,
                "u64" => BuiltInType::U64,
                "u128" => BuiltInType::U128,
                "f32" => BuiltInType::F32,
                "f64" => BuiltInType::F64,
                "pointer" => BuiltInType::Pointer,
                _ => return Err(()),
            };

            return Ok(t);
        }

        Err(())
    }

    fn from_var_type(var: VariableType) -> Result<Self, ()> {
        match var {
            VariableType::Native(n) => Self::from_native(n),
            VariableType::Custom(s) => todo!(),
            VariableType::BuiltIn(s) => Ok(s),
            VariableType::Constant(c) => Self::from_constant(c),
        }
    }

    fn from_constant(c: ConstantType) -> Result<Self, ()> {
        match c {
            ConstantType::BuiltIn(s) => Ok(s),
            ConstantType::Custom(_) => todo!(),
        }
    }

    fn from_native(t: Type) -> Result<Self, ()> {
        return Ok(match t {
            types::I8 => BuiltInType::I8,
            types::I16 => BuiltInType::I16,
            types::I32 => BuiltInType::I32,
            types::I64 => BuiltInType::I64,
            types::I128 => BuiltInType::I128,
            types::F32 => BuiltInType::F32,
            types::F64 => BuiltInType::F64,

            _ => return Err(()),
        });
    }
}

impl TryFrom<VariableType> for BuiltInType {
    type Error = ();

    fn try_from(value: VariableType) -> Result<Self, Self::Error> {
        Self::from_var_type(value)
    }
}

#[derive(Clone, Debug)]
pub struct MsVariable {
    pub name: String,
    pub var_type: VariableType,
}

pub struct MsValue {
    pub var_type: BuiltInType,
    pub value: Value,
}

impl MsValue {
    pub fn new(var_type: BuiltInType, value: Value) -> Self {
        Self { var_type, value }
    }
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
    pub ms_var: MsVariable,
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
                fbx.def_var(variable.var.clone(), value.value);
            }
            Expression::ConstLiteral(_, _) => todo!(),
            Expression::Declare(var, val) => {
                let value = val.translate(ms_ctx, fbx, module);
                if let Some(variable) = ms_ctx.local_variables.get(&var.name) {
                    fbx.def_var(variable.var.clone(), value.value);
                } else {
                    if let VariableType::Native(t) = var.var_type {
                        let variable = ms_ctx.new_variable();
                        fbx.declare_var(variable, t);
                        // let value = val.translate(ms_ctx, fbx, module);
                        fbx.def_var(variable, value.value);
                        ms_ctx.local_variables.insert(
                            var.name.clone(),
                            CraneliftVariable {
                                var: variable,
                                ms_var: var.clone(),
                            },
                        );
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
                    let mut value = val.translate(ms_ctx, fbx, module).value;
                    // if return_type.value_type == types::I32 {
                    //     value = fbx.ins().ireduce(return_type.value_type, value);
                    // }
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
                    ms_ctx.local_variables.insert(
                        variable.name.clone(),
                        CraneliftVariable {
                            var,
                            ms_var: variable.clone(),
                        },
                    );
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

            log::info!("Function Declared {} {}", self.name, func_id);

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

    CastExpr {
        lhs: Box<Node>,
        cast_to: BuiltInType,
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
                MantisLexerTokens::As => {
                    node = Some(Node::CastExpr {
                        lhs: Box::new(Self::parse(&tokens[0..i])?),
                        cast_to: BuiltInType::from_str(&tokens[i + 1])?,
                    });

                    break;
                }

                MantisLexerTokens::Word(var_name) => {
                    if var_name.chars().all(|x| x.is_numeric()) {
                        node = Some(Node::Variable(MsVariable::new(
                            var_name,
                            VariableType::Constant((ConstantType::BuiltIn(BuiltInType::I64))),
                        )));
                    } else {
                        node = Some(Node::Variable(MsVariable::new(
                            var_name,
                            VariableType::BuiltIn(BuiltInType::I64),
                        )));
                    }
                }

                MantisLexerTokens::Integer(value) => {
                    node = Some(Node::Variable(MsVariable::new(
                        value.to_string(),
                        VariableType::Constant((ConstantType::BuiltIn(BuiltInType::I64))),
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
                MantisLexerTokens::BracketClose => {
                    // if let Some(Node::Variable(mut var)) = node.take() {
                    //     var.var_type = VariableType::BuiltIn(BuiltInType::Function);
                    //     let args = parse_fn_call_args(&tokens[0..i - 1]);
                    //     node = Some(Node::FuncExpr { lhs: var, args });
                    //     break;
                    // }
                }

                _ => {
                    log::error!("unknown token {:?}", tokens);
                    return Err(());
                }
            }
        }

        match node {
            Some(n) => Ok(n),
            None => {
                log::info!("no processing of {:?}", tokens);
                Err(())
            }
        }
    }

    pub fn translate(
        &self,
        ms_ctx: &mut MsContext,
        fbx: &mut FunctionBuilder<'_>,
        module: &mut ObjectModule,
    ) -> MsValue {
        let mut local_variables = &mut ms_ctx.local_variables;
        match self {
            Node::Variable(var) => {
                if let Some(val) = local_variables.get(&var.name) {
                    return MsValue::new(
                        val.ms_var.var_type.clone().try_into().unwrap(),
                        fbx.use_var(val.var),
                    );
                } else {
                    if let VariableType::Constant(vtype) = &var.var_type {
                        if let ConstantType::BuiltIn(BuiltInType::String) = vtype {
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

                                log::info!("Declared global string {} {}", data_id, var.name);
                                data_id
                            };

                            let string_ptr = module.declare_data_in_func(data_id, fbx.func);
                            let val = fbx.ins().global_value(types::I64, string_ptr);

                            return MsValue::new(BuiltInType::String, val);
                        } else if let ConstantType::BuiltIn(t) = vtype {
                            let ty = t.to_cranelift_type().unwrap();
                            let val = match t {
                                BuiltInType::F32 => {
                                    fbx.ins().f32const(var.name.parse::<f32>().unwrap())
                                }
                                BuiltInType::F64 => {
                                    fbx.ins().f64const(var.name.parse::<f64>().unwrap())
                                }
                                BuiltInType::I8 | BuiltInType::U8 => {
                                    fbx.ins().iconst(ty, var.name.parse::<i64>().unwrap())
                                }
                                BuiltInType::I16 | BuiltInType::U16 => {
                                    fbx.ins().iconst(ty, var.name.parse::<i64>().unwrap())
                                }
                                BuiltInType::I32 | BuiltInType::U32 => {
                                    fbx.ins().iconst(ty, var.name.parse::<i64>().unwrap())
                                }
                                BuiltInType::I64 | BuiltInType::U64 => {
                                    fbx.ins().iconst(ty, var.name.parse::<i64>().unwrap())
                                }
                                _ => todo!(),
                            };

                            // let val = fbx.ins().iconst(
                            //     t.to_cranelift_type().unwrap(),
                            //     var.name.parse::<i64>().unwrap(),
                            // );
                            return MsValue::new(*t, val);
                        }
                    } else {
                        panic!("Undeclared Variable {:?}", var);
                    }
                }
            }
            Node::FuncExpr { lhs, args } => {
                if let Some(FuncOrDataId::Func(fn_id)) = module.get_name(&lhs.name) {
                    let fn_ref = module.declare_func_in_func(fn_id, fbx.func);

                    log::info!("Calling {} with args {:?}", lhs.name, args);
                    let fn_args: Vec<Value> = args
                        .iter()
                        .map(|arg| arg.translate(ms_ctx, fbx, module).value)
                        .collect();

                    let inst = fbx.ins().call(fn_ref, &fn_args);
                    let results = fbx.inst_results(inst);
                    let value = results[0];
                    let var_type = fbx.func.dfg.value_type(value);

                    return MsValue::new(BuiltInType::from_native(var_type).unwrap(), value);
                } else {
                    panic!("Undeclared Function {:?}", lhs.name);
                }
            }
            Node::BinaryExpr { op, lhs, rhs } => match op {
                Operator::Assign => {
                    let rhs_val = rhs.translate(ms_ctx, fbx, module);
                    if let Node::Variable(var) = lhs.as_ref() {
                        if let Some(variable) = ms_ctx.local_variables.get(&var.name) {
                            fbx.def_var(variable.var, rhs_val.value);
                            return rhs_val;
                        } else {
                            panic!("undeclared variable {:?}", var.name);
                        }
                    } else {
                        panic!("LHS can't be an expression when assigning");
                    }
                }
                Operator::Add => {
                    let lhs_val = lhs.translate(ms_ctx, fbx, module);
                    let rhs_val = rhs.translate(ms_ctx, fbx, module);
                    return MsValue::new(
                        lhs_val.var_type,
                        fbx.ins().iadd(lhs_val.value, rhs_val.value),
                    );
                }
                Operator::Sub => {
                    let lhs_val = lhs.translate(ms_ctx, fbx, module);
                    let rhs_val = rhs.translate(ms_ctx, fbx, module);
                    return MsValue::new(
                        lhs_val.var_type,
                        fbx.ins().isub(lhs_val.value, rhs_val.value),
                    );
                }
                Operator::Divide => {
                    let lhs_val = lhs.translate(ms_ctx, fbx, module);
                    let rhs_val = rhs.translate(ms_ctx, fbx, module);
                    return MsValue::new(
                        lhs_val.var_type,
                        fbx.ins().sdiv(lhs_val.value, rhs_val.value),
                    );
                }
                Operator::Multiply => {
                    let lhs_val = lhs.translate(ms_ctx, fbx, module);
                    let rhs_val = rhs.translate(ms_ctx, fbx, module);
                    return MsValue::new(
                        lhs_val.var_type,
                        fbx.ins().imul(lhs_val.value, rhs_val.value),
                    );
                }

                Operator::EqualTo => {
                    let lhs_val = lhs.translate(ms_ctx, fbx, module);
                    let rhs_val = rhs.translate(ms_ctx, fbx, module);
                    return MsValue::new(
                        BuiltInType::Bool,
                        fbx.ins()
                            .icmp(condcodes::IntCC::Equal, lhs_val.value, rhs_val.value),
                    );
                }
                Operator::NotEqualTo => {
                    let lhs_val = lhs.translate(ms_ctx, fbx, module);
                    let rhs_val = rhs.translate(ms_ctx, fbx, module);
                    return MsValue::new(
                        BuiltInType::Bool,
                        fbx.ins()
                            .icmp(condcodes::IntCC::NotEqual, lhs_val.value, rhs_val.value),
                    );
                }
                Operator::GreaterThan => {
                    let lhs_val = lhs.translate(ms_ctx, fbx, module);
                    let rhs_val = rhs.translate(ms_ctx, fbx, module);
                    return MsValue::new(
                        BuiltInType::Bool,
                        fbx.ins().icmp(
                            condcodes::IntCC::SignedGreaterThan,
                            lhs_val.value,
                            rhs_val.value,
                        ),
                    );
                }
                Operator::LessThan => {
                    let lhs_val = lhs.translate(ms_ctx, fbx, module);
                    let rhs_val = rhs.translate(ms_ctx, fbx, module);
                    return MsValue::new(
                        BuiltInType::Bool,
                        fbx.ins().icmp(
                            condcodes::IntCC::SignedLessThan,
                            lhs_val.value,
                            rhs_val.value,
                        ),
                    );
                }

                _ => todo!(),
            },
            Node::CastExpr { lhs, cast_to } => {
                if let Some(t) = cast_to.to_cranelift_type() {
                    let value = lhs.translate(ms_ctx, fbx, module);
                    let diff = cast_to.size() - value.var_type.size();
                    if diff == 0 {
                        if value.var_type.is_float() {
                            if cast_to.is_unsigned_int() {
                                return MsValue::new(
                                    *cast_to,
                                    fbx.ins().fcvt_to_uint(
                                        cast_to.to_cranelift_type().unwrap(),
                                        value.value,
                                    ),
                                );
                            } else if cast_to.is_signed_int() {
                                return MsValue::new(
                                    *cast_to,
                                    fbx.ins().fcvt_to_sint(
                                        cast_to.to_cranelift_type().unwrap(),
                                        value.value,
                                    ),
                                );
                            } else {
                                panic!("Unhandled case");
                            }
                        } else if value.var_type.is_signed_int() {
                            return MsValue::new(
                                *cast_to,
                                fbx.ins().fcvt_from_sint(
                                    cast_to.to_cranelift_type().unwrap(),
                                    value.value,
                                ),
                            );
                        } else if value.var_type.is_unsigned_int() {
                            return MsValue::new(
                                *cast_to,
                                fbx.ins().fcvt_from_uint(
                                    cast_to.to_cranelift_type().unwrap(),
                                    value.value,
                                ),
                            );
                        }
                    } else if diff > 0 {
                        if cast_to.is_signed_int() && value.var_type.is_signed_int() {
                            return MsValue::new(
                                *cast_to,
                                fbx.ins()
                                    .sextend(cast_to.to_cranelift_type().unwrap(), value.value),
                            );
                        } else if cast_to.is_unsigned_int() && value.var_type.is_unsigned_int() {
                            return MsValue::new(
                                *cast_to,
                                fbx.ins()
                                    .uextend(cast_to.to_cranelift_type().unwrap(), value.value),
                            );
                        }
                    } else {
                        return MsValue::new(
                            *cast_to,
                            fbx.ins()
                                .ireduce(cast_to.to_cranelift_type().unwrap(), value.value),
                        );
                    }
                }
            }
        };

        todo!()
    }
}

pub fn cast_instruction(cast_from: BuiltInType, value: Value, cast_to: BuiltInType) {}
