use crate::utils::{rc_str::RcStr, rc_vec::RcVec};

pub struct Program {
    pub types: RcVec<Type>,
    pub functions: RcVec<FunctionDeclaration>,
}

impl Program {
    pub fn new(
        types: impl Into<RcVec<Type>>,
        functions: impl Into<RcVec<FunctionDeclaration>>,
    ) -> Self {
        Self {
            types: types.into(),
            functions: functions.into(),
        }
    }
}

#[derive(Clone)]
pub struct Variable {
    pub name: RcStr,
    pub r#type: Type,
    pub is_mutable: bool,
}

#[derive(Clone)]
pub struct Type {
    pub name: RcStr,
}

impl Type {
    pub fn new(name: impl Into<RcStr>) -> Self {
        Self { name: name.into() }
    }
}

#[derive(Clone)]
pub struct FunctionSignature {
    pub name: RcStr,
    pub args: RcVec<Type>,
    pub return_type: Type,
}

impl FunctionSignature {
    pub fn new(name: impl Into<RcStr>, args: impl Into<RcVec<Type>>, return_type: Type) -> Self {
        Self {
            name: name.into(),
            args: args.into(),
            return_type,
        }
    }
}

#[derive(Clone)]
pub struct FunctionDeclaration {
    pub signature: FunctionSignature,
    pub statements: RcVec<Expression>,
}

impl FunctionDeclaration {
    pub fn new(signature: FunctionSignature, statements: impl Into<RcVec<Expression>>) -> Self {
        Self {
            signature: signature.into(),
            statements: statements.into(),
        }
    }
}

pub struct Function {}

pub struct BlockScope {
    pub variables: RcVec<Variable>,
    pub expression: RcVec<Expression>,
}

#[derive(Clone)]
pub struct AssignExpression {
    pub lhs: Variable,
    pub rhs: Variable,
}

#[derive(Clone)]
pub struct MethodCallExpression {
    pub callee: Variable,
    pub method: FunctionSignature,
    pub args: RcVec<Variable>,
}

#[derive(Clone)]
pub struct FunctionCallExpression {
    pub function: FunctionSignature,
    pub args: RcVec<Variable>,
}

#[derive(Clone)]
pub struct BinaryExpression {
    pub lhs: Variable,
    pub rhs: Variable,
    pub operation: Operation,
}

#[derive(Clone, Copy)]
pub enum Operation {
    Assign,
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Clone)]
pub enum Expression {
    Assign(AssignExpression),
    Binary(BinaryExpression),
    MethodCall(MethodCallExpression),
    FunctionCall(FunctionCallExpression),
}
