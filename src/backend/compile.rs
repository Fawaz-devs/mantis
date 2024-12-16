use std::collections::HashMap;

use cranelift::prelude::FunctionBuilder;
use cranelift_object::ObjectModule;
use mantis_expression::node::{BinaryOperation, Node};

use crate::{
    registries::{types::MsTypeRegistry, variable::MsVarRegistry},
    scope::MsScopes,
};

pub struct MsContext {
    type_registry: MsTypeRegistry,
    scopes: MsScopes<HashMap<String, String>>,
}

pub fn compile_node(node: &Node, fbx: &mut FunctionBuilder<'_>, module: &mut ObjectModule) {
    match node {
        Node::None => todo!(),
        Node::Binary(op, lhs, rhs) => compile_binary_op(*op, &lhs, &rhs, fbx, module),
        Node::Var(var) => todo!(),
        Node::Tuple(_) => todo!(),
        Node::Expr(expr) => compile_node(&expr, fbx, module),
    }
}

pub fn compile_binary_op(
    op: BinaryOperation,
    lhs: &Node,
    rhs: &Node,
    fbx: &mut FunctionBuilder<'_>,
    module: &mut ObjectModule,
) {
    use BinaryOperation::*;
    match op {
        Add => todo!(),
        Sub => todo!(),
        Div => todo!(),
        Mult => todo!(),
        Cast => todo!(),
        Call => todo!(),
        Access => todo!(),
        Assign => todo!(),
        Empty => todo!(),
    }
}
