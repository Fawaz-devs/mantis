use crate::lexer::ast::Program;
use cranelift::{
    codegen::isa::{Builder, TargetFrontendConfig, TargetIsa},
    prelude::*,
};

pub fn compile_program(program: Program) {
    let fn_builder_ctx = FunctionBuilderContext::new();
    let ctx = codegen::Context::new();
    Architecture;
}
