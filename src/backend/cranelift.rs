use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use crate::lexer::ast::Program;
use anyhow::anyhow;
use cranelift::{
    codegen::{
        ir::{
            types::{I64, R64},
            UserExternalName, UserFuncName,
        },
        isa::{CallConv, TargetFrontendConfig, TargetIsa},
        Context,
    },
    prelude::*,
};
use cranelift_module::{default_libcall_names, DataDescription, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};

pub fn compile_program(program: Program, output: impl AsRef<Path>) -> anyhow::Result<()> {
    let data_description = DataDescription::new();
    let mut flag_builder = settings::builder();
    let isa_builder = cranelift_native::builder().map_err(|x| anyhow!(x))?;
    let isa = isa_builder.finish(settings::Flags::new(flag_builder))?;
    let libcalls = default_libcall_names();
    let mut module = ObjectModule::new(ObjectBuilder::new(isa.clone(), "main", libcalls)?);
    let mut fn_builder_ctx = FunctionBuilderContext::new();
    let mut ctx = module.make_context();

    // let mut signature = Signature::new(CallConv::SystemV);
    // let usize = module.target_config().pointer_type();
    // signature.params = Vec::from([AbiParam::new(R64), AbiParam::new(R64)]);
    // module.declare_function("print", Linkage::Local, &signature);

    build_main_fn(&mut ctx, &mut fn_builder_ctx);

    let func_id = module.declare_function("add", Linkage::Export, &ctx.func.signature)?;

    module.define_function(func_id, &mut ctx)?;
    module.clear_context(&mut ctx);

    let object_product = module.finish();

    let bytes = object_product.emit()?;

    std::fs::write(output, bytes)?;

    Ok(())
}

fn build_main_fn(ctx: &mut Context, fbx: &mut FunctionBuilderContext) {
    ctx.func.signature.params = vec![AbiParam::new(I64), AbiParam::new(I64)];
    ctx.func.signature.returns = vec![AbiParam::new(I64)];
    let mut builder = FunctionBuilder::new(&mut ctx.func, fbx);
    let entry_block = builder.create_block();

    builder.append_block_params_for_function_params(entry_block);
    builder.switch_to_block(entry_block);
    builder.seal_block(entry_block);

    let mut variables = HashMap::new();
    let a = builder.block_params(entry_block)[0];
    let var = Variable::new(0);
    variables.insert("a", var);
    builder.declare_var(var, I64);
    builder.def_var(var, a);
    let b = builder.block_params(entry_block)[1];
    let var = Variable::new(1);
    variables.insert("b", var);
    builder.declare_var(var, I64);
    builder.def_var(var, b);

    let c = builder.ins().iadd(a, b);
    let var = Variable::new(2);
    variables.insert("c", var);
    builder.declare_var(var, I64);
    builder.def_var(var, c);

    builder.use_var(var);
    builder.ins().return_(&[c]);

    builder.finalize();
}
