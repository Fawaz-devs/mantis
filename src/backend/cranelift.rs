use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use crate::lexer::ast::Program;
use anyhow::anyhow;
use codegen::ir::condcodes;
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
use types::I32;

#[test]
fn test_cranelift() -> anyhow::Result<()> {
    let data_description = DataDescription::new();
    let mut flag_builder = settings::builder();
    let isa_builder = cranelift_native::builder().map_err(|x| anyhow!(x))?;
    let isa = isa_builder.finish(settings::Flags::new(flag_builder))?;
    let libcalls = default_libcall_names();
    let mut module = ObjectModule::new(ObjectBuilder::new(isa.clone(), "main", libcalls)?);
    let mut fn_builder_ctx = FunctionBuilderContext::new();
    let mut ctx = module.make_context();

    build_main_fn(&mut ctx, &mut fn_builder_ctx);

    let func_id = module.declare_function("main", Linkage::Preemptible, &ctx.func.signature)?;

    module.define_function(func_id, &mut ctx)?;
    module.clear_context(&mut ctx);

    let object_product = module.finish();

    let bytes = object_product.emit()?;

    std::fs::write("/tmp/output.o", bytes)?;

    Ok(())
}

pub struct IfElseChain {
    else_block: Block,
    end_block: Block,
}

impl IfElseChain {
    pub fn new_block(
        cond: Value,
        fbx: &mut FunctionBuilder,
        block_instruction_builder: fn(f: &mut FunctionBuilder),
    ) -> Self {
        let if_block = fbx.create_block();
        let else_block = fbx.create_block();
        let end_block = fbx.create_block();

        fbx.ins().brif(cond, if_block, &[], else_block, &[]);
        fbx.switch_to_block(if_block);
        fbx.seal_block(if_block);
        block_instruction_builder(fbx);
        fbx.ins().jump(end_block, &[]);

        Self {
            else_block,
            end_block,
        }
    }

    pub fn elseif_block(
        &mut self,
        cond: Value,
        fbx: &mut FunctionBuilder,
        block_instruction_builder: fn(f: &mut FunctionBuilder),
    ) {
        let else_block = fbx.create_block();
        let if_block = self.else_block;
        let end_block = self.end_block;

        fbx.ins().brif(cond, if_block, &[], else_block, &[]);
        fbx.switch_to_block(if_block);
        fbx.seal_block(if_block);
        block_instruction_builder(fbx);
        fbx.ins().jump(end_block, &[]);

        self.else_block = else_block;
    }

    pub fn else_block(
        &mut self,
        fbx: &mut FunctionBuilder,
        block_instruction_builder: fn(f: &mut FunctionBuilder),
    ) {
        fbx.switch_to_block(self.else_block);
        fbx.seal_block(self.else_block);
        block_instruction_builder(fbx);
        fbx.ins().jump(self.end_block, &[]);
    }

    pub fn end(self, fbx: &mut FunctionBuilder) {
        fbx.switch_to_block(self.end_block);
        fbx.seal_block(self.end_block);
    }
}

fn build_main_fn(ctx: &mut Context, fbx: &mut FunctionBuilderContext) {
    ctx.func.signature.params = vec![AbiParam::new(I32), AbiParam::new(I64)];
    ctx.func.signature.returns = vec![AbiParam::new(I32)];
    let mut f = FunctionBuilder::new(&mut ctx.func, fbx);
    let entry_block = f.create_block();

    f.append_block_params_for_function_params(entry_block);
    f.switch_to_block(entry_block);
    f.seal_block(entry_block);

    let i = Variable::new(1);
    f.declare_var(i, types::I32);
    let a = f.block_params(entry_block)[0];
    f.def_var(i, a);

    // if a == 1 make v = 11
    let v11_block = f.create_block();
    let v22_block = f.create_block();
    let end_block = f.create_block();

    let else_block = f.create_block();

    // f.ins().jump(v11_block, &[]);

    {
        let cond_value = f.ins().icmp_imm(condcodes::IntCC::Equal, a, 1);
        f.ins().brif(cond_value, v11_block, &[], else_block, &[]);

        f.switch_to_block(v11_block);
        f.seal_block(v11_block);
        let val = f.ins().iconst(types::I32, 11);
        f.def_var(i, val);
        f.ins().jump(end_block, &[]);
    }
    {
        // // if a == 2 make v = 22
        // let cond_value = f.ins().icmp_imm(condcodes::IntCC::Equal, a, 2);
        // // let v22_block = f.create_block();
        // // f.ins().jump(v22_block, &[]);
        // f.ins().brif(cond_value, v22_block, &[], else_block, &[]);
        // f.switch_to_block(v22_block);
        // f.seal_block(v22_block);
        // let val = f.ins().iconst(types::I32, 22);
        // f.def_var(i, val);
        // f.ins().jump(end_block, &[]);
    }
    {
        f.switch_to_block(else_block);
        f.seal_block(else_block);
        let val = f.ins().iconst(types::I32, 6969);
        f.def_var(i, val);
        f.ins().jump(end_block, &[]);
    }
    {
        // endif block
        // let end_block = f.create_block();
        // f.ins().jump(end_block, &[]);
        f.switch_to_block(end_block);
        f.seal_block(end_block);
    }
    // let val = f.ins().iconst(types::I32, 22);
    // f.def_var(i, val);

    /*
        if_true block:
        // elseif_true1 block:
        // else:
        endif block:
    */

    let ret = f.use_var(i);
    f.ins().return_(&[ret]);
    f.finalize();
}
