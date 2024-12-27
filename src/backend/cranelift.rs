use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use crate::lexer::ast::Program;
use anyhow::anyhow;
use codegen::ir::{condcodes, Inst};
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

    build_loop_fn(&mut ctx, &mut fn_builder_ctx);
    let func_id = module.declare_function("main", Linkage::Preemptible, &ctx.func.signature)?;
    module.define_function(func_id, &mut ctx)?;
    module.clear_context(&mut ctx);

    build_ifelse_main_fn(&mut ctx, &mut fn_builder_ctx);
    let func_id =
        module.declare_function("ifelse_function", Linkage::Preemptible, &ctx.func.signature)?;
    module.define_function(func_id, &mut ctx)?;
    module.clear_context(&mut ctx);

    let object_product = module.finish();

    let bytes = object_product.emit()?;

    std::fs::write("/tmp/output.o", bytes)?;

    Ok(())
}

pub struct IfElseChain {
    else_block: Option<Block>,
    end_block: Block,
}

impl IfElseChain {
    pub fn new_block(
        fbx: &mut FunctionBuilder,
        cond: impl FnOnce(&mut FunctionBuilder) -> Value,
        block_instruction_builder: impl FnOnce(&mut FunctionBuilder) -> Option<Inst>,
    ) -> Self {
        let if_block = fbx.create_block();
        let else_block = fbx.create_block();
        let end_block = fbx.create_block();

        let value = cond(fbx);
        fbx.ins().brif(value, if_block, &[], else_block, &[]);
        fbx.switch_to_block(if_block);
        if block_instruction_builder(fbx).is_none() {
            fbx.ins().jump(end_block, &[]);
        }
        fbx.seal_block(if_block);

        Self {
            else_block: Some(else_block),
            end_block,
        }
    }

    pub fn elseif_block(
        &mut self,
        fbx: &mut FunctionBuilder,
        cond: impl FnOnce(&mut FunctionBuilder) -> Value,
        block_instruction_builder: impl FnOnce(&mut FunctionBuilder) -> Option<Inst>,
    ) {
        let if_block = fbx.create_block();
        let else_block = fbx.create_block();
        let previous_else_block = self.else_block.replace(else_block).unwrap();
        let end_block = self.end_block;
        fbx.switch_to_block(previous_else_block);
        let value = cond(fbx);
        fbx.ins().brif(value, if_block, &[], else_block, &[]);
        fbx.seal_block(previous_else_block);

        fbx.switch_to_block(if_block);
        if block_instruction_builder(fbx).is_none() {
            fbx.ins().jump(end_block, &[]);
        }
        fbx.seal_block(if_block);
    }

    pub fn else_block(
        &mut self,
        fbx: &mut FunctionBuilder,
        block_instruction_builder: impl FnOnce(&mut FunctionBuilder) -> Option<Inst>,
    ) {
        let else_block = self.else_block.take().unwrap();
        fbx.switch_to_block(else_block);

        if block_instruction_builder(fbx).is_none() {
            fbx.ins().jump(self.end_block, &[]);
        }
        fbx.seal_block(else_block);
    }

    pub fn end(&mut self, fbx: &mut FunctionBuilder) {
        if self.else_block.is_some() {
            self.else_block(fbx, |_f| None);
        }
        fbx.switch_to_block(self.end_block);
        fbx.seal_block(self.end_block);
    }
}

fn build_ifelse_main_fn(ctx: &mut Context, fbx: &mut FunctionBuilderContext) {
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

    {
        let mut chain = IfElseChain::new_block(
            &mut f,
            |f| f.ins().icmp_imm(condcodes::IntCC::Equal, a, 1),
            |f| {
                let val = f.ins().iconst(types::I32, 11);
                f.def_var(i, val);

                None
            },
        );

        chain.elseif_block(
            &mut f,
            |f| f.ins().icmp_imm(condcodes::IntCC::Equal, a, 2),
            |f| {
                let val = f.ins().iconst(types::I32, 22);
                f.def_var(i, val);
                None
            },
        );
        chain.elseif_block(
            &mut f,
            |f| f.ins().icmp_imm(condcodes::IntCC::Equal, a, 3),
            |f| {
                let val = f.ins().iconst(types::I32, 33);
                f.def_var(i, val);
                None
            },
        );

        chain.else_block(&mut f, |f| {
            let val = f.ins().iconst(types::I32, 69);
            f.def_var(i, val);
            None
        });

        chain.end(&mut f);
    }

    let ret = f.use_var(i);
    f.ins().return_(&[ret]);
    f.finalize();
}

pub struct Loop {
    end_block: Block,
    loop_block: Block,
}

impl Loop {
    pub fn new(fbx: &mut FunctionBuilder) -> Self {
        let loop_block = fbx.create_block();
        let end_block = fbx.create_block();
        fbx.ins().jump(loop_block, &[]);

        fbx.switch_to_block(loop_block);

        Self {
            end_block,
            loop_block,
        }
    }

    pub fn break_inst(&self, fbx: &mut FunctionBuilder) -> Inst {
        fbx.ins().jump(self.end_block, &[])
    }

    pub fn continue_inst(&self, fbx: &mut FunctionBuilder) -> Inst {
        fbx.ins().jump(self.loop_block, &[])
    }

    pub fn end(&self, fbx: &mut FunctionBuilder) {
        // fbx.ins().jump(self.loop_reenter_block, &[]);

        // fbx.switch_to_block(self.loop_reenter_block);
        // fbx.seal_block(self.loop_reenter_block);
        fbx.ins().jump(self.loop_block, &[]);
        fbx.seal_block(self.loop_block);

        fbx.switch_to_block(self.end_block);
        fbx.seal_block(self.end_block);
    }
}

fn build_loop_fn(ctx: &mut Context, fbx: &mut FunctionBuilderContext) {
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
    let zero = f.ins().iconst(types::I32, 0);
    f.def_var(i, zero);

    {
        let loopp = Loop::new(&mut f);

        let mut chain = IfElseChain::new_block(
            &mut f,
            |f| {
                let ivalue = f.use_var(i);
                f.ins().icmp(condcodes::IntCC::SignedLessThan, a, ivalue)
            },
            |f| {
                let ivalue = f.use_var(i);
                let val = f.ins().iadd_imm(ivalue, 1);
                f.def_var(i, val);
                None
            },
        );

        chain.elseif_block(
            &mut f,
            |f| {
                let ivalue = f.use_var(i);
                f.ins().icmp_imm(condcodes::IntCC::NotEqual, ivalue, 6)
            },
            |f| {
                let val = f.ins().iconst(I32, 66);
                Some(f.ins().return_(&[val]))
            },
        );

        chain.else_block(&mut f, |f| Some(loopp.break_inst(f)));

        chain.end(&mut f);

        let ival = f.use_var(i);
        let val = f.ins().iadd_imm(ival, 2);
        f.def_var(i, val);

        loopp.end(&mut f);
    }

    let ret = f.use_var(i);
    f.ins().return_(&[ret]);
    f.finalize();
}
