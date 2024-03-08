use anyhow::anyhow;
use cranelift::{
    codegen::{
        entity::EntityRef,
        ir::{
            self, types, AbiParam, FuncRef, Inst, InstBuilder, MemFlags, UserExternalName,
            UserFuncName,
        },
        settings::{self, Configurable},
    },
    frontend::{FunctionBuilder, FunctionBuilderContext, Variable},
};
use cranelift_module::{default_libcall_names, DataDescription, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};

pub fn malloc_test() -> anyhow::Result<Vec<u8>> {
    let data_description = DataDescription::new();
    let mut flag_builder = settings::builder();
    flag_builder.set("is_pic", "true");
    flag_builder.set("use_colocated_libcalls", "false");
    let isa_builder = cranelift_native::builder().map_err(|x| anyhow!(x))?;
    let isa = isa_builder.finish(settings::Flags::new(flag_builder))?;
    let libcalls = default_libcall_names();
    let mut module = ObjectModule::new(ObjectBuilder::new(isa.clone(), "main", libcalls)?);
    let mut fn_builder_ctx = FunctionBuilderContext::new();
    let mut ctx = module.make_context();

    // Defining Malloc Signature
    let libc_malloc_fn_id = {
        // let mut signature = module.make_signature();
        ctx.func.signature.params.push(AbiParam::new(types::I64));
        ctx.func.signature.returns.push(AbiParam::new(types::I64));
        // ctx.func.signature = signature.clone();
        let func_id = module.declare_function("malloc", Linkage::Import, &ctx.func.signature)?;
        // let sig = ctx.func.signature.clone();
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut fn_builder_ctx);
        // builder
        //     .func
        //     .declare_imported_user_function(UserExternalName::new(0, func_id.as_u32()));
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        let func_ref = module.declare_func_in_func(func_id, &mut builder.func);
        let malloc_size = builder.block_params(entry_block)[0];
        let inst = builder.ins().call(func_ref, &[malloc_size]);
        let result = builder.inst_results(inst)[0];
        builder.ins().return_(&[result]);

        // for (idx, value) in builder
        //     .block_params(entry_block)
        //     .to_vec()
        //     .iter()
        //     .enumerate()
        // {
        //     let var = Variable::new(idx);
        //     builder.declare_var(var, sig.params[idx].value_type);

        //     builder.def_var(var, *value);
        // }

        // let malloc_func_id = func_id;
        // print!(
        //     "{:?}\n",
        //     module.declarations().get_function_decl(malloc_func_id)
        // );
        // let malloc_func_ref = FuncRef::from_u32(malloc_func_id.as_u32());
        // let malloc_size = builder.block_params(entry_block)[0];
        // let call_inst = builder.ins().call(malloc_func_ref, &[malloc_size]);
        // let returned_value = builder.inst_results(call_inst)[0];
        // builder.ins().return_(&[returned_value]);
        // let value = builder.ins().iconst(types::I64, 1025);
        // builder.ins().return_(&[value]);

        builder.seal_all_blocks();
        builder.finalize();

        let id = module.declare_function("libc_malloc", Linkage::Local, &ctx.func.signature)?;

        module.define_function(id, &mut ctx)?;

        module.clear_context(&mut ctx);

        id
    };

    let main_func_id = {
        let mut signature = module.make_signature();
        signature.params.push(AbiParam::new(types::I32));
        signature.params.push(AbiParam::new(types::I64));
        signature.returns.push(AbiParam::new(types::I32));
        ctx.func.signature = signature.clone();
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut fn_builder_ctx);
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        // for (idx, value) in builder
        //     .block_params(entry_block)
        //     .to_vec()
        //     .iter()
        //     .enumerate()
        // {
        //     let var = Variable::new(idx);
        //     builder.declare_var(var, signature.params[idx].value_type);

        //     builder.def_var(var, *value);
        // }

        let argc = builder.block_params(entry_block)[0];
        let size_val = builder.ins().iconst(types::I64, 1024);
        let func_ref = module.declare_func_in_func(libc_malloc_fn_id, &mut builder.func);
        let call_inst = builder.ins().call(func_ref, &[size_val]);
        let malloc_ptr = builder.inst_results(call_inst)[0];
        let malloc_ptr = builder.ins().ireduce(types::I32, malloc_ptr);
        builder.ins().return_(&[malloc_ptr]);
        builder.seal_all_blocks();
        builder.finalize();
        let func_id = module.declare_function("main", Linkage::Export, &signature)?;
        module.define_function(func_id, &mut ctx)?;
        module.clear_context(&mut ctx);
        func_id
    };

    let obj = module.finish();
    let bytes = obj.emit()?;

    Ok(bytes)
}
