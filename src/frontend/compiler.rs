use cranelift::{
    codegen::settings::{self, Configurable, Flags},
    frontend::FunctionBuilderContext,
};
use cranelift_module::{default_libcall_names, DataDescription, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};

use crate::libc::libc::link_libc_functions;

use super::tokens::FunctionDeclaration;
use anyhow::anyhow;

pub fn compile(functions: Vec<FunctionDeclaration>) -> anyhow::Result<Vec<u8>> {
    let data_description = DataDescription::new();
    let mut flag_builder = settings::builder();
    flag_builder.set("is_pic", "true");
    flag_builder.set("use_colocated_libcalls", "false");
    let flags = Flags::new(flag_builder);
    let isa_builder = cranelift_native::builder().map_err(|x| anyhow!(x))?;
    let isa = isa_builder.finish(flags)?;
    let libcalls = default_libcall_names();
    let mut module = ObjectModule::new(ObjectBuilder::new(isa.clone(), "main", libcalls)?);
    let mut fbx = FunctionBuilderContext::new();
    let mut ctx = module.make_context();

    // let print_i64_fn = declare_print_i64_fn(&mut module, &mut ctx)?;

    // let libc_fns = link_libc_functions(&mut module, &mut fbx, &mut ctx).unwrap();
    // println!("LIBC FNS: {:?}", libc_fns);

    let mut offset = 0usize;
    for function in functions {
        function.declare(&mut ctx, &mut fbx, &mut module, offset);
        offset += 100;
    }

    let obj = module.finish();

    let bytes = obj.emit()?;

    Ok(bytes)
}
