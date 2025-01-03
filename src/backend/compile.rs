use std::path::PathBuf;

use cranelift::{
    codegen::Context,
    prelude::{settings, types, AbiParam, Configurable, FunctionBuilder, FunctionBuilderContext},
};
use cranelift_module::{default_libcall_names, DataDescription, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use mantis_expression::pratt::{Block, Declaration, FunctionDecl, Type};

use crate::{
    frontend::tokens::MsContext,
    registries::{modules::resolve_module_by_word, types::MsTypeRegistry},
};

use super::compile_function::compile_function;

pub fn compile_binary(
    declarations: Vec<Declaration>,
    include_dirs: Vec<String>,
    module_name: &str,
) -> anyhow::Result<Vec<u8>> {
    let data_description = DataDescription::new();
    let mut flag_builder = settings::builder();
    flag_builder.set("preserve_frame_pointers", "true");
    let isa_builder = cranelift_native::builder().map_err(|x| anyhow::anyhow!(x))?;
    let isa = isa_builder.finish(settings::Flags::new(flag_builder))?;
    let libcalls = default_libcall_names();
    let mut module = ObjectModule::new(ObjectBuilder::new(isa.clone(), module_name, libcalls)?);
    let mut fbx = FunctionBuilderContext::new();
    let mut ctx = module.make_context();
    let mut ms_ctx = MsContext::new(0);

    for declaration in declarations {
        match declaration {
            Declaration::Function(function_decl) => {
                compile_function(function_decl, &mut module, &mut ctx, &mut fbx, &mut ms_ctx);
            }
            Declaration::Type(name, ty) => {
                todo!("add types to ms_context");
            }
            Declaration::Use(_use_decl) => {
                // let mut iter = use_decl.path.iter();
                // let entry = resolve_module_by_word(&include_dirs, &word)
                //     .expect(&format!("unresolved module {:?}", use_decl.path));
                // match entry {
                //     crate::registries::modules::ModuleEntry::Module(content) => {

                //     }
                //     crate::registries::modules::ModuleEntry::Dir(path) => {
                //         path_buf = path;
                //     }
                // }
                todo!("use decl should compile the modules");
            }
            Declaration::Trait(trait_decl) => todo!("add traits to ms_context"),
            Declaration::TraitImpl(trait_decl, _) => {
                todo!("add functions from trait implementations to ms_context")
            }
        }
    }

    let object_product = module.finish();

    let bytes = object_product.emit()?;

    Ok(bytes)
}
