use std::{path::PathBuf, rc::Rc};

use cranelift::{
    codegen::Context,
    prelude::{settings, types, AbiParam, Configurable, FunctionBuilder, FunctionBuilderContext},
};
use cranelift_module::{default_libcall_names, DataDescription, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use mantis_expression::pratt::{Block, Declaration, FunctionDecl, Type};

use crate::{
    frontend::tokens::MsContext,
    registries::{
        modules::{resolve_module_by_word, MsResolved},
        types::{MsGenericTemplate, MsGenericType, MsTypeRegistry},
    },
};

use super::compile_function::compile_function;

pub fn resolve_type_term(ty: &Type, ms_ctx: &MsContext) {
    match ty {
        Type::Struct { fields } => todo!(),
        Type::WithGenerics(_, vec) => todo!(),
        _ => todo!(),
    };
}

pub fn compile_binary(
    declarations: Vec<Declaration>,
    include_dirs: Vec<String>,
    module_name: &str,
) -> anyhow::Result<Vec<u8>> {
    let data_description = DataDescription::new();
    let mut flag_builder = settings::builder();
    flag_builder.set("preserve_frame_pointers", "true");
    flag_builder.set("is_pic", "true");
    flag_builder.set("use_colocated_libcalls", "false");

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
                if let Some(MsResolved::Type(resolved)) = ms_ctx.current_module.resolve(&ty) {
                    let alias = name.to_string();
                    log::info!("type aliased {} -> {:?}", alias, resolved);
                    ms_ctx
                        .current_module
                        .type_registry
                        .registry
                        .insert(alias, resolved);
                } else {
                    log::warn!("found an undefined type, creating type");
                }

                match name {
                    Type::WithGenerics(name, generics) => {
                        let Some(MsResolved::Type(to_ty)) = ms_ctx.current_module.resolve(&ty)
                        else {
                            panic!("function types not implmementd yet")
                        };

                        let template = MsGenericTemplate {
                            generics: generics.into_iter().map(|x| x.to_string()).collect(),
                            inner_type: todo!(),
                        };
                        ms_ctx
                            .current_module
                            .type_templates
                            .registry
                            .insert(name.to_string().into(), Rc::new(template));
                    }
                    Type::Word(word_span) => {
                        let MsResolved::Type(to_ty) =
                            ms_ctx.current_module.resolve(&ty).expect("undefined type")
                        else {
                            panic!("function types not implemented yet");
                        };

                        let alias = word_span.as_str();
                        log::info!("type aliased {} -> {:?}", alias, to_ty);
                        ms_ctx
                            .current_module
                            .type_registry
                            .registry
                            .insert(alias.into(), to_ty);
                    }
                    _ => unreachable!(),
                }

                todo!("add types to ms_context");
            }
            Declaration::Use(_use_decl) => {
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

pub fn compile_main_fn(
    module: &mut ObjectModule,
    ctx: &mut Context,
    fbx: &mut FunctionBuilderContext,
    ms_ctx: &mut MsContext,
) {
    ctx.func.signature.params.push(AbiParam::new(types::I32)); // argv
    ctx.func.signature.params.push(AbiParam::new(types::I64)); // char** argc
    ctx.func.signature.returns.push(AbiParam::new(types::I32)); // exit code

    let func_id = module
        .declare_function("main", Linkage::Preemptible, &ctx.func.signature)
        .unwrap();
}
