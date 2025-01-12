use std::{collections::HashMap, path::PathBuf, rc::Rc};

use cranelift::{
    codegen::Context,
    prelude::{settings, types, AbiParam, Configurable, FunctionBuilder, FunctionBuilderContext},
};
use cranelift_module::{default_libcall_names, DataDescription, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use mantis_expression::pratt::{Block, Declaration, FunctionDecl, Type};

use crate::{
    backend::compile_function::TraitFunctionFor,
    ms::MsContext,
    registries::{
        functions::{MsFunctionRegistry, MsGenericFunction},
        modules::{resolve_module_by_word, MsResolved},
        types::{MsGenericTemplate, MsTypeRegistry, TypeNameWithGenerics},
    },
};

use super::compile_function::{compile_function, random_string};

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
                compile_function(
                    function_decl,
                    &mut module,
                    &mut ctx,
                    &mut fbx,
                    &mut ms_ctx,
                    None,
                    None,
                );
            }
            Declaration::Type(name, ty) => {
                match name {
                    Type::WithGenerics(word_span, generics) => {
                        let generics = generics
                            .iter()
                            .map(|x| x.word().unwrap().to_string())
                            .collect::<Vec<_>>();
                        let template =
                            Rc::new(ms_ctx.current_module.resolve_with_generics(&ty, &generics));
                        let key = word_span.word().unwrap();
                        log::info!("template generated aliased {} -> {:?}", key, template);
                        ms_ctx
                            .current_module
                            .type_templates
                            .registry
                            .insert(key.into(), template.clone());
                    }
                    Type::Word(word_span) => {
                        if let Some(MsResolved::Type(resolved)) = ms_ctx.current_module.resolve(&ty)
                        {
                            let alias = word_span.as_str();
                            ms_ctx
                                .current_module
                                .type_registry
                                .add_alias(alias, resolved.id);
                        } else {
                            log::warn!("found an undefined type, creating type");
                            todo!("add types to ms_context");
                        }
                    }
                    _ => todo!(),
                };
            }
            Declaration::Use(_use_decl) => {
                todo!("use decl should compile the modules");
            }
            Declaration::Trait(trait_decl) => {
                let trait_name = trait_decl.name.word().unwrap();
                let functions = trait_decl.functions;

                ms_ctx
                    .current_module
                    .trait_templates
                    .registry
                    .insert(trait_name.into(), functions);
                ms_ctx
                    .current_module
                    .trait_registry
                    .registry
                    .insert(trait_name.into(), Default::default());

                log::info!("Added functions of trait {}", trait_name);

                // todo!("add traits to ms_context");
            }
            Declaration::TraitImpl(trait_decl, ty) => {
                if trait_decl.generics.is_empty() {
                    let ty = ms_ctx.current_module.resolve(&ty).unwrap().ty().unwrap();
                    ms_ctx
                        .current_module
                        .add_alias(TypeNameWithGenerics::new("Self".into(), vec![]), ty.clone());

                    let trait_name = trait_decl.name.word().unwrap();
                    let mut fn_registry = MsFunctionRegistry::default();

                    for function in trait_decl.functions {
                        let trait_fn_for = TraitFunctionFor {
                            trait_name,
                            on_type: &ty,
                        };

                        let fn_name = random_string(24);
                        let decl = compile_function(
                            function,
                            &mut module,
                            &mut ctx,
                            &mut fbx,
                            &mut ms_ctx,
                            Some(trait_fn_for),
                            Some(&fn_name),
                        );
                    }
                } else {
                    let mut generics = Vec::new();
                    for function in trait_decl.functions {
                        let func_name: Box<str> = function.name.word().unwrap().into();
                        let template = MsGenericFunction {
                            decl: Rc::new(function),
                            generics: generics.clone(),
                        };

                        let ty_name = ty.word().unwrap();

                        let registry = if let Some(registry) = ms_ctx
                            .current_module
                            .trait_generic_templates
                            .registry
                            .get_mut(ty_name)
                        {
                            registry
                        } else {
                            ms_ctx
                                .current_module
                                .trait_generic_templates
                                .registry
                                .insert(ty_name.into(), Default::default());

                            ms_ctx
                                .current_module
                                .trait_generic_templates
                                .registry
                                .get_mut(ty_name)
                                .unwrap()
                        };

                        registry.push(template);
                    }

                    // it is a trait template
                    todo!("Trait Template registration");
                }
                ms_ctx.current_module.clear_aliases();
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
