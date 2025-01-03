use cranelift::{
    codegen::Context,
    prelude::{types, AbiParam, FunctionBuilder, FunctionBuilderContext},
};
use cranelift_module::{Linkage, Module};
use cranelift_object::ObjectModule;
use mantis_expression::pratt::{Block, FunctionDecl, Type};

use crate::{frontend::tokens::MsContext, registries::types::MsTypeRegistry};

pub enum FinalType {
    Name(Box<str>),
    Type(Type),
    CraneliftType(types::Type),
}

pub fn resolve_typename(ty: &Type) -> Box<str> {
    match ty {
        Type::Enum { fields } => todo!(),
        Type::Struct { fields } => todo!(),
        Type::WithGenerics(_, vec) => todo!(),
        Type::Nested(_, _) => todo!(),
        Type::Unknown => todo!(),
        Type::Word(word_span) => todo!(),
        Type::Ref(_, _) => todo!(),
    }
}

pub fn type_name_into_cranelift_type(ty_name: &str, ctx: &MsContext) -> Option<types::Type> {
    todo!()
}

pub fn compile_function(
    function: &FunctionDecl,
    module: &mut ObjectModule,
    ctx: &mut Context,
    fbx: &mut FunctionBuilderContext,
    ms_ctx: &mut MsContext,
) {
    let name = resolve_typename(&function.name);

    let mut linkage = Linkage::Preemptible;

    if function.is_extern {
        if matches!(function.block, Block::Empty) {
            linkage = Linkage::Import;
        } else {
            linkage = Linkage::Export;
        }
    } else {
        for (_k, v) in &function.arguments {
            let Some(ty) = type_name_into_cranelift_type(&resolve_typename(v), &ms_ctx) else {
                unreachable!()
            };

            ctx.func.signature.params.push(AbiParam::new(ty));
        }
        {
            if let Some(ty) =
                type_name_into_cranelift_type(&resolve_typename(&function.return_type), &ms_ctx)
            {
                ctx.func.signature.returns.push(AbiParam::new(ty));
            } else {
            };
        }

        let mut f = FunctionBuilder::new(&mut ctx.func, fbx);
        compile_block(&function.block, module, &mut f);
    }

    let func_id = module
        .declare_function(&name, linkage, &ctx.func.signature)
        .unwrap();
    module.define_function(func_id, ctx).unwrap();
    ctx.clear();
}
pub fn compile_block(function: &Block, module: &mut ObjectModule, fbx: &mut FunctionBuilder) {}
