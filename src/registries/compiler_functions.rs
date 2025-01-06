use std::collections::HashMap;

use cranelift::prelude::{types, FunctionBuilder, InstBuilder, StackSlotData};
use cranelift_object::ObjectModule;
use mantis_tokens::MantisLexerTokens;

use crate::{
    frontend::tokens::{MsContext, MsNode},
    native::instructions::NodeResult,
};

use super::{
    types::MsType,
    variable::{MsVal, MsVar},
    MsRegistry, MsRegistryExt,
};

pub type MsCompilerFunction = Box<
    dyn for<'a> Fn(
        &'a [&'a MsNode],
        &'a mut MsContext,
        &'a mut FunctionBuilder,
        &'a mut ObjectModule,
    ) -> NodeResult,
>;

pub struct MsCompilerFunctionsRegistry {
    registry: HashMap<String, MsCompilerFunction>,
}

impl MsRegistry<MsCompilerFunction> for MsCompilerFunctionsRegistry {
    fn get_registry(&self) -> &HashMap<String, MsCompilerFunction> {
        &self.registry
    }

    fn get_registry_mut(&mut self) -> &mut HashMap<String, MsCompilerFunction> {
        &mut self.registry
    }
}

impl MsRegistryExt<MsCompilerFunction> for MsCompilerFunctionsRegistry {}

impl Default for MsCompilerFunctionsRegistry {
    fn default() -> Self {
        let mut registry = HashMap::<String, MsCompilerFunction>::new();

        registry.insert("init".into(), Box::new(ms_init_fn));
        registry.insert("size_of".into(), Box::new(ms_size_of_fn));

        Self { registry }
    }
}

fn ms_init_fn(
    nodes: &[&MsNode],
    ms_ctx: &mut MsContext,
    fbx: &mut FunctionBuilder,
    _module: &mut ObjectModule,
) -> NodeResult {
    let Some(MsNode::Var(MantisLexerTokens::Word(ty_name))) = nodes.first() else {
        panic!("Invalid type name or missing {:?}", nodes);
    };

    let ty = ms_ctx.type_registry.get(&ty_name).expect("undefined type");

    ms_init_struct(ty.clone(), fbx)
}
fn ms_size_of_fn(
    nodes: &[&MsNode],
    ms_ctx: &mut MsContext,
    fbx: &mut FunctionBuilder,
    _module: &mut ObjectModule,
) -> NodeResult {
    let Some(MsNode::Var(MantisLexerTokens::Word(ty_name))) = nodes.first() else {
        panic!("Invalid type name or missing {:?}", nodes);
    };

    let ty = ms_ctx.type_registry.get(&ty_name).expect("undefined type");

    ms_size_of(ty.clone(), fbx)
}

fn ms_init_struct(ty: MsType, fbx: &mut FunctionBuilder) -> NodeResult {
    let stack_slot = fbx.create_sized_stack_slot(StackSlotData::new(
        cranelift::prelude::StackSlotKind::ExplicitSlot,
        ty.size() as u32,
    ));

    let ptr = fbx.ins().stack_addr(types::I64, stack_slot, 0);
    NodeResult::Val(MsVal::new(ptr, ty, "i64"))
}

fn ms_size_of(ty: MsType, fbx: &mut FunctionBuilder) -> NodeResult {
    let size = ty.size();
    let value = fbx.ins().iconst(types::I64, size as i64);
    NodeResult::Val(MsVal::new(
        value,
        MsType::Native(super::types::MsNativeType::I64),
        "i64",
    ))
}
