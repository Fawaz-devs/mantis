use std::collections::HashMap;

use cranelift::prelude::{FunctionBuilder, Value, Variable};

use super::{types::MsType, MsRegistry, MsRegistryExt};

#[derive(Clone, Debug)]
pub struct MsVar {
    pub ty: MsType,
    pub c_var: Variable,
}

impl MsVar {
    pub fn new(ty: MsType, c_var: Variable) -> Self {
        Self { ty, c_var }
    }

    pub fn value(&self, fbx: &mut FunctionBuilder) -> Value {
        fbx.use_var(self.c_var)
    }
}

#[derive(Clone, Debug)]
pub struct MsVal {
    pub value: Value,
    pub ty: MsType,
}

impl MsVal {
    pub fn new(value: Value, ty: MsType) -> Self {
        Self { value, ty }
    }

    pub fn value(&self) -> Value {
        self.value
    }
}

#[derive(Default, Clone, Debug)]
pub struct MsVarRegistry {
    registry: HashMap<String, MsVar>, // variable name -> variable type name
}

impl MsRegistry<MsVar> for MsVarRegistry {
    fn get_registry(&self) -> &HashMap<String, MsVar> {
        &self.registry
    }

    fn get_registry_mut(&mut self) -> &mut HashMap<String, MsVar> {
        &mut self.registry
    }
}

impl MsRegistryExt<MsVar> for MsVarRegistry {}
