use std::collections::HashMap;

use cranelift::prelude::{FunctionBuilder, Value, Variable};

use super::{
    types::{MsType, MsTypeId},
    MsRegistry, MsRegistryExt,
};

#[derive(Clone, Debug)]
pub struct MsVar {
    pub c_var: Variable,
    pub ty_id: MsTypeId,
    pub is_mutable: bool,
}

impl MsVar {
    pub fn new(ty_id: MsTypeId, c_var: Variable) -> Self {
        Self {
            ty_id,
            c_var,
            is_mutable: false,
        }
    }

    pub fn value(&self, fbx: &mut FunctionBuilder) -> Value {
        fbx.use_var(self.c_var)
    }
}

#[derive(Clone, Debug)]
pub struct MsVal {
    pub value: Value,
    pub ty_id: MsTypeId,
}

impl MsVal {
    pub fn new(ty_id: MsTypeId, value: Value) -> Self {
        Self { ty_id, value }
    }

    pub fn value(&self) -> Value {
        self.value
    }
}

#[derive(Default, Clone, Debug)]
pub struct MsVarRegistry {
    pub registry: HashMap<Box<str>, MsVar>, // variable name -> variable type name
    pub stack: Vec<Box<str>>,
}
