use std::collections::HashMap;

use cranelift::prelude::{FunctionBuilder, Value, Variable};

use super::{types::MsType, MsRegistry, MsRegistryExt};

#[derive(Clone, Debug)]
pub struct MsVar {
    pub ty: MsType,
    pub c_var: Variable,
    is_mutable: bool,
}

impl MsVar {
    pub fn new(ty: MsType, c_var: Variable) -> Self {
        Self {
            ty,
            c_var,
            is_mutable: false,
        }
    }

    pub fn value(&self, fbx: &mut FunctionBuilder) -> Value {
        fbx.use_var(self.c_var)
    }

    pub fn mark_mutability(&mut self, mutable: bool) {
        self.is_mutable = mutable;
    }

    pub fn is_mutable(&self) -> bool {
        self.is_mutable
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
    pub registry: HashMap<Box<str>, MsVar>, // variable name -> variable type name
    pub stack: Vec<Box<str>>,
}
