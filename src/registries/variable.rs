use std::collections::HashMap;

use cranelift::prelude::{FunctionBuilder, Value, Variable};

use super::{types::MsType, MsRegistry, MsRegistryExt};

#[derive(Clone, Debug)]
pub struct MsVar {
    pub ty: MsType,
    pub c_var: Variable,
    ty_name: Box<str>,
    is_mutable: bool,
}

impl MsVar {
    pub fn new(ty: MsType, c_var: Variable, ty_name: impl Into<Box<str>>) -> Self {
        Self {
            ty,
            c_var,
            is_mutable: false,
            ty_name: ty_name.into(),
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

    pub fn set_type_name(&mut self, ty_name: impl Into<Box<str>>) {
        self.ty_name = ty_name.into()
    }

    pub fn type_name(&self) -> &str {
        &self.ty_name
    }
}

#[derive(Clone, Debug)]
pub struct MsVal {
    pub value: Value,
    pub ty: MsType,
    ty_name: Box<str>,
}

impl MsVal {
    pub fn new(value: Value, ty: MsType, ty_name: impl Into<Box<str>>) -> Self {
        Self {
            value,
            ty,
            ty_name: ty_name.into(),
        }
    }

    pub fn value(&self) -> Value {
        self.value
    }

    pub fn set_type_name(&mut self, ty_name: impl Into<Box<str>>) {
        self.ty_name = ty_name.into()
    }

    pub fn type_name(&self) -> &str {
        &self.ty_name
    }
}

#[derive(Default, Clone, Debug)]
pub struct MsVarRegistry {
    pub registry: HashMap<Box<str>, MsVar>, // variable name -> variable type name
    pub stack: Vec<Box<str>>,
}
