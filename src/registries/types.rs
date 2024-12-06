use std::collections::HashMap;

use super::{functions::MsFunctionType, structs::MsStructType, MsRegistry};

#[derive(Clone, Debug, Copy)]
pub enum MsNativeType {
    Bool,
    Void,
    String,
    Function,
    I64,
    I32,
    F32,
    F64,
    U32,
    U64,
    I8,
    U8,
    I16,
    U16,
}

#[derive(Clone, Debug)]
pub enum MsType {
    Native(MsNativeType),
    Struct(MsStructType),
    Function(MsFunctionType),
}

pub struct TypeRegistry {
    registry: HashMap<String, MsType>,
}

impl TypeRegistry {
    pub fn new() -> Self {
        Self {
            registry: HashMap::new(),
        }
    }
}

impl MsRegistry<MsType> for TypeRegistry {
    fn get_registry(&self) -> &HashMap<String, MsType> {
        &self.registry
    }

    fn get_registry_mut(&mut self) -> &mut HashMap<String, MsType> {
        &mut self.registry
    }
}
