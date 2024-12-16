use std::{
    collections::{BTreeMap, HashMap},
    rc::Rc,
};

use cranelift::prelude::*;
use mantis_expression::node::Node;

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

impl MsNativeType {
    pub fn size(&self) -> usize {
        match self {
            Self::Bool | Self::I8 | Self::U8 => 1,
            Self::I16 | Self::U16 => 2,
            Self::I32 | Self::U32 | Self::F32 => 4,
            Self::I64 | Self::U64 | Self::F64 => 8,
            _ => {
                panic!("unhandled type {:?}", self);
                return 0;
            }
        }
    }

    pub fn align(&self) -> usize {
        self.size()
    }

    pub fn from_str(s: &str) -> Option<Self> {
        Some(match s {
            "i8" => Self::I8,
            "i16" => Self::I16,
            "i32" => Self::I32,
            "i64" => Self::I64,
            "u8" => Self::U8,
            "u16" => Self::U16,
            "u32" => Self::U32,
            "u64" => Self::U64,
            "f32" => Self::F32,
            "f64" => Self::F64,
            "str" => Self::String,
            "bool" => Self::Bool,
            _ => {
                return None;
            }
        })
    }

    pub fn to_abi_param(&self) -> Option<AbiParam> {
        Some(match self {
            MsNativeType::Void => return None,
            MsNativeType::Bool => AbiParam::new(types::I8),
            MsNativeType::F32 => AbiParam::new(types::F32),
            MsNativeType::F64 => AbiParam::new(types::F64),
            MsNativeType::I8 => AbiParam::new(types::I8).sext(),
            MsNativeType::I16 => AbiParam::new(types::I16).sext(),
            MsNativeType::I32 => AbiParam::new(types::I32).sext(),
            MsNativeType::I64 => AbiParam::new(types::I64).sext(),
            MsNativeType::U8 => AbiParam::new(types::I8).uext(),
            MsNativeType::U16 => AbiParam::new(types::I16).uext(),
            MsNativeType::U32 => AbiParam::new(types::I32).uext(),
            MsNativeType::U64 => AbiParam::new(types::I64).uext(),
            _ => todo!(),
        })
    }
    pub fn to_cl_type(&self) -> Option<Type> {
        Some(match self {
            MsNativeType::Void => return None,
            MsNativeType::Bool => types::I8,
            MsNativeType::F32 => types::F32,
            MsNativeType::F64 => types::F64,
            MsNativeType::I8 => types::I8,
            MsNativeType::I16 => types::I16,
            MsNativeType::I32 => types::I32,
            MsNativeType::I64 => types::I64,
            MsNativeType::U8 => types::I8,
            MsNativeType::U16 => types::I16,
            MsNativeType::U32 => types::I32,
            MsNativeType::U64 => types::I64,
            _ => todo!(),
        })
    }
}

#[derive(Clone, Debug)]
pub struct MsGenericType {
    generic_type: MsType,
    generic_map: BTreeMap<String, MsType>,
}

#[derive(Clone, Debug)]
pub enum MsType {
    Native(MsNativeType),
    Struct(Rc<MsStructType>),
    Function(Rc<MsFunctionType>),
    Generic(Rc<MsGenericType>),
}

impl MsType {
    pub fn size(&self) -> usize {
        match self {
            MsType::Native(ty) => ty.size(),
            MsType::Struct(ty) => ty.size(),
            // MsType::Function(ty) => ty.size(),
            // MsType::Generic(ty) => ty.size(),
            _ => todo!(),
        }
    }

    pub fn align(&self) -> usize {
        match self {
            MsType::Native(ty) => ty.align(),
            MsType::Struct(ty) => ty.align(),
            // MsType::Function(ty) => ty.align(),
            // MsType::Generic(ty) => ty.align(),
            _ => todo!(),
        }
    }

    pub fn to_abi_param(&self) -> Option<AbiParam> {
        match self {
            MsType::Native(ty) => ty.to_abi_param(),
            MsType::Struct(ty) => Some(ty.to_abi_param()),
            _ => todo!(),
        }
    }

    pub fn to_cl_type(&self) -> Option<Type> {
        match self {
            MsType::Native(ty) => ty.to_cl_type(),
            MsType::Struct(ty) => todo!(),
            _ => todo!(),
        }
    }
}

#[derive(Debug)]
pub struct MsTypeRegistry {
    registry: HashMap<String, MsType>,
}

impl MsRegistry<MsType> for MsTypeRegistry {
    fn get_registry(&self) -> &HashMap<String, MsType> {
        &self.registry
    }

    fn get_registry_mut(&mut self) -> &mut HashMap<String, MsType> {
        &mut self.registry
    }
}

impl Default for MsTypeRegistry {
    fn default() -> Self {
        let mut registry = HashMap::<String, MsType>::with_capacity(512);
        registry.insert("i8".to_owned(), MsType::Native(MsNativeType::I8));
        registry.insert("i16".to_owned(), MsType::Native(MsNativeType::I16));
        registry.insert("i32".to_owned(), MsType::Native(MsNativeType::I32));
        registry.insert("i64".to_owned(), MsType::Native(MsNativeType::I64));
        registry.insert("u8".to_owned(), MsType::Native(MsNativeType::U8));
        registry.insert("u16".to_owned(), MsType::Native(MsNativeType::U16));
        registry.insert("u32".to_owned(), MsType::Native(MsNativeType::U32));
        registry.insert("u64".to_owned(), MsType::Native(MsNativeType::U64));
        registry.insert("f32".to_owned(), MsType::Native(MsNativeType::F32));
        registry.insert("f64".to_owned(), MsType::Native(MsNativeType::F64));
        registry.insert("str".to_owned(), MsType::Native(MsNativeType::String));
        registry.insert("bool".to_owned(), MsType::Native(MsNativeType::Bool));

        Self { registry }
    }
}
