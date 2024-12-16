use std::collections::HashMap;

use cranelift::prelude::AbiParam;
use linear_map::LinearMap;

use super::{types::MsType, MsRegistry};

#[derive(Debug, Clone)]
pub struct MsStructFieldValue {
    offset: usize,
    ty: MsType,
}

#[derive(Clone, Debug, Default)]
pub struct MsStructType {
    fields: LinearMap<String, MsStructFieldValue>,
    size: usize,
}

impl MsStructType {
    pub fn size(&self) -> usize {
        self.size
    }

    pub fn align(&self) -> usize {
        if self.size % 8 == 0 {
            self.size
        } else {
            self.size + (8 - self.size % 8)
        }
    }

    pub fn add_field(&mut self, field_name: String, field_type: MsType) {
        let size = field_type.size();
        let align = field_type.align();

        if self.size % align != 0 {
            self.size += align - (self.size % align);
        }

        self.fields.insert(
            field_name,
            MsStructFieldValue {
                offset: self.size,
                ty: field_type,
            },
        );
        self.size += size;
    }

    pub fn get_field(&self, field_name: &str) -> Option<&MsStructFieldValue> {
        self.fields.get(field_name)
    }

    pub fn to_abi_param(&self) -> AbiParam {
        todo!()
    }
}
