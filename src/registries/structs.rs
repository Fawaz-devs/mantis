use std::collections::{BTreeMap, HashMap};

use cranelift::prelude::{types, AbiParam, FunctionBuilder, InstBuilder};
use cranelift_module::{DataDescription, Linkage, Module};
use cranelift_object::ObjectModule;
use linear_map::LinearMap;

use crate::frontend::compile::MsContext;

use super::{types::MsType, variable::MsVal, MsRegistry};

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

    pub fn add_field(&mut self, field_name: impl Into<String>, field_type: MsType) {
        let size = field_type.size();
        let align = field_type.align();

        if self.size % align != 0 {
            self.size += align - (self.size % align);
        }

        self.fields.insert(
            field_name.into(),
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

    pub fn set_data(
        &self,
        ptr: MsVal,
        values: BTreeMap<String, MsVal>,
        ms_ctx: &mut MsContext,
        fbx: &mut FunctionBuilder<'_>,
        module: &mut ObjectModule,
    ) {
        for (k, v) in self.fields.iter() {
            let val = values.get(k).expect("Missing field on struct");

            if !v.ty.equal(&val.ty) {
                panic!("Types don match {:?} != {:?}", v.ty, val.ty);
            }

            let field = self.get_field(k).unwrap();
            let offset = fbx.ins().iconst(types::I64, field.offset as i64);
            let field_ptr = fbx.ins().iadd(ptr.value, offset);

            todo!()
        }
    }
}

pub fn array_struct() -> MsStructType {
    let mut st = MsStructType::default();

    st.add_field("size", MsType::Native(super::types::MsNativeType::U64));
    st.add_field("ptr", MsType::Native(super::types::MsNativeType::U64));

    return st;
}
