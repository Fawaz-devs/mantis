use std::{
    collections::{BTreeMap, HashMap},
    rc::Rc,
};

use codegen::ir::{condcodes, Inst};
use cranelift::prelude::*;
use cranelift_object::ObjectModule;
use mantis_expression::node::{BinaryOperation, Node};

use crate::frontend::compile::MsContext;

use super::{
    functions::MsFunctionType,
    structs::{array_struct, MsStructType},
    MsRegistry, MsRegistryExt,
};

#[derive(Clone, Debug, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum MsNativeType {
    Bool,
    Void,
    Array,
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
            "array" => Self::Array,
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

    pub(crate) fn add(&self, lhs: Value, rhs: Value, fbx: &mut FunctionBuilder<'_>) -> Value {
        if self.is_int() {
            fbx.ins().iadd(lhs, rhs)
        } else if self.is_float() {
            fbx.ins().fadd(lhs, rhs)
        } else {
            unreachable!()
        }
    }
    pub(crate) fn sub(&self, lhs: Value, rhs: Value, fbx: &mut FunctionBuilder<'_>) -> Value {
        if self.is_int() {
            fbx.ins().isub(lhs, rhs)
        } else if self.is_float() {
            fbx.ins().fsub(lhs, rhs)
        } else {
            unreachable!()
        }
    }
    pub(crate) fn mult(&self, lhs: Value, rhs: Value, fbx: &mut FunctionBuilder<'_>) -> Value {
        if self.is_int() {
            fbx.ins().imul(lhs, rhs)
        } else if self.is_float() {
            fbx.ins().fmul(lhs, rhs)
        } else {
            unreachable!()
        }
    }

    pub(crate) fn div(&self, lhs: Value, rhs: Value, fbx: &mut FunctionBuilder<'_>) -> Value {
        if self.is_int() {
            if self.is_uint() {
                fbx.ins().udiv(lhs, rhs)
            } else {
                fbx.ins().sdiv(lhs, rhs)
            }
        } else if self.is_float() {
            fbx.ins().fdiv(lhs, rhs)
        } else {
            unreachable!()
        }
    }

    pub(crate) fn compare(
        &self,
        op: BinaryOperation,
        lhs: Value,
        rhs: Value,
        fbx: &mut FunctionBuilder<'_>,
    ) -> Value {
        if self.is_int() {
            fbx.ins().icmp(
                binary_cmp_op_to_condcode_intcc(op, !self.is_uint()),
                lhs,
                rhs,
            )
        } else if self.is_float() {
            fbx.ins().fcmp(binary_cmp_op_to_condcode_fcc(op), lhs, rhs)
        } else {
            unreachable!()
        }
    }

    pub fn is_int(&self) -> bool {
        match self {
            MsNativeType::I8
            | MsNativeType::U8
            | MsNativeType::I16
            | MsNativeType::U16
            | MsNativeType::U32
            | MsNativeType::U64
            | MsNativeType::I64
            | MsNativeType::I32 => true,
            _ => false,
        }
    }

    pub fn is_uint(&self) -> bool {
        match self {
            MsNativeType::U32 | MsNativeType::U8 | MsNativeType::U16 | MsNativeType::U64 => true,
            _ => false,
        }
    }

    pub fn is_sint(&self) -> bool {
        match self {
            MsNativeType::I32 | MsNativeType::I8 | MsNativeType::I16 | MsNativeType::I64 => true,
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            MsNativeType::F32 | MsNativeType::F64 => true,
            _ => false,
        }
    }

    pub(crate) fn cast_to(&self, lhs: Value, r: &MsType, fbx: &mut FunctionBuilder) -> Value {
        let MsType::Native(rnty) = r else {
            panic!("Non native casting not supported yet");
        };
        let diff = r.size() as isize - self.size() as isize;
        let cl_type = rnty.to_cl_type().unwrap();
        if self.is_int() {
            if rnty.is_int() {
                if diff < 0 {
                    return fbx.ins().ireduce(cl_type, lhs);
                } else {
                    if rnty.is_uint() {
                        return fbx.ins().uextend(cl_type, lhs);
                    } else {
                        return fbx.ins().sextend(cl_type, lhs);
                    }
                }
            } else if rnty.is_float() {
                if self.is_uint() {
                    return fbx.ins().fcvt_from_uint(cl_type, lhs);
                } else {
                    return fbx.ins().fcvt_from_sint(cl_type, lhs);
                }
            }
        }
        if self.is_float() {
            if rnty.is_uint() {
                return fbx.ins().fcvt_to_uint(cl_type, lhs);
            } else if rnty.is_sint() {
                return fbx.ins().fcvt_to_sint(cl_type, lhs);
            } else if rnty.is_float() {
                if diff > 0 {
                    return fbx.ins().fpromote(cl_type, lhs);
                } else if diff < 0 {
                    return fbx.ins().fdemote(cl_type, lhs);
                }
            }
        }

        lhs
    }

    pub(crate) fn store(
        &self,
        ptr: Value,
        offset: i32,
        rhs: Value,
        fbx: &mut FunctionBuilder,
    ) -> Inst {
        fbx.ins().store(MemFlags::new(), rhs, ptr, offset)
    }
}

fn binary_cmp_op_to_condcode_fcc(op: BinaryOperation) -> condcodes::FloatCC {
    use condcodes::FloatCC;
    match op {
        BinaryOperation::GreaterThan => FloatCC::GreaterThan,
        BinaryOperation::GreaterThanOrEqualTo => FloatCC::GreaterThanOrEqual,
        BinaryOperation::EqualTo => FloatCC::Equal,
        BinaryOperation::NotEqualTo => FloatCC::NotEqual,
        BinaryOperation::LessThan => FloatCC::LessThan,
        BinaryOperation::LessThanOrEqualTo => FloatCC::GreaterThanOrEqual,
        _ => unreachable!(),
    }
}

pub fn binary_cmp_op_to_condcode_intcc(op: BinaryOperation, signed: bool) -> condcodes::IntCC {
    use condcodes::IntCC;
    if signed {
        match op {
            BinaryOperation::GreaterThan => IntCC::SignedGreaterThan,
            BinaryOperation::GreaterThanOrEqualTo => IntCC::SignedGreaterThanOrEqual,
            BinaryOperation::EqualTo => IntCC::Equal,
            BinaryOperation::NotEqualTo => IntCC::NotEqual,
            BinaryOperation::LessThan => IntCC::SignedLessThan,
            BinaryOperation::LessThanOrEqualTo => IntCC::SignedGreaterThanOrEqual,
            _ => unreachable!(),
        }
    } else {
        match op {
            BinaryOperation::GreaterThan => IntCC::UnsignedGreaterThan,
            BinaryOperation::GreaterThanOrEqualTo => IntCC::UnsignedGreaterThanOrEqual,
            BinaryOperation::EqualTo => IntCC::Equal,
            BinaryOperation::NotEqualTo => IntCC::NotEqual,
            BinaryOperation::LessThan => IntCC::UnsignedLessThan,
            BinaryOperation::LessThanOrEqualTo => IntCC::UnsignedGreaterThanOrEqual,
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct MsGenericType {
    generic_type: MsType,
    generic_map: BTreeMap<String, MsType>,
}

#[derive(Clone, Debug)]
pub enum MsType {
    Function(Rc<MsFunctionType>),
    Generic(Rc<MsGenericType>),
    Native(MsNativeType),
    Struct(Rc<MsStructType>),
}

impl MsType {
    pub fn equal(&self, ty: &MsType) -> bool {
        match (self, ty) {
            (MsType::Function(t0), MsType::Function(t1)) => Rc::ptr_eq(t0, t1),
            (MsType::Generic(t0), MsType::Generic(t1)) => Rc::ptr_eq(t0, t1),
            (MsType::Native(t0), MsType::Native(t1)) => t0 == t1,
            (MsType::Struct(t0), MsType::Struct(t1)) => Rc::ptr_eq(t0, t1),
            _ => false,
        }
    }

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
        registry.insert("i8".into(), MsType::Native(MsNativeType::I8));
        registry.insert("i16".into(), MsType::Native(MsNativeType::I16));
        registry.insert("i32".into(), MsType::Native(MsNativeType::I32));
        registry.insert("i64".into(), MsType::Native(MsNativeType::I64));
        registry.insert("u8".into(), MsType::Native(MsNativeType::U8));
        registry.insert("u16".into(), MsType::Native(MsNativeType::U16));
        registry.insert("u32".into(), MsType::Native(MsNativeType::U32));
        registry.insert("u64".into(), MsType::Native(MsNativeType::U64));
        registry.insert("f32".into(), MsType::Native(MsNativeType::F32));
        registry.insert("f64".into(), MsType::Native(MsNativeType::F64));
        // registry.insert("str".into(), MsType::Native(MsNativeType::Array));
        registry.insert("bool".into(), MsType::Native(MsNativeType::Bool));
        registry.insert("array".into(), MsType::Struct(Rc::new(array_struct())));

        Self { registry }
    }
}

impl MsRegistryExt<MsType> for MsTypeRegistry {}
