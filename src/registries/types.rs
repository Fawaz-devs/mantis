use std::{
    collections::{BTreeMap, HashMap},
    hash::Hash,
    rc::Rc,
};

use codegen::ir::{condcodes, Inst};
use cranelift::prelude::*;
use linear_map::LinearMap;
use mantis_expression::node::BinaryOperation;

use crate::native::instructions::Either;

use super::{
    structs::{pointer_template, MsStructType},
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

    fn to_string(&self) -> &'static str {
        match self {
            MsNativeType::Bool => "bool",
            MsNativeType::Void => "void",
            MsNativeType::Array => "array",
            MsNativeType::Function => "function",
            MsNativeType::I64 => "i64",
            MsNativeType::I32 => "i32",
            MsNativeType::F32 => "f32",
            MsNativeType::F64 => "f64",
            MsNativeType::U32 => "u32",
            MsNativeType::U64 => "u64",
            MsNativeType::I8 => "I8",
            MsNativeType::U8 => "U8",
            MsNativeType::I16 => "I16",
            MsNativeType::U16 => "U16",
        }
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
    pub generics: Vec<String>,
    pub generic_map: BTreeMap<String, String>, // field ->  type_name
}

#[derive(Clone, Debug, Default)]
pub struct MsGenericTemplate {
    generics: Vec<String>,
    generic_map: LinearMap<String, Either<MsType, String>>, // field ->  type_name
}

impl MsGenericTemplate {
    pub fn add_generic(&mut self, s: impl Into<String>) {
        let s: String = s.into();
        assert!(!self.generics.contains(&s));
        self.generics.push(s);
    }

    pub fn add_field(&mut self, field_name: impl Into<String>, field_type: Either<MsType, String>) {
        self.generic_map.insert(field_name.into(), field_type);
    }

    pub fn to_struct(&self, generics: &BTreeMap<String, MsType>) -> MsStructType {
        assert!(self.generics.len() == generics.len());

        let mut st = MsStructType::default();

        for (k, v) in &self.generic_map {
            let ty = match v {
                Either::Left(ty) => ty,
                Either::Right(gen) => generics.get(gen).expect("Missing generic type"),
            };

            match ty {
                MsType::Native(nty) => st.add_field(k, MsType::Native(*nty)),
                MsType::Struct(field) => {
                    st.add_field(k, MsType::Struct(field.clone()));
                }
                // MsType::Generic(gen) => {
                //     let field = gen.to_struct(generics);
                //     st.add_field(k, MsType::Struct(Rc::new(field)));
                // }
                MsType::Ref(ms_type, _) => todo!(),
            };
        }
        st
    }

    pub(crate) fn generate(&self, real_types: Vec<super::modules::MsResolved<'_>>) -> MsType {
        todo!()
    }
}

#[derive(Clone, Debug)]
pub enum MsType {
    Native(MsNativeType),
    Struct(Rc<MsStructType>),
    Ref(Box<MsType>, bool),
}

impl PartialEq for MsType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (MsType::Native(ty1), MsType::Native(ty2)) => ty1 == ty2,
            (MsType::Struct(ty1), MsType::Struct(ty2)) => Rc::ptr_eq(ty1, ty2),
            (MsType::Ref(ty1, mut1), MsType::Ref(ty2, mut2)) => ty1 == ty2 && mut1 == mut2,
            _ => false,
        }
    }
}

impl Hash for MsType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            MsType::Native(ty) => state.write(ty.to_string().as_bytes()),
            MsType::Struct(rc) => todo!(),
            MsType::Ref(ms_type, _) => todo!(),
        }
    }
}

impl MsType {
    pub fn equal(&self, ty: &MsType) -> bool {
        match (self, ty) {
            (MsType::Native(t0), MsType::Native(t1)) => t0 == t1,
            (MsType::Struct(t0), MsType::Struct(t1)) => Rc::ptr_eq(t0, t1),
            _ => false,
        }
    }

    pub fn size(&self) -> usize {
        match self {
            MsType::Native(ty) => ty.size(),
            MsType::Struct(ty) => ty.size(),
            _ => todo!(),
        }
    }

    pub fn align(&self) -> usize {
        match self {
            MsType::Native(ty) => ty.align(),
            MsType::Struct(ty) => ty.align(),
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
            _ => todo!(),
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            MsType::Native(ty) => ty.to_string().into(),
            _ => todo!(),
        }
    }
}

#[derive(Debug)]
pub struct MsTypeRegistry {
    pub registry: HashMap<String, MsType>,
}

impl MsRegistry<MsType> for MsTypeRegistry {
    fn get_registry(&self) -> &HashMap<String, MsType> {
        &self.registry
    }

    fn get_registry_mut(&mut self) -> &mut HashMap<String, MsType> {
        &mut self.registry
    }
}

impl MsRegistryExt<MsType> for MsTypeRegistry {}

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
        registry.insert("bool".into(), MsType::Native(MsNativeType::Bool));
        // registry.insert("array".into(), MsType::Struct(Rc::new(array_struct())));

        let pointer_ty = pointer_template();

        {
            let mut generics = BTreeMap::new();
            generics.insert("T".into(), MsType::Native(MsNativeType::Void));
            registry.insert(
                "pointer[void]".into(),
                MsType::Struct(Rc::new(pointer_ty.to_struct(&generics))),
            );
            generics.insert("T".into(), MsType::Native(MsNativeType::I64));
            registry.insert(
                "pointer[i64]".into(),
                MsType::Struct(Rc::new(pointer_ty.to_struct(&generics))),
            );
            generics.insert("T".into(), MsType::Native(MsNativeType::F64));
            registry.insert(
                "pointer[f64]".into(),
                MsType::Struct(Rc::new(pointer_ty.to_struct(&generics))),
            );
            generics.insert("T".into(), MsType::Native(MsNativeType::U8));
            registry.insert(
                "pointer[u8]".into(),
                MsType::Struct(Rc::new(pointer_ty.to_struct(&generics))),
            );
        }

        // registry.insert("pointer".into(), MsType::Generic(Rc::new(pointer_ty)));

        // registry.insert("str".into(), MsType::Native(MsNativeType::Array));

        Self { registry }
    }
}
#[derive(Debug)]
pub struct MsTemplateRegistry {
    registry: HashMap<String, MsGenericTemplate>,
}

impl MsRegistry<MsGenericTemplate> for MsTemplateRegistry {
    fn get_registry(&self) -> &HashMap<String, MsGenericTemplate> {
        &self.registry
    }

    fn get_registry_mut(&mut self) -> &mut HashMap<String, MsGenericTemplate> {
        &mut self.registry
    }
}
impl MsRegistryExt<MsGenericTemplate> for MsTemplateRegistry {}

impl Default for MsTemplateRegistry {
    fn default() -> Self {
        let registry = HashMap::<String, MsGenericTemplate>::with_capacity(512);

        Self { registry }
    }
}

#[derive(Default, Debug)]
pub struct MsTypeTemplates {
    pub registry: HashMap<Box<str>, Rc<MsGenericTemplate>>,
}
