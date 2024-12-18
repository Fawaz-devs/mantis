use std::collections::BTreeMap;

use mantis_tokens::MantisLexerTokens;
use smallstring::SmallString;

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub enum BinaryOperation {
    Add,
    Sub,
    Div,
    Mult,
    Cast,
    Call,
    Access,
    Assign,
    Empty,

    GreaterThan,
    GreaterThanOrEqualTo,
    EqualTo,
    NotEqualTo,
    LessThan,
    LessThanOrEqualTo,
}
impl TryFrom<&MantisLexerTokens> for BinaryOperation {
    type Error = anyhow::Error;

    fn try_from(value: &MantisLexerTokens) -> Result<Self, Self::Error> {
        match value {
            MantisLexerTokens::Assign => Ok(Self::Assign),
            MantisLexerTokens::Add => Ok(Self::Add),
            MantisLexerTokens::Sub => Ok(Self::Sub),
            MantisLexerTokens::Multiply => Ok(Self::Mult),
            MantisLexerTokens::Divide => Ok(Self::Div),
            MantisLexerTokens::As => Ok(Self::Cast),
            MantisLexerTokens::Dot => Ok(Self::Access),

            MantisLexerTokens::GreaterThan => Ok(Self::GreaterThan),
            MantisLexerTokens::GreaterThanOrEqualTo => Ok(Self::GreaterThanOrEqualTo),
            MantisLexerTokens::EqualTo => Ok(Self::EqualTo),
            MantisLexerTokens::NotEqualTo => Ok(Self::NotEqualTo),
            MantisLexerTokens::LessThan => Ok(Self::LessThan),
            MantisLexerTokens::LessThanOrEqualTo => Ok(Self::LessThanOrEqualTo),

            _ => Err(anyhow::anyhow!("Invalid Binary Operation")),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum NativeType {
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

impl NativeType {
    pub fn is_same_type(&self, value: &NativeValue) -> bool {
        match (self, value) {
            (NativeType::Bool, NativeValue::Bool(_)) => true,
            (NativeType::I64, NativeValue::I64(_)) => true,
            (NativeType::I32, NativeValue::I32(_)) => true,
            (NativeType::I32, NativeValue::F32(_)) => true,
            (NativeType::F32, NativeValue::I32(_)) => true,
            (NativeType::F64, NativeValue::F64(_)) => true,
            (NativeType::U32, NativeValue::U32(_)) => true,
            (NativeType::U64, NativeValue::U64(_)) => true,
            (NativeType::I8, NativeValue::I8(_)) => true,
            (NativeType::U8, NativeValue::U8(_)) => true,
            (NativeType::I16, NativeValue::I16(_)) => true,
            (NativeType::I16, NativeValue::U16(_)) => true,
            (NativeType::U16, NativeValue::U16(_)) => true,
            _ => false,
        }
    }

    pub fn size(&self) -> usize {
        match self {
            Self::Bool | Self::I8 | Self::U8 => 1,
            Self::I16 | Self::U16 => 2,
            Self::I32 | Self::U32 | Self::F32 => 4,
            Self::I64 | Self::U64 | Self::F64 => 8,
            _ => {
                panic!("unhandled type {:?}", self);
            }
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            Self::F32 | Self::F64 => true,
            _ => false,
        }
    }

    pub fn is_signed_int(&self) -> bool {
        match self {
            Self::I64 | Self::I32 | Self::I16 | Self::I8 => true,
            _ => false,
        }
    }

    pub fn is_unsigned_int(&self) -> bool {
        match self {
            Self::U64 | Self::U32 | Self::U16 | Self::U8 => true,
            _ => false,
        }
    }

    pub fn from_str(s: &str) -> Result<Self, ()> {
        let t = match s {
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
            _ => return Err(()),
        };

        return Ok(t);
    }
}

#[derive(Clone, Debug)]
pub struct StructMapValue {
    pub fields: BTreeMap<SmallString, Box<MsValue>>,
}
#[derive(Clone, Debug)]
pub struct StructMapType {
    pub fields: BTreeMap<SmallString, Box<MsVarType>>,
}

impl StructMapType {
    pub fn is_same_type(&self, value: &StructMapValue) -> bool {
        for ((ek, ev), (ok, ov)) in self.fields.iter().zip(value.fields.iter()) {
            if ek == ok && ev.is_same_type(ov) {
                eprintln!("Expected Type: {:?}, Received Type: {:?}", ev, ov);
                return false;
            }
        }
        return true;
    }
}

#[derive(Clone, Debug)]
pub enum NativeValue {
    Bool(bool),
    Void,
    I64(i64),
    I32(i32),
    F32(f32),
    F64(f64),
    U32(u32),
    U64(u64),
    I8(i8),
    U8(u8),
    I16(i16),
    U16(u16),
}

#[derive(Clone, Debug)]
pub enum MsVarType {
    Native(NativeType),
    Struct(StructMapType),
}

impl MsVarType {
    pub fn is_same_type(&self, value: &MsValue) -> bool {
        match (self, value) {
            (MsVarType::Native(ntype), MsValue::Native(nvalue)) => ntype.is_same_type(&nvalue),
            (MsVarType::Struct(stype), MsValue::Struct(svalue)) => stype.is_same_type(&svalue),
            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
pub enum MsValue {
    Native(NativeValue),
    Struct(StructMapValue),
}

#[derive(Clone, Debug)]
pub struct Constant {
    pub name: SmallString,
    pub value: MsValue,
    pub var_type: MsVarType,
}

#[derive(Clone, Debug)]
pub struct Variable {
    pub name: SmallString,
    pub var_type: MsVarType,
}

#[derive(Clone, Debug)]
pub enum Value {
    Constant(Constant),
    Variable(Variable),
}

#[derive(Clone, Debug, Copy)]
pub enum UnaryOperation {
    Address,
}

#[derive(Clone, Debug)]
pub enum Node {
    None,
    Binary(BinaryOperation, Box<Node>, Box<Node>),
    Unary(UnaryOperation, Box<Node>),
    Var(MantisLexerTokens),
    Tuple(Vec<Node>),
    Expr(Box<Node>),
}
