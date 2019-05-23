use crate::ast::{Type, TypeRef};
use std::fmt::{self, Display, Formatter};

#[derive(Clone, Debug)]
pub enum LLVMType {
    I(u8),
    F(u8),
    V,
    Array(usize, Box<LLVMType>),
    Ptr(Box<LLVMType>),
    Struct{
        name: String,
        size: usize,
    },
}

impl LLVMType {
    pub fn is_integer_type(&self) -> bool {
        match self {
            LLVMType::I(_) |
            LLVMType::Ptr(_) => true,
            LLVMType::Array(_, t) => t.is_integer_type(),
            _ => false,
        }
    }

    pub fn is_float_type(&self) -> bool {
        match self {
            LLVMType::F(_) => true,
            LLVMType::Array(_, t) => t.is_float_type(),
            _ => false,
        }
    }

    pub fn default_value(&self) -> &'static str {
        match self {
            LLVMType::Ptr(_) => "null",
            LLVMType::I(_) => "0",
            LLVMType::F(_) => "0.0",
            LLVMType::V => "void",
            LLVMType::Struct{..} |
            LLVMType::Array(_, _) => unimplemented!(),
        }
    }
}

impl From<TypeRef> for LLVMType {
    fn from(t: TypeRef) -> LLVMType {
        match t.as_ref() {
            Type::Integer => LLVMType::I(32),
            Type::Double => LLVMType::F(64),
            Type::Void => LLVMType::V,
            Type::Boolean => LLVMType::I(1),
            Type::String => LLVMType::Ptr(box LLVMType::I(8)),
            Type::Pointer(t) => LLVMType::Ptr(box t.into()),
            Type::Struct {
                name,
                fields,
            } => {
                LLVMType::Struct {
                    name: name.clone(),
                    size: fields.iter().map(|(_, t)| t.byte_size()).sum(),
                }
            }
        }
    }
}

impl From<&TypeRef> for LLVMType {
    fn from(t: &TypeRef) -> LLVMType {
        t.clone().into()
    }
}

impl From<Type> for LLVMType {
    fn from(t: Type) -> LLVMType {
        match t {
            Type::Integer => LLVMType::I(32),
            Type::Double => LLVMType::F(64),
            Type::Void => LLVMType::V,
            Type::Boolean => LLVMType::I(1),
            Type::String => LLVMType::Ptr(box LLVMType::I(8)),
            Type::Pointer(t) => LLVMType::Ptr(box t.into()),
            Type::Struct {
                name,
                fields,
            } => {
                LLVMType::Struct{
                    name: name.clone(),
                    size: fields.iter().map(|(_, t)| t.byte_size()).sum(),
                }
            }
        }
    }
}

impl Display for LLVMType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use LLVMType::*;
        match self {
            I(bits) => write!(f, "i{}", bits)?,
            F(16) => write!(f, "half")?,
            F(32) => write!(f, "float")?,
            F(64) => write!(f, "double")?,
            F(_) => unimplemented!("Only floating point types of size 16, 32, & 64 are supported."),
            V => write!(f, "void")?,
            Array(len, box t) => write!(f, "[{} x {}]", len, t)?,
            Ptr(box t) => write!(f, "{}*", t)?,
            Struct{name, ..} => write!(f, "%{}", name)?,
        }
        Ok(())
    }
}
