use crate::ast::Type;
use std::fmt::{self, Display, Formatter};

#[derive(Clone, Debug)]
pub enum LLVMType {
    I(u8),
    F(u8),
    V,
    Array(usize, Box<LLVMType>),
    Ptr(Box<LLVMType>),
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
            LLVMType::I(_) => "0",
            LLVMType::F(_) => "0.0",
            LLVMType::V => "void",
            LLVMType::Array(_, _) |
            LLVMType::Ptr(_) => unimplemented!(),
        }
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
        }
        Ok(())
    }
}
