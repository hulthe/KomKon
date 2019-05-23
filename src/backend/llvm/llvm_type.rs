use crate::ast::{Type, TypeRef};
use crate::util::write_list;
use std::fmt::{self, Display, Formatter};

#[derive(Clone, Debug)]
pub enum LLVMType {
    I(u8),
    F(u8),
    V,
    Array(usize, Box<LLVMType>),
    Ptr(Box<LLVMType>),
    Struct(Vec<LLVMType>),
}

impl LLVMType {
    pub fn bytes(&self) -> usize {
        match self {
            &LLVMType::V => 0,
            &LLVMType::F(n) |
            &LLVMType::I(n) => n as usize / 8,
            // 32 or 64 bits depending on architecure
            LLVMType::Ptr(_) => std::mem::size_of::<usize>(),
            LLVMType::Array(n, t) => t.bytes() * n,
            LLVMType::Struct(fields) => fields.iter()
                                            .map(|f| f.bytes())
                                            .sum(),
        }
    }

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
            LLVMType::Struct(_) |
            LLVMType::Array(_, _) |
            LLVMType::Ptr(_) => unimplemented!(),
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
                LLVMType::Struct(fields.iter().map(|(_, t)| t.into()).collect())
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
            _ => unimplemented!(),
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
            Struct(fields) => {
                write!(f, "{{ ")?;
                write_list(f, ", ", fields.iter(), |f, field| write!(f, "{}", field))?;
                write!(f, " }}")?;
            }
        }
        Ok(())
    }
}
