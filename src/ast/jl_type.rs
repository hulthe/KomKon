use crate::ast::{FromPair, TypeMap, Rule, ASTError};
use pest::iterators::Pair;
use std::fmt::{self, Debug, Display, Formatter};
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone)]
pub enum Type {
    Integer,
    Double,
    Boolean,
    Void,
    String,
    Struct {
        name: String,
        fields: Vec<(String, TypeRef)>,
    },
    Pointer(TypeRef),
}

impl Type {
    pub fn byte_size(&self) -> usize {
        match self {
            &Type::Void => 0,
            &Type::Boolean => 1,
            &Type::Integer => 4,
            &Type::Double => 8,
            // 32 or 64 bits depending on architecure
            Type::String |
            Type::Pointer(_) => std::mem::size_of::<usize>(),
            Type::Struct{fields, ..} => fields.iter().map(|(_, t)| t.byte_size()).sum(),
        }
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Void, Type::Void) => true,
            (Type::Double, Type::Double) => true,
            (Type::String, Type::String) => true,
            (Type::Integer, Type::Integer) => true,
            (Type::Boolean, Type::Boolean) => true,
            (Type::Pointer(t1), Type::Pointer(t2)) => t1 == t2,
            (Type::Struct{name: n1, ..}, Type::Struct{name: n2, ..}) => n1 == n2,
            _ => false,
        }
    }
}

pub type TypeRef = Rc<Type>;

impl Debug for Type {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Type::Integer => write!(f, "integer"),
            Type::Double  => write!(f, "double"),
            Type::Boolean => write!(f, "boolean"),
            Type::Void    => write!(f, "void"),
            Type::String  => write!(f, "string"),
            Type::Struct { name, .. } => write!(f, "{}", name),
            Type::Pointer(t) => write!(f, "*{}", t),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

