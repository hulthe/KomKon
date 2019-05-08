use crate::ast::{FromPair, Rule, ASTError};
use pest::iterators::Pair;
use std::fmt::{self, Display, Formatter};
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Type {
    Integer,
    Double,
    Boolean,
    Void,
    String,
    Struct { fields: HashMap<String, String> } /* ident => type */,
    Pointer(Box<Type>),
}

pub enum TypeDef {}


impl<'a> FromPair<'a> for Type {
    fn from_pair(pair: Pair<'a, Rule>) -> Result<Self, ASTError> {
        match pair.as_str() {
            "int" => Ok(Type::Integer),
            "double" => Ok(Type::Double),
            "boolean" => Ok(Type::Boolean),
            "void" => Ok(Type::Void),
            t => Err(ASTError::NonExistentType(t.to_owned())),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            Type::Integer => "integer",
            Type::Double => "double",
            Type::Boolean => "boolean",
            Type::Void => "void",
            Type::String => "string",
            Type::Struct { fields: _ } => "",
            Type::Pointer(t) => { write!(f, "*{}", t) }
        })
    }
}

