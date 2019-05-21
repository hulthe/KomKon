use crate::ast::{FromPair, TypeMap, Rule, ASTError};
use pest::iterators::Pair;
use std::fmt::{self, Display, Formatter};
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Integer,
    Double,
    Boolean,
    Void,
    String,
    Struct {
        name: String,
        fields: HashMap<String, TypeRef>,
    },
    Pointer(TypeRef),
}

pub type TypeRef = Rc<Type>;

pub enum TypeDef {}


//impl<'a> FromPair<'a> for Type {
//    fn from_pair(pair: Pair<'a, Rule>, types: &TypeMap) -> Result<Self, ASTError> {
//        match pair.as_str() {
//            "int" => Ok(Type::Integer),
//            "double" => Ok(Type::Double),
//            "boolean" => Ok(Type::Boolean),
//            "void" => Ok(Type::Void),
//            t => Err(ASTError::NonExistentType(t.to_owned())),
//        }
//    }
//}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Type::Integer => write!(f, "integer"),
            Type::Double  => write!(f, "double"),
            Type::Boolean => write!(f, "boolean"),
            Type::Void    => write!(f, "void"),
            Type::String  => write!(f, "string"),
            Type::Struct { name, fields } => unimplemented!("Can't fmt Struct"),
            Type::Pointer(t) => write!(f, "*{}", t),
        }
    }
}

