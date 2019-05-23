use crate::ast::{Rule, FromPair, TypeRef, TypeMap, ASTError};
use pest::iterators::Pair;

#[derive(Debug, Clone)]
pub struct Arg(pub TypeRef, pub String);

impl<'a> FromPair<'a> for Arg {
    fn from_pair(pair: Pair<'a, Rule>, types: &TypeMap) -> Result<Self, ASTError> {
        let mut type_ = None;
        let mut ident = None;
        for pair in pair.into_inner() {
            match pair.as_rule() {
                Rule::Type => type_ = Some(types.get_or(pair.as_str())?.clone()),
                Rule::Ident => ident = Some(pair.as_str().to_owned()),
                _ => Err("No matching rule for Arg")?,
            }
        }
        Ok(Arg(type_.unwrap(), ident.unwrap()))
    }
}

impl Arg {
    pub fn from_pair_str<'a>(pair: Pair<'a, Rule>) -> Result<(&'a str, &'a str), ASTError> {
        let mut type_ = None;
        let mut ident = None;
        for pair in pair.into_inner() {
            match pair.as_rule() {
                Rule::Type => type_ = Some(pair.as_str()),
                Rule::Ident => ident = Some(pair.as_str()),
                _ => Err("No matching rule for Arg")?,
            }
        }
        Ok((type_.unwrap(), ident.unwrap()))
    }
}

