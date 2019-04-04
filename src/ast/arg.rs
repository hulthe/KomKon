use crate::ast::{Rule, FromPair, Type, ASTError};
use pest::iterators::Pair;

#[derive(Debug, Clone)]
pub struct Arg(pub Type, pub String);

impl<'a> FromPair<'a> for Arg {
    fn from_pair(pair: Pair<'a, Rule>) -> Result<Self, ASTError> {
        let mut type_ = None;
        let mut ident = None;
        for pair in pair.into_inner() {
            match pair.as_rule() {
                Rule::Type => type_ = Some(Type::from_pair(pair)?),
                Rule::Ident => ident = Some(pair.as_str().to_owned()),
                _ => Err("No matching rule for Arg")?,
            }
        }
        Ok(Arg(type_.unwrap(), ident.unwrap()))
    }
}

