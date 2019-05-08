use crate::ast::{Rule, FromPair, ASTError};
use pest::iterators::Pair;

#[derive(Debug)]
pub enum VarRef {
    Deref(String, Box<VarRef>),
    Ident(String),
}

impl FromPair<'_> for VarRef {
    fn from_pair(pair: Pair<'_, Rule>) -> Result<Self, ASTError> {
        let rules = pair.into_inner()
            .map(|pair| (pair.as_rule(), pair))
            .collect::<Vec<_>>();
        Ok(match &rules[..] {
            [(Rule::Ident, i), (Rule::Deref, _), (Rule::Variable, v)]
                => VarRef::Deref(
                    i.as_str().to_owned(),
                    box VarRef::from_pair(v.clone())?,
                ),
            [(Rule::Ident, i)]
                => VarRef::Ident(i.as_str().to_owned()),
            _ => Err("No matching rule for VarRef")?,
        })
    }
}

