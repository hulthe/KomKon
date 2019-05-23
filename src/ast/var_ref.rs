use crate::ast::{Rule, FromPair, TypeMap, Node, ASTError};
use pest::iterators::Pair;
use std::collections::vec_deque::VecDeque;

#[derive(Debug)]
pub enum VarRef<'a> {
    Deref(Node<'a, VarRef<'a>>, String),
    Ident(String),
}

impl<'a> VarRef<'a> {
    pub fn ident(&self) -> &str {
        match self {
            VarRef::Deref(_, name) |
            VarRef::Ident(name) => name,
        }
    }

    fn from_pair_rec(slice: &'a str, rules: &[(Rule, Pair<'a, Rule>)], types: &TypeMap) -> Result<Self, ASTError> {
        Ok(match &rules[..] {
            [head.., (Rule::Deref, _), (Rule::Ident, i)]
            => VarRef::Deref(
                Node::new(Self::from_pair_rec(slice, head, types)?, slice),
                i.as_str().to_owned(),
            ),
            [(Rule::Ident, i)] => VarRef::Ident(i.as_str().to_owned()),

            _ => Err("No matching rule for VarRef")?,
        })
    }
}

impl<'a> FromPair<'a> for VarRef<'a> {
    fn from_pair(pair: Pair<'a, Rule>, types: &TypeMap) -> Result<Self, ASTError> {
        let slice = pair.as_str();
        let rules = pair.into_inner()
            .map(|pair| (pair.as_rule(), pair))
            .collect::<Vec<_>>();
        Self::from_pair_rec(slice, &rules[..], types)
    }
}
