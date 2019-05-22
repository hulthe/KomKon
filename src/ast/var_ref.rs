use crate::ast::{Rule, FromPair, TypeMap, ASTError};
use pest::iterators::Pair;
use crate::ast::expr::Expr;
use std::collections::vec_deque::VecDeque;
use crate::ast::jl_type::Type;

#[derive(Debug)]
pub enum VarRef {
    Deref(String, Box<VarRef>),
    Ident(String),
}

impl VarRef {
    pub fn ident(&self) -> &str {
        match self {
            VarRef::Deref(name, _) |
            VarRef::Ident(name) => name,
        }
    }
}

impl FromPair<'_> for VarRef {
    fn from_pair(pair: Pair<'_, Rule>, types: &TypeMap) -> Result<Self, ASTError> {
        let rules = pair.into_inner()
            .map(|pair| (pair.as_rule(), pair))
            .collect::<Vec<_>>();
        Ok(match &rules[..] {
            [(Rule::Ident, i), (Rule::Deref, _), (Rule::Variable, v)]
            => VarRef::Deref(
                i.as_str().to_owned(),
                box VarRef::from_pair(v.clone(), types)?,
            ),
            [(Rule::Ident, i)] => VarRef::Ident(i.as_str().to_owned()),

            _ => Err("No matching rule for VarRef")?,
        })
    }
}

/// Flattens a recursive VarRef-structure into a Vec<String>
fn flatten_var_ref(vr: VarRef) -> (VecDeque<String>) {
    match vr {
        VarRef::Deref(ident, next) => {
            let mut vec = flatten_var_ref(*next);
            vec.push_front(ident);
            return vec;
        }
        VarRef::Ident(id) => {
            let mut vec = VecDeque::new();
            vec.push_front(id);
            return vec;
        }
    }
}