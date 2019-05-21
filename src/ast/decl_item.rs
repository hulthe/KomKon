use crate::ast::{Rule, FromPair, Expr, ASTError};
use pest::iterators::Pair;

#[derive(Debug)]
pub enum DeclItem<'a> {
    NoInit(String),
    Init(String, Expr<'a>),
}

impl DeclItem<'_> {
    pub fn get_ident(&self) -> &str {
        match self {
            DeclItem::NoInit(ident) | DeclItem::Init(ident, _) => &ident
        }
    }
}

impl<'a> FromPair<'a> for DeclItem<'a> {
    fn from_pair(pair: Pair<'a, Rule>) -> Result<Self, ASTError> {
        let rules = pair.into_inner()
            .map(|pair| (pair.as_rule(), pair))
            .collect::<Vec<_>>();
        Ok(match &rules[..] {
            [(Rule::Ident, idenp), (Rule::Assign, _), (Rule::Expr, expp)]
            => DeclItem::Init(idenp.as_str().to_owned(), Expr::from_pair(expp.clone())?),

            [(Rule::Ident, idenp)] => DeclItem::NoInit(idenp.as_str().to_owned()),

            _ => Err("No matching rule for DeclItem")?,
        })
    }
}

