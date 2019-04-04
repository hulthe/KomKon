use crate::ast::{Rule, FromPair, Node, Stmt, ASTError};
use pest::iterators::Pair;

#[derive(Debug)]
pub struct Blk<'a>(pub Vec<Node<'a, Stmt<'a>>>);

impl<'a> FromPair<'a> for Blk<'a> {
    fn from_pair(pair: Pair<'a, Rule>) -> Result<Self, ASTError> {
        let statements = pair.into_inner()
            .filter(|pair| pair.as_rule() == Rule::Stmt)
            .map(|p| (p.as_str(), Stmt::from_pair(p)))
            .map(|(s, r)| r.map(|stmt| Node::new(stmt, s)))
            .collect::<Result<Vec<_>, ASTError>>()?;
        Ok(Blk(statements))
    }
}

