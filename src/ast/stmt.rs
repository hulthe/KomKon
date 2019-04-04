use crate::ast::{Rule, FromPair, Type, Node, Blk, Expr, DeclItem, ASTError};
use pest::iterators::Pair;

#[derive(Debug)]
pub enum Stmt<'a> {
    Return(Expr<'a>),
    ReturnVoid,
    If(Expr<'a>, Node<'a, Stmt<'a>>),
    IfElse(Expr<'a>, Node<'a, Stmt<'a>>, Node<'a, Stmt<'a>>),
    While(Expr<'a>, Node<'a, Stmt<'a>>),
    Block(Blk<'a>),
    Assignment(String, Expr<'a>),
    Increment(String),
    Decrement(String),
    Expression(Expr<'a>),
    Declare(Type, Vec<DeclItem<'a>>),
    Empty,
}

impl<'a> FromPair<'a> for Stmt<'a> {
    fn from_pair(pair: Pair<'a, Rule>) -> Result<Self, ASTError> {
        let rules = pair.into_inner()
            .map(|pair| (pair.as_rule(), pair))
            .collect::<Vec<_>>();
        let stmt = match &rules[..] {
            [(Rule::Return, _), (Rule::Expr, expp)] => Stmt::Return(Expr::from_pair(expp.clone())?),

            [(Rule::Return, _), ] => Stmt::ReturnVoid,

            [(Rule::If, _), (Rule::Expr, expr), (Rule::Stmt, stmt1), (Rule::Else, _), (Rule::Stmt, stmt2)]
            => Stmt::IfElse(
                Expr::from_pair(expr.clone())?,
                Node::from_pair(stmt1.clone())?,
                Node::from_pair(stmt2.clone())?,
            ),

            [(Rule::If, _), (Rule::Expr, expr), (Rule::Stmt, stmt)]
            => Stmt::If(Expr::from_pair(expr.clone())?, Node::from_pair(stmt.clone())?),

            [(Rule::While, _), (Rule::Expr, expr), (Rule::Stmt, stmt)]
            => Stmt::While(Expr::from_pair(expr.clone())?, Node::from_pair(stmt.clone())?),

            [(Rule::Blk, blk)] => Stmt::Block(Blk::from_pair(blk.clone())?),

            [(Rule::Ident, idep), (Rule::Assign, _), (Rule::Expr, expp)]
            => Stmt::Assignment(idep.as_str().to_owned(), Expr::from_pair(expp.clone())?),

            [(Rule::Ident, idep), (Rule::Inc, _)] => Stmt::Increment(idep.as_str().to_owned()),

            [(Rule::Ident, idep), (Rule::Dec, _)] => Stmt::Decrement(idep.as_str().to_owned()),

            [(Rule::Expr, expr)] => Stmt::Expression(Expr::from_pair(expr.clone())?),

            [(Rule::Type, typp), items..] => {
                let items = items.into_iter()
                    .map(|(_, pair)| DeclItem::from_pair(pair.clone()))
                    .collect::<Result<Vec<_>, ASTError>>()?;
                Stmt::Declare(Type::from_pair(typp.clone())?, items)
            }

            [] => Stmt::Empty,

            _ => Err("No matching rule for Stmt")?,
        };
        Ok(stmt)
    }
}

