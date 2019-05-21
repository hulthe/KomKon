use crate::ast::{Rule, FromPair, TypeRef, Type, TypeMap, Node, Blk, Expr, DeclItem, VarRef, ASTError};
use pest::iterators::Pair;

#[derive(Debug)]
pub enum Stmt<'a> {
    Return(Node<'a, Expr<'a>>),
    ReturnVoid,
    If(Node<'a, Expr<'a>>, Node<'a, Stmt<'a>>),
    IfElse(Node<'a, Expr<'a>>, Node<'a, Stmt<'a>>, Node<'a, Stmt<'a>>),
    While(Node<'a, Expr<'a>>, Node<'a, Stmt<'a>>),
    Block(Blk<'a>),
    Assignment(VarRef, Node<'a, Expr<'a>>),
    Increment(VarRef),
    Decrement(VarRef),
    Expression(Node<'a, Expr<'a>>),
    Declare(TypeRef, Vec<DeclItem<'a>>),
    Empty,
}

impl<'a> FromPair<'a> for Stmt<'a> {
    fn from_pair(pair: Pair<'a, Rule>, types: &TypeMap) -> Result<Self, ASTError> {
        let rules = pair.into_inner()
            .map(|pair| (pair.as_rule(), pair))
            .collect::<Vec<_>>();
        let stmt = match &rules[..] {
            [(Rule::Return, _), (Rule::Expr, expp)] => Stmt::Return(Node::from_pair(expp.clone(), types)?),

            [(Rule::Return, _), ] => Stmt::ReturnVoid,

            [(Rule::If, _), (Rule::Expr, expr), (Rule::Stmt, stmt1), (Rule::Else, _), (Rule::Stmt, stmt2)]
            => Stmt::IfElse(
                Node::from_pair(expr.clone(), types)?,
                Node::from_pair(stmt1.clone(), types)?,
                Node::from_pair(stmt2.clone(), types)?,
            ),

            [(Rule::If, _), (Rule::Expr, expr), (Rule::Stmt, stmt)]
            => Stmt::If(Node::from_pair(expr.clone(), types)?, Node::from_pair(stmt.clone(), types)?),

            [(Rule::While, _), (Rule::Expr, expr), (Rule::Stmt, stmt)]
            => Stmt::While(Node::from_pair(expr.clone(), types)?, Node::from_pair(stmt.clone(), types)?),

            [(Rule::Blk, blk)] => Stmt::Block(Blk::from_pair(blk.clone(), types)?),

            [(Rule::Variable, idep), (Rule::Assign, _), (Rule::Expr, expp)]
            => Stmt::Assignment(VarRef::from_pair(idep.clone(), types)?, Node::from_pair(expp.clone(), types)?),

            [(Rule::Variable, idep), (Rule::Inc, _)] => Stmt::Increment(VarRef::from_pair(idep.clone(), types)?),

            [(Rule::Variable, idep), (Rule::Dec, _)] => Stmt::Decrement(VarRef::from_pair(idep.clone(), types)?),

            [(Rule::Expr, expr)] => Stmt::Expression(Node::from_pair(expr.clone(), types)?),

            [(Rule::Type, typp), items..] => {
                let items = items.into_iter()
                    .map(|(_, pair)| DeclItem::from_pair(pair.clone(), types))
                    .collect::<Result<Vec<_>, ASTError>>()?;
                Stmt::Declare(types.get(typp.as_str()).unwrap().clone(), items)
            }

            [] => Stmt::Empty,

            _ => Err("No matching rule for Stmt")?,
        };
        Ok(stmt)
    }
}

