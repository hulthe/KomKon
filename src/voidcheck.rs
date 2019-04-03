use crate::ast::{Program, Node, TopDef, Blk, Stmt, Expr};

#[derive(Debug)]
pub enum Error {
    VoidExpression
}

pub fn void_check(prog: &Program) -> Result<(), Error> {
    prog.check()
}

pub trait VoidCheckable {
    /// Checks that
    fn check(&self) -> Result<(), Error>;
}

impl<'a> VoidCheckable for Program<'a> {
    fn check(&self) -> Result<(), Error>
    {
        for td in &self.0 { td.check()?; }
        Ok(())
    }
}

impl<'a, T> VoidCheckable for Node<'a, T>
    where T: VoidCheckable {
    fn check(&self) -> Result<(), Error> {
        self.elem.check()
    }
}

impl<'a> VoidCheckable for TopDef<'a> {
    fn check(&self) -> Result<(), Error> {
        self.body.check()
    }
}

impl<'a> VoidCheckable for Stmt<'a> {
    fn check(&self) -> Result<(), Error> {
        match self {
            Stmt::Expression(child)
            => match child {
                Expr::Double(_)
                | Expr::Integer(_)
                | Expr::Boolean(_)
                | Expr::Ident(_)
                | Expr::Str(_)
                => Err(Error::VoidExpression),
                _ => Ok(())
            }

            Stmt::Block(child)
            => child.check(),

            Stmt::If(_, child)
            | Stmt::While(_, child)
            => child.check(),

            Stmt::IfElse(_, child1, child2)
            => {
                child1.check()?;
                child2.check()?;
                Ok(())
            }

            _ => Ok(())
        }
    }
}

impl<'a> VoidCheckable for Blk<'a> {
    fn check(&self) -> Result<(), Error> {
        for stmt in &self.0 {
            stmt.check()?;
        }
        Ok(())
    }
}