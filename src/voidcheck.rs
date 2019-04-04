use crate::ast::{Program, Node, TopDef, Blk, Stmt, Expr};
use std::io::{self, Write, StderrLock};
use colored::*;
use crate::CompilerError;

#[derive(Debug)]
pub enum Error {
    VoidExpression
}

impl CompilerError for Error {
    fn display(&self, w: &mut StderrLock, _: &str) -> io::Result<()> {
        match self {
            Error::VoidExpression => {
                write!(w, "  {}\n", "Void expressions are not allowed".bright_red())
            }
        }
    }
}

pub fn void_check(prog: &Program) -> Result<(), Error> {
    prog.check()
}

/// An AST-element which can be checked for illegal void expressions
pub trait VoidCheckable {
    /// Should check that the element and all its children do not include void statements
    /// such as '1;'
    fn check(&self) -> Result<(), Error>;
}

impl<'a> VoidCheckable for Program<'a> {
    fn check(&self) -> Result<(), Error> {
        for td in &self.0 { td.check()?; }
        Ok(())
    }
}

impl<'a> VoidCheckable for TopDef<'a> {
    fn check(&self) -> Result<(), Error> {
        self.body.check()
    }
}

impl<'a, T> VoidCheckable for Node<'a, T>
    where T: VoidCheckable {
    fn check(&self) -> Result<(), Error> {
        self.elem.check()
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

