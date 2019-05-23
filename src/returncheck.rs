use crate::ast::{Program, Node, Function, Blk, Type, Stmt, Expr};
use std::io::{self, Write, StderrLock};
use colored::*;
use crate::CompilerError;

#[derive(Debug)]
pub enum Error {
    NonReturningFunction,
    UnreachableStatement,
}

impl CompilerError for Error {
    fn display(&self, w: &mut StderrLock, _: &str) -> io::Result<()> {
        match self {
            Error::NonReturningFunction => {
                write!(w, "  {}\n", "Function does not always return.".bright_red())
            }
            Error::UnreachableStatement => {
                write!(w, "  {}\n", "Unreachable statement.".bright_red())
            }
        }
    }
}

macro_rules! n {
    ($p:pat) => {Node{ elem: box $p, .. }}
}

/// Checks that functions in the program aren't obviously non-returning
///
/// # Remarks
///
/// Assumes program which passes type checks, i.e. does not control
/// that the return type matches the function declaration
///
/// Assumes simplified constants, e.g. 1==1 should have been replaced with true
pub fn return_check(prog: &Program) -> Result<bool, Error> {
    for td in &prog.functions {
        let n!(Function { return_type, .. }) = td;
        if return_type.as_ref() == &Type::Void {
            td.check()?;    // will error on unreachable statement
        } else {
            if !td.check()? {
                return Err(Error::NonReturningFunction);
            }
        }
    }
    return Ok(true);
}


trait ReturnCheckable {
    /// Should return Ok(true) iff the branch is returning
    fn check(&self) -> Result<bool, Error>;
}

impl ReturnCheckable for Function<'_> {
    /// A function is returning iff its block is returning
    fn check(&self) -> Result<bool, Error> {
        self.body.check()
    }
}

impl<T> ReturnCheckable for Node<'_, T>
    where T: ReturnCheckable {
    fn check(&self) -> Result<bool, Error> {
        self.elem.check()
    }
}

impl ReturnCheckable for Blk<'_> {
    /// A block is returning iff a statement within the block is returning
    fn check(&self) -> Result<bool, Error> {
        for st in &self.0 {
            if st.check()? {
                return Ok(true);
                // TODO this is where could check for unreachable statements
            }
        }
        return Ok(false);
    }
}

impl ReturnCheckable for Stmt<'_> {
    /// A statement is returning iff it is a Return-statement or
    /// it is a conditional with a constant expression which results
    /// in a returning block to always be evaluated, or if all branches
    /// are returning
    fn check(&self) -> Result<bool, Error> {
        match self {
            Stmt::Return(_) => {
                Ok(true)
            }
            Stmt::IfElse(expr, stmt1, stmt2) => {
                let true_branch = stmt1.check()?;
                let false_branch = stmt2.check()?;

                // if constant expr, only evaluate corresponding branch
                match expr {
                    n!(Expr::Boolean(true)) => Ok(true_branch),
                    n!(Expr::Boolean(false)) => Ok(false_branch),
                    _ => Ok(true_branch && false_branch)
                }
            }

            Stmt::If(expr, stmt) => {
                if let n!(Expr::Boolean(true)) = expr {
                    stmt.check()
                } else { Ok(false) }
            }

            Stmt::While(expr, stmt) => {
                if let n!(Expr::Boolean(true)) = expr {
                    stmt.check()
                } else { Ok(false) }
            }

            Stmt::Block(blk) => blk.check(),

            _ => Ok(false)
        }
    }
}
