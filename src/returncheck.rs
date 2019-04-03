use crate::ast::{Program, TopDef, Blk, Type, Stmt, Expr};

#[derive(Debug)]
pub enum Error {
    NonReturningFunction,
    UnreachableStatement,
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
    for td in &prog.0 {
        if let TopDef { return_type: Type::Void, ident: _, args: _, body: _ } = td {
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

impl ReturnCheckable for TopDef {
    /// A function is returning iff its block is returning
    fn check(&self) -> Result<bool, Error> {
        self.body.check()
    }
}

impl ReturnCheckable for Blk {
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

impl ReturnCheckable for Stmt {
    /// A statement is returning iff it is a Return-statement or
    /// it is a conditional with a constant expression which results
    /// in a returning block to always be evaluated, or if all branches
    /// are returning
    fn check(&self) -> Result<bool, Error> {
        match self {
            Stmt::Return(_) => {
                Ok(true)
            }
            Stmt::IfElse(expr, stmt1, stmt2) =>
                {
                    let true_branch = stmt1.check()?;
                    let false_branch = stmt2.check()?;
                    // if constant expr, only evaluate corresponding branch
                    if let Expr::Boolean(val) = expr {
                        if *val {
                            Ok(true_branch)
                        } else {
                            Ok(false_branch)
                        }
                    } else {
                        // otherwise, both must return
                        Ok(true_branch && false_branch)
                    }
                }

            Stmt::If(expr, stmt) => {
                if let Expr::Boolean(true) = expr {
                    stmt.check()
                } else {
                    Ok(false)
                }
            }

            Stmt::While(expr, stmt) => {
                if let Expr::Boolean(true) = expr {
                    stmt.check()
                } else { Ok(false) }
            }

            Stmt::Block(blk) => blk.check(),

            //  Stmt::ReturnVoid() => panic!("http://y2u.be/dQw4w9WgXcQ"),

            _ => Ok(false)
        }
    }
}
