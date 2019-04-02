use crate::ast::{Program, TopDef, Blk};

enum Error {
    NonReturningFunction
}

/// Checks that all branches in all functions in a program return
///
///
/// # Remarks
///
/// Assumes program which passes type checks, i.e. does not control
/// that the return type matches the function declaration
fn return_check(prog: &Program) -> Result<bool, Error> {
    for td in &prog.0 {
        if let TopDef { return_type: Type::Void } = td {
            continue;
        } else if !td.check()? {
            return Err(Error::NonReturningFunction);
            // TODO currently only returns first error
        }
    }
    return Ok(true);
}


trait ReturnCheckable {
    /// Should return Ok(true) iff the branch is returning
    fn check(&self) -> Result<bool, ()>;
}

impl ReturnCheckable for TopDef {
    /// A function is returning iff its block is returning
    fn check(&self) -> Result<bool, ()> {
        self.body.check()
    }
}

impl ReturnCheckable for Blk {
    /// A block is returning iff a statement within the block is returning
    fn check(&self) -> Result<bool, ()> {
        for st in self.0 {
            if st.check()? {
                return Ok(true);
                // TODO this is where could check for unreachable statements
            }
        }
        return false;
    }
}

impl ReturnCheckable for Stmt {
    /// A statement is returning iff it is a Return-statement or
    /// it is a conditional with a constant expression which results
    /// in a returning block to always be evaluated
    fn check(&self) -> Result<bool, ()> {
        match self {
            Stmt::Return(_) => Ok(true),
            Stmt::If(expr, stmt) => {
                if let Expr::Boolean(true) = expr {
                    stmt.check()
                } else { Ok(false) }
            }

            Stmt::IfElse(expr, stmt1, stmt2) =>
                {
                    if let Expr::Boolean(val) = expr {
                        if val {
                            stmt1.check()
                        } else {
                            stmt2.check()
                        }
                    } else {
                        Ok(false)
                    }
                }

            Stmt::While(expr, stmt) => {
                if let Expr::Boolean(true) = expr {
                    stmt.check()
                } else { Ok(false) }
            }

            Stmt::ReturnVoid() => panic!("http://y2u.be/dQw4w9WgXcQ"),

            _ => Ok(false)
        }
    }
}
