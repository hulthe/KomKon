use crate::ast::{Program, Node, TopDef, Blk, Stmt, Expr};

/// This trait is for nodes in the AST which can be minimized.
/// e.g. Expr:s on the form Add(Sub(2, 1), 3) should become 4.
pub trait Minimize {
    fn minimize(&mut self);
}

impl<'a> Minimize for Program<'a> {
    fn minimize(&mut self) {
        self.0.iter_mut().for_each(Minimize::minimize);
    }
}

impl<'a, T> Minimize for Node<'a, T>
where T: Minimize {
    fn minimize(&mut self) {
        self.elem.minimize()
    }
}

impl<'a> Minimize for TopDef<'a> {
    fn minimize(&mut self) {
        self.body.minimize()
    }
}

impl<'a> Minimize for Stmt<'a> {
    fn minimize(&mut self) {
        match self {
            Stmt::Expression(expr) |
            Stmt::Assignment(_, expr) |
            Stmt::Return(expr) => expr.minimize(),
            Stmt::While(expr, body) |
            Stmt::If(expr, body) => {
                expr.minimize();
                body.minimize();
            }
            Stmt::IfElse(expr, body1, body2) => {
                expr.minimize();
                body1.minimize();
                body2.minimize();
            }
            Stmt::Block(block) => block.minimize(),

            Stmt::Empty |
            Stmt::ReturnVoid |
            Stmt::Declare(_, _) |
            Stmt::Increment(_) |
            Stmt::Decrement(_) => {}
        }
    }
}

impl<'a> Minimize for Blk<'a> {
    fn minimize(&mut self) {
        for stmt in &mut self.0 {
            stmt.minimize();
        }
    }
}

// Convenience macro for unwrapping Node:s while pattern matching
macro_rules! n {
    ($n:pat) => {Node{ elem: box $n, ..}}
}

impl<'a> Minimize for Expr<'a> {
    fn minimize(&mut self) {
        use self::Expr::*;
        match self {
            Expr::LOr(e1, e2) |
            Expr::LAnd(e1, e2) |
            Expr::GT(e1, e2) |
            Expr::GE(e1, e2) |
            Expr::LT(e1, e2) |
            Expr::LE(e1, e2) |
            Expr::EQ(e1, e2) |
            Expr::NE(e1, e2) |
            Expr::Mul(e1, e2) |
            Expr::Div(e1, e2) |
            Expr::Mod(e1, e2) |
            Expr::Add(e1, e2) |
            Expr::Sub(e1, e2) => {
                e1.minimize();
                e2.minimize();
            }
            Expr::Neg(expr) |
            Expr::Not(expr) => expr.minimize(),
            Expr::FunctionCall(_, exprs) => exprs.iter_mut().for_each(Minimize::minimize),
            Expr::Double(_) |
            Expr::Integer(_) |
            Expr::Boolean(_) |
            Expr::Ident(_) |
            Expr::Str(_) => {}
        }

        let new = match self {
            LOr(n!(Boolean(b1)), n!(Boolean(b2)))  => Some(Boolean(*b1 || *b2)),
            LAnd(n!(Boolean(b1)), n!(Boolean(b2))) => Some(Boolean(*b1 && *b2)),

            GT(n!(Integer(i1)), n!(Integer(i2))) => Some(Boolean(*i1 > *i2)),
            GT(n!(Double(d1)), n!(Double(d2)))   => Some(Boolean(*d1 > *d2)),

            GE(n!(Integer(i1)), n!(Integer(i2))) => Some(Boolean(*i1 >= *i2)),
            GE(n!(Double(d1)), n!(Double(d2)))   => Some(Boolean(*d1 >= *d2)),

            LT(n!(Integer(i1)), n!(Integer(i2))) => Some(Boolean(*i1 < *i2)),
            LT(n!(Double(d1)), n!(Double(d2)))   => Some(Boolean(*d1 < *d2)),

            LE(n!(Integer(i1)), n!(Integer(i2))) => Some(Boolean(*i1 <= *i2)),
            LE(n!(Double(d1)), n!(Double(d2)))   => Some(Boolean(*d1 <= *d2)),

            EQ(n!(Boolean(b1)), n!(Boolean(b2))) => Some(Boolean(*b1 == *b2)),
            EQ(n!(Integer(i1)), n!(Integer(i2))) => Some(Boolean(*i1 == *i2)),
            EQ(n!(Double(d1)), n!(Double(d2)))   => Some(Boolean(*d1 == *d2)),

            NE(n!(Boolean(b1)), n!(Boolean(b2))) => Some(Boolean(*b1 != *b2)),
            NE(n!(Integer(i1)), n!(Integer(i2))) => Some(Boolean(*i1 != *i2)),
            NE(n!(Double(d1)), n!(Double(d2)))   => Some(Boolean(*d1 != *d2)),

            Mul(n!(Integer(i1)), n!(Integer(i2))) => Some(Integer(*i1 * *i2)),
            Mul(n!(Double(d1)), n!(Double(d2)))   => Some(Double(*d1 * *d2)),

            Div(n!(Integer(i1)), n!(Integer(i2))) => Some(Integer(*i1 / *i2)),
            Div(n!(Double(d1)), n!(Double(d2)))   => Some(Double(*d1 / *d2)),

            Mod(n!(Integer(i1)), n!(Integer(i2))) => Some(Integer(*i1 % *i2)),
            Mod(n!(Double(d1)), n!(Double(d2)))   => Some(Double(*d1 % *d2)),

            Add(n!(Integer(i1)), n!(Integer(i2))) => Some(Integer(*i1 + *i2)),
            Add(n!(Double(d1)), n!(Double(d2)))   => Some(Double(*d1 + *d2)),

            Sub(n!(Integer(i1)), n!(Integer(i2))) => Some(Integer(*i1 - *i2)),
            Sub(n!(Double(d1)), n!(Double(d2)))   => Some(Double(*d1 - *d2)),

            Neg(n!(Double(d))) => Some(Double(-*d)),
            Neg(n!(Integer(i))) => Some(Integer(-*i)),

            Not(n!(Boolean(b))) => Some(Boolean(!*b)),
            _ => None,
        };

        if let Some(mut new) = new {
            std::mem::swap(&mut new, self);
        }
    }
}