use crate::ast::{Program, Node, Function, Blk, Stmt, Expr, DeclItem};

/// This trait is for nodes in the AST which can be minimized.
/// e.g. Expr:s on the form Add(Sub(2, 1), 3) should become 4.
pub trait Minimize {
    fn minimize(&mut self);
}

impl Minimize for Program<'_> {
    fn minimize(&mut self) {
        self.functions.iter_mut().for_each(Minimize::minimize);
    }
}

impl<T> Minimize for Node<'_, T>
where T: Minimize {
    fn minimize(&mut self) {
        self.elem.minimize()
    }
}

impl Minimize for Function<'_> {
    fn minimize(&mut self) {
        self.body.minimize()
    }
}

impl Minimize for Stmt<'_> {
    fn minimize(&mut self) {
        match self {
            Stmt::Expression(expr) |
            Stmt::Assignment(_, expr) |
            Stmt::Return(expr) => expr.minimize(),
            Stmt::While(expr, body) => {
                expr.minimize();
                body.minimize();
            }
            Stmt::If(expr, body) => {
                expr.minimize();
                body.minimize();
                if let box Expr::Boolean(true) = expr.elem {
                    let mut new_stmt = Stmt::Empty;
                    std::mem::swap(&mut new_stmt, body.elem.as_mut());
                    std::mem::swap(&mut new_stmt, self);
                }
            }
            Stmt::IfElse(expr, body1, body2) => {
                expr.minimize();
                body1.minimize();
                body2.minimize();

                match expr.elem {
                    box Expr::Boolean(true) => {
                        let mut new_stmt = Stmt::Empty;
                        std::mem::swap(&mut new_stmt, body1.elem.as_mut());
                        std::mem::swap(&mut new_stmt, self);
                    }
                    box Expr::Boolean(false) => {
                        let mut new_stmt = Stmt::Empty;
                        std::mem::swap(&mut new_stmt, body2.elem.as_mut());
                        std::mem::swap(&mut new_stmt, self);
                    }
                    _ => {}
                }
            }
            Stmt::Block(block) => block.minimize(),

            Stmt::Declare(_, items) => items.iter_mut().for_each(Minimize::minimize),

            Stmt::Empty |
            Stmt::ReturnVoid |
            Stmt::Increment(_) |
            Stmt::Decrement(_) => {}
        }
    }
}

impl Minimize for Blk<'_> {
    fn minimize(&mut self) {
        for stmt in &mut self.0 {
            stmt.minimize();
        }
    }
}

impl Minimize for DeclItem<'_> {
    fn minimize(&mut self) {
        if let DeclItem::Init(_, expr) = self {
            expr.minimize();
        }
    }
}

// Convenience macro for unwrapping Node:s while pattern matching
macro_rules! n {
    ($n:pat) => {Node{ elem: box $n, ..}}
}

impl Minimize for Expr<'_> {
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
            Expr::Var(_) |
            Expr::NullPtr(_) |
            Expr::New(_) |
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
