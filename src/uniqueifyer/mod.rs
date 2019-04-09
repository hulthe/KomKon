use crate::ast::{Program, Node, TopDef, Blk, Stmt, Expr, DeclItem};

#[cfg(test)]
mod test;

struct NameGenerator {
    index: usize,
}

impl NameGenerator {
    pub fn new() -> Self {
        NameGenerator{index: 0}
    }
    pub fn generate(&mut self, stack: &mut Stack) -> String {
        loop {
            let s = format!("v{}", self.index);
            self.index += 1;
            if let None = search_stack(stack, &s) {
                break s;
            }
        }
    }
}

enum StackElem {
    Scope(&'static str),
    Mapping(String, String),
}

type Stack = Vec<StackElem>;

fn search_stack<'a>(stack: &'a Stack, s: &str) -> Option<&'a str> {
    stack.iter()
        .rev()
        .filter_map(|e| match e {
            StackElem::Mapping(from, to) => Some((from.as_ref(), to.as_ref())),
            StackElem::Scope(_) => None
        })
        .find(|(from, _): &(&str, &str)| from == &s)
        .map(|(_, to)| to)
}

/// Pops the stack to and including the latest scope
fn pop_scope(stack: &mut Stack) {
    while let Some(elem) = stack.pop() {
        if let StackElem::Scope(_) = elem {
            return;
        }
    }
    panic!("Stack was empty")
}

fn swap_name(old: &mut String, stack: &Stack) {
    let mut new_name = search_stack(stack, old)
        .expect("Variables used before declaration")
        .to_owned();
    std::mem::swap(&mut new_name, old);
}

pub fn uniqueify(program: &mut Program) {
    let mut stack = vec![];

    stack.push(StackElem::Scope("Global"));
    for td in program.0.iter() {
        // Make sure no auto-generated names conflict with the existing top-def identifiers
        stack.push(StackElem::Mapping(td.elem.ident.clone(), td.elem.ident.clone()));
    }

    for td in program.0.iter_mut() {
        let mut names = NameGenerator::new();
        td.uniqueify(&mut names, &mut stack);
    }
}

trait Uniqueify {
    fn uniqueify(&mut self, names: &mut NameGenerator, stack: &mut Stack);
}

impl<'a, T> Uniqueify for Node<'a, T>
where T: Uniqueify {
    fn uniqueify(&mut self, names: &mut NameGenerator, stack: &mut Stack) {
        self.elem.uniqueify(names, stack)
    }
}

impl<'a> Uniqueify for TopDef<'a> {
    fn uniqueify(&mut self, names: &mut NameGenerator, stack: &mut Stack) {
        stack.push(StackElem::Scope("Function"));
        for a in self.args.iter_mut() {
            let mut new_name = names.generate(stack);
            let m = StackElem::Mapping(a.1.clone(), new_name.clone());
            stack.push(m);
            std::mem::swap(&mut a.1, &mut new_name);
        }
        self.body.uniqueify(names, stack);
        pop_scope(stack);
    }
}

impl<'a> Uniqueify for Blk<'a> {
    fn uniqueify(&mut self, names: &mut NameGenerator, stack: &mut Stack) {
        self.0.iter_mut().for_each(|s| s.uniqueify(names, stack))
    }
}

impl<'a> Uniqueify for Stmt<'a> {
    fn uniqueify(&mut self, names: &mut NameGenerator, stack: &mut Stack) {
        match self {
            Stmt::Return(expr) |
            Stmt::Expression(expr) => expr.uniqueify(names, stack),

            Stmt::If(expr, stmt) |
            Stmt::While(expr, stmt) => {
                expr.uniqueify(names, stack);
                stmt.uniqueify(names, stack);
            }

            Stmt::IfElse(expr, stmt1, stmt2) => {
                expr.uniqueify(names, stack);
                stmt1.uniqueify(names, stack);
                stmt2.uniqueify(names, stack);
            }

            Stmt::Block(blk) => blk.uniqueify(names, stack),

            Stmt::Assignment(ident, expr) => {
                expr.uniqueify(names, stack);
                swap_name(ident, stack);
            }

            Stmt::Increment(ident) |
            Stmt::Decrement(ident) => {
                swap_name(ident, stack);
            }

            Stmt::Declare(_, items) => {
                items.iter_mut()
                    .filter_map(|d| match d {
                        DeclItem::NoInit(_) => None,
                        DeclItem::Init(_, expr) => Some(expr),
                    })
                    .for_each(|expr| expr.uniqueify(names, stack));
                items.iter_mut()
                    .for_each(|item| match item {
                        DeclItem::NoInit(ident) |
                        DeclItem::Init(ident, _) => {
                            let mut new_name = names.generate(stack);
                            stack.push(StackElem::Mapping(ident.to_owned(), new_name.clone()));
                            std::mem::swap(&mut new_name, ident);
                        }
                    });
            }

            Stmt::ReturnVoid |
            Stmt::Empty => {}
        }
    }
}

impl<'a> Uniqueify for Expr<'a> {
    fn uniqueify(&mut self, names: &mut NameGenerator, stack: &mut Stack) {
        match self {
            Expr::Ident(ident) => swap_name(ident, stack),

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
                e1.uniqueify(names, stack);
                e2.uniqueify(names, stack);
            }

            Expr::Neg(expr) |
            Expr::Not(expr) => expr.uniqueify(names, stack),

            Expr::FunctionCall(_, exprs) => exprs
                .iter_mut()
                .for_each(|expr| expr.uniqueify(names, stack)),

            Expr::Double(_) |
            Expr::Integer(_) |
            Expr::Boolean(_) |
            Expr::Str(_) => {}

        }
    }
}
