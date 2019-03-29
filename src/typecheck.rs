use crate::ast::{Type, Program, TopDef, Blk, Stmt, Arg, DeclItem, Expr};

enum StackElem {
    Scope(&'static str),
    Variable(Type, String),
    Function(Type, String, Vec<Arg>),
}

enum Error {
    TypeError
}

pub fn type_check(prog: &Program) -> Result<bool, ()> {
    let mut stack = vec![]; // :Vec<StackElem>

    // first pass, push function identities
    for td in &prog.0 {
        stack.push(StackElem::Function(
            td.return_type,
            td.ident.clone(),
            td.args.clone(),
        ));
    }

    // recursively check each function
    for td in &prog.0 {
        td.check(&mut stack, Type::Void)?;
    }

    Ok(true)
}

fn pop_scope(stack: &mut Vec<StackElem>) {
    while let Some(elem) = stack.pop() {
        if let StackElem::Scope(_) = elem {
            return;
        }
    }
    panic!("Stack was empty")
}

trait TypeCheckable {
    /// Checks the type of the element and its children
    ///
    /// # Arguments
    ///
    /// * `stack` -     A stack on which identifiers are pushed and checked against
    ///
    /// * `func_type` - The return type of the function being traversed
    ///
    fn check(&self, stack: &mut Vec<StackElem>, func_type: Type) -> Result<Type, ()>;
}

impl TypeCheckable for TopDef {
    fn check(&self, stack: &mut Vec<StackElem>, func_type: Type) -> Result<Type, ()> {
        stack.push(StackElem::Scope("Function"));
        for Arg(type_, ident) in &self.args {
            stack.push(StackElem::Variable(*type_, ident.clone()));
        }
        self.body.check(stack, self.return_type)?;
        pop_scope(stack);
        Ok(func_type)
    }
}

impl TypeCheckable for Blk
{
    fn check(&self, stack: &mut Vec<StackElem>, func_type: Type) -> Result<Type, ()> {
        stack.push(StackElem::Scope("Block"));
        for st in &self.0 {
            st.check(stack, func_type)?;
        }
        pop_scope(stack);
        Ok(Type::Void)
    }
}

impl TypeCheckable for Stmt {
    fn check(&self, stack: &mut Vec<StackElem>, func_type: Type) -> Result<Type, ()> {
        match self {
            Stmt::Return(expr)
            => if expr.check(stack, func_type)? == func_type { Ok(Type::Void) } else { Err(()) },

            Stmt::ReturnVoid
            => if func_type == Type::Void { Ok(Type::Void) } else { Err(()) },

            Stmt::If(expr, box block) | Stmt::While(expr, box block)
            => if expr.check(stack, func_type)? == Type::Boolean {
                block.check(stack, func_type)?;
                Ok(Type::Void)
            } else { Err(()) },

            Stmt::IfElse(expr, box block1, box block2)
            => if expr.check(stack, func_type)? == Type::Boolean {
                block1.check(stack, func_type)?;
                block2.check(stack, func_type)?;
                Ok(Type::Void)
            } else { Err(()) },

            Stmt::Assignment(ident, stmt)
            => if let StackElem::Variable(type_, _) = search_stack(stack, ident)? {
                if type_.clone() == stmt.check(stack, func_type)? {
                    Ok(Type::Void)
                } else {
                    Err(())
                }
            } else {
                Err(())
            }

            Stmt::Increment(ident) | Stmt::Decrement(ident)
            => match search_stack(stack, ident)? {
                StackElem::Variable(type_, _ident)
                if *type_ == Type::Integer => Ok(Type::Void),

                _ => Err(())
            }

            Stmt::Declare(type_, decl_items)
            => {
                for item in decl_items {
                    if let DeclItem::Init(_, expr) = item {
                        if expr.check(stack, func_type)? != *type_ { return Err(()); }
                    }
                }
                for item in decl_items {
                    stack.push(StackElem::Variable(*type_, item.get_ident().into()))
                }
                Ok(Type::Void)
            }

            Stmt::Block(child) => child.check(stack, func_type),
            Stmt::Expression(child) => child.check(stack, func_type),

            _ => unreachable!("oh noes")
        }
    }
}

impl TypeCheckable for Expr {
    fn check(&self, stack: &mut Vec<StackElem>, func_type: Type) -> Result<Type, ()> {
        match self {
            // bool -> bool -> bool
            Expr::LOr(box lhs, box rhs) |
            Expr::LAnd(box lhs, box rhs)
            => if lhs.check(stack, func_type)? == Type::Boolean &&
                rhs.check(stack, func_type)? == Type::Boolean { Ok(Type::Boolean) } else { Err(()) },

            // PartOrd A => A -> A -> bool
            Expr::GT(box lhs, box rhs) |
            Expr::LT(box lhs, box rhs)
            => {
                let lhs_type = lhs.check(stack, func_type)?;
                let rhs_type = rhs.check(stack, func_type)?;
                if rhs_type == lhs_type &&
                    is_part_ord(&rhs_type) &&
                    is_part_ord(&lhs_type) { Ok(Type::Boolean) } else { Err(()) }
            }

            // Ord A => A -> A -> bool
            Expr::GE(box lhs, box rhs) |
            Expr::LE(box lhs, box rhs)
            => {
                let lhs_type = lhs.check(stack, func_type)?;
                let rhs_type = rhs.check(stack, func_type)?;
                if rhs_type == lhs_type &&
                    is_ord(&rhs_type) &&
                    is_ord(&lhs_type) { Ok(Type::Boolean) } else { Err(()) }
            }

            // Eq A => A -> A -> bool
            Expr::EQ(box lhs, box rhs) |
            Expr::NE(box lhs, box rhs)
            => {
                let lhs_type = lhs.check(stack, func_type)?;
                let rhs_type = rhs.check(stack, func_type)?;
                if rhs_type == lhs_type &&
                    is_eq(&rhs_type) &&
                    is_eq(&lhs_type) { Ok(Type::Boolean) } else { Err(()) }
            }
            // Number A => A -> A -> A
            Expr::Mul(box lhs, box rhs) |
            Expr::Div(box lhs, box rhs) |
            Expr::Mod(box lhs, box rhs) |
            Expr::Add(box lhs, box rhs) |
            Expr::Sub(box lhs, box rhs)
            => {
                let lhs_type = lhs.check(stack, func_type)?;
                let rhs_type = rhs.check(stack, func_type)?;
                if rhs_type == lhs_type &&
                    is_number(&rhs_type) &&
                    is_number(&lhs_type) { Ok(lhs_type) } else { Err(()) }
            }

            // Number A => A -> A
            Expr::Neg(box op)
            => {
                let op_type = op.check(stack, func_type)?;
                if is_number(&op_type) { Ok(op_type) } else { Err(()) }
            }

            // bool -> bool
            Expr::Not(box op)
            => {
                if op.check(stack, func_type)? == Type::Boolean {
                    Ok(Type::Boolean)
                } else { Err(()) }
            }

            // A -> A
            Expr::Double(_)
            => Ok(Type::Double),
            Expr::Integer(_)
            => Ok(Type::Integer),
            Expr::Boolean(_)
            => Ok(Type::Boolean),
            Expr::Str(_)
            => Ok(Type::String),
            Expr::Ident(ident)
            => match search_stack(stack, ident)? {
                StackElem::Variable(t, _) => Ok(*t),
                StackElem::Function(t, ..) => Ok(*t),
                _ => Err(())
            }

            // Function call (type of function, check that expression types match)
            Expr::FunctionCall(ident, args)
            => match search_stack(stack, ident) {
                Ok(StackElem::Function(t, _, params)) => {
                    let t = *t;
                    let it: Vec<_> = args.iter()
                        .zip(params.iter().map(|Arg(param_t, _)| *param_t)).collect();
                    for (arg, param_t) in it {
                        if param_t != arg.check(stack, func_type)? {
                            return Err(());
                        }
                    }
                    Ok(t)
                }
                _ => Err(())
            }
        }
    }
}


fn is_number(t: &Type) -> bool {
    match t {
        Type::Integer |
        Type::Double => true,
        _ => false
    }
}

// TODO: make sure the rights types are ord/part_ord
fn is_ord(t: &Type) -> bool {
    match t {
        Type::Integer |
        Type::Double => true,
        _ => false
    }
}

fn is_part_ord(t: &Type) -> bool {
    match t {
        Type::Integer |
        Type::Double => true,
        _ => false
    }
}

fn is_eq(t: &Type) -> bool {
    match t {
        Type::Integer |
        Type::Boolean => true,
        _ => false
    }
}

/// Searches a stack top to bottom (higher indexes first) for an non-Scope element matching identity
///
/// # Arguments
///
/// * `stack` -
///
/// * `ident` -
fn search_stack<'a>(stack: &'a Vec<StackElem>, ident: &str) -> Result<&'a StackElem, ()> {
    stack.iter().rev().find(|it| match it {
        StackElem::Scope(_) => false,
        StackElem::Function(_, id, _) | StackElem::Variable(_, id)
        => id == ident,
    }).ok_or(())
}
