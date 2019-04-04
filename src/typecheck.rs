use crate::ast::{Type, Program, TopDef, Blk, Stmt, Arg, DeclItem, Expr, Node};
use std::fmt::{Display, Formatter, self};

#[derive(Debug)]
enum StackType {
    Variable(Type, String),
    Function(Type, String, Vec<Arg>),
}

impl StackType {
    pub fn get_ident(&self) -> &str {
        match self {
            StackType::Variable(_, ident) | StackType::Function(_, ident, _) => ident,
        }
    }
}

#[derive(Debug)]
enum StackElem {
    Scope(&'static str),
    Type(StackType),
}

#[derive(Debug)]
pub enum Error<'a> {
    Context(&'a str, ErrorKind),
    NoContext(ErrorKind),
}

#[derive(Debug)]
pub enum ErrorKind {
    Type { expected: Type, got: Type },
    MismatchedType { lhs: Type, rhs: Type },
    InvalidReturnType { expected: Type, got: Type },
    Undeclared {},
    AlreadyDeclared {},
    NotPartialOrdered { got: Type },
    NotOrdered { got: Type },
    NotEq { got: Type },
    NotNumber { got: Type },
    NotAFunction,
    InvalidArgumentCount { expected: usize, got: usize },
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            ErrorKind::Type { expected, got } => write!(f, "Invalid Type. Expected {}. Got {}.", expected, got),
            ErrorKind::MismatchedType { lhs, rhs } => write!(f, "Mismatched Types. {} != {}.", lhs, rhs),
            ErrorKind::InvalidReturnType { expected, got } => write!(f, "Invalid Return Type. Expected {}. Got {}.", expected, got),
            ErrorKind::Undeclared {} => write!(f, "Use of undeclared identifier"),
            ErrorKind::AlreadyDeclared {} => write!(f, "Cannot redeclare variable within the same scope"),
            ErrorKind::NotPartialOrdered { got } => write!(f, "The type {} is not PartialOrd", got),
            ErrorKind::NotOrdered { got } => write!(f, "The type {} is not Ord", got),
            ErrorKind::NotEq { got } => write!(f, "The type {} is not Eq", got),
            ErrorKind::NotNumber { got } => write!(f, "The type {} does not represent a number", got),
            ErrorKind::NotAFunction => write!(f, "Attempted to call a variable as a function"),
            ErrorKind::InvalidArgumentCount { expected, got } => write!(f, "Invalid argument count. Expected {}. Got {}", expected, got),
        }
    }
}

pub fn type_check<'a>(prog: &'a Program) -> Result<(), Error<'a>> {
    use self::{StackType::*};
    let mut stack: Vec<StackElem> = vec![];

    stack.push(StackElem::Type(Function(Type::Void, "printInt".into(), vec![Arg(Type::Integer, "n".into())])));
    stack.push(StackElem::Type(Function(Type::Void, "printDouble".into(), vec![Arg(Type::Double, "x".into())])));
    stack.push(StackElem::Type(Function(Type::Void, "printString".into(), vec![Arg(Type::String, "s".into())])));
    stack.push(StackElem::Type(Function(Type::Integer, "readInt".into(), vec![])));
    stack.push(StackElem::Type(Function(Type::Double, "readDouble".into(), vec![])));

    for td in &prog.0 {
        stack.push(StackElem::Type(Function(
            td.elem.return_type,
            td.elem.ident.clone(),
            td.elem.args.clone(),
        )));
    }

    for td in &prog.0 {
        td.elem.check(&mut stack, Type::Void)?;
    }

    Ok(())
}


/// Pops the stack to and including the latest scope
fn pop_scope(stack: &mut Vec<StackElem>) {
    while let Some(elem) = stack.pop() {
        if let StackElem::Scope(_) = elem {
            return;
        }
    }
    panic!("Stack was empty")
}

trait TypeCheckable<'a> {
    /// Checks the type of the element and its children
    ///
    /// # Arguments
    ///
    /// * `stack` -     A stack on which identifiers and scopes are pushed and checked against
    ///
    /// * `func_type` - The return type of the function being traversed
    ///
    fn check(&'a self, stack: &mut Vec<StackElem>, func_type: Type) -> Result<Type, Error<'a>>;
}

impl<'a, T> TypeCheckable<'a> for Node<'a, T>
    where T: TypeCheckable<'a> {
    fn check(&'a self, stack: &mut Vec<StackElem>, func_type: Type) -> Result<Type, Error<'a>> {
        match self.elem.check(stack, func_type) {
            Err(Error::NoContext(e)) => {
                Err(Error::Context(self.get_slice(), e))
            }
            r => r,
        }
    }
}

impl<'a> TypeCheckable<'a> for TopDef<'a> {
    fn check(&'a self, stack: &mut Vec<StackElem>, func_type: Type) -> Result<Type, Error<'a>> {
        stack.push(StackElem::Scope("Function"));
        for Arg(type_, ident) in &self.args {
            push_stack_def(stack, StackType::Variable(*type_, ident.clone()))?;
        }
        self.body.check(stack, self.return_type)?;
        pop_scope(stack);
        Ok(func_type)
    }
}

impl<'a> TypeCheckable<'a> for Blk<'a> {
    fn check(&'a self, stack: &mut Vec<StackElem>, func_type: Type) -> Result<Type, Error<'a>> {
        stack.push(StackElem::Scope("Block"));
        for st in &self.0 {
            st.check(stack, func_type)?;
        }
        pop_scope(stack);
        Ok(Type::Void)
    }
}

impl<'a> TypeCheckable<'a> for Stmt<'a> {
    fn check(&'a self, stack: &mut Vec<StackElem>, func_type: Type) -> Result<Type, Error<'a>> {
        match self {
            Stmt::Return(expr)
            => { assert_type(func_type, expr.check(stack, func_type)?)?; }

            Stmt::ReturnVoid
            => if func_type == Type::Void {} else {
                return Err(Error::NoContext(ErrorKind::InvalidReturnType { expected: func_type, got: Type::Void }));
            },

            Stmt::If(expr, block) | Stmt::While(expr, block)
            => {
                assert_type(Type::Boolean, expr.check(stack, func_type)?)?;
                block.check(stack, func_type)?;
            }

            Stmt::IfElse(expr, block1, block2)
            => {
                assert_type(Type::Boolean, expr.check(stack, func_type)?)?;
                block1.check(stack, func_type)?;
                block2.check(stack, func_type)?;
            }

            Stmt::Assignment(ident, stmt)
            => if let Ok(StackType::Variable(type_, _)) = search_stack(stack, ident) {
                assert_type(*type_, stmt.check(stack, func_type)?)?;
            } else {
                return Err(Error::NoContext(ErrorKind::Undeclared {}));
            }

            Stmt::Increment(ident) | Stmt::Decrement(ident)
            => match search_stack(stack, ident) {
                Ok(StackType::Variable(type_, _ident))
                => { assert_type(Type::Integer, *type_)?; }

                _ => return Err(Error::NoContext(ErrorKind::Undeclared {}))
            }

            Stmt::Declare(decl_type, decl_items)
            => {
                for item in decl_items {
                    if let DeclItem::Init(_, expr) = item {
                        assert_type(*decl_type, expr.check(stack, func_type)?)?;
                    }
                }
                for item in decl_items {
                    push_stack_def(stack, StackType::Variable(*decl_type, item.get_ident().into()))?;
                }
            }

            Stmt::Block(child) => { child.check(stack, func_type)?; }
            Stmt::Expression(child) => { child.check(stack, func_type)?; }
            Stmt::Empty => {}
        }
        Ok(Type::Void)
    }
}

impl<'a> TypeCheckable<'a> for Expr<'a> {
    fn check(&'a self, stack: &mut Vec<StackElem>, func_type: Type) -> Result<Type, Error<'a>> {
        match self {
            // bool -> bool -> bool
            Expr::LOr(lhs, rhs) |
            Expr::LAnd(lhs, rhs)
            => {
                assert_type(Type::Boolean, lhs.check(stack, func_type)?)?;
                assert_type(Type::Boolean, rhs.check(stack, func_type)?)?;
                Ok(Type::Boolean)
            }

            // PartOrd A => A -> A -> bool
            Expr::GT(lhs, rhs) |
            Expr::LT(lhs, rhs)
            => {
                let lhs = lhs.check(stack, func_type)?;
                let rhs = rhs.check(stack, func_type)?;
                if lhs != rhs {
                    Err(Error::NoContext(ErrorKind::MismatchedType { lhs, rhs }))
                } else if !is_part_ord(&lhs) {
                    Err(Error::NoContext(ErrorKind::NotPartialOrdered { got: lhs }))
                } else {
                    Ok(Type::Boolean)
                }
            }

            // Ord A => A -> A -> bool
            Expr::GE(lhs, rhs) |
            Expr::LE(lhs, rhs)
            => {
                let lhs = lhs.check(stack, func_type)?;
                let rhs = rhs.check(stack, func_type)?;
                if lhs != rhs {
                    Err(Error::NoContext(ErrorKind::MismatchedType { lhs, rhs }))
                } else if !is_ord(&lhs) {
                    Err(Error::NoContext(ErrorKind::NotOrdered { got: lhs }))
                } else {
                    Ok(Type::Boolean)
                }
            }

            // Eq A => A -> A -> bool
            Expr::EQ(lhs, rhs) |
            Expr::NE(lhs, rhs)
            => {
                let lhs = lhs.check(stack, func_type)?;
                let rhs = rhs.check(stack, func_type)?;
                if lhs != rhs {
                    Err(Error::NoContext(ErrorKind::MismatchedType { lhs, rhs }))
                } else if !is_eq(&lhs) {
                    Err(Error::NoContext(ErrorKind::NotEq { got: lhs }))
                } else {
                    Ok(Type::Boolean)
                }
            }

            // Number A => A -> A -> A
            Expr::Mul(lhs, rhs) |
            Expr::Div(lhs, rhs) |
            Expr::Add(lhs, rhs) |
            Expr::Sub(lhs, rhs)
            => {
                let lhs = lhs.check(stack, func_type)?;
                let rhs = rhs.check(stack, func_type)?;
                if lhs != rhs {
                    Err(Error::NoContext(ErrorKind::MismatchedType { lhs, rhs }))
                } else if !is_number(&lhs) {
                    Err(Error::NoContext(ErrorKind::NotNumber { got: lhs }))
                } else {
                    Ok(lhs)
                }
            }

            // Integer A => A -> A -> A
            Expr::Mod(lhs, rhs) => {
                assert_type(Type::Integer, lhs.check(stack, func_type)?)?;
                assert_type(Type::Integer, rhs.check(stack, func_type)?)?;
                Ok(Type::Integer)
            }

            // Number A => A -> A
            Expr::Neg(op)
            => {
                let op_type = op.check(stack, func_type)?;
                if !is_number(&op_type) {
                    Err(Error::NoContext(ErrorKind::NotNumber { got: op_type }))
                } else { Ok(op_type) }
            }

            // bool -> bool
            Expr::Not(op)
            => assert_type(Type::Boolean, op.check(stack, func_type)?),

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
            => match search_stack(stack, ident) {
                Ok(StackType::Variable(t, _)) => Ok(*t),
                Ok(StackType::Function(t, ..)) => Ok(*t),
                _ => Err(Error::NoContext(ErrorKind::Undeclared {}))
            }

            // Function call (type of function, check that expression types and length match)
            Expr::FunctionCall(ident, args)
            => match search_stack(stack, ident) {
                Ok(StackType::Function(t, _, params)) => {
                    let t = *t;
                    if params.len() != args.len() {
                        return Err(Error::NoContext(ErrorKind::InvalidArgumentCount {
                            expected: params.len(),
                            got: args.len(),
                        }));
                    }
                    let it: Vec<_> = args.iter()
                        .zip(params.iter().map(|Arg(param_t, _)| *param_t)).collect();
                    for (arg, param_t) in it {
                        let arg_t = arg.check(stack, func_type)?;
                        if param_t != arg_t {
                            return Err(Error::NoContext(ErrorKind::Type { expected: param_t, got: arg_t }));
                        }
                    }
                    Ok(t)
                }
                Ok(_) => Err(Error::NoContext(ErrorKind::NotAFunction)),
                Err(_) => Err(Error::NoContext(ErrorKind::Undeclared {})),
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
        Type::Double |
        Type::Boolean => true,
        _ => false
    }
}

/// Pushes an identifier onto the stack. If already defined in scope, produces Error
fn push_stack_def<'a>(stack: &mut Vec<StackElem>, elem: StackType) -> Result<(), Error<'a>> {
    if let None = search_stack_scope(stack, elem.get_ident()) {
        stack.push(StackElem::Type(elem));
        Ok(())
    } else {
        Err(Error::NoContext(ErrorKind::AlreadyDeclared {}))
    }
}

/// Searches a stack top to bottom (higher indexes first) for an element with specified identity
fn search_stack<'a>(stack: &'a Vec<StackElem>, ident: &str) -> Result<&'a StackType, ()> {
    stack.iter().rev()
        .filter_map(|it| match it {
            StackElem::Scope(_) => None,
            StackElem::Type(t) => Some(t),
        })
        .find(|it| match it {
            StackType::Function(_, id, _) | StackType::Variable(_, id) => id == ident,
        }).ok_or(())
}

/// As 'search_stack' but limited to the current scope
fn search_stack_scope<'a>(stack: &'a Vec<StackElem>, ident: &str) -> Option<&'a StackType> {
    stack.iter().rev()
        .take_while(|it| match it {
            StackElem::Scope(_) => false,
            _ => true,
        })
        .filter_map(|it| match it {
            StackElem::Scope(_) => None,
            StackElem::Type(t) => Some(t),
        })
        .find(|it| match it {
            StackType::Function(_, id, _) | StackType::Variable(_, id) => id == ident,
        })
}


fn assert_type<'a>(expected: Type, got: Type) -> Result<Type, Error<'a>> {
    if expected == got {
        Ok(got)
    } else {
        Err(Error::NoContext(ErrorKind::Type { expected, got }))
    }
}
