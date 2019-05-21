use crate::CompilerError;
use crate::ast::{Type, TypeRef, Program, Function, Blk, Stmt, Arg, DeclItem, Expr, VarRef, Node};
use crate::util::{get_internal_slice_pos, byte_pos_to_line, print_error};
use colored::*;
use std::io::{self, Write, StderrLock};
use std::fmt::{Display, Formatter, self};

#[derive(Debug)]
enum StackType {
    Variable(TypeRef, String),
    Function(TypeRef, String, Vec<Arg>),
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

impl CompilerError for Error<'_> {
    fn display(&self, w: &mut StderrLock, source_code: &str) -> io::Result<()> {
        let kind = match self {
            Error::NoContext(kind) => kind,
            Error::Context(s, kind) => {
                if let Some((i, len)) = get_internal_slice_pos(source_code, s) {
                    let j = i + len;
                    let i = byte_pos_to_line(source_code, i);
                    let j = byte_pos_to_line(source_code, j);
                    print_error(w, source_code, &format!("{}", kind), i, j)?;
                    return Ok(());
                }
                kind
            }
        };
        write!(w, "  {}\n", &format!("{}", kind).bright_red())?;
        Ok(())
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    Type { expected: TypeRef, got: TypeRef },
    MismatchedType { lhs: TypeRef, rhs: TypeRef },
    InvalidReturnType { expected: TypeRef, got: TypeRef },
    Undeclared {},
    AlreadyDeclared {},
    NotPartialOrdered { got: TypeRef },
    NotOrdered { got: TypeRef },
    NotEq { got: TypeRef },
    NotNumber { got: TypeRef },
    NotAFunction,
    InvalidArgumentCount { expected: usize, got: usize },
    InvalidMainDef,
    MissingMain,
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            ErrorKind::Type { expected, got } => write!(f, "Invalid Type. Expected {}. Got {}.", expected, got),
            ErrorKind::MismatchedType { lhs, rhs } => write!(f, "Mismatched Types. {} != {}.", lhs, rhs),
            ErrorKind::InvalidReturnType { expected, got } => write!(f, "Invalid Return Type. Expected {}. Got {}.", expected, got),
            ErrorKind::Undeclared {} => write!(f, "Use of undeclared identifier"),
            ErrorKind::AlreadyDeclared {} => write!(f, "Cannot redeclare a function or a variable within the same scope"),
            ErrorKind::NotPartialOrdered { got } => write!(f, "The type {} is not PartialOrd", got),
            ErrorKind::NotOrdered { got } => write!(f, "The type {} is not Ord", got),
            ErrorKind::NotEq { got } => write!(f, "The type {} is not Eq", got),
            ErrorKind::NotNumber { got } => write!(f, "The type {} does not represent a number", got),
            ErrorKind::NotAFunction => write!(f, "Attempted to call a variable as a function"),
            ErrorKind::InvalidArgumentCount { expected, got } => write!(f, "Invalid argument count. Expected {}. Got {}", expected, got),
            ErrorKind::InvalidMainDef => write!(f, "Invalid definition of \"main\".\nMust have return type int and no parameters."),
            ErrorKind::MissingMain => write!(f, "Function \"main\" must be defined."),
        }
    }
}

pub fn type_check<'a>(prog: &mut Program<'a>) -> Result<(), Error<'a>> {
    use self::{StackType::*};
    let mut stack: Vec<StackElem> = vec![];

    stack.push(StackElem::Type(Function(Type::Void.into(), "printInt".into(), vec![Arg(Type::Integer.into(), "n".into())])));
    stack.push(StackElem::Type(Function(Type::Void.into(), "printDouble".into(), vec![Arg(Type::Double.into(), "x".into())])));
    stack.push(StackElem::Type(Function(Type::Void.into(), "printString".into(), vec![Arg(Type::String.into(), "s".into())])));
    stack.push(StackElem::Type(Function(Type::Integer.into(), "readInt".into(), vec![])));
    stack.push(StackElem::Type(Function(Type::Double.into(), "readDouble".into(), vec![])));

    for f in &prog.functions {
        if f.elem.ident == "main" {
            if f.elem.return_type != Type::Integer.into() ||
                f.elem.args.len() != 0 {
                return Err(Error::Context(f.get_slice(), ErrorKind::InvalidMainDef));
            }
        }
        push_stack_def(&mut stack, Function(
            f.elem.return_type.clone().into(),
            f.elem.ident.clone(),
            f.elem.args.clone(),
        ))?;
    }

    search_stack(&stack, "main").ok_or(Error::NoContext(ErrorKind::MissingMain))?;

    for f in &mut prog.functions {
        f.elem.check(&mut stack, Type::Void.into())?;
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
    fn check(&mut self, stack: &mut Vec<StackElem>, func_type: TypeRef) -> Result<TypeRef, Error<'a>>;
}

impl<'a, T> TypeCheckable<'a> for Node<'a, T>
    where T: TypeCheckable<'a> {
    fn check(&mut self, stack: &mut Vec<StackElem>, func_type: TypeRef) -> Result<TypeRef, Error<'a>> {
        match self.elem.check(stack, func_type) {
            Err(Error::NoContext(e)) => {
                Err(Error::Context(self.get_slice(), e))
            }

            Ok(tp) => {
                self.tp = Some(tp.clone().into());
                Ok(tp)
            }

            r => r,
        }
    }
}

impl<'a> TypeCheckable<'a> for Function<'a> {
    fn check(&mut self, stack: &mut Vec<StackElem>, _: TypeRef) -> Result<TypeRef, Error<'a>> {
        stack.push(StackElem::Scope("Function"));
        for Arg(type_, ident) in &self.args {
            push_stack_def(stack, StackType::Variable((type_.clone()).into(), ident.clone()))?;
        }
        self.body.check(stack, self.return_type.clone().into())?;
        pop_scope(stack);
        Ok(self.return_type.clone())
    }
}

impl<'a> TypeCheckable<'a> for Blk<'a> {
    fn check(&mut self, stack: &mut Vec<StackElem>, func_type: TypeRef) -> Result<TypeRef, Error<'a>> {
        stack.push(StackElem::Scope("Block"));
        for st in &mut self.0 {
            st.check(stack, func_type.clone())?;
        }
        pop_scope(stack);
        Ok(Type::Void.into())
    }
}

impl<'a> TypeCheckable<'a> for Stmt<'a> {
    fn check(&mut self, stack: &mut Vec<StackElem>, func_type: TypeRef) -> Result<TypeRef, Error<'a>> {
        match self {
            Stmt::Return(expr) => {
                assert_type(func_type.clone(), expr.check(stack, func_type.clone())?)?;
                return Ok(func_type);
            }

            Stmt::ReturnVoid
            => if func_type == Type::Void.into() {} else {
                return Err(Error::NoContext(ErrorKind::InvalidReturnType { expected: func_type, got: Type::Void.into() }));
            },

            Stmt::If(expr, block) | Stmt::While(expr, block)
            => {
                assert_type(Type::Boolean.into(), expr.check(stack, func_type.clone())?)?;
                block.check(stack, func_type.clone())?;
            }

            Stmt::IfElse(expr, block1, block2)
            => {
                assert_type(Type::Boolean.into(), expr.check(stack, func_type.clone())?)?;
                block1.check(stack, func_type.clone())?;
                block2.check(stack, func_type.clone())?;
            }

            Stmt::Assignment(VarRef::Deref(_, _), _) => unimplemented!("Pointer deref typecheck"),
            Stmt::Assignment(VarRef::Ident(ident), stmt)
            => if let Some(StackType::Variable(type_, _)) = search_stack(stack, ident) {
                assert_type(type_.clone(), stmt.check(stack, func_type)?)?;
            } else {
                return Err(Error::NoContext(ErrorKind::Undeclared {}));
            }

            Stmt::Increment(VarRef::Deref(_, _)) |
            Stmt::Decrement(VarRef::Deref(_, _)) => unimplemented!("Pointer deref typecheck"),

            Stmt::Increment(VarRef::Ident(ident)) | Stmt::Decrement(VarRef::Ident(ident))
            => match search_stack(stack, ident) {
                Some(StackType::Variable(type_, _ident))
                => { assert_type(Type::Integer.into(), type_.clone())?; }

                _ => return Err(Error::NoContext(ErrorKind::Undeclared {}))
            }

            Stmt::Declare(decl_type, decl_items)
            => {
                for item in decl_items.iter_mut() {
                    if let DeclItem::Init(_, expr) = item {
                        assert_type(decl_type.clone(), expr.check(stack, func_type.clone())?)?;
                    }
                }
                for item in decl_items.iter_mut() {
                    push_stack_def(stack, StackType::Variable(decl_type.clone(), item.get_ident().into()))?;
                }
            }

            Stmt::Block(child) => { child.check(stack, func_type)?; }
            Stmt::Expression(child) => {
                assert_type(Type::Void.into(), child.check(stack, func_type)?)?;
            }
            Stmt::Empty => {}
        }
        Ok(Type::Void.into())
    }
}

impl<'a> TypeCheckable<'a> for Expr<'a> {
    fn check(&mut self, stack: &mut Vec<StackElem>, func_type: TypeRef) -> Result<TypeRef, Error<'a>> {
        match self {
            // bool -> bool -> bool
            Expr::LOr(lhs, rhs) |
            Expr::LAnd(lhs, rhs)
            => {
                assert_type(Type::Boolean.into(), lhs.check(stack, func_type.clone())?)?;
                assert_type(Type::Boolean.into(), rhs.check(stack, func_type.clone())?)?;
                Ok(Type::Boolean.into())
            }

            // PartOrd A => A -> A -> bool
            Expr::GT(lhs, rhs) |
            Expr::LT(lhs, rhs)
            => {
                let lhs = lhs.check(stack, func_type.clone())?;
                let rhs = rhs.check(stack, func_type.clone())?;
                if lhs != rhs {
                    Err(Error::NoContext(ErrorKind::MismatchedType { lhs, rhs }))
                } else if !is_part_ord(&lhs) {
                    Err(Error::NoContext(ErrorKind::NotPartialOrdered { got: lhs }))
                } else {
                    Ok(Type::Boolean.into())
                }
            }

            // Ord A => A -> A -> bool
            Expr::GE(lhs, rhs) |
            Expr::LE(lhs, rhs)
            => {
                let lhs = lhs.check(stack, func_type.clone())?;
                let rhs = rhs.check(stack, func_type.clone())?;
                if lhs != rhs {
                    Err(Error::NoContext(ErrorKind::MismatchedType { lhs, rhs }))
                } else if !is_ord(&lhs) {
                    Err(Error::NoContext(ErrorKind::NotOrdered { got: lhs }))
                } else {
                    Ok(Type::Boolean.into())
                }
            }

            // Eq A => A -> A -> bool
            Expr::EQ(lhs, rhs) |
            Expr::NE(lhs, rhs)
            => {
                let lhs = lhs.check(stack, func_type.clone())?;
                let rhs = rhs.check(stack, func_type.clone())?;
                if lhs != rhs {
                    Err(Error::NoContext(ErrorKind::MismatchedType { lhs, rhs }))
                } else if !is_eq(&lhs) {
                    Err(Error::NoContext(ErrorKind::NotEq { got: lhs }))
                } else {
                    Ok(Type::Boolean.into())
                }
            }

            // Number A => A -> A -> A
            Expr::Mul(lhs, rhs) |
            Expr::Div(lhs, rhs) |
            Expr::Add(lhs, rhs) |
            Expr::Sub(lhs, rhs)
            => {
                let lhs = lhs.check(stack, func_type.clone())?;
                let rhs = rhs.check(stack, func_type.clone())?;
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
                assert_type(Type::Integer.into(), lhs.check(stack, func_type.clone())?)?;
                assert_type(Type::Integer.into(), rhs.check(stack, func_type.clone())?)?;
                Ok(Type::Integer.into())
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
            => assert_type(Type::Boolean.into(), op.check(stack, func_type)?),

            // A -> A
            Expr::Double(_)
            => Ok(Type::Double.into()),
            Expr::Integer(_)
            => Ok(Type::Integer.into()),
            Expr::Boolean(_)
            => Ok(Type::Boolean.into()),
            Expr::Str(_)
            => Ok(Type::String.into()),
            Expr::Var(VarRef::Deref(_, _)) => unimplemented!("Pointer dereference not implemented"),
            Expr::Var(VarRef::Ident(ident))
            => match search_stack(stack, ident) {
                Some(StackType::Variable(t, _)) => Ok(t.clone()),
                Some(StackType::Function(t, ..)) => Ok(t.clone()),
                _ => Err(Error::NoContext(ErrorKind::Undeclared {}))
            }

            // Function call (type of function, check that expression types and length match)
            Expr::FunctionCall(ident, args)
            => match search_stack(stack, ident) {
                Some(StackType::Function(t, _, params)) => {
                    let t = t.clone();
                    if params.len() != args.len() {
                        return Err(Error::NoContext(ErrorKind::InvalidArgumentCount {
                            expected: params.len(),
                            got: args.len(),
                        }));
                    }
                    let it: Vec<_> = args.iter_mut()
                        .zip(params.iter().map(|Arg(param_t, _)| param_t.clone())).collect();
                    for (arg, param_t) in it {
                        let arg_t = arg.check(stack, func_type.clone())?;
                        if param_t != arg_t {
                            return Err(Error::NoContext(ErrorKind::Type { expected: param_t, got: arg_t }));
                        }
                    }
                    Ok(t)
                }
                Some(_) => Err(Error::NoContext(ErrorKind::NotAFunction)),
                None => Err(Error::NoContext(ErrorKind::Undeclared {})),
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
fn search_stack<'a>(stack: &'a Vec<StackElem>, ident: &str) -> Option<&'a StackType> {
    stack.iter().rev()
        .filter_map(|it| match it {
            StackElem::Scope(_) => None,
            StackElem::Type(t) => Some(t),
        })
        .find(|it| match it {
            StackType::Function(_, id, _) | StackType::Variable(_, id) => id == ident,
        })
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


fn assert_type(expected: TypeRef, got: TypeRef) -> Result<TypeRef, Error<'static>> {
    if expected == got {
        Ok(got)
    } else {
        Err(Error::NoContext(ErrorKind::Type { expected, got }))
    }
}
