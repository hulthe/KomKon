use crate::ast::{Type, Program, TopDef, Blk, Stmt, Arg, DeclItem, Expr, Node};
use crate::util::{NameGenerator, stack::HasIdentifier};
use std::fmt::{self, Display, Formatter};

#[derive(Debug)]
pub enum LLVMElem {
    TopDef(String, LLVMType, Vec<(LLVMType, String)>),
    ExtDef(String, LLVMType, Vec<LLVMType>),
    EndTopDef,
    Label(String),
    Jump(String),
    Branch {cond: LLVMVal, if_true: String, if_false: String},
    Ret(LLVMType, LLVMVal),
    RetV,
    Assign(String, LLVMExpr),
    Store {
        val_t: LLVMType,
        val: LLVMVal,
        into_t: LLVMType,
        into_i: String,
    },
    Expr(LLVMExpr),
    Empty,
}

#[derive(Clone, Debug)]
pub enum LLVMVal {
    Ident(String),
    Const(String),
}

#[derive(Debug)]
pub enum LLVMExpr {
    AllocA(LLVMType),
    Load(LLVMType, LLVMType, String),
    CmpI(LLVMIOrd, LLVMType, LLVMVal, LLVMVal),
    Add(LLVMType, LLVMVal, LLVMVal),
    Sub(LLVMType, LLVMVal, LLVMVal),
    Mul(LLVMType, LLVMVal, LLVMVal),
    Div(LLVMType, LLVMVal, LLVMVal),
    Call(LLVMType, String, Vec<(LLVMType, LLVMVal)>),
}

#[derive(Debug)]
pub enum LLVMIOrd {
    EQ, //equal
    NE, //not equal
    UGT, //unsigned greater than
    UGE, //unsigned greater or equal
    ULT, //unsigned less than
    ULE, //unsigned less or equal
    SGT, //signed greater than
    SGE, //signed greater or equal
    SLT, //signed less than
    SLE, //signed less or equal
}

fn write_list<F, I, D>(f: &mut Formatter, separator: &str, iter: I, disp: F) -> fmt::Result
where F: Fn(&mut Formatter, D) -> fmt::Result,
      I: Iterator<Item=D>,
{
    let mut fst = true;
    for item in iter {
        if !fst {
            write!(f, "{}", separator)?;
        }
        fst = false;
        disp(f, item)?;
    }
    Ok(())
}

impl Display for LLVMElem {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use LLVMElem::*;
        match self {
            TopDef(ident, ret, params) => {
                write!(f, "define {} @{}(", ret, ident)?;
                write_list(f, ", ", params.iter(),
                    |f, (param_t, param_ident)| write!(f, "{} %{}", param_t, param_ident))?;
                write!(f, ") {{\n")
            },

            ExtDef(ident, ret, params) => {
                write!(f, "declare {} @{}(", ret, ident)?;
                write_list(f, ", ", params.iter(),
                    |f, t| write!(f, "{}", t))?;
                write!(f, ")\n")
            }

            EndTopDef => write!(f, "}}\n"),

            Label(label) => write!(f, "{}:", label),

            Jump(label) => write!(f, "\tbr label %{}\n", label),

            Branch {cond, if_true, if_false}
                => write!(f, "\tbr i1 {}, label %{}, label %{}\n", cond, if_true, if_false),

            Ret(tp, val) => write!(f, "\tret {} {}\n", tp, val),

            RetV => write!(f, "\tret void\n"),

            Assign(ident, expr) => write!(f, "\t%{} = {}\n", ident, expr),

            Store {val_t, val, into_t, into_i} =>
                write!(f, "\tstore {} {}, {} %{}\n", val_t, val, into_t, into_i),

            Expr(expr) => write!(f, "\t{}\n", expr),

            Empty => write!(f, "\n"),
        }
    }
}

impl HasIdentifier for LLVMElem {
    fn get_identifier(&self) -> Option<&str> {
        match self {
            LLVMElem::Label(ident) |
            LLVMElem::Assign(ident, _) => Some(ident),
            _ => None,
        }
    }
}

impl Display for LLVMExpr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use LLVMExpr::*;
        match self {
            AllocA(tp) => write!(f, "alloca {}", tp),
            Call(tp, ident, args) => {
                write!(f, "call {} @{}(", tp, ident)?;
                write_list(f, ", ", args.iter(),
                    |f, (arg_t, arg_i)| write!(f, "{} {}", arg_t, arg_i))?;
                write!(f, ")")
            }
            Add(t, i1, i2) => {
                if t.is_integer_type() {
                    write!(f, "add {} {}, {}", t, i1, i2)
                } else if t.is_float_type() {
                    write!(f, "fadd {} {}, {}", t, i1, i2)
                } else {
                    panic!("Attempting to add two values of type {}.", t);
                }
            }
            Sub(t, i1, i2) => {
                if t.is_integer_type() {
                    write!(f, "sub {} {}, {}", t, i1, i2)
                } else if t.is_float_type() {
                    write!(f, "fsub {} {}, {}", t, i1, i2)
                } else {
                    panic!("Attempting to sub two values of type {}.", t);
                }
            }
            Mul(t, i1, i2) => {
                if t.is_integer_type() {
                    write!(f, "mul {} {}, {}", t, i1, i2)
                } else if t.is_float_type() {
                    write!(f, "fmul {} {}, {}", t, i1, i2)
                } else {
                    panic!("Attempting to mul two values of type {}.", t);
                }
            }
            Div(t, i1, i2) => {
                if t.is_integer_type() {
                    // NOTE: sdiv is specifically for signed integers
                    write!(f, "sdiv {} {}, {}", t, i1, i2)
                } else if t.is_float_type() {
                    write!(f, "fdiv {} {}, {}", t, i1, i2)
                } else {
                    panic!("Attempting to div two values of type {}.", t);
                }
            }
            Load(into_t, from_t, from_i) => write!(f, "load {}, {} %{}", into_t, from_t, from_i),
            CmpI(ord, t, v1, v2) => write!(f, "icmp {} {} {}, {}", ord, t, v1, v2),
        }
    }
}

impl From<bool> for LLVMVal {
    fn from(b: bool) -> LLVMVal {
        LLVMVal::Const((b as u8).to_string())
    }
}

impl From<i32> for LLVMVal {
    fn from(i: i32) -> LLVMVal {
        LLVMVal::Const(i.to_string())
    }
}

impl From<f64> for LLVMVal {
    fn from(f: f64) -> LLVMVal {
        LLVMVal::Const(f.to_string())
    }
}

impl From<String> for LLVMVal {
    fn from(s: String) -> LLVMVal {
        LLVMVal::Ident(s)
    }
}

impl From<&str> for LLVMVal {
    fn from(s: &str) -> LLVMVal {
        LLVMVal::Ident(s.to_owned())
    }
}

impl Display for LLVMVal {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            LLVMVal::Ident(ident) => write!(f, "%{}", ident),
            LLVMVal::Const(s) => write!(f, "{}", s),
        }
    }
}

impl Display for LLVMIOrd {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", format!("{:?}", self).to_lowercase())
    }
}

#[derive(Clone, Debug)]
pub enum LLVMType {
    I(u8),
    F(u8),
    V,
    Array(usize, Box<LLVMType>),
    Ptr(Box<LLVMType>),
}

impl LLVMType {
    pub fn is_integer_type(&self) -> bool {
        match self {
            LLVMType::I(_) |
            LLVMType::Ptr(_) => true,
            LLVMType::Array(_, t) => t.is_integer_type(),
            _ => false,
        }
    }

    pub fn is_float_type(&self) -> bool {
        match self {
            LLVMType::F(_) => true,
            LLVMType::Array(_, t) => t.is_float_type(),
            _ => false,
        }
    }

    pub fn default_value(&self) -> &'static str {
        match self {
            LLVMType::I(_) => "0",
            LLVMType::F(_) => "0.0",
            LLVMType::V => "void",
            LLVMType::Array(_, _) |
            LLVMType::Ptr(_) => unimplemented!(),
        }
    }
}

impl From<Type> for LLVMType {
    fn from(t: Type) -> LLVMType {
        match t {
            Type::Integer => LLVMType::I(32),
            Type::Double => LLVMType::F(64),
            Type::Void => LLVMType::V,
            Type::Boolean => LLVMType::I(1),
            Type::String => LLVMType::Ptr(box LLVMType::I(8)),
        }
    }
}

impl Display for LLVMType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use LLVMType::*;
        match self {
            I(bits) => write!(f, "i{}", bits)?,
            F(16) => write!(f, "half")?,
            F(32) => write!(f, "float")?,
            F(64) => write!(f, "double")?,
            F(_) => unimplemented!("Only floating point types of size 16, 32, & 64 are supported."),
            V => write!(f, "void")?,
            Array(len, box t) => write!(f, "[{} x {}]", len, t)?,
            Ptr(box t) => write!(f, "{}*", t)?,
        }
        Ok(())
    }
}

pub fn to_llvm(prog: &Program) -> Vec<LLVMElem> {
    let mut out = vec![];
    out.push(LLVMElem::ExtDef("printInt".into(), LLVMType::V, vec![LLVMType::I(32)]));
    out.push(LLVMElem::ExtDef("printDouble".into(), LLVMType::V, vec![LLVMType::F(64)]));
    out.push(LLVMElem::ExtDef("printString".into(), LLVMType::V, vec![LLVMType::Ptr(box LLVMType::I(8))]));
    out.push(LLVMElem::ExtDef("readInt".into(), LLVMType::I(32), vec![]));
    out.push(LLVMElem::ExtDef("readDouble".into(), LLVMType::F(64), vec![]));
    out.push(LLVMElem::Empty);

    let mut names = NameGenerator::new("t");
    prog.transform(&mut out, &mut names, Type::Void);
    out
}

trait ToLLVM {
    /// Should modify out pushing LLVMElem:s representing
    /// itself and its children.
    ///
    /// # Arguments
    ///
    /// * `out` a vector to push LLVMElem:s to
    ///
    /// * `tp`  the type of the current context
    ///         in general only set by Blk:s
    ///
    fn transform(&self, out: &mut Vec<LLVMElem>, names: &mut NameGenerator, tp: Type) -> Option<LLVMVal>;
}

impl ToLLVM for Program<'_> {
    fn transform(&self, out: &mut Vec<LLVMElem>, names: &mut NameGenerator, tp: Type) -> Option<LLVMVal> {
        for node in &self.0 {
            node.transform(out, names, tp);
        }
        None
    }
}

impl ToLLVM for TopDef<'_> {
    fn transform(&self, out: &mut Vec<LLVMElem>, names: &mut NameGenerator, tp: Type) -> Option<LLVMVal> {
        let args: Vec<(String, LLVMType, String)> = self.args.iter()
            .map(|arg| (arg.0.into(), arg.1.clone()))
            .map(|(t, id)| (names.generate(out), t, id))
            .collect();
        out.push(LLVMElem::TopDef(
            self.ident.clone(),
            self.return_type.into(),
            args.iter().map(|(nid, t, _)| (t.clone(), nid.clone())).collect(),
        ));

        out.push(LLVMElem::Label("entry".into()));

        for (nid, t, oid) in args {
            out.push(LLVMElem::Assign(oid.clone(), LLVMExpr::AllocA(t.clone())));
            out.push(LLVMElem::Store{
                val_t: t.clone(),
                val: nid.into(),
                into_t: LLVMType::Ptr(box t),
                into_i: oid,
            });
        }

        self.body.transform(out, names, tp);

        if let Some(LLVMElem::Label(_)) =  out.last() {
            out.push(LLVMElem::Empty);
        }

        out.push(LLVMElem::EndTopDef);
        None
    }
}

impl<T> ToLLVM for Node<'_, T>
where T: ToLLVM {
    /// Assigns the type information for the current context
    fn transform(&self, out: &mut Vec<LLVMElem>, names: &mut NameGenerator, tp: Type) -> Option<LLVMVal> {
        let tp = self.tp.unwrap_or(tp);
        self.elem.transform(out, names, tp)
    }
}

impl ToLLVM for Blk<'_> {
    fn transform(&self, out: &mut Vec<LLVMElem>, names: &mut NameGenerator, tp: Type) -> Option<LLVMVal> {
        for node in &self.0 {
            node.transform(out, names, tp);
        }
        None
    }
}

impl ToLLVM for Stmt<'_> {
    fn transform(&self, out: &mut Vec<LLVMElem>, names: &mut NameGenerator, tp: Type) -> Option<LLVMVal> {
        use LLVMElem::*;
        use LLVMExpr::*;
        match self {
            Stmt::Return(expr) => {
                let val = expr.transform(out, names, tp);
                out.push(LLVMElem::Ret(tp.into(), val.unwrap()));
            }
            Stmt::ReturnVoid => out.push(LLVMElem::RetV),

            Stmt::If(expr, block) => {
                let expr = expr.transform(out, names, tp).unwrap();
                let lab_if_true = names.generate(out);
                let lab_end_if = names.generate(out);
                out.push(LLVMElem::Branch{
                    cond: expr,
                    if_true: lab_if_true.clone(),
                    if_false: lab_end_if.clone(),
                });

                out.push(LLVMElem::Label(lab_if_true));
                block.transform(out, names, tp);
                out.push(LLVMElem::Jump(lab_end_if.clone()));

                out.push(LLVMElem::Label(lab_end_if));
            }

            Stmt::IfElse(expr, block1, block2) => {
                let expr = expr.transform(out, names, tp).unwrap();
                let lab_if_true = names.generate(out);
                let lab_if_false = names.generate(out);
                let lab_end_if = names.generate(out);
                out.push(LLVMElem::Branch{
                    cond: expr,
                    if_true: lab_if_true.clone(),
                    if_false: lab_if_false.clone(),
                });

                out.push(LLVMElem::Label(lab_if_true));
                block1.transform(out, names, tp);
                out.push(LLVMElem::Jump(lab_end_if.clone()));

                out.push(LLVMElem::Label(lab_if_false));
                block2.transform(out, names, tp);
                out.push(LLVMElem::Jump(lab_end_if.clone()));

                out.push(LLVMElem::Label(lab_end_if));
            }

            Stmt::While(expr, block) => {
                let lab_entry = names.generate(out);
                let lab_body = names.generate(out);
                let lab_exit = names.generate(out);


                out.push(LLVMElem::Jump(lab_entry.clone()));
                out.push(LLVMElem::Label(lab_entry.clone()));
                let expr = expr.transform(out, names, tp).unwrap();
                out.push(LLVMElem::Branch{
                    cond: expr.clone(),
                    if_true: lab_body.clone(),
                    if_false: lab_exit.clone(),
                });

                out.push(LLVMElem::Label(lab_body.clone()));
                block.transform(out, names, tp);
                out.push(LLVMElem::Jump(lab_entry.clone()));

                out.push(LLVMElem::Label(lab_exit));
            }

            Stmt::Block(block) => {
                block.transform(out, names, tp);
            }
            Stmt::Assignment(ident, expr) => {
                let tp = expr.tp.unwrap();
                let val = expr.transform(out, names, tp).unwrap();
                out.push(Store{
                    val_t: tp.into(),
                    val,
                    into_t: LLVMType::Ptr(box tp.into()),
                    into_i: ident.clone(),
                });
            }

            Stmt::Increment(_ident) => unimplemented!(),

            Stmt::Decrement(_ident) => unimplemented!(),

            Stmt::Expression(expr) => {
                expr.transform(out, names, tp);
            }

            Stmt::Declare(t, items) => {
                for item in items {
                    let ident = item.get_ident();
                    let val = item.transform(out, names, *t).unwrap();
                    let t: LLVMType = (*t).into();
                    out.push(Assign(ident.to_owned(), AllocA(t.clone())));
                    out.push(Store{
                        val_t: t.clone(),
                        val,
                        into_t: LLVMType::Ptr(box t),
                        into_i: ident.to_owned(),
                    });
                }
            }

            Stmt::Empty => {}
        }
        None
    }
}

impl ToLLVM for DeclItem<'_> {
    fn transform(&self, out: &mut Vec<LLVMElem>, names: &mut NameGenerator, tp: Type) -> Option<LLVMVal> {
        match self {
            DeclItem::NoInit(_) => {
                let tp: LLVMType = tp.into();
                Some(LLVMVal::Const(tp.default_value().to_owned()))
            }
            DeclItem::Init(_, expr) => {
                expr.transform(out, names, tp)
            }
        }
    }
}

fn op_expr<F>(e1: &Node<'_, Expr<'_>>, e2: &Node<'_, Expr<'_>>, out: &mut Vec<LLVMElem>, names: &mut NameGenerator, f: F) -> Option<LLVMVal>
where F: Fn(LLVMType, LLVMVal, LLVMVal) -> LLVMExpr {
    let tp = e1.tp.unwrap();
    let v1 = e1.transform(out, names, tp).unwrap();
    let v2 = e2.transform(out, names, tp).unwrap();
    let i = names.generate(out);
    out.push(LLVMElem::Assign(
        i.clone(),
        f(tp.into(), v1, v2),
    ));
    Some(LLVMVal::Ident(i))
}

impl ToLLVM for Expr<'_> {
    /// By convention, a  Expr should always push its
    /// final calculated value onto the stack last, so
    /// that consecutive operations can find it easily
    fn transform(&self, out: &mut Vec<LLVMElem>, names: &mut NameGenerator, tp: Type) -> Option<LLVMVal> {
        match self {
            Expr::LOr(_expr1, _expr2) => unimplemented!(),
            Expr::LAnd(_expr1, _expr2) => unimplemented!(),
            Expr::GT(e1, e2) => op_expr(e1, e2, out, names, |tp, v1, v2| LLVMExpr::CmpI(LLVMIOrd::SGT, tp, v1, v2)),
            Expr::GE(e1, e2) => op_expr(e1, e2, out, names, |tp, v1, v2| LLVMExpr::CmpI(LLVMIOrd::SGE, tp, v1, v2)),
            Expr::LT(e1, e2) => op_expr(e1, e2, out, names, |tp, v1, v2| LLVMExpr::CmpI(LLVMIOrd::SLT, tp, v1, v2)),
            Expr::LE(e1, e2) => op_expr(e1, e2, out, names, |tp, v1, v2| LLVMExpr::CmpI(LLVMIOrd::SLE, tp, v1, v2)),
            Expr::EQ(e1, e2) => op_expr(e1, e2, out, names, |tp, v1, v2| LLVMExpr::CmpI(LLVMIOrd::EQ, tp, v1, v2)),
            Expr::NE(e1, e2) => op_expr(e1, e2, out, names, |tp, v1, v2| LLVMExpr::CmpI(LLVMIOrd::NE, tp, v1, v2)),
            Expr::Mul(e1, e2) => op_expr(e1, e2, out, names, |tp, v1, v2| LLVMExpr::Mul(tp, v1, v2)),
            Expr::Div(e1, e2) => op_expr(e1, e2, out, names, |tp, v1, v2| LLVMExpr::Div(tp, v1, v2)),
            Expr::Mod(_e1, _e2) => unimplemented!(),
            Expr::Add(e1, e2) => op_expr(e1, e2, out, names, |tp, v1, v2| LLVMExpr::Add(tp, v1, v2)),
            Expr::Sub(e1, e2) => op_expr(e1, e2, out, names, |tp, v1, v2| LLVMExpr::Sub(tp, v1, v2)),
            Expr::Neg(expr) => unimplemented!(),
            Expr::Not(expr) => unimplemented!(),
            &Expr::Double(f) => Some(f.into()),
            &Expr::Integer(i) => Some(i.into()),
            &Expr::Boolean(b) => Some(b.into()),
            Expr::Ident(ident) => {
                let i = names.generate(out);
                out.push(LLVMElem::Assign(
                        i.clone(),
                        LLVMExpr::Load(
                            tp.into(),
                            LLVMType::Ptr(box tp.into()),
                            ident.clone()
                        )
                ));
                Some(i.into())
            },
            Expr::Str(s) => unimplemented!(),
            Expr::FunctionCall(ident, args) => {
                let args = args.iter()
                    .map(|expr| (expr.tp.unwrap().into(), expr.transform(out, names, tp).unwrap()))
                    .collect();
                match tp {
                    Type::Void => {
                        out.push(LLVMElem::Expr(LLVMExpr::Call(tp.into(), ident.clone(), args)));
                        None
                    }
                    tp => {
                        let i = names.generate(out);
                        out.push(LLVMElem::Assign(i.clone(), LLVMExpr::Call(tp.into(), ident.clone(), args)));
                        Some(LLVMVal::Ident(i))
                    }
                }
            }
        }
    }
}
