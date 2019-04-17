/// This module contains type which maps almost 1 to 1 with LLVM IR
use crate::ast::Type;
use crate::util::stack::HasIdentifier;
use std::fmt::{self, Display, Formatter};
use std::io::{self, Write};

pub struct LLVM {
    lines: Vec<LLVMElem>,
}

impl LLVM {
    pub fn write<W: Write>(&self, w: &mut W) -> io::Result<()> {
        for l in &self.lines {
            write!(w, "{}", l)?;
        }
        Ok(())
    }

    pub fn push(&mut self, elem: LLVMElem) {
        self.lines.push(elem);
    }
}

/// This is the top-level structure for LLVM ops.
/// A list of these could constitute a valid LLVM IR.
#[derive(Debug)]
pub enum LLVMElem {
    /// Example: `define i32 @main() {`
    TopDef(String, LLVMType, Vec<(LLVMType, String)>),

    /// Example: `declare i32 @readInt()`
    ExtDef(String, LLVMType, Vec<LLVMType>),

    /// This simply represents `}`
    EndTopDef,

    InternalConst(String, LLVMType, LLVMVal),

    /// Example: `entry:'
    Label(String),

    /// Unconditional branch.
    Jump(String),

    /// Conditional branch. Example: `br i1 1, label %always, label %never`
    Branch {cond: LLVMVal, if_true: String, if_false: String},

    /// Return statement.
    Ret(LLVMType, LLVMVal),

    /// Return statement (void).
    RetV,

    /// Example: `%t0 = EXPR`
    Assign(String, LLVMExpr),

    /// Store a value into an address
    Store {
        val_t: LLVMType,
        val: LLVMVal,
        into_t: LLVMType,
        into_i: String,
    },

    Expr(LLVMExpr),

    /// Newline.
    Empty,
}

#[derive(Clone, Debug)]
pub enum LLVMVal {
    /// An identifier mapping to some registry.
    /// I.E. `Ident("t0")` will be rendered as `%t0`.
    Ident(String),

    /// A string mapping to some valid LLVM IR constant
    /// I.E. `Const("42.0")` will be rendered as `42.0`.
    Const(String),

    /// A string constant. Will be escaped when rendered.
    /// I.E: `String("Hello!") will be rendered as `c"Hello!\00"`
    Str(String),
}

#[derive(Debug)]
pub enum LLVMExpr {
    /// Allocate a stack variable
    AllocA(LLVMType),

    /// Load a value from an address
    Load(LLVMType, LLVMType, String),

    GetElementPtr(LLVMType, String, Vec<(LLVMType, LLVMVal)>),

    /// Compare two integers.
    CmpI(LLVMIOrd, LLVMType, LLVMVal, LLVMVal),

    /// Compare two floating point numbers.
    CmpF(LLVMFOrd, LLVMType, LLVMVal, LLVMVal),

    Phi(LLVMType, Vec<(LLVMVal, String)>),

    Add(LLVMType, LLVMVal, LLVMVal),
    Sub(LLVMType, LLVMVal, LLVMVal),
    Mul(LLVMType, LLVMVal, LLVMVal),
    Div(LLVMType, LLVMVal, LLVMVal),
    Mod(LLVMType, LLVMVal, LLVMVal),

    Neg(LLVMType, LLVMVal),

    /// Function call
    Call(LLVMType, String, Vec<(LLVMType, LLVMVal)>),
}

#[derive(Clone, Debug)]
pub enum LLVMType {
    I(u8),
    F(u8),
    V,
    Array(usize, Box<LLVMType>),
    Ptr(Box<LLVMType>),
}

/// This enum defines the various ways in which two integers can be compared.
#[derive(Clone, Copy, Debug)]
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

#[derive(Clone, Copy, Debug)]
pub enum LLVMFOrd {
    OEQ, //ordered and equal
    OGT, //ordered and greater than
    OGE, //ordered and greater than or equal
    OLT, //ordered and less than
    OLE, //ordered and less than or equal
    ONE, //ordered and not equal
    ORD, //ordered (no nans)
    UEQ, //unordered or equal
    UGT, //unordered or greater than
    UGE, //unordered or greater than or equal
    ULT, //unordered or less than
    ULE, //unordered or less than or equal
    UNE, //unordered or not equal
    UNO, //unordered (either nans)
}

// write a list with a non-trailing separator.
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

            InternalConst(ident, tp, value)
                => write!(f, "@{} = internal constant {} {}\n", ident, tp, value),

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
            LLVMElem::TopDef(ident, _ ,_) |
            LLVMElem::ExtDef(ident, _ ,_) |
            LLVMElem::InternalConst(ident, _ ,_) |
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
            Load(into_t, from_t, from_i) => write!(f, "load {}, {} %{}", into_t, from_t, from_i),
            GetElementPtr(ptr_type, ptr, indices) => {
                write!(f, "getelementptr {0}, {0}* @{1}", ptr_type, ptr)?;
                for (t, v) in indices {
                    write!(f, ", {} {}", t, v)?;
                }
                Ok(())
            }
            CmpI(ord, t, v1, v2) => write!(f, "icmp {} {} {}, {}", ord, t, v1, v2),
            CmpF(ord, t, v1, v2) => write!(f, "fcmp {} {} {}, {}", ord, t, v1, v2),
            Phi(tp, vals) => {
                write!(f, "phi {}", tp)?;
                write_list(f, ", ", vals.iter(),
                    |f, (val, label)| write!(f, "[ {}, %{} ]", val, label))
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
            Mod(t, i1, i2) => {
                if t.is_integer_type() {
                    // NOTE: srem is specifically for signed integers
                    write!(f, "srem {} {}, {}", t, i1, i2)
                } else if t.is_float_type() {
                    write!(f, "frem {} {}, {}", t, i1, i2)
                } else {
                    panic!("Attempting to rem two values of type {}.", t);
                }
            }
            Neg(t, i) => {
                match t {
                    LLVMType::I(1) => {
                        write!(f, "xor i1 1, {}", i)
                    }
                    &LLVMType::I(_) => {
                        write!(f, "sub {} 0, {}", t, i)
                    }
                    LLVMType::F(_) => {
                        write!(f, "fsub {} 0.0, {}", t, i)
                    }
                    _ => unimplemented!(),
                }
            }
            Call(tp, ident, args) => {
                write!(f, "call {} @{}(", tp, ident)?;
                write_list(f, ", ", args.iter(),
                    |f, (arg_t, arg_i)| write!(f, "{} {}", arg_t, arg_i))?;
                write!(f, ")")
            }
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
        LLVMVal::Const(format!("{:?}", f))
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
            LLVMVal::Str(s) => {
                write!(f, "c\"")?;
                for c in s.chars() {
                    match c {
                        '\n' | '\t' | '\r' | '\\' | '"' => { write!(f, "\\{:02X}", c as u8)?; }
                        c if c.is_ascii() => { write!(f, "{}", c)?; }
                        c => { write!(f, "{}", c)?; } // TODO: Does unicode work????
                    }
                }
                write!(f, "\\00\"")
            },
        }
    }
}

impl Display for LLVMIOrd {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", format!("{:?}", self).to_lowercase())
    }
}

impl Display for LLVMFOrd {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", format!("{:?}", self).to_lowercase())
    }
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
