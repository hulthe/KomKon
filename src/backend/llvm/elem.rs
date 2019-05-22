use super::{LLVMExpr, LLVMVal, LLVMType};
use std::fmt::{self, Display, Formatter};
use crate::util::stack::HasIdentifier;
use crate::util::write_list;

/// This is the top-level structure for LLVM ops.
/// A list of these could constitute a valid LLVM IR.
#[derive(Debug)]
pub enum LLVMElem {
    /// Example: `define i32 @main() {`
    TopDef(String, LLVMType, Vec<(LLVMType, String)>),

    /// Example: %T = type {i32, i32}
    TypeDef(String, LLVMType),

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

impl HasIdentifier for LLVMElem {
    fn get_identifier(&self) -> Option<&str> {
        match self {
            LLVMElem::TopDef(ident, _ ,_) |
            LLVMElem::TypeDef(ident, _) |
            LLVMElem::ExtDef(ident, _ ,_) |
            LLVMElem::InternalConst(ident, _ ,_) |
            LLVMElem::Label(ident) |
            LLVMElem::Assign(ident, _) => Some(ident),
            _ => None,
        }
    }
}

impl Display for LLVMElem {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use LLVMElem::*;
        match self {
            TopDef(ident, ret, params) => {
                write!(f, "define {} @{}(", ret, ident)?;
                write_list(f, ", ", params.iter(),
                    |f, (param_t, param_ident)| write!(f, "{} %{}", param_t, param_ident))?;
                writeln!(f, ") {{")
            },

            TypeDef(ident, type_) => writeln!(f, "%{} = type {}", ident, type_),

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
