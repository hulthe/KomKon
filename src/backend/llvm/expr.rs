use super::{LLVMVal, LLVMType, LLVMIOrd, LLVMFOrd};
use std::fmt::{self, Display, Formatter};
use crate::util::write_list;

#[derive(Debug)]
pub enum LLVMExpr {
    /// Allocate a stack variable
    AllocA(LLVMType),

    /// Load a value from an address
    Load(LLVMType, LLVMType, String),

    GetElementPtr(LLVMType, LLVMVal, Vec<(LLVMType, LLVMVal)>),

    /// Compare two integers.
    CmpI(LLVMIOrd, LLVMType, LLVMVal, LLVMVal),

    /// Compare two floating point numbers.
    CmpF(LLVMFOrd, LLVMType, LLVMVal, LLVMVal),

    Phi(LLVMType, Vec<(LLVMVal, String)>),

    Bitcast(LLVMType, LLVMVal, LLVMType),

    Add(LLVMType, LLVMVal, LLVMVal),
    Sub(LLVMType, LLVMVal, LLVMVal),
    Mul(LLVMType, LLVMVal, LLVMVal),
    Div(LLVMType, LLVMVal, LLVMVal),
    Mod(LLVMType, LLVMVal, LLVMVal),

    Neg(LLVMType, LLVMVal),

    /// Function call
    Call(LLVMType, String, Vec<(LLVMType, LLVMVal)>),
}

impl Display for LLVMExpr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use LLVMExpr::*;
        match self {
            AllocA(tp) => write!(f, "alloca {}", tp),
            Load(into_t, from_t, from_i) => write!(f, "load {}, {} %{}", into_t, from_t, from_i),
            GetElementPtr(ptr_type, ptr, indices) => {
                write!(f, "getelementptr {0}, {0}* {1}", ptr_type, ptr)?;
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
            Bitcast(from_t, val, to_t) => write!(f, "bitcast {} {} to {}", from_t, val, to_t),
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
                    _ => panic!("Attempting to negate non-number type {}", t),
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

