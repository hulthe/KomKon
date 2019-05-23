use std::fmt::{self, Display, Formatter};

#[derive(Clone, Debug)]
pub enum LLVMVal {
    /// An identifier mapping to some registry.
    /// I.E. `Ident("t0")` will be rendered as `%t0`.
    Ident(String),

    /// An identifier mapping to some global.
    /// I.E. `Global("s0")` will be rendered as `@s0`.
    Global(String),

    /// A string mapping to some valid LLVM IR constant
    /// I.E. `Const("42.0")` will be rendered as `42.0`.
    Const(String),

    /// A string constant. Will be escaped when rendered.
    /// I.E: `String("Hello!") will be rendered as `c"Hello!\00"`
    Str(String),
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
            LLVMVal::Global(ident) => write!(f, "@{}", ident),
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

