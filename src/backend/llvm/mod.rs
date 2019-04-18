/// This module contains types which maps almost 1 to 1 with LLVM IR
mod elem;
mod expr;
mod val;
mod llvm_type;
mod ord;

pub use self::elem::LLVMElem;
pub use self::expr::LLVMExpr;
pub use self::val::LLVMVal;
pub use self::ord::{LLVMIOrd, LLVMFOrd};
pub use self::llvm_type::LLVMType;

