use crate::ast::{Type, Program, TopDef, Blk, Stmt, Arg, DeclItem, Expr, Node};

enum LLVMElem {
    TopDef,
    Label,
    Jump,
    Ret(),
}

enum LLVMType {
    I(u8),
    F(u8),
}


pub fn to_llvm(prog: &Program) -> Vec<LLVMElem> {
    let mut out = vec![];
    prog.transform(&mut out);
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
    fn transform(&self, out: &mut Vec<LLVMElem>, tp: Type);
}

impl ToLLVM for Program {
    fn transform(&self, out: &mut Vec<LLVMElem>, tp: Type) {
        for node in self.0 {
            node.transform(&out, tp)
        }
    }
}

impl ToLLVM for Node<T> {
    /// Retrieves the type annotations here1111111111111
    fn transform(&self, out: &mut Vec<LLVMElem>, tp: Type) {
        self.elem.transform(&out, node.tp)
    }
}

impl ToLLVM for Blk {
    fn transform(&self, out: &mut Vec<LLVMElem>, tp: Type) {
        for node in self.0 {
            node.transform(&out, tp)
        }
    }
}

impl ToLLVM for Stmt {
    fn transform(&self, out: &mut Vec<LLVMElem>, tp: Type) {
        match self {
            Stmt::Return(expr) => out.push(),
            Stmt::ReturnVoid() => unimplemented!(),
            Stmt::If(expr, block) => unimplemented!(),
            Stmt::IfElse(expr, block1, block2) => unimplemented!(),
            Stmt::While(expr, block) => unimplemented!(),
            Stmt::Block(block) => unimplemented!(),
            Stmt::Assignment(ident, stmt) => unimplemented!(),
            Stmt::Increment(ident) => unimplemented!(),
            Stmt::Decrement(ident) => unimplemented!(),
            Stmt::Expression(ident) => unimplemented!(),
            Stmt::Declare(ident) => unimplemented!(),
            Stmt::Empty => unimplemented!(),
            _ => panic!("cool beans")
        }
    }
}

fn type_to_llvm_type(tp: Type) {
    match tp {
        Integer => I(32),
        Double => F(64),
        Boolean => I(1),
        Void => V,
        String => unimplemented!(),
    }
}