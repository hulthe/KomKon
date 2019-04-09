use crate::ast::{Type, Program, TopDef, Blk, Stmt, Arg, DeclItem, Expr, Node};

enum LLVMElem {
    TopDef(String, LLVMType, Vec<LLVMType>),
    Label(String),
    IfBranch(Expr, Label, Label),
    Jump(Label),
    Ret(LLVMType),
    RetV,
    Assign(String, LLVMExpr),
}

enum LLVMExpr {}

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
    /// Assigns the type information for the current context
    fn transform(&self, out: &mut Vec<LLVMElem>, tp: Type) {
        self.elem.transform(&out, self.tp)
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
            Stmt::Return(expr) => out.push(Ret(type_to_llvm_type(tp))),
            Stmt::ReturnVoid() => out.push(RetV),

            Stmt::If(expr, block) => {
                let expr = expr.transform(&out, tp);
                let lab_if_true = Label(unimplemented!());
                let lab_end_if = Label(unimplemented!());
                out.push(IfBranch(expr, lab_if_true, lab_end_if));
                out.push(lab_if_true);
                block.transform(out, tp);
                out.push(lab_end_if)
            }

            Stmt::IfElse(expr, block1, block2) => {
                let expr = expr.transform(&out, tp);
                let lab_if_true = Label(unimplemented!());
                let lab_if_false = Label(unimplemented!());
                let lab_end_if = Label(unimplemented!());
                out.push(IfBranch(expr, lab_if_true, lab_if_false));
                out.push(lab_if_true);
                block1.tranform(out, tp);
                out.push(lab_if_false);
                block2.transform(out, tp);
                out.push(lab_end_if);
            }

            Stmt::While(expr, block) => {
                let expr = expr.transform(&out, tp);
                let lab_if_true = Label(unimplemented!());
                let lab_end_if = Label(unimplemented!());
                out.push(IfBranch(expr, lab_if_true, lab_end_if));
                out.push(lab_if_true);
                block1.transform(out, tp);
                out.push(IfBranch(expr, lab_if_true, lab_end_if));
                out.push(lab_end_if);
            }

            Stmt::Block(block) => block.transform(out, tp),
            Stmt::Assignment(ident, stmt) => {
                expr.transform(out, tp);
                out.push(Assign(unimplemented!(), unimplemented!()))
            }

            Stmt::Increment(ident) => unimplemented!(),

            Stmt::Decrement(ident) => unimplemented!(),

            Stmt::Expression(expr) => expr.transform(),

            Stmt::Declare(ident) => unimplemented!(),

            Stmt::Empty => return,
            _ => panic!("cool beans")
        }
    }
}

impl ToLLVM for Expr {
    /// By convention, a  Expr should always push its
    /// final calculated value onto the stack last, so
    /// that consecutive operations can find it easily
    fn transform(&self, out: &mut Vec<LLVMElem>, tp: Type) {
        unimplemented!()
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