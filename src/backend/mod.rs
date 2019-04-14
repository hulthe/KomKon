pub mod llvm;

use crate::ast::{Type, Program, TopDef, Blk, Stmt, DeclItem, Expr, Node};
use crate::util::NameGenerator;
use self::llvm::{LLVMElem, LLVMExpr, LLVMType, LLVMVal, LLVMIOrd};

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
    clean_llvm(out)
}

fn clean_llvm(ir: Vec<LLVMElem>) -> Vec<LLVMElem> {
    let mut unreachable = false;
    ir.into_iter()
        .filter(|elem| match elem {
            LLVMElem::Jump(_) |
            LLVMElem::RetV |
            LLVMElem::Ret(_, _) if !unreachable => {
                unreachable = true;
                true
            }

            LLVMElem::EndTopDef |
            LLVMElem::Label(_) => {
                unreachable = false;
                true
            }
            _ => !unreachable,
        })
        .collect()
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

        if tp == Type::Void {
            out.push(LLVMElem::RetV);
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
        let mut nodes =  self.0.iter();
        if let Some(node) = nodes.next() {
            node.transform(out, names, tp);
            for node in nodes {
                out.push(LLVMElem::Empty);
                node.transform(out, names, tp);
            }
        }
        None
    }
}

impl ToLLVM for Stmt<'_> {
    fn transform(&self, out: &mut Vec<LLVMElem>, names: &mut NameGenerator, tp: Type) -> Option<LLVMVal> {
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
                if let box Expr::Boolean(true) = expr.elem {
                    let lab_loop = names.generate(out);
                    out.push(LLVMElem::Jump(lab_loop.clone()));
                    out.push(LLVMElem::Label(lab_loop.clone()));
                    block.transform(out, names, tp);
                    out.push(LLVMElem::Jump(lab_loop));
                } else {
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
            }

            Stmt::Block(block) => {
                block.transform(out, names, tp);
            }
            Stmt::Assignment(ident, expr) => {
                let tp = expr.tp.unwrap();
                let val = expr.transform(out, names, tp).unwrap();
                out.push(LLVMElem::Store{
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
                    out.push(LLVMElem::Assign(ident.to_owned(), LLVMExpr::AllocA(t.clone())));
                    out.push(LLVMElem::Store{
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

fn op_expr<F: Fn(LLVMType, LLVMVal, LLVMVal) -> LLVMExpr>(
    e1: &Node<'_, Expr<'_>>,
    e2: &Node<'_, Expr<'_>>,
    out: &mut Vec<LLVMElem>,
    names: &mut NameGenerator,
    f: F,
) -> Option<LLVMVal> {
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
            Expr::Mod(e1, e2) => op_expr(e1, e2, out, names, |tp, v1, v2| LLVMExpr::Mod(tp, v1, v2)),
            Expr::Add(e1, e2) => op_expr(e1, e2, out, names, |tp, v1, v2| LLVMExpr::Add(tp, v1, v2)),
            Expr::Sub(e1, e2) => op_expr(e1, e2, out, names, |tp, v1, v2| LLVMExpr::Sub(tp, v1, v2)),
            Expr::Neg(expr) |
            Expr::Not(expr) => {
                let tp = expr.tp.unwrap();
                let v = expr.transform(out, names, tp).unwrap();
                let i = names.generate(out);
                match tp.into() {
                    LLVMType::F(_) |
                    LLVMType::I(_) => {
                        out.push(LLVMElem::Assign(i.clone(), LLVMExpr::Neg(tp.into(), v)));
                        Some(i.into())
                    }
                    tp => unimplemented!("Not/Neg not implemented for {}", tp),
                }
            }
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
            Expr::Str(_s) => unimplemented!(),
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
