pub mod llvm;

use crate::ast::{Type, Program, TopDef, Blk, Stmt, DeclItem, Expr, Node};
use crate::util::NameGenerator;
use self::llvm::{LLVMElem, LLVMExpr, LLVMType, LLVMVal, LLVMFOrd, LLVMIOrd};
use std::collections::{VecDeque, HashMap, BTreeSet};
use std::io::{self, Write};

pub struct LLVM {
    pub lines: VecDeque<LLVMElem>,
    var_names: NameGenerator,
    lab_names: NameGenerator,
    str_names: NameGenerator,
}

impl LLVM {
    pub fn from_program(p: &Program) -> Self {
        let mut out = LLVM {
            lines: VecDeque::new(),
            var_names: NameGenerator::new("t"),
            lab_names: NameGenerator::new("l"),
            str_names: NameGenerator::new("s"),
        };
        out.lines.push_back(LLVMElem::ExtDef("printInt".into(), LLVMType::V, vec![LLVMType::I(32)]));
        out.lines.push_back(LLVMElem::ExtDef("printDouble".into(), LLVMType::V, vec![LLVMType::F(64)]));
        out.lines.push_back(LLVMElem::ExtDef("printString".into(), LLVMType::V, vec![LLVMType::Ptr(box LLVMType::I(8))]));
        out.lines.push_back(LLVMElem::ExtDef("readInt".into(), LLVMType::I(32), vec![]));
        out.lines.push_back(LLVMElem::ExtDef("readDouble".into(), LLVMType::F(64), vec![]));
        out.lines.push_back(LLVMElem::Empty);

        let mut names = NameGenerator::new("t");
        p.transform(&mut out, &mut names, Type::Void);
        out.clean()
        //out
    }

    /// Remove unreachable labels and statements
    fn clean(self) -> Self {
        // TODO: This function probably needs some cleaning up...
        let mut unreachable = false;
        let mut lines: VecDeque<_> = self.lines
            .into_iter()
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
            .collect();

        let mut to_remove: BTreeSet<usize> = BTreeSet::new();
        {
            let mut i = lines.iter().enumerate();
            while let Some((_, l)) = i.next() {
                if let LLVMElem::TopDef(_,_,_) = l {
                    let mut reachable: HashMap<&str, Vec<&str>> = HashMap::new();
                    let mut to_remove_fn: BTreeSet<usize> = BTreeSet::new();
                    let mut label_indices: HashMap<&str, usize> = HashMap::new();
                    let mut current_label: &str = match i.next() {
                        Some((i, LLVMElem::Label(s))) if s == "entry" => {
                            to_remove_fn.insert(i);
                            label_indices.insert(s, i);
                            reachable.insert(s, vec![]);
                            s
                        },
                        _ => panic!(),
                    };
                    while let Some((i, l)) = i.next() {
                        match l {
                            LLVMElem::EndTopDef => break,
                            LLVMElem::Label(s) => {
                                to_remove_fn.insert(i);
                                label_indices.insert(s, i);
                                reachable.insert(s, vec![]);
                                current_label = s;
                            }
                            LLVMElem::Branch{if_true, if_false, ..} => {
                                reachable.entry(current_label).or_default().push(if_true);
                                reachable.entry(current_label).or_default().push(if_false);
                            }
                            LLVMElem::Jump(label) => {
                                reachable.entry(current_label).or_default().push(label);
                            }
                            _ => {}
                        }
                    }
                    unremove(&reachable, &mut to_remove_fn, &label_indices, "entry");

                    to_remove.append(&mut to_remove_fn);
                }
            }
        }

        fn unremove(
            reachable: &HashMap<&str, Vec<&str>>,
            to_remove: &mut BTreeSet<usize>,
            label_indices: &HashMap<&str, usize>,
            label: &str) {
            let index = label_indices.get(label).unwrap();
            if to_remove.remove(index) {
                reachable.get(label).unwrap().iter()
                    .for_each(|&label| unremove(reachable, to_remove, label_indices, label));
            }
        }

        for &i in to_remove.iter().rev() {
            loop {
                match lines[i+1] {
                    LLVMElem::EndTopDef |
                    LLVMElem::Label(_) => break,
                    _ => { lines.remove(i+1); }
                }
            }
            lines.remove(i);
        }

        LLVM {
            lines,
            ..self
        }
    }


    pub fn write<W: Write>(&self, w: &mut W) -> io::Result<()> {
        for l in &self.lines {
            write!(w, "{}", l)?;
        }
        Ok(())
    }

    pub fn new_var_name(&mut self) -> String {
        self.var_names.generate(self.lines.iter())
    }

    pub fn new_lab_name(&mut self) -> String {
        self.lab_names.generate(self.lines.iter())
    }

    pub fn constants(&self) -> impl Iterator<Item=(&str, &LLVMType, &LLVMVal)> {
        self.lines
            .iter()
            .filter_map(|e| match e {
                LLVMElem::InternalConst(ident, tp, value)
                    => Some((ident.as_ref(), tp, value)),
                _ => None,
            })
    }

    pub fn put_string_const(&mut self, s: &str) -> (String, LLVMType) {
        let existing = self.constants()
            .find(|(_, _, value)| match value {
                LLVMVal::Str(v) => s == v,
                _ => false,
            })
            .map(|(ident, tp, _)| (ident, tp));

        if let Some((i, tp)) = existing {
            (i.to_owned(), tp.clone())
        } else {
            let ident = self.str_names.generate(self.lines.iter());
            let tp = LLVMType::Array(
                s.as_bytes().len() + 1, // Add 1 for null terminator
                box LLVMType::I(8),
            );
            let s = LLVMVal::Str(s.to_owned());

            self.lines.push_front(LLVMElem::InternalConst(ident, tp, s));
            if let LLVMElem::InternalConst(ident, tp, _) = &self.lines[0] {
                (ident.clone(), tp.clone())
            } else {
                unreachable!()
            }
        }
    }
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
    fn transform(&self, out: &mut LLVM, names: &mut NameGenerator, tp: Type) -> Option<LLVMVal>;
}

impl ToLLVM for Program<'_> {
    fn transform(&self, out: &mut LLVM, names: &mut NameGenerator, _: Type) -> Option<LLVMVal> {
        for node in &self.0 {
            node.transform(out, names, node.elem.return_type);
        }
        None
    }
}

impl ToLLVM for TopDef<'_> {
    fn transform(&self, out: &mut LLVM, names: &mut NameGenerator, tp: Type) -> Option<LLVMVal> {
        let args: Vec<(String, LLVMType, String)> = self.args.iter()
            .map(|arg| (arg.0.into(), arg.1.clone()))
            .map(|(t, id)| (out.new_var_name(), t, id))
            .collect();
        out.lines.push_back(LLVMElem::TopDef(
            self.ident.clone(),
            self.return_type.into(),
            args.iter().map(|(nid, t, _)| (t.clone(), nid.clone())).collect(),
        ));

        out.lines.push_back(LLVMElem::Label("entry".into()));

        for (nid, t, oid) in args {
            out.lines.push_back(LLVMElem::Assign(oid.clone(), LLVMExpr::AllocA(t.clone())));
            out.lines.push_back(LLVMElem::Store{
                val_t: t.clone(),
                val: nid.into(),
                into_t: LLVMType::Ptr(box t),
                into_i: oid,
            });
        }

        self.body.transform(out, names, tp);

        if let Some(LLVMElem::Label(_)) =  out.lines.back() {
            out.lines.push_back(LLVMElem::Empty);
        }

        if tp == Type::Void {
            out.lines.push_back(LLVMElem::RetV);
        }

        out.lines.push_back(LLVMElem::EndTopDef);
        None
    }
}

impl<T> ToLLVM for Node<'_, T>
where T: ToLLVM {
    /// Assigns the type information for the current context
    fn transform(&self, out: &mut LLVM, names: &mut NameGenerator, tp: Type) -> Option<LLVMVal> {
        let tp = self.tp.unwrap_or(tp);
        self.elem.transform(out, names, tp)
    }
}

impl ToLLVM for Blk<'_> {
    fn transform(&self, out: &mut LLVM, names: &mut NameGenerator, tp: Type) -> Option<LLVMVal> {
        let mut nodes =  self.0.iter();
        if let Some(node) = nodes.next() {
            node.transform(out, names, tp);
            for node in nodes {
                out.lines.push_back(LLVMElem::Empty);
                node.transform(out, names, tp);
            }
        }
        None
    }
}

impl ToLLVM for Stmt<'_> {
    fn transform(&self, out: &mut LLVM, names: &mut NameGenerator, tp: Type) -> Option<LLVMVal> {
        match self {
            Stmt::Return(expr) => {
                let val = expr.transform(out, names, tp);
                out.lines.push_back(LLVMElem::Ret(expr.tp.unwrap().into(), val.unwrap()));
            }
            Stmt::ReturnVoid => out.lines.push_back(LLVMElem::RetV),

            Stmt::If(expr, block) => {
                let expr = expr.transform(out, names, tp).unwrap();
                let lab_if_true = out.new_lab_name();
                let lab_end_if = out.new_lab_name();
                out.lines.push_back(LLVMElem::Branch{
                    cond: expr,
                    if_true: lab_if_true.clone(),
                    if_false: lab_end_if.clone(),
                });

                out.lines.push_back(LLVMElem::Label(lab_if_true));
                block.transform(out, names, tp);
                out.lines.push_back(LLVMElem::Jump(lab_end_if.clone()));

                out.lines.push_back(LLVMElem::Label(lab_end_if));
            }

            Stmt::IfElse(expr, block1, block2) => {
                let expr = expr.transform(out, names, tp).unwrap();
                let lab_if_true = out.new_lab_name();
                let lab_if_false = out.new_lab_name();
                let lab_end_if = out.new_lab_name();
                out.lines.push_back(LLVMElem::Branch{
                    cond: expr,
                    if_true: lab_if_true.clone(),
                    if_false: lab_if_false.clone(),
                });

                out.lines.push_back(LLVMElem::Label(lab_if_true));
                block1.transform(out, names, tp);
                out.lines.push_back(LLVMElem::Jump(lab_end_if.clone()));

                out.lines.push_back(LLVMElem::Label(lab_if_false));
                block2.transform(out, names, tp);
                out.lines.push_back(LLVMElem::Jump(lab_end_if.clone()));

                out.lines.push_back(LLVMElem::Label(lab_end_if));
            }

            Stmt::While(expr, block) => {
                if let box Expr::Boolean(true) = expr.elem {
                    let lab_loop = out.new_lab_name();
                    out.lines.push_back(LLVMElem::Jump(lab_loop.clone()));
                    out.lines.push_back(LLVMElem::Label(lab_loop.clone()));
                    block.transform(out, names, tp);
                    out.lines.push_back(LLVMElem::Jump(lab_loop));
                } else {
                    let lab_entry = out.new_lab_name();
                    let lab_body = out.new_lab_name();
                    let lab_exit = out.new_lab_name();

                    out.lines.push_back(LLVMElem::Jump(lab_entry.clone()));
                    out.lines.push_back(LLVMElem::Label(lab_entry.clone()));
                    let expr = expr.transform(out, names, tp).unwrap();
                    out.lines.push_back(LLVMElem::Branch{
                        cond: expr.clone(),
                        if_true: lab_body.clone(),
                        if_false: lab_exit.clone(),
                    });

                    out.lines.push_back(LLVMElem::Label(lab_body.clone()));
                    block.transform(out, names, tp);
                    out.lines.push_back(LLVMElem::Jump(lab_entry.clone()));

                    out.lines.push_back(LLVMElem::Label(lab_exit));
                }
            }

            Stmt::Block(block) => {
                block.transform(out, names, tp);
            }
            Stmt::Assignment(ident, expr) => {
                let tp = expr.tp.unwrap();
                let val = expr.transform(out, names, tp).unwrap();
                out.lines.push_back(LLVMElem::Store{
                    val_t: tp.into(),
                    val,
                    into_t: LLVMType::Ptr(box tp.into()),
                    into_i: ident.clone(),
                });
            }

            Stmt::Increment(ident) => {
                let tp = Type::Integer;
                let i1 = out.new_var_name();
                out.lines.push_back(LLVMElem::Assign(
                        i1.clone(),
                        LLVMExpr::Load(
                            tp.into(),
                            LLVMType::Ptr(box tp.into()),
                            ident.clone()
                        )
                ));

                let i2 = out.new_var_name();
                out.lines.push_back(LLVMElem::Assign(i2.clone(), LLVMExpr::Add(tp.into(), i1.into(), 1.into())));
                out.lines.push_back(LLVMElem::Store{
                    val_t: tp.into(),
                    val: i2.into(),
                    into_t: LLVMType::Ptr(box tp.into()),
                    into_i: ident.to_owned(),
                });
            }

            // TODO: Remove duplicated code
            Stmt::Decrement(ident) => {
                let tp = Type::Integer;
                let i1 = out.new_var_name();
                out.lines.push_back(LLVMElem::Assign(
                        i1.clone(),
                        LLVMExpr::Load(
                            tp.into(),
                            LLVMType::Ptr(box tp.into()),
                            ident.clone()
                        )
                ));

                let i2 = out.new_var_name();
                out.lines.push_back(LLVMElem::Assign(i2.clone(), LLVMExpr::Sub(tp.into(), i1.into(), 1.into())));
                out.lines.push_back(LLVMElem::Store{
                    val_t: tp.into(),
                    val: i2.into(),
                    into_t: LLVMType::Ptr(box tp.into()),
                    into_i: ident.to_owned(),
                });
            }

            Stmt::Expression(expr) => {
                expr.transform(out, names, tp);
            }

            Stmt::Declare(t, items) => {
                for item in items {
                    let ident = item.get_ident();
                    let val = item.transform(out, names, *t).unwrap();
                    let t: LLVMType = (*t).into();
                    out.lines.push_back(LLVMElem::Assign(ident.to_owned(), LLVMExpr::AllocA(t.clone())));
                    out.lines.push_back(LLVMElem::Store{
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
    fn transform(&self, out: &mut LLVM, names: &mut NameGenerator, tp: Type) -> Option<LLVMVal> {
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
    out: &mut LLVM,
    names: &mut NameGenerator,
    f: F,
) -> Option<LLVMVal> {
    let tp = e1.tp.unwrap();
    let v1 = e1.transform(out, names, tp).unwrap();
    let v2 = e2.transform(out, names, tp).unwrap();
    let i = out.new_var_name();
    out.lines.push_back(LLVMElem::Assign(
        i.clone(),
        f(tp.into(), v1, v2),
    ));
    Some(LLVMVal::Ident(i))
}

fn cmp_expr(
    e1: &Node<'_, Expr<'_>>,
    e2: &Node<'_, Expr<'_>>,
    out: &mut LLVM,
    names: &mut NameGenerator,
    i_ord: LLVMIOrd,
    f_ord: LLVMFOrd,
) -> Option<LLVMVal> {
    match e1.tp.unwrap() {
        Type::Boolean |
        Type::Integer => op_expr(e1, e2, out, names, |tp, v1, v2| LLVMExpr::CmpI(i_ord, tp, v1, v2)),
        Type::Double => op_expr(e1, e2, out, names, |tp, v1, v2| LLVMExpr::CmpF(f_ord, tp, v1, v2)),
        tp => panic!("Invalid type: can't compare \"{}\"s", tp),
    }
}

impl ToLLVM for Expr<'_> {
    /// By convention, a  Expr should always push its
    /// final calculated value onto the stack last, so
    /// that consecutive operations can find it easily
    fn transform(&self, out: &mut LLVM, names: &mut NameGenerator, tp: Type) -> Option<LLVMVal> {
        match self {
            Expr::LOr(e1, e2) => {
                let lab_check_lhs = out.new_lab_name();
                let lab_check_rhs = out.new_lab_name();
                let lab_success = out.new_lab_name();
                let lab_failed = out.new_lab_name();
                let lab_exit = out.new_lab_name();

                out.lines.push_back(LLVMElem::Jump(lab_check_lhs.clone()));

                // First check LHS
                out.lines.push_back(LLVMElem::Label(lab_check_lhs.clone()));
                let lhs = e1.transform(out, names, tp).unwrap();
                out.lines.push_back(LLVMElem::Branch{
                    cond: lhs,
                    if_true: lab_success.clone(),
                    if_false: lab_check_rhs.clone(),
                });
                // ---------------

                // If false: check RHS
                out.lines.push_back(LLVMElem::Label(lab_check_rhs.clone()));
                let rhs = e2.transform(out, names, tp).unwrap();
                out.lines.push_back(LLVMElem::Branch{
                    cond: rhs,
                    if_true: lab_success.clone(),
                    if_false: lab_failed.clone(),
                });
                // -------------------

                out.lines.push_back(LLVMElem::Label(lab_failed.clone()));
                out.lines.push_back(LLVMElem::Jump(lab_exit.clone()));

                out.lines.push_back(LLVMElem::Label(lab_success.clone()));
                out.lines.push_back(LLVMElem::Jump(lab_exit.clone()));

                // Exit
                out.lines.push_back(LLVMElem::Label(lab_exit.clone()));
                let res = out.new_var_name();
                // Set expression value based on previous label
                out.lines.push_back(LLVMElem::Assign(
                        res.clone(),
                        LLVMExpr::Phi(LLVMType::I(1), vec![
                            (1.into(), lab_success),
                            (0.into(), lab_failed),
                        ])
                ));
                Some(res.into())
            }
            Expr::LAnd(e1, e2) => {
                let lab_check_lhs = out.new_lab_name();
                let lab_check_rhs = out.new_lab_name();
                let lab_success = out.new_lab_name();
                let lab_failed = out.new_lab_name();
                let lab_exit = out.new_lab_name();

                out.lines.push_back(LLVMElem::Jump(lab_check_lhs.clone()));

                // First check LHS
                out.lines.push_back(LLVMElem::Label(lab_check_lhs.clone()));
                let lhs = e1.transform(out, names, tp).unwrap();
                out.lines.push_back(LLVMElem::Branch{
                    cond: lhs,
                    if_true: lab_check_rhs.clone(),
                    if_false: lab_failed.clone(),
                });
                // ---------------

                // If true: check RHS
                out.lines.push_back(LLVMElem::Label(lab_check_rhs.clone()));
                let rhs = e2.transform(out, names, tp).unwrap();
                out.lines.push_back(LLVMElem::Branch{
                    cond: rhs,
                    if_true: lab_success.clone(),
                    if_false: lab_failed.clone(),
                });
                // -------------------

                // If either expression is not true: branch here:
                out.lines.push_back(LLVMElem::Label(lab_failed.clone()));
                out.lines.push_back(LLVMElem::Jump(lab_exit.clone()));

                // If either expression is not true: branch here:
                out.lines.push_back(LLVMElem::Label(lab_success.clone()));
                out.lines.push_back(LLVMElem::Jump(lab_exit.clone()));

                // Exit
                out.lines.push_back(LLVMElem::Label(lab_exit.clone()));
                let res = out.new_var_name();
                // Set expression value based on previous label
                out.lines.push_back(LLVMElem::Assign(
                        res.clone(),
                        LLVMExpr::Phi(LLVMType::I(1), vec![
                            (1.into(), lab_success),
                            (0.into(), lab_failed),
                        ])
                ));
                Some(res.into())
            }
            Expr::GT(e1, e2) => cmp_expr(e1, e2, out, names, LLVMIOrd::SGT, LLVMFOrd::OGT),
            Expr::GE(e1, e2) => cmp_expr(e1, e2, out, names, LLVMIOrd::SGE, LLVMFOrd::OGE),
            Expr::LT(e1, e2) => cmp_expr(e1, e2, out, names, LLVMIOrd::SLT, LLVMFOrd::OLT),
            Expr::LE(e1, e2) => cmp_expr(e1, e2, out, names, LLVMIOrd::SLE, LLVMFOrd::OLE),
            Expr::EQ(e1, e2) => cmp_expr(e1, e2, out, names, LLVMIOrd::EQ, LLVMFOrd::OEQ),
            Expr::NE(e1, e2) => cmp_expr(e1, e2, out, names, LLVMIOrd::NE, LLVMFOrd::ONE),
            Expr::Mul(e1, e2) => op_expr(e1, e2, out, names, |tp, v1, v2| LLVMExpr::Mul(tp, v1, v2)),
            Expr::Div(e1, e2) => op_expr(e1, e2, out, names, |tp, v1, v2| LLVMExpr::Div(tp, v1, v2)),
            Expr::Mod(e1, e2) => op_expr(e1, e2, out, names, |tp, v1, v2| LLVMExpr::Mod(tp, v1, v2)),
            Expr::Add(e1, e2) => op_expr(e1, e2, out, names, |tp, v1, v2| LLVMExpr::Add(tp, v1, v2)),
            Expr::Sub(e1, e2) => op_expr(e1, e2, out, names, |tp, v1, v2| LLVMExpr::Sub(tp, v1, v2)),
            Expr::Neg(expr) |
            Expr::Not(expr) => {
                let tp = expr.tp.unwrap();
                let v = expr.transform(out, names, tp).unwrap();
                let i = out.new_var_name();
                match tp.into() {
                    LLVMType::F(_) |
                    LLVMType::I(_) => {
                        out.lines.push_back(LLVMElem::Assign(i.clone(), LLVMExpr::Neg(tp.into(), v)));
                        Some(i.into())
                    }
                    tp => panic!("Not/Neg not implemented for {}", tp),
                }
            }
            &Expr::Double(f) => Some(f.into()),
            &Expr::Integer(i) => Some(i.into()),
            &Expr::Boolean(b) => Some(b.into()),
            Expr::Ident(ident) => {
                let i = out.new_var_name();
                out.lines.push_back(LLVMElem::Assign(
                        i.clone(),
                        LLVMExpr::Load(
                            tp.into(),
                            LLVMType::Ptr(box tp.into()),
                            ident.clone()
                        )
                ));
                Some(i.into())
            },
            Expr::Str(s) => {
                let (si, tp) = out.put_string_const(&s);
                let i = out.new_var_name();
                out.lines.push_back(LLVMElem::Assign(
                    i.clone(),
                    LLVMExpr::GetElementPtr(tp.clone(), si.to_owned(), vec![
                        (LLVMType::I(32), 0.into()), // [i8 x n]* -> [i8 x n]
                        (LLVMType::I(32), 0.into()), // [i8 x n]  -> i8*
                    ])
                ));
                Some(i.into())
            }
            Expr::FunctionCall(ident, args) => {
                let args = args.iter()
                    .map(|expr| (expr.tp.unwrap().into(), expr.transform(out, names, tp).unwrap()))
                    .collect();
                match tp {
                    Type::Void => {
                        out.lines.push_back(LLVMElem::Expr(LLVMExpr::Call(tp.into(), ident.clone(), args)));
                        None
                    }
                    tp => {
                        let i = out.new_var_name();
                        out.lines.push_back(LLVMElem::Assign(i.clone(), LLVMExpr::Call(tp.into(), ident.clone(), args)));
                        Some(LLVMVal::Ident(i))
                    }
                }
            }
        }
    }
}
