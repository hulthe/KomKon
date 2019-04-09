use crate::ast::{Type, Program, Node, TopDef, Arg, Blk, Stmt, Expr, DeclItem};
use crate::uniqueifyer::uniqueify;

macro_rules! n {
    ($e:expr) => {Node::new($e, "")}
}

macro_rules! d {
    ($n:pat) => {Node{ elem: box $n, ..}}
}

#[test]
fn minimize() {
    let mut program = Program(vec![
        n!(TopDef {
            return_type: Type::Integer,
            ident: "add".into(),
            args: vec![
                Arg(Type::Integer, "a".into()),
                Arg(Type::Integer, "b".into()),
            ],
            body: n!(Blk(vec![
                n!(Stmt::Declare(Type::Integer, vec![
                    DeclItem::Init("a".into(),
                        Expr::Add(
                            n!(Expr::Ident("a".into())),
                            n!(Expr::Ident("b".into())),
                        )
                    ),
                ])),
                n!(Stmt::Return(
                    Expr::Ident("a".into())
                )),
            ])),
        }),
        n!(TopDef {
            return_type: Type::Void,
            ident: "main".into(),
            args: vec![],
            body: n!(Blk(vec![
                n!(Stmt::Declare(Type::Integer, vec![
                    DeclItem::Init("a".into(), Expr::Integer(1)),
                    DeclItem::Init("b".into(), Expr::Integer(1)),
                ])),
            ])),
        }),
    ]);

    uniqueify(&mut program);

    assert_eq!(program.0[0].elem.args[0].1, "v0");
    assert_eq!(program.0[0].elem.args[1].1, "v1");

    if let box Stmt::Declare(_, items) = &program.0[0].elem.body.elem.0[0].elem {
        if let DeclItem::Init(s0, Expr::Add(
            d!(Expr::Ident(s1)),
            d!(Expr::Ident(s2)),
        )) = &items[0] {
            assert_eq!(s0, "v2");
            assert_eq!(s1, "v0");
            assert_eq!(s2, "v1");
        } else {
            assert!(false);
        }
    } else {
        assert!(false);
    }

    if let box Stmt::Declare(_, items) = &program.0[1].elem.body.elem.0[0].elem {
        if let (DeclItem::Init(s0, _), DeclItem::Init(s1, _)) = (&items[0], &items[1]) {
            assert_eq!(s0, "v0");
            assert_eq!(s1, "v1");
        } else {
            assert!(false);
        }
    } else {
        assert!(false);
    }
}
