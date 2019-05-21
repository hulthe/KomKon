use crate::ast::*;
use crate::typecheck::type_check;
use crate::returncheck::return_check;
use crate::minimize::Minimize;

macro_rules! test_pointers {
    ($file:ident) => {
        #[test]
        fn $file() {
            let source = include_str!(concat!("examples/extensions/pointers/", stringify!($file), ".jl"));
            println!("Trying to compile:\n{}", source);
            let p = Program::parse(source);
            if let Ok(mut p) = p {
                // All good!
                type_check(&mut p).expect("type_check failed");
                p.minimize();
                return_check(&p).expect("return_check failed");
            } else {
                println!("{:#?}", p);
                assert!(false, concat!(stringify!($file), ".jl failed to compile!"));
            }
        }
    }
}

test_pointers!(list);
test_pointers!(tree);
