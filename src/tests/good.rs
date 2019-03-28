use crate::ast::*;

macro_rules! test_good {
    ($file:ident) => {
        #[test]
        fn $file() {
            let source = include_str!(concat!("examples/good/", stringify!($file), ".jl"));
            println!("Trying to compile:\n{}", source);
            let p = Program::parse(source);
            if let Ok(_) = p {
                // All good!
            } else {
                println!("{:#?}", p);
                assert!(false, concat!(stringify!($file), ".jl failed to compile!"));
            }
        }
    }
}

test_good!(core001);
test_good!(core002);
test_good!(core003);
test_good!(core004);
test_good!(core005);
test_good!(core006);
test_good!(core007);
test_good!(core008);
test_good!(core009);
test_good!(core010);
test_good!(core011);
test_good!(core012);
test_good!(core013);
test_good!(core014);
test_good!(core015);
test_good!(core016);
test_good!(core017);
test_good!(core018);
test_good!(core019);
test_good!(core020);
test_good!(core021);
test_good!(core022);
test_good!(core023);
test_good!(core024);
test_good!(core025);
test_good!(core026);
test_good!(core027);
test_good!(core028);
test_good!(core029);
test_good!(core030);
test_good!(core031);
