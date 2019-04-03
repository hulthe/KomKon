use crate::ast::*;
use crate::typecheck::type_check;
use crate::returncheck::return_check;

macro_rules! test_bad {
    ($file:ident) => {
        #[test]
        fn $file() {
            let source = include_str!(concat!("examples/bad/", stringify!($file), ".jl"));
            println!("Trying to compile invalid program:\n{}", source);
            let p = Program::parse(source);
            match p {
                Err(e) => {
                println!("Success! Program rejected by grammar with the following error: {:?}", e);
                }
                Ok(p) => {
                    if let Err(e) = type_check(&p) {
                        println!("Success! Program rejected by type checker with the following error: {:?}", e);
                        if let Err(e) = return_check(&p){
                            println!("Success! Program rejected by return checker with the following error: {:?}",e)
                        }
                    } else {
                        assert!(false, concat!("Invalid program ", stringify!($file), ".jl compiled successfully."));
                    }
                }
            }
        }
    }
}

test_bad!(bad001);
test_bad!(bad002);
test_bad!(bad003);
test_bad!(bad004);
test_bad!(bad005);
test_bad!(bad006);
test_bad!(bad007);
test_bad!(bad008);
test_bad!(bad009);

test_bad!(bad010);
test_bad!(bad011);
test_bad!(bad012);
test_bad!(bad013);
//test_bad!(bad014);
test_bad!(bad015);
test_bad!(bad016);
test_bad!(bad017);
test_bad!(bad018);
test_bad!(bad019);

test_bad!(bad020);
test_bad!(bad021);
test_bad!(bad022);
test_bad!(bad023);
test_bad!(bad024);
test_bad!(bad025);
test_bad!(bad026);
test_bad!(bad027);
test_bad!(bad028);
test_bad!(bad029);

test_bad!(bad030);
test_bad!(bad031);
test_bad!(bad032);
test_bad!(bad033);
test_bad!(bad034);
test_bad!(bad035);
test_bad!(bad036);
test_bad!(bad037);
test_bad!(bad038);
test_bad!(bad039);

test_bad!(bad040);
test_bad!(bad041);
test_bad!(bad042);
test_bad!(bad043);
test_bad!(bad044);
test_bad!(bad045);
test_bad!(bad046);
test_bad!(bad047);
test_bad!(bad048);
test_bad!(bad049);

test_bad!(bad050);
test_bad!(bad051);
test_bad!(bad052);
test_bad!(bad053);
test_bad!(bad054);
test_bad!(bad055);
test_bad!(bad056);
//test_bad!(bad057);
test_bad!(bad058);
test_bad!(bad059);

test_bad!(bad060);
test_bad!(bad061);
test_bad!(bad062);
test_bad!(bad063);

