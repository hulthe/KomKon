use crate::ast::{JavaletteParser, Rule, FromPair, Node, Function, ASTError};
use pest::Parser;
use std::collections::HashMap;
use crate::ast::jl_type::{Type, TypeRef};
use crate::ast::arg::Arg;
use crate::ast::jl_type::Type::Struct;
use std::rc::Rc;

pub type TypeMap = HashMap<String, TypeRef>;

#[derive(Debug)]
pub struct Program<'a> {
    pub functions: Vec<Node<'a, Function<'a>>>,
    //pub types: HashMap<String, Type>,
}

impl<'a> Program<'a> {
    pub fn parse(raw: &'a str) -> Result<Self, ASTError> {
        let mut parse = JavaletteParser::parse(Rule::Program, raw)?;
        let program = parse.next().unwrap();
        let mut functions: Vec<Node<'a, Function<'a>>> = vec![];
        let mut types: TypeMap = HashMap::new();
        let mut struct_fields: HashMap<String, HashMap<String, String>> = HashMap::new();

        types.insert("int".into(), Type::Integer.into());
        types.insert("double".into(), Type::Double.into());
        types.insert("boolean".into(), Type::Boolean.into());
        types.insert("string".into(), Type::String.into());

        let mut rest = vec![];
        // put all structs into the types-map
        // structs at this stage have string-based fields
        for pair in program.into_inner() {
            match pair.as_rule() {
                Rule::StructDef => {
                    let mut fields = HashMap::new();
                    let mut name = None;

                    for pair in pair.into_inner() {
                        match pair.as_rule() {
                            Rule::Ident => name = Some(pair.as_str().to_owned()),
                            Rule::Arg => {
                                let (type_, ident) = Arg::from_pair_str(pair)?;

                                if fields.contains_key(ident) {
                                    return Err(format!("{} already declared", ident).into());
                                }
                                fields.insert(ident.to_owned(), type_.to_owned());
                            }
                            _ => {}
                        }
                    }
                    let name = match name {
                        None => { return Err("struct missing identity".into()) }
                        Some(name) => name
                    };

                    if types.contains_key(&name) {
                        return Err(format!("struct {} declared multiple times", name).into());
                    }

                    println!("Insert struct {}", name);
                    struct_fields.insert(name.clone(), fields);
                    types.insert(name.clone(), Rc::new(Type::Struct { name, fields: HashMap::new() }));
                }

                _ => rest.push(pair)
            }
        }

        let mut rest2 = vec![];
        // put all typedefs into the types-map
        for pair in rest {
            match pair.as_rule() {
                Rule::TypeDef => {
                    let mut pairs = pair.into_inner();
                    let s = pairs.nth(1).unwrap().as_str();
                    let target_type = types.get(s).expect(&format!("Nani? {}", s));
                    let ident = pairs.next().unwrap().as_str();

                    if types.contains_key(ident) {
                        return Err(format!("{} already declared", ident).into());
                    }

                    types.insert(ident.to_owned(), Rc::new(Type::Pointer(target_type.clone())));

                    // TODO only allowed to point at structs?
                }
                _ => rest2.push(pair),
            }
        }

        for (name, fields) in struct_fields {
            let new_fields = fields.into_iter()
                .filter_map(|(name, tp)| {
                    match types.get(&tp) {
                        Some(type_rc) => Some((name, type_rc.clone())),
                        // TODO: Err instead of None
                        None => None,
                    }
                })
                .collect();
            unsafe {
                if let Some(type_rc) = types.get_mut(&name) {
                    if let Type::Struct {
                        fields,
                        ..
                    } = (Rc::into_raw(type_rc.clone()) as *mut Type).as_mut().unwrap() {
                        *fields = new_fields;
                    } else {
                        panic!("Fields belonged to type which was not a struct"); //TODO: Return error
                    }
                } else {
                    panic!("Fields belonged to type which was never defined"); // TODO: Return error
                }
            }
        }

        let functions = rest2
            .into_iter()
            .filter(|pair| pair.as_rule() == Rule::Function)
            .map(|pair| (pair.as_str(), Function::from_pair(pair, &types)))
            .map(|(s, r)| r.map(|f| Node::new(f, s)))
            .collect::<Result<Vec<_>, ASTError>>()?;
        Ok(Program {
            functions,
        })
    }
}

