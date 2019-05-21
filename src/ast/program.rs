use crate::ast::{JavaletteParser, Rule, FromPair, Node, Function, ASTError};
use pest::Parser;
use std::collections::HashMap;
use crate::ast::jl_type::{Type, TypeDef};
use crate::ast::arg::Arg;
use crate::ast::jl_type::Type::Struct;

#[derive(Debug)]
pub struct Program<'a> {
    pub functions: Vec<Node<'a, Function<'a>>>,
    pub types: HashMap<String, Type>,
}

impl<'a> Program<'a> {
    pub fn parse(raw: &'a str) -> Result<Self, ASTError> {
        let mut parse = JavaletteParser::parse(Rule::Program, raw)?;
        let program = parse.next().unwrap();
        let mut functions = vec![];
        let mut types = HashMap::new();
        let mut struct_fields: HashMap<String, HashMap<String, String>> = HashMap::new();

        let mut rest = vec![];
        // put all structs into the types-map
        // structs at this stage have string-based fields
        for pair in program.into_inner() {
            match pair {
                Rule::StructDef => {
                    let mut fields = HashMap::new();
                    let mut name = None;

                    for pair in pair.into_inner() {
                        match pair {
                            Rule::Ident => name = Some(pair.to_string()),
                            Rule::Arg => {
                                let Arg(type_, ident) = Arg::from_pair(pair)?;

                                if fields.contains_key(ident) {
                                    Err(format!("{} already declared", ident))
                                }
                            }
                            _ => {}
                        }
                        fields.insert(ident, type_);
                    }
                    let struct_ident = match name {
                        None => Err("struct missing identity"),
                        id => id
                    };

                    if types.contains_key(struct_ident) {
                        Err(format!("struct {} declared multiple times", ident))
                    }

                    struct_fields.insert(struct_ident.unwrap(), fields);
                    types.insert(struct_ident, Type::Struct { name, fields: HashMap::new() });
                }

                o => rest.push(o)
            }
        }

        let mut rest2 = vec![];
        // put all typedefs into the types-map
        for pair in rest {
            match pair {
                Rule::TypeDef => {
                    let pairs = pair.into_inner();
                    let target_type = Type::from_pair(pairs.nth(1).unwrap());
                    let ident = pairs.next().unwrap().as_str();

                    if types.contains_key(ident) {
                        Err(format!("{} already declared", ident))
                    }

                    types.insert(ident, Type::Pointer(target_type))

                    // TODO only allowed to point at structs?
                }

                o => rest2.push(o)
            }
        }

        let functions = rest2
            .filter(|pair| pair.as_rule() == Rule::Function)
            .map(|pair| (pair.as_str(), Function::from_pair(pair)))
            .map(|(s, r)| r.map(|f| Node::new(f, s)))
            .collect::<Result<Vec<_>, ASTError>>()?;
        Ok(Program {
            functions,
        })
    }
}

