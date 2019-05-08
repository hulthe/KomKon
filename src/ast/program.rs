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

        for pair in program.into_inner() {
            match pair {
                Rule::Function =>
                    functions.push(
                        Node::new(pair.as_str(), Function::from_pair(pair))),

                Rule::StructDef => {
                    let mut fields = HashMap::new();
                    let mut struct_ident = "";

                    for pair in pair.into_inner() {
                        match pair {
                            Rule::Ident => struct_ident = pair,
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
                    if (types.contains_key(struct_ident)) { Err(format!("struct {} declared multiple times", ident)) }
                    types.insert(struct_ident, Struct { fields });
                }
                Rule::TypeDef => {
                    let pairs = pair.into_inner().filter(|child| child == Rule::Ident);
                    let ident = pairs[0].as_str();
                    let target_type = pairs[1].as_str();

                    if types.contains_key(ident) {
                        Err(format!("{} already declared", ident))
                    }
                    

                    // TODO make sure that there is a check afterwards that ensures all types are declared
                }
                _ => {}
            }
        }

        let functions = program.into_inner()
            .filter(|pair| pair.as_rule() == Rule::Function)
            .map(|pair| (pair.as_str(), Function::from_pair(pair)))
            .map(|(s, r)| r.map(|f| Node::new(f, s)))
            .collect::<Result<Vec<_>, ASTError>>()?;
        Ok(Program {
            functions,
        })
    }
}

