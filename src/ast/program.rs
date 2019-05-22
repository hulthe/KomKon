use crate::ast::{JavaletteParser, Rule, FromPair, Node, Function, ASTError};
use pest::Parser;
use std::collections::HashMap;
use crate::ast::jl_type::{Type, TypeRef};
use crate::ast::arg::Arg;
use std::rc::Rc;

// TODO: make TypeMap into a struct with custom get methods which return Result<_, ASTError>
pub type TypeMap = HashMap<String, TypeRef>;

#[derive(Debug)]
pub struct Program<'a> {
    pub functions: Vec<Node<'a, Function<'a>>>,
    pub types: TypeMap,
}

impl<'a> Program<'a> {
    pub fn parse(raw: &'a str) -> Result<Self, ASTError> {
        let mut parse = JavaletteParser::parse(Rule::Program, raw)?;
        let program = parse.next().unwrap();
        let mut types: TypeMap = HashMap::new();
        let mut struct_fields: HashMap<String, HashMap<String, String>> = HashMap::new();

        types.insert("void".into(), Type::Void.into());
        types.insert("int".into(), Type::Integer.into());
        types.insert("double".into(), Type::Double.into());
        types.insert("boolean".into(), Type::Boolean.into());

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
                    let (type_ident, ident) = match (pairs.nth(1), pairs.next()) {
                        (Some(type_p), Some(ident_p)) => (type_p.as_str(), ident_p.as_str()),
                        (None, _) => { return Err("TypeDef rule: missing type".into()); }
                        (_, None) => { return Err("TypeDef rule: missing ident".into()); }
                    };

                    let target_type = match types.get(type_ident) {
                        Some(t) => t,
                        None => { return Err(ASTError::NonExistentType(type_ident.to_owned())); }
                    };

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
                .map(|(name, tp)| {
                    match types.get(&tp) {
                        Some(type_rc) => Ok((name, type_rc.clone())),
                        None => Err(ASTError::NonExistentType(tp.clone())),
                    }
                })
                .collect::<Result<_, _>>()?;
            unsafe {
                if let Some(type_rc) = types.get_mut(&name) {
                    if let Type::Struct {
                        fields,
                        ..
                    } = (Rc::into_raw(type_rc.clone()) as *mut Type).as_mut().unwrap() {
                        *fields = new_fields;
                    } else {
                        return Err("Field belonged to type which was not a struct".into());
                    }
                } else {
                    return Err(ASTError::NonExistentType(name.clone()));
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
            types,
        })
    }
}

