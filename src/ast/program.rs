use crate::ast::{JavaletteParser, Rule, FromPair, Node, TopDef, ASTError};
use pest::Parser;

#[derive(Debug)]
pub struct Program<'a>(pub Vec<Node<'a, TopDef<'a>>>);

impl<'a> Program<'a> {
    pub fn parse(raw: &'a str) -> Result<Self, ASTError> {
        let mut parse = JavaletteParser::parse(Rule::Program, raw)?;
        let program = parse.next().unwrap();
        let top_defs = program.into_inner()
            .filter(|pair| pair.as_rule() == Rule::TopDef)
            .map(|pair| (pair.as_str(), TopDef::from_pair(pair)))
            .map(|(s, r)| r.map(|t| Node::new(t, s)))
            .collect::<Result<Vec<_>, ASTError>>()?;
        Ok(Program(top_defs))
    }
}

