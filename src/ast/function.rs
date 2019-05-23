use crate::ast::{Rule, FromPair, TypeRef, TypeMap, Node, Blk, Arg, ASTError};
use pest::iterators::Pair;

#[derive(Debug)]
pub struct Function<'a> {
    pub return_type: TypeRef,
    pub ident: String,
    pub args: Vec<Arg>,
    pub body: Node<'a, Blk<'a>>,
}

impl<'a> FromPair<'a> for Function<'a> {
    fn from_pair(pair: Pair<'a, Rule>, types: &TypeMap) -> Result<Self, ASTError> {
        let mut type_ = None;
        let mut ident = None;
        let mut args = vec![];
        let mut block = None;
        for pair in pair.into_inner().filter(|pair| pair.as_rule() != Rule::WHITESPACE) {
            match pair.as_rule() {
                Rule::Type => type_ = Some(types.get_or(pair.as_str())?.clone()),
                Rule::Ident => ident = Some(pair.as_str().to_owned()),
                Rule::Arg => args.push(Arg::from_pair(pair, types)?),
                Rule::Blk => block = Some(Node::from_pair(pair, types)?),
                r => Err(format!("No matching rule for Function: {:?}", r))?,
            }
        }
        Ok(Function {
            return_type: type_.ok_or("No Type set for Function")?,
            ident: ident.ok_or("No Ident set for Function")?,
            args,
            body: block.ok_or("No Body set for Function")?,
        })
    }
}

