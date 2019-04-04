use crate::ast::{Rule, FromPair, Type, Node, Blk, Arg, ASTError};
use pest::iterators::Pair;

#[derive(Debug)]
pub struct TopDef<'a> {
    pub return_type: Type,
    pub ident: String,
    pub args: Vec<Arg>,
    pub body: Node<'a, Blk<'a>>,
}

impl<'a> FromPair<'a> for TopDef<'a> {
    fn from_pair(pair: Pair<'a, Rule>) -> Result<Self, ASTError> {
        let mut type_ = None;
        let mut ident = None;
        let mut args = vec![];
        let mut block = None;
        for pair in pair.into_inner().filter(|pair| pair.as_rule() != Rule::WHITESPACE) {
            match pair.as_rule() {
                Rule::Type => type_ = Some(Type::from_pair(pair)?),
                Rule::Ident => ident = Some(pair.as_str().to_owned()),
                Rule::Arg => args.push(Arg::from_pair(pair)?),
                Rule::Blk => block = Some(Node::from_pair(pair)?),
                _ => Err("No matching rule for TopDef")?,
            }
        }
        Ok(TopDef {
            return_type: type_.ok_or("No Type set for TopDef")?,
            ident: ident.ok_or("No Ident set for TopDef")?,
            args,
            body: block.ok_or("No Body set for TopDef")?,
        })
    }
}
