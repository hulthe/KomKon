use crate::ast::{Rule, FromPair, Node, ASTError, VarRef};
use pest::iterators::Pair;

#[derive(Debug)]
pub enum Expr<'a> {
    LOr(Node<'a, Expr<'a>>, Node<'a, Expr<'a>>),
    LAnd(Node<'a, Expr<'a>>, Node<'a, Expr<'a>>),
    GT(Node<'a, Expr<'a>>, Node<'a, Expr<'a>>),
    GE(Node<'a, Expr<'a>>, Node<'a, Expr<'a>>),
    LT(Node<'a, Expr<'a>>, Node<'a, Expr<'a>>),
    LE(Node<'a, Expr<'a>>, Node<'a, Expr<'a>>),
    EQ(Node<'a, Expr<'a>>, Node<'a, Expr<'a>>),
    NE(Node<'a, Expr<'a>>, Node<'a, Expr<'a>>),
    Mul(Node<'a, Expr<'a>>, Node<'a, Expr<'a>>),
    Div(Node<'a, Expr<'a>>, Node<'a, Expr<'a>>),
    Mod(Node<'a, Expr<'a>>, Node<'a, Expr<'a>>),
    Add(Node<'a, Expr<'a>>, Node<'a, Expr<'a>>),
    Sub(Node<'a, Expr<'a>>, Node<'a, Expr<'a>>),
    Neg(Node<'a, Expr<'a>>),
    Not(Node<'a, Expr<'a>>),
    Double(f64),
    Integer(i32),
    Boolean(bool),
    Var(VarRef),
    Str(String),
    FunctionCall(String, Vec<Node<'a, Expr<'a>>>),
}

impl<'a> FromPair<'a> for Expr<'a> {
    fn from_pair(pair: Pair<'a, Rule>) -> Result<Self, ASTError> {
        let slice = pair.as_str();
        let rules = pair.into_inner()
            .map(|pair| (pair.as_rule(), pair))
            .collect::<Vec<_>>();
        Self::from_pair_rec(slice, &rules[..])
    }
}

impl<'a> Expr<'a> {
    /// Recursively un-nests an expression
    fn from_pair_rec(slice: &'a str, rules: &[(Rule, Pair<'a, Rule>)]) -> Result<Self, ASTError> {
        Ok(match &rules[..] {
            [(Rule::Expr1, expp), (Rule::LOr, _), tail..]
            => Expr::LOr(Node::from_pair(expp.clone())?, Node::new(Expr::from_pair_rec(slice, tail)?, slice)),

            [(Rule::Expr2, expp), (Rule::LAnd, _), tail..]
            => Expr::LAnd(Node::from_pair(expp.clone())?, Node::new(Expr::from_pair_rec(slice, tail)?, slice)),

            [(Rule::Expr3, expp), (Rule::GT, _), tail..]
            => Expr::GT(Node::from_pair(expp.clone())?, Node::new(Expr::from_pair_rec(slice, tail)?, slice)),
            [(Rule::Expr3, expp), (Rule::GE, _), tail..]
            => Expr::GE(Node::from_pair(expp.clone())?, Node::new(Expr::from_pair_rec(slice, tail)?, slice)),
            [(Rule::Expr3, expp), (Rule::LT, _), tail..]
            => Expr::LT(Node::from_pair(expp.clone())?, Node::new(Expr::from_pair_rec(slice, tail)?, slice)),
            [(Rule::Expr3, expp), (Rule::LE, _), tail..]
            => Expr::LE(Node::from_pair(expp.clone())?, Node::new(Expr::from_pair_rec(slice, tail)?, slice)),
            [(Rule::Expr3, expp), (Rule::EQ, _), tail..]
            => Expr::EQ(Node::from_pair(expp.clone())?, Node::new(Expr::from_pair_rec(slice, tail)?, slice)),
            [(Rule::Expr3, expp), (Rule::NE, _), tail..]
            => Expr::NE(Node::from_pair(expp.clone())?, Node::new(Expr::from_pair_rec(slice, tail)?, slice)),

            [(Rule::Expr4, expp1), (Rule::Plus, _), tail..]
            => Expr::Add(Node::from_pair(expp1.clone())?, Node::new(Expr::from_pair_rec(slice, tail)?, slice)),
            [(Rule::Expr4, expp1), (Rule::Minus, _), tail..]
            => Expr::Sub(Node::from_pair(expp1.clone())?, Node::new(Expr::from_pair_rec(slice, tail)?, slice)),

            [(Rule::Expr5, expp1), (Rule::Star, _), tail..]
            => Expr::Mul(Node::from_pair(expp1.clone())?, Node::new(Expr::from_pair_rec(slice, tail)?, slice)),
            [(Rule::Expr5, expp1), (Rule::Slash, _), tail..]
            => Expr::Div(Node::from_pair(expp1.clone())?, Node::new(Expr::from_pair_rec(slice, tail)?, slice)),
            [(Rule::Expr5, expp1), (Rule::Modulus, _), tail..]
            => Expr::Mod(Node::from_pair(expp1.clone())?, Node::new(Expr::from_pair_rec(slice, tail)?, slice)),

            [(Rule::Not, _), (Rule::Expr6, expp)]
            => Expr::Not(Node::from_pair(expp.clone())?),
            [(Rule::Neg, _), (Rule::Expr6, expp)]
            => Expr::Neg(Node::from_pair(expp.clone())?),

            [(Rule::Expr1, expp)] |
            [(Rule::Expr2, expp)] |
            [(Rule::Expr3, expp)] |
            [(Rule::Expr4, expp)] |
            [(Rule::Expr5, expp)] |
            [(Rule::Expr6, expp)] => Expr::from_pair(expp.clone())?,

            [(Rule::Double, dblp)] => Expr::Double(dblp.as_str().parse().unwrap()),

            [(Rule::Integer, intp)] => Expr::Integer(intp.as_str().parse().unwrap()),

            [(Rule::Boolean, boop)] => Expr::Boolean(boop.as_str().parse().unwrap()),

            [(Rule::String, strp)] => {
                let s = strp.as_str();
                let l = s.len()-2;
                let mut s: String = String::with_capacity(l);
                let mut escaped: bool = false;
                for c in strp.as_str()
                    .chars()
                    .skip(1)  // Skip starting "
                    .take(l) {// Skip ending "
                    if escaped {
                        s.push(match c {
                            'n' => '\n',
                            't' => '\t',
                            '\\' => '\\',
                            '"' => '"',
                            c => return Err(ASTError::InvalidEscapeSequence(c)),
                        });
                        escaped = false;
                    } else {
                        if c == '\\' {
                            escaped = true;
                        } else {
                            s.push(c)
                        }
                    }
                }

                Expr::Str(s)
            }

            [(Rule::LPar, _), (Rule::Expr, expp), (Rule::RPar, _)]
            => Expr::from_pair(expp.clone())?,

            [(Rule::Ident, idnp), (Rule::LPar, _), exprs.., (Rule::RPar, _)] => {
                let exprs = exprs.into_iter()
                    .map(|(_, pair)| Expr::from_pair(pair.clone()))
                    .map(|r| r.map(|expr| Node::new(expr, slice)))
                    .collect::<Result<Vec<_>, ASTError>>()?;
                Expr::FunctionCall(idnp.as_str().to_owned(), exprs)
            }

            [(Rule::Variable, v)] => Expr::Var(VarRef::from_pair(v.clone())?),

            _ => Err("No matching rule for Expr")?,
        })
    }
}

