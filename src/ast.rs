use pest::Parser;
use pest::iterators::Pair;
use pest::error::LineColLocation;
use std::fmt::{self, Display, Formatter};
use std::io::{self, Write, StderrLock};
use colored::*;
use crate::CompilerError;
use crate::util::print_error;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct JavaletteParser;

pub type PestError = pest::error::Error<Rule>;

#[derive(Debug)]
pub enum ASTError {
    /// Pest could not parse the input string
    Pest(PestError),

    /// The pest-generated token tree could not be parsed as a typed AST.
    /// This is an error in the compiler
    GrammarError(String),
}

impl CompilerError for ASTError {
    fn display(&self, w: &mut StderrLock, source_code: &str) -> io::Result<()> {
        match self {
            ASTError::GrammarError(s) => {
                write!(w, "{}\n{}\n", "Something went wrong during parsing.".bright_red(), s.bright_red())?;
            }
            ASTError::Pest(e) => {
                let (sl, sc, el, _ec) = match e.line_col {
                    LineColLocation::Pos((sl, sc)) => (sl, sc, sl, sc),
                    LineColLocation::Span((sl, sc), (el, ec)) => (sl, sc, el, ec),
                };

                use pest::error::ErrorVariant;
                match &e.variant {
                    ErrorVariant::CustomError { message } => {
                        print_error(w, source_code, &message, sl, el)?;
                    }
                    ErrorVariant::ParsingError { positives, negatives } => {
                        print_error(w, source_code,
                                    &format!("Parse error at line {}, column {}.\n  Positives: {:?}\n  Negatives: {:?}", sl, sc, positives, negatives), sl, el)?;
                    }
                }
            }
        }
        Ok(())
    }
}

impl From<PestError> for ASTError {
    fn from(e: PestError) -> ASTError {
        ASTError::Pest(e)
    }
}

impl From<String> for ASTError {
    fn from(s: String) -> ASTError {
        ASTError::GrammarError(s)
    }
}

impl From<&str> for ASTError {
    fn from(s: &str) -> ASTError {
        ASTError::GrammarError(s.to_owned())
    }
}


/// This wraps AST-elements to include meta data
#[derive(Debug, Clone)]
pub struct Node<'a, T> {
    pub elem: Box<T>,
    slice: &'a str,
}

impl<'a, T> Node<'a, T> {
    fn new(wrap: T, slice: &'a str) -> Node<'a, T> {
        Node {
            elem: box wrap,
            slice,
        }
    }

    /// Get the source code for this element
    pub fn get_slice(&self) -> &'a str {
        self.slice
    }
}

impl<'a, T> AsRef<T> for Node<'a, T> {
    fn as_ref(&self) -> &T {
        &self.elem
    }
}

impl<'a, T> AsMut<T> for Node<'a, T> {
    fn as_mut(&mut self) -> &mut T {
        &mut self.elem
    }
}

/// Trait for converting pest pairs into concrete types
///
/// # Example
///
/// `pest::Pair{rule: Rule::Expr}` to `Expr`
trait FromPair<'a>
    where Self: Sized {
    fn from_pair(pair: Pair<'a, Rule>) -> Result<Self, ASTError>;
}

impl<'a, T> FromPair<'a> for Node<'a, T>
    where T: FromPair<'a> + Sized {
    fn from_pair(pair: Pair<'a, Rule>) -> Result<Self, ASTError> {
        Ok(Node {
            slice: pair.as_str(),
            elem: box T::from_pair(pair)?,
        })
    }
}

#[derive(Debug)]
pub struct Program<'a>(pub Vec<Node<'a, TopDef<'a>>>);

#[derive(Debug)]
pub struct TopDef<'a> {
    pub return_type: Type,
    pub ident: String,
    pub args: Vec<Arg>,
    pub body: Node<'a, Blk<'a>>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Type {
    Integer,
    Double,
    Boolean,
    Void,
    String,
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            Type::Integer => "integer",
            Type::Double => "double",
            Type::Boolean => "boolean",
            Type::Void => "void",
            Type::String => "string",
        })
    }
}

#[derive(Debug, Clone)]
pub struct Arg(pub Type, pub String);

#[derive(Debug)]
pub struct Blk<'a>(pub Vec<Node<'a, Stmt<'a>>>);

#[derive(Debug)]
pub enum Stmt<'a> {
    Return(Expr<'a>),
    ReturnVoid,
    If(Expr<'a>, Node<'a, Stmt<'a>>),
    IfElse(Expr<'a>, Node<'a, Stmt<'a>>, Node<'a, Stmt<'a>>),
    While(Expr<'a>, Node<'a, Stmt<'a>>),
    Block(Blk<'a>),
    Assignment(String, Expr<'a>),
    Increment(String),
    Decrement(String),
    Expression(Expr<'a>),
    Declare(Type, Vec<DeclItem<'a>>),
    Empty,
}

#[derive(Debug)]
pub enum DeclItem<'a> {
    NoInit(String),
    Init(String, Expr<'a>),
}

impl<'a> DeclItem<'a> {
    pub fn get_ident(&self) -> &str {
        match self {
            DeclItem::NoInit(ident) | DeclItem::Init(ident, _) => &ident
        }
    }
}

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
    Ident(String),
    Str(String),
    FunctionCall(String, Vec<Node<'a, Expr<'a>>>),
}

impl<'a> FromPair<'a> for Stmt<'a> {
    fn from_pair(pair: Pair<'a, Rule>) -> Result<Self, ASTError> {
        let rules = pair.into_inner()
            .map(|pair| (pair.as_rule(), pair))
            .collect::<Vec<_>>();
        let stmt = match &rules[..] {
            [(Rule::Return, _), (Rule::Expr, expp)] => Stmt::Return(Expr::from_pair(expp.clone())?),

            [(Rule::Return, _), ] => Stmt::ReturnVoid,

            [(Rule::If, _), (Rule::Expr, expr), (Rule::Stmt, stmt1), (Rule::Else, _), (Rule::Stmt, stmt2)]
            => Stmt::IfElse(
                Expr::from_pair(expr.clone())?,
                Node::from_pair(stmt1.clone())?,
                Node::from_pair(stmt2.clone())?,
            ),

            [(Rule::If, _), (Rule::Expr, expr), (Rule::Stmt, stmt)]
            => Stmt::If(Expr::from_pair(expr.clone())?, Node::from_pair(stmt.clone())?),

            [(Rule::While, _), (Rule::Expr, expr), (Rule::Stmt, stmt)]
            => Stmt::While(Expr::from_pair(expr.clone())?, Node::from_pair(stmt.clone())?),

            [(Rule::Blk, blk)] => Stmt::Block(Blk::from_pair(blk.clone())?),

            [(Rule::Ident, idep), (Rule::Assign, _), (Rule::Expr, expp)]
            => Stmt::Assignment(idep.as_str().to_owned(), Expr::from_pair(expp.clone())?),

            [(Rule::Ident, idep), (Rule::Inc, _)] => Stmt::Increment(idep.as_str().to_owned()),

            [(Rule::Ident, idep), (Rule::Dec, _)] => Stmt::Decrement(idep.as_str().to_owned()),

            [(Rule::Expr, expr)] => Stmt::Expression(Expr::from_pair(expr.clone())?),

            [(Rule::Type, typp), items..] => {
                let items = items.into_iter()
                    .map(|(_, pair)| DeclItem::from_pair(pair.clone()))
                    .collect::<Result<Vec<_>, ASTError>>()?;
                Stmt::Declare(Type::from_pair(typp.clone())?, items)
            }

            [] => Stmt::Empty,

            _ => Err("No matching rule for Stmt")?,
        };
        Ok(stmt)
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

            [(Rule::String, strp)] => Expr::Str(strp.as_str().to_owned()),

            [(Rule::LPar, _), (Rule::Expr, expp), (Rule::RPar, _)]
            => Expr::from_pair(expp.clone())?,

            [(Rule::Ident, idnp), (Rule::LPar, _), exprs.., (Rule::RPar, _)] => {
                let exprs = exprs.into_iter()
                    .map(|(_, pair)| Expr::from_pair(pair.clone()))
                    .map(|r| r.map(|expr| Node::new(expr, slice)))
                    .collect::<Result<Vec<_>, ASTError>>()?;
                Expr::FunctionCall(idnp.as_str().to_owned(), exprs)
            }

            [(Rule::Ident, boop)] => Expr::Ident(boop.as_str().to_owned()),

            _ => Err("No matching rule for Expr")?,
        })
    }
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

impl<'a> FromPair<'a> for DeclItem<'a> {
    fn from_pair(pair: Pair<'a, Rule>) -> Result<Self, ASTError> {
        let rules = pair.into_inner()
            .map(|pair| (pair.as_rule(), pair))
            .collect::<Vec<_>>();
        Ok(match &rules[..] {
            [(Rule::Ident, idenp), (Rule::Assign, _), (Rule::Expr, expp)]
            => DeclItem::Init(idenp.as_str().to_owned(), Expr::from_pair(expp.clone())?),

            [(Rule::Ident, idenp)] => DeclItem::NoInit(idenp.as_str().to_owned()),

            _ => Err("No matching rule for Stmt")?,
        })
    }
}

impl<'a> FromPair<'a> for Type {
    fn from_pair(pair: Pair<'a, Rule>) -> Result<Self, ASTError> {
        match pair.as_str() {
            "int" => Ok(Type::Integer),
            "double" => Ok(Type::Double),
            "boolean" => Ok(Type::Boolean),
            "void" => Ok(Type::Void),
            t => Err(format!("\"{}\" is not a valid Type", t))?,
        }
    }
}

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

impl<'a> FromPair<'a> for Arg {
    fn from_pair(pair: Pair<'a, Rule>) -> Result<Self, ASTError> {
        let mut type_ = None;
        let mut ident = None;
        for pair in pair.into_inner() {
            match pair.as_rule() {
                Rule::Type => type_ = Some(Type::from_pair(pair)?),
                Rule::Ident => ident = Some(pair.as_str().to_owned()),
                _ => Err("No matching rule for Arg")?,
            }
        }
        Ok(Arg(type_.unwrap(), ident.unwrap()))
    }
}

impl<'a> FromPair<'a> for Blk<'a> {
    fn from_pair(pair: Pair<'a, Rule>) -> Result<Self, ASTError> {
        let statements = pair.into_inner()
            .filter(|pair| pair.as_rule() == Rule::Stmt)
            .map(|p| (p.as_str(), Stmt::from_pair(p)))
            .map(|(s, r)| r.map(|stmt| Node::new(stmt, s)))
            .collect::<Result<Vec<_>, ASTError>>()?;
        Ok(Blk(statements))
    }
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

