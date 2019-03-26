use pest::Parser;
use pest::iterators::Pair;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct JavaletteParser;

#[derive(Debug)]
pub struct Program(Vec<TopDef>);

#[derive(Debug)]
pub struct TopDef {
    return_type: Type,
    ident: String,
    args: Vec<Arg>,
    body: Blk,
}

#[derive(Debug)]
pub enum Type {
    Integer,
    Double,
    Boolean,
    Void,
}

#[derive(Debug)]
pub struct Arg(Type, String);

#[derive(Debug)]
pub struct Blk(Vec<Stmt>);

#[derive(Debug)]
pub enum Stmt {
    Return(Expr),
    ReturnVoid,
    If(Expr, Box<Stmt>),
    IfElse(Expr, Box<Stmt>, Box<Stmt>),
    While(Expr, Box<Stmt>),
    Block(Blk),
    Assignment(String, Expr),
    Increment(String),
    Decrement(String),
    Expression(Expr),
    Declare(Type, Vec<DeclItem>),
}

#[derive(Debug)]
pub enum DeclItem {
    NoInit(String),
    Init(String, Expr),
}

#[derive(Debug)]
pub enum Expr {
    LOr(Box<Expr>, Box<Expr>),
    LAnd(Box<Expr>, Box<Expr>),
    GT(Box<Expr>, Box<Expr>),
    GE(Box<Expr>, Box<Expr>),
    LT(Box<Expr>, Box<Expr>),
    LE(Box<Expr>, Box<Expr>),
    EQ(Box<Expr>, Box<Expr>),
    NE(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Neg(Box<Expr>),
    Not(Box<Expr>),
    Double(f64),
    Integer(i32),
    Boolean(bool),
    Ident(String),
    Str(String),
    FunctionCall(String, Vec<Expr>),
}

impl Stmt {
    pub fn from_pair(pair: Pair<'_, Rule>) -> Result<Self, ()> {
        let rules = pair.into_inner()
            .map(|pair| (pair.as_rule(), pair))
            .collect::<Vec<_>>();
        let stmt = match &rules[..] {
            [(Rule::Return, _), (Rule::Expr, expp)] => Stmt::Return(Expr::from_pair(expp.clone())?),

            [(Rule::Return, _),] => Stmt::ReturnVoid,

            [(Rule::If, _), (Rule::Expr, expr), (Rule::Stmt, stmt1), (Rule::Else, _), (Rule::Stmt, stmt2)]
                => Stmt::IfElse(
                    Expr::from_pair(expr.clone())?,
                    box Stmt::from_pair(stmt1.clone())?,
                    box Stmt::from_pair(stmt2.clone())?,
                ),

            [(Rule::If, _), (Rule::Expr, expr), (Rule::Stmt, stmt)]
                => Stmt::If(Expr::from_pair(expr.clone())?, box Stmt::from_pair(stmt.clone())?),

            [(Rule::While, _), (Rule::Expr, expr), (Rule::Stmt, stmt)]
                => Stmt::While(Expr::from_pair(expr.clone())?, box Stmt::from_pair(stmt.clone())?),

            [(Rule::Blk, blk)] => Stmt::Block(Blk::from_pair(blk.clone())?),

            [(Rule::Ident, idep), (Rule::Assign, _), (Rule::Expr, expp)]
                => Stmt::Assignment(idep.as_str().to_owned(), Expr::from_pair(expp.clone())?),

            [(Rule::Ident, idep), (Rule::Inc, _)] => Stmt::Increment(idep.as_str().to_owned()),

            [(Rule::Ident, idep), (Rule::Dec, _)] => Stmt::Decrement(idep.as_str().to_owned()),

            [(Rule::Expr, expr)] => Stmt::Expression(Expr::from_pair(expr.clone())?),

            [(Rule::Type, typp), items..] => {
                let items = items.into_iter()
                    .map(|(_, pair)| DeclItem::from_pair(pair.clone()))
                    .collect::<Result<Vec<_>, ()>>()?;
                Stmt::Declare(Type::from_pair(typp.clone())?, items)
            }

            _ => unreachable!("oh noes")
        };
        Ok(stmt)
    }
}

impl Expr {
    fn from_pair_rec(rules: &[(Rule, Pair<'_, Rule>)]) -> Result<Self, ()> {
        Ok(match &rules[..] {
            [(Rule::Expr1, expp), (Rule::LOr, _), tail..]
                => Expr::LOr(box Expr::from_pair(expp.clone())?, box Expr::from_pair_rec(tail)?),

            [(Rule::Expr2, expp), (Rule::LAnd, _), tail..]
                => Expr::LAnd(box Expr::from_pair(expp.clone())?, box Expr::from_pair_rec(tail)?),

            [(Rule::Expr3, expp), (Rule::GT, _), tail..]
                => Expr::GT(box Expr::from_pair(expp.clone())?, box Expr::from_pair_rec(tail)?),
            [(Rule::Expr3, expp), (Rule::GE, _), tail..]
                => Expr::GE(box Expr::from_pair(expp.clone())?, box Expr::from_pair_rec(tail)?),
            [(Rule::Expr3, expp), (Rule::LT, _), tail..]
                => Expr::LT(box Expr::from_pair(expp.clone())?, box Expr::from_pair_rec(tail)?),
            [(Rule::Expr3, expp), (Rule::LE, _), tail..]
                => Expr::LE(box Expr::from_pair(expp.clone())?, box Expr::from_pair_rec(tail)?),
            [(Rule::Expr3, expp), (Rule::EQ, _), tail..]
                => Expr::EQ(box Expr::from_pair(expp.clone())?, box Expr::from_pair_rec(tail)?),
            [(Rule::Expr3, expp), (Rule::NE, _), tail..]
                => Expr::NE(box Expr::from_pair(expp.clone())?, box Expr::from_pair_rec(tail)?),

            [(Rule::Expr4, expp1), (Rule::Plus, _), tail..]
                => Expr::Add(box Expr::from_pair(expp1.clone())?, box Expr::from_pair_rec(tail)?),
            [(Rule::Expr4, expp1), (Rule::Minus, _), tail..]
                => Expr::Sub(box Expr::from_pair(expp1.clone())?, box Expr::from_pair_rec(tail)?),

            [(Rule::Expr5, expp1), (Rule::Star, _), tail..]
                => Expr::Mul(box Expr::from_pair(expp1.clone())?, box Expr::from_pair_rec(tail)?),
            [(Rule::Expr5, expp1), (Rule::Slash, _), tail..]
                => Expr::Div(box Expr::from_pair(expp1.clone())?, box Expr::from_pair_rec(tail)?),
            [(Rule::Expr5, expp1), (Rule::Modulus, _), tail..]
                => Expr::Mod(box Expr::from_pair(expp1.clone())?, box Expr::from_pair_rec(tail)?),

            [(Rule::Not, _), (Rule::Expr6, expp)]
                => Expr::Not(box Expr::from_pair(expp.clone())?),
            [(Rule::Neg, _), (Rule::Expr6, expp)]
                => Expr::Neg(box Expr::from_pair(expp.clone())?),

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

            [(Rule::Ident, idnp), (Rule::LPar, _), exprs.., (Rule::RPar, _)] => {
                let exprs = exprs.into_iter()
                    .map(|(_, pair)| Expr::from_pair(pair.clone()))
                    .collect::<Result<Vec<_>, ()>>()?;
                Expr::FunctionCall(idnp.as_str().to_owned(), exprs)
            }

            [(Rule::Ident, boop)] => Expr::Ident(boop.as_str().to_owned()),

            p => panic!("Oh no: {:#?}\n Something went wrong here^", p),
        })
    }

    pub fn from_pair(pair: Pair<'_, Rule>) -> Result<Self, ()> {
        let rules = pair.into_inner()
            .map(|pair| (pair.as_rule(), pair))
            .collect::<Vec<_>>();
        Self::from_pair_rec(&rules[..])
    }
}

impl DeclItem {
    pub fn from_pair(pair: Pair<'_, Rule>) -> Result<Self, ()> {
        let rules = pair.into_inner()
            .map(|pair| (pair.as_rule(), pair))
            .collect::<Vec<_>>();
        Ok(match &rules[..] {
            [(Rule::Ident, idenp), (Rule::Assign, _), (Rule::Expr, expp)]
                => DeclItem::Init(idenp.as_str().to_owned(), Expr::from_pair(expp.clone())?),

            [(Rule::Ident, idenp)] => DeclItem::NoInit(idenp.as_str().to_owned()),

            _ => panic!("oh noooes")
        })
    }
}

impl Type {
    pub fn from_pair(pair: Pair<'_, Rule>) -> Result<Self, ()> {
        match pair.as_str() {
            "int" => Ok(Type::Integer),
            "double" => Ok(Type::Double),
            "boolean" => Ok(Type::Boolean),
            "void" => Ok(Type::Void),
            _ => Err(()),
        }
    }
}

impl Program {
    pub fn parse(raw: &str) -> Result<Self, ()> {
        let mut parse = JavaletteParser::parse(Rule::Program, raw).unwrap();
        println!("{:#?}", parse);
        let program = parse.next().unwrap();
        let top_defs = program.into_inner()
            .filter(|pair| pair.as_rule() == Rule::TopDef)
            .map(TopDef::from_pair)
            .collect::<Result<Vec<TopDef>,()>>()?;
        Ok(Program(top_defs))
    }
}

impl Arg {
    pub fn from_pair(pair: Pair<'_, Rule>) -> Result<Self, ()> {
        let mut type_ = None;
        let mut ident = None;
        for pair in pair.into_inner() {
            match pair.as_rule() {
                Rule::Type => type_ = Some(Type::from_pair(pair)?),
                Rule::Ident => ident = Some(pair.as_str().to_owned()),
                _ => unreachable!("Non-valid Rule in TopDef")
            }
        }
        Ok(Arg(type_.unwrap(), ident.unwrap()))
    }
}

impl Blk {
    pub fn from_pair(pair: Pair<'_, Rule>) -> Result<Self, ()> {
        let statements = pair.into_inner()
            .filter(|pair| pair.as_rule() == Rule::Stmt)
            .map(Stmt::from_pair)
            .collect::<Result<Vec<Stmt>,()>>()?;
        Ok(Blk(statements))
    }
}

impl TopDef {
    pub fn from_pair(pair: Pair<'_, Rule>) -> Result<Self, ()> {
        let mut type_ = None;
        let mut ident = None;
        let mut args = vec![];
        let mut block = None;
        for pair in pair.into_inner().filter(|pair| pair.as_rule() != Rule::WHITESPACE) {
            match pair.as_rule() {
                Rule::Type => type_ = Some(Type::from_pair(pair)?),
                Rule::Ident => ident = Some(pair.as_str().to_owned()),
                Rule::Arg => args.push(Arg::from_pair(pair)?),
                Rule::Blk => block = Some(Blk::from_pair(pair)?),
                _ => unreachable!("Non-valid Rule in TopDef")
            }
        }
        Ok(TopDef{
            return_type: type_.ok_or(())?,
            ident: ident.ok_or(())?,
            args,
            body: block.ok_or(())?,
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse() {
        let p = Program::parse(r#"int main() {
                1+1 % 5 || 5==2;
                main(1, 2.4e-3);
                return hello;
            }

            void hello(int x, int y) {
                return x - y + 1;
                {
                    int a = 23, c, b;
                }
            }
            "#).expect("Failed");
        eprintln!("{:#?}", p);
        assert!(false);
    }
}
