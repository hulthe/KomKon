use pest::iterators::Pair;

mod error;
mod jl_type;
mod node;
mod program;
mod top_def;
mod blk;
mod stmt;
mod expr;
mod arg;
mod decl_item;

pub use error::ASTError;
pub use jl_type::Type;
pub use node::Node;
pub use program::Program;
pub use top_def::TopDef;
pub use blk::Blk;
pub use stmt::Stmt;
pub use expr::Expr;
pub use arg::Arg;
pub use decl_item::DeclItem;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct JavaletteParser;


/// Trait for converting pest pairs into concrete types
///
/// # Example
///
/// `pest::Pair{rule: Rule::Expr}` to `Expr`
trait FromPair<'a>
    where Self: Sized {
    fn from_pair(pair: Pair<'a, Rule>) -> Result<Self, ASTError>;
}

