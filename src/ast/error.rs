use crate::CompilerError;
use crate::util::print_error;
use colored::*;
use pest::error::LineColLocation;
use std::io::{self, Write, StderrLock};
use super::Rule;

pub type PestError = pest::error::Error<Rule>;

#[derive(Debug)]
pub enum ASTError {
    /// Pest could not parse the input string
    Pest(PestError),

    /// The pest-generated token tree could not be parsed as a typed AST.
    /// This is an error in the compiler
    GrammarError(String),

    /// A string literal contained an invalid escape sequence.
    InvalidEscapeSequence(char),
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
                        print_error(w, source_code, &message, sl-1, el-1)?;
                    }
                    ErrorVariant::ParsingError { positives, negatives } => {
                        print_error(
                            w,
                            source_code,
                            &format!(
                                "Parse error at line {}, column {}.\n  Positives: {:?}\n  Negatives: {:?}",
                                sl,
                                sc,
                                positives,
                                negatives,
                            ),
                            sl-1,
                            el-1,
                        )?;
                    }
                }
            }
            ASTError::InvalidEscapeSequence(c) => {
                write!(w, "{} \\{}\n", "Invalid escape sequence:".red(), c)?;
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

