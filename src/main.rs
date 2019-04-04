#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(slice_patterns)]

#[macro_use]
extern crate pest_derive;

#[cfg(test)]
mod tests;

pub mod ast;
pub mod typecheck;
pub mod returncheck;
pub mod minimize;
pub mod voidcheck;
pub mod util;

use clap::{clap_app, crate_version, crate_authors, crate_description};
use std::io::{self, Read, Write, StderrLock};
use crate::ast::Program;
use crate::returncheck::return_check;
use crate::typecheck::{type_check, Error as TypeError};
use crate::minimize::Minimize;
use crate::voidcheck::void_check;
use crate::util::{get_internal_slice_pos, byte_pos_to_line, print_error};
use colored::*;

pub trait CompilerError {
    fn display(&self, writer: &mut StderrLock, source_code: &str) -> io::Result<()>;
}

impl<'a> CompilerError for TypeError<'a> {
    fn display(&self, w: &mut StderrLock, source_code: &str) -> io::Result<()> {
        let kind = match self {
            TypeError::NoContext(kind) => kind,
            TypeError::Context(s, kind) => {
                if let Some((i, len)) = get_internal_slice_pos(source_code, s) {
                    let j = i + len;
                    let i = byte_pos_to_line(source_code, i);
                    let j = byte_pos_to_line(source_code, j);
                    print_error(w, source_code, &format!("{}", kind), i, j)?;
                    return Ok(());
                }
                kind
            }
        };
        write!(w, "  {}\n", &format!("{}", kind).bright_red())?;
        Ok(())
    }
}

fn step<I, O, E: CompilerError>(input: I, source_code: &str, f: fn(I) -> Result<O, E>) -> Result<O, ()> {
    match f(input) {
        Ok(o) => Ok(o),
        Err(e) => {
            let stderr = io::stderr();
            let mut handle = stderr.lock();
            write!(handle, "{}", "ERROR\n".red()).expect("Could not write to stderr");
            e.display(&mut handle, source_code).expect("Could not write compiler error message");
            Err(())
        }
    }
}

fn compile(source_code: &str) -> Result<(), ()> {

    let mut p = step(source_code, source_code, Program::parse)?;
    step(&p, source_code, type_check)?;
    step(&p, source_code, return_check)?;
    p.minimize();
    step(&p, source_code, void_check)?;

    Ok(())
}

fn main() -> io::Result<()> {
    let _matches = clap_app!(jlc =>
        (version: crate_version!())
        (author: crate_authors!("\n"))
        (about: crate_description!())
    ).get_matches();
    let mut buffer = String::new();
    {
        let stdin = io::stdin();
        let mut handle = stdin.lock();
        handle.read_to_string(&mut buffer)?;
    }

    match compile(&buffer) {
        Ok(()) => {
            eprintln!("OK");
            Ok(())
        },
        Err(_) => std::process::exit(1),
    }
}
