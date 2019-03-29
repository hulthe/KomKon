#![feature(box_syntax, box_patterns)]
#![feature(slice_patterns)]

#[macro_use]
extern crate pest_derive;

#[cfg(test)]
mod tests;

pub mod ast;
pub mod typecheck;

use clap::{clap_app, crate_version, crate_authors, crate_description};
use std::io::{self, Read};
use crate::ast::Program;

fn main() -> io::Result<()> {
    let _matches = clap_app!(jlc =>
        (version: crate_version!())
        (author: crate_authors!("\n"))
        (about: crate_description!())
    ).get_matches();

    let mut buffer = String::new();
    let stdin = io::stdin();
    let mut handle = stdin.lock();

    handle.read_to_string(&mut buffer)?;

    if let Ok(_) = Program::parse(&buffer) {
        eprintln!("OK");
        Ok(())
    } else {
        eprintln!("ERROR");

        // TODO: Custom error type
        Err(io::Error::new(io::ErrorKind::InvalidInput, "Could not compile."))
    }
}
