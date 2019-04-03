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

use clap::{clap_app, crate_version, crate_authors, crate_description};
use std::io::{self, Read, Write};
use crate::ast::Program;
use crate::returncheck::{return_check, Error as ReturnError};
use crate::typecheck::{type_check, Error};
use crate::minimize::Minimize;
use crate::voidcheck::VoidCheckable;
use colored::*;

fn get_internal_slice_pos(raw: &str, slice: &str) -> Option<(usize, usize)> {
    let other_s = raw.as_ptr() as usize;
    let other_e = other_s + raw.len();
    let our_s = slice.as_ptr() as usize;
    let our_e = our_s + slice.len();

    if other_s <= our_s && other_e >= our_e {
        Some((our_s - other_s, slice.len()))
    } else {
        None
    }
}

fn print_compiler_error(source: &str, error: Error) -> io::Result<()> {
    let stderr = io::stderr();
    let mut handle = stderr.lock();

    write!(handle, "{}\n", "ERROR".red())?;
    match error {
        Error::Context(slice, kind) => {
            if let Some((start_byte, len)) = get_internal_slice_pos(source, slice) {
                let mut start = 0;
                let mut end = source.len();
                let mut byte = 0;
                let mut iter = source.chars();
                while let Some(c) = iter.next() {
                    byte += c.len_utf8();
                    if c == '\n' {
                        start = byte;
                    }
                    if byte >= start_byte {
                        //println!("Found start @ {}", start);
                        break;
                    }
                }

                for c in iter {
                    if byte >= start_byte + len && c == '\n' {
                        end = byte;
                        //println!("Found end @ {}", end);
                        break;
                    }
                    byte += c.len_utf8();
                }

                let mut line_count = 1;
                for c in source[0..start_byte].chars() {
                    if c == '\n' {
                        line_count += 1;
                    }
                }

                let error_slice = &source[start..end];

                write!(handle, "     {} {}\n", "*".red(), format!("{}", kind).bright_red())?;
                write!(handle, "     {}\n", "|".blue())?;
                for line in error_slice.lines() {
                    write!(handle, "{} {} {}\n", format!("{:4}", line_count).blue(), "|".blue(), line)?;
                    line_count += 1;
                }
                write!(handle, "     {}\n", "|".blue())?;
            } else {
                write!(handle, "Error at \"{}\":\n  {}\n", slice, kind)?;
            }
        }
        Error::NoContext(kind) => {
            write!(handle, "{}", kind)?;
        }
    }
    Ok(())
}

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

    match Program::parse(&buffer) {
        Ok(mut r) => match type_check(&r) {
            Ok(_) => {
                r.minimize();
                match return_check(&r) {
                    Ok(_) => {
                        eprintln!("OK");
                        Ok(())
                    }
                    Err(ReturnError::NonReturningFunction) => {
                        eprintln!("ERROR\nFunction does not always return.");
                        Err(io::Error::new(io::ErrorKind::InvalidInput, "Could not compile."))
                    }
                    Err(ReturnError::UnreachableStatement) => {
                        eprintln!("ERROR\nUnreachable statement.");
                        Err(io::Error::new(io::ErrorKind::InvalidInput, "Could not compile."))
                    }
                }
            }
            Err(e) => {
                print_compiler_error(&buffer, e)?;
                Err(io::Error::new(io::ErrorKind::InvalidInput, "Could not compile."))
            }
        }
        Err(e) => {
            eprintln!("ERROR");
            eprintln!("Failed to parse source code.");
            eprintln!("{:?}", e);

            // TODO: Custom error type
            Err(io::Error::new(io::ErrorKind::InvalidInput, "Could not compile."))
        }
    }
}
