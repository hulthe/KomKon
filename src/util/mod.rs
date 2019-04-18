pub mod stack;
mod name_generator;

use std::io::{self, Write};
use std::fmt::{self, Formatter};
use colored::*;
pub use name_generator::NameGenerator;

pub fn get_internal_slice_pos(raw: &str, slice: &str) -> Option<(usize, usize)> {
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

pub fn byte_pos_to_line(s: &str, i: usize) -> usize {
    let mut iter= s.chars();
    let mut byte: usize = 0;
    let mut line = 0;
    while let Some(c) = iter.next() {
        byte += c.len_utf8();
        if c == '\n' {
            line += 1;
        }
        if byte >= i {
            break;
        }
    }
    line
}

pub fn print_error<W>(w: &mut W, source: &str, msg: &str, start_line: usize, end_line: usize) -> io::Result<()>
where W: Write {
    for l in msg.lines() {
        write!(w, "     {} {}\n", "*".red(), l.bright_red())?;
    }

    let lines: Vec<_> = source.lines().enumerate().filter(|&(i, _)| i >= start_line && i <= end_line).collect();
    if lines.len() > 0 {
        write!(w, "     {}\n", "|".blue())?;
        for (i, l) in lines {
            write!(w, "{} {} {}\n", format!("{:4}", i+1).blue(), "|".blue(), l)?;
        }
        write!(w, "     {}\n", "|".blue())?;
    }

    Ok(())
}

// write a list with a non-trailing separator.
pub fn write_list<F, I, D>(f: &mut Formatter, separator: &str, iter: I, disp: F) -> fmt::Result
where F: Fn(&mut Formatter, D) -> fmt::Result,
      I: Iterator<Item=D>,
{
    let mut fst = true;
    for item in iter {
        if !fst {
            write!(f, "{}", separator)?;
        }
        fst = false;
        disp(f, item)?;
    }
    Ok(())
}

