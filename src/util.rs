use std::io::{self, Write};
use colored::*;

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
            write!(w, "{} {} {}\n", format!("{:4}", i).blue(), "|".blue(), l)?;
        }
        write!(w, "     {}\n", "|".blue())?;
    }

    Ok(())
}
