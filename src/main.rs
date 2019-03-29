#![feature(box_syntax, box_patterns)]
#![feature(slice_patterns)]

#[macro_use]
extern crate pest_derive;

#[cfg(test)]
mod tests;

pub mod ast;
pub mod typecheck;

fn main() {
    unimplemented!("main() is not yet implemented!");
}
