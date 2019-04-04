use pest::iterators::Pair;
use crate::ast::{Rule, FromPair, ASTError};

/// This wraps AST-elements to include meta data
#[derive(Debug, Clone)]
pub struct Node<'a, T> {
    pub elem: Box<T>,
    slice: &'a str,
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

impl<'a, T> Node<'a, T> {
    pub fn new(wrap: T, slice: &'a str) -> Node<'a, T> {
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

