use crate::util::stack::{HasIdentifier, search_stack};

pub struct NameGenerator {
    index: usize,
    prefix: &'static str,
}

impl NameGenerator {
    pub fn new(prefix: &'static str) -> Self {
        NameGenerator{
            index: 0,
            prefix,
        }
    }
    pub fn generate<T>(&mut self, stack: &Vec<T>) -> String
    where T: HasIdentifier {
        loop {
            let s = format!("{}{}", self.prefix, self.index);
            self.index += 1;
            if let None = search_stack(stack.iter().rev(), &s) {
                break s;
            }
        }
    }
}
