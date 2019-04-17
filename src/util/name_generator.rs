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

    pub fn generate<'a, I, T>(&mut self, stack: I) -> String
    where I: Clone + DoubleEndedIterator<Item=&'a T>,
          T: 'a + HasIdentifier, {
        loop {
            let stack = stack.clone();
            let s = format!("{}{}", self.prefix, self.index);
            self.index += 1;
            if let None = search_stack(stack.rev(), &s) {
                break s;
            }
        }
    }
}
