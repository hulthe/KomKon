pub trait HasIdentifier {
    fn get_identifier(&self) -> Option<&str>;
}

pub fn search_stack<'a, I, T>(stack: I, s: &str) -> Option<(&'a str, &'a T)>
where T: HasIdentifier,
      I: Iterator<Item=&'a T>, {
    stack.filter_map(|elem| Some((elem.get_identifier()?, elem)))
        .filter(|&(ident, elem)| ident == s)
        .next()
}
