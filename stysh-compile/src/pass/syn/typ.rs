//! Syntactic passes, aka parsing.
//!
//! Type Parser.

use model::syn::TypeIdentifier;
use model::tt;

use super::com::RawParser;

pub struct TypeParser<'a, 'g, 'local> {
    raw: RawParser<'a, 'g, 'local>
}

pub fn parse_type<'a, 'g, 'local>(raw: &mut RawParser<'a, 'g, 'local>)
    -> TypeIdentifier
{
    let mut parser = TypeParser::new(*raw);
    let t = parser.parse();
    *raw = parser.into_raw();
    t
}

impl<'a, 'g, 'local> TypeParser<'a, 'g, 'local> {
    pub fn new(raw: RawParser<'a, 'g, 'local>) -> TypeParser<'a, 'g, 'local> {
        TypeParser { raw: raw }
    }

    pub fn into_raw(self) -> RawParser<'a, 'g, 'local> { self.raw }

    pub fn parse(&mut self) -> TypeIdentifier {
        let result = match self.raw.peek() {
            Some(tt::Node::Run(tokens)) => tokens,
            _ => unimplemented!(),
        };

        assert!(!result.is_empty());
        assert_eq!(result[0].kind(), tt::Kind::NameType);

        self.raw.pop_tokens(1);

        TypeIdentifier(result[0].range())
    }
}
