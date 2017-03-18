//! Syntactic passes, aka parsing.
//!
//! Token Parser.

use model::tt;

use super::com::RawParser;

pub fn pop_token<'a, 'g, 'local>(
    kind: tt::Kind,
    raw: &mut RawParser<'a, 'g, 'local>
)
    -> Option<tt::Token>
{
    let tokens = match raw.peek() {
        Some(tt::Node::Run(tokens)) => tokens,
        _ => return None,
    };

    tokens.first().cloned().and_then(|tok| {
        if tok.kind() == kind {
            raw.pop_tokens(1);
            Some(tok)
        } else {
            None
        }
    })
}
