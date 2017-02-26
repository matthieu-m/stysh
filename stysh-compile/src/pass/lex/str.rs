//! String Literal lexing pass.

use basic::{com, mem};

use model::tt::*;
use super::raw::RawToken;

pub fn parse<'g>(
    tok: RawToken,
    offset: usize,
    with_new_lines: bool,
    global_arena: &'g mem::Arena,
    local_arena: &mem::Arena
)
    -> (Token, &'g [StringFragment], Token)
{
    let quote = tok.raw[offset];

    debug_assert!(offset == 0 || (offset == 1 && tok.raw[0] == b'b'));
    debug_assert!(tok.raw[offset] == 0x22 || tok.raw[offset] == b'\'');

    let mut raw = &tok.raw[offset+1..];

    let start_token = {
        let kind = if quote == b'\'' {
            Kind::QuoteSingle
        } else {
            Kind::QuoteDouble
        };
        Token::new(kind, tok.offset + offset, 1)
    };

    let end_token = {
        let length = if quote == tok.raw[tok.raw.len() - 1] {
            1
        } else {
            0
        };
        raw = &raw[..raw.len() - length];
        let o = tok.offset + tok.raw.len() - length;
        Token::new(start_token.kind(), o, length)
    };

    let fragments = parse_fragments(
        raw,
        quote,
        start_token.offset() + 1,
        tok.line_indent,
        with_new_lines,
        global_arena,
        local_arena
    );
    
    (start_token, fragments, end_token)
}

//
//  Implementation Details
//
fn parse_fragments<'g>(
    raw: &[u8],
    quote: u8,
    offset: usize,
    line_indent: usize,
    with_new_lines: bool,
    global_arena: &'g mem::Arena,
    local_arena: &mem::Arena
)
    -> &'g [StringFragment]
{
    let mut raw = raw;

    let mut buffer = mem::Array::new(local_arena);

    if with_new_lines {
        let first_new_line =
            raw.iter().position(|&b| b == b'\n').expect("LF in multi-line");
        raw = &raw[first_new_line + 1..];

        while let Some(&b' ') = raw.last() {
            raw = &raw[..raw.len() - 1];
        }

        if let Some(&b'\n') = raw.last() {
            raw = &raw[..raw.len() - 1];
        }
    }

    let offset_and_length = offset + raw.len();
    
    loop {
        let offset = offset_and_length - raw.len();
        raw = parse_line(quote, offset, raw, &mut buffer);

        if raw.is_empty() {
            return global_arena.insert_slice(buffer.into_slice());
        }

        debug_assert!(with_new_lines);

        if raw.len() <= line_indent {
            return global_arena.insert_slice(buffer.into_slice());
        }

        debug_assert!(
            raw.iter().position(|&b| b != b' ').unwrap() >= line_indent
        );

        raw = &raw[line_indent..];
    }
}

fn parse_line<'a, 'local>(
    quote: u8,
    offset: usize,
    raw: &'a [u8],
    buffer: &mut mem::Array<'local, StringFragment>
)
    -> &'a [u8]
{
    let mut raw = raw;
    let mut offset = offset;

    while !raw.is_empty() {
        let (new_raw, fragment, eol) = parse_fragment(quote, offset, raw);

        buffer.push(fragment);

        if eol { return new_raw; }

        debug_assert!(
            raw.len() != new_raw.len(),
            "raw ({} @ {}): '{:?}'",
            raw.len(),
            offset,
            raw
        );

        debug_assert!(
            fragment.offset() == offset,
            "raw ({} @ {}): '{:?}', fragment ({} @ {}): '{:?}'",
            raw.len(),
            offset,
            raw,
            fragment.length(),
            fragment.offset(),
            fragment
        );

        //  When escaping a character, it is skipped.
        debug_assert!(
            (fragment.length() == raw.len() - new_raw.len()) ||
            (fragment.length() == raw.len() - new_raw.len() - 1),
            "raw ({}): '{:?}', fragment ({}): '{:?}', new_raw ({}): '{:?}'",
            raw.len(),
            raw,
            fragment.length(),
            fragment,
            new_raw.len(),
            new_raw
        );

        offset += raw.len() - new_raw.len();
        raw = new_raw;
    }

    return &[];
}

fn parse_fragment<'a>(quote: u8, offset: usize, raw: &'a [u8])
    -> (&'a [u8], StringFragment, bool)
{
    debug_assert!(!raw.is_empty());

    fn special(raw: &[u8], offset: usize) -> StringFragment {
        if raw.is_empty() {
            return StringFragment::Interpolated(
                Token::new(Kind::StringPositionalArgument, offset, 0),
                Token::new(Kind::Colon, offset, 0),
                Token::new(Kind::StringFormat, offset, 0)
            );
        }

        if raw[0] >= b'A' && raw[0] <= b'Z' {
            return StringFragment::SpecialCharacter(
                Token::new(Kind::StringSpecialCharacter, offset, raw.len())
            );
        }

        let (id, colon, format) =
            if let Some(colon) = raw.iter().position(|&b| b == b':') {
                (&raw[..colon], &raw[colon..colon + 1], &raw[colon + 1..])
            } else {
                (raw, &raw[0..0], &raw[0..0])
            };

        //  TODO(matthieum): validate identifier, index and name.
        let id = match id.get(0) {
            None => Token::new(Kind::StringPositionalArgument, offset, 0),
            Some(&b'.') =>
                Token::new(Kind::StringNamedArgument, offset, id.len()),
            Some(&c) if c >= b'0' && c <= b'9' =>
                Token::new(Kind::StringIndexedArgument, offset, id.len()),
            Some(&c) if c >= b'a' && c <= b'z' =>
                Token::new(Kind::StringIdentifier, offset, id.len()),
            _ => return StringFragment::Unexpected(
                com::Range::new(offset - 1, raw.len() + 2)
            ),
        };

        let colon =
            Token::new(Kind::Colon, id.offset() + id.length(), colon.len());

        //  TODO(matthieum): validate format.
        let format = Token::new(
            Kind::StringFormat,
            colon.offset() + colon.length(),
            format.len()
        );

        StringFragment::Interpolated(id, colon, format)
    }

    fn text(offset: usize, length: usize) -> StringFragment {
        StringFragment::Text(
            Token::new(Kind::StringText, offset, length)
        )
    }

    fn unexpected(offset: usize, length: usize) -> StringFragment {
        StringFragment::Unexpected(com::Range::new(offset, length))
    }

    if raw.len() >= 2 && raw[0] == b'{' && raw[1] != b'{' {
        let mut start = 0;
        loop {
            let end =
                raw[start..].iter()
                    .position(|&b| b == b'}' || b == b'\n')
                    .map(|p| p + start)
                    .unwrap_or(raw.len());

            return match raw.get(end) {
                None => (&[], unexpected(offset, end), false),
                Some(&b'\n') => (&raw[end + 1..], unexpected(offset, end), true),
                Some(&b'}') if raw.get(end + 1) == Some(&b'}') => {
                    start = end + 2;
                    continue
                },
                Some(&b'}') =>
                    (&raw[end + 1..], special(&raw[1..end], offset + 1), false),
                _ => unreachable!(),
            };
        }
    }

    if raw.len() >= 2 && raw[0] == b'}' && raw[1] != b'}' {
        return (&raw[1..], unexpected(offset, 1), false);
    }

    let end =
        raw.iter()
            .position(|&b| b == b'{' || b == b'}' || b == quote || b == b'\n')
            .unwrap_or(raw.len());

    match raw.get(end) {
        None => (&[], text(offset, raw.len()), false),

        Some(&c) if c == quote => {
            debug_assert!(raw[end+1] == quote);
            (&raw[end+2..], text(offset, end + 1), false)
        },

        Some(&b'{') if raw.get(end + 1) == Some(&b'{') => 
            (&raw[end+2..], text(offset, end + 1), false),

        Some(&b'}') if raw.get(end + 1) == Some(&b'}') => 
            (&raw[end+2..], text(offset, end + 1), false),

        Some(&b'{') | Some(&b'}') =>
            (&raw[end..], text(offset, end), false),

        Some(&b'\n') => (&raw[end+1..], text(offset, end), true),

        _ => unreachable!(),
    }
}

//
//  Tests
//
#[cfg(test)]
mod tests {
    use basic::{com, mem};
    use model::tt::{Kind, StringFragment, Token};
    use super::super::raw::{RawKind, RawToken};

    #[test]
    fn simple() {
        assert_eq!(
            lexit(b"'Hello, World!'", 0),
            (
                single(0),
                vec![text(1, 13)],
                single(14),
            )
        );
    }

    #[test]
    fn escaped_braces() {
        assert_eq!(
            lexit(b"'Hello {{name, World!'", 0),
            (
                single(0),
                vec![
                    text(1, 7),
                    text(9, 12)
                ],
                single(21),
            )
        );

        assert_eq!(
            lexit(b"'Hello name}}, World!'", 0),
            (
                single(0),
                vec![
                    text(1, 11),
                    text(13, 8)
                ],
                single(21),
            )
        );

        assert_eq!(
            lexit(b"'Hello {{name}}, World!'", 0),
            (
                single(0),
                vec![
                    text(1, 7),
                    text(9, 5),
                    text(15, 8)
                ],
                single(23),
            )
        );

        assert_eq!(
            lexit(b"'Hello {name:}}}, World!'", 0),
            (
                single(0),
                vec![
                    text(1, 6),
                    inter_format(Kind::StringIdentifier, 7, 4, 2),
                    text(16, 8)
                ],
                single(24),
            )
        );
    }

    #[test]
    fn misescaped_braces() {
        assert_eq!(
            lexit(b"'Hello {name, World!'", 0),
            (
                single(0),
                vec![
                    text(1, 6),
                    unexpected(7, 13)
                ],
                single(20),
            )
        );

        assert_eq!(
            lexit(b"'Hello {name}}, World!'", 0),
            (
                single(0),
                vec![
                    text(1, 6),
                    unexpected(7, 15)
                ],
                single(22),
            )
        );

        assert_eq!(
            lexit(b"'Hello {{name}, World!'", 0),
            (
                single(0),
                vec![
                    text(1, 7),
                    text(9, 4),
                    unexpected(13, 1),
                    text(14, 8)
                ],
                single(22),
            )
        );
    }

    #[test]
    fn special_character() {
        assert_eq!(
            lexit(b"'Hello, World!{N}'", 0),
            (
                single(0),
                vec![
                    text(1, 13),
                    character(14, 3)
                ],
                single(17),
            )
        );

        assert_eq!(
            lexit(b"'Hello, World!{LF}'", 0),
            (
                single(0),
                vec![
                    text(1, 13),
                    character(14, 4)
                ],
                single(18),
            )
        );

        assert_eq!(
            lexit(b"'Hello, World!{CR}{U+0A}'", 0),
            (
                single(0),
                vec![
                    text(1, 13),
                    character(14, 4),
                    character(18, 6)
                ],
                single(24),
            )
        );
    }

    #[test]
    fn interpolated_noformat() {
        assert_eq!(
            lexit(b"'Hello, World!{}'", 0),
            (
                single(0),
                vec![
                    text(1, 13),
                    inter_noformat(Kind::StringPositionalArgument, 14, 2)
                ],
                single(16),
            )
        );
        assert_eq!(
            lexit(b"'Hello, World!{12}'", 0),
            (
                single(0),
                vec![
                    text(1, 13),
                    inter_noformat(Kind::StringIndexedArgument, 14, 4)
                ],
                single(18),
            )
        );
        assert_eq!(
            lexit(b"'Hello, World!{blurb}'", 0),
            (
                single(0),
                vec![
                    text(1, 13),
                    inter_noformat(Kind::StringIdentifier, 14, 7)
                ],
                single(21),
            )
        );
        assert_eq!(
            lexit(b"'Hello, World!{.blurb}'", 0),
            (
                single(0),
                vec![
                    text(1, 13),
                    inter_noformat(Kind::StringNamedArgument, 14, 8)
                ],
                single(22),
            )
        );
    }

    #[test]
    fn interpolated_format() {
        assert_eq!(
            lexit(b"'Hello, World!{:format}'", 0),
            (
                single(0),
                vec![
                    text(1, 13),
                    inter_format(Kind::StringPositionalArgument, 14, 0, 6)
                ],
                single(23),
            )
        );

        assert_eq!(
            lexit(b"'Hello, World!{9:format}'", 0),
            (
                single(0),
                vec![
                    text(1, 13),
                    inter_format(Kind::StringIndexedArgument, 14, 1, 6)
                ],
                single(24),
            )
        );

        assert_eq!(
            lexit(b"'Hello, World!{blurb:format}'", 0),
            (
                single(0),
                vec![
                    text(1, 13),
                    inter_format(Kind::StringIdentifier, 14, 5, 6)
                ],
                single(28),
            )
        );

        assert_eq!(
            lexit(b"'Hello, World!{.blurb:format}'", 0),
            (
                single(0),
                vec![
                    text(1, 13),
                    inter_format(Kind::StringNamedArgument, 14, 6, 6)
                ],
                single(29),
            )
        );
    }

    fn single(offset: usize) -> Token {
        Token::new(Kind::QuoteSingle, offset, 1)
    }

    fn text(offset: usize, length: usize) -> StringFragment {
        StringFragment::Text(
            Token::new(Kind::StringText, offset, length)
        )
    }

    fn character(offset: usize, length: usize) -> StringFragment {
        StringFragment::SpecialCharacter(
            //  Note: the token itself does not account for the braces {}.
            Token::new(Kind::StringSpecialCharacter, offset + 1, length - 2)
        )
    }

    fn inter_noformat(kind: Kind, offset: usize, len: usize) -> StringFragment {
        StringFragment::Interpolated(
            //  Note: the token itself does not account for the braces {}.
            Token::new(kind, offset + 1, len - 2),
            Token::new(Kind::Colon, offset + len - 1, 0),
            Token::new(Kind::StringFormat, offset + len - 1, 0),
        )
    }

    fn inter_format(kind: Kind, offset: usize, len_id: usize, len_format: usize)
        -> StringFragment
    {
        StringFragment::Interpolated(
            //  Note: the token itself does not account for the braces {}.
            Token::new(kind, offset + 1, len_id),
            Token::new(Kind::Colon, offset + 1 + len_id, 1),
            Token::new(Kind::StringFormat, offset + 2 + len_id, len_format),
        )
    }

    fn unexpected(offset: usize, len: usize) -> StringFragment {
        StringFragment::Unexpected(com::Range::new(offset, len))
    }

    fn lexit(raw: &[u8], indent: usize) ->
        (Token, Vec<StringFragment>, Token)
    {
        use super::parse;

        let global_arena = mem::Arena::new();
        let mut local_arena = mem::Arena::new();

        let is_bytes = raw[0] == b'b';
        let with_new_lines = raw.iter().position(|&b| b == b'\n').is_some();

        let kind = match (is_bytes, with_new_lines) {
            (true , false) => RawKind::Bytes,
            (true , true ) => RawKind::BytesMultiLines,
            (false, false) => RawKind::String,
            (false, true ) => RawKind::StringMultiLines,
        };

        let tok = RawToken::new(kind, raw, indent, 0, indent, indent);

        let (start, fragments, end) =
            parse(tok, 0, with_new_lines, &global_arena, &local_arena);

        local_arena.recycle();

        (start, fragments.iter().cloned().collect(), end)
    }
}
