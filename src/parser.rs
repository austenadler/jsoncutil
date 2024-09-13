// use anyhow::Result;
use std::collections::VecDeque;
use std::fmt::Debug;
use std::io::BufRead;
use std::io::ErrorKind;
use std::io::Write;

use derivative::Derivative;

use crate::indentor::Indentor;

const INDENT: &[u8] = b"  ";
const RECORD_SEPARATOR: &[u8] = b"\n";
const NEWLINE: &[u8] = b"\n";

const C_CR: u8 = b'\r';
const C_LF: u8 = b'\n';
const C_TAB: u8 = b'\t';
const C_SPACE: u8 = b' ';

const C_COMMA: u8 = b',';
const C_COLON: u8 = b':';
const C_QUOTE: u8 = b'"';
const C_BACKSLASH: u8 = b'\\';

const C_LEFT_BRACE: u8 = b'{';
const C_LEFT_BRACKET: u8 = b'[';
const C_RIGHT_BRACE: u8 = b'}';
const C_RIGHT_BRACKET: u8 = b']';

const C_SLASH: u8 = b'/';
const C_STAR: u8 = b'*';

const C_PLUS: u8 = b'+';
const C_DOT: u8 = b'.';
const C_MINUS: u8 = b'-';
const C_E: u8 = b'-';
const C_F: u8 = b'f';
const C_T: u8 = b't';
const C_N: u8 = b'n';

/// Write some bytes to the writer
macro_rules! w {
    ($dst: expr, $buf:expr) => {{
        let buf = $buf;
        let x: &[u8] = buf.as_ref();
        // eprintln!(
        //     "### \x1b[93mWriting\x1b[0m {:?}",
        //     ::std::string::String::from_utf8_lossy(&x)
        // );
        $dst.write_all(x)?;
    }};
}

/// Mode of operation of ouptut of the parser
#[derive(Debug, PartialEq, Eq)]
pub enum Mode {
    /// Add trailing commas, and do not strip comments
    Jsoncc,
    /// Strip comments, and add whitespace and newlines
    Json,
    /// Strip comments, and strip all optional whitespace
    CompactJson,
}

impl Mode {
    /// Check if the mode wants to keep comments or strip them
    fn keep_comments(&self) -> bool {
        match self {
            Mode::Jsoncc => true,
            Mode::Json | Mode::CompactJson => false,
        }
    }
}

impl Default for Mode {
    fn default() -> Self {
        Self::Jsoncc
    }
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    /// The input buffer is empty, but we need a token
    #[error("Buffer unexpectedly empty")]
    BufferEmpty,
    /// Bytes that look like a value (true, false, null, a number, or a string) was found in the wrong position
    #[error("Unexpected value type")]
    UnexpectedValue,
    /// A byte was found in an unexpected position
    #[error("Unexpected char {0:?}")]
    UnexpectedChar(char),
    /// A collection end token was found in an unexpected position
    #[error("Unexpected collection ending")]
    UnexpectedCollectionEnd,
    /// An IO error occured when reading or writing
    #[error("IO Error: {0}")]
    Io(#[from] std::io::Error),
}

impl Error {
    pub fn is_eof(&self) -> bool {
        matches!(self, Self::Io(e) if e.kind() == ErrorKind::UnexpectedEof)
    }
}

/// A token found in the input stream
///
/// This does not track `:` or `,` for two reasons:
///
/// 1. All input is jsoncc, which has optional `,`. `,` provides no extra information as the next token would need to be checked to decide if the current value is the last value
/// 1. `:` state is derived by the [`CollectionState::Object`] `awaiting_key` field
#[derive(Derivative, Clone, Copy, PartialEq, Eq)]
#[derivative(Debug)]
pub enum Token {
    /// We have reached an EOF at a position that is not in a value
    Eof,
    /// The root of the input
    ///
    /// Note that there can be multiple root tokens. For example, for input `{}{}`, root tokens are sent at these positions: `^{}^{}`
    Root,
    /// The start of an object or array
    CollectionStart { ty: CollectionType },
    /// The end of an object or array
    CollectionEnd { ty: CollectionType },
    /// A block or line comment
    Comment {
        ty: CommentType,
        /// Should this comment be on its own line?
        ///
        /// This is derived from the input.
        /// If the comment is read on a line with only whitespace tokens, this is set to true
        own_line: bool,
    },
    /// A value that is not a collection
    Value {
        ty: ValueType,
        #[derivative(Debug(format_with = "fmt_u8_as_char"))]
        first_char: u8,
    },
}

fn fmt_u8_as_char(c: &u8, f: &mut std::fmt::Formatter) -> std::result::Result<(), std::fmt::Error> {
    Debug::fmt(&(*c as char), f)?;
    Ok(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CollectionType {
    Object,
    Array,
}

#[derive(Debug, Clone, Copy)]
enum CollectionState {
    Object { awaiting_key: bool },
    Array,
}

impl CollectionState {
    fn ty(&self) -> CollectionType {
        match self {
            Self::Object { awaiting_key: _ } => CollectionType::Object,
            Self::Array => CollectionType::Array,
        }
    }
}

impl CollectionType {
    fn as_state(&self) -> CollectionState {
        match self {
            Self::Object => CollectionState::Object { awaiting_key: true },
            Self::Array => CollectionState::Array,
        }
    }

    fn start_str(&self) -> &'static str {
        match self {
            Self::Object => "{",
            Self::Array => "[",
        }
    }

    fn end_str(&self) -> &'static str {
        match self {
            Self::Object => "}",
            Self::Array => "]",
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum CommentType {
    Line,
    Block,
}

impl CommentType {
    fn start_str(&self) -> &'static str {
        match self {
            Self::Line => "//",
            Self::Block => "/*",
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ValueType {
    // TODO: Do we want to allow the possibility of unquoted object keys?
    // Unquoted values would be a bad idea. For example, there would be ambiguity for {x: true} (is it {"x": "true"} or {"x": true})
    // You could force `true`/`false`/`null`/numbers to be non-strings, but then you end up with the yaml `yes`/`no` problem
    // Also, if someone types a number like `-1.4e4.`, we don't want that converted to a string, we should keep it as an (invalid) number
    // UnquotedString,
    String,
    Number,
    Boolean,
    Null,
}

#[derive(Derivative)]
#[derivative(Debug)]
pub struct Parser<R, W>
where
    R: BufRead,
    W: Write,
{
    /// Input reader
    #[derivative(Debug = "ignore")]
    input: R,
    /// Output writer
    #[derivative(Debug = "ignore")]
    write: W,
    /// Stack tracking the state of the parser
    ///
    /// When descending into an array or object, push a [`CollectionState`] here
    state_stack: VecDeque<CollectionState>,
    /// The current token the parser has received
    current_token: Token,
    // TODO: This can be used to add whitespace (if [`num_empty_lines`] > 1)
    /// The number of empty lines read from [`input`] in a row
    ///
    /// Empty lines are lines that only contain whitespace
    num_empty_lines: u8,
    /// The mode of operation of the parser
    mode: Mode,
    /// Buffered indent strings so repeated calls do not have to repeated call [`std::io::repeat`]
    #[derivative(Debug = "ignore")]
    indentor: Indentor,
}

impl<R, W> Parser<R, W>
where
    R: BufRead,
    W: Write,
{
    pub fn new(mode: Mode, read: R, write: W) -> Self {
        Self {
            input: read,
            write,
            state_stack: VecDeque::new(),
            current_token: Token::Root,
            num_empty_lines: 0,
            mode,
            indentor: Indentor::new(INDENT),
        }
    }

    /// Send the rest of the input to the writer until the end of the comment is reached
    fn drain_comment(&mut self, ty: &CommentType) -> Result<()> {
        let mut maybe_block_end = false;

        loop {
            let buf = self.input.fill_buf()?;

            if buf.is_empty() {
                return Err(Error::BufferEmpty);
            }

            match ty {
                CommentType::Line => match line_comment_end(buf) {
                    Some(idx) => {
                        if self.mode.keep_comments() {
                            w!(self.write, &buf[0..idx]);
                        }

                        self.input.consume(idx);
                        break;
                    }
                    None => {
                        if self.mode.keep_comments() {
                            w!(self.write, buf);
                        }
                        let len = buf.len();
                        self.input.consume(len);
                    }
                },
                CommentType::Block => {
                    if maybe_block_end && buf[0] == b'/' {
                        // We ended the block comment
                        if self.mode.keep_comments() {
                            w!(self.write, b"/");
                        }
                        break;
                    }

                    maybe_block_end = false;

                    match block_comment_end(buf) {
                        BlockCommentEnd::Position(idx) => {
                            if self.mode.keep_comments() {
                                w!(self.write, &buf[0..idx]);
                            }

                            self.input.consume(idx);
                            break;
                        }
                        BlockCommentEnd::MaybeEnd => {
                            if self.mode.keep_comments() {
                                w!(self.write, buf);
                            }
                            let len = buf.len();
                            self.input.consume(len);
                            maybe_block_end = true;
                        }
                        BlockCommentEnd::None => {
                            if self.mode.keep_comments() {
                                w!(self.write, buf);
                            }
                            let len = buf.len();
                            self.input.consume(len);
                        }
                    }
                }
            }
        }

        Ok(())
    }

    /// Send the rest of the input to the writer until the end of the value is reached
    fn drain_value(&mut self, ty: &ValueType, first_char: u8) -> Result<()> {
        match (ty, first_char) {
            (ValueType::String, C_QUOTE) => {
                let mut next_char_escaped = false;
                w!(self.write, [C_QUOTE]);

                // Loop until we are done with the string
                loop {
                    if next_char_escaped {
                        // The previous buffer ended in `\`
                        // Send this character out
                        let next_char = self.next_char()?;
                        w!(self.write, [next_char]);
                    }
                    next_char_escaped = false;

                    let buf = self.input.fill_buf()?;

                    if buf.is_empty() {
                        return Err(Error::BufferEmpty);
                    }

                    match string_end(buf) {
                        StringEnd::Position(idx) => {
                            w!(self.write, &buf[0..idx]);
                            self.input.consume(idx);
                            break;
                        }
                        StringEnd::MaybeEnd => {
                            w!(self.write, buf);
                            let len = buf.len();
                            self.input.consume(len);
                            next_char_escaped = true;
                        }
                        StringEnd::None => {
                            w!(self.write, buf);
                            let len = buf.len();
                            self.input.consume(len);
                        }
                    }
                }

                let next_char = self.next_char()?;
                w!(self.write, [next_char]);

                Ok(())
            }
            (ValueType::Number, mut c) => {
                loop {
                    w!(self.write, [c]);
                    c = self.peek_next_char()?;
                    // Any of the json numerical characters
                    if c == C_PLUS
                        || c == C_MINUS
                        || c == C_DOT
                        || (c as char).is_ascii_digit()
                        || c == C_E
                    {
                        self.next_char()?;
                    } else {
                        break;
                    }
                }
                Ok(())
            }
            (ValueType::Boolean, C_T) => {
                let mut chr = [0_u8; 3];
                self.input.read_exact(&mut chr)?;

                if chr == *b"rue" {
                    w!(self.write, "true");
                    Ok(())
                } else {
                    Err(Error::UnexpectedValue)
                }
            }
            (ValueType::Boolean, C_F) => {
                let mut chr = [0_u8; 4];
                self.input.read_exact(&mut chr)?;

                if chr == *b"alse" {
                    w!(self.write, "false");
                    Ok(())
                } else {
                    Err(Error::UnexpectedValue)
                }
            }
            (ValueType::Null, C_N) => {
                let mut chr = [0_u8; 3];
                self.input.read_exact(&mut chr)?;

                if chr == *b"ull" {
                    w!(self.write, "null");
                    Ok(())
                } else {
                    Err(Error::UnexpectedValue)
                }
            }
            _ => {
                eprintln!("Value type: {ty:?}, with first char {first_char:?}");
                Err(Error::UnexpectedValue)
            }
        }
    }

    // /// Write some bytes to the writer
    // fn write(&mut self, buf: impl AsRef<[u8]>) -> Result<()> {
    //     w!(&mut self.write, buf);
    //     eprintln!("### Writing {:?}", String::from_utf8_lossy(buf.as_ref()));
    //     self.write.write_all(buf.as_ref())?;
    //     Ok(())
    // }

    /// Write the record separator to the writer
    fn record_separator(&mut self) -> Result<()> {
        w!(self.write, RECORD_SEPARATOR);

        Ok(())
    }

    /// Add extra padding after `:` or before the `//`/`/*` in a comment, if the format requests it
    fn extra_spacing(&mut self) -> Result<()> {
        match self.mode {
            Mode::Jsoncc | Mode::Json => w!(self.write, " "),
            Mode::CompactJson => {}
        }

        Ok(())
    }

    /// Add a comma only if we are not at the root level
    fn comma(&mut self) -> Result<()> {
        // We don't want a comma if this is a root element
        if self.state_stack.is_empty() {
            return Ok(());
        }

        w!(self.write, ",");

        Ok(())
    }

    /// Add a trailing comma only if we are not at the root level and we are in [`Mode::Jsoncc`]
    fn trailing_comma(&mut self) -> Result<()> {
        match self.mode {
            Mode::Jsoncc => self.comma()?,
            Mode::Json | Mode::CompactJson => {}
        }

        Ok(())
    }

    /// Write a newline and add indentation
    fn newline(&mut self) -> Result<()> {
        match self.mode {
            Mode::Jsoncc | Mode::Json => {
                w!(self.write, NEWLINE);
                self.write
                    .write_all(self.indentor.get_indent(self.state_stack.len()))?;
            }
            Mode::CompactJson => {}
        }

        Ok(())
    }

    /// Leave a collection
    ///
    /// Call this after you see a `]` or `}` token, and you want the parser to ensure that we were in the right kind of collection before leaving it
    fn exit_collection(&mut self, ty: &CollectionType) -> Result<()> {
        if Some(*ty)
            != self
                .state_stack
                .pop_back()
                .as_ref()
                .map(CollectionState::ty)
        {
            return Err(Error::UnexpectedCollectionEnd);
        }

        Ok(())
    }

    /// Format the reader into the writer and consume the [`Parser`] by reading tokens and sending formatted output
    ///
    /// Generally, the writer state ends with each token written with the ending `:` as required by the next token
    /// A `,` is decided if the `current_token` is a value and the next token is something that warrants a `,` (either another value, a collection, or a collection end in jsoncc mode)
    ///
    /// For example:
    /// ```text
    /// ["a", "b"]
    ///       ^
    /// ```
    ///
    /// At this position, `self::current_token` is a `Value` (representing `"a"`) and `next_token` represents `"b"`, so we know a `,` has been written
    /// In Jsoncc/Json mode, write a newline, indent, and flush the `"b"` Value
    pub fn format_buf(mut self) -> Result<()> {
        loop {
            // eprintln!("========================================================");
            // eprintln!("{:#?}", self);

            let mut next_token = self.get_next_token()?;

            // eprintln!("{:#?}\n{:#?}", self.current_token, next_token);
            // eprintln!();

            match (self.current_token, &next_token) {
                // root -> [/{
                (Token::Root, Token::CollectionStart { ty }) => {
                    self.state_stack.push_back(ty.as_state());
                    w!(self.write, ty.start_str());
                }
                // root -> //
                (Token::Root, Token::Comment { ty, own_line: _ }) => {
                    w!(self.write, ty.start_str());
                    self.drain_comment(ty)?;
                }
                // root -> ""
                (Token::Root, Token::Value { ty, first_char }) => {
                    self.drain_value(ty, *first_char)?;
                    next_token = Token::Root;
                }
                // {/[ -> [/{
                (Token::CollectionStart { ty: _ }, Token::CollectionStart { ty }) => {
                    self.newline()?;
                    w!(self.write, ty.start_str());
                    self.set_awaiting_key(true)?;
                    self.state_stack.push_back(ty.as_state());
                }
                // {/[ -> ]/}
                (Token::CollectionStart { ty: _ }, Token::CollectionEnd { ty }) => {
                    self.exit_collection(ty)?;
                    w!(self.write, ty.end_str());
                }
                // {/[ -> //
                (Token::CollectionStart { ty: _ }, Token::Comment { ty, own_line: _ }) => {
                    // Force own_line to be true
                    self.newline()?;
                    w!(self.write, ty.start_str());
                    self.drain_comment(ty)?;
                }
                // {/[ -> ""
                (Token::CollectionStart { ty: _ }, Token::Value { ty, first_char }) => {
                    self.newline()?;
                    self.drain_value(ty, *first_char)?;
                    if self.is_awaiting_key()? {
                        // We are in an object and we were awaiting a key
                        // We got it, so write :
                        w!(self.write, ":");
                    }
                    self.toggle_awaiting_key()?;
                }
                // }/] -> [/{
                // This can't occur if the outer collection is an object (`{{}: []}` is not valid json)
                // Therefore, we don't need to worry
                (Token::CollectionEnd { ty: _ }, Token::CollectionStart { ty }) => {
                    self.comma()?;
                    self.newline()?;
                    w!(self.write, ty.start_str());
                    self.state_stack.push_back(ty.as_state());
                }
                // }/] -> ]/}
                (Token::CollectionEnd { ty: _ }, Token::CollectionEnd { ty }) => {
                    self.trailing_comma()?;
                    self.exit_collection(ty)?;
                    self.newline()?;
                    w!(self.write, ty.end_str());
                }
                // }/] -> //
                (Token::CollectionEnd { ty: _ }, Token::Comment { ty, own_line: _ }) => {
                    // Force own_line to be true
                    self.trailing_comma()?;
                    self.newline()?;
                    w!(self.write, ty.start_str());
                    self.drain_comment(ty)?;
                }
                // }/] -> ""
                (Token::CollectionEnd { ty: _ }, Token::Value { ty, first_char }) => {
                    if self.is_after_value()? {
                        self.comma()?;
                    }
                    self.newline()?;
                    self.drain_value(ty, *first_char)?;
                    if self.is_awaiting_key()? {
                        w!(self.write, ":");
                        // self.set_awaiting_key(false)?;
                    }
                    self.toggle_awaiting_key()?;
                }
                // // -> [/{
                (Token::Comment { ty: _, own_line: _ }, Token::CollectionStart { ty }) => {
                    // TODO: The newline should be conditional
                    // {"a":/**/[]} should not have a newline before the `[`
                    self.newline()?;
                    w!(self.write, ty.start_str());
                    self.state_stack.push_back(ty.as_state());
                }
                // // -> ]/}
                (Token::Comment { ty: _, own_line: _ }, Token::CollectionEnd { ty }) => {
                    self.exit_collection(ty)?;
                    self.newline()?;
                    w!(self.write, ty.end_str());
                    // self.trailing_comma()?;
                }
                // // -> //
                (Token::Comment { ty: _, own_line: _ }, Token::Comment { ty, own_line: _ }) => {
                    // Force own_line to be true
                    self.newline()?;
                    w!(self.write, ty.start_str());
                    self.drain_comment(ty)?;
                }
                // // -> ""
                (Token::Comment { ty: _, own_line: _ }, Token::Value { ty, first_char }) => {
                    // TODO: The newline should be conditional
                    // {"a":/**/"b"} should not have a newline
                    self.newline()?;
                    self.drain_value(ty, *first_char)?;

                    if self.is_awaiting_key()? {
                        w!(self.write, ":");
                        // self.set_awaiting_key(false)?;
                    }
                    self.toggle_awaiting_key()?;
                }
                // "" -> [/{
                (
                    Token::Value {
                        ty: _,
                        first_char: _,
                    },
                    Token::CollectionStart { ty },
                ) => {
                    if self.is_after_value()? {
                        // We are in an arry
                        self.comma()?;
                        self.newline()?;
                    } else if !self.is_awaiting_key()? {
                        // We are in an object
                        self.extra_spacing()?;
                    }

                    w!(self.write, ty.start_str());
                    self.toggle_awaiting_key()?;
                    self.state_stack.push_back(ty.as_state());
                }
                // "" -> ]/}
                (
                    Token::Value {
                        ty: _,
                        first_char: _,
                    },
                    Token::CollectionEnd { ty },
                ) => {
                    self.trailing_comma()?;
                    self.exit_collection(ty)?;
                    self.newline()?;
                    w!(self.write, ty.end_str());
                }
                // "" -> //
                (
                    Token::Value {
                        ty: _,
                        first_char: _,
                    },
                    Token::Comment { ty, own_line },
                ) => {
                    if self.is_after_value()? {
                        self.comma()?;
                    }

                    if *own_line {
                        self.newline()?;
                    } else {
                        self.extra_spacing()?;
                    }
                    w!(self.write, ty.start_str());
                    self.drain_comment(ty)?;
                }
                // "" -> ""
                (
                    Token::Value {
                        ty: _,
                        first_char: _,
                    },
                    Token::Value { ty, first_char },
                ) => {
                    if self.is_after_value()? {
                        self.comma()?;
                        self.newline()?;
                    } else if !self.is_awaiting_key()? {
                        // The previous value was an object key, so put a space after the `:`
                        self.extra_spacing()?;
                    }
                    self.drain_value(ty, *first_char)?;
                    if self.is_awaiting_key()? {
                        w!(self.write, ":");
                    }
                    self.toggle_awaiting_key()?;
                }

                // root -> eof
                (Token::Root, Token::Eof) if self.state_stack.is_empty() => {
                    // We read the whole file successfully!
                    return Ok(());
                }

                (a, b) => {
                    panic!("Invalid state transition: {a:?} => {b:?}")
                }
            }

            if (matches!(next_token, Token::CollectionEnd { .. }) || next_token == Token::Root)
                && self.state_stack.is_empty()
            {
                self.record_separator()?;
                next_token = Token::Root;
            }

            self.current_token = next_token;
        }
    }

    /// Search for a token while in [`ParserMode::Normal`]
    fn get_next_token(&mut self) -> Result<Token> {
        let ret = loop {
            let chr = self.next_char();

            if Err(true) == chr.as_ref().map_err(Error::is_eof) {
                // TODO: If our nested depth is 0, this is just a Root token??
                break Ok(Token::Eof);
            }
            let chr = chr?;

            // eprintln!("Got next char: {:?}", chr as char);

            break Ok(match chr {
                C_CR | C_LF => {
                    self.num_empty_lines = self.num_empty_lines.saturating_add(1);
                    continue;
                }
                C_TAB | C_SPACE => continue,
                // C_COMMA => Token::Comma,
                C_COLON => continue,
                // TODO: Allow unquoted strings?
                C_QUOTE => Token::Value {
                    ty: ValueType::String,
                    first_char: b'"',
                },
                // C_BACKSLASH => {}
                C_LEFT_BRACE => Token::CollectionStart {
                    ty: CollectionType::Object,
                },
                C_LEFT_BRACKET => Token::CollectionStart {
                    ty: CollectionType::Array,
                },
                C_RIGHT_BRACE => Token::CollectionEnd {
                    ty: CollectionType::Object,
                },
                C_RIGHT_BRACKET => Token::CollectionEnd {
                    ty: CollectionType::Array,
                },
                C_SLASH => {
                    // We can't send comment tokens if using json
                    let maybe_next_token_ty = match self.next_char()? {
                        C_SLASH => CommentType::Line,

                        C_STAR => CommentType::Block,

                        c => {
                            eprintln!("{:#?}", self);
                            eprintln!("X {:?}", (c as char));
                            break Err(Error::UnexpectedChar(c as char));
                        }
                    };

                    if self.mode.keep_comments() {
                        Token::Comment {
                            ty: maybe_next_token_ty,
                            own_line: self.num_empty_lines > 0,
                        }
                    } else {
                        // We need to drain this comment by reading the buffer
                        // This function won't write anything in json modes
                        self.drain_comment(&maybe_next_token_ty)?;

                        self.num_empty_lines = 0;
                        continue;
                    }
                }
                C_COMMA => continue,

                c @ C_T | c @ C_F => Token::Value {
                    ty: ValueType::Boolean,
                    first_char: c,
                },
                c @ b'n' => Token::Value {
                    ty: ValueType::Null,
                    first_char: c,
                },

                c @ C_PLUS | c @ C_MINUS | c if (c as char).is_ascii_digit() => Token::Value {
                    ty: ValueType::Number,
                    first_char: c,
                },

                c => {
                    eprintln!("Unexpected char?? {self:#?}");
                    break Err(Error::UnexpectedChar(c as char));
                }
            });
        };

        self.num_empty_lines = 0;
        ret
    }

    /// Check the next char without consuming it
    fn peek_next_char(&mut self) -> Result<u8> {
        self.input
            .fill_buf()?
            .first()
            .ok_or(Error::BufferEmpty)
            .copied()
    }

    /// Consume the next character from the reader
    fn next_char(&mut self) -> Result<u8> {
        let mut chr = [0_u8];
        self.input.read_exact(&mut chr)?;
        Ok(chr[0])
    }

    /// Returns `true` if we are in an object and the next value is actually an object key
    fn is_awaiting_key(&self) -> Result<bool> {
        Ok(
            match self.state_stack.back().ok_or(Error::UnexpectedValue)? {
                CollectionState::Object { awaiting_key } => *awaiting_key,
                CollectionState::Array => false,
            },
        )
    }

    fn is_after_value(&self) -> Result<bool> {
        Ok(
            match self.state_stack.back().ok_or(Error::UnexpectedValue)? {
                CollectionState::Object { awaiting_key } => *awaiting_key,
                CollectionState::Array => true,
            },
        )
    }

    /// Toggles the `awaiting_key` value. Called after reading a value
    ///
    /// Has no affect if the current collection is an array, so this is safe to call after reading any value or CollectionEnd token
    fn toggle_awaiting_key(&mut self) -> Result<()> {
        match self.state_stack.back_mut().ok_or(Error::UnexpectedValue)? {
            CollectionState::Object { awaiting_key } => *awaiting_key = !*awaiting_key,
            CollectionState::Array => {}
        }

        Ok(())
    }

    fn set_awaiting_key(&mut self, value: bool) -> Result<()> {
        match self.state_stack.back_mut().ok_or(Error::UnexpectedValue)? {
            CollectionState::Object { awaiting_key } => *awaiting_key = value,
            CollectionState::Array => {}
        }

        Ok(())
    }
}

/// Gets the position in a buf that a block comment ends
/// ```text
/// /* abc */ def
///          ^
/// ```
fn block_comment_end(buf: &[u8]) -> BlockCommentEnd {
    for star_idx in memchr::memchr_iter(C_STAR, buf) {
        match buf.get(star_idx + 1) {
            Some(&C_SLASH) => {
                // We found `*/` at position `star_idx`
                return BlockCommentEnd::Position(star_idx + 2);
            }
            Some(_) => {}
            None => {
                // We found `*` at the end of the buffer
                return BlockCommentEnd::MaybeEnd;
            }
        }
    }
    BlockCommentEnd::None
}

/// Gets the position in a buf that the string ends
/// ```text
/// xyzabc": 123,
///        ^
/// ```
/// Note that the `xyzabc` is part of a string, but the start of the string must have come from a previous buffer
fn string_end(buf: &[u8]) -> StringEnd {
    let mut n = 0;

    loop {
        match memchr::memchr2(C_QUOTE, C_BACKSLASH, &buf[n..])
            .and_then(|idx| Some((idx, buf.get(idx + n)?)))
        {
            Some((idx, &C_QUOTE)) => {
                n += idx;
                return StringEnd::Position(n);
            }
            Some((idx, &C_BACKSLASH)) => {
                n += idx;

                // We found a `\` at the end of the buf
                if buf.len() == n + 1 {
                    // The `/` is at the end of `buf`
                    return StringEnd::MaybeEnd;
                } else {
                    // The end of the string won't be the `\` and the next byte
                    n += 2;
                }
            }
            Some((idx, chr)) => {
                eprintln!("Buf: {:?}", String::from_utf8(buf.to_vec()));
                panic!(
                    "memchr2 returned unexpected result ({} @ {})",
                    *chr as char,
                    idx + n
                );
            }
            None => {
                // There are no `"` in the string, so we know the rest of the buf is just part of the string
                return StringEnd::None;
            }
        }
    }
}

/// Gets the position in a buf that a line comment ends
fn line_comment_end(buf: &[u8]) -> Option<usize> {
    memchr::memchr2(C_CR, C_LF, buf)
}

/// A case that a buf ends in a block comment ending `*/`
enum BlockCommentEnd {
    /// The block comment ended at this position
    Position(usize),
    /// The buffer did not have any `*/`, but it ended in a `*`
    MaybeEnd,
    /// The block comment does not end in this buf
    None,
}

/// A case that a buf ends in a string ending `"` that was not escaped by `\`
#[derive(PartialEq, Eq, Debug)]
enum StringEnd {
    /// The string ended at this position
    Position(usize),
    /// The buffer did not have any unescaped `"`, but it ended in a `*`
    MaybeEnd,
    /// The string does not end in this buf
    None,
}

#[cfg(test)]
mod tests {
    use std::io::{BufReader, BufWriter};

    use super::*;

    fn format_to_string(input: &[u8], mode: Mode) -> String {
        let mut output = vec![];
        Parser::new(
            mode,
            BufReader::new(input),
            &mut BufWriter::new(&mut output),
        )
        .format_buf()
        .unwrap();
        String::from_utf8(output).unwrap()
    }

    #[test]
    fn test_string_end() {
        assert_eq!(string_end(br#"ABC"#), StringEnd::None);
        assert_eq!(string_end(br#"ABC\"#), StringEnd::MaybeEnd);
        assert_eq!(string_end(br#"ABC""#), StringEnd::Position(3));
    }

    #[test]
    fn test_formatting() {
        let x = r#"[]
{}
[]
{
    "a": "b"
}
{"a":"b"}
{
    "a": "b",
}
{"a":"b",}
[]
            "#;

        eprintln!("{}", format_to_string(x.as_bytes(), Mode::Jsoncc));
        assert_eq!(
            format_to_string(x.as_bytes(), Mode::Jsoncc),
            r#"[]
{}
[]
{
  "a": "b",
}
{
  "a": "b",
}
{
  "a": "b",
}
{
  "a": "b",
}
[]
"#
        );

        assert_eq!(
            format_to_string(x.as_bytes(), Mode::Json),
            r#"[]
{}
[]
{
  "a": "b"
}
{
  "a": "b"
}
{
  "a": "b"
}
{
  "a": "b"
}
[]
"#
        );

        assert_eq!(
            format_to_string(x.as_bytes(), Mode::CompactJson),
            r#"[]
{}
[]
{"a":"b"}
{"a":"b"}
{"a":"b"}
{"a":"b"}
[]
"#
        );
    }

    #[test]
    fn test_formatting_comments() {
        let x = r#"[]
{
  /*1*/
}
[
  /*2*/
]
{
  //
  "a": "b",
}
{
  //
  "a": "b",
}
{
  /*1*/
  "a": "b", /*2*/
  /*3*/
  "c":"d",
  /*4*/ "e":"f"/*5*/, /*6*/
}
{/*w*/
  /*x*/
  "a"/*y*/:/*z*/"b",/*a*/
}
[]"#;

        eprintln!("{}", format_to_string(x.as_bytes(), Mode::Json));

        assert_eq!(
            format_to_string(x.as_bytes(), Mode::Jsoncc),
            r#"[]
{
  /*1*/
}
[
  /*2*/
]
{
  //
  "a": "b",
}
{
  //
  "a": "b",
}
{
  /*1*/
  "a": "b", /*2*/
  /*3*/
  "c": "d",
  /*4*/
  "e": "f", /*5*/
  /*6*/
}
{
  /*w*/
  /*x*/
  "a": /*y*/
  /*z*/
  "b", /*a*/
}
[]
"#
        );

        assert_eq!(
            format_to_string(x.as_bytes(), Mode::CompactJson),
            r#"[]
{}
[]
{"a":"b"}
{"a":"b"}
{"a":"b","c":"d","e":"f"}
{"a":"b"}
[]
"#
        );
        assert_eq!(
            format_to_string(x.as_bytes(), Mode::Json),
            r#"[]
{}
[]
{
  "a": "b"
}
{
  "a": "b"
}
{
  "a": "b",
  "c": "d",
  "e": "f"
}
{
  "a": "b"
}
[]
"#
        );
    }

    // static G: AtomicUsize = AtomicUsize::new(0);

    // fn fork(i: &str) -> Vec<String> {
    //     let a = i.replacen(
    //         "_",
    //         &format!("/*{}*/", G.fetch_add(1, Ordering::Relaxed)),
    //         1,
    //     );
    //     let b = i.replacen("_", "", 1);

    //     if a.contains("_") {
    //         let mut ret = fork(&a);
    //         ret.append(&mut fork(&b));
    //         ret
    //     } else {
    //         vec![a, b]
    //     }
    // }
}
