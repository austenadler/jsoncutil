use crate::CsvArgs;
use std::io::BufRead;
use std::io::Write;

use anyhow::{bail, Context, Result};

#[derive(PartialEq, Eq, Debug)]
enum ParserState {
    WaitingForRow { saw_cr: bool },
    InUnquotedField { start: bool },
    InQuotedField { maybe_ending: bool },
}

impl Default for ParserState {
    fn default() -> Self {
        Self::WaitingForRow { saw_cr: false }
    }
}

pub struct Parser {
    pub csv_args: CsvArgs,
}

impl Parser {
    pub fn new(csv_args: CsvArgs) -> Self {
        Self { csv_args }
    }

    pub fn parse_buf<R: BufRead, W: Write>(self, reader: R, writer: W) -> Result<()> {
        ParserInner {
            first_row: true,
            reader,
            writer,
            state: ParserState::WaitingForRow { saw_cr: false },
            args: self.csv_args,
        }
        .parse_buf()
    }
}

struct ParserInner<R: BufRead, W: Write> {
    first_row: bool,
    reader: R,
    writer: W,
    state: ParserState,
    args: CsvArgs,
}

impl<R: BufRead, W: Write> ParserInner<R, W> {
    fn parse_buf(mut self) -> Result<()> {
        if self.args.wrap {
            self.writer
                .write_all(b"[")
                .context("Writing initial byte to output")?
        }

        loop {
            let buf = self.reader.fill_buf()?;

            if buf.is_empty() {
                break;
            }

            let c = buf[0];
            //     eprintln!("[{:?} => {:?}]", self.state, c as char);
            match self.state {
                ParserState::WaitingForRow { saw_cr } => match c {
                    b'\n' | b'\r' => {
                        if saw_cr && c == b'\n' {
                            // We saw a CR, but now we see an LF, so the LF is ignored
                            self.reader.consume(1);
                        } else {
                            // This is empty, so start and then end the row
                            self.start_row()?;
                            // TODO: Does this csv file have one field or 0 fields?
                            // I think 1 because it *is* a line
                            self.start_field()?;
                            self.end_field()?;
                            self.end_row()?;
                            self.reader.consume(1);
                        }
                        // n += 1;
                        continue;
                    }
                    // a,b\n,
                    //      ^
                    c if c == self.args.separator => {
                        self.start_row()?;
                        // Output an empty string for this field, but also start the next field
                        self.start_field()?;
                        self.end_field()?;
                        self.start_field()?;
                        self.state = ParserState::InUnquotedField { start: true };
                    }
                    // a,b\n"a",
                    //      ^
                    c if c == self.args.quote_character => {
                        // We saw a quote, so we know there is data in this row
                        self.start_row()?;
                        // We saw a quote, so we know there will be data in this field
                        self.start_field()?;

                        self.state = ParserState::InQuotedField {
                            maybe_ending: false,
                        };
                    }
                    c => {
                        // We saw data, so this is the start of a row
                        self.start_row()?;
                        // We saw data, so we know this unquoted field was started
                        self.start_field()?;
                        self.write_escaped_json_char(c)?;

                        self.state = ParserState::InUnquotedField { start: false };
                    }
                },
                ParserState::InUnquotedField { start } => match c {
                    // ab,
                    //   ^
                    c if c == self.args.separator => {
                        self.end_field()?;
                        self.start_field()?;
                        self.state = ParserState::InUnquotedField { start: true };
                    }
                    // abc,"def",ghi
                    //     ^
                    c if c == self.args.quote_character && start => {
                        self.state = ParserState::InQuotedField {
                            maybe_ending: false,
                        }
                    }

                    // ab"c,
                    //   ^
                    c if c == self.args.quote_character && !start => {
                        self.write_escaped_json_char(c)?;
                    }

                    // abc,def\n
                    //        ^^
                    c @ b'\n' | c @ b'\r' => {
                        // We know we were in a field, so end it first
                        self.end_field()?;
                        // Now, end this row
                        self.end_row()?;

                        // Now we wait for the next row
                        self.state = ParserState::WaitingForRow { saw_cr: c == b'\r' };
                    }
                    // abc
                    //  ^
                    c => {
                        self.write_escaped_json_char(c)?;
                        self.state = ParserState::InUnquotedField { start: false };
                    }
                },
                ParserState::InQuotedField {
                    maybe_ending: false,
                } => {
                    if c == self.args.quote_character {
                        // "abc""def"
                        //     ^
                        self.state = ParserState::InQuotedField { maybe_ending: true };
                    } else {
                        // "abc""def"
                        //   ^
                        self.write_escaped_json_char(c)?;
                    }
                }
                ParserState::InQuotedField { maybe_ending: true } => match c {
                    // "abc""def"
                    //      ^
                    c if c == self.args.quote_character => {
                        // We got a second quote, so we aren't ending
                        self.write_escaped_json_char(c)?;
                        self.state = ParserState::InQuotedField {
                            maybe_ending: false,
                        };
                    }
                    // "abc""def",ghi
                    //           ^
                    c if c == self.args.separator => {
                        self.end_field()?;
                        self.start_field()?;
                        self.state = ParserState::InUnquotedField { start: true };
                    }
                    // "abc""def"\n
                    //           ^^
                    b'\n' | b'\r' => {
                        self.end_field()?;
                        self.end_row()?;
                        self.state = ParserState::WaitingForRow { saw_cr: c == b'\r' };
                    }
                    c => {
                        bail!("Unexpected character {:?} found after '\"'", c as char);
                    }
                },
            }
            self.reader.consume(1);
        }

        // We are done reading the input file
        // If we were in a field, end it if possible
        match self.state {
            ParserState::WaitingForRow { saw_cr: _ } => {}
            ParserState::InUnquotedField { start: _ } => {
                self.end_field()?;
                self.end_row()?;
            }
            ParserState::InQuotedField { maybe_ending: _ } => {
                bail!("Quoted field did not have closing quote")
            }
        }

        if self.args.wrap {
            self.writer
                .write_all(b"]")
                .context("Writing final byte to self.writer")?;
        }

        Ok(())
    }

    fn start_row(&mut self) -> Result<()> {
        self.write(b"[")
    }

    fn end_row(&mut self) -> Result<()> {
        let ret = self.write(b"]");
        // We ended at least 1 row, so we are not on the first row anymore
        // But do this after we try writing ]
        self.first_row = false;
        ret
    }

    fn start_field(&mut self) -> Result<()> {
        self.write(b"\"")
    }

    fn end_field(&mut self) -> Result<()> {
        self.write(b"\",")
    }

    fn write(&mut self, buf: &[u8]) -> Result<()> {
        if self.should_print() {
            self.writer.write_all(buf)?;
        }

        Ok(())
    }

    /// Checks if we should be writing to output right now
    fn should_print(&self) -> bool {
        // We do not want to write if we are on the first row, but we want to skip headers
        !(self.first_row && self.args.skip_header)
    }

    fn write_escaped_json_char(&mut self, c: u8) -> Result<()> {
        // eprintln!("Writing: {:?}", c as char);
        match c {
            b'"' => self.write(b"\\\""),
            b'\\' => self.write(b"\\\\"),
            b'/' => self.write(b"\\/"),
            0x08 => self.write(b"\\b"),
            0x0c => self.write(b"\\f"),
            b'\n' => self.write(b"\\n"),
            b'\r' => self.write(b"\\r"),
            b'\t' => self.write(b"\\t"),
            c => self.write(&[c]),
        }
    }
}
