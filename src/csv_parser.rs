use crate::CsvArgs;
use crate::IoArg;
use std::io::Write;

use anyhow::{bail, Context, Result};

#[derive(PartialEq, Eq, Debug)]
enum ParserState {
    WaitingForRow { saw_cr: bool },
    InUnquotedField { start: bool },
    InQuotedField { maybe_ending: bool },
}

pub fn csv_reader_to_json_writer(
    args: CsvArgs,
    input: IoArg,
    mut output: impl Write,
) -> Result<()> {
    let mut input = input
        .as_output()
        .input_to_reader()
        .context("Converting input file to a reader")?;

    let mut state = ParserState::WaitingForRow { saw_cr: false };

    if args.wrap {
        output
            .write_all(b"[")
            .context("Writing initial byte to output")?
    }

    loop {
        let buf = input.fill_buf()?;

        if buf.is_empty() {
            break;
        }

        match state {
            ParserState::WaitingForRow { saw_cr } => match buf[0] {
                b'\n' | b'\r' => {
                    if saw_cr && buf[0] == b'\n' {
                        // We saw a CR, but now we see an LF, so the LF
                        input.consume(1);
                    } else {
                        output.write_all(b"[],")?;
                        input.consume(1);
                    }
                    // n += 1;
                    continue;
                }
                c if c == args.separator => {
                    output.write_all(b"[\"\",\"")?;
                    state = ParserState::InUnquotedField { start: true };
                }
                c if c == args.quote_character => {
                    output.write_all(b"[\"")?;
                    state = ParserState::InQuotedField {
                        maybe_ending: false,
                    };
                }
                c => {
                    output.write_all(b"[\"")?;
                    write_escaped_json_char(&mut output, c)?;
                    state = ParserState::InUnquotedField { start: false };
                }
            },
            ParserState::InUnquotedField { start } => match buf[0] {
                c if c == args.separator => {
                    output.write_all(b"\",\"")?;
                    state = ParserState::InUnquotedField { start: true };
                }
                c if c == args.quote_character => {
                    if start {
                        state = ParserState::InQuotedField {
                            maybe_ending: false,
                        };
                    } else {
                        write_escaped_json_char(&mut output, b'\"')?;
                    }
                }
                b'\n' | b'\r' => {
                    // End this row
                    output.write_all(b"\"],")?;
                    state = ParserState::WaitingForRow {
                        saw_cr: buf[0] == b'\r',
                    };
                }
                c => {
                    write_escaped_json_char(&mut output, c)?;
                    state = ParserState::InUnquotedField { start: false };
                }
            },
            ParserState::InQuotedField {
                maybe_ending: false,
            } => match buf[0] {
                c if c == args.quote_character => {
                    state = ParserState::InQuotedField { maybe_ending: true };
                }
                c => {
                    write_escaped_json_char(&mut output, c)?;
                }
            },
            ParserState::InQuotedField { maybe_ending: true } => match buf[0] {
                c if c == args.quote_character => {
                    // We got a second quote, so we aren't ending
                    write_escaped_json_char(&mut output, b'"')?;
                    state = ParserState::InQuotedField {
                        maybe_ending: false,
                    };
                }
                c if c == args.separator => {
                    output.write_all(b"\",\"")?;
                    state = ParserState::InUnquotedField { start: true };
                }
                b'\n' | b'\r' => {
                    output.write_all(b"\"],")?;
                    state = ParserState::WaitingForRow {
                        saw_cr: buf[0] == b'\r',
                    };
                }
                c => {
                    bail!("Unexpected character {:?} found after '\"'", c as char);
                }
            },
        }
        input.consume(1);
    }

    match state {
        ParserState::WaitingForRow { saw_cr: _ } => {}
        ParserState::InUnquotedField { start: _ } => output.write_all(b"\"]")?,
        ParserState::InQuotedField { maybe_ending: _ } => {
            bail!("Quoted field did not have closing quote")
        }
    }

    if args.wrap {
        output
            .write_all(b"]")
            .context("Writing final byte to output")?;
    }

    Ok(())
}

fn write_escaped_json_char(mut w: impl Write, c: u8) -> std::io::Result<()> {
    match c {
        b'"' => w.write_all(b"\\\""),
        b'\\' => w.write_all(b"\\\\"),
        b'/' => w.write_all(b"\\/"),
        0x08 => w.write_all(b"\\b"),
        0x0c => w.write_all(b"\\f"),
        b'\n' => w.write_all(b"\\n"),
        b'\r' => w.write_all(b"\\r"),
        b'\t' => w.write_all(b"\\t"),
        c => w.write_all(&[c]),
    }
}
