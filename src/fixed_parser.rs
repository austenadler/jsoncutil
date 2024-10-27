use encoding_rs_io::DecodeReaderBytes;
use memchr::memchr2;
use std::io::BufRead;
use std::io::BufReader;
use std::io::BufWriter;
use std::io::Write;
use std::ops::Range;

use anyhow::{bail, Context, Result};

use crate::FixedArgs;
use crate::FixedColumnDesc;
use crate::FixedFieldStart;

pub struct Parser {
    // pub args: FixedArgs,
    column_descs: Vec<ColumnDescInner>,
}

/// A description for a column, in range form
#[derive(PartialEq, Eq, Debug)]
struct ColumnDescInner {
    range: Range<usize>,
    name: Option<String>,
}

impl ColumnDescInner {
    fn len(&self) -> usize {
        self.range.end - self.range.start
    }
}

impl Parser {
    pub fn new(args: FixedArgs) -> Result<Self> {
        Ok(Self {
            column_descs: to_column_descs_inner(args.column)?,
        })
    }

    pub fn parse_buf<R: BufRead, W: Write>(self, reader: R, writer: W) -> Result<()> {
        // CSVs can have a BOM, so use encoding-rs to decode it
        let decoded_reader = DecodeReaderBytes::new(reader);

        ParserInner {
            first_row: true,
            reader: BufReader::new(decoded_reader),
            writer: BufWriter::new(writer),
            state: ParserRowState::default(),
            column_descs: self.column_descs,
        }
        .parse_buf()
    }
}

/// Ensure columns don't overlap or are out of order
fn to_column_descs_inner(columns: Vec<FixedColumnDesc>) -> Result<Vec<ColumnDescInner>> {
    if columns.is_empty() {
        bail!("Specifying columns is required");
    }

    let mut ret = Vec::with_capacity(columns.len());
    // The ending of the previous column
    let mut previous_end = 0;

    for (idx, c) in columns.into_iter().enumerate() {
        let from = match c.start {
            FixedFieldStart::Position(n) => {
                // The arguments are 1-indexed, but this is 0-indexed
                let n = n - 1;

                if n < previous_end {
                    bail!(
                        "Column {idx} starting position overlaps with a previous column definition"
                    );
                } else {
                    n
                }
            }
            FixedFieldStart::Offset(o) => previous_end + o,
        };

        if c.length == 0 {
            bail!("Column {}'s length cannot be 0 in arguments", idx);
        }

        let to = from + c.length;
        previous_end = to;

        ret.push(ColumnDescInner {
            range: from..to,
            name: c.name,
        });
    }

    Ok(ret)
}

struct ParserInner<R: BufRead, W: Write> {
    first_row: bool,
    reader: R,
    writer: W,
    state: ParserRowState,
    column_descs: Vec<ColumnDescInner>,
}

#[derive(Debug, PartialEq, Eq)]
enum ParserRowState {
    WaitingFor {
        saw_cr: bool,
    },
    Within {
        col: usize,
        already_read_bytes: usize,
    },
    /// We are done reading the row, but there might be extra data at the end
    DoneReading,
}

impl Default for ParserRowState {
    fn default() -> Self {
        Self::WaitingFor { saw_cr: false }
    }
}

impl<R: BufRead, W: Write> ParserInner<R, W> {
    fn parse_buf(mut self) -> Result<()> {
        // // The current position in a current line
        // let mut position_in_record = 0;

        loop {
            let mut buf = self.reader.fill_buf()?;
            eprintln!("{:?}", self.state);
            eprintln!("Got buf: {buf:?}");

            if buf.is_empty() {
                break;
            }

            // ParserState::WaitingForRow { saw_cr } => match c {
            //     b'\n' | b'\r' => {
            //         if saw_cr && c == b'\n' {
            //             // We saw a CR, but now we see an LF, so the LF is ignored
            //             self.reader.consume(1);
            //         } else {
            //             // This is empty, so start and then end the row
            //             self.start_row()?;
            //             // TODO: Does this csv file have one field or 0 fields?
            //             // I think 1 because it *is* a line
            //             self.start_field()?;
            //             self.end_field()?;
            //             self.end_row()?;
            //             self.reader.consume(1);
            //         }
            //         // n += 1;
            //         continue;
            //     }

            // let first_char = buf[0];
            // match first_char {
            //     b'\n' if self.state == ParserRowState::WaitingFor { saw_cr: true } => {
            //         self.reader.consume(1);
            //         continue;
            //     }
            //     b'\n' | b'\r' => {
            //         self.drain_remaining_empty()?;
            //         self.state = ParserRowState::WaitingFor { saw_cr: true };
            //     }
            //     _ => {}
            // }

            if buf[0] == b'\n' && self.state == (ParserRowState::WaitingFor { saw_cr: true }) {
                self.reader.consume(1);
                self.state = ParserRowState::WaitingFor { saw_cr: false };
                continue;
            } else if buf[0] == b'\n' || buf[0] == b'\r' {
                self.drain_remaining_empty()?;
                self.state = ParserRowState::WaitingFor {
                    saw_cr: buf[0] == b'\r',
                };
                continue;
            }

            if buf[0] == b'\r' {
                self.drain_remaining_empty()?;
                self.state = ParserRowState::WaitingFor { saw_cr: true };
                self.reader.consume(1);
                continue;
            } else if buf[0] == b'\n' {
                self.drain_remaining_empty()?;
                self.state = ParserRowState::WaitingFor { saw_cr: false };
                self.reader.consume(1);
                continue;
            }

            // If there's a newline in this buffer, truncate buf
            // In the next loop iteration, we will catch the newline
            if let Some(idx) = memchr2(b'\n', b'\r', buf) {
                buf = &buf[0..idx];
            }

            // Buf now contains a (maybe complete) row
            // If we were waiting for a row, we no longer are waiting
            let (col, already_read_bytes) = match self.state {
                ParserRowState::WaitingFor { saw_cr: _saw_cr } => {
                    self.state = ParserRowState::Within {
                        col: 0,
                        already_read_bytes: 0,
                    };
                    self.writer.write_all(b"[")?;
                    (0, 0)
                }
                ParserRowState::Within {
                    col,
                    already_read_bytes,
                } => (col, already_read_bytes),
                ParserRowState::DoneReading => {
                    // We are done witih this row already. Consume the rest of buf, since we don't care about it
                    let buf_len = buf.len();
                    self.reader.consume(buf_len);
                    continue;
                }
            };

            // Read one column's worth of data to writer
            if already_read_bytes == 0 {
                // This is the start of this column
                self.writer.write_all(b"\"")?;
                // I don't know why this doesn't work
                // self.start_field()?;
            }

            let column_len = self.column_descs[col].len();
            let available_len = std::cmp::min(column_len, buf.len());
            // self.write(&buf[0..column_len])?;
            self.writer.write_all(&buf[0..available_len])?;
            self.reader.consume(available_len);

            if already_read_bytes == column_len {
                // We are done with this column
                self.end_field()?;

                self.state = if dbg!(self.column_descs.len()) <= dbg!(col + 1) {
                    // This was the last column in the row, too. End the row
                    self.end_row()?;
                    ParserRowState::DoneReading
                } else {
                    // We have another column in this row
                    ParserRowState::Within {
                        col: col + 1,
                        already_read_bytes: 0,
                    }
                };
            } else {
                // This buf did not completely encompass the field
                // Increase the number of bytes read and leave the column number alone

                self.state = ParserRowState::Within {
                    col,
                    already_read_bytes: 0,
                }
            }
        }

        Ok(())
    }

    /// If we are in the middle of writing a row out, we know we ended prematurely, and the rest of the columns are empty
    fn drain_remaining_empty(&mut self) -> Result<()> {
        eprintln!("Draining: {:?}", self.state);
        match self.state {
            ParserRowState::WaitingFor { saw_cr } => return Ok(()),
            ParserRowState::Within {
                col,
                already_read_bytes,
            } => {
                todo![]
            }
            ParserRowState::DoneReading => todo!(),
        }
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
        // !(self.first_row && self.args.skip_header)
        true
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

    fn handle_row(&self, buf: &[u8]) -> Result<()> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use super::*;

    #[test]
    fn test_to_column_descs_inner() {
        assert_eq!(
            dbg!(to_column_descs_inner(vec![FixedColumnDesc::from_str("1").unwrap()]).unwrap()),
            vec![ColumnDescInner {
                range: 0..1,
                name: None
            }]
        );
        assert_eq!(
            dbg!(to_column_descs_inner(dbg!(["1", "5,10"]
                .into_iter()
                .map(|d| FixedColumnDesc::from_str(d).expect("Could not parse column desc"))
                .collect::<Vec<_>>()))
            .unwrap()),
            vec![
                ColumnDescInner {
                    range: 0..1,
                    name: None
                },
                ColumnDescInner {
                    range: 4..14,
                    name: None
                }
            ]
        );

        assert_eq!(
            dbg!(
                to_column_descs_inner(dbg!(["1", "10;Second", "12,1;", "+2,8"]
                    .into_iter()
                    .map(|d| FixedColumnDesc::from_str(d).expect("Could not parse column desc"))
                    .collect::<Vec<_>>()))
                .unwrap()
            ),
            vec![
                ColumnDescInner {
                    range: 0..1,
                    name: None
                },
                ColumnDescInner {
                    range: 1..11,
                    name: Some(String::from("Second"))
                },
                ColumnDescInner {
                    range: 11..12,
                    name: None
                },
                ColumnDescInner {
                    range: 14..22,
                    name: None
                },
            ]
        );
    }
}
