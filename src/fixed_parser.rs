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
            column_descs: dbg!(to_column_descs_inner(args.column)?),
        })
    }

    pub fn parse_buf<R: BufRead, W: Write>(self, reader: R, mut writer: W) -> Result<()> {
        // CSVs can have a BOM, so use encoding-rs to decode it
        let decoded_reader = BufReader::new(DecodeReaderBytes::new(reader));

        ParserInner {
            first_row: true,
            state: ParserRowState::default(),
            column_descs: self.column_descs,
        }
        .parse_buf(decoded_reader, writer)
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

struct ParserInner {
    first_row: bool,
    state: ParserRowState,
    column_descs: Vec<ColumnDescInner>,
}

#[derive(Debug, PartialEq, Eq)]
enum ParserRowState {
    Waiting {
        saw_cr: bool,
    },
    Within {
        col: usize,
        /// The number of bytes we read in this column, or 0
        already_read_bytes: usize,
    },
    /// We are done reading the row, but there might be extra data at the end
    DoneReading,
}

impl Default for ParserRowState {
    fn default() -> Self {
        Self::Waiting { saw_cr: false }
    }
}

impl ParserInner {
    fn parse_buf<R: BufRead, W: Write>(mut self, mut reader: R, mut writer: W) -> Result<()> {
        // // The current position in a current line
        // let mut position_in_record = 0;

        loop {
            let mut buf = reader.fill_buf()?;
            eprintln!("{:?}", self.state);
            eprintln!("Got buf: {:?}", String::from_utf8_lossy(buf));

            if buf.is_empty() {
                break;
            }

            if buf[0] == b'\n' && self.state == (ParserRowState::Waiting { saw_cr: true }) {
                // We just saw a \r, and now we see a \n
                // Ignore it and move on
                reader.consume(1);
                self.state = ParserRowState::Waiting { saw_cr: false };
                continue;
            } else if buf[0] == b'\n' || buf[0] == b'\r' {
                // We saw a newline
                // Make sure any columns that we were expecting are output as empty
                self.drain_remaining_empty(&mut writer)?;
                self.state = ParserRowState::Waiting {
                    saw_cr: buf[0] == b'\r',
                };
                reader.consume(1);
                continue;
            }

            // If there's a newline in this buffer, truncate buf
            // In the next loop iteration, we will catch the newline
            if let Some(idx) = memchr2(b'\n', b'\r', buf) {
                buf = &buf[0..idx];
            }

            // Buf now contains a (maybe complete) row
            // If we were waiting for a row, we no longer are waiting
            let (col, mut already_read_bytes) = match self.state {
                ParserRowState::Waiting { saw_cr: _saw_cr } => {
                    self.state = ParserRowState::Within {
                        col: 0,
                        already_read_bytes: 0,
                    };
                    self.start_row(&mut writer)?;
                    (0, 0)
                }
                ParserRowState::Within {
                    col,
                    already_read_bytes,
                } => (col, already_read_bytes),
                ParserRowState::DoneReading => {
                    // We are done witih this row already. Consume the rest of buf, since we don't care about it
                    let buf_len = buf.len();
                    reader.consume(buf_len);
                    continue;
                }
            };

            // Read one column's worth of data to writer
            if already_read_bytes == 0 {
                // This is the start of this column
                self.start_field(&mut writer)?;
            }

            let column_len = self.column_descs[col].len();
            let wanted_len = column_len - already_read_bytes;
            let available_len = std::cmp::min(wanted_len, buf.len());
            // self.write(&buf[0..column_len])?;
            self.write(&mut writer, &buf[0..available_len])?;
            reader.consume(available_len);
            already_read_bytes += available_len;

            if already_read_bytes == column_len {
                // We are done with this column
                self.end_field(&mut writer)?;

                self.state = if dbg!(self.column_descs.len()) <= col + 1 {
                    // This was the last column in the row, too. End the row
                    self.end_row(&mut writer)?;
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
                    already_read_bytes,
                }
            }
        }

        Ok(())
    }

    /// If we are in the middle of writing a row out, we know we ended prematurely, and the rest of the columns are empty
    fn drain_remaining_empty(&mut self, writer: &mut impl Write) -> Result<()> {
        eprintln!("Draining: {:?}", self.state);
        match self.state {
            ParserRowState::Waiting { .. } => {}
            ParserRowState::Within {
                col,
                already_read_bytes: _,
            } => {
                self.end_field(writer)?;

                // For every column after this one, output as empty
                for _ in (col + 1)..self.column_descs.len() {
                    self.start_field(writer)?;
                    self.end_field(writer)?;
                }

                self.end_row(writer)?;
            }
            ParserRowState::DoneReading => {}
        }

        Ok(())
    }

    fn start_row(&mut self, writer: &mut impl Write) -> Result<()> {
        self.write(writer, b"[")
    }

    fn end_row(&mut self, writer: &mut impl Write) -> Result<()> {
        let ret = self.write(writer, b"]");
        // We ended at least 1 row, so we are not on the first row anymore
        // But do this after we try writing ]
        self.first_row = false;
        ret
    }

    fn start_field(&mut self, writer: &mut impl Write) -> Result<()> {
        self.write(writer, b"\"")
    }

    fn end_field(&mut self, writer: &mut impl Write) -> Result<()> {
        self.write(writer, b"\",")
    }

    fn write(&mut self, writer: &mut impl Write, buf: &[u8]) -> Result<()> {
        if self.should_print() {
            eprintln!("> Writing: {:?}", String::from_utf8_lossy(buf));
            writer.write_all(buf)?;
        }

        Ok(())
    }

    /// Checks if we should be writing to output right now
    fn should_print(&self) -> bool {
        // We do not want to write if we are on the first row, but we want to skip headers
        // !(self.first_row && self.args.skip_header)
        true
    }

    fn write_escaped_json_char(&mut self, writer: &mut impl Write, c: u8) -> Result<()> {
        // eprintln!("Writing: {:?}", c as char);
        match c {
            b'"' => self.write(writer, b"\\\""),
            b'\\' => self.write(writer, b"\\\\"),
            b'/' => self.write(writer, b"\\/"),
            0x08 => self.write(writer, b"\\b"),
            0x0c => self.write(writer, b"\\f"),
            b'\n' => self.write(writer, b"\\n"),
            b'\r' => self.write(writer, b"\\r"),
            b'\t' => self.write(writer, b"\\t"),
            c => self.write(writer, &[c]),
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
