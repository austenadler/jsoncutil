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
    object_format_names: Option<Vec<String>>,
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
        // Optional containing array of same length as column
        let object_format_names = args.object_format.then(|| {
            args.column
                .iter()
                .enumerate()
                .map(|(idx, a)| {
                    let mut ret = Vec::new();
                    json::JsonValue::String(
                        a.name
                            .as_deref()
                            .map(String::from)
                            .unwrap_or_else(|| format!("Column {}", idx + 1)),
                    )
                    .write(&mut ret)
                    .expect("Could not serialize argument name as json");
                    let ret: String = String::from_utf8(ret)
                        .expect("Internal error: json-encoded string is no longer utf8");

                    ret
                })
                .collect()
        });

        Ok(Self {
            column_descs: to_column_descs_inner(args.column)?,
            object_format_names,
        })
    }

    pub fn parse_buf<R: BufRead, W: Write>(self, reader: R, mut writer: W) -> Result<()> {
        // CSVs can have a BOM, so use encoding-rs to decode it
        let decoded_reader = BufReader::new(DecodeReaderBytes::new(reader));

        let state = ParserRowState::default();

        ParserInner {
            first_row: true,
            state,
            column_descs: self.column_descs,
            object_format_names: self.object_format_names,
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
                let n = n
                    .checked_sub(1)
                    .expect("Columns are 1-indexed; do not provide 0 as an index");

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
    /// If `-O` is specified, then output in object format
    object_format_names: Option<Vec<String>>,
}

#[derive(Debug, PartialEq, Eq)]
enum ParserRowState {
    Waiting {
        saw_cr: bool,
    },
    Within {
        // Number of bytes required to skip before parsing this column
        skip: usize,
        // Index of the column we are within
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
            // eprintln!("{:?}", self.state);
            // eprintln!("Got buf: {:?}", String::from_utf8_lossy(buf));

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
                // if matches!(self.state, ParserRowState::Waiting { .. }) {
                //     self.start_row(&mut writer)?;
                //     self.drain_remaining_empty(&mut writer)?;
                //     self.end_row(&mut writer)?;
                // }
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
                ParserRowState::Within {
                    skip: 0,
                    col,
                    already_read_bytes,
                } => {
                    // We are ready to read the column data
                    (col, already_read_bytes)
                }
                ParserRowState::Waiting { saw_cr: _saw_cr } => {
                    self.state = ParserRowState::Within {
                        skip: self.column_descs[0].range.start,
                        col: 0,
                        already_read_bytes: 0,
                    };
                    self.start_row(&mut writer)?;
                    continue;
                }
                ParserRowState::Within {
                    skip,
                    col,
                    already_read_bytes,
                } => {
                    // We need to discard `skip` bytes before we start reading the column data
                    self.state = if skip > buf.len() {
                        // This buffer won't completely satisfy the skip count
                        let buf_len = buf.len();
                        reader.consume(buf_len);
                        ParserRowState::Within {
                            skip: skip - buf_len,
                            col,
                            already_read_bytes,
                        }
                    } else {
                        // We can consume the rest required
                        reader.consume(skip);
                        ParserRowState::Within {
                            skip: 0,
                            col,
                            already_read_bytes: 0,
                        }
                    };
                    continue;
                }
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
                self.start_field(&mut writer, col)?;
            }

            // The width of this column
            let column_len = self.column_descs[col].len();
            // The width of the column - the number of bytes we've already read
            let wanted_len = column_len - already_read_bytes;
            // The number of bytes we can actually read from the buf
            let available_len = std::cmp::min(wanted_len, buf.len());
            // self.write(&mut writer, &buf[0..available_len])?;
            crate::escape_string(&mut writer, &buf[0..available_len])?;
            reader.consume(available_len);
            already_read_bytes += available_len;

            if already_read_bytes == column_len {
                // We are done with this column
                self.end_field(&mut writer)?;

                self.state = if self.column_descs.len() <= col + 1 {
                    // This was the last column in the row, too. End the row
                    self.end_row(&mut writer)?;
                    ParserRowState::DoneReading
                } else {
                    // We have another column in this row
                    ParserRowState::Within {
                        skip: self.column_descs[col + 1].range.start
                            - self.column_descs[col].range.end,
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
                    skip: 0,
                }
            }
        }

        Ok(())
    }

    /// If we are in the middle of writing a row out, we know we ended prematurely, and the rest of the columns are empty
    fn drain_remaining_empty(&mut self, writer: &mut impl Write) -> Result<()> {
        // eprintln!("Draining: {:?}", self.state);
        match self.state {
            ParserRowState::Waiting { .. } => {
                self.start_row(writer)?;

                // For every column after this one, output as empty
                for col in 0..self.column_descs.len() {
                    self.start_field(writer, col)?;
                    self.end_field(writer)?;
                }

                self.end_row(writer)?;
            }
            ParserRowState::Within {
                col,
                skip: _,
                already_read_bytes,
            } => {
                if already_read_bytes == 0 && self.column_descs[col].range.end != already_read_bytes
                {
                    // We didn't finish reading this column
                    self.start_field(writer, col)?;
                }
                self.end_field(writer)?;

                // For every column after this one, output as empty
                for col in (col + 1)..self.column_descs.len() {
                    self.start_field(writer, col)?;
                    self.end_field(writer)?;
                }

                self.end_row(writer)?;
            }
            ParserRowState::DoneReading => {}
        }

        Ok(())
    }

    fn start_row(&mut self, writer: &mut impl Write) -> Result<()> {
        self.write(writer, if self.use_object_format() { b"{" } else { b"[" })
    }

    fn end_row(&mut self, writer: &mut impl Write) -> Result<()> {
        let ret = self.write(writer, if self.use_object_format() { b"}" } else { b"]" });
        // We ended at least 1 row, so we are not on the first row anymore
        // But do this after we try writing ]
        self.first_row = false;
        ret
    }

    fn start_field(&mut self, writer: &mut impl Write, col: usize) -> Result<()> {
        if let Some(ref object_format_names) = self.object_format_names {
            // They want object format
            // We do not need to add quotes because they are already quoted
            writer.write_all(object_format_names[col].as_bytes())?;
            self.write(writer, b":\"")?;
            Ok(())
        } else {
            // They want array format
            self.write(writer, b"\"")
        }
    }

    fn end_field(&mut self, writer: &mut impl Write) -> Result<()> {
        self.write(writer, b"\",")
    }

    fn write(&mut self, writer: &mut impl Write, buf: &[u8]) -> Result<()> {
        if self.should_print() {
            // eprintln!("> Writing: {:?}", String::from_utf8_lossy(buf));
            writer.write_all(buf)?;
        }

        Ok(())
    }

    fn use_object_format(&self) -> bool {
        self.object_format_names.is_some()
    }

    /// Checks if we should be writing to output right now
    fn should_print(&self) -> bool {
        // We do not want to write if we are on the first row, but we want to skip headers
        // !(self.first_row && self.args.skip_header)
        true
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
                to_column_descs_inner(dbg!(["1", "10-Second", "12,1-", "+2,8"]
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
