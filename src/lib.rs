use atomicwrites::{
    AtomicFile,
    OverwriteBehavior::{self, AllowOverwrite},
};
use clap::Args;
use std::{
    convert::Infallible,
    error::Error,
    fs::File,
    io::{stdin, stdout, BufRead, BufReader, Write},
    path::{Path, PathBuf},
    str::FromStr,
};

use anyhow::{Context, Result};

pub mod csv_parser;
pub mod fixed_parser;
pub mod indentor;
pub mod parser;

#[derive(Debug)]
pub enum IoArgRef<'a> {
    Stdio,
    File(&'a Path),
}

/// An argument that represents a file or stdin/stdout
#[derive(Debug, Clone)]
pub enum IoArg {
    Stdio,
    File(PathBuf),
}

impl IoArg {
    pub fn to_writer(&self) -> Writer {
        match self {
            Self::Stdio => Writer::BufferedWriter(Box::new(stdout().lock())),
            Self::File(f) => Writer::AtomicFile(AtomicFile::new(f, ATOMIC_FILE_OPTIONS)),
        }
    }

    pub fn to_reader(&self) -> Result<Box<dyn BufRead>> {
        Ok(match self {
            Self::Stdio => Box::new(stdin().lock()),
            Self::File(f) => Box::new(BufReader::new(File::open(f).context("Reading input")?)),
        })
    }
}

pub const ATOMIC_FILE_OPTIONS: OverwriteBehavior = AllowOverwrite;

pub enum Writer {
    BufferedWriter(Box<dyn Write>),
    AtomicFile(AtomicFile),
}

impl Default for IoArg {
    fn default() -> Self {
        Self::Stdio
    }
}

impl FromStr for IoArg {
    type Err = Infallible;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        if s == "-" {
            Ok(Self::Stdio)
        } else {
            PathBuf::from_str(s).map(Self::File)
        }
    }
}

impl IoArg {
    pub fn as_output(&self) -> IoArgRef {
        match self {
            Self::Stdio => IoArgRef::Stdio,
            Self::File(f) => IoArgRef::File(f),
        }
    }
}

#[derive(Args, Debug, Clone)]
pub struct CsvArgs {
    #[clap(long, help = "CSV quote character", default_value = "\"", value_parser = parse_single_char)]
    pub quote_character: u8,

    #[clap(long, help = "CSV field separator", default_value = ",", value_parser = parse_single_char)]
    pub field_separator: u8,

    #[clap(
        short = 'H',
        long,
        help = "Exclude the header row of the CSV from the output"
    )]
    pub skip_header: bool,

    #[clap(short = 'w', long, help = "Wrap output in an array")]
    pub wrap: bool,

    /// Output in object format instead of array format
    #[clap(
        short = 'O',
        long = "object",
        global = true,
        conflicts_with = "skip_header"
    )]
    pub object_format: bool,
}

#[derive(Args, Debug, Clone)]
pub struct FixedArgs {
    /// A list of columns in the fixed width file
    #[clap(
        short = 'C', long, value_parser = FixedColumnDesc::from_str,
        help = "A fixed column description in the format [+OFFSET,]WIDTH[-FIELD_NAME] or [POSITION,]WIDTH[-FIELD_NAME] where OFFSET is the offset from the end of the previous column"
    )]
    pub column: Vec<FixedColumnDesc>,

    /// Output in object format instead of array format
    #[clap(short = 'O', long = "object")]
    pub object_format: bool,
}

/// A description of a fixed column, including a start, length, and optional name
///
/// The start of a column could be an absolute position, or an offset from the previous column
///
/// ```rust
/// use jsoncutil::FixedColumnDesc;
/// use jsoncutil::FixedFieldStart;
/// use std::str::FromStr;
///
/// assert_eq!(FixedColumnDesc::from_str("10").unwrap(), FixedColumnDesc {
///     start: FixedFieldStart::Offset(0),
///     length: 10,
///     name: None,
/// });
/// assert_eq!(FixedColumnDesc::from_str("10-Name").unwrap(), FixedColumnDesc {
///     start: FixedFieldStart::Offset(0),
///     length: 10,
///     name: Some(String::from("Name")),
/// });
/// assert_eq!(FixedColumnDesc::from_str("5,10-Name-with-dashes").unwrap(), FixedColumnDesc {
///     start: FixedFieldStart::Position(5),
///     length: 10,
///     name: Some(String::from("Name-with-dashes")),
/// });
/// assert_eq!(FixedColumnDesc::from_str("+5,10-Name").unwrap(), FixedColumnDesc {
///     start: FixedFieldStart::Offset(5),
///     length: 10,
///     name: Some(String::from("Name")),
/// });
/// ```
#[derive(Args, Debug, Clone, PartialEq, Eq)]
pub struct FixedColumnDesc {
    #[clap(value_parser = FixedFieldStart::from_str)]
    pub start: FixedFieldStart,
    pub length: usize,
    pub name: Option<String>,
}

impl FromStr for FixedColumnDesc {
    type Err = Box<dyn Error + Send + Sync>;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        // "2,3;Name" => Some("Name")
        // "2,3;" or "2,3" or "2" => None
        let (rest, name) = s
            .split_once('-')
            .map(|(rest, name)| {
                (
                    rest,
                    if name.is_empty() {
                        None
                    } else {
                        Some(name.to_string())
                    },
                )
            })
            .unwrap_or((s, None));

        // Length is required, but start is optional
        let (start, length) = rest
            .split_once(",")
            .map(|(a, b)| (a.trim(), b.trim()))
            .unwrap_or(("", rest.trim()));

        let start = if start.is_empty() {
            FixedFieldStart::Offset(0)
        } else {
            FixedFieldStart::from_str(start)?
        };

        let length = length
            .parse()
            .map_err(|e| format!("Could not parse length: {e:?}"))?;

        Ok(FixedColumnDesc {
            start,
            length,
            name,
        })
    }
}

/// A column description for a fixed-width column
///
/// * "123" => FixedFieldStart::Position(123)
/// * "+1" => FixedFieldStart::Offset(1)
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FixedFieldStart {
    Position(usize),
    Offset(usize),
}

impl FromStr for FixedFieldStart {
    type Err = Box<dyn Error + Send + Sync>;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        if s.is_empty() {
            Ok(Self::Offset(0))
        } else if s.len() >= 2 && s.starts_with('+') {
            Ok(s[1..].parse::<usize>().map(Self::Offset)?)
        } else {
            Ok(s.parse::<usize>().map(Self::Position)?)
        }
    }
}

fn parse_single_char(arg: &str) -> Result<u8, &'static str> {
    if arg.len() != 1 {
        return Err("Only one char allowed");
    }

    let bytes = arg.as_bytes();
    if bytes.len() != 1 {
        return Err("Only ascii supported");
    }

    Ok(bytes[0])
}

pub(crate) fn escape_string(writer: &mut impl Write, s: &[u8]) -> Result<()> {
    let mut buf = Vec::with_capacity(s.len() + 10);

    json::JsonValue::String(
        String::from_utf8(s.to_vec()).context("Cannot escape non-utf8 string")?,
    )
    .write(&mut buf)
    .context("Can not write escaped json to output")?;

    std::io::copy(&mut &buf[1..buf.len() - 1], writer)?;

    Ok(())
}
