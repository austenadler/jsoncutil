use atomicwrites::{
    AtomicFile,
    OverwriteBehavior::{self, AllowOverwrite},
};
use clap::Args;
use std::{
    convert::Infallible,
    fs::File,
    io::{stdin, stdout, BufRead, BufReader, Write},
    path::{Path, PathBuf},
    str::FromStr,
};

use anyhow::{Context, Result};

pub mod csv_parser;
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
