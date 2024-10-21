use clap::Args;
use std::{
    convert::Infallible,
    fs::File,
    io::{BufRead, BufReader},
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

impl<'a> IoArgRef<'a> {
    pub fn input_to_reader(&self) -> Result<Box<dyn BufRead>> {
        Ok(if let IoArgRef::File(input_filename) = self {
            Box::new(BufReader::new(
                File::open(input_filename).context("Reading input")?,
            ))
        } else {
            // stdin is already buffered, so we don't need to wrap it in a bufreader
            Box::new(std::io::stdin().lock())
        })
    }
}

/// An argument that represents a file or stdin/stdout
#[derive(Debug, Clone)]
pub enum IoArg {
    Stdio,
    File(PathBuf),
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
    #[clap(help = "CSV Input file, or `-` for stdin", default_value = "-")]
    pub input: IoArg,

    #[clap(short = 's', long, help = "CSV record separator", default_value = ",", value_parser = parse_single_char)]
    pub separator: u8,
    #[clap(short = 'q', long, help = "CSV quote character", default_value = "\"", value_parser = parse_single_char)]
    pub quote_character: u8,
    // #[clap(short = 'H', long, help = "Skip the header row of the CSV")]
    // pub skip_header: bool,
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
