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
