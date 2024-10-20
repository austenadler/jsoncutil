use crate::IoArg;
use std::io::Write;

use anyhow::{Context, Result};

pub fn csv_reader_to_json_writer(input: IoArg, mut output: impl Write) -> Result<()> {
    // eprintln!("Starting reading {input:?}");
    // eprintln!("{:?}", read_to_string(input.as_output().input_to_reader()?));
    // panic!("x");
    let mut reader = csv::ReaderBuilder::new()
        .has_headers(false)
        .flexible(true)
        .from_reader(
            input
                .as_output()
                .input_to_reader()
                .context("Converting input file to a reader")?,
        );

    output
        .write_all(b"[")
        .context("Writing initial byte to output")?;
    for result in reader.records() {
        let record = result.context("Reading record")?;

        let json = serde_json::to_string(&record.iter().collect::<Vec<_>>())
            .context("Serializing strings to json")?;
        output
            .write_all(json.as_bytes())
            .context("Writing output")?;
    }
    output
        .write_all(b"]")
        .context("Writing final byte to output")?;

    Ok(())
}
