use anyhow::{bail, Context, Result};
use atomicwrites::{AtomicFile, OverwriteBehavior::AllowOverwrite};
use clap::{Args, Parser, Subcommand};
use notify_debouncer_mini::{new_debouncer, notify::*, DebounceEventResult};
use std::{
    fs,
    io::Write,
    path::{Path, PathBuf},
    time::Duration,
};

#[derive(Parser, Debug)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand, Debug)]
enum Command {
    Fmt(FmtArgs),
    Watch(WatchArgs),
}

#[derive(Args, Debug)]
struct WatchArgs {
    path: PathBuf,

    #[clap(short = 'I', long = "inplace")]
    inplace: bool,
}

#[derive(Args, Debug)]
struct FmtArgs {
    // #[clap(short = 'i', long = "input")]
    input: PathBuf,

    #[clap(short = 'o', long = "output")]
    output: Option<PathBuf>,

    #[clap(short = 'O', long = "json-output")]
    json_output: Option<PathBuf>,

    #[clap(short = 'c', long = "compact")]
    compact: bool,

    #[clap(short = 'I', long = "inplace")]
    inplace: bool,
}

impl FmtArgs {
    /// Where should we format Jsonc output to?
    fn jsonc_output(&self) -> Option<JsoncOutput> {
        if self.inplace {
            Some(JsoncOutput::File(&self.input))
        } else if let Some(ref output_file) = &self.output {
            Some(JsoncOutput::File(output_file))
        } else if self.json_output.is_some() {
            // We don't want to output jsonc anywhere if they don't specify -o and they do specify -O
            None
        } else {
            // If they don't have any output specified, default to stdout
            Some(JsoncOutput::Stdout)
        }
    }
}

enum JsoncOutput<'a> {
    Stdout,
    File(&'a Path),
}

fn main() -> Result<()> {
    let options = Cli::parse();

    eprintln!("Options: {options:?}");

    match options.command {
        Command::Fmt(a) => {
            // TODO: Figure out how to validate this in clap Parser
            if a.compact && a.json_output.is_none() {
                bail!("Cannot compact format jsonc. Specify --json-output if you want to use --compact");
            }
            if a.inplace && a.output.is_some() {
                bail!("Cannot format --inplace when --output is specified");
            }
            format_single_file(&a)?;
        }
        Command::Watch(a) => watch(&a)?,
    }

    Ok(())
}

fn format_single_file(args: &FmtArgs) -> Result<()> {
    let input_str = fs::read_to_string(&args.input).context("Reading input")?;

    // First, format jsonc
    if let Some(jsonc_output) = args.jsonc_output() {
        let output = fjson::to_jsonc(&input_str).context("Formatting to jsonc")?;

        match jsonc_output {
            JsoncOutput::Stdout => print!("{output}"),
            JsoncOutput::File(output_file) => AtomicFile::new(output_file, AllowOverwrite)
                .write(|f| f.write_all(&output.as_bytes()))
                .context("Writing jsonc output")?,
        }
    }

    // Format json next
    if let Some(ref json_output_file) = args.json_output {
        let output = if args.compact {
            fjson::to_json(&input_str).context("Formatting to json")
        } else {
            fjson::to_json_compact(&input_str).context("Formatting to json")
        }?;

        AtomicFile::new(json_output_file, AllowOverwrite)
            .write(|f| f.write_all(&output.as_bytes()))
            .context("Writing jsonc output")?;
    }

    Ok(())
}

fn watch(args: &WatchArgs) -> Result<()> {
    // Select recommended watcher for debouncer.
    // Using a callback here, could also be a channel.
    let mut debouncer =
        new_debouncer(
            Duration::from_millis(50),
            |res: DebounceEventResult| match res {
                Ok(events) => events
                    .iter()
                    .for_each(|e| println!("Event {:?} for {:?}", e.kind, e.path)),
                Err(e) => println!("Error {:?}", e),
            },
        )
        .context("Creating debouncer")?;

    // Add a path to be watched. All files and directories at that path and
    // below will be monitored for changes.
    debouncer
        .watcher()
        .watch(Path::new("."), RecursiveMode::Recursive)
        .context("Adding watch to debouncer")?;

    // note that dropping the debouncer (as will happen here) also ends the debouncer
    // thus this demo would need an endless loop to keep running

    Ok(())
}
