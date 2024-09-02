use anyhow::{bail, Context, Error, Result};
use atomicwrites::{AtomicFile, OverwriteBehavior::AllowOverwrite};
use clap::{Args, Parser, Subcommand};
use crossbeam_channel::{Receiver, Sender};
use notify_debouncer_mini::{new_debouncer, notify::*, DebounceEventResult};
use std::{
    collections::HashSet,
    ffi::OsString,
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

    #[clap(short = 'e', long = "extension", default_values = ["jsonc", "jsoncc"])]
    extensions: Vec<OsString>,

    #[clap(short = 'r', long = "recursive")]
    recursive: bool,

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
            format_single_file(
                &a.input,
                a.jsonc_output().as_ref(),
                a.json_output.as_ref(),
                a.compact,
            )?;
        }
        Command::Watch(a) => watch(&a)?,
    }

    Ok(())
}

fn format_single_file(
    input: impl AsRef<Path>,
    jsonc_output: Option<&JsoncOutput>,
    json_output: Option<impl AsRef<Path>>,
    json_compact: bool,
) -> Result<()> {
    let input_str = fs::read_to_string(&input).context("Reading input")?;

    // First, format jsonc
    if let Some(jsonc_output) = jsonc_output {
        let output = fjson::to_jsonc(&input_str).context("Formatting to jsonc")?;

        match jsonc_output {
            JsoncOutput::Stdout => print!("{output}"),
            JsoncOutput::File(output_file) => AtomicFile::new(output_file, AllowOverwrite)
                .write(|f| f.write_all(&output.as_bytes()))
                .context("Writing jsonc output")?,
        }
    }

    // Format json next
    if let Some(ref json_output_file) = json_output {
        let output = if json_compact {
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
    let (terminate_tx, terminate_rx): (Sender<Result<PathBuf>>, Receiver<Result<PathBuf>>) =
        crossbeam_channel::bounded(100);

    let mut debouncer = new_debouncer(
        Duration::from_millis(50),
        move |res: DebounceEventResult| match res {
            Ok(events) => events.into_iter().for_each(|evt| {
                let _ = terminate_tx.send(Ok(evt.path));
            }),
            Err(e) => {
                let _ = terminate_tx.send(Err(<notify_debouncer_mini::notify::Error as Into<
                    Error,
                >>::into(e)
                .context("Getting debounced result")));
            }
        },
    )
    .context("Creating debouncer")?;

    debouncer
        .watcher()
        // TODO: Make this recursive or not
        .watch(
            Path::new(&args.path),
            if args.recursive {
                RecursiveMode::Recursive
            } else {
                RecursiveMode::NonRecursive
            },
        )
        .context("Adding watch to debouncer")?;

    let mut just_formatted = HashSet::new();
    while let Ok(evt) = terminate_rx.recv() {
        match evt {
            Ok(path) => {
                if !(args
                    .extensions
                    .iter()
                    .any(|ext| (&path).extension() == Some(ext))
                    || args.extensions.is_empty())
                {
                    // This extension doesn't match their requested extensions
                    continue;
                }

                if just_formatted.remove(&path) {
                    // We just formatted it. This event came from us
                    continue;
                }

                eprintln!("Got result: {path:#?}");

                match format_single_file(
                    &path,
                    Some(&JsoncOutput::File(&path)),
                    None::<&Path>,
                    false,
                ) {
                    Ok(()) => {
                        eprintln!("Formatted file {:?}", path);
                        just_formatted.insert(path);
                    }
                    Err(e) => {
                        eprintln!("Error formatting file {:?}: {e:?}", path);
                    }
                }
            }
            Err(e) => {
                eprintln!("Stopping watch because of error: {e:?}");
            }
        }
    }

    Ok(())
}
