mod indentor;
mod parser;

use anyhow::{bail, Context, Error, Result};
use atomicwrites::AtomicFile;
use clap::{Args, Parser, Subcommand};
use crossbeam_channel::{Receiver, Sender};
use interprocess::unnamed_pipe;
use jsoncutil::{csv_parser, fixed_parser, IoArg};
use jsoncutil::{CsvArgs, ATOMIC_FILE_OPTIONS};
use jsoncutil::{FixedArgs, Writer};
use notify_debouncer_mini::{new_debouncer, notify::*, DebounceEventResult};
use parser::Mode;
use std::io::stdout;
use std::{
    collections::HashSet,
    ffi::OsString,
    fs::File,
    io::{BufRead, BufReader, BufWriter},
    path::PathBuf,
    time::Duration,
};

#[derive(Parser, Debug)]
struct Cli {
    #[clap(
        help = "Input file, or `-` for stdin",
        default_value = "-",
        global = true
    )]
    input: IoArg,

    #[command(subcommand)]
    command: Option<Command>,

    #[command(flatten)]
    fmt_args: FmtArgs,

    #[clap(
        short = 'o',
        long = "output",
        help = "Output file; will be stdout if no output is specified",
        conflicts_with = "inplace",
        global = true
    )]
    output: Option<IoArg>,

    #[clap(
        long = "json",
        short = 'j',
        help = "Output json instead of jsonc",
        default_value_if("compact", "true", Some("true")),
        global = true
    )]
    output_json: bool,

    #[clap(
        long = "jsoncc-output",
        help = "Output file for jsonc",
        value_name = "FILE",
        conflicts_with = "output",
        conflicts_with = "output_json",
        conflicts_with = "inplace",
        global = true
    )]
    jsoncc_output: Option<IoArg>,

    #[clap(
        long = "json-output",
        help = "Output file for json",
        value_name = "FILE",
        conflicts_with = "output",
        conflicts_with = "output_json",
        conflicts_with = "inplace"
    )]
    json_output: Option<IoArg>,

    #[clap(short = 'U', long, help = "Allow unbounded operations", global = true)]
    unbounded: bool,
}

#[derive(Subcommand, Debug)]
enum Command {
    // #[clap(about = "Format a single file or stdin")]
    // Fmt(FmtArgs),
    #[clap(about = "Watch a file or directory for changes")]
    Watch(WatchArgs),

    #[clap(about = "Use a CSV file for the input instead of JSONC")]
    Csv(CsvArgs),

    #[clap(about = "Use a fixed-width file for the input instead of JSONC")]
    Fixed(FixedArgs),
}

#[derive(Args, Debug)]
struct WatchArgs {
    // path: PathBuf,
    #[clap(short = 'e', long = "extension", default_values = ["jsonc", "jsoncc"], help = "File extensions to track")]
    extensions: Vec<OsString>,

    #[clap(short = 'r', long = "recursive", help = "Recursively search files")]
    recursive: bool,
    // #[clap(short = 'I', long = "inplace", help = "Replace each file inplace")]
    // inplace: bool,
}

#[derive(Args, Debug, Clone)]
struct FmtArgs {
    #[clap(
        short = 'c',
        long = "compact",
        help = "Compact json format",
        conflicts_with = "output",
        global = true
    )]
    compact: bool,

    #[clap(
        short = 'N',
        long = "no-commas",
        help = "Exclude all commas",
        global = true
    )]
    no_commas: bool,

    #[clap(
        short = 'I',
        long = "inplace",
        help = "Replace file contents inplace",
        requires = "input",
        global = true
    )]
    inplace: bool,

    #[clap(short = 'V', long = "validate", help = "Validate input is valid")]
    validate: bool,
}

impl Cli {
    /// Where should we format Jsonc output to?
    fn jsonc_output(&self) -> Option<Writer> {
        match (
            self.output_json,
            self.fmt_args.inplace,
            self.jsoncc_output.is_some(),
        ) {
            (true, _, _) => {
                // --json implies we aren't using --jsonc-output, so we will not have anything here
                None
            }
            (false, true, _) => {
                // --inplace
                if !matches!(self.input, IoArg::File(_)) {
                    panic!("--inplace was specified, but input is not a file");
                }

                Some(self.input.to_writer())
            }
            (_, _, true) => {
                // --output-json
                self.jsoncc_output.as_ref().map(IoArg::to_writer)
            }
            (false, _, _) => Some(
                // No --json (default to stdout if unspecified)
                self.output
                    .as_ref()
                    .map(IoArg::to_writer)
                    .unwrap_or_else(|| {
                        Writer::BufferedWriter(Box::new(BufWriter::new(stdout().lock())))
                    }),
            ),
        }
    }

    fn json_output(&self) -> Option<Writer> {
        // These will never conflict because clap(conflicts_with) is specified
        match (
            self.output_json,
            self.fmt_args.inplace,
            self.json_output.is_some(),
        ) {
            (true, true, _) => {
                // --json --inplace
                if !matches!(self.input, IoArg::File(_)) {
                    panic!("--inplace was specified, but input is not a file");
                }

                Some(self.input.to_writer())
            }
            (_, _, true) => {
                // --output-json -
                self.json_output.as_ref().map(IoArg::to_writer)
            }
            (true, _, _) => Some(
                // --json (default to stdout if unspecified)
                self.output
                    .as_ref()
                    .map(IoArg::to_writer)
                    .unwrap_or_else(|| {
                        Writer::BufferedWriter(Box::new(BufWriter::new(stdout().lock())))
                    }),
            ),
            _ => None,
        }
    }
}

// impl<'a> AsRef<Output<'a>> for IoArg {
//     fn as_ref<'b>(&'b self) -> &'b Output<'a> {
//         match self {
//             IoArg::Stdio => &Output::Stdio,
//             IoArg::File(f) => &Output::File(f),
//         }
//     }
// }

// #[derive(Debug)]
// pub(crate) enum IoArgRef<'a> {
//     Stdio,
//     File(&'a Path),
// }

fn main() -> Result<()> {
    let cli = Cli::parse();

    // TODO: Figure out how to validate this in clap Parser
    if cli.fmt_args.compact && cli.json_output.is_none() && !cli.output_json {
        bail!("Cannot compact format jsonc. Specify --json or use --json-output if you want to use --compact");
    }

    match &cli.command {
        None => {
            format_single_file(
                cli.input.to_reader()?,
                cli.jsonc_output(),
                cli.json_output(),
                &cli.fmt_args,
            )?;
        }
        Some(Command::Watch(a)) => watch(&cli, a)?,
        Some(Command::Csv(csv_args)) => {
            if !cli.unbounded && csv_args.object_format {
                bail!("-O requires you to set -U to allow unbounded operation");
            }

            // The input is CSV, so we hvae to use the csv parser
            format_single_csv(
                cli.input.clone(),
                cli.jsonc_output(),
                cli.json_output(),
                &cli.fmt_args,
                csv_args,
            )?;
        }
        Some(Command::Fixed(fixed_args)) => {
            // The input is CSV, so we hvae to use the csv parser
            format_single_fixed(
                cli.input.clone(),
                cli.jsonc_output(),
                cli.json_output(),
                &cli.fmt_args,
                fixed_args,
            )?;
        }
    }

    Ok(())
}

fn format_single_fixed(
    input: IoArg,
    jsonc_output: Option<Writer>,
    json_output: Option<Writer>,
    fmt_args: &FmtArgs,
    fixed_args: &FixedArgs,
) -> Result<()> {
    let (writer, reader) = unnamed_pipe::pipe()?;

    let parser = fixed_parser::Parser::new(fixed_args.clone())?;
    let handle = std::thread::spawn(move || -> Result<()> {
        // let br = input_to_reader(&input)?;
        parser.parse_buf(&mut input.to_reader()?, writer)?;
        Ok(())
    });

    // panic!("Output: {}", std::io::read_to_string(reader).unwrap());

    let formatting_result = format_single_file(
        Box::new(BufReader::new(reader)),
        jsonc_output,
        json_output,
        fmt_args,
    )
    .context("Could not format csv output");

    let parsing_result = handle
        .join()
        .map_err(|e| anyhow::anyhow!("Could not join thread: {e:?}"))?
        .context("Could not parse as CSV");

    if parsing_result.is_err() || formatting_result.is_err() {
        eprintln!("Parsing as CSV: {parsing_result:?}");
        eprintln!("Formatting parsed CSV: {formatting_result:?}");
        bail!("Could not format CSV");
    }

    Ok(())
}

fn format_single_csv(
    input: IoArg,
    jsonc_output: Option<Writer>,
    json_output: Option<Writer>,
    fmt_args: &FmtArgs,
    csv_args: &CsvArgs,
) -> Result<()> {
    let (writer, reader) = unnamed_pipe::pipe()?;

    let csv_args = csv_args.clone();
    let parser = csv_parser::Parser::new(csv_args);
    let handle = std::thread::spawn(move || -> Result<()> {
        // let br = input_to_reader(&input)?;
        let ret = parser.parse_buf(&mut input.to_reader()?, writer);
        if let Err(e) = ret {
            eprintln!("{e:?}");
        }
        Ok(())
    });

    // panic!("Output: {}", std::io::read_to_string(reader).unwrap());

    let formatting_result = format_single_file(
        Box::new(BufReader::new(reader)),
        jsonc_output,
        json_output,
        fmt_args,
    )
    .context("Could not format csv output");

    let parsing_result = handle
        .join()
        .map_err(|e| anyhow::anyhow!("Could not join thread: {e:?}"))?
        .context("Could not parse as CSV");

    if parsing_result.is_err() || formatting_result.is_err() {
        eprintln!("Parsing as CSV: {parsing_result:?}");
        eprintln!("Formatting parsed CSV: {formatting_result:?}");
        bail!("Could not format CSV");
    }

    Ok(())
}

// TODO: Accept a [`FmtArgs`]
fn format_single_file(
    mut input: Box<dyn BufRead>,
    jsonc_output2: Option<Writer>,
    json_output2: Option<Writer>,
    fmt_args: &FmtArgs,
) -> Result<()> {
    // let mut input = input_to_reader(input)?;

    // First, format jsonc
    if let Some(jsonc_output) = jsonc_output2 {
        let parser = parser::Parser::new(
            parser::Mode::Jsoncc {
                include_commas: !fmt_args.no_commas,
            },
            // &mut input,
            // BufWriter::new(std::io::stdout()),
        );

        match jsonc_output {
            Writer::BufferedWriter(mut w) => parser.format_buf(&mut input, &mut w)?,
            Writer::AtomicFile(f) => {
                f.write(|w| parser.format_buf(&mut input, &mut BufWriter::new(w)))?
            }
        }
    }

    // Format json next
    if let Some(json_output) = json_output2 {
        let mode = if fmt_args.compact {
            Mode::CompactJson
        } else {
            Mode::Json
        };

        let parser = parser::Parser::new(mode);

        match json_output {
            Writer::BufferedWriter(mut w) => parser.format_buf(&mut input, &mut w)?,
            Writer::AtomicFile(f) => {
                f.write(|w| parser.format_buf(&mut input, &mut BufWriter::new(w)))?
            }
        }
    }

    // Validate - Just duplicate code here. If they want to validate, it adds a little extra cost anyway
    // Reformatting is probably not a big cost
    // if fmt_args.validate {
    //     // TODO: Not
    //     let mut buf = vec![];
    //     parser::Parser::new(Mode::CompactJson, &mut input, BufWriter::new(&mut buf))
    //         .format_buf()
    //         .context("Formatting file")?;

    //     oxidized_json_checker::validate(&buf[..])?;
    // }

    Ok(())
}

fn watch(cli: &Cli, args: &WatchArgs) -> Result<()> {
    // The path to watch
    let IoArg::File(ref watch_path) = cli.input else {
        panic!("Input must be specified")
    };
    // True if we are watching only a single file
    let is_watching_file = watch_path.is_file();

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
            watch_path,
            if args.recursive {
                RecursiveMode::Recursive
            } else {
                RecursiveMode::NonRecursive
            },
        )
        .context("Adding watch to debouncer")?;

    // Keep track of files that have just been formatted
    let mut just_formatted = HashSet::new();

    eprintln!("Watching {:?}", watch_path);

    while let Ok(evt) = terminate_rx.recv() {
        match evt {
            Ok(path) => {
                if !(args
                    .extensions
                    .iter()
                    .any(|ext| path.extension() == Some(ext))
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
                    Box::new(BufReader::new(File::open(&path).context("Reading input")?)),
                    Some(Writer::AtomicFile(AtomicFile::new(
                        &path,
                        ATOMIC_FILE_OPTIONS,
                    ))),
                    None,
                    &cli.fmt_args,
                ) {
                    Ok(()) => {
                        eprintln!("Formatted file {:?}", path);

                        if is_watching_file {
                            // If they are having us just watch a single file, we need to re-watch it
                            // This is because on formatting, the file is unlinked, so we lose our watch
                            debouncer
                                .watcher()
                                .watch(&path, RecursiveMode::NonRecursive)
                                .context("Adding watch to debouncer")?;
                        }

                        // We don't want to trigger anything for this file, so we ignore it next time
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
