use anyhow::{bail, Context, Result};
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
    // Watch(WatchArgs),
}

// #[derive(Args, Debug)]
// struct WatchArgs {
//     dir: PathBuf
// }

#[derive(Args, Debug)]
struct FmtArgs {
    #[clap(short = 'i', long = "input")]
    input: PathBuf,
    #[clap(short = 'o', long = "output")]
    output: Option<PathBuf>,

    #[clap(short = 'O', long = "json-output")]
    json_output: Option<PathBuf>,

    #[clap(short = 'c', long = "compact")]
    compact: bool,
}

fn main() -> Result<()> {
    let options = Cli::parse();

    eprintln!("Options: {options:?}");

    match options.command {
        Command::Fmt(a) => {
            if a.compact && a.json_output.is_none() {
                bail!("Cannot compact format jsonc. Specify --json-output if you want to use --compact");
            }
            format_single_file(&a)?;
        }
    }

    Ok(())
}

fn format_single_file(args: &FmtArgs) -> Result<()> {
    let input_str = fs::read_to_string(&args.input).context("Reading input")?;

    if args.output.is_none() && args.json_output.is_none() {
        // They only want to print to stdout
        println!(
            "{}",
            fjson::to_jsonc(&input_str).context("Formatting to jsonc")?
        );

        return Ok(());
    }

    if let Some(ref output) = args.output {
        let mut output_file = fs::OpenOptions::new()
            .create(true)
            .truncate(true)
            .write(true)
            .open(output)
            .context("Opening output file")?;

        let output = fjson::to_jsonc(&input_str).context("Formatting to jsonc")?;

        output_file
            .write_all(&output.as_bytes())
            .context("Writing jsonc output")?;

        output_file.flush().context("Flushing jsonc output")?;
    }

    if let Some(ref json_output) = args.json_output {
        let mut json_output_file = fs::OpenOptions::new()
            .create(true)
            .truncate(true)
            .write(true)
            .open(json_output)
            .context("Opening output file")?;

        let output = if args.compact {
            fjson::to_json(&input_str).context("Formatting to json")
        } else {
            fjson::to_json_compact(&input_str).context("Formatting to json")
        }?;

        json_output_file
            .write_all(&output.as_bytes())
            .context("Writing json output")?;
        json_output_file.flush().context("Flushing json output")?;
    }

    Ok(())
}

// fn x() {
//     // Select recommended watcher for debouncer.
//     // Using a callback here, could also be a channel.
//     let mut debouncer =
//         new_debouncer(
//             Duration::from_millis(50),
//             |res: DebounceEventResult| match res {
//                 Ok(events) => events
//                     .iter()
//                     .for_each(|e| println!("Event {:?} for {:?}", e.kind, e.path)),
//                 Err(e) => println!("Error {:?}", e),
//             },
//         )
//         .unwrap();

//     // Add a path to be watched. All files and directories at that path and
//     // below will be monitored for changes.
//     debouncer
//         .watcher()
//         .watch(Path::new("."), RecursiveMode::Recursive)
//         .unwrap();

//     // note that dropping the debouncer (as will happen here) also ends the debouncer
//     // thus this demo would need an endless loop to keep running
// }
