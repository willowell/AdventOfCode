use std::{path::PathBuf, fmt::format};

use clap::{Args, Parser, Subcommand, ValueEnum};
use reqwest;
use serde::{Serialize, Deserialize};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
#[command(propagate_version = true)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Fetch the input for a given day.
    Fetch(Fetch),

    /// Run one or more solution(s) on the actual input.
    Run(Run),

    /// Test one or more solution(s) using the challenge's example inputs.
    Test(Test),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum Part {
    A,
    B,
}

#[derive(Debug)]
struct Opts {
    /// Configuration file (YAML) to read session token from.
    config_file: Option<PathBuf>,

    /// Directory where input data is stored.
    input: Option<PathBuf>,
}

#[derive(Args, Debug)]
struct Fetch {
    /// Which day's challenge to fetch the input for.
    /// Omit to fetch all inputs.
    #[arg(value_parser = clap::value_parser!(u16).range(1..=25))]
    day: Option<u16>,

    /// Configuration file (YAML) to read session token from.
    config_file: Option<PathBuf>,

    /// Directory where input data is stored.
    input: Option<PathBuf>,
}

#[derive(Args, Debug)]
struct Run {
    /// Which day's challenge to fetch or run the solution for.
    /// Omit to run all solutions.
    #[arg(value_parser = clap::value_parser!(u16).range(1..=25))]
    day: Option<u16>,

    /// Which part of a challenge to run.
    /// Omit to run all parts.
    part: Option<Part>,

    /// Configuration file (YAML) to read session token from.
    #[arg(long = "cfg", name = "CONFIG_FILE")]
    config_file: Option<PathBuf>,

    /// Directory where input data is stored.
    #[arg(short = 'i', long = "input", name = "DIR")]
    input: Option<PathBuf>,

    /// Automatically submit the answer.
    submit: Option<bool>,
}


#[derive(Args, Debug)]
struct Test {
    /// Which day's challenge to fetch or run the solution for.
    /// Omit to run all solutions.
    #[arg(value_parser = clap::value_parser!(u16).range(1..=25))]
    day: Option<u16>,

    /// Which part of a challenge to run.
    /// Omit to run all parts.
    part: Option<Part>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct Cfg {
    #[serde(rename = "session-token")]
    session_token: String,
}

fn parse_cfg_file<P>(filename: P) -> Result<Cfg, Box<dyn std::error::Error>> where P: AsRef<std::path::Path> {
    let f = std::fs::File::open(filename)?;
    let d = serde_yaml::from_reader(f)?;

    Ok(d)
}

async fn fetch_input<P>(year: u16, config_file: P, day: u16) -> Result<(), Box<dyn std::error::Error>> where P: AsRef<std::path::Path> {
    println!("Fetching input for day {}", day);

    let cfg = parse_cfg_file(config_file);

    match cfg {
        Err(_) => {
            println!("Cant't fetch input: missing session token!");
            std::process::exit(1);
        },
        Ok(cfg) => {
            let client = reqwest::Client::new();

            let res = client.get()
                .header("Cookie", format!("session={}", cfg.session_token))
                .send()
                .await?
                .text()
                .await?;
        },
    }

    Ok(())
}

fn main() {
    let cli = Cli::parse();

    // You can check for the existence of subcommands, and if found use their
    // matches just as you would the top level cmd
    match &cli.command {
        Commands::Fetch(args) => {
            println!("'myapp fetch' was used, got `{:?}`", args)
        },
        Commands::Run(args) => {
            println!("'myapp solve' was used, got `{:?}`", args)
        },
        Commands::Test(args) => {
            println!("'myapp test' was used, got `{:?}`", args)
        }
    }
}
