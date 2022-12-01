use std::path::PathBuf;

use clap::{Args, Parser, Subcommand, ValueEnum};

use reqwest::{self, Url};
use serde::{Serialize, Deserialize};

use aoc_rust::{
    Solution,
    solutions::{
        day01::Day01,
    },
};


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

fn parse_cfg_file<P>(
    filename: P
) -> Result<Cfg, Box<dyn std::error::Error>>
where
    P: AsRef<std::path::Path>
{
    let f = std::fs::File::open(filename)?;
    let d = serde_yaml::from_reader(f)?;

    Ok(d)
}

async fn fetch_input<P>(
    year: u16,
    config_file: Option<P>,
    day: Option<u16>,
) -> Result<(), Box<dyn std::error::Error>>
where
    P: AsRef<std::path::Path>
{
    println!("Fetching input for day {:?}", day);


    let cfg = config_file.map(|f| parse_cfg_file(f)).transpose();

    match cfg {
        Err(_) | Ok(None) => {
            println!("Cant't fetch input: missing session token!");
            std::process::exit(1);
        },
        Ok(Some(cfg)) => {
            let client = reqwest::Client::new();

            match day {
                Some(day) => {
                    let mut url = Url::parse("https://adventofcode.com/")?;
                    url.set_path(&format!("{year}/day/{day}/input"));
        
                    println!("QUERYING FROM {}", url.as_str());

                    // let res = client.get(url)
                    //     .header("Cookie", format!("session={}", cfg.session_token))
                    //     .send()
                    //     .await?
                    //     .text()
                    //     .await?;

                    
                },
                None => {
                    println!("TODO: QUERY ALL DAYS IN YEAR");
                }
            }
        },
    }

    Ok(())
}

fn write_input_file<P>(
    input_dir: P,
    day: u16,
) -> std::io::Result<()>
where
    P: AsRef<std::path::Path>
{
    let out_file = format!("{}/day{}.txt", input_dir.as_ref().display(), day);

    println!("{out_file}");

    Ok(())
}

async fn run(year: u16) -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();

    // You can check for the existence of subcommands, and if found use their
    // matches just as you would the top level cmd
    match &cli.command {
        Commands::Fetch(args) => {
            println!("'myapp fetch' was used, got `{:?}`", args);

            fetch_input(year, args.config_file.as_ref(), args.day).await?;

            write_input_file(args.input.as_ref().unwrap(), args.day.unwrap());
        },
        Commands::Run(args) => {
            println!("'myapp solve' was used, got `{:?}`", args)
        },
        Commands::Test(args) => {
            println!("'myapp test' was used, got `{:?}`", args)
        }
    }

    Ok(())
}


#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let year = 2021;

    let input = "foo";

    let solutions = [
        Day01,
    ];

    if let Ok((_unconsumed, res)) = Day01.decode_input(input) {
        let part_a = Day01.part_a(res);

        let part_b = Day01.part_b(res);

        println!("Part A: {:?}\nPart B: {:?}", part_a, part_b);
    }

    //run(year).await?;

    Ok(())
}
