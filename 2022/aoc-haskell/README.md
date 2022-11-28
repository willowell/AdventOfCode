# Haskell Implementation

## How to setup this implementation

First, please install GHCup - https://www.haskell.org/ghcup/.

Then, install Cabal and a GHC 9.2.x or 9.4.x version, set it as your default.

Next, verify your GHC installation by running the REPL via `ghci`.

## Heads up: `cabal install` does not work like `npm install`!

If you are used to NPM, you may be tempted to use `cabal install` to install dependencies, but Cabal does not work this way - `cabal install` is more synonymous to `make install`.

When you run `cabal install`, Cabal will build the project and then copy/symlink the binary over to `~/.cabal/bin`.

## How to build this implementation



## Usage

`cabal run solve -- <ARGS>`

```
➜ cabal run solve -- fetch --help
Usage: solve fetch [--cfg CONFIG_FILE] [-i|--input DIR] DAY

  Fetch the input for a given day.

Available options:
  --cfg CONFIG_FILE        Configuration file (TOML) to read session token from.
                           (default: aoc.toml)
  -i,--input DIR           Directory where input data is stored.
                           (default: input)
  DAY                      Which day's challenge to fetch or run the solution
                           for. Omit to run all solutions.
  -h,--help                Show this help text
```

```
➜ cabal run solve -- test --help 
Usage: solve test [DAY] [PART]

  Test one or more solution(s) using the challenge's example inputs.

Available options:
  DAY                      Which day's challenge to fetch or run the solution
                           for. Omit to run all solutions.
  PART                     Which part of a challenge to run. Omit to run all
                           parts.
  -h,--help                Show this help text
```

```
➜ cabal run solve -- run --help 
Usage: solve run [--cfg CONFIG_FILE] [-i|--input DIR] [DAY] [PART] [--submit]

  Run one or more solution(s) on the actual input.

Available options:
  --cfg CONFIG_FILE        Configuration file (TOML) to read session token from.
                           (default: aoc.toml)
  -i,--input DIR           Directory where input data is stored.
                           (default: input)
  DAY                      Which day's challenge to fetch or run the solution
                           for. Omit to run all solutions.
  PART                     Which part of a challenge to run. Omit to run all
                           parts.
  --submit                 Automatically submit the answer.
  -h,--help                Show this help text
```
