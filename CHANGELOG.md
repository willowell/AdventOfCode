# Revision History for my Advent of Code Solutions

Please note that the dates are listed with most recent first.

## 2022-12-02

### AOC 2022

#### Haskell

* Implemented solution for Day 2.
* TBD - will fill this out more later.

### AOC 2020

#### Haskell

* Started migrating my old solutions to this new template.

At the time, I implemented my AOC 2020 solutions using Stack and without any parser combinators, as I was not familiar with using Cabal on its own and with using `Control.Monad.Combinators` or `Megaparsec` at the time.

Instead, I used Regex, and as you can imagine, my solutions turned out really messy on the passport validation and luggage puzzles.

I plan on revisiting these solutions and migrating them to using parser combinators, minding "parse, don't validate" along the way.

## 2022-12-01

### AOC 2022

It's show time!

#### Haskell

* Implemented solution for Day 1.
* Adjusted doc comment for `Solution` in `Solution.hs`.
* Scaffolded empty solution files for days 2 through 7, including a template "Day 0".
* Alphabetized `build-depends` field in `aoc.cabal` and added comments and links for the dependencies.

## 2022-11-30

### AOC 2022

#### Rust

* Scaffolded the Rust implementation using the Haskell implementation as a guide. The Rust implementation is not ready for Advent of Code yet, but it *could* be soon.

## 2022-11-28

### AOC 2021

### General

* Added AGPLv3 license to whole project. Since Rust (and potentially GHC Haskell in the near future) can be compiled to WASM and therefore used on the web, this ensures the spirit of the GPLv3 is maintained when used on a website.

#### Haskell

* Copied Haskell implementation from https://github.com/Solonarv/adventofcode-2021
* Replaced GPLv3 license with AGPLv3 license.
* Renamed instances of `aocYYYY` to just `aoc`, as the year is hoisted up in the directory structure.

#### Cabal file changes

* Updated `author` field in `aoc.cabal` (previously `aoc2021.cabal`) to include Solonarv's GitHub profile.
* Likewise updated the `maintainer` field in `aoc.cabal` with my email address.
* Likewise updated the `homepage` field in `aoc.cabal` with this repository's `README.md`.
* Added the `tested-with` field to share which GHC versions this has been tested against.
* Reformatted the file to my taste.

#### Code changes

* Upgraded project to support GHC 9.2.5 on macOS Ventura with Aarch64.
* Replaced TOML with YAML due to a an unsatisfiable dependency constraint: htoml does not support Aeson 2.0, but Aeson 1.x does not support GHC 9.
* Adjusted code in `harness/AOC/Harness.hs` accordingly.
* Added `hie.yaml`, to aid compatibility with HLS. Might not be needed?

### AOC 2022

### Haskell

* Initialized Haskell implementation from AOC 2021 directory.
* Removed all solutions carried over from 2021 implementation; added dummy solution for Day 1 to test setup.
* Added some comments to `Solution` type.

### Rust

* Initialized Rust implementation.
