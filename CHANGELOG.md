# Revision History for my Advent of Code Solutions

Please note that the dates are listed with most recent first.

## 2022-11-28

### AOC 2021

### General

* Added AGPLv3 license to whole project. Since Rust (and potentially GHC Haskell in the near future) can be compiled to WASM and therefore used on the web, this ensures the spirit of the GPLv3 is maintained when used on a website.

### Haskell

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