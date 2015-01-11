# phoneng README

## Overview

This phoneng program suite (short for: Phonetic English) shows how English
is pronounced and offers an alternative spelling system that is more
consistent and easier to learn and use than the traditional spelling.

TODO These programs aren't implemented yet.
The `pronounce` command shows how English texts are pronounced. The
`lytspel` command converts them into a simplified spelling.

The provided tools can also be used to implement your own spelling reform
proposals or to adapt the chosen respellings as needed.

## Installation

The phoneng program suite is written in
[Haskell](https://www.haskell.org/haskellwiki/Haskell). To build it from
source, you need the [Cabal](https://www.haskell.org/cabal/) build system.
If you use a Debian-based system, install the `cabal-install` package to
get it.

Afterwards clone this repository from GitHub and run the following commands
in the main directory:

    cabal configure && cabal build && cabal install

The compiled programs should now be in your path and ready to run.

## Usage

TODO document

## Defining own respelling schemes

TODO document
