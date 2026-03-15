# pGenie

Type-safe PostgreSQL client code generator. This repository contains the source code for the `pgn` CLI tool.

📚 **Full documentation:** [pgenie.io/docs](https://pgenie.io/docs/)

## Quick Start

New to pGenie? The **[Learn pGenie in Y minutes](https://pgenie.io/docs/tutorials/learn-pgenie-in-y-minutes/)** tutorial gets you up and running fast.

Want to explore a working project right away? Check out the **[demo repository](https://github.com/pgenie-io/demo)** — a ready-to-run project you can clone and experiment with immediately.

## Installation

### From a Binary Distribution

Pre-built binaries are available for common platforms. See the **[Installation Guide](https://pgenie.io/docs/guides/installation/)** for instructions.

### Building from Source

#### Prerequisites

- **GHC 9.12.2** — The Glasgow Haskell Compiler. Install it with [GHCup](https://www.haskell.org/ghcup/): `ghcup install ghc 9.12.2`
- **Docker** — Must be installed and running. pGenie uses Docker to analyze SQL in a real PostgreSQL environment.

#### Using Stack

[Stack](https://docs.haskellstack.org/en/stable) is the quickest way to build and install pGenie from source:

```bash
git clone https://github.com/pgenie-io/pgenie.git
cd pgenie
stack install
```

Stack manages the GHC toolchain automatically. See the [Stack documentation](https://docs.haskellstack.org/en/stable) for more details.

#### Using Cabal

[Cabal](https://www.haskell.org/cabal/) is the preferred option for Haskell developers already familiar with the ecosystem:

```bash
git clone https://github.com/pgenie-io/pgenie.git
cd pgenie
cabal update
cabal install
```

See the [Cabal documentation](https://cabal.readthedocs.io/en/stable/) for more details.

## Supported Artifacts

- [**hasql**](https://github.com/pgenie-io/haskell-hasql.gen) — Type-safe Haskell client library for [hasql](https://hackage.haskell.org/package/hasql)
