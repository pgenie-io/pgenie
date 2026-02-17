# pGenie

Type-safe PostgreSQL client code generator.

## What it does

Checks and analyzes SQL migrations and queries and generates type-safe client SDKs.

## How it works

1. **Define your schema** — Write SQL migrations in `migrations/` directory
2. **Write your queries** — Create parameterized SQL queries in `queries/` directory  
3. **Configure project** — Specify artifacts to generate in `project.pgn1.yaml`
4. **Generate code** — Run `pgn generate` to produce type-safe client libraries

## Example project structure

```
my-project/
├── project.pgn1.yaml      # Project configuration
├── migrations/            # Schema definitions
│   ├── 1.sql
│   └── 2.sql
└── queries/               # SQL queries
    ├── select_user.sql
    └── insert_user.sql
```

For a complete example including the generated code see the [demo project](./demo/).

## Supported artifacts

- [**hasql**](https://github.com/pgenie-io/haskell-hasql.gen) — Type-safe Haskell client library

## Installation

1. Clone the repository: `git clone https://github.com/nikita-volkov/pgenie.git`

2. Navigate to the cloned repo and run `cabal install`

### Prerequisites

`pGenie` requires Docker to be installed and running on your machine, as it uses Docker containers to analyze SQL queries in a real PostgreSQL environment.

To install `pGenie` you need to install Cabal, which is the Haskell package manager. You can install it by following the instructions on the [Cabal website](https://www.haskell.org/cabal/).

## Usage

Run `pgn generate` in a directory with `project.pgn1.yaml` to generate type-safe client libraries.

### First Run Notice

**The first time you run pGenie it will take 2-3 minutes** while it performs initial setup:

- **Docker image caching** — Downloads and caches the PostgreSQL Docker image  
- **Code generator caching** — Downloads and caches the Dhall code for the configured generators

You may notice pGenie appears to "hang" on the "Caching/Loading" stage during this initial setup. This is normal!

**After the first run, subsequent executions will complete in just a few seconds.**
