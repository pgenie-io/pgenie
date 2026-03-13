# pGenie

Type-safe PostgreSQL client code generator.

## What it does

Checks and analyzes SQL migrations and queries and generates type-safe client SDKs.

## How it works

1. **Define your schema** — Write SQL migrations in `migrations/` directory
2. **Write your queries** — Create parameterized SQL queries in `queries/` directory  
3. **Configure project** — Specify artifacts to generate in `project1.pgn.yaml`
4. **Generate code** — Run `pgn generate` to produce type-safe client libraries

## Supported artifacts

- [**hasql**](https://github.com/pgenie-io/haskell-hasql.gen) — Type-safe Haskell client library

## Installation

### Prerequisites

- **Docker** — Must be installed and running, as pGenie uses Docker containers to analyze SQL queries in a real PostgreSQL environment.
- **Cabal** — The Haskell package manager. Install it by following the instructions on the [Cabal website](https://www.haskell.org/cabal/).

### Steps

1. Clone the repository: `git clone https://github.com/nikita-volkov/pgenie.git`

2. Navigate to the cloned repo and run `cabal install`

## Usage

Run `pgn generate` in a directory with `project1.pgn.yaml` to generate type-safe client libraries.

> [!NOTE]
> The first time you run pGenie it will take **2–3 minutes** while it performs initial setup:
>
> - Docker image caching
> - Code generator caching
>
> You may notice pGenie appears to hang on the "Loading" stage during this initial setup. This is normal!
>
> After the first run, subsequent executions will complete in **a few seconds**.

### Demo

For a complete example including the generated code see the [demo project](https://github.com/pgenie-io/demo).

### Query parameters

Use `$param_name` syntax in your SQL queries to define named parameters:

```sql
SELECT * FROM users WHERE id = $user_id AND name = $user_name
```

Parameters are automatically typed based on the database schema.
