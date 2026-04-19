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

- **Docker** *(optional)* — Required only when `--database-url` is **not** provided. pGenie can spin up a temporary PostgreSQL container automatically, but you can skip Docker entirely by pointing it at a running server with `--database-url`.
- **libpq** — Required for building the Haskell bindings to PostgreSQL. On Debian/Ubuntu, install with `sudo apt-get install libpq-dev`, on macOS with `brew install libpq`.

#### Using Stack

**Stack** is the quickest way to build and install pGenie from source.

1. [Install `Stack`](https://docs.haskellstack.org/en/stable)

2. Clone the repository and build:

```bash
git clone https://github.com/pgenie-io/pgenie.git
cd pgenie
stack install
```

#### Using Cabal

**Cabal** is the preferred option for Haskell developers already familiar with the ecosystem:

1. [Install `Cabal`](https://www.haskell.org/cabal/)

2. Clone the repository and build:

```bash
git clone https://github.com/pgenie-io/pgenie.git
cd pgenie
cabal update
cabal install
```

## Using a Running PostgreSQL Server

By default, `pgn` launches a temporary Docker container for each run. You can skip Docker entirely by supplying a PostgreSQL connection string with `--database-url`:

```bash
pgn --database-url "postgresql://user:password@localhost:5432/mydb" analyse
pgn --database-url "postgresql://user:password@localhost:5432/mydb" generate
pgn --database-url "host=localhost port=5432 user=myuser password=mypass dbname=mydb" manage-indexes
```

Both URI format (`postgresql://...`) and libpq keyword=value format are accepted.

### Requirements

- **CREATEDB privilege** — pGenie creates a temporary database on the server for each run and drops it afterwards. The connecting user must hold the `CREATEDB` privilege (or be a superuser).
- **Version match** — The connected server's major version must match the `postgres` field in your project file (default: 18). If they differ, pGenie reports an error such as:

  ```
  PostgreSQL server version 16 does not match the project target version 18
  ```

  Fix this by either updating `postgres:` in `project1.pgn.yaml` to match your running server, or connecting to a server with the correct major version.
