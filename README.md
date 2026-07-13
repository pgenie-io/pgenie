# pGenie

pGenie turns PostgreSQL migrations and parameterized queries into fully typed client code.

You write plain SQL. pGenie validates it against a real PostgreSQL instance and generates idiomatic, type-safe client libraries for your application.

This repository contains the source code for the `pgn` CLI tool.

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

> **Security note:** Credentials in command-line arguments may appear in shell history and process listings (e.g., `ps aux`). For production or shared environments, prefer using a [PostgreSQL connection service file](https://www.postgresql.org/docs/current/libpq-pgservice.html) (`~/.pg_service.conf`) or the `PGPASSFILE` / `PGPASSWORD` environment variables so that the URL itself contains no secrets.

### Requirements

- **CREATEDB privilege** — pGenie creates a temporary database on the server for each run and drops it afterwards. The connecting user must hold the `CREATEDB` privilege (or be a superuser).
- **Version match** — The connected server's major version must match the `postgres` field in your project file (default: 18). If they differ, pGenie reports an error such as:

  ```
  PostgreSQL server version 16 does not match the project target version 18
  ```

  Fix this by either updating `postgres:` in `project1.pgn.yaml` to match your running server, or connecting to a server with the correct major version.

## Reusing a Docker Container Across Runs

By default, each Docker-mode run starts a fresh PostgreSQL container and throws it away afterwards, which means paying the container's startup cost every time. Pass `--reuse-container` to keep the container running between invocations instead:

```bash
pgn --reuse-container analyse
pgn --reuse-container generate
```

The first `--reuse-container` run starts a container as usual. Subsequent `--reuse-container` runs (with the same project's PostgreSQL image tag) find and reuse that same still-running container rather than starting a new one, cutting cold-start time out of the loop. Each run still gets its own throwaway database inside the container, so runs don't interfere with each other's schema objects.

This flag is only meaningful in Docker mode; it has no effect together with `--database-url`.

Reused containers are **never automatically cleaned up** — this mirrors Testcontainers-Java's own reuse feature, which is explicitly not intended for CI. Find and remove them manually when you're done:

```bash
docker ps --filter label=org.testcontainers.hs.reuse=true
docker rm -f <container-id>
```
