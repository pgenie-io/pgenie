# v0.6.4

## Fixes

- Remove an unactionable suggestion (#61)

# v0.6.3

## Fixes

- Fix false seq-scan findings when EXPLAIN succeeds with no seq scans (#57 and #58)

# v0.6.2

## Fixes

- Prevent garbled terminal output during generator loading upon Dhall warning emission.

# v0.6.1

## Non-breaking changes

- Update the bundled `pgenie-gen` SDK, including compatibility with the new query result classification model (`void`, `rows_affected`, and row-returning results).

# v0.6.0

## Breaking changes

- Migrate to new Dhall for performance optimizations. Older versions of generators will not be compatible.

## Non-breaking changes

- Update progress output so each completed stage prints its full breadcrumb ending with a green `Done`, and only the final completion line prints `Done!`.

# v0.5.1

## Non-breaking changes

- Add early termination with a clear error message for Windows users who try to use Docker mode, which is not supported yet. Windows users can still use the live Postgres mode by providing `--database-url` pointing to a running PostgreSQL server.

# v0.5.0

## Non-breaking changes

- Add `--database-url` support so `pgn` can target an existing PostgreSQL server instead of spawning Docker, including temporary database provisioning and cleanup for running-server mode.
- Add custom type signature files and the flow to generate and consume them.

# v0.4.1

## Non-breaking changes

- Make the `artifacts` field in the project file optional, defaulting to an empty list if not provided.

## Fixes

- Fix implicit table composite type resolution error
- Fix a bug in error path nesting that caused the paths to be reversed.

# v0.4.0

## Non-breaking changes

- Add support for query `idempotent` flag in signature files. It will be transferred to the code gens and may be used for automatic retries in the generated code.
- More precise cardinality analysis based on AST

## Fixes

- A bug in analysis fixed that caused the app to fail with "unsupported type" for many types that were actually supported.
- A bug in the name parser that caused names with adjacent letters and digits to cause error.

# v0.3.0

## Non-breaking changes

- Add support for the "ltree" type.

# v0.2.1

## Fixes

- Make the app exit with non-zero code when an error occurs

# v0.2.0

- Project file extended with a `postgres: [integer]` setting , which specifies the major PostgreSQL version to use for SQL analysis. When unspecified pGenie defaults to `18`.
