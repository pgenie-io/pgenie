# v0.4.0

## Non-breaking changes

- Add support for query `idempotent` flag in signature files. It will be transferred to the code gens and may be used for automatic retries in the generated code.

## Fixes

- A bug in analysis fixed that caused the app to fail with "unsupported type" for many types that were actually supported.

# v0.3.0

## Non-breaking changes

- Add support for the "ltree" type.

# v0.2.1

## Fixes

- Make the app exit with non-zero code when an error occurs

# v0.2.0

- Project file extended with a `postgres: [integer]` setting , which specifies the major PostgreSQL version to use for SQL analysis. When unspecified pGenie defaults to `18`.
