# pgenie

CLI that analyses a PostgreSQL project (migrations + queries) against a simulation database and generates client-code artifacts via pluggable generators.

## Language

**Project File**:
The project's configuration file declaring space, name, version, and artifacts.
_Avoid_: ProjectModel, project config

**Migration**:
A numbered `N.sql` file under `migrations/` applied in order to build the schema.

**SQL Template**:
A query file under `queries/` with `$arg`/`:arg` parameter placeholders, rendered to native SQL for analysis.
_Avoid_: SqlTemplates (plural)

**Signature File**:
A per-query YAML file recording the query's parameter and result signature; reconciled against the inferred signature on each analysis.
_Avoid_: QuerySignatures

**Custom Type Signature File**:
The Signature File counterpart for user-defined Postgres types.
_Avoid_: CustomTypeSignatures

**Field Signature**:
The per-field type/nullability encoding (type name, not_null, dims, element_not_null) shared by Signature Files and Custom Type Signature Files, including its YAML form and reconciliation rules.
_Avoid_: FieldSig/CompositeFieldSig (as concept names), SignatureField

**Primitive**:
A built-in Postgres scalar type known to pgenie, carrying its OID and YAML type name; canonically `Gen.Input.Primitive` per ADR 0003.
_Avoid_: PostgresType, TypeVocabulary, Sessions-level Primitive duplicates

**Query Analysis**:
Inference of a query's parameter and result-column types by describing it against the simulation database.

**Syntax Analyser**:
AST-level analysis of a query determining result-row cardinality and whether it affects rows.

**Seq Scan Detector**:
Detection of sequential scans in a query, from `EXPLAIN` output or a SQL-text heuristic fallback, producing Seq Scan Findings.
_Avoid_: SeqScanExplain, SeqScanFinding (as a module name)

**Index Optimizer**:
Computes index create/drop actions from the index catalog and observed query needs, and renders them as a Migration.
_Avoid_: IndexOptimization, IndexCatalog, ManageIndexes (for the algebra; ManageIndexes is the workflow)

**Simulation Database**:
The throwaway Postgres instance that migrations are executed against and queries are described, explained, and index-inspected on during analysis.
_Avoid_: target database, analyser device

**Container**:
The Docker-hosted Postgres server process a Simulation Database runs inside. Normally torn down with its Simulation Database at scope exit; with `--reuse-container` it outlives the run and is left for manual cleanup.
_Avoid_: conflating with Simulation Database — the container may persist across runs, the database inside it never does.

**Generator Runtime**:
Loads generators (by URL or path, integrity-checked via the freeze file) and runs them over the analysed project to produce Artifacts.
_Avoid_: Gen loading, GeneratorHashes (the freeze file is an implementation detail)

**Artifact**:
One generated code package, configured in the Project File and written under `artifacts/`.

**Name**:
A normalized identifier of lowercase word parts, convertible to snake/camel/pascal/kebab/scream casings.
_Avoid_: Naming (as a module name)

**Report**:
A problem report (warning or error) with path, message, optional suggestion, and details.

**Staging**:
Progress reporting: nested named stages whose exits advance a progress fraction.

## Workflows

**Analyse**, **Generate**, **ManageIndexes**:
The orchestration workflows behind the CLI commands of the same names. **AnalyseProject** is the shared core they all run first.
