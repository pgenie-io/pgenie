# Dependency sources

Study the contents of the `cabal.project` file. In the `source-repository-package` sections you'll see references to repositories of dependencies that are not on Hackage. Load them into memory.

# Instructions

- Avoid polluting the top namespace with definitions that could be placed in `where` blocks. Alternatively introduce helper modules if the definitions are more general. Place the helper modules in a deeper namespace of the current module, e.g. `A.B.Helper` for `A.B`.
- Keep logic tests in the same hierarchy as the production code: use `Logic.spec` as the root test entry point, group domain specs under `Logic.Domain.spec`, and keep leaf specs next to the code they exercise.

# Changelog management

Accumulate the unreleased changes in the `Upcoming` section at the top of the `CHANGELOG.md` file. The release workflow automatically relabels the `Upcoming` section to the new version.

When implementing changes, describe every user-facing change in the changelog. 

**DO NOT** describe the minorities and chores like refactorings, tests and CI.

**DO NOT** describe changes to the sublibraries, instead focus on the executable's behaviour! 

Focus on the following categories:

- Non-breaking changes: New features, improvements and optimizations that do not break existing functionality.
- Fixes: Bug fixes and error handling improvements that do not break existing functionality.
- Breaking changes: Changes that break existing functionality, such as API changes, removed features, or changes in behavior. These should be clearly marked and described in detail to help users understand the impact of the change and how to adapt their code if necessary.
