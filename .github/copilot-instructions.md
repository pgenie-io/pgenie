# Dependency sources

Study the contents of the `cabal.project` file. In the `source-repository-package` sections you'll see references to repositories of dependencies that are not on Hackage. Load them into memory.

# Instructions

- Avoid polluting the top namespace with definitions that could be placed in `where` blocks. Alternatively introduce helper modules if the definitions are more general. Place the helper modules in a deeper namespace of the current module, e.g. `A.B.Helper` for `A.B`.
