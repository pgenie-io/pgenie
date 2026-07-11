# Coding standards

This project follows [nikita-volkov/haskell-coding-standards](https://github.com/nikita-volkov/haskell-coding-standards) (branch `master`) for all Haskell code. That repo is the source of truth; nothing here is a substitute for it.

## Pinned revision

`.haskell-coding-standards.lock` at the repo root records the SHA of `haskell-coding-standards` this codebase was last audited against, e.g.:

```
<SHA>  # verified <date>
```

When the pin is stale (differs from the current `master`), the codebase should be re-audited against what changed before the lockfile is bumped.

## Reference

Base URL: `https://raw.githubusercontent.com/nikita-volkov/haskell-coding-standards/master/`

| Topic | Path |
|---|---|
| Index & glossary | `README.md` |
| Imports | `conventions/imports.md` |
| Exports | `conventions/exports.md` |
| Naming | `conventions/naming.md` |
| Errors | `conventions/errors.md` |
| Deriving | `conventions/deriving.md` |
| Formatting | `conventions/formatting.md` |
| Documentation | `conventions/documentation.md` |
| Language Extensions | `conventions/language-extensions.md` |
| Aggregator Namespace | `patterns/aggregator-namespace.md` |
| Execution Capability | `patterns/execution-capability.md` |
| Port | `patterns/port.md` |
| Domain Preludes | `patterns/preludes.md` |
| Logic Layer architecture | `architecture/logic-layer.md` |

## For agents

Use the `haskell-coding-standards` skill rather than relying on this file from memory: it fetches the live documents above and includes the version-pinning workflow. This file exists mainly so tools that expect a repo-root standards document (e.g. code review) have a starting pointer.
