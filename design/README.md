
# Dependency structure of sublibs

```mermaid
graph TD

modeling --> catalog-pg-statements
modeling --> libpq-lawful-conversions

modeling --> base
catalog-pg-statements --> base
libpq-lawful-conversions --> base
```
