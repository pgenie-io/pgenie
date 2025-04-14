
# Dependency structure of sublibs

```mermaid
graph TD

modelling --> catalog-pg-statements
modelling --> libpq-lawful-conversions

modelling --> base
catalog-pg-statements --> base
libpq-lawful-conversions --> base
```
