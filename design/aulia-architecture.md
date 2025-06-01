# Abstract

```mermaid
flowchart TD

app --> integration
app --> ui
integration --> algebra
ui --> logic
logic --> algebra
```

- App: initializes integrations and supplies them to UI
- Integration: implementation of abstractions in algebra
- UI:
  - rest-api running unintegrated logic
  - CLI arguments parser triggerring unintegrated logic
- Logic:
  - Features implemented based on algebra
  - Business logic
- Layers may be skipped if they are not needed
- A component exposes any of these layers

# Specific

```mermaid
flowchart TD

app-integrations ---> app-algebra
app-logic --> app-algebra
app-logic --> v1-hasql-haskell-gen
app-logic --> v1-jdbc-java-gen
cli-ui --> app-logic
cli-app --> app-integrations
cli-app --> cli-ui
custom-gen-app --> gen-ui
app-algebra --> gen-algebra
v1-hasql-haskell-gen --> gen-algebra
v1-jdbc-java-gen --> gen-algebra
gen-ui --> gen-algebra
custom-gen-app --> custom-gen
custom-gen --> gen-algebra
```
