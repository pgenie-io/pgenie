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

app-logic --> app-algebra
app-integrations ---> app-algebra
app-integrations --> v1-hasql-haskell-gen
app-integrations --> v1-jdbc-java-gen
cli-ui --> app-logic
cli-app --> app-integrations
cli-app --> cli-ui
custom-gen-app --> gen-ui
app-algebra --> gen-algebra
v1-hasql-haskell-gen --> gen-algebra
v1-jdbc-java-gen --> gen-algebra
gen-ui ----> gen-algebra
custom-gen-app --> custom-gen
custom-gen --> gen-algebra
cli-ui -----> command-cli-ui-algebra

infrastructure(DDD Infrastructure) -..- app-integrations
application(DDD Application) -...- app-logic
domain(DDD Domain) -....- app-algebra

subgraph cli-ui
  generate-command
  analyse-command
end

subgraph app-integrations
  temp-db
  file-system
end
```

- Specific gens are under integrations because the Effects get executed there and currently we've abstracted the gens in the app-algebra via MonadReader providing a list of gens
