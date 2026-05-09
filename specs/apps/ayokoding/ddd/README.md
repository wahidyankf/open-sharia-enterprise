# AyoKoding DDD Artifacts

Domain-Driven Design artifacts for the `ayokoding-web` bounded-context architecture.
These files are the machine-readable source of truth consumed by `rhino-cli ddd bc` and
`rhino-cli ddd ul` during `nx run ayokoding-web:test:quick`.

DDD artifacts live at the application root (`specs/apps/ayokoding/ddd/`), not under
`components/web/`, because the ubiquitous language belongs to the bounded context — not to
one implementation surface. AyoKoding has only one container (`web`) today; the registry
shape supports adding more code paths per BC if/when other surfaces (e.g. a separate API
container) materialize, without a folder rename.

## Structure

```
specs/apps/ayokoding/ddd/
├── README.md                  # This file
├── bounded-contexts.yaml      # Registry — 6 bounded contexts with layers, paths, relationships
├── bounded-context-map.md     # Visual bounded-context map with Mermaid diagrams
└── ubiquitous-language/       # Per-context glossaries (one .md per bounded context)
    ├── README.md              # Authoring rules and index
    └── *.md                   # One glossary file per bounded context
```

## Files

- **[bounded-contexts.yaml](./bounded-contexts.yaml)** — Declares every bounded context: layer
  subfolders, code path under `apps/ayokoding-web/src/contexts/<bc>/`, glossary path, gherkin
  path, and inter-context relationships. Read by `rhino-cli ddd bc` to validate structural
  parity against the filesystem.

- **[bounded-context-map.md](./bounded-context-map.md)** — Visual bounded-context map with
  Mermaid diagram, strategic relationships table, and the multi-perspective gherkin
  workaround documentation.

- **[ubiquitous-language/](./ubiquitous-language/README.md)** — One Markdown glossary per
  bounded context. Each term entry maps a domain concept to its code identifiers and feature
  file references. Read by `rhino-cli ddd ul` to validate vocabulary consistency.

## How enforcement works

See [specs/apps/ayokoding/README.md § DDD Registry](../README.md#ddd-registry-bounded-contextsyaml)
for full details on what each command checks.

## Related

- [bounded-context-map.md](./bounded-context-map.md) — Visual bounded-context map with Mermaid
- [rhino-cli ddd commands](../../../../apps/rhino-cli/README.md)
