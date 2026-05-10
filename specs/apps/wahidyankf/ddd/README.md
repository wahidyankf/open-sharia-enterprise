# wahidyankf-web DDD Artifacts

Domain-Driven Design artifacts for the `wahidyankf-web` bounded-context architecture.
These files are the machine-readable source of truth consumed by `rhino-cli ddd bc` and
`rhino-cli ddd ul` during `nx run wahidyankf-web:test:quick`.

DDD artifacts live at the application root (`specs/apps/wahidyankf/ddd/`), not under
`components/web/`, because the ubiquitous language belongs to the bounded context — not to
one implementation surface.

## Structure

```
specs/apps/wahidyankf/ddd/
├── README.md                  # This file
├── bounded-contexts.yaml      # Registry — 5 bounded contexts with layers, paths, relationships
├── bounded-context-map.md     # Visual bounded-context map with Mermaid diagram
└── ubiquitous-language/       # Per-context glossaries (one .md per bounded context)
    ├── README.md              # Authoring rules and index
    ├── app-shell.md
    ├── cv.md
    ├── home.md
    ├── personal-projects.md
    └── search.md
```

## Files

- **[bounded-contexts.yaml](./bounded-contexts.yaml)** — Declares every bounded context:
  layer subfolders, code path, glossary path, gherkin path, and inter-context relationships.
  Read by `rhino-cli ddd bc` to validate structural parity against the filesystem.

- **[bounded-context-map.md](./bounded-context-map.md)** — Mermaid diagram of the five
  bounded contexts and their relationships.

- **[ubiquitous-language/](./ubiquitous-language/README.md)** — One Markdown glossary per
  bounded context. Each term entry maps a domain concept to its code identifiers and feature
  file references. Read by `rhino-cli ddd ul` to validate vocabulary consistency.

## How enforcement works

See [specs/apps/wahidyankf/README.md § DDD Registry](../README.md#ddd-registry-bounded-contextsyaml)
for full details on what each command checks.

## Related

- [bounded-context-map.md](./bounded-context-map.md) — Visual map
- [rhino-cli ddd commands](../../../../apps/rhino-cli/README.md#ddd-enforcement)
