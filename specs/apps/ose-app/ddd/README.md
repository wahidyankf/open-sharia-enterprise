# OSE Application DDD Artifacts

Domain-Driven Design artifacts for the `ose-app-be` bounded-context architecture.
These files are the machine-readable source of truth consumed by `rhino-cli ddd bc` and
`rhino-cli ddd ul`.

## Structure

```
specs/apps/ose-app/ddd/
├── README.md                  # This file
├── bounded-contexts.yaml      # Registry — 4 bounded contexts
├── bounded-context-map.md     # Visual bounded-context map with Mermaid diagrams
└── ubiquitous-language/       # Per-context glossaries (one .md per bounded context)
    ├── README.md              # Authoring rules and index
    └── *.md                   # One glossary file per bounded context
```

## Files

- **[bounded-contexts.yaml](./bounded-contexts.yaml)** — Declares every bounded context
- **[ubiquitous-language/](./ubiquitous-language/README.md)** — One Markdown glossary per bounded context

## Related

- [bounded-context-map.md](./bounded-context-map.md) — Visual bounded-context map
- [rhino-cli ddd commands](../../../../apps/rhino-cli/README.md#ddd-enforcement)
