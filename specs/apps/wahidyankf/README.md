# wahidyankf-web Application Specs

Platform-agnostic specifications for the wahidyankf-web personal portfolio site. The
application is a static Next.js 16 site deployed on Vercel at
[www.wahidyankf.com](https://www.wahidyankf.com). It presents a professional profile —
CV timeline, personal projects, and contact links — with a client-side search feature.
There is no backend API, no database, and no authentication.

## Structure

```
specs/apps/wahidyankf/
├── README.md              # This file
├── product/               # Product framing (above C4)
│   └── README.md
├── system-context/        # C4 L1 — actors and external systems
│   └── README.md
├── containers/            # C4 L2 — deployable units
│   └── README.md
├── components/            # C4 L3 — per-container internals
│   ├── README.md
│   └── web/               # Next.js frontend component specs
│       └── README.md
├── ddd/                   # DDD artifacts (platform-agnostic; shared by all surfaces)
│   ├── README.md
│   ├── bounded-contexts.yaml
│   ├── bounded-context-map.md
│   └── ubiquitous-language/
│       ├── README.md
│       └── *.md           # One glossary file per bounded context
└── behavior/              # Gherkin scenarios (UI-semantic only)
    ├── README.md
    └── web/gherkin/       # Frontend Gherkin scenarios (per bounded context)
```

## Containers

One container: the `web` Next.js app. No backend container exists today.

| Container | Perspective                       | Background                 | Scenarios                                                 | Consumed by                        |
| --------- | --------------------------------- | -------------------------- | --------------------------------------------------------- | ---------------------------------- |
| `web`     | UI-semantic (clicks, types, sees) | `Given the app is running` | [behavior/web/gherkin/](./behavior/web/gherkin/README.md) | `apps/wahidyankf-web` (Next.js 16) |

## Bounded Contexts

Counts are Gherkin features per container.

| Bounded Context     | `web` features | Description                                                              |
| ------------------- | -------------- | ------------------------------------------------------------------------ |
| `app-shell`         | 3              | Navigation chrome, theme toggle, responsive layout, accessibility wiring |
| `home`              | 1              | Landing page — intro hero, skills, contact links                         |
| `cv`                | 1              | `/cv` page — work history timeline, skills, education                    |
| `personal-projects` | 1              | `/personal-projects` page — project listing with tech-tag filters        |
| `search`            | 1              | Cross-area client-side search across all page content                    |

## Spec Artifacts

- **[ddd/](./ddd/README.md)** — DDD artifacts:
  [bounded-contexts.yaml](./ddd/bounded-contexts.yaml) (registry) and
  [ubiquitous-language/](./ddd/ubiquitous-language/README.md) (glossaries);
  consumed by `rhino-cli ddd bc` and `rhino-cli ddd ul`
- **[system-context/](./system-context/README.md)**, **[containers/](./containers/README.md)**,
  **[components/](./components/README.md)** — C4 architecture diagrams (L1/L2/L3)
- **[components/web/](./components/web/README.md)** — Frontend component specs
  ([Gherkin features](./behavior/web/gherkin/README.md))

## DDD Registry (`bounded-contexts.yaml`)

`bounded-contexts.yaml` is the machine-readable declaration of every bounded context in
`wahidyankf-web`. Two `rhino-cli ddd` subcommands read it to enforce structural and
vocabulary invariants automatically in `nx run wahidyankf-web:test:quick`.

### Schema

Each entry under `contexts:` declares:

| Field           | What it means                                                                                 |
| --------------- | --------------------------------------------------------------------------------------------- |
| `name`          | Identifier — must match the folder name under `src/contexts/`                                 |
| `summary`       | One-paragraph human description                                                               |
| `layers`        | Ordered list of DDD layers that must exist as subfolders (e.g. `[application, presentation]`) |
| `code`          | Filesystem path to the context's implementation root                                          |
| `glossary`      | Path to the context's ubiquitous-language Markdown file                                       |
| `gherkin`       | Path to the context's Gherkin scenario directory                                              |
| `relationships` | List of inter-context relationships with `to`, `kind`, and `role`                             |

Relationship `kind` values: `customer-supplier`, `conformist`, `shared-kernel`.

### `rhino-cli ddd bc wahidyankf` — structural parity

Reads the registry and verifies the **filesystem** matches exactly:

- Every declared `code:` path exists with **exactly** the declared `layers:` subfolders
- Every declared `glossary:` file exists on disk
- Every declared `gherkin:` directory exists and contains ≥1 `.feature` file
- No orphan directories under `src/contexts/` outside the registry

### `rhino-cli ddd ul wahidyankf` — glossary parity

Reads the registry to locate every `glossary:` file, then validates each:

- Required frontmatter keys present (`Bounded context`, `Maintainer`, `Last reviewed`)
- Terms table header matches canonical columns
- Code identifiers exist somewhere in the declared `code:` path
- Feature file references resolve to real `.feature` files

## Related

- [Three-Level Testing Standard](../../../governance/development/quality/three-level-testing-standard.md)
- [BDD Standards](../../../docs/explanation/software-engineering/development/behavior-driven-development-bdd/README.md)
