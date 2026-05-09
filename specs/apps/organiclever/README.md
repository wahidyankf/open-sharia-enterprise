# OrganicLever Application Specs

Platform-agnostic specifications for the OrganicLever fullstack application. v0 ships a
marketing landing site, a system-status diagnostic page, and a `/api/v1/health` backend
endpoint — no authenticated screens, no remote sync. The application consists of an
F#/Giraffe backend REST API and a Next.js 16 frontend.

## Structure

```
specs/apps/organiclever/
├── README.md              # This file
├── ddd/                   # DDD artifacts (registry + glossaries) consumed by rhino-cli ddd
│   ├── README.md
│   ├── bounded-contexts.yaml  # Registry — 9 bounded contexts with layers, paths, relationships
│   └── ubiquitous-language/   # Per-bounded-context glossary (shared by FE + future BE)
│       ├── README.md          # Index + authoring rules
│       └── *.md               # One glossary file per bounded context
├── c4/                    # Unified C4 architecture diagrams
│   ├── README.md          # Diagram index
│   ├── context.md         # L1 — system context (2 actors)
│   ├── container.md       # L2 — containers (FE, BE)
│   ├── component-be.md    # L3 — F#/Giraffe REST API internals
│   └── component-web.md    # L3 — Next.js frontend internals
├── be/                    # Backend specs (HTTP-semantic)
│   ├── README.md
│   └── gherkin/           # Backend Gherkin scenarios (see be/gherkin/README)
└── web/                   # Frontend specs (UI-semantic)
    ├── README.md
    └── gherkin/           # Frontend Gherkin scenarios (see web/gherkin/README)
```

## Backend vs Frontend

| Aspect      | Backend (be/)                                 | Frontend (fe/)                            |
| ----------- | --------------------------------------------- | ----------------------------------------- |
| Perspective | HTTP-semantic (GET, POST, status codes)       | UI-semantic (clicks, types, sees)         |
| Background  | `Given the API is running`                    | `Given the app is running`                |
| Scenarios   | See [be/gherkin/](./be/gherkin/README.md)     | See [web/gherkin/](./web/gherkin/README.md) |
| Domains     | health                                        | landing, system, layout, routing          |
| Consumed by | `apps/organiclever-be` (F#/Giraffe, TickSpec) | `apps/organiclever-web` (Next.js 16)      |

The frontend's system-status page consumes the backend's health endpoint. Otherwise the v0
frontend is local-first.

## Bounded Contexts

| Bounded Context | BE Features | FE Features | Description                                              |
| --------------- | ----------- | ----------- | -------------------------------------------------------- |
| app-shell       | --          | 2           | Navigation chrome, accessibility, entry-logging overlays |
| health          | 1           | 1           | Service health status (BE probe + FE diagnostic page)    |
| journal         | --          | 2           | Append-only event log — system of record (PGlite)        |
| landing         | --          | 1           | Marketing landing page                                   |
| routine         | --          | 1           | Workout routine management                               |
| routing         | --          | 2           | App routing and disabled-route 404 guards                |
| settings        | --          | 3           | User preferences — dark mode, language                   |
| stats           | --          | 2           | History and progress projections over journal events     |
| workout-session | --          | 1           | Active workout session FSM                               |

## Spec Artifacts

- **[ddd/](./ddd/README.md)** — DDD artifacts: [bounded-contexts.yaml](./ddd/bounded-contexts.yaml) (registry) and [ubiquitous-language/](./ddd/ubiquitous-language/README.md) (glossaries); consumed by `rhino-cli ddd bc` and `rhino-cli ddd ul`
- **[c4/](./c4/README.md)** — C4 architecture diagrams (context, container, 2 component)
- **[be/](./be/README.md)** — Backend API specs ([Gherkin features](./be/gherkin/README.md))
- **[web/](./web/README.md)** — Frontend app specs ([Gherkin features](./web/gherkin/README.md))

## DDD Registry (`bounded-contexts.yaml`)

`bounded-contexts.yaml` is the machine-readable declaration of every bounded context in
`organiclever-web`. Two `rhino-cli ddd` subcommands read it to enforce structural and
vocabulary invariants automatically in `nx run organiclever-web:test:quick`.

### Schema

Each entry under `contexts:` declares:

| Field           | What it means                                                                 |
| --------------- | ----------------------------------------------------------------------------- |
| `name`          | Identifier — must match the folder name under `src/contexts/`                 |
| `summary`       | One-paragraph human description                                               |
| `layers`        | Ordered list of DDD layers that must exist as subfolders (e.g. `[domain, application, infrastructure, presentation]`) |
| `code`          | Filesystem path to the context's implementation root                          |
| `glossary`      | Path to the context's ubiquitous-language Markdown file                       |
| `gherkin`       | Path to the context's Gherkin scenario directory                              |
| `relationships` | List of inter-context relationships with `to`, `kind`, and `role`             |

Relationship `kind` values: `customer-supplier`, `conformist`, `shared-kernel`.
For `customer-supplier` and `conformist`, both sides must declare the relationship
(symmetry enforced by `ddd bc`).

### `rhino-cli ddd bc organiclever` — structural parity

Reads the registry and verifies the **filesystem** matches exactly:

- Every declared `code:` path exists with **exactly** the declared `layers:` subfolders
  (extra or missing layer = error)
- Every declared `glossary:` file exists on disk
- Every declared `gherkin:` directory exists and contains ≥1 `.feature` file
- No **orphan** directories exist under `src/contexts/` that aren't in the registry
- Relationship declarations are symmetric across both context entries

### `rhino-cli ddd ul organiclever` — glossary parity

Reads the registry to locate every `glossary:` file, then validates each:

- Required frontmatter keys present (`Bounded context`, `Maintainer`, `Last reviewed`)
- Terms table header matches canonical columns
- Code identifiers (backtick-wrapped in the table) exist somewhere in the declared
  `code:` path — stale identifiers from renamed types or deleted functions are caught here
- Feature file references in the table resolve to real `.feature` files under the
  declared `gherkin:` path
- Same term in two glossaries → both must carry mutual `Forbidden-synonyms` cross-links

### Severity and escape hatch

Both commands default to `error` severity — a finding fails the build.

```bash
# Downgrade to warnings locally (never commit with this set)
ORGANICLEVER_RHINO_DDD_SEVERITY=warn nx run organiclever-web:test:quick

# Or per-command
rhino-cli ddd bc organiclever --severity=warn
rhino-cli ddd ul organiclever --severity=warn
```

### Adding a new bounded context

1. Add an entry to `bounded-contexts.yaml` with all six fields.
2. Create the code directory with the declared layer subfolders.
3. Create the glossary file at the declared path (use an existing one as a template).
4. Create the gherkin directory and add at least one `.feature` file.
5. Run `nx run organiclever-web:test:quick` — `ddd bc` and `ddd ul` will confirm
   the registry matches the filesystem before any unit tests run.

## Spec Consumption

All backends consume the backend Gherkin specs at **all three test levels**:

- **`test:unit`** — steps call service functions with mocked dependencies; Gherkin spec paths
  are included in Nx cache inputs so cache invalidates when specs change
- **`test:quick`** — unit + coverage check; Gherkin spec paths included in Nx cache inputs
- **`test:integration`** — steps call service functions with real PostgreSQL; cache disabled

## Related

- [Three-Level Testing Standard](../../../governance/development/quality/three-level-testing-standard.md)
- [BDD Spec-Test Mapping](../../../governance/development/infra/bdd-spec-test-mapping.md)
- [BDD Standards](../../../docs/explanation/software-engineering/development/behavior-driven-development-bdd/README.md)
