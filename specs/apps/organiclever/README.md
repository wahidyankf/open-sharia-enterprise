# OrganicLever Application Specs

Platform-agnostic specifications for the OrganicLever fullstack application. The
application is rolling-release on `main` (Trunk Based Development): scope grows
incrementally rather than landing in numbered releases. Today the system ships a
marketing landing site, a system-status diagnostic page, and a `/api/v1/health` backend
endpoint — no authenticated screens, no remote sync. The application consists of an
F#/Giraffe backend REST API and a Next.js 16 frontend.

## Structure

```
specs/apps/organiclever/
├── README.md              # This file
├── product/               # Product framing (above C4)
│   └── README.md
├── system-context/        # C4 L1 — actors and external systems
│   ├── README.md
│   └── context.md
├── containers/            # C4 L2 — deployable units
│   ├── README.md
│   ├── container.md
│   └── contracts/         # OpenAPI 3.1 contract spec (consumed by codegen)
├── components/            # C4 L3 — per-container internals
│   ├── README.md
│   ├── be/                # F#/Giraffe backend component specs
│   │   ├── README.md
│   │   └── component-be.md
│   └── web/               # Next.js frontend component specs
│       ├── README.md
│       └── component-web.md
├── ddd/                   # DDD artifacts (platform-agnostic; shared by all surfaces)
│   ├── README.md
│   ├── bounded-contexts.yaml
│   ├── bounded-context-map.md
│   └── ubiquitous-language/
│       ├── README.md
│       └── *.md           # One glossary file per bounded context
└── behavior/              # Gherkin scenarios (HTTP-semantic + UI-semantic)
    ├── README.md
    ├── be/gherkin/        # Backend Gherkin scenarios
    └── web/gherkin/       # Frontend Gherkin scenarios (per bounded context)
```

## Containers

One row per deployable container (C4 L2). Container slug is canonical: it indexes
`components/<slug>/`, `behavior/<slug>/gherkin/`, the container README, and the Gherkin
glob. Adding a future container (e.g. `mobile`, `desktop`, a second backend) means adding
a row here, not changing the schema.

| Container | Perspective                             | Background                 | Scenarios                                                 | Domains                          | Consumed by                                   |
| --------- | --------------------------------------- | -------------------------- | --------------------------------------------------------- | -------------------------------- | --------------------------------------------- |
| `be`      | HTTP-semantic (GET, POST, status codes) | `Given the API is running` | [behavior/be/gherkin/](./behavior/be/gherkin/README.md)   | health                           | `apps/organiclever-be` (F#/Giraffe, TickSpec) |
| `web`     | UI-semantic (clicks, types, sees)       | `Given the app is running` | [behavior/web/gherkin/](./behavior/web/gherkin/README.md) | landing, system, layout, routing | `apps/organiclever-web` (Next.js 16)          |

The `web` container's system-status page consumes the `be` container's health endpoint.
Otherwise `web` is local-first today.

## Bounded Contexts

Counts are Gherkin features per container. `--` means no features in that container today.

| Bounded Context | `be` features | `web` features | Description                                                |
| --------------- | ------------- | -------------- | ---------------------------------------------------------- |
| app-shell       | --            | 2              | Navigation chrome, accessibility, entry-logging overlays   |
| health          | 1             | 1              | Service health status (`be` probe + `web` diagnostic page) |
| journal         | --            | 2              | Append-only event log — system of record (PGlite)          |
| landing         | --            | 1              | Marketing landing page                                     |
| routine         | --            | 1              | Workout routine management                                 |
| routing         | --            | 2              | App routing and disabled-route 404 guards                  |
| settings        | --            | 3              | User preferences — dark mode, language                     |
| stats           | --            | 2              | History and progress projections over journal events       |
| workout-session | --            | 1              | Active workout session FSM                                 |

## Spec Artifacts

- **[ddd/](./ddd/README.md)** — DDD artifacts:
  [bounded-contexts.yaml](./ddd/bounded-contexts.yaml) (registry) and
  [ubiquitous-language/](./ddd/ubiquitous-language/README.md) (glossaries);
  consumed by `rhino-cli ddd bc` and `rhino-cli ddd ul`
- **[system-context/](./system-context/README.md)**, **[containers/](./containers/README.md)**,
  **[components/](./components/README.md)** — C4 architecture diagrams (L1/L2/L3)
- **[components/be/](./components/be/README.md)** — Backend API component specs
  ([Gherkin features](./behavior/be/gherkin/README.md))
- **[components/web/](./components/web/README.md)** — Frontend component specs
  ([Gherkin features](./behavior/web/gherkin/README.md))

## DDD Registry (`bounded-contexts.yaml`)

`bounded-contexts.yaml` is the machine-readable declaration of every bounded context in
`organiclever-web`. Two `rhino-cli ddd` subcommands read it to enforce structural and
vocabulary invariants automatically in `nx run organiclever-web:test:quick`.

### Schema

Each entry under `contexts:` declares:

| Field           | What it means                                                                                                         |
| --------------- | --------------------------------------------------------------------------------------------------------------------- |
| `name`          | Identifier — must match the folder name under `src/contexts/`                                                         |
| `summary`       | One-paragraph human description                                                                                       |
| `layers`        | Ordered list of DDD layers that must exist as subfolders (e.g. `[domain, application, infrastructure, presentation]`) |
| `code`          | Filesystem path to the context's implementation root                                                                  |
| `glossary`      | Path to the context's ubiquitous-language Markdown file                                                               |
| `gherkin`       | Path to the context's Gherkin scenario directory                                                                      |
| `relationships` | List of inter-context relationships with `to`, `kind`, and `role`                                                     |

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
OSE_RHINO_DDD_SEVERITY=warn nx run organiclever-web:test:quick

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

## For Product / Project Managers

**Audience note**: This folder is written for engineers and SWE-background TPMs (the
kind embedded with a developer-tools or productivity team — not non-technical PMs). The
C4 diagrams and DDD-applied vocabulary (bounded context, ubiquitous language, aggregate)
will be familiar if you have worked with system diagrams and event-storming. If you are
new to DDD, ask an engineer to walk you through `bounded-context-map.md` first.

**Reading order**:

1. **[product/overview.md](./product/overview.md)** — Start here. Plain-language
   summary of what OrganicLever does, who it is for, what ships today, and what is deferred.
2. **[system-context/context.md](./system-context/context.md)** — Where OrganicLever
   sits in the broader technical landscape: actors, external systems, trust boundaries.
3. **[containers/container.md](./containers/container.md)** — The two deployable units:
   Next.js web app (Vercel) and F#/Giraffe backend (Kubernetes). How they connect.
   Also see [containers/deployment.md](./containers/deployment.md) for environments
   and Docker image details.
4. **[components/web/](./components/web/README.md)** — Frontend internals:
   bounded-context architecture, routes and screens, design system.
   [components/be/api.md](./components/be/api.md) covers the backend API surface.
5. **[behavior/](./behavior/README.md)** — What the system is supposed to do, expressed
   as Gherkin (Given-When-Then) acceptance criteria per bounded context. The same files
   drive automated tests.

**In plain language**:

- You log what you did (workout, reading, meal, focus). It remembers. You see it later.
- No account. No subscription. No data leaves your device.
- The streak badge is the only "game mechanic" today.

## Related

- [Three-Level Testing Standard](../../../governance/development/quality/three-level-testing-standard.md)
- [BDD Spec-Test Mapping](../../../governance/development/infra/bdd-spec-test-mapping.md)
- [BDD Standards](../../../docs/explanation/software-engineering/development/behavior-driven-development-bdd/README.md)
