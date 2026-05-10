---
title: "App README vs Specs Convention"
description: Defines what content lives in app/infra READMEs vs specs/, the C4-aware five-folder spec tree shape, and the PM-readability contract for specs/.
category: explanation
subcategory: conventions
status: "Pilot — initial issue"
tags:
  - conventions
  - readme
  - specs
  - spec-tree-shape
  - pm-readability
  - c4
created: 2026-05-09
---

# App README vs Specs Convention

App READMEs drift. They accumulate routes tables, architecture diagrams, bounded-context narratives, and API endpoint listings — content that describes what a system does rather than how to run it locally. This drift makes READMEs long, makes specifications hard to find, and forces engineers to maintain the same information in two places.

This convention draws a hard boundary. App and infra READMEs contain only dev-runtime content. Everything describing system behavior, architecture, contracts, or design intent lives in `specs/apps/<app-family>/` following a C4-aware five-folder tree. Both audiences — engineers and Technical Product/Project Managers — benefit from knowing exactly where to look.

## Principles Implemented/Respected

This convention implements the following core principles:

- **[Documentation First](../../principles/content/documentation-first.md)**: The split makes documentation a deliberate, maintained artifact rather than content that accumulates wherever it is written first. Specs are the canonical record of what the system does.

- **[Explicit Over Implicit](../../principles/software-engineering/explicit-over-implicit.md)**: The two-category mapping table eliminates judgment calls. Every piece of content belongs to exactly one category, with explicit rules for splitting mixed content.

- **[Simplicity Over Complexity](../../principles/general/simplicity-over-complexity.md)**: App READMEs become thin, focused entry points. Specs become complete, organized references. Neither has to carry both roles.

- **[Accessibility First](../../principles/content/accessibility-first.md)**: The PM-readability contract calibrates glossing precisely — niche terms get plain-language notes on first use; mainstream SWE vocabulary does not. Over-glossing is patronizing noise; under-glossing bars non-specialist readers.

- **[Progressive Disclosure](../../principles/content/progressive-disclosure.md)**: The app README carries a single pointer into `specs/`. The specs tree structures content by zoom level (C4 L1 → L2 → L3) so readers can drill as far as they need.

## Purpose

This convention governs THREE interrelated decisions:

1. **Content Split Rule** — which content belongs in an app/infra README (Category A) vs in `specs/apps/<app-family>/` (Category B).
2. **Spec Tree Shape** — what the canonical five-folder C4-aware tree looks like and how it varies by surface profile.
3. **PM-Readability Contract** — six rules every file under `specs/apps/` must satisfy so a SWE-background Technical Product/Project Manager can form a working mental model on first read.

A fourth rule covers adoption expectations for BDD, DDD, and API contracts.

The convention applies to all apps and infra directories in the monorepo. Its OrganicLever application is the reference pilot. Rollout to `ayokoding`, `oseplatform`, `wahidyankf`, and `rhino` follows the same rules.

## Scope

### What This Convention Covers

- Content placement decisions for app and infra `README.md` files
- The canonical five-folder spec tree (`product/`, `system-context/`, `containers/`, `components/`, `behavior/`) and per-surface variants
- PM-readability requirements for every file under `specs/apps/`
- BDD, DDD, and API contract adoption expectations by app type
- Cross-link requirements between app READMEs and their corresponding `specs/` trees
- Line-count caps for app and infra READMEs
- Migration path from flat-root spec trees to the C4-aware layout

### What This Convention Does NOT Cover

- Gherkin writing standards — see [Acceptance Criteria Convention](../../development/infra/acceptance-criteria.md)
- C4 diagram content and internal structure — see the C4 files within each app's spec tree
- OpenAPI authoring standards — see contract project documentation
- README writing quality (voice, scannability, engagement) — see [README Quality Convention](../writing/readme-quality.md)
- The canonical path pattern for Gherkin feature files within `behavior/` — see [Specs Directory Structure Convention](./specs-directory-structure.md)

## Standards

### Standard 1 — Content Split Rule (Category A vs Category B)

Each piece of content in an app or infra README belongs to exactly one of two categories. Apply these rules paragraph-by-paragraph when reviewing or trimming an existing README.

**Category A — Dev-runtime (stays in `apps/<app>/README.md` or `infra/*/README.md`)**

Content a developer needs to run, build, test, or lint THIS specific checkout on THEIR machine. It is intrinsically about the filesystem layout of the app folder and the Nx targets defined by its `project.json`.

| Content                                                    | Why it stays                                                                 |
| ---------------------------------------------------------- | ---------------------------------------------------------------------------- |
| One-paragraph "what is this" intro                         | Reader orientation                                                           |
| Status banner (pre-alpha, etc.)                            | Visible warning at app entry point                                           |
| Quick Start commands                                       | Setting up dev server is THIS-checkout-specific                              |
| Nx targets table (`nx dev`, `nx build`, `nx run X:test:Y`) | Targets are defined in `project.json` of THIS app                            |
| Environment variables consumed at runtime                  | Wire-level, depends on which env file the app reads                          |
| Project layout (top-level `src/`, `tests/`, configs)       | Filesystem of THIS checkout — top-level only, not per-context recursion      |
| Tech-stack version pinning                                 | "I'm running Node 24.13.1, Next.js 16, F# .NET 10" — version source-of-truth |
| One paragraph + link to `specs/` for behavior              | The pointer that completes the split                                         |

**Category B — Behavior, contract, or architecture (moves to `specs/apps/<app-family>/`)**

Content that describes WHAT the system does — what URLs it exposes, what user flows exist, what API endpoints, what bounded contexts, what design decisions, what integration points. This content is platform-agnostic and survives even if the app were rewritten in a different framework.

| Content                                                                 | Destination                                                    |
| ----------------------------------------------------------------------- | -------------------------------------------------------------- |
| Routes table (URLs the app serves)                                      | `specs/apps/<app-family>/components/web/routes-and-screens.md` |
| Screens table (user-visible pages)                                      | `specs/apps/<app-family>/components/web/routes-and-screens.md` |
| Entry-flow tables                                                       | `specs/apps/<app-family>/components/web/routes-and-screens.md` |
| Bounded-context project layout (full `src/contexts/<bc>/...` recursion) | `specs/apps/<app-family>/components/web/architecture.md`       |
| Layer rules (`domain` ← no imports, etc.)                               | `specs/apps/<app-family>/components/web/architecture.md`       |
| Dormant code listing                                                    | `specs/apps/<app-family>/components/web/architecture.md`       |
| Bounded-context map narrative + diagram                                 | `specs/apps/<app-family>/ddd/bounded-context-map.md`           |
| Design system palette / fonts / dark-mode / token import                | `specs/apps/<app-family>/components/web/design-system.md`      |
| Component variant catalog                                               | `specs/apps/<app-family>/components/web/design-system.md`      |
| API endpoints table                                                     | `specs/apps/<app-family>/components/be/api.md`                 |
| Backend architecture diagram (DI, project tree)                         | `specs/apps/<app-family>/components/be/api.md`                 |
| Backend testing strategy                                                | `specs/apps/<app-family>/components/be/api.md`                 |
| E2E architecture (bddgen pipeline, feature → spec → test flow)          | `specs/apps/<app-family>/behavior/{web,be}/gherkin/README.md`  |

**Applying the rule — three questions in order:**

1. Does this section answer "how do I run THIS checkout?" → Category A, keep.
2. Does this section answer "what does THIS app do (regardless of framework)?" → Category B, move.
3. Both? → Split. The "what" part moves; a one-line "see `specs/`..." stays in the README.

When a paragraph genuinely fits both categories, bias toward moving. The app README must stay thin.

### Standard 2 — Required and Forbidden Sections in App READMEs

**Required headings (Category A content):**

- `## Quick Start` — dev server commands
- `## Nx Targets` — table of all `nx run` targets for this app
- `## Environment Variables` — env vars consumed at runtime (omit if none)
- `## Project Layout` — top-level directory listing (NOT per-context recursion)
- `## Tech Stack` — versions pinned via Volta or toolchain files
- `## Behavior and Architecture` — one paragraph + link to `specs/apps/<app-family>/`

**Forbidden content in app READMEs:**

- Routes tables listing URL paths (belongs in `components/web/routes-and-screens.md`)
- API endpoint tables (belongs in `components/be/api.md`)
- Bounded-context maps or narrative descriptions (belongs in `ddd/`)
- Architecture diagrams showing internal structure deeper than one level (belongs in `components/`)
- Design system palettes, font specs, or component variant catalogs (belongs in `components/web/design-system.md`)
- Full `src/contexts/<bc>/...` directory recursion (belongs in architecture.md)

### Standard 3 — Line-Count Caps

| Location                               | Hard cap  |
| -------------------------------------- | --------- |
| `apps/<app>/README.md`                 | 120 lines |
| `infra/dev/<app>/README.md`            | 60 lines  |
| `infra/k8s/<app>/staging/README.md`    | 30 lines  |
| `infra/k8s/<app>/production/README.md` | 30 lines  |

These caps are enforced by `repo-rules-checker`. Exceeding a cap is a HIGH finding. The cap exists because a README that exceeds it almost certainly contains Category B content.

### Standard 4 — Spec Tree Shape (C4-Aware Five-Folder Layout)

New apps create a spec tree at `specs/apps/<app-family>/` following the canonical five-folder layout. Existing apps with flat-root trees (`be/`, `web/`, `cli/`, `c4/`, `contracts/` at the root) migrate to this layout per Standard 4.5 below.

#### Canonical layout

```
specs/apps/<app-family>/
├── README.md
├── product/
│   ├── README.md
│   └── overview.md
├── system-context/
│   ├── README.md
│   └── context.md
├── containers/
│   ├── README.md
│   ├── container.md
│   ├── contracts/          # OpenAPI specs (full-stack only)
│   │   ├── README.md
│   │   ├── openapi.yaml
│   │   ├── paths/
│   │   ├── schemas/
│   │   └── generated/
│   └── deployment.md
├── components/
│   ├── README.md
│   ├── be/                 # Full-stack only
│   │   ├── README.md
│   │   ├── component-be.md
│   │   └── api.md
│   └── web/                # Web and full-stack
│       ├── README.md
│       ├── component-web.md
│       ├── architecture.md
│       ├── design-system.md
│       ├── routes-and-screens.md
│       └── ddd/            # When DDD adopted
│           ├── README.md
│           ├── bounded-contexts.yaml
│           ├── bounded-context-map.md
│           └── ubiquitous-language/
│               ├── README.md
│               └── <bc>.md
└── behavior/
    ├── README.md
    ├── be/
    │   └── gherkin/        # Full-stack only
    │       ├── README.md
    │       └── <domain>/
    └── web/
        └── gherkin/
            ├── README.md
            └── <domain>/
```

#### Folder purposes

| Folder            | Reader question it answers                                         | Why top-level (not nested)                                                                                                           |
| ----------------- | ------------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------ |
| `product/`        | "What does this product do for the user? What is in this version?" | PM-first content. Not architecture (so not under `system-context/`). Not behavior (so not under `behavior/`). Deserves its own home. |
| `system-context/` | "What is the system boundary? Who/what interacts with it?"         | C4 L1 — the canonical system context level.                                                                                          |
| `containers/`     | "What runtime processes exist? What are their boundaries?"         | C4 L2 — naturally hosts API contracts and deployment topology.                                                                       |
| `components/`     | "What is inside each container?"                                   | C4 L3. Bounded contexts are components, so DDD lives here naturally.                                                                 |
| `behavior/`       | "Does the system actually do what the specs say?"                  | Gherkin tests behavior at every C4 level — orthogonal to zoom hierarchy. Forcing it under one C4 level would misrepresent its scope. |

#### Per-surface variant table

| Surface profile                   | Folders populated                                                                                                                            | Folders absent or empty                                 |
| --------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------- |
| Full-stack (e.g., `organiclever`) | All five top-level folders; `components/be/` + `components/web/` + `containers/contracts/`; `behavior/be/gherkin/` + `behavior/web/gherkin/` | None — full tree                                        |
| Web-only (e.g., `wahidyankf`)     | `product/`, `system-context/`, `containers/`, `components/web/`, `behavior/web/gherkin/`                                                     | `containers/contracts/` (no API), `components/be/`      |
| CLI-only (e.g., `rhino`)          | `product/`, `system-context/`, `containers/`, `components/cli/`, `behavior/cli/gherkin/`                                                     | `components/{be,web}/`, `containers/contracts/`         |
| Multi-CLI (e.g., `ayokoding`)     | Same as CLI-only, with `components/cli/` + `behavior/cli/gherkin/` alongside web layers if applicable                                        | Nothing additional omitted — same shape, more populated |

#### Creation rules

- New apps create only the folders they need. Do not pre-create empty `behavior/` if there are no Gherkin specs yet.
- Once a folder exists, it carries a `README.md` index pointing at its children.
- The order of folders in any README listing follows the canonical order: `product/`, `system-context/`, `containers/`, `components/`, `behavior/`.

#### Standard 4.5 — Migration path (flat-root to C4-aware)

For existing spec trees with a flat-root layout (`be/`, `web/`, `cli/`, `c4/`, `contracts/`):

1. Create the five top-level folders with placeholder `README.md` files (no content moves yet).
2. In one atomic commit: `git mv` all old subfolders to their new positions per the canonical layout. Update ALL path references in the same commit (rhino-cli path constants, Nx cache inputs, step definition files, governance cross-links).
3. Update `specs/apps/<app-family>/README.md` to reflect the new tree.
4. Verify with `rhino-cli specs validate-tree <app>` and `npm run lint:md`.

The commit that moves files and updates paths MUST be atomic — splitting them causes test failures between commits.

### Standard 5 — PM-Readability Contract for specs/

Every NEW or MOVED file under `specs/apps/` must be readable by a **SWE-background Technical Product/Project Manager** — the kind of TPM embedded with a developer-tools team who has shipped software and reads code fluently. The contract is calibrated to gloss only the genuinely niche. Over-glossing mainstream SWE vocabulary is patronizing noise.

**Terms that do NOT need glossing** (the SWE-background TPM already knows): TypeScript, JavaScript, Next.js, React, Node.js, Postgres, Docker, Kubernetes, REST, HTTP, JSON, YAML, OpenAPI, IndexedDB, FSM, finite state machine, CI, CD, CI/CD, ADR, Architecture Decision Record, build pipeline, lockfile, version pinning, Volta, npm, ESLint, Prettier, Mermaid, Playwright, Vercel, monorepo, Nx.

**Terms that DO need glossing on first use within each file** (genuinely niche to this product):

| Term                                                                     | Gloss to use on first occurrence                                                                                                     |
| ------------------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------ |
| DDD (when first introducing the OrganicLever-specific application of it) | Domain-Driven Design — here applied as one bounded context per UI screen domain                                                      |
| bounded context                                                          | a self-contained slice of the app with its own vocabulary, types, and rules; contexts communicate only through narrow published APIs |
| aggregate                                                                | a cluster of domain objects treated as one consistent unit by writes                                                                 |
| ubiquitous language                                                      | the shared vocabulary used by both the team and the code for one bounded context                                                     |
| PGlite                                                                   | Postgres-WASM — Postgres compiled to WebAssembly running directly in the browser, persisted via IndexedDB                            |
| XState                                                                   | a JavaScript/TypeScript state-machine library used here for UI flow orchestration                                                    |
| Effect TS                                                                | a TypeScript library for typed effect composition, used in the infrastructure layer                                                  |
| F#                                                                       | functional .NET language used for the OrganicLever backend                                                                           |
| Giraffe                                                                  | F# web framework on top of ASP.NET Core, used for the OrganicLever HTTP API                                                          |

#### Rule 1 — Required header block

Every spec file starts with this block (first 10 lines after H1):

```markdown
# <Title>

> **Audience**: Engineers, Technical Product/Project Managers
>
> **Plain-language summary**: <one paragraph free of jargon for the niche
> stack choices and DDD-applied vocabulary; mainstream SWE vocabulary is fine.
> A SWE-background TPM should be able to form a working mental model on first read.>

## <First section heading>
```

#### Rule 2 — Intent before mechanism

Every section leads with what the feature or component enables for the user (1-2 sentences) before describing how the code is shaped. A SWE-background TPM should be able to read the first paragraph of any section and walk away knowing the user-facing point.

```markdown
<!-- FAIL: opens with mechanism -->

## Journal Context

The journal context owns the `JournalEvent` aggregate and exposes `appendEvent` use-cases via PGlite store.

<!-- PASS: opens with intent, mechanism follows -->

## Journal Context

The journal records every life-event the user logs. It is the system of record — every other feature reads from or writes to here.

Under the hood the context owns the `JournalEvent` aggregate (Domain-Driven Design — a cluster of domain objects treated as one consistent unit by writes) and exposes three use-cases backed by a PGlite (Postgres-WASM — Postgres compiled to WebAssembly running directly in the browser) store.
```

#### Rule 3 — Glossary on first use, scoped narrowly

The first occurrence of each niche project-specific term in a file carries a parenthetical gloss. Subsequent uses in the same file are gloss-free. Only the terms listed in the table above need glossing — anything not on that list is mainstream and must NOT be glossed.

#### Rule 4 — Tables over prose where possible

Routes, screens, endpoints, environment variables, and feature lists are presented as tables. SWE-background TPMs scan tables faster than prose.

#### Rule 5 — Code blocks are introduced

Every code or Mermaid block is preceded by a one-sentence "what this shows" introduction. The intro lets readers decide whether to read the block.

#### Rule 6 — Link forward to engineering depth

When a section requires hands-on engineering depth (e.g., DDD layer rules with ESLint boundary enforcement, Effect Layer composition), the section opens with a one-line TPM-skim cue and links to a deeper subsection or external doc.

### Standard 6 — BDD/DDD/Contracts Adoption (FR-10)

This standard defines adoption expectations per app type and rollout timeline. "SHOULD" means the convention recommends adoption; not adopting is a MEDIUM finding that requires explicit justification. "REQUIRED" means the convention mandates adoption; not adopting is a HIGH finding.

#### Adoption matrix by surface profile

| Surface profile | BDD (Gherkin specs) | DDD (bounded contexts, BC map, ubiquitous language) | API Contracts (OpenAPI) |
| --------------- | ------------------- | --------------------------------------------------- | ----------------------- |
| Full-stack app  | REQUIRED            | SHOULD                                              | REQUIRED                |
| Web-only app    | REQUIRED            | SHOULD                                              | NOT APPLICABLE          |
| CLI app         | REQUIRED            | DEFERRED — see note                                 | NOT APPLICABLE          |
| Multi-CLI       | REQUIRED            | DEFERRED — see note                                 | NOT APPLICABLE          |

**CLI DDD note**: CLI apps defer DDD adoption. CLI commands are independent operations without the bounded-context groupings or aggregate-shaped state that DDD vocabulary addresses. Revisit if a CLI grows past approximately 10 commands or shows aggregate-shaped state (e.g., workspace state, multi-step session management).

#### Rollout adoption mapping

| App            | BDD              | DDD                          | Contracts       |
| -------------- | ---------------- | ---------------------------- | --------------- |
| `organiclever` | Adopted (pilot)  | Adopted (pilot)              | Adopted (pilot) |
| `ayokoding`    | Adopted          | Deferred (multi-CLI profile) | NOT APPLICABLE  |
| `oseplatform`  | Adopted          | SHOULD — backlog             | NOT APPLICABLE  |
| `wahidyankf`   | SHOULD — backlog | SHOULD — backlog             | NOT APPLICABLE  |
| `rhino`        | Adopted          | Deferred (CLI)               | NOT APPLICABLE  |

#### Validation hooks

- **HIGH**: Full-stack or web-only app missing BDD Gherkin specs entirely after one full rollout cycle
- **MEDIUM**: Full-stack or web-only app has BDD but no DDD adoption after two full rollout cycles
- **MEDIUM**: Full-stack app missing API contracts when it exposes a REST API
- **LOW**: CLI app adopting DDD without documented rationale (may be premature)

### Standard 7 — Cross-Link Integrity

App READMEs and their corresponding specs trees maintain two-way navigation:

- The app README `## Behavior and Architecture` section carries a direct link to `specs/apps/<app-family>/README.md`.
- The spec tree `specs/apps/<app-family>/README.md` carries a link back to each app README it covers.
- When a spec file moves (e.g., during flat-root migration), ALL inbound links to that file update in the same commit.

Cross-link violations are CRITICAL findings if a link points to a non-existent file, HIGH findings if a required cross-link is missing.

## Examples

### Before/After — App README Trim

**Before** (Category B content in README — 300+ lines):

```markdown
## Routes and Screens

| Route       | Screen       | Description         |
| ----------- | ------------ | ------------------- |
| `/`         | Dashboard    | Main activity feed  |
| `/workouts` | Workout list | All workout history |

...

## Bounded Context Map

The app is split into five bounded contexts: journal, stats, workout-session...
[40 lines of architecture narrative]
```

**After** (Category A only — link pointer for Category B):

```markdown
## Behavior and Architecture

OrganicLever Web is a productivity tracker. See [specs/apps/organiclever/](../../specs/apps/organiclever/README.md) for routes, screens, bounded-context map, architecture decisions, and design system.
```

### Before/After — PM-Readable Spec File Header

**Before** (no audience block, opens with mechanism):

```markdown
# Architecture

The journal context owns the `JournalEvent` aggregate and exposes `appendEvent`, `bumpEvent`, and `listEvents` use-cases via PGlite store.
```

**After** (Rule 1 header block, Rule 2 intent-before-mechanism):

```markdown
# Architecture

> **Audience**: Engineers, Technical Product/Project Managers
>
> **Plain-language summary**: OrganicLever Web stores and displays productivity data in the browser using a local database. The app divides its logic into five areas — journal (what happened), stats (summaries), workout-session (active workout), routines (templates), and diagnostics. Each area owns its data and exposes a narrow API to the others.

## Journal

The journal records every life-event the user logs — workouts, meals, reading, focus sessions. It is the system of record; every other area either writes events here or reads from here.

Under the hood the journal area uses PGlite (Postgres-WASM — Postgres compiled to WebAssembly running directly in the browser, persisted via IndexedDB) and models its core record as a `JournalEvent` aggregate (a cluster of domain objects treated as one consistent unit by writes).
```

### Before/After — Spec Tree Migration (Flat-Root to C4-Aware)

**Before** (flat-root layout):

```
specs/apps/organiclever/
├── be/
│   └── gherkin/
├── web/
│   └── gherkin/
├── ddd/
├── c4/
└── contracts/
```

**After** (C4-aware five-folder layout):

```
specs/apps/organiclever/
├── product/
├── system-context/
├── containers/
│   └── contracts/
├── components/
│   ├── be/
│   └── web/
│       └── ddd/
└── behavior/
    ├── be/
    │   └── gherkin/
    └── web/
        └── gherkin/
```

**Migration checklist**:

1. Create five top-level folders with `README.md` placeholders.
2. In one atomic `git mv` commit: move `be/gherkin/` → `behavior/be/gherkin/`, `web/gherkin/` → `behavior/web/gherkin/`, `c4/*.md` files → their new positions, `contracts/` → `containers/contracts/`. `ddd/` stays at the app root (do not relocate it under `components/web/`; the ubiquitous language is per bounded context, not per surface).
3. In the same commit: update rhino-cli path constants, Nx `project.json` `inputs`, step file references, and governance cross-links.
4. Run `rhino-cli specs validate-tree <app>` to verify.

## Validation

`repo-rules-checker` and `specs-checker` enforce this convention. The `rhino-cli specs` subcommands handle deterministic checks; `specs-checker` handles semantic and narrative checks.

### Deterministic checks (rhino-cli)

| Check                                             | Command                                    | Finding level |
| ------------------------------------------------- | ------------------------------------------ | ------------- |
| README line-count cap exceeded                    | `rhino-cli specs validate-counts <folder>` | HIGH          |
| Spec tree top-level folder names wrong            | `rhino-cli specs validate-tree <app>`      | HIGH          |
| README count claims differ from actual file count | `rhino-cli specs validate-counts <folder>` | HIGH/MEDIUM   |
| BDD/DDD/Contracts adoption gap                    | `rhino-cli specs validate-adoption <app>`  | HIGH/MEDIUM   |

### LLM semantic checks (specs-checker)

| Check                                                                       | Finding level |
| --------------------------------------------------------------------------- | ------------- |
| Spec file missing required header block (audience + plain-language summary) | HIGH          |
| Section opens with mechanism rather than intent                             | MEDIUM        |
| Niche term used without gloss on first occurrence                           | MEDIUM        |
| Mainstream SWE term glossed unnecessarily                                   | LOW           |
| Code block missing one-sentence introduction                                | LOW           |

### Forbidden content audit (repo-rules-checker)

`repo-rules-checker` scans app READMEs for forbidden headings:

- `## Routes`, `## Screens`, `## API`, `## Endpoints` → HIGH (Category B content in README)
- `## Architecture` with more than 10 lines of content → HIGH (move to `components/*/architecture.md`)
- `## Bounded Context`, `## Design System` → HIGH

A README exceeding its line-count cap is a HIGH finding regardless of content.

## Refinement log

| Date       | Entry                                                                                                |
| ---------- | ---------------------------------------------------------------------------------------------------- |
| 2026-05-09 | CLI DDD adoption deferred; revisit if a CLI grows past ~10 commands or shows aggregate-shaped state. |

## Related

- [Specs Directory Structure Convention](./specs-directory-structure.md) — canonical path patterns and domain subdirectory rules within the `behavior/` tree
- [README Quality Convention](../writing/readme-quality.md) — README writing quality: voice, scannability, and engagement standards
- [Acceptance Criteria Convention](../../development/infra/acceptance-criteria.md) — Gherkin writing standards for feature files
- [Three-Level Testing Standard](../../development/quality/three-level-testing-standard.md) — unit, integration, and E2E testing levels consuming Gherkin specs
- [Repository Governance Architecture](../../repository-governance-architecture.md) — six-layer governance hierarchy
