# ayokoding-web — DDD + New Specs Format

**Status**: In Progress
**Scope**: `ose-public` — `apps/ayokoding-web/`, `specs/apps/ayokoding/` (web parts only)
**Branch**: `worktree/ayokoding-web-ddd-and-specs-format` (assigned at execution time)

## Problem

`specs/apps/ayokoding/` is the same legacy shape as `oseplatform/`: a `c4/` folder of disjoint markdown documents, `web/gherkin/` and `be/gherkin/` at the spec-tree root rather than under `behavior/`, and no `ddd/` folder. Three additional complications make this app slightly heavier than oseplatform:

- **Multilingual content**. ayokoding-web ships English (primary) and Indonesian. Locale routing is wired through `src/middleware.ts`, which is owned by an `i18n` bounded context that has both an HTTP-semantic API surface (locale negotiation procedures) and a UI surface (locale switcher).
- **`build-tools/gherkin/`**. ayokoding has a fourth slug for index-generation build scripts that is unique to this app. Out of scope here; preserved as legacy.
- **`cli/gherkin/`**. Same shape as oseplatform — `ayokoding-cli` is its own deployable. Out of scope.

`apps/ayokoding-web/src/` follows the same flatness as oseplatform's: `src/server/` mixes content-api, search-api, navigation-api, i18n-api, and health-check routers; `src/app/` and `src/components/` are not BC-organized.

## Goal

Same shape as plan 2 (oseplatform):

- Five-folder spec tree
- DDD artefacts
- Source code reshaped to `src/contexts/<bc>/<layer>/`
- **Slug rename `be` → `api`** for the same reason oseplatform does it (tRPC server runs inside Next.js, not a separate container)
- Pre-push gates wired

## Scope: 6 bounded contexts

| BC           | Layers                                        | Owns                                                  |
| ------------ | --------------------------------------------- | ----------------------------------------------------- |
| `app-shell`  | `[presentation]`                              | Responsive layout + accessibility chrome              |
| `content`    | `[application, infrastructure, presentation]` | Content rendering + tRPC content-api                  |
| `search`     | `[application, infrastructure, presentation]` | Search + tRPC search-api                              |
| `i18n`       | `[application, infrastructure, presentation]` | Locale switching + tRPC i18n-api + Next.js middleware |
| `navigation` | `[application, presentation]`                 | Top-level nav + tRPC navigation-api                   |
| `health`     | `[application]`                               | Health-check tRPC procedure                           |

## Containers and perspectives

- **Container**: 1 deployable, `web` (Next.js + tRPC).
- **Behavior perspectives**: 2 slugs, `web` (UI-semantic) and `api` (tRPC HTTP-semantic). Same slug-vs-container distinction as oseplatform.
- **Other ayokoding containers deferred**: `ayokoding-cli` (with its `cli/gherkin/`) and the build-tools layer (with its `build-tools/gherkin/`) are preserved unchanged. Separate plans if/when they move.

## Out of scope

- `ayokoding-cli` and its `cli/gherkin/`.
- `build-tools/gherkin/` (index-generation build scripts).
- E2E spec migration (`apps/ayokoding-web-{be,fe}-e2e/` step files).
- Any content (article files in `content/`) — only code and specs reshaped.
- Adopting the `validate:specs-shape` pre-push gate — handled in `bdd-ddd-tooling-gap-fill`.

## Dependencies / sequencing

- **Independent of plans 1 and 2.**
- **Unblocks plan 4** — `bdd-ddd-tooling-gap-fill` allowlists ayokoding once the new format ships.

## Documents

- [brd.md](./brd.md) — business rationale
- [prd.md](./prd.md) — bounded-context inventory + Gherkin acceptance criteria
- [tech-docs.md](./tech-docs.md) — registry shape, slug rename, i18n middleware ownership, refactor mechanics
- [delivery.md](./delivery.md) — TDD-shaped phased checklist
