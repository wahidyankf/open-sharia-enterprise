# wahidyankf-web — DDD + New Specs Format

**Status**: Done
**Scope**: `ose-public` — `apps/wahidyankf-web/`, `specs/apps/wahidyankf/`

## Problem

`specs/apps/wahidyankf/` is flat: a `README.md` at root and seven `.feature` files dumped under `fe/gherkin/`. No C4 layer, no DDD registry, no per-bounded-context organization. `apps/wahidyankf-web/src/` follows the same flatness — `src/components/`, `src/utils/` mix all concerns. There is no machine-validated link between specs and code; `rhino-cli ddd bc wahidyankf` cannot run because `specs/apps/wahidyankf/ddd/bounded-contexts.yaml` does not exist.

The result: (a) any reorg in `src/` breaks specs silently, (b) ubiquitous-language drift is invisible, (c) the per-bounded-context Gherkin coverage gate (`spec-coverage` walking `behavior/web/gherkin/<bc>/`) cannot be wired.

## Goal

Apply the same C4 + DDD layout that `organiclever` already uses to `wahidyankf-web`:

- Five-folder spec tree (`product/`, `system-context/`, `containers/`, `components/`, `behavior/`)
- DDD artefacts (`ddd/bounded-contexts.yaml` + per-BC glossaries)
- Source code reshaped to `src/contexts/<bc>/<layer>/` (per-BC layer subset — only the layers each BC actually needs)
- Pre-push gates wired: `ddd bc wahidyankf`, `ddd ul wahidyankf`, `spec-coverage` per BC

## Scope: 5 bounded contexts

| BC                  | Layers                        | Existing source roughly                             |
| ------------------- | ----------------------------- | --------------------------------------------------- |
| `app-shell`         | `[presentation]`              | `src/components/{shell,nav,theme,a11y}` parts       |
| `home`              | `[presentation]`              | `src/components/home`, `src/app/page.tsx`           |
| `cv`                | `[application, presentation]` | `src/components/cv`, `src/utils/cv-data` style data |
| `personal-projects` | `[application, presentation]` | `src/components/personal-projects` style content    |
| `search`            | `[application, presentation]` | `src/components/search` (search index + UI)         |

Layer subset is **per-BC** (Choice B2): a BC only declares the layers it actually has on disk. Pure-chrome BCs declare `[presentation]` only; data-bearing BCs declare `[application, presentation]`. No empty layer stubs.

## Container

One container: `web` (Next.js Vercel deployment). No tRPC server-side code today, so no `api` perspective Gherkins; the spec tree carries `behavior/web/gherkin/` only.

## Out of scope

- E2E spec migration (`apps/wahidyankf-web-fe-e2e/` step files) — separate plan.
- `apps/wahidyankf-web/src/test/` reorganization — colocate alongside refactored contexts where possible, but no test re-architecture.
- Adopting the `validate:specs-adoption` and `validate:specs-tree` pre-push gates — handled in `bdd-ddd-tooling-gap-fill` (this plan only adopts the format; tooling waits).

## Dependencies / sequencing

- **Independent of plans 2 and 3** — wahidyankf-web has no shared kernel with `oseplatform-web` or `ayokoding-web`. The three web-app plans can run in any order.
- **Unblocks plan 4** — `bdd-ddd-tooling-gap-fill` allowlists wahidyankf in the new pre-push adoption gate; until this plan lands, the allowlist must skip wahidyankf.

## Worktree

See [`delivery.md` § Worktree](./delivery.md#worktree) for the canonical worktree provisioning block consumed by the plan-execution workflow.

## Documents

- [brd.md](./brd.md) — business rationale
- [prd.md](./prd.md) — bounded-context inventory + Gherkin acceptance criteria
- [tech-docs.md](./tech-docs.md) — registry shape, layer subset rules, refactor mechanics
- [delivery.md](./delivery.md) — TDD-shaped phased checklist
