# oseplatform-web — DDD + New Specs Format

**Status**: In Progress
**Scope**: `ose-public` — `apps/oseplatform-web/`, `specs/apps/oseplatform/` (web parts only)

## Problem

`specs/apps/oseplatform/` is fractured across legacy folders that predate the C4 + DDD format `organiclever` already uses:

- `c4/` carries five disjoint markdown files (`context.md`, `container.md`, `component-web.md`, `component-be.md`, `README.md`) that should be split across `system-context/`, `containers/`, and `components/<slug>/`.
- `web/gherkin/` and `be/gherkin/` are the two perspective slugs but they sit at the spec-tree root rather than under a `behavior/` parent — and the `be` slug is misleading because the tRPC server lives **inside** the same Next.js process, not in a separate deployable container.
- There is no `ddd/` folder. No `bounded-contexts.yaml`. No glossaries. So `rhino-cli ddd bc oseplatform` cannot run.
- `apps/oseplatform-web/src/` flattens server logic (`src/server/`) and UI (`src/app/`, `src/components/`) without per-bounded-context organization.

## Goal

Apply the same C4 + DDD layout that `organiclever` already uses to `oseplatform-web`, with one deliberate deviation:

- Five-folder spec tree (`product/`, `system-context/`, `containers/`, `components/`, `behavior/`)
- DDD artefacts (`ddd/bounded-contexts.yaml` + per-BC glossaries + bounded-context-map)
- Source code reshaped to `src/contexts/<bc>/<layer>/` (per-BC layer subset)
- **Slug rename**: tRPC HTTP-semantic Gherkin scenarios live under the `api` slug (not `be`), because the tRPC server is **not a separate container** — it executes inside the Next.js process. Documented in `tech-docs.md` so future readers don't trip on the slug-vs-container distinction the way `organiclever` doesn't have to.
- Pre-push gates wired: `ddd bc oseplatform`, `ddd ul oseplatform`, `spec-coverage` per perspective

## Scope: 7 bounded contexts

| BC          | Layers                                        | Owns                                                |
| ----------- | --------------------------------------------- | --------------------------------------------------- |
| `app-shell` | `[presentation]`                              | Theme, navigation, responsive, accessibility chrome |
| `landing`   | `[presentation]`                              | Marketing landing page at `/`                       |
| `content`   | `[application, infrastructure, presentation]` | Content retrieval (tRPC) + rendering                |
| `search`    | `[application, infrastructure, presentation]` | Search backend (tRPC) + UI                          |
| `rss-feed`  | `[application, infrastructure]`               | RSS feed generation route handler                   |
| `seo`       | `[application, presentation]`                 | SEO metadata + sitemap                              |
| `health`    | `[application, presentation]`                 | Health endpoint + system-status diagnostic page     |

## Containers and perspectives

- **Container**: 1 deployable, `web` (Next.js + tRPC + content layer in one Vercel deployment).
- **Behavior perspectives**: 2 slugs, `web` (UI-semantic) and `api` (tRPC HTTP-semantic). The `api` slug indexes `behavior/api/gherkin/` and `components/api/component-api.md`; it does **not** map to a separate container.
- **CLI container deferred**: `oseplatform-cli` is a separate container with its own existing `cli/gherkin/`. Out of scope for this plan; legacy folder structure preserved.

## Out of scope

- `oseplatform-cli` and its specs (`specs/apps/oseplatform/cli/`) — separate plan if/when CLI moves to the new format.
- E2E spec migration (`apps/oseplatform-web-{be,fe}-e2e/` step files).
- Any change to deployment pipeline or `prod-oseplatform-web` branch.
- Adopting the `validate:specs-shape` pre-push gate — handled in `bdd-ddd-tooling-gap-fill` (this plan only adopts the format).

## Dependencies / sequencing

- **Independent of plans 1 and 3.**
- **Unblocks plan 4** — `bdd-ddd-tooling-gap-fill` allowlists oseplatform in the new pre-push adoption gate; until this plan lands, the allowlist must skip oseplatform.

## Worktree

Path: `worktrees/oseplatform-web-ddd-and-specs-format/`

Provisioning:

```bash
cd /Users/wkf/ose-projects/ose-public && claude --worktree oseplatform-web-ddd-and-specs-format
```

Or via git directly:

```bash
git worktree add worktrees/oseplatform-web-ddd-and-specs-format -b worktree/oseplatform-web-ddd-and-specs-format
```

Per [Worktree Path Convention](../../../governance/conventions/structure/worktree-path.md): worktrees in `ose-public` land at `worktrees/<name>/` in repo root. Path is gitignored.

## Documents

- [brd.md](./brd.md) — business rationale
- [prd.md](./prd.md) — bounded-context inventory + Gherkin acceptance criteria
- [tech-docs.md](./tech-docs.md) — registry shape, slug-vs-container note, refactor mechanics
- [delivery.md](./delivery.md) — TDD-shaped phased checklist
