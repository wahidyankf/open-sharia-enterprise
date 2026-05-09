# Delivery Checklist — oseplatform-web DDD + New Specs Format

All steps follow Red → Green → Refactor (TDD). Run `nx run oseplatform-web:test:quick` at the end of each phase. Do not advance phases out of order.

---

## Worktree

Worktree path: `worktrees/oseplatform-web-ddd-and-specs-format/`

Provision before execution (run from repo root):

```bash
claude --worktree oseplatform-web-ddd-and-specs-format
```

See [Worktree Path Convention](../../../governance/conventions/structure/worktree-path.md) and [Plans Organization Convention §Worktree Specification](../../../governance/conventions/structure/plans.md#worktree-specification).

---

## Environment Setup

- [ ] Provision worktree: `claude --worktree oseplatform-web-ddd-and-specs-format` (creates `worktrees/oseplatform-web-ddd-and-specs-format/` in repo root; see [Worktree Path Convention](../../../governance/conventions/structure/worktree-path.md)).
- [ ] Initialize toolchain in the root worktree: `npm install && npm run doctor -- --fix` (see [Worktree Toolchain Initialization](../../../governance/development/workflow/worktree-setup.md)).
- [ ] Verify existing tests pass before making changes: `nx run oseplatform-web:test:quick`.

---

## Phase 0 — Pre-flight inventory

- [ ] **0.1** Read `apps/oseplatform-web/src/` end-to-end. Enumerate every file under `src/server/`, `src/components/`, `src/app/`, `src/lib/`, `src/scripts/`. Produce a concrete file-by-file mapping table (old path → new path) and append it to `tech-docs.md` under "Source refactor mechanics" (the table currently shows initial estimate only).
- [ ] **0.2** Confirm `apps/oseplatform-web-be-e2e/` and `-fe-e2e/` step files do not import directly from `apps/oseplatform-web/src/server/` or `src/components/`. Record any cross-import findings; flag for separate plan if found.
- [ ] **0.3** Read every `c4/*.md` and existing `web/gherkin/*` and `be/gherkin/*` to confirm the BC inventory in `prd.md` does not miss a domain (e.g., a tRPC procedure not yet in the BC list).
- [ ] **0.4** Create worktree `worktrees/oseplatform-web-ddd/` per `governance/conventions/structure/worktree-path.md`.

---

## Phase 1 — Spec scaffolding (no source code touched)

- [ ] **1.1** Create the empty five-folder tree under `specs/apps/oseplatform/`: `product/`, `system-context/`, `containers/`, `components/{web,api}/`, `behavior/{web,api}/gherkin/`, `ddd/ubiquitous-language/`.
- [ ] **1.2** `git mv` the C4 documents:
  - `c4/context.md` → `system-context/context.md`
  - `c4/container.md` → `containers/container.md`
  - `c4/component-web.md` → `components/web/component-web.md`
  - `c4/component-be.md` → `components/api/component-api.md`
- [ ] **1.3** Edit `containers/container.md` to declare 1 container (`web`) and document the slug-vs-container distinction (per `tech-docs.md` top section).
- [ ] **1.4** Edit `components/api/component-api.md`: rename title "Component — be" to "Component — api", update inline references, update Mermaid diagram if it contains a "be" container box that should now be a perspective.
- [ ] **1.5** `git rm -r specs/apps/oseplatform/c4/`.
- [ ] **1.6** Author one-paragraph `README.md` for each new folder (lift from `specs/apps/organiclever/<folder>/README.md` and adapt).
- [ ] **1.7 RED** Run `rhino-cli specs validate-tree oseplatform` — fails: legacy `web/` and `be/` folders still at root.
- [ ] **1.8 GREEN** Replace root `specs/apps/oseplatform/README.md` with a copy of `specs/apps/organiclever/README.md` adapted for oseplatform (1 container `web`, 7 BCs, 2 perspectives `web`+`api`).
- [ ] **1.9** `npm run lint:md` — fix violations.

---

## Phase 2 — Move and re-shape Gherkin features

### 2.1 Web perspective moves

- [ ] **2.1.1** `git mv specs/apps/oseplatform/web/gherkin/responsive/responsive.feature specs/apps/oseplatform/behavior/web/gherkin/app-shell/responsive.feature`
- [ ] **2.1.2** Same for `navigation/navigation.feature`, `theme/theme.feature`, `accessibility/accessibility.feature` → all into `app-shell/`.
- [ ] **2.1.3** `git mv .../web/gherkin/landing-page/landing-page.feature .../behavior/web/gherkin/landing/landing-page.feature`.
- [ ] **2.1.4** `git rm -r specs/apps/oseplatform/web/`.

### 2.2 API perspective moves (slug rename `be` → `api`)

- [ ] **2.2.1** `git mv specs/apps/oseplatform/be/gherkin/health/health.feature specs/apps/oseplatform/behavior/api/gherkin/health/health.feature`
- [ ] **2.2.2** Same for `rss-feed/`, `search/`, `content-retrieval/`, `seo/` → into respective per-BC folders. Note: `content-retrieval/content-retrieval.feature` lands at `behavior/api/gherkin/content/content-retrieval.feature`.
- [ ] **2.2.3** `git rm -r specs/apps/oseplatform/be/`.

### 2.3 Behavior READMEs

- [ ] **2.3.1** Author `behavior/README.md` describing both perspectives + the slug-vs-container distinction (lift from `tech-docs.md` Phase 1.3 wording).
- [ ] **2.3.2** Author `behavior/web/gherkin/README.md` and `behavior/api/gherkin/README.md` listing per-BC subfolder + feature count (template per organiclever).

### 2.4 Validate

- [ ] **2.4 RED+GREEN** Run `nx run oseplatform-web-be-e2e:test:e2e` and `oseplatform-web-fe-e2e:test:e2e`. If broken (because step files referenced legacy paths), fix step file globs in their respective `playwright.config.ts`. Aim for green E2E before phase 3.

---

## Phase 3 — DDD scaffolding (registry + glossaries + map)

- [ ] **3.1 RED** Run `rhino-cli ddd bc oseplatform` — fails: registry not found.
- [ ] **3.2 GREEN** Author `specs/apps/oseplatform/ddd/bounded-contexts.yaml` per the template in `tech-docs.md`. Schema version `2`, 7 contexts, layer subset per BC.
- [ ] **3.3 RED** Run `rhino-cli ddd bc oseplatform` — fails: code dirs do not exist (refactor not done).
- [ ] **3.4** Author `ddd/bounded-context-map.md` with a Mermaid relationship diagram (7 BCs, edges from registry's `relationships:`).
- [ ] **3.5** Author the 7 glossaries under `ddd/ubiquitous-language/<bc>.md`.
- [ ] **3.6** Author `ddd/README.md` and `ddd/ubiquitous-language/README.md` (lift from organiclever, adapt).
- [ ] **3.7 RED** Run `rhino-cli ddd ul oseplatform` — fails: code identifiers not yet in `src/contexts/`. Expected.

---

## Phase 4 — tRPC router split (one BC at a time)

For each tRPC-bearing BC: extract procedures from `src/server/router.ts` into `src/contexts/<bc>/application/router.ts`. Run `nx run oseplatform-web-be-e2e:test:e2e` after each.

- [ ] **4.1 `health`** — extract `health` procedure. Smallest, lowest risk; canonical first.
- [ ] **4.2 `seo`** — extract metadata + sitemap procedures.
- [ ] **4.3 `rss-feed`** — extract feed-builder procedure(s) + the route handler that streams XML.
- [ ] **4.4 `content`** — extract content-retrieval procedures + DTOs. Move filesystem adapters into `src/contexts/content/infrastructure/`.
- [ ] **4.5 `search`** — extract search procedures. Move index implementation into `src/contexts/search/infrastructure/`.
- [ ] **4.6** Move root router stitching into `src/contexts/app-shell/application/root-router.ts`. Move shared tRPC context creation + middleware into `src/lib/trpc/` (kept as cross-cutting infra).
- [ ] **4.7** Delete `src/server/`. `nx run oseplatform-web-be-e2e:test:e2e` green.

---

## Phase 5 — UI source refactor (one BC at a time)

For each BC: move UI files from `src/components/` and `src/app/` into `src/contexts/<bc>/presentation/`. Update imports. Run `nx run oseplatform-web:test:quick` after each.

- [ ] **5.1 `app-shell`** — header, footer, theme toggle, navigation, accessibility chrome.
- [ ] **5.2 `landing`** — landing page sections.
- [ ] **5.3 `content`** — article + content-list rendering components.
- [ ] **5.4 `search`** — search input + results dropdown.
- [ ] **5.5 `seo`** — `<head>` injectors via `generateMetadata`.
- [ ] **5.6 `health`** — system-status diagnostic page (if present in `src/app/system/status/`).
- [ ] **5.7** `git rm -r` empty `src/components/` if fully drained. `src/app/` keeps Next.js route files (thin glue importing from contexts/).

---

## Phase 6 — Wire validators into project.json

- [ ] **6.1 GREEN check** `(cd ../../apps/rhino-cli && CGO_ENABLED=0 go run main.go ddd bc oseplatform)` — passes.
- [ ] **6.2 GREEN check** `... ddd ul oseplatform` — passes.
- [ ] **6.3** Edit `apps/oseplatform-web/project.json`:
  - Prepend the two `ddd bc/ul` commands to `test:quick.options.commands`.
  - Set `test:quick.options.parallel` to `false` if not already.
  - Add the four new `inputs` paths.
  - Add `rhino-cli` to `implicitDependencies` if absent.
- [ ] **6.4** Add the single `spec-coverage` target running both perspectives sequentially per `tech-docs.md`.
- [ ] **6.5 GREEN** `nx run oseplatform-web:test:quick` runs DDD validators first, then existing pipeline. All green.
- [ ] **6.6 GREEN** `nx run oseplatform-web:spec-coverage` — 0 step gaps across both perspectives.

---

## Phase 7 — Documentation cross-links

- [ ] **7.1** Update `apps/oseplatform-web/README.md`:
  - Replace any `specs/apps/oseplatform/c4/`, `web/gherkin/`, `be/gherkin/` references with new paths.
  - Add a "Specs" section linking to the five-folder tree, the ddd registry, and the glossaries.
  - Note the `be → api` slug rename, with a one-time deprecation note.
- [ ] **7.2** Update `specs/apps/oseplatform/README.md` to mirror `specs/apps/organiclever/README.md` structure exactly (sample tree block, container table, BC table, ddd section).
- [ ] **7.3** `npm run lint:md` — fix violations.

---

## Phase 8 — Final validation gate

> **Important**: Fix ALL failures found during quality gates, not just those caused by your changes. This follows the root cause orientation principle — proactively fix preexisting errors encountered during work.

- [ ] **8.1** `rhino-cli specs validate-tree oseplatform` — 0 findings.
- [ ] **8.2** `rhino-cli specs validate-counts specs/apps/oseplatform` — 0 findings.
- [ ] **8.3** `rhino-cli specs validate-links specs/apps/oseplatform` — 0 findings.
- [ ] **8.4** `rhino-cli specs validate-adoption oseplatform` — 0 findings.
- [ ] **8.5** `rhino-cli ddd bc oseplatform` — 0 findings.
- [ ] **8.6** `rhino-cli ddd ul oseplatform` — 0 findings.
- [ ] **8.7** `nx run oseplatform-web:test:quick` — 0 findings, coverage ≥80%.
- [ ] **8.8** `nx run oseplatform-web:spec-coverage` — 0 step gaps across both perspectives.
- [ ] **8.9** `nx run oseplatform-web-be-e2e:test:e2e` — every tRPC scenario passes.
- [ ] **8.10** `nx run oseplatform-web-fe-e2e:test:e2e` — every UI scenario passes.
- [ ] **8.11** `nx affected -t typecheck lint test:quick spec-coverage --base=HEAD~1` — full pre-push gate green.
- [ ] **8.12** `npm run lint:md` — 0 violations.

### Manual UI Verification (Playwright MCP)

- [ ] **8.13** Start dev server: `nx dev oseplatform-web` (listens at `localhost:3100`).
- [ ] **8.14** Navigate to the home page via `browser_navigate` to `http://localhost:3100/` — confirm landing page renders without blank sections.
- [ ] **8.15** `browser_snapshot` — verify bounded-context-organized components render correctly (content listings, navigation, theme visible).
- [ ] **8.16** Navigate to a content article page and a search page; `browser_snapshot` each — confirm no layout regressions from the tRPC router split or source refactor.
- [ ] **8.17** `browser_console_messages` — must show 0 JS errors across all tested pages.
- [ ] **8.18** `browser_take_screenshot` for each page — attach for visual record.

---

## Phase 9 — Commit, push, archive

### Commit Guidelines

- [ ] Commit changes thematically — group related changes into logically cohesive commits.
- [ ] Follow Conventional Commits format: `<type>(<scope>): <description>`.
- [ ] Split different domains/concerns into separate commits (e.g., spec scaffolding separate from tRPC router split separate from project.json wiring).
- [ ] Do NOT bundle unrelated fixes into a single commit.

- [ ] **9.1** Commit (single atomic, or per-phase if maintainer prefers):
  - Message: `feat(oseplatform-web): adopt C4 + DDD specs format with api slug`
  - Body lists: 7 BCs, slug rename `be → api`, tRPC router split, DDD wiring.
- [ ] **9.2** Push via Trunk Based Development (default) or draft PR (optional).
- [ ] **9.3** Wait for `main` CI green — specifically monitor the `CI` workflow at `https://github.com/wahidyankf/ose-public/actions` for the push commit. Per `governance/development/workflow/ci-monitoring.md`.
- [ ] **9.4** Move plan folder to `plans/done/YYYY-MM-DD__oseplatform-web-ddd-and-specs-format/`.
- [ ] **9.5** Update `plans/in-progress/README.md` and `plans/done/README.md`.
- [ ] **9.6** Notify `bdd-ddd-tooling-gap-fill` plan: oseplatform now allowlist-eligible.
