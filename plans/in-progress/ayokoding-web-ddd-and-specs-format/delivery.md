# Delivery Checklist — ayokoding-web DDD + New Specs Format

All steps follow Red → Green → Refactor (TDD). Run `nx run ayokoding-web:test:quick` at the end of each phase. Do not advance phases out of order.

---

## Worktree

Worktree path: `worktrees/ayokoding-web-ddd-and-specs-format/`

Provision before execution (run from repo root):

```bash
claude --worktree ayokoding-web-ddd-and-specs-format
```

See [Worktree Path Convention](../../../governance/conventions/structure/worktree-path.md) and [Plans Organization Convention §Worktree Specification](../../../governance/conventions/structure/plans.md#worktree-specification).

---

## Environment Setup

- [ ] Provision worktree: `claude --worktree ayokoding-web-ddd-and-specs-format` (creates `worktrees/ayokoding-web-ddd-and-specs-format/` in repo root; see [Worktree Path Convention](../../../governance/conventions/structure/worktree-path.md)).
- [ ] Initialize toolchain in the root worktree: `npm install && npm run doctor -- --fix` (see [Worktree Toolchain Initialization](../../../governance/development/workflow/worktree-setup.md)).
- [ ] Verify existing tests pass before making changes: `nx run ayokoding-web:test:quick`.

---

## Phase 0 — Pre-flight inventory

- [ ] **0.1** Read `apps/ayokoding-web/src/` end-to-end. Enumerate every file under `src/server/`, `src/components/`, `src/app/`, `src/lib/`, `src/middleware.ts`, `src/scripts/`. Produce a concrete file-by-file mapping table.
- [ ] **0.2** Confirm `apps/ayokoding-web-be-e2e/` and `-fe-e2e/` step files do not import from `src/server/` or `src/components/`.
- [ ] **0.3** Read every `c4/*.md`, `web/gherkin/**`, `be/gherkin/**` to confirm BC inventory captures every domain. Confirm `build-tools/gherkin/` and `cli/gherkin/` are out-of-scope and no scenario crosses scope boundaries.
- [ ] **0.4** Create worktree `worktrees/ayokoding-web-ddd/`.

---

## Phase 1 — Spec scaffolding (no source code touched)

- [ ] **1.1** Create the empty five-folder tree under `specs/apps/ayokoding/`: `product/`, `system-context/`, `containers/`, `components/{web,api}/`, `behavior/{web,api}/gherkin/`, `ddd/ubiquitous-language/`.
- [ ] **1.2** `git mv` the C4 documents:
  - `c4/context.md` → `system-context/context.md`
  - `c4/container.md` → `containers/container.md`
  - `c4/component-web.md` → `components/web/component-web.md`
  - `c4/component-be.md` → `components/api/component-api.md`
- [ ] **1.3** Edit `containers/container.md` to declare 1 container (`web`); document the slug-vs-container distinction; explicitly note `cli` and `build-tools` legacy slugs are out of scope.
- [ ] **1.4** Edit `components/api/component-api.md`: rename title and inline references from "be" to "api".
- [ ] **1.5** `git rm -r specs/apps/ayokoding/c4/`.
- [ ] **1.6** Author one-paragraph `README.md` for each new folder.
- [ ] **1.7 RED** Run `rhino-cli specs validate-tree ayokoding` — fails because legacy `web/` and `be/` folders still at root.
- [ ] **1.8 GREEN** Replace root `specs/apps/ayokoding/README.md` with a copy of `specs/apps/organiclever/README.md` adapted (1 container, 6 BCs, 2 perspectives `web`+`api`, plus a footnote on out-of-scope `cli/` and `build-tools/`).
- [ ] **1.9** `npm run lint:md` — fix violations.

---

## Phase 2 — Move and re-shape Gherkin features

### 2.1 Web perspective moves

- [ ] **2.1.1** `git mv` web Gherkins:
  - `web/gherkin/responsive/responsive.feature` → `behavior/web/gherkin/app-shell/responsive.feature`
  - `web/gherkin/accessibility/accessibility.feature` → `behavior/web/gherkin/app-shell/accessibility.feature`
  - `web/gherkin/content-rendering/content-rendering.feature` → `behavior/web/gherkin/content/content-rendering.feature`
  - `web/gherkin/search/search.feature` → `behavior/web/gherkin/search/search.feature`
  - `web/gherkin/i18n/i18n.feature` → `behavior/web/gherkin/i18n/i18n.feature`
  - `web/gherkin/navigation/navigation.feature` → `behavior/web/gherkin/navigation/navigation.feature`
- [ ] **2.1.2** `git rm -r specs/apps/ayokoding/web/`.

### 2.2 API perspective moves (slug rename `be` → `api`)

- [ ] **2.2.1** `git mv` be Gherkins:
  - `be/gherkin/health/health-check.feature` → `behavior/api/gherkin/health/health-check.feature`
  - `be/gherkin/content-api/content-api.feature` → `behavior/api/gherkin/content/content-api.feature`
  - `be/gherkin/search-api/search-api.feature` → `behavior/api/gherkin/search/search-api.feature`
  - `be/gherkin/navigation-api/navigation-api.feature` → `behavior/api/gherkin/navigation/navigation-api.feature`
  - `be/gherkin/i18n/i18n-api.feature` → `behavior/api/gherkin/i18n/i18n-api.feature`
- [ ] **2.2.2** `git rm -r specs/apps/ayokoding/be/`.

### 2.3 READMEs

- [ ] **2.3.1** Author `behavior/README.md` describing both perspectives + slug-vs-container distinction.
- [ ] **2.3.2** Author `behavior/web/gherkin/README.md` and `behavior/api/gherkin/README.md`.

### 2.4 Validate

- [ ] **2.4** Run `nx run ayokoding-web-be-e2e:test:e2e` and `-fe-e2e:test:e2e`. Fix step file globs if needed.

### 2.5 Out-of-scope confirmation

- [ ] **2.5.1** `cli/` and `build-tools/` paths under `specs/apps/ayokoding/` unchanged. No file in either tree edited or moved.

---

## Phase 3 — DDD scaffolding

- [ ] **3.1 RED** `rhino-cli ddd bc ayokoding` — fails: registry not found.
- [ ] **3.2 GREEN** Author `ddd/bounded-contexts.yaml` per `tech-docs.md` template (6 contexts, schema v2). For the 4 multi-perspective BCs (`content`, `search`, `i18n`, `navigation`): set `gherkin:` to the web-side path (`behavior/web/gherkin/<bc>`) per the registry limitation workaround in `tech-docs.md §Multi-perspective gherkin: workaround`. Do NOT set the api-side path here — api-side paths will be registered via plan 4 fix #11 once the `gherkin: []string` schema extension ships.
- [ ] **3.3 RED** `rhino-cli ddd bc ayokoding` — fails: code dirs not yet created. Expected.
- [ ] **3.4** Author `ddd/bounded-context-map.md` with Mermaid relationship diagram.
- [ ] **3.5** Author 6 glossaries under `ddd/ubiquitous-language/<bc>.md`.
- [ ] **3.6** Author `ddd/README.md` and `ddd/ubiquitous-language/README.md`.
- [ ] **3.7 RED** `rhino-cli ddd ul ayokoding` — fails: code identifiers not yet under `src/contexts/`. Expected.

---

## Phase 4 — tRPC router split (one BC at a time)

For each tRPC-bearing BC: extract from `src/server/router.ts` into `src/contexts/<bc>/application/router.ts`. Run BE-E2E after each.

- [ ] **4.1 `health`** — smallest first.
- [ ] **4.2 `i18n-api`** — extract i18n procedures.
- [ ] **4.3 `navigation-api`** — extract navigation procedures.
- [ ] **4.4 `content-api`** — extract content procedures + DTOs. Move filesystem adapters into `src/contexts/content/infrastructure/`.
- [ ] **4.5 `search-api`** — extract search procedures. Move index implementation into `src/contexts/search/infrastructure/`.
- [ ] **4.6** Move root router stitching into `src/contexts/app-shell/application/root-router.ts`.
- [ ] **4.7** Delete `src/server/`. `nx run ayokoding-web-be-e2e:test:e2e` green.

---

## Phase 5 — UI source refactor (one BC at a time)

- [ ] **5.1 `app-shell`** — header, footer, responsive chrome, accessibility wiring.
- [ ] **5.2 `content`** — article + content-list rendering.
- [ ] **5.3 `search`** — search input + results dropdown.
- [ ] **5.4 `navigation`** — top-level nav components.
- [ ] **5.5 `i18n` (UI part only)** — locale switcher component. Run `nx run ayokoding-web-fe-e2e:test:e2e` after this with both locales.
- [ ] **5.6** `git rm -r` empty `src/components/` if drained.

---

## Phase 6 — i18n middleware migration

- [ ] **6.1 RED** Read existing `src/middleware.ts`; capture its full body for unit test reference.
- [ ] **6.2 GREEN** Create `src/contexts/i18n/application/middleware.ts` with the implementation copied verbatim. Export `middleware` and `config`.
- [ ] **6.3 GREEN** Replace `src/middleware.ts` body with a one-line re-export:

  ```ts
  export { middleware, config } from "./contexts/i18n/application/middleware";
  ```

- [ ] **6.4 GREEN** Run `nx run ayokoding-web-fe-e2e:test:e2e` with English locale. Pass.
- [ ] **6.5 GREEN** Run `nx run ayokoding-web-fe-e2e:test:e2e` with Indonesian locale. Pass.
- [ ] **6.6 GREEN** Manually verify in browser: `/` with `Accept-Language: id` redirects to `/id`, with `Accept-Language: en` redirects to `/en`.

---

## Phase 7 — Wire validators into project.json

- [ ] **7.1 GREEN check** `(cd ../../apps/rhino-cli && CGO_ENABLED=0 go run main.go ddd bc ayokoding)` passes.
- [ ] **7.2 GREEN check** `... ddd ul ayokoding` passes.
- [ ] **7.3** Edit `apps/ayokoding-web/project.json`:
  - Prepend the two `ddd bc/ul` commands to `test:quick.options.commands`.
  - Set `test:quick.options.parallel: false`.
  - Add the four new `inputs` paths.
  - Add `rhino-cli` to `implicitDependencies` if absent.
- [ ] **7.4** Add the single `spec-coverage` target running both perspectives sequentially per `tech-docs.md`.
- [ ] **7.5 GREEN** `nx run ayokoding-web:test:quick` runs DDD validators first, then existing pipeline. All green.
- [ ] **7.6 GREEN** `nx run ayokoding-web:spec-coverage` — 0 step gaps across both perspectives.

---

## Phase 8 — Documentation cross-links

- [ ] **8.1** Update `apps/ayokoding-web/README.md`:
  - Replace any `specs/apps/ayokoding/c4/`, `web/gherkin/`, `be/gherkin/` references.
  - Add a "Specs" section.
  - Note `be → api` slug rename + i18n middleware ownership change.
- [ ] **8.2** Update `specs/apps/ayokoding/README.md` to mirror `specs/apps/organiclever/README.md` structure.
- [ ] **8.3** `npm run lint:md` — fix violations.

---

## Phase 9 — Final validation gate

> **Important**: Fix ALL failures found during quality gates, not just those caused by your changes. This follows the root cause orientation principle — proactively fix preexisting errors encountered during work.

- [ ] **9.1** `rhino-cli specs validate-tree ayokoding` — 0 findings.
- [ ] **9.2** `rhino-cli specs validate-counts specs/apps/ayokoding` — 0 findings.
- [ ] **9.3** `rhino-cli specs validate-links specs/apps/ayokoding` — 0 findings.
- [ ] **9.4** `rhino-cli specs validate-adoption ayokoding` — 0 findings.
- [ ] **9.5** `rhino-cli ddd bc ayokoding` — 0 findings.
- [ ] **9.6** `rhino-cli ddd ul ayokoding` — 0 findings.
- [ ] **9.7** `nx run ayokoding-web:test:quick` — 0 findings, coverage ≥80%.
- [ ] **9.8** `nx run ayokoding-web:spec-coverage` — 0 step gaps across both perspectives.
- [ ] **9.9** `nx run ayokoding-web-be-e2e:test:e2e` — every tRPC scenario passes.
- [ ] **9.10** `nx run ayokoding-web-fe-e2e:test:e2e` — every UI scenario passes in both English and Indonesian.
- [ ] **9.11** `nx affected -t typecheck lint test:quick spec-coverage --base=HEAD~1` — full pre-push gate green.
- [ ] **9.12** `npm run lint:md` — 0 violations.
- [ ] **9.13** Confirm `specs/apps/ayokoding/cli/` and `build-tools/` unchanged.

### Manual UI Verification (Playwright MCP)

- [ ] **9.14** Start dev server: `nx dev ayokoding-web` (listens at `localhost:3101`).
- [ ] **9.15** Navigate to `http://localhost:3101/en/` via `browser_navigate` — confirm English content renders without blank sections.
- [ ] **9.16** Navigate to `http://localhost:3101/id/` — confirm Indonesian locale renders correctly (locale switcher visible, content in Indonesian).
- [ ] **9.17** `browser_snapshot` on each locale — verify bounded-context-organized components render correctly (content listings, navigation, search visible).
- [ ] **9.18** Test locale switching: click locale switcher in UI via `browser_click` — confirm redirect to correct locale URL.
- [ ] **9.19** `browser_console_messages` — must show 0 JS errors across both locales.
- [ ] **9.20** `browser_take_screenshot` for English and Indonesian home pages — attach for visual record.

---

## Phase 10 — Commit, push, archive

### Commit Guidelines

- [ ] Commit changes thematically — group related changes into logically cohesive commits.
- [ ] Follow Conventional Commits format: `<type>(<scope>): <description>`.
- [ ] Split different domains/concerns into separate commits (e.g., spec scaffolding separate from tRPC router split separate from i18n middleware migration separate from project.json wiring).
- [ ] Do NOT bundle unrelated fixes into a single commit.

- [ ] **10.1** Commit (single atomic):
  - Message: `feat(ayokoding-web): adopt C4 + DDD specs format with api slug + i18n BC ownership`
  - Body lists: 6 BCs, slug rename `be → api`, tRPC router split, i18n middleware migration, DDD wiring.
- [ ] **10.2** Push via Trunk Based Development.
- [ ] **10.3** Wait for `main` CI green — specifically monitor the `CI` workflow at `https://github.com/wahidyankf/ose-public/actions` for the push commit. Per `governance/development/workflow/ci-monitoring.md`.
- [ ] **10.4** Move plan folder to `plans/done/YYYY-MM-DD__ayokoding-web-ddd-and-specs-format/`.
- [ ] **10.5** Update `plans/in-progress/README.md` and `plans/done/README.md`.
- [ ] **10.6** Notify `bdd-ddd-tooling-gap-fill` plan: ayokoding now allowlist-eligible. With plans 1+2+3 done, plan 4 is unblocked.
