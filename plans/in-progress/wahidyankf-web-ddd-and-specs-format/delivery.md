# Delivery Checklist — wahidyankf-web DDD + New Specs Format

All steps follow Red → Green → Refactor (TDD). Run `nx run wahidyankf-web:test:quick` at the end of each phase. Do not advance to the next phase until current is green.

---

## Worktree

Worktree path: `worktrees/wahidyankf-web-ddd-and-specs-format/`

Provision before execution (run from repo root):

```bash
claude --worktree wahidyankf-web-ddd-and-specs-format
```

See [Worktree Path Convention](../../../governance/conventions/structure/worktree-path.md) and [Plans Organization Convention §Worktree Specification](../../../governance/conventions/structure/plans.md#worktree-specification).

---

## Environment Setup

- [ ] Provision worktree: `claude --worktree wahidyankf-web-ddd-and-specs-format` (creates `worktrees/wahidyankf-web-ddd-and-specs-format/` in repo root; see [Worktree Path Convention](../../../governance/conventions/structure/worktree-path.md)).
- [ ] Initialize toolchain in the root worktree: `npm install && npm run doctor -- --fix` (see [Worktree Toolchain Initialization](../../../governance/development/workflow/worktree-setup.md)).
- [ ] Verify existing tests pass before making changes: `nx run wahidyankf-web:test:quick`.

---

## Phase 0 — Pre-flight inventory

- [ ] **0.1** Read `apps/wahidyankf-web/src/` end-to-end: enumerate every file under `src/components/`, `src/utils/`, `src/app/`. Produce a concrete file-by-file mapping table (old path → new path) and append it to `tech-docs.md` under "Source refactor mechanics" (the table currently shows initial estimate only).
- [ ] **0.2** Confirm `apps/wahidyankf-web-fe-e2e/` step files do not import directly from `apps/wahidyankf-web/src/components/` (they should query selectors, not import code). Record findings.
- [ ] **0.3** Stash any in-flight work; create worktree `worktrees/wahidyankf-web-ddd/` per `governance/conventions/structure/worktree-path.md`.

---

## Phase 1 — Spec scaffolding (no source code touched)

- [ ] **1.1** Create the empty five-folder tree under `specs/apps/wahidyankf/`: `product/`, `system-context/`, `containers/`, `components/web/`, `behavior/web/gherkin/`, `ddd/ubiquitous-language/`. Each new directory gets a placeholder `README.md` (one sentence + heading).
- [ ] **1.2 RED** Run `rhino-cli specs validate-tree wahidyankf` — fails because the legacy `fe/` folder is still present and the `README.md` files are placeholder. Capture failure output.
- [ ] **1.3 GREEN** Replace the legacy root `specs/apps/wahidyankf/README.md` with a copy of `specs/apps/organiclever/README.md` adapted to wahidyankf-web (single container `web`, 5 BCs, no tRPC). Validate `rhino-cli specs validate-tree wahidyankf` passes.
- [ ] **1.4 GREEN** Author each scaffolded README.md with one paragraph of context describing the folder's purpose. Lift wording from `specs/apps/organiclever/<folder>/README.md` and adapt.
- [ ] **1.5 REFACTOR** Run `npm run lint:md` — fix any markdownlint findings. `nx run wahidyankf-web:test:quick` still passes (no DDD wiring yet, so this run is unchanged).

---

## Phase 2 — Move and re-shape Gherkin features

- [ ] **2.1** `git mv` the seven feature files into per-BC subfolders under `behavior/web/gherkin/`:
  - `fe/gherkin/theme.feature` → `behavior/web/gherkin/app-shell/theme.feature`
  - `fe/gherkin/responsive.feature` → `behavior/web/gherkin/app-shell/responsive.feature`
  - `fe/gherkin/accessibility.feature` → `behavior/web/gherkin/app-shell/accessibility.feature`
  - `fe/gherkin/home.feature` → `behavior/web/gherkin/home/home.feature`
  - `fe/gherkin/cv.feature` → `behavior/web/gherkin/cv/cv.feature`
  - `fe/gherkin/personal-projects.feature` → `behavior/web/gherkin/personal-projects/personal-projects.feature`
  - `fe/gherkin/search.feature` → `behavior/web/gherkin/search/search.feature`
- [ ] **2.2** Author `behavior/web/gherkin/README.md` listing every BC subfolder with feature count (template per `specs/apps/organiclever/behavior/web/gherkin/README.md`).
- [ ] **2.3** Author `behavior/README.md` describing the `web` perspective (UI-semantic). Note in this README that there is no `api` perspective today; if a server side appears later, `behavior/api/gherkin/` is added.
- [ ] **2.4** `git rm -r specs/apps/wahidyankf/fe/` — removes the legacy folder tree.
- [ ] **2.5 RED** Run `nx run wahidyankf-web-fe-e2e:test:e2e` — confirm the moves did not break E2E (step files locate features by filename, not folder). If broken, fix the step file glob in `apps/wahidyankf-web-fe-e2e/playwright.config.ts` or equivalent.
- [ ] **2.6 GREEN** All 7 features reachable under `behavior/web/gherkin/<bc>/<feature>.feature`. E2E green.
- [ ] **2.7 REFACTOR** Update any markdown link in `apps/wahidyankf-web/README.md` or `specs/apps/wahidyankf/README.md` that still points at the legacy `fe/gherkin/` location.

---

## Phase 3 — DDD scaffolding (registry + glossaries + map)

- [ ] **3.1 RED** Run `rhino-cli ddd bc wahidyankf` — fails: registry not found.
- [ ] **3.2 GREEN** Author `specs/apps/wahidyankf/ddd/bounded-contexts.yaml` per the template in `tech-docs.md`. Schema version `2`, 5 contexts, layer subset per BC.
- [ ] **3.3 RED** Run `rhino-cli ddd bc wahidyankf` — fails: code dirs do not exist (refactor not done yet). Expected.
- [ ] **3.4** Author `specs/apps/wahidyankf/ddd/bounded-context-map.md` with a Mermaid relationship diagram (5 BCs, edges from registry's `relationships:`). Pattern: copy `specs/apps/organiclever/ddd/bounded-context-map.md` structure.
- [ ] **3.5** Author the 5 glossaries under `ddd/ubiquitous-language/<bc>.md` per the template in `tech-docs.md`. Each: frontmatter (3 keys), one-line summary, term index table, terms in detail (≥3 terms per BC), forbidden synonyms list.
- [ ] **3.6** Author `ddd/README.md` (lift from `specs/apps/organiclever/ddd/README.md` and adapt: 5 contexts, no tRPC, single container).
- [ ] **3.7** Author `ddd/ubiquitous-language/README.md` (lift from organiclever's equivalent, adapted).
- [ ] **3.8 RED** Run `rhino-cli ddd ul wahidyankf` — fails: code identifiers don't exist yet (still in `src/components/`). Expected.
- [ ] **3.9 GREEN check** Confirm only the expected failures: missing code dirs from `ddd bc`, missing identifiers from `ddd ul`. No frontmatter / table-header / feature-reference findings.

---

## Phase 4 — Source code refactor (one BC at a time)

For each BC, follow the same micro-cycle: skeleton → move → fix imports → run tests.

### 4.0 Lock the move table

- [ ] **4.0** Append the concrete file-by-file move table to `tech-docs.md` (replacing the initial estimate). Confirm with the maintainer before proceeding.

### 4.1 `app-shell` (presentation only)

- [ ] **4.1.1** Create empty `apps/wahidyankf-web/src/contexts/app-shell/presentation/`.
- [ ] **4.1.2 RED** `git mv` chrome files (header, footer, theme-toggle, nav, etc.) into the new path.
- [ ] **4.1.3 GREEN** Update import statements project-wide (project-wide find-replace per file, then `nx run wahidyankf-web:typecheck`).
- [ ] **4.1.4 GREEN** `nx run wahidyankf-web:test:quick` passes.
- [ ] **4.1.5** Run `rhino-cli ddd ul wahidyankf` — `app-shell` glossary's identifiers now resolve. Other 4 BCs still error (expected).

### 4.2 `home` (presentation only)

- [ ] **4.2.1** Create empty `apps/wahidyankf-web/src/contexts/home/presentation/`. `git mv` home files (see move table in `tech-docs.md §Source refactor mechanics`): all files under `src/components/home/` → `src/contexts/home/presentation/`. Update import statements project-wide via `nx run wahidyankf-web:typecheck` to confirm zero import errors. Run `nx run wahidyankf-web:test:quick` — passes. Run `rhino-cli ddd ul wahidyankf` — `home` glossary identifiers now resolve. After: 2 BCs resolve in `ddd ul`.

### 4.3 `cv` (application + presentation)

- [ ] **4.3.1** Create `src/contexts/cv/{application,presentation}/`.
- [ ] **4.3.2 RED** Move CV components into `presentation/`; move CV data + helpers into `application/`. Split where one file mixes both.
- [ ] **4.3.3 GREEN** Update imports; `nx run wahidyankf-web:typecheck` passes.
- [ ] **4.3.4 GREEN** `nx run wahidyankf-web:test:quick` passes.

### 4.4 `personal-projects` (application + presentation)

- [ ] **4.4.1** Create `src/contexts/personal-projects/{application,presentation}/`. `git mv` personal-projects components (see move table in `tech-docs.md §Source refactor mechanics`) into `presentation/`; move project records + filter logic into `application/`. Split any mixed file. Update imports; run `nx run wahidyankf-web:typecheck`. Run `nx run wahidyankf-web:test:quick` — passes.

### 4.5 `search` (application + presentation)

- [ ] **4.5.1** Create `src/contexts/search/{application,presentation}/`. `git mv` search files (see move table in `tech-docs.md §Source refactor mechanics`): search-index builder + scoring → `application/`; search input + results UI → `presentation/`. Update imports; run `nx run wahidyankf-web:typecheck`. Run `nx run wahidyankf-web:test:quick` — passes.

### 4.6 Cleanup

- [ ] **4.6** `git rm -r` any legacy folders that are now empty: `src/components/{cv,home,personal-projects,search,...}/`, leftover `src/utils/` files moved into per-BC `application/`. Confirm `nx graph` shows no broken edges.

---

## Phase 5 — Wire DDD validators into `test:quick`

- [ ] **5.1 RED** Manually run `(cd ../../apps/rhino-cli && CGO_ENABLED=0 go run main.go ddd bc wahidyankf)` — should now pass. Same for `ddd ul wahidyankf`.
- [ ] **5.2 GREEN** Edit `apps/wahidyankf-web/project.json`:
  - Prepend the two `ddd bc/ul` commands to `test:quick.options.commands`.
  - Set `test:quick.options.parallel` to `false`.
  - Add the three new `inputs` paths.
  - Add `rhino-cli` to `implicitDependencies` if absent.
- [ ] **5.3** Add new `spec-coverage` target per `tech-docs.md`.
- [ ] **5.4 GREEN** `nx run wahidyankf-web:test:quick` runs DDD validators first, then vitest + coverage. All green.
- [ ] **5.5 GREEN** `nx run wahidyankf-web:spec-coverage` reports 0 step gaps.

---

## Phase 6 — Documentation cross-links

- [ ] **6.1** Update `apps/wahidyankf-web/README.md`:
  - Replace any "specs at `specs/apps/wahidyankf/fe/`" reference with the new path.
  - Add a "Specs" section linking to the five-folder tree, the ddd registry, and the glossaries.
- [ ] **6.2** Update `specs/apps/wahidyankf/README.md` to use the same structure as `specs/apps/organiclever/README.md` — sample tree block, container table, bounded-contexts table with Gherkin counts, links to ddd/, etc.
- [ ] **6.3** `npm run lint:md` — fix any new violations.

---

## Phase 7 — Final validation gate

> **Important**: Fix ALL failures found during quality gates, not just those caused by your changes. This follows the root cause orientation principle — proactively fix preexisting errors encountered during work.

- [ ] **7.1** `rhino-cli specs validate-tree wahidyankf` — 0 findings.
- [ ] **7.2** `rhino-cli specs validate-counts specs/apps/wahidyankf` — 0 findings.
- [ ] **7.3** `rhino-cli specs validate-links specs/apps/wahidyankf` — 0 findings.
- [ ] **7.4** `rhino-cli specs validate-adoption wahidyankf` — 0 findings.
- [ ] **7.5** `rhino-cli ddd bc wahidyankf` — 0 findings.
- [ ] **7.6** `rhino-cli ddd ul wahidyankf` — 0 findings.
- [ ] **7.7** `nx run wahidyankf-web:test:quick` — 0 findings, coverage ≥80%.
- [ ] **7.8** `nx run wahidyankf-web:spec-coverage` — 0 step gaps.
- [ ] **7.9** `nx run wahidyankf-web-fe-e2e:test:e2e` — every E2E scenario passes against `nx dev wahidyankf-web` (manually started, not Vercel).
- [ ] **7.10** `nx affected -t typecheck lint test:quick spec-coverage --base=HEAD~1` from worktree root — full pre-push gate green.
- [ ] **7.11** `npm run lint:md` — 0 violations.

### Manual UI Verification (Playwright MCP)

- [ ] **7.12** Start dev server: `nx dev wahidyankf-web` (listens at `localhost:3201`).
- [ ] **7.13** Navigate to the home page via `browser_navigate` to `http://localhost:3201/` — confirm page renders without blank sections.
- [ ] **7.14** `browser_snapshot` — verify bounded-context-organized components render correctly (hero, featured-project teaser visible).
- [ ] **7.15** Navigate to `/cv` and `/personal-projects`; `browser_snapshot` each — confirm pages render with no layout regressions from the source refactor.
- [ ] **7.16** `browser_console_messages` — must show 0 JS errors across all three pages.
- [ ] **7.17** `browser_take_screenshot` for each page — attach for visual record.

---

## Phase 8 — Commit, push, archive

### Commit Guidelines

- [ ] Commit changes thematically — group related changes into logically cohesive commits.
- [ ] Follow Conventional Commits format: `<type>(<scope>): <description>`.
- [ ] Split different domains/concerns into separate commits (e.g., spec scaffolding separate from source refactor separate from project.json wiring).
- [ ] Do NOT bundle unrelated fixes into a single commit.

- [ ] **8.1** Single atomic commit (or commit-per-phase if maintainer prefers; both legal):
  - Message: `feat(wahidyankf-web): adopt C4 + DDD specs format`
  - Body lists: 5 bounded contexts, source refactor scope, DDD validator wiring, spec-coverage gate.
- [ ] **8.2** Push the worktree branch through whichever publish path applies (direct-to-main per Trunk Based Development is the default for `ose-public`; draft PR optional).
- [ ] **8.3** Wait for `main` CI green — specifically monitor the `CI` workflow at `https://github.com/wahidyankf/ose-public/actions` for the push commit. Per `governance/development/workflow/ci-monitoring.md`.
- [ ] **8.4** Move this plan folder to `plans/done/YYYY-MM-DD__wahidyankf-web-ddd-and-specs-format/` (date prefix added at archival per `governance/conventions/structure/plans.md`).
- [ ] **8.5** Update `plans/in-progress/README.md` and `plans/done/README.md` indices.
- [ ] **8.6** If `bdd-ddd-tooling-gap-fill` is still in-progress or backlog, post a note in its `delivery.md` Phase 0 confirming wahidyankf is now allowlist-eligible.
