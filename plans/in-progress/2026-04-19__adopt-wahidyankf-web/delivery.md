# Delivery — Adopt wahidyankf-web

Each phase ends with **one Conventional-Commits commit** and **one
`git push`**, per the user requirement. Every checkbox is one concrete,
independently verifiable action. The plan-execution workflow drives the
list top-down; do not re-order within a phase unless a step fails and
requires triage.

## Preconditions (verified once before P0)

- [ ] Confirm the current session is inside `ose-public/.claude/worktrees/<name>/`.
- [ ] Confirm the worktree branch follows the `worktree-<name>` naming convention (Subrepo Worktree Workflow, Standard 14).
- [ ] Confirm the worktree branch is not `main` directly (Standard 14 prohibits direct commits to `main` from a worktree).
- [ ] Confirm a draft PR against `origin/main` has been or will be opened before the first push from this worktree.
- [ ] Confirm `origin` points at `wahidyankf/ose-public`.
- [ ] Confirm `git status` is clean.
- [ ] Run `npm install` from workspace root to install dependencies.
- [ ] Run `npm run doctor -- --fix` to converge the full polyglot toolchain (Go, F#, etc.). The `postinstall` hook runs `doctor || true` and silently tolerates drift; explicit `doctor --fix` is the only action that guarantees all toolchains are available. This is required before any `test:quick` call because that target invokes `rhino-cli` (Go binary).

## Phase P0 — Prep & Gap Resolution

- [ ] Fetch the upstream `wahidyankf/oss` repo state at HEAD via `gh api repos/wahidyankf/oss/contents/apps-standalone/wahidyankf-web` and record the commit SHA in `plans/in-progress/2026-04-19__adopt-wahidyankf-web/prep-notes.md` (temporary file; deleted in P7).
- [ ] Read upstream `LICENSE` file and confirm MIT-compatibility; record the license string in `prep-notes.md`.
- [ ] Record the confirmed production domain (user-supplied; default placeholder `www.wahidyankf.com`) in `prep-notes.md`.
- [ ] Confirm dev port `3201` is unused by any `nx.json` / existing `apps/*/project.json` via `rg -n "3201" nx.json apps`.
- [ ] For each row of the `tech-docs.md` dependency upgrade matrix, run `npm view <pkg> version` AND `jq -r '.dependencies["<pkg>"] // .devDependencies["<pkg>"]' apps/ayokoding-web/package.json apps/oseplatform-web/package.json apps/organiclever-fe/package.json` and record both live-latest and current sibling pins in `prep-notes.md`. Update `tech-docs.md` rows in-place only when sibling pins have already moved — otherwise preserve sibling parity per the stack-parity rule.
- [ ] Confirm the tag vocabulary extension (`domain:wahidyankf`) has no conflicting allowlist hard-coded anywhere: run `rg -n "domain:(ayokoding|oseplatform|organiclever|demo-be|demo-fe|tooling)" --type ts --type json --type md | grep -v generated-reports`.
- [ ] Record the hit list from the previous step in `prep-notes.md` so P6 knows every place to extend the `domain:` allowed values list.
- [ ] Commit: `docs(plans): record wahidyankf-web adoption prep notes`.
- [ ] Push to origin worktree branch.

## Phase P1 — Scaffold & Port Source

- [ ] Create directory `apps/wahidyankf-web/` with subdirectories `src/app/`, `src/components/`, `src/utils/`, `src/test/`, `test/unit/`, `public/`.
- [ ] Create `apps/wahidyankf-web/package.json` with initial deps at `apps/ayokoding-web/package.json` test-stack pins (the closest sibling content platform) — use that file as the source of truth for `vitest`, `jsdom`, `@vitejs/plugin-react`, `vite-tsconfig-paths`, `@amiceli/vitest-cucumber`, `tailwindcss`, `@tailwindcss/postcss`, `@testing-library/*`, `@vitest/coverage-v8`, `@types/node`, `@types/react*`, `typescript`. We bump as needed in P2; this commit's lock must be internally consistent.
- [ ] Create `apps/wahidyankf-web/tsconfig.json` extending `../../tsconfig.base.json`.
- [ ] Create `apps/wahidyankf-web/next.config.ts` with `output: "standalone"` and `images.unoptimized: true`.
- [ ] Create `apps/wahidyankf-web/oxlint.json` (copy from `apps/organiclever-fe/oxlint.json`).
- [ ] Create `apps/wahidyankf-web/postcss.config.mjs` (Tailwind 4 style).
- [ ] Create `apps/wahidyankf-web/vitest.config.ts` by copying `apps/ayokoding-web/vitest.config.ts` as the template — thresholds 80/80/80/80 (lines/functions/branches/statements); projects `unit-fe` (jsdom) + `integration` (node); `setupFiles: ["./src/test/setup.ts"]` on the jsdom project. Omit the `unit` (node) project until node-only code exists in the app.
- [ ] Create `apps/wahidyankf-web/project.json` with the Nx target shape from `tech-docs.md` (tags include `domain:wahidyankf`; real validity of that tag is fixed in P6 — until then, `repo-rules-checker` is expected to flag it).
- [ ] Copy upstream `src/app/page.tsx` → `apps/wahidyankf-web/src/app/page.tsx`; update imports to `@/` alias.
- [ ] Copy upstream `src/app/layout.tsx` → `apps/wahidyankf-web/src/app/layout.tsx`.
- [ ] Copy upstream `src/app/head.tsx` → `apps/wahidyankf-web/src/app/head.tsx`.
- [ ] Copy upstream `src/app/globals.css` → `apps/wahidyankf-web/src/app/globals.css` (Tailwind-3 syntax at this phase; P2 rewrites for Tailwind 4).
- [ ] Copy upstream `src/app/favicon.ico`.
- [ ] Copy upstream `src/app/fonts/` directory.
- [ ] Copy upstream `src/app/data.ts`.
- [ ] Copy upstream `src/app/cv/page.tsx`.
- [ ] Copy upstream `src/app/personal-projects/page.tsx`.
- [ ] Copy upstream `src/components/HighlightText.tsx`.
- [ ] Copy upstream `src/components/Navigation.tsx`.
- [ ] Copy upstream `src/components/ScrollToTop.tsx`.
- [ ] Copy upstream `src/components/SearchComponent.tsx`.
- [ ] Copy upstream `src/components/ThemeToggle.tsx`.
- [ ] Copy upstream `src/utils/search.ts`.
- [ ] Copy upstream `src/utils/markdown.tsx` (note: upstream extension is `.tsx`, not `.ts`).
- [ ] Copy upstream `src/utils/style.ts`.
- [ ] Copy upstream `src/utils/style.test.ts` → `apps/wahidyankf-web/src/utils/style.unit.test.ts` (rename per `.unit.test.*` convention).
- [ ] Copy upstream `public/` contents.
- [ ] Create `apps/wahidyankf-web/LICENSE` from upstream (MIT-compatible).
- [ ] Create `apps/wahidyankf-web/README.md` modelled on `apps/organiclever-fe/README.md` with the app's overview, dev commands, test commands, tech stack.
- [ ] Create `apps/wahidyankf-web/.gitignore` mirroring `apps/organiclever-fe/.gitignore`.
- [ ] Run `npm install` from workspace root.
- [ ] Run `npm run doctor -- --fix` to ensure Go and other polyglot toolchains are available (required for `test:quick` which calls `rhino-cli`).
- [ ] Run `nx build wahidyankf-web` and confirm success.
- [ ] Run `nx run wahidyankf-web:typecheck` and confirm success.
- [ ] Commit: `feat(wahidyankf-web): scaffold Nx app and port source`.
- [ ] Push.

## Phase P2 — Upgrade Dependencies

- [ ] Open `apps/wahidyankf-web/package.json`; update every row to the value in `tech-docs.md`'s Dependency Upgrade Matrix (which is already aligned to `ayokoding-web`/`oseplatform-web` pins as of 2026-04-19).
- [ ] Remove the `eslint`, `eslint-config-next`, `eslint-plugin-vitest-globals`, `prettier`, `husky`, `lint-staged`, `vite`, `@vitest/ui`, `tailwindcss-animate` rows.
- [ ] Add `@amiceli/vitest-cucumber ^6.3.0`, `@testing-library/jest-dom ^6.0.0`, `@testing-library/react ^16.0.0`, `@vitest/coverage-v8 ^4.0.0`, `vite-tsconfig-paths ^5.0.0`, `@tailwindcss/postcss ^4.0.0` rows (all exact siblings' pins).
- [ ] Diff `apps/wahidyankf-web/package.json` test-stack rows against `apps/ayokoding-web/package.json` and `apps/oseplatform-web/package.json`: `jq -r '.devDependencies | to_entries | map(select(.key | test("(vitest|testing|jsdom|@tailwind|@vitejs|vite-tsconfig|@amiceli|@types|typescript)"))) | .[] | "\(.key)=\(.value)"' apps/{wahidyankf-web,ayokoding-web,oseplatform-web}/package.json`. Values on the three rows must match exactly; fix any delta.
- [ ] Run `npm install` from workspace root.
- [ ] Run `npm run doctor -- --fix` to re-verify toolchain convergence after the lock-file change.
- [ ] Run `npx @next/codemod@canary upgrade latest apps/wahidyankf-web` to apply Next.js 14→16 codemods.
- [ ] Run `npx @tailwindcss/upgrade@latest apps/wahidyankf-web` to apply Tailwind 3→4 migration.
- [ ] Manually edit `apps/wahidyankf-web/src/app/globals.css` if the upgrade tool missed the `@import "tailwindcss";` replacement.
- [ ] Manually edit any React-19-specific breakages surfaced by `tsc --noEmit`.
- [ ] Run `nx run wahidyankf-web:typecheck`; fix failures in place.
- [ ] Run `nx build wahidyankf-web`; fix failures in place.
- [ ] Run `nx dev wahidyankf-web` and verify manually that `/`, `/cv`, `/personal-projects` return HTTP 200. Use Playwright MCP if available: `browser_navigate` to each URL, `browser_snapshot` to inspect DOM, `browser_console_messages` to confirm zero JS errors, `browser_take_screenshot` for visual record, `browser_click` on the theme toggle to verify both themes render. Otherwise use `curl http://localhost:3201/` + `curl http://localhost:3201/cv` + `curl http://localhost:3201/personal-projects`.
- [ ] Commit: `chore(wahidyankf-web): upgrade dependencies to 2026-04 stable`.
- [ ] Push.

## Phase P3 — Unit Tests + Gherkin

- [ ] Port upstream `src/test/setup.ts` into `apps/wahidyankf-web/src/test/setup.ts` (upstream file already exists — update its imports to `@testing-library/jest-dom ^6.0.0` if needed for Vitest 4 compatibility).
- [ ] Rename every ported `*.test.tsx` / `*.test.ts` file to `*.unit.test.tsx` / `*.unit.test.ts` (including `src/utils/markdown.test.tsx` → `src/utils/markdown.unit.test.tsx`).
- [ ] Port each upstream test's internals to Vitest 4 + Testing Library 16 API (e.g., `screen.findByRole` instead of legacy patterns).
- [ ] Add `apps/wahidyankf-web/src/components/ThemeToggle.unit.test.tsx` covering theme-toggle behaviour (upstream lacked a test; filling the coverage gap to reach **80%**).
- [ ] Create `specs/apps/wahidyankf/README.md` describing the domain and the `@amiceli/vitest-cucumber` BDD framework.
- [ ] Create `specs/apps/wahidyankf/fe/gherkin/home.feature` matching the Home scenarios from `prd.md`.
- [ ] Create `specs/apps/wahidyankf/fe/gherkin/search.feature`.
- [ ] Create `specs/apps/wahidyankf/fe/gherkin/cv.feature`.
- [ ] Create `specs/apps/wahidyankf/fe/gherkin/theme.feature`.
- [ ] Create `specs/apps/wahidyankf/fe/gherkin/personal-projects.feature` matching the Personal projects scenarios from `prd.md`.
- [ ] Create `apps/wahidyankf-web/test/unit/steps/home.steps.ts` implementing `home.feature` steps against the rendered component tree.
- [ ] Create `apps/wahidyankf-web/test/unit/steps/search.steps.ts`.
- [ ] Create `apps/wahidyankf-web/test/unit/steps/cv.steps.ts`.
- [ ] Create `apps/wahidyankf-web/test/unit/steps/theme.steps.ts`.
- [ ] Create `apps/wahidyankf-web/test/unit/steps/personal-projects.steps.ts`.
- [ ] Run `nx run wahidyankf-web:test:unit`; fix failures in place.
- [ ] Run `nx run wahidyankf-web:test:quick`; confirm exit 0 and that `apps/wahidyankf-web/coverage/lcov.info` exists and passes the **80%** line threshold (aligned to `ayokoding-web` / `oseplatform-web`). If coverage falls short, add targeted unit tests for uncovered branches in `src/utils/` and `src/components/` before committing — do NOT lower the threshold.
- [ ] Commit: `test(wahidyankf-web): port unit tests and add Gherkin acceptance specs`.
- [ ] Push.

## Phase P4 — E2E Runner

- [ ] Create `apps/wahidyankf-web-e2e/` with files `package.json`, `project.json`, `tsconfig.json`, `playwright.config.ts`, `README.md`, `.gitignore`, `steps/` directory.
- [ ] `package.json` deps per `tech-docs.md` E2E row.
- [ ] `playwright.config.ts` mirrors `apps/organiclever-fe-e2e/playwright.config.ts` with `baseURL` default `http://localhost:3201` and `featuresRoot` pointing at `../../specs/apps/wahidyankf/fe/gherkin`.
- [ ] `project.json` tags: `["type:e2e", "platform:playwright", "lang:ts", "domain:wahidyankf"]`; `implicitDependencies: ["wahidyankf-web"]`.
- [ ] Create `specs/apps/wahidyankf/fe/gherkin/accessibility.feature` per the `prd.md` Accessibility feature.
- [ ] Create `apps/wahidyankf-web-e2e/steps/home.steps.ts`, `cv.steps.ts`, `personal-projects.steps.ts`, `accessibility.steps.ts`.
- [ ] `accessibility.steps.ts` uses `@axe-core/playwright` with `.withTags(["wcag2a", "wcag2aa"])`.
- [ ] Run `nx run wahidyankf-web-e2e:install`.
- [ ] Start the app in one shell: `nx dev wahidyankf-web`.
- [ ] Run `nx run wahidyankf-web-e2e:test:e2e` in a second shell; confirm all scenarios pass.
- [ ] Stop the dev server.
- [ ] Commit: `test(wahidyankf-web-e2e): add Playwright-BDD runner with a11y smoke`.
- [ ] Push.

## Phase P5 — Quality Gates

- [ ] Ensure `apps/wahidyankf-web/project.json` `test:quick` inputs include `default` and `{workspaceRoot}/specs/apps/wahidyankf/fe/gherkin/**/*.feature`.
- [ ] Ensure `apps/wahidyankf-web/project.json` `test:unit` inputs include `default` and the same Gherkin glob.
- [ ] Ensure `apps/wahidyankf-web/project.json` `spec-coverage` input globs include the feature file tree and the app's `src/**/*.{ts,tsx}` tree.
- [ ] Ensure `apps/wahidyankf-web-e2e/project.json` `spec-coverage` input globs include the same feature tree and the runner's `**/*.ts` tree.
- [ ] Run `nx run wahidyankf-web:spec-coverage`; fix any missing step definitions.
- [ ] Run `nx run wahidyankf-web-e2e:spec-coverage`; fix any missing step definitions.
- [ ] Run `nx affected -t typecheck lint test:quick spec-coverage`; confirm exit 0.
- [ ] Run `npm run lint:md` and fix any markdown issues in new READMEs or spec READMEs.
- [ ] Commit: `ci(wahidyankf-web): wire typecheck, lint, spec-coverage, pre-push gate`.
- [ ] Push.

## Phase P6 — Deployment Wiring

- [ ] Create `apps/wahidyankf-web/vercel.json` as specified in `tech-docs.md`.
- [ ] Create `apps/wahidyankf-web/Dockerfile` by copying `apps/organiclever-fe/Dockerfile` and updating `WORKDIR`/port/app-name tokens.
- [ ] Create `apps/wahidyankf-web/.dockerignore` by copying `apps/organiclever-fe/.dockerignore`.
- [ ] Create `.github/workflows/test-and-deploy-wahidyankf-web.yml` per `tech-docs.md`.
- [ ] Verify `.github/workflows/_reusable-test-and-deploy.yml` already handles an arbitrary `app-name` / `prod-branch` input (no change expected; flag if it does not).
- [ ] Create `.claude/agents/apps-wahidyankf-web-deployer.md` by copying `.claude/agents/apps-organiclever-fe-deployer.md` and replacing `organiclever-fe` → `wahidyankf-web` and `prod-organiclever-web` → `prod-wahidyankf-web` throughout.
- [ ] Run `npm run sync:claude-to-opencode` so `.opencode/agent/apps-wahidyankf-web-deployer.md` lands.
- [ ] Update `governance/development/infra/nx-targets.md` — add `wahidyankf` to the `domain:` allowed values row, and add `wahidyankf-web` + `wahidyankf-web-e2e` rows to the "Current Project Tags" table.
- [ ] Update `governance/conventions/formatting/emoji.md` (and any other convention enumerating allowed-file categories) if `.claude/agents/` hasn't already been listed — verify only.
- [ ] **Prerequisite**: Confirm the worktree's draft PR has been merged to `origin/main` before creating `prod-wahidyankf-web`. Run `git fetch origin` and `git ls-remote origin main` to verify `origin/main` reflects the P5 commit SHA before proceeding. Do NOT run the next step from the worktree branch — all P6 push commands below operate on `origin/main`, not the worktree branch.
- [ ] From a clean `main` (after PR merge and local pull), create the production branch: `git push origin main:prod-wahidyankf-web`.
- [ ] Confirm remote branch exists: `git ls-remote origin prod-wahidyankf-web`.
- [ ] Run `nx affected -t typecheck lint test:quick spec-coverage` once more; confirm exit 0.
- [ ] Commit: `ci(wahidyankf-web): add Vercel deploy workflow and deployer agent`.
- [ ] Push.
- [ ] After pushing, monitor the CI run: `gh run list --workflow=test-and-deploy-wahidyankf-web.yml --limit 1`; then `gh run watch <run-id>`. If the workflow fails, fix the root cause and push a follow-up commit. Do NOT proceed to P7 until CI is green.

## Phase P7 — Docs & Close-out

- [ ] Update top-level `CLAUDE.md` — add `apps/wahidyankf-web/` and `apps/wahidyankf-web-e2e/` to the Current Apps list, and add a `### wahidyankf-web` subsection under "Web Sites" with URL, framework, dev port, prod branch, and command block.
- [ ] Update top-level `README.md` to mention the new app alongside the other three web apps.
- [ ] Update `apps/README.md` inventory.
- [ ] Update `docs/how-to/add-new-app.md` ONLY if the port surfaced a genuinely missing step (do not edit speculatively).
- [ ] Run `nx affected -t typecheck lint test:quick spec-coverage`; confirm exit 0.
- [ ] Run `npm run lint:md`; confirm zero errors.
- [ ] Run final visual verification on `nx dev wahidyankf-web`: use Playwright MCP — `browser_navigate` to each of `/`, `/cv`, `/personal-projects`; `browser_snapshot` to inspect DOM structure; `browser_console_messages` to confirm zero JS errors; `browser_take_screenshot` for visual record. Toggle theme with `browser_click` on the theme toggle and re-snapshot to verify both dark and light themes render correctly. If Playwright MCP is unavailable, perform the same checks via a regular browser session and record observations in a comment.
- [ ] Delete the temporary `plans/in-progress/2026-04-19__adopt-wahidyankf-web/prep-notes.md`.
- [ ] Move `plans/in-progress/2026-04-19__adopt-wahidyankf-web/` → `plans/done/YYYY-MM-DD__adopt-wahidyankf-web/` where `YYYY-MM-DD` is today's date at time of execution: `git mv plans/in-progress/2026-04-19__adopt-wahidyankf-web plans/done/$(date +%Y-%m-%d)__adopt-wahidyankf-web`.
- [ ] Update `plans/in-progress/README.md` to remove the plan from the active list.
- [ ] Update `plans/done/README.md` to include the plan in the completed list.
- [ ] Commit: `docs(wahidyankf-web): add to platform docs and close adoption plan`.
- [ ] Push.
- [ ] After pushing, monitor the CI run: `gh run list --workflow=test-and-deploy-wahidyankf-web.yml --limit 1`; then `gh run watch <run-id>`. If the workflow fails, fix the root cause and push a follow-up commit before declaring the phase done.

## Quality Gates (enforced every phase)

- `nx affected -t typecheck lint test:quick spec-coverage` must exit 0 before every push. If any phase's push violates this, revert before pushing and triage.
- `npm run lint:md` must exit 0 on markdown changes.
- Markdown-formatted files (including this plan) are auto-formatted by the repo's Prettier pre-commit hook.

> **Important**: Fix ALL failures found during quality gates — including preexisting failures not caused by the current phase's changes. This follows the root cause orientation principle: proactively fix preexisting errors encountered during work. Never use `--no-verify` or lower a coverage threshold to get a push through.

## Verification

The plan is done when:

- `apps/wahidyankf-web/` and `apps/wahidyankf-web-e2e/` exist on `main`.
- `origin/prod-wahidyankf-web` exists and is an exact copy of `main` at the P6 commit.
- `.claude/agents/apps-wahidyankf-web-deployer.md` and its `.opencode/` mirror exist.
- `.github/workflows/test-and-deploy-wahidyankf-web.yml` exists and references the app-name and prod-branch correctly.
- `nx affected -t typecheck lint test:quick spec-coverage` exits 0 on the commit that concludes P7.
- `rhino-cli test-coverage validate apps/wahidyankf-web/coverage/lcov.info 80` exits 0.
- The testing-stack rows of `apps/wahidyankf-web/package.json` exactly match the corresponding rows in `apps/ayokoding-web/package.json` and `apps/oseplatform-web/package.json`.
- All Gherkin ACs from `prd.md` are represented in `specs/apps/wahidyankf/fe/gherkin/*.feature`.
- `plans/in-progress/2026-04-19__adopt-wahidyankf-web/` has been moved to `plans/done/`.
