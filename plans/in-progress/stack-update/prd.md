# Product Requirements Document

## Product Overview

This plan brings the entire `ose-public` monorepo to the latest stable versions of all
runtimes, toolchains, and dependencies as of 2026-05-15. It resolves known security CVEs
in Next.js and Spring Boot, fixes preexisting CI breakage (missing `setup-jvm` composite
action), corrects stale rhino-cli doctor paths, and updates all apps to policy-compliant
pinned versions per the
[Dependency Bump Stability & Safety Policy](../../repo-governance/development/workflow/dependency-bump-policy.md)
(cutoff: 2026-03-16).

The one MAJOR library migration executed in this plan is **Shiki 4.0.2** (Phase 10, Path B
pre-cutoff). TypeScript stays on the **5.x stay-on-line** (5.8.3, Path B); ESLint stays on
the **9.x stay-on-line**. Zod 4, lucide-react 1.x, and @xstate/react 6 are all
**DEFERRED** to future plans — see `plans/ideas.md` for tracking notes.

The product outcome is a monorepo where:
- `npm run doctor` returns all-OK with accurate data
- CI pipeline is fully operational (no broken composite actions)
- All apps build, typecheck, lint, and test clean against the updated toolchain
- No known CVEs remain in any user-facing package

---

## Personas

**Solo maintainer (primary user)**: A single developer who both authors this plan and
executes it. Wears all hats: developer, security auditor, DevOps operator. The delivery
checklist is the primary artifact consumed — it must be executable without additional
lookup. Agent executors (`plan-executor`) execute the checklist on behalf of the maintainer.

**Agent executor (secondary user)**: A sonnet-tier Claude agent executing the delivery
checklist step by step. Requires: explicit file paths, verbatim shell commands, and
observable acceptance criteria for every checkbox. Cannot handle ambiguity or "decide"
instructions.

---

## User Stories

**As the solo maintainer**, I want all runtimes and dependencies updated to latest stable,
so that the monorepo does not accumulate security debt or version drift.

**As the solo maintainer**, I want the CI pipeline fully operational after this plan,
so that future PRs against Java-tagged projects (`organiclever-be`) pass CI automatically.

**As the solo maintainer**, I want `npm run doctor` to return accurate runtime version data,
so that I can trust the health tool and detect future drift early.

**As the agent executor**, I want each delivery step to contain explicit file paths and
commands, so that I can execute the step without consulting external sources or making
judgment calls.

---

## Product Scope

### In Scope

- All runtime version bumps (Node.js, Go, .NET, Erlang, Rust, Dart, Java, Elixir) to
  policy-compliant eligible versions per the dependency-bump policy (cutoff 2026-03-16)
- All npm/package.json dependency bumps for all 11 non-E2E apps, 9 E2E apps, and 4 libs
  to exact pinned versions (no carets/tildes)
- All Go module updates (`go get -u ./...` + `go mod tidy`) for all 5 Go modules
- Spring Boot and Maven dependency updates for `organiclever-be`
- **TypeScript 5.8.3 stay-on-line** (Path B; TS 6.0 is post-cutoff — deferred)
- **ESLint 9.x stay-on-line** + react-hooks 5.x stay + @typescript-eslint 8.57.0
  (ESLint 10 + react-hooks 7 are post-cutoff — deferred)
- **Shiki 4.0.2 migration** (Phase 10 — the one MAJOR migration in this plan)
- **Zod 3.x stay-on-line** (Zod 4 is post-cutoff — deferred to future plan)
- **lucide-react 0.577.0 stay-on-0.x** (1.x is post-cutoff — deferred)
- **@xstate/react 5.x stay-on-line** (6.x is post-cutoff — deferred)
- Storybook 10.2.10 (DOWNGRADE from ^10.3.3; CVE-2026-27148 patch at 10.2.10),
  @effect/platform `0.94.5` (Path B), @vitejs/plugin-react 6.0.1 migrations
- Preexisting bug fixes: doctor paths, playwright-bdd alignment, vitest mismatch,
  AGENTS.md correction, missing CI composite actions, CI go-version pins
- CI composite action `setup-jvm` creation and unreachable job cleanup

### Out of Scope

- Volta → `mise` migration (separate future plan)
- TypeScript 6.0 migration (entirely deferred — post-cutoff; tracked in `plans/ideas.md`)
- ESLint 10 + react-hooks 7 migration (entirely deferred — post-cutoff)
- Zod 4.x migration (entirely deferred — post-cutoff)
- lucide-react 1.x migration (entirely deferred — post-cutoff)
- @xstate/react 6.x migration (entirely deferred — post-cutoff)
- Effect v4 beta adoption (stay on v3 stable)
- Upgrading Erlang from OTP 27 to OTP 28 (stay on OTP 27 patch branch at 27.3.3)
- Any new feature development (this is a pure maintenance/upgrade plan)

---

## Requirements

### R0 — Dependency Bump Policy Compliance (META)

Every version pinned in this plan complies with the
[Dependency Bump Stability & Safety Policy](../../repo-governance/development/workflow/dependency-bump-policy.md).
Each pin is classified Path A (LTS), Path B (≥60-day pre-cutoff stable + CVE-clean),
or Path C (security-override waiver). Cutoff date: **2026-03-16**. Waivers are documented
in `tech-docs.md §Security Waivers`.

### R1 — Runtime Language Versions (Path A LTS for Node/Java/.NET; Path B for others)

All language runtimes installed on the dev machine and pinned in config files must reach
the eligible versions defined in [tech-docs.md](./tech-docs.md) §Version Table. Machine
install commands are provided in `tech-docs.md §Machine Install Sequence`. Notable
post-cutoff downgrades from "latest":

- Go: 1.26.3 (post-cutoff) → **1.25.8** (Path B)
- Rust: 1.95.0 (post-cutoff) → **1.93.0** (Path B)
- Dart: 3.11.5 (post-cutoff) → **3.11.0** (Path B)
- Flutter: 3.41.9 (post-cutoff) → **3.41.0** (Path B)
- Erlang: 27.3.4.11 (post-cutoff) → **27.3.3** (Path B)
- npm: 11.14.1 (post-cutoff) → **11.11.0** (Path B)
- golangci-lint: 2.12.2 (post-cutoff) → **2.11.3** (Path B)

### R2 — Node.js / npm Volta Pin

`package.json` (root) `volta` block must reflect `node: 24.15.0` and `npm: 11.11.0`
(Path B; 11.14.1 is post-cutoff — deferred). Volta must resolve those versions locally
after pin update.

### R3 — .NET SDK Pin

`apps/ose-app-be/global.json` `sdk.version` must be `10.0.300`. `dotnet --version` after
install must return `10.0.300`.

### R4 — Go Runtime

`go.mod` directive `go 1.26` remains (minimum language level, not toolchain version).
Machine binary updated to `go1.25.8` (Path B; 1.26.3 is post-cutoff). All Go apps pass
`go vet ./...` and `golangci-lint run` with the new version.

### R5 — Nx Workspace

Root `package.json` `nx` devDependency: `22.5.4` (Path B; 22.7.2 is post-cutoff). All `@nx/*` packages at the same
version. `nx affected -t build` produces zero errors.

### R6 — Next.js Apps (all 5)

`next` dep in each app's `package.json` pinned to `16.2.6` (exact, not range). Dev server
starts; production build succeeds; no deprecation warnings from 16.1.6 → 16.2.6.

Apps: `ayokoding-web`, `ose-web`, `organiclever-web`, `wahidyankf-web`, `ose-app-web`.

### R7 — React 19.2.6 (Path C WAIVER)

React pinned to `19.2.6` (exact; Path C waiver — CVE-2025-55182 requires post-cutoff patch)
in all Next.js app `package.json` files. `react-dom` version synchronized to `19.2.6`.

### R8 — TypeScript 5.8.3 Stay-on-Line (Path B; TS 6 DEFERRED)

All apps and libs pin TypeScript to `5.8.3` (exact, no caret). TS 6.0.3 (2026-03-23) is
post-cutoff and DEFERRED to a future plan once 6.x has 60+ days of soak.
All `tsconfig.json` files validated against TS 5.8.3 (from prior 5.6.0). `typecheck`
target passes for every project.

Key TS 5.8.3 changes from 5.6.0:

- Stricter function type inference may require explicit return types in some places.
- Index signature types may need `Record<string, unknown>` in stricter patterns.

DEFERRED (future plan): TS 6.0 breaking changes (`--out` removal,
`--suppressImplicitAnyIndexErrors` removal, `--noImplicitOverride` stricter enforcement).

### R9 — Tailwind CSS 4.2.1 Stay-on-Line (Path B; 4.3 DEFERRED)

Tailwind stays at `4.2.1` (exact, no caret) in all apps. Already at pre-cutoff
(2026-02-23). 4.3.0 (2026-05-08) is post-cutoff — DEFERRED to future plan.
No custom plugin breakages. Build succeeds.

### R10 — Vitest Alignment

- All apps: `vitest` and `@vitest/coverage-v8` pinned to `4.1.0` (exact; Path B, released
  2026-03-12; 4.1.6 released 2026-05-12 is post-cutoff).
- `libs/web-ui`: bump from `^3.2.0` to `4.1.0` (exact; preexisting mismatch).
- `test:quick` passes for all affected projects.

### R11 — Playwright Alignment (all E2E apps)

All 9 E2E apps pinned to `@playwright/test: 1.60.0` and `playwright-bdd: 8.5.1`
(exact, no carets). Playwright browsers updated on machine (`npx playwright install`).
`test:e2e` passes for at least one app per suite type (BE/FE).

E2E apps: `ayokoding-web-be-e2e`, `ayokoding-web-fe-e2e`, `ose-web-be-e2e`,
`ose-web-fe-e2e`, `ose-app-web-e2e`, `ose-app-be-e2e`, `organiclever-web-e2e`,
`organiclever-be-e2e`, `wahidyankf-web-fe-e2e`.

### R12 — Shiki 4.x Migration

`shiki` updated to `^4.0.2` in `ayokoding-web` and `ose-web`. Shiki 4.x has a revised
async API; all usage sites migrated. Syntax highlighting renders correctly in dev mode.

### R13 — Zod 3.x Stay-on-Line (Path B; Zod 4 DEFERRED)

`zod` stays on 3.x in `ayokoding-web` and `ose-web`. Pin to `3.25.76` (Path B; latest
3.x release 2025-07-08). Zod 4.0 is post-cutoff — DEFERRED to a future plan.
All Zod schemas validated at runtime. `typecheck` passes.

### R14 — lucide-react 0.577.0 Stay-on-0.x (Path B; 1.x DEFERRED)

`lucide-react` pinned to `0.577.0` (exact, no caret) in `ayokoding-web`, `ose-web`,
`wahidyankf-web`, and `libs/web-ui`. 1.x is post-cutoff (1.0.1 ~2026-03-23) — DEFERRED
to a future plan. Build and Storybook stories render correctly.

### R15 — @xstate/react 5.x Stay-on-Line (Path B; 6.x DEFERRED)

`@xstate/react` stays on 5.x in `organiclever-web` and `ose-app-web`. Pin to `5.0.5`
(Path B; current pin already at latest 5.x version 2025-05-31). XState 5 core unchanged.
6.x is post-cutoff — DEFERRED to a future plan.

### R16 — Storybook 10.2.10 (DOWNGRADE; CVE patch)

`storybook` and all `@storybook/*` addons pinned to `10.2.10` (DOWNGRADE from current
`^10.3.3`). CVE-2026-27148 (High WebSocket hijacking) was patched at 10.2.10 (2026-02-25,
pre-cutoff). 10.3.x and 10.4.x are post-cutoff — deferred. Storybook build succeeds;
stories render for `libs/web-ui`.

### R17 — @effect/platform (latest pre-cutoff version)

`@effect/platform` updated to **latest pre-cutoff version** (verify on npm — likely in
0.85.x–0.95.x range; 0.96.1 from 2026-04-24 is post-cutoff and DEFERRED) in
`organiclever-web` and `ose-app-web`. Pin to exact version (no caret). Verify
changelog for breaking changes between current and target. TypeScript types resolve.

### R18 — lint-staged 16.4.0 Stay-on-16 (Path B; 17.x DEFERRED)

Root `package.json` `lint-staged` pinned to `16.4.0` (exact; released 2026-03-14, latest
pre-cutoff). lint-staged 17.x is post-cutoff — DEFERRED. Verify `.husky/` hook still works
with lint-staged 16.4.0 after pin update.

### R19 — golangci-lint 2.11.3 (Path B; pre-cutoff 2026-03-10)

- Machine binary updated to 2.11.3 (Path B; 2.12.2 is post-cutoff).
- `apps/rhino-cli/internal/doctor/tools.go` `readReq` hardcoded value updated from
  `"2.11.1"` to `"2.11.3"`.
- All Go apps pass `golangci-lint run`.

### R20 — Doctor Path Fixes (preexisting)

`apps/rhino-cli/internal/doctor/tools.go` `buildToolDefs` corrected:

- `pomXMLPath`: change from `apps/organiclever-be-jasb/pom.xml` →
  `apps/organiclever-be/pom.xml` (actual Java app).
- `globalJSONPath`: change from `apps/ose-grc-be/global.json` →
  `apps/ose-app-be/global.json` (actual .NET app).
- `pythonVersionPath`, `cargoTomlPath`, `pubspecPath`: demo apps extracted to `ose-primer`;
  keep `noReq` fallback but update source-comment strings to note extraction.

### R21 — AGENTS.md / CLAUDE.md Correction (preexisting)

`organiclever-be` description changed from "F#/Giraffe REST API" to
"Java/Spring Boot 4 REST API" in `AGENTS.md` and `CLAUDE.md`.

### R22 — Spring Boot / Maven (organiclever-be)

- Spring Boot parent updated from `4.0.4` → **`4.0.6`** (8 CVE patches per spring.io).
- Cucumber `cucumber.version` property updated from `7.34.2` → **`7.34.3`**.
- Java toolchain stays at 25 (correct).
- `mvn verify` passes unit tests; `nx run organiclever-be:test:quick` passes.

### R23 — markdownlint-cli2 + Prettier (Path B)

- `markdownlint-cli2` stays at `0.21.0` (exact; 0.22.x is post-cutoff — deferred).
- `prettier` pinned to `3.8.1` (exact; Path B released 2026-01-21; 3.8.2/3.8.3 are
  post-cutoff).
- `npm run lint:md` passes with zero errors.

### R24 — ESLint 9.x Stay-on-Line + @typescript-eslint 8.57.0 (Path B; ESLint 10 DEFERRED)

`eslint` stays on 9.x line (pin to `9.39.4`, Path B; 2026-03-06). ESLint 10 is post-cutoff
— DEFERRED. `eslint-plugin-react-hooks` stays at `5.1.0` (v7 requires ESLint 10 —
deferred). `@typescript-eslint/parser` pinned to `8.57.0` (Path B; 2026-03-09 pre-cutoff;
8.58.0+ post-cutoff). `eslint-plugin-boundaries` at `5.4.0`, `eslint-plugin-import` at
`2.32.0`. `nx affected -t lint` must pass with zero errors.

### R25 — @vitejs/plugin-react 6.0.1 (MAJOR; Path B)

`@vitejs/plugin-react` updated `^4.0.0` / `^5.1.4` → `6.0.1` (exact; Path B released
2026-03-13; 6.0.2 released 2026-05-14 is post-cutoff) in all consumers. v6 removes
Babel as a dependency in favor of Oxc-based React Refresh. Verify no app uses custom
Babel plugins through this package. Vitest tests using `@vitejs/plugin-react` (most apps
+ web-ui lib) must continue to pass.

### R26 — Other minor dep bumps (root + apps)

- `tsx` `^4.20.6` → `4.21.0` (exact; Path B; 4.22.0 is post-cutoff)
- `@hey-api/openapi-ts` `^0.94.2` → `0.94.2` (exact; stay; 0.94.3+ post-cutoff)
- `@hey-api/client-fetch` `^0.13.1` → `0.13.1` (exact; stay pre-cutoff)
- `@redocly/cli` `^2.22.1` → `2.22.1` (exact; stay pre-cutoff; 2.22.2+ post-cutoff)
- `@stoplight/spectral-cli` `^6.15.0` → `6.15.0` (exact; stay pre-cutoff)
- `@axe-core/playwright` `^4.10.1` → `4.11.1` (exact; released 2026-02-03, pre-cutoff; 4.11.2/4.11.3 post-cutoff)
- `@openapitools/openapi-generator-cli` `^2.30.2` → `2.30.2` (exact; stay pre-cutoff)

### R27 — CI workflow version pins

- `.github/workflows/_reusable-backend-e2e.yml`: `go-version: "1.26.0"` → `"1.25.8"`
  (Path B; not 1.26.3 which is post-cutoff).
- `.github/workflows/pr-validate-links.yml`: same fix to `"1.25.8"`.
- `node-version: "24"` already floating — no change.

### R28 — Missing CI composite actions (preexisting)

`.github/workflows/pr-quality-gate.yml` references 5 composite actions that do not exist.
Resolution options (pick per-action, may differ):

- **`setup-jvm`** — MUST be created (or replaced with inline `actions/setup-java@v4`
  steps). The `jvm` job is reachable today via `tag:lang:java` (organiclever-be).
- **`setup-rust`, `setup-elixir`, `setup-clojure`, `setup-flutter`** — currently unreached
  (no apps tagged with their `lang:*` tags). Acceptable resolutions: (a) delete the
  unreachable jobs from `pr-quality-gate.yml` until apps re-appear, OR (b) create stub
  composite actions, OR (c) guard the jobs with `if:` conditions that check for affected
  projects with the relevant tag.

### R29 — Hugo-commons Go module

`libs/hugo-commons/go.mod` participates in the Go module update sweep. After
`go get -u ./... && go mod tidy`, `npx nx run-many -t test:quick lint -p hugo-commons`
must pass.

### R30 — crane-cli .NET SDK alignment

After `global.json` bump, `nx build crane-cli` and `nx run crane-cli:test:quick` must
succeed under .NET SDK 10.0.300.

### R31 — Full CI Gate

After all updates: `npx nx affected -t typecheck lint test:quick spec-coverage` passes
for all affected projects from `origin/main` base. `npm run doctor` reports all tools OK.
GitHub Actions `pr-quality-gate.yml` `jvm` job runs to completion (assuming R28 resolved).

---

## Acceptance Criteria (Gherkin)

```gherkin
Feature: Stack update — all runtimes and deps at latest stable

  Background:
    Given the worktree branch "worktree/stack-update" is checked out
    And "npm install" has been run in the repo root

  Scenario: Node.js and npm pinned to latest LTS
    When I run "node --version"
    Then the output starts with "v24.15"
    When I run "npm --version"
    Then the output starts with "11.11"

  Scenario: Go binary updated to Path B pin
    When I run "go version"
    Then the output contains "go1.25.8"

  Scenario: .NET SDK updated
    When I run "dotnet --version"
    Then the output starts with "10.0.3"

  Scenario: Erlang/OTP patched
    When I run "erl -noshell -eval 'io:format("~s",[erlang:system_info(otp_release)]),halt()."
    Then the output is "27"
    When I check the installed OTP version via asdf
    Then the version is "27.3.3"

  Scenario: golangci-lint updated to Path B pin
    When I run "golangci-lint version"
    Then the output contains "2.11.3"

  Scenario: Next.js security CVEs resolved
    Given "apps/ayokoding-web/package.json" "dependencies.next" equals "16.2.6"
    And   "apps/ose-web/package.json" "dependencies.next" equals "16.2.6"
    And   "apps/organiclever-web/package.json" "dependencies.next" equals "16.2.6"
    And   "apps/wahidyankf-web/package.json" "dependencies.next" equals "16.2.6"
    And   "apps/ose-app-web/package.json" "dependencies.next" equals "16.2.6"
    When I run "nx build <app>"
    Then the build succeeds for each app

  Scenario: TypeScript 5.8.3 pinned across all TS projects (TS 6 deferred)
    Given all app and lib package.json files specify "typescript": "5.8.3"
    When I run "npx nx affected -t typecheck"
    Then zero type errors are reported

  Scenario: Playwright aligned to 1.60.0 in all E2E apps (exact pins)
    Given all E2E app package.json files specify "@playwright/test": "1.60.0"
    And   all E2E app package.json files specify "playwright-bdd": "8.5.1"
    When I run "npx playwright --version"
    Then the output contains "1.60"

  Scenario: Vitest aligned to 4.1.0 (exact pin) including web-ui lib
    Given "libs/web-ui/package.json" "devDependencies.vitest" is "4.1.0"
    When I run "npx nx run web-ui:test:quick"
    Then the tests pass with coverage

  Scenario: Doctor passes with zero failures
    When I run "npm run doctor"
    Then the output shows all tools as OK with no FAIL entries

  Scenario: Full quality gate passes
    When I run "npx nx affected -t typecheck lint test:quick spec-coverage"
    Then all targets complete with exit code 0

  Scenario: Doctor reads Java version from correct pom.xml
    Given "apps/rhino-cli/internal/doctor/tools.go" references "apps/organiclever-be/pom.xml"
    And the file "apps/organiclever-be-jasb/pom.xml" does not exist
    When I run "npm run doctor"
    Then the Java tool check reads version from "apps/organiclever-be/pom.xml"
```

---

## Product Risks

| Risk | Impact | Likelihood | Notes |
|---|---|---|---|
| Shiki 4.x theme rename causes blank code blocks on ayokoding-web / ose-web | MEDIUM | LOW | Phase 10 includes dev-server Playwright MCP verification; visible immediately |
| Next.js 16.2.6 (Path C waiver) introduces a regression despite CVE need | MEDIUM | LOW | Phase 6 includes build + test:quick + dev-server spot-check for all 5 apps |
| React 19.2.6 (Path C waiver) introduces a regression | MEDIUM | LOW | Phase 6 includes typecheck + test:quick; E2E tests cover main user flows |
| mermaid 11.15.0 (Path C waiver) introduces a regression | LOW | VERY LOW | Phase 2E includes lint:md and format:md check; mermaid renders in browser |
| Storybook 10.2.10 DOWNGRADE breaks web-ui component stories | LOW | LOW | Phase 7C includes Storybook build verification; 10.2.10 is a stable release |
| The 15-phase plan spans multiple sessions causing context loss | MEDIUM | HIGH | Each phase ends with a thematic commit; delivery checklist is the resume point |
| DEFERRED migrations (TS 6, ESLint 10, Zod 4, lucide-react 1.x, @xstate/react 6) accumulate technical debt | LOW | LOW | Tracked in plans/ideas.md; deferral is policy-compliant per 60-day rule |
