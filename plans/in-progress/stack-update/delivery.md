# Delivery Checklist

Execute phases in order. Within each phase, tasks are ordered by dependency.
Run `git add -p && git commit` at the end of each phase.

---

## Worktree

Worktree path: `worktrees/stack-update/`

Provision before execution (run from repo root):

```bash
claude --worktree stack-update
```

See [Worktree Path Convention](../../repo-governance/conventions/structure/worktree-path.md) and [Plans Organization Convention §Worktree Specification](../../repo-governance/conventions/structure/plans.md#worktree-specification).

---

## Environment Setup

- [ ] Provision worktree: `claude --worktree stack-update` (creates `worktrees/stack-update/` in repo root)
- [ ] Initialize toolchain in the worktree: `npm install && npm run doctor -- --fix` (see [Worktree Toolchain Initialization](../../repo-governance/development/workflow/worktree-setup.md))
- [ ] Verify dev server starts for a representative app: `npx nx dev ayokoding-web` → loads on `localhost:3101`
- [ ] Verify existing tests pass before making changes: `npx nx affected -t test:quick` — all pass

---

## Commit Guidelines

- Commit changes thematically — group related changes into logically cohesive commits
- Follow Conventional Commits format: `<type>(<scope>): <description>`
- Split different domains/concerns into separate commits (e.g., separate `fix(ci)` from `chore(deps)`)
- Do NOT bundle unrelated fixes into a single commit
- Example: separate `fix(doctor): update stale paths` from `chore(deps): bump playwright to 1.60`

---

## Pinning + Stability + Safety Policy (HARD RULE)

This plan complies with the repo-wide
[Dependency Bump Stability & Safety Policy](../../repo-governance/development/workflow/dependency-bump-policy.md).

**Cutoff date: 2026-03-16** (today − 60 days). Three paths:

- **Path A (LTS)** — Use latest LTS-line patch. CVE-clean confirmed. Recency irrelevant.
- **Path B (60-day stable + CVE-clean)** — Latest version released ≤ 2026-03-16 + CVE-clean.
- **Path C (Security waiver)** — Recent CVE patch with documented waiver.

**Pinning is exact (no carets/tildes/`latest`).** When this checklist says `react 19.2.6`,
the literal package.json value is `"react": "19.2.6"` (not `"^19.2.6"` or `"~19.2.6"`).

After every `package.json` edit, verify the absence of caret/tilde with:

```bash
grep -E '"\^|"~' apps/<changed-app>/package.json libs/<changed-lib>/package.json && echo "FAIL: caret/tilde found" || echo "OK: all exact"
```

---

## Security Clearance Reference

Every version pinned in this plan was audited against NVD, GitHub Security Advisories,
Snyk DB, and vendor security pages on 2026-05-15. Full audit table at
[tech-docs.md §Security Clearance Status](./tech-docs.md#security-clearance-status-audit-date-2026-05-15).

**One REMEDIATE-priority indirect dep**: `golang.org/x/image v0.18.0` (CVE-2026-33809,
CVE-2026-33812) — must be force-upgraded to **v0.39.0** in every Go module via `go.mod`
`require` directive. See Phase 3.X.

**One HIGH-priority Docker image change**: `eclipse-temurin:25-jdk-alpine` has 2 unfixed
binutils High CVEs in the Alpine layer (CVE-2025-69649, CVE-2025-69650 CVSS 7.5).
Switch `Dockerfile.integration` to `eclipse-temurin:25.0.3+9-jdk` (Ubuntu base) for
runtime stage. See Phase 4.X.

---

## Phase 0 — Machine Installs (eligible versions per policy)

> Bring the machine to the target runtime versions per the three-path policy. Most are
> Path B (latest pre-2026-03-16 stable + CVE-clean); LTS lines are Path A.

- [ ] **M0.1** Install Go **1.25.8** (Path B; pre-cutoff 2026-03-05; patches same CVEs as 1.26.3):
  `gimme 1.25.8` (or download from go.dev). `go version` → `go1.25.8`
- [ ] **M0.2** Install Erlang OTP **27.3.3** (Path B; pre-cutoff 2025-04-16; patches CVE-2025-32433):
  `asdf install erlang 27.3.3 && asdf global erlang 27.3.3`
- [ ] **M0.3** Install .NET SDK **10.0.300** (Path A — LTS; May 2026 Patch Tuesday):
  `brew upgrade dotnet && dotnet --version` → starts with `10.0.3`
- [ ] **M0.4** Install Java Temurin **25.0.3** (Path A — LTS):
  `source "$HOME/.sdkman/bin/sdkman-init.sh" && sdk install java 25.0.3+9-tem && sdk default java 25.0.3+9-tem`
- [ ] **M0.5** Install Rust **1.93.0** (Path B; pre-cutoff 2026-02-12):
  `rustup install 1.93.0 && rustup default 1.93.0 && rustc --version` → `1.93.0`
- [ ] **M0.6** Install Dart 3.11.0 / Flutter **3.41.0** (Path B; pre-cutoff 2026-02-10/11):
  `fvm install 3.41.0 && fvm global 3.41.0 && dart --version` → `3.11.0` AND `flutter --version` → `3.41.0`
- [ ] **M0.7** Install golangci-lint **2.11.3** (Path B; pre-cutoff 2026-03-10):
  `go install github.com/golangci/golangci-lint/cmd/golangci-lint@v2.11.3 && golangci-lint version` → `2.11.3`
- [ ] **M0.8** Update Playwright browsers: `npx playwright install`
- [ ] **M0.9** Install `govulncheck` for Go security scanning: `go install golang.org/x/vuln/cmd/govulncheck@latest && govulncheck -version`

---

## Phase 1 — Preexisting Bug Fixes

> Fix bugs that exist independently of any version bump. Commit separately.

### 1A — Doctor path fixes (rhino-cli)

File: `apps/rhino-cli/internal/doctor/tools.go`

- [ ] **1A.1** Change `pomXMLPath` from `apps/organiclever-be-jasb/pom.xml` → `apps/organiclever-be/pom.xml`
- [ ] **1A.2** Change `globalJSONPath` from `apps/ose-grc-be/global.json` → `apps/ose-app-be/global.json`
- [ ] **1A.3** Update golangci-lint `readReq` return value: `"2.11.1"` → `"2.11.3"` (Path B; pre-cutoff 2026-03-10; 2.12.2 is post-cutoff)
- [ ] **1A.4** Update `source` comment for `python` tool: `"apps/a-demo-be-python-fastapi/.python-version"` → `"(demo extracted to ose-primer — no local requirement)"`
- [ ] **1A.5** Update `source` comment for `rust` tool: `"apps/a-demo-be-rust-axum/Cargo.toml → rust-version"` → `"(demo extracted to ose-primer — no local requirement)"`
- [ ] **1A.6** Update `source` comment for `dart` tool: `"apps/a-demo-fe-dart-flutterweb/pubspec.yaml → environment.sdk"` → `"(demo extracted to ose-primer — no local requirement)"`
- [ ] **1A.7** Update `source` comment for `flutter` tool: analogous to dart above
- [ ] **1A.8** Update `source` comment for `java` tool: `"apps/organiclever-be-jasb/pom.xml"` → `"apps/organiclever-be/pom.xml"`
- [ ] **1A.9** Build rhino-cli: `npx nx build rhino-cli --skip-nx-cache`
- [ ] **1A.10** Run `npm run doctor` — confirm Java and dotnet now show correct source files
- [ ] **1A.11** Run rhino-cli unit tests: `npx nx run rhino-cli:test:quick`

### 1B — Documentation correction (AGENTS.md)

- [ ] **1B.1** In `AGENTS.md`, change `organiclever-be` description from `F#/Giraffe REST API backend` → `Java/Spring Boot 4 REST API backend`
- [ ] **1B.2** In `CLAUDE.md` (if it also lists organiclever-be with the wrong description), apply same fix
- [ ] **1B.3** Run `npm run lint:md` to confirm markdown passes

### 1C — Playwright-bdd alignment (preexisting version scatter)

> CORRECTION from prior plan iteration: `playwright-bdd 8.5.1` IS published on npm
> (verified via `npm view playwright-bdd versions`). The earlier "8.5.0 doesn't exist"
> claim was wrong. Align all 9 E2E apps to **exact** `8.5.1`.

- [ ] **1C.1** `apps/ose-app-be-e2e/package.json`: change `"playwright-bdd": "^8.2.0"` → `"playwright-bdd": "8.5.1"`
- [ ] **1C.2** `apps/organiclever-be-e2e/package.json`: change `"playwright-bdd": "^8.2.0"` → `"playwright-bdd": "8.5.1"`
- [ ] **1C.3** `apps/ayokoding-web-be-e2e/package.json`: change `"playwright-bdd": "^8.5.0"` → `"playwright-bdd": "8.5.1"`
- [ ] **1C.4** `apps/ayokoding-web-fe-e2e/package.json`: change `"playwright-bdd": "^8.5.0"` → `"playwright-bdd": "8.5.1"`
- [ ] **1C.5** `apps/ose-web-be-e2e/package.json`: change `"playwright-bdd": "^8.5.0"` → `"playwright-bdd": "8.5.1"`
- [ ] **1C.6** `apps/ose-web-fe-e2e/package.json`: change `"playwright-bdd": "^8.5.0"` → `"playwright-bdd": "8.5.1"`
- [ ] **1C.7** `apps/organiclever-web-e2e/package.json`: change `"playwright-bdd": "^8.4.2"` → `"playwright-bdd": "8.5.1"`
- [ ] **1C.8** `apps/ose-app-web-e2e/package.json`: change `"playwright-bdd": "^8.4.2"` → `"playwright-bdd": "8.5.1"`
- [ ] **1C.9** `apps/wahidyankf-web-fe-e2e/package.json`: change `"playwright-bdd": "^8.4.2"` → `"playwright-bdd": "8.5.1"`

### 1D — Vitest alignment (web-ui lib preexisting mismatch)

- [ ] **1D.1** `libs/web-ui/package.json`: change `"vitest": "^3.2.0"` → `"vitest": "4.1.0"` (Path B; 4.1.6 post-cutoff)
- [ ] **1D.2** `libs/web-ui/package.json`: change `"@vitest/coverage-v8": "^3.2.0"` → `"@vitest/coverage-v8": "4.1.0"`

### 1E — CI workflow go-version pins (preexisting)

- [ ] **1E.1** `.github/workflows/_reusable-backend-e2e.yml`: change `go-version: "1.26.0"` → `go-version: "1.25.8"` (Path B; pre-cutoff)
- [ ] **1E.2** `.github/workflows/pr-validate-links.yml`: change `go-version: "1.26.0"` → `go-version: "1.25.8"`

### 1E2 — Update setup-golang composite action defaults

The `.github/actions/setup-golang/action.yml` composite action has stale default versions
(`go-version: "1.26.0"` and `golangci-lint-version: "v2.10.1"`). The `pr-quality-gate.yml`
`golang` job calls this action without overriding these defaults, so CI would provision
Go 1.26.0 and golangci-lint v2.10.1 even after 1E updates the inline workflow pins.

- [ ] **1E2.1** Update `.github/actions/setup-golang/action.yml` default `go-version`
  from `"1.26.0"` → `"1.25.8"` (Path B; exact, no floating)
- [ ] **1E2.2** Update `.github/actions/setup-golang/action.yml` default
  `golangci-lint-version` from `"v2.10.1"` → `"v2.11.3"` (Path B)
- [ ] **1E2.3** Verify changes: `grep 'default:' .github/actions/setup-golang/action.yml`
  — must show `"1.25.8"` and `"v2.11.3"` as defaults.

### 1F — Missing CI composite actions (CRITICAL preexisting)

`pr-quality-gate.yml` references 5 composite actions that do not exist on disk. The `jvm`
job is reachable in CI today (organiclever-be has `tag:lang:java`) and must be fixed.

- [ ] **1F.1** Create `.github/actions/setup-jvm/action.yml` that calls
  `actions/setup-java@v4` with `distribution: temurin` and `java-version: "25"`. Mirror
  the input/output shape of `setup-golang/action.yml` for consistency. Confirm against
  the `pr-quality-gate.yml` `jvm` job's expected behavior.
- [ ] **1F.2** Delete the `rust`, `elixir`, `clojure`, and `dart` jobs from
  `.github/workflows/pr-quality-gate.yml` (the jobs calling `setup-rust`, `setup-elixir`,
  `setup-clojure`, `setup-flutter` composite actions). No apps carry the corresponding
  `tag:lang:*` tags. Verify by running:
  ```bash
  grep -c 'setup-rust\|setup-elixir\|setup-clojure\|setup-flutter' .github/workflows/pr-quality-gate.yml
  ```
  Output must be `0`.
- [ ] **1F.3** Trigger `pr-quality-gate.yml` via PR (or `gh workflow run` if dispatch
  configured) and confirm `jvm` job runs to completion under organiclever-be.

**Commit Phase 1:** `fix(stack): preexisting bugs — doctor paths, playwright-bdd pins, vitest mismatch, agents.md, ci go-version, missing composite actions`

---

## Phase 2 — Low-Risk Runtime and Tooling Bumps

> Runtime config files and low-risk tooling. No JS code changes.

### 2A — Node.js / npm Volta pin (Path A LTS for Node; Path B for npm)

- [ ] **2A.1** `package.json` root: change `"volta.node"` from `"24.13.1"` → `"24.15.0"` (Path A LTS Krypton)
- [ ] **2A.2** `package.json` root: change `"volta.npm"` from `"11.10.1"` → `"11.11.0"` (Path B; pre-cutoff 2026-02-25)
- [ ] **2A.3** Run `volta install node@24.15.0 npm@11.11.0`
- [ ] **2A.4** Verify: `node --version` → `v24.15.0`, `npm --version` → `11.11.0`

### 2B — Erlang/OTP .tool-versions (Path B)

- [ ] **2B.1** `.tool-versions`: change `erlang 27.3` → `erlang 27.3.3` (Path B; pre-cutoff 2025-04-16; patches CVE-2025-32433 Critical)
- [ ] **2B.2** Verify Elixir still starts: `elixir --version` (should still show 1.19.5 with OTP 27)

### 2C — .NET SDK global.json

- [ ] **2C.1** `apps/ose-app-be/global.json`: change `sdk.version` from `10.0.103` → `10.0.300`
- [ ] **2C.2** Verify: `dotnet --version` → `10.0.300`
- [ ] **2C.3** Build ose-app-be: `npx nx build ose-app-be`
- [ ] **2C.4** Run tests: `npx nx run ose-app-be:test:quick`
- [ ] **2C.5** Build crane-cli (also targets net10.0): `npx nx build crane-cli`
- [ ] **2C.6** Run crane-cli tests: `npx nx run crane-cli:test:quick`

### 2D — golangci-lint version (Path B 2.11.3; tools.go updated in Phase 1A)

- [ ] **2D.1** Confirm machine binary is 2.11.3 (from M0.7)
- [ ] **2D.2** Confirm `apps/rhino-cli/internal/doctor/tools.go` `golangci-lint` `readReq` returns `"2.11.3"` (corrects the 1A.3 fix that originally said 2.12.2)
- [ ] **2D.3** Run golangci-lint on rhino-cli: `cd apps/rhino-cli && golangci-lint run ./...`
- [ ] **2D.4** Run golangci-lint on ayokoding-cli: `cd apps/ayokoding-cli && golangci-lint run ./...`
- [ ] **2D.5** Run golangci-lint on ose-cli: `cd apps/ose-cli && golangci-lint run ./...`
- [ ] **2D.6** Run golangci-lint on golang-commons: `cd libs/golang-commons && golangci-lint run ./...`
- [ ] **2D.7** Run golangci-lint on hugo-commons: `cd libs/hugo-commons && golangci-lint run ./...`

### 2E — Root package.json tooling bumps (EXACT pins; eligible versions per policy)

- [ ] **2E.1** `package.json` root: `"markdownlint-cli2": "^0.21.0"` → `"markdownlint-cli2": "0.21.0"` (Path B; 0.22.x post-cutoff; remove caret)
- [ ] **2E.2** `package.json` root: `"prettier": "^3.6.2"` → `"prettier": "3.8.1"` (Path B; pre-cutoff 2026-01-21)
- [ ] **2E.3** `package.json` root: `"lint-staged": "^16.2.6"` → `"lint-staged": "16.4.0"` (Path B; pre-cutoff 2026-03-14; v17 deferred)
- [ ] **2E.4** `package.json` root: `"tsx": "^4.20.6"` → `"tsx": "4.21.0"` (Path B; pre-cutoff 2025-11-30)
- [ ] **2E.5** `package.json` root: `"@hey-api/openapi-ts": "^0.94.2"` → `"@hey-api/openapi-ts": "0.94.2"` (Path B; remove caret)
- [ ] **2E.6** `package.json` root: `"@redocly/cli": "^2.22.1"` → `"@redocly/cli": "2.22.1"` (Path B; remove caret; 2.30.x post-cutoff)
- [ ] **2E.7** `package.json` root: `"@stoplight/spectral-cli": "^6.15.0"` → `"@stoplight/spectral-cli": "6.15.0"` (Path B; remove caret; 6.15.1 borderline, 6.16.0 post-cutoff)
- [ ] **2E.8** `package.json` root: `"@hey-api/client-fetch": "^0.13.1"` → `"@hey-api/client-fetch": "0.13.1"` (remove caret)
- [ ] **2E.9** `package.json` root: `"@openapitools/openapi-generator-cli": "^2.30.2"` → `"@openapitools/openapi-generator-cli": "2.30.2"` (Path B; remove caret; 2.30.3+ post-cutoff)
- [ ] **2E.10** `package.json` root: `"@commitlint/cli": "^20.1.0"` → `"@commitlint/cli": "20.5.0"` (Path B; pre-cutoff 2026-03-15; v21 post-cutoff/deferred)
- [ ] **2E.11** `package.json` root: `"@commitlint/config-conventional": "^20.0.0"` → `"@commitlint/config-conventional": "20.5.0"` (Path B)
- [ ] **2E.12** `package.json` root: `"husky": "^9.1.7"` → `"husky": "9.1.7"` (remove caret; pre-cutoff)
- [ ] **2E.13** `package.json` root: `"prettier-plugin-tailwindcss": "^0.7.2"` → `"prettier-plugin-tailwindcss": "0.7.2"` (Path B; remove caret; 0.8.0 post-cutoff)
- [ ] **2E.14** `package.json` root: `"eslint-plugin-jsx-a11y": "^6.10.2"` → `"eslint-plugin-jsx-a11y": "6.10.2"` (remove caret)
- [ ] **2E.15** `package.json` root: `"tailwindcss": "^4.2.1"` → `"tailwindcss": "4.2.1"` (Path B; remove caret; 4.3.0 post-cutoff)
- [ ] **2E.16** `package.json` root `optionalDependencies`: pin all native binaries to exact eligible versions:
  - `"@rollup/rollup-linux-x64-gnu": "4.59.0"` (Path B; pre-cutoff 2026-02-22)
  - `"@tailwindcss/oxide-linux-x64-musl": "4.2.1"`
  - `"@tailwindcss/oxide-linux-x64-gnu": "4.2.1"`
  - `"@next/swc-linux-x64-musl": "16.2.6"` (Path C waiver — matches `next`)
  - `"@next/swc-linux-x64-gnu": "16.2.6"` (Path C waiver)
  - `"@esbuild/linux-x64": "0.28.0"` (already pre-cutoff)
  - `"@nx/nx-linux-x64-musl": "22.5.4"` (Path B; matches Nx 22.5.4)
  - `"@nx/nx-linux-x64-gnu": "22.5.4"`
  - `"lightningcss-linux-x64-musl": "1.32.0"` (already pre-cutoff)
  - `"lightningcss-linux-x64-gnu": "1.32.0"`
- [ ] **2E.17** Run `npm install`
- [ ] **2E.18** Verify no caret/tilde in root: `grep -E '"\^|"~' package.json && echo FAIL || echo OK`
- [ ] **2E.19** Run `npm audit --audit-level=moderate` — record any findings; remediate or document waiver
- [ ] **2E.20** Run `npm run lint:md` — must pass with zero errors
- [ ] **2E.21** Run `npm run format:md:check` — must pass
- [ ] **2E.22** Stage a test file change and run `git commit --dry-run` to verify lint-staged 16.4.0 + commitlint 20.5.0 hooks still work
- [ ] **2E.23** Run contract codegen smoke test: `nx run organiclever-contracts:lint` (verifies @redocly/cli + @stoplight/spectral-cli upgrades)

### 2F — Nx bump

- [ ] **2F.1** Run: `npx nx migrate 22.5.4` (generates `migrations.json` if needed)
- [ ] **2F.2** Run: `npx nx migrate --run-migrations` (applies migrations)
- [ ] **2F.3** Update root `package.json` `devDependencies.nx` to `22.5.4` (confirm nx migrate did this)
- [ ] **2F.4** Update all `@nx/*` packages to `22.5.4` in root `package.json` (check `optionalDependencies` too)
- [ ] **2F.5** Run `npm install`
- [ ] **2F.6** Run `npx nx --version` → must show `22.5.4`
- [ ] **2F.7** Run `npx nx affected -t lint` — must pass

**Commit Phase 2:** `chore(stack): bump runtimes and tooling — node 24.15.0, npm 11.11.0, erlang 27.3.3, dotnet 10.0.300, nx 22.5.4, prettier 3.8.1, lint-staged 16.4.0`

---

## Phase 3 — Go Module Updates

> `go get -u ./...` and `go mod tidy` for all Go modules.

- [ ] **3.1** `libs/golang-commons`:
  ```bash
  cd libs/golang-commons
  go get -u ./...
  go mod tidy
  cd ../..
  ```
- [ ] **3.2** `libs/hugo-commons` (used by ayokoding-cli and ose-cli via replace directive):
  ```bash
  cd libs/hugo-commons
  go get -u ./...
  go mod tidy
  cd ../..
  ```
- [ ] **3.3** `apps/rhino-cli`:
  ```bash
  cd apps/rhino-cli
  go get -u ./...
  go mod tidy
  cd ../..
  ```
- [ ] **3.4** `apps/ayokoding-cli`:
  ```bash
  cd apps/ayokoding-cli
  go get -u ./...
  go mod tidy
  cd ../..
  ```
- [ ] **3.5** `apps/ose-cli`:
  ```bash
  cd apps/ose-cli
  go get -u ./...
  go mod tidy
  cd ../..
  ```
- [ ] **3.6** Build all Go apps: `npx nx run-many -t build -p rhino-cli ayokoding-cli ose-cli`
- [ ] **3.7** Run tests all Go apps + libs: `npx nx run-many -t test:quick -p rhino-cli ayokoding-cli ose-cli golang-commons hugo-commons`
- [ ] **3.8** Run golangci-lint on all Go modules:
  - `cd apps/rhino-cli && golangci-lint run ./...`
  - `cd apps/ayokoding-cli && golangci-lint run ./...`
  - `cd apps/ose-cli && golangci-lint run ./...`
  - `cd libs/golang-commons && golangci-lint run ./...`
  - `cd libs/hugo-commons && golangci-lint run ./...`

### 3.9 — REMEDIATE: force `golang.org/x/image` to v0.39.0 (CVE-2026-33809, CVE-2026-33812)

> Indirect transitive dep of `github.com/narqo/go-badge` which has not released since v0.3.
> Both CVEs (TIFF OOM Medium 5.3; font OOM) are patched in v0.39.0. `go get -u ./...`
> may not bump indirect deps; force it explicitly per-module.

- [ ] **3.9.1** For each Go module, add explicit require to force the eligible patched version.
  golang.org/x/image v0.38.0 (released 2025-12 per pkg.go.dev/vuln; pre-cutoff) patches CVE-2026-33809.
  v0.39.0 (~2026 release) patches CVE-2026-33812 — verify release date; if v0.39.0 is post-cutoff, this is a Path C waiver:
  ```bash
  for mod in libs/golang-commons libs/hugo-commons apps/rhino-cli apps/ayokoding-cli apps/ose-cli; do
    (cd "$mod" && go get golang.org/x/image@v0.39.0 && go mod tidy)
  done
  ```
  Document as Path C waiver in tech-docs.md `## Security Waivers` if v0.39.0 release date is after 2026-03-16.
- [ ] **3.9.2** Verify pin landed in each `go.mod`:
  ```bash
  grep -H 'golang.org/x/image' libs/*/go.mod apps/*/go.mod
  ```
  Each line must show `v0.39.0` (not v0.18.0).
- [ ] **3.9.3** Run govulncheck on each module — confirm no findings for `GO-2026-4815`
  (CVE-2026-33809) or `GO-2026-4962` (CVE-2026-33812):
  ```bash
  for mod in libs/golang-commons libs/hugo-commons apps/rhino-cli apps/ayokoding-cli apps/ose-cli; do
    echo "=== $mod ==="; (cd "$mod" && govulncheck ./...)
  done
  ```
- [ ] **3.9.4** Re-run all Go module builds and tests after the bump:
  - `npx nx run-many -t build,test:quick -p rhino-cli ayokoding-cli ose-cli golang-commons hugo-commons`

### 3.10 — DEFER document: aws-sdk-go v1 EOL (S3-crypto CVEs)

> `github.com/aws/aws-sdk-go v1.49.4` is indirect via `github.com/narqo/go-badge`.
> Two S3-crypto CVEs (CVE-2020-8911 Medium, CVE-2020-8912 Low) only affect codepaths
> using `s3crypto` package — our CLIs do NOT use S3 client-side encryption.
> AWS ended v1 support 2025-07-31. Migration to `aws-sdk-go-v2` tracked separately.

- [ ] **3.10.1** Verify no `s3crypto` import in any Go source:
  ```bash
  grep -rn 's3crypto\|aws/aws-sdk-go/service/s3/s3crypto' apps/ libs/
  ```
  Output must be empty.
- [ ] **3.10.2** Add a tracking note to `plans/ideas.md`: "Future plan: migrate
  aws-sdk-go v1 → v2 (currently transitive via narqo/go-badge; v1 EOL 2025-07-31)".

**Commit Phase 3:** `chore(deps): go get -u for all Go modules + force x/image v0.39.0 (CVE-2026-33809/33812)`

---

## Phase 4 — Spring Boot / Maven (organiclever-be)

- [ ] **4.1** `apps/organiclever-be/pom.xml`: change `<parent><version>4.0.4</version>` → `<version>4.0.6</version>`
  [Web-cited: 8 CVE patches in 4.0.6 per https://spring.io/blog/2026/04/23/spring-boot-4-0-6-available-now/,
  accessed 2026-05-15 — CVE-2026-40976, 40973, 40972, 40970, 40971, 40974, 40975, 40977]
- [ ] **4.2** `apps/organiclever-be/pom.xml`: change `<cucumber.version>7.34.2</cucumber.version>` → `<cucumber.version>7.34.3</cucumber.version>`
- [ ] **4.3** Optionally run `mvn versions:display-dependency-updates` from `apps/organiclever-be` to surface any other transitive deps that have updates worth taking
- [ ] **4.4** Review `pom.xml` diff — confirm Spring Boot stays in 4.0.x line (not 4.1.x or 5.x)
- [ ] **4.5** Run unit tests:
  ```bash
  npx nx run organiclever-be:test:quick
  ```
- [ ] **4.6** Verify `mvn verify` succeeds locally (unit tests only, no Docker needed)
- [ ] **4.7** REMEDIATE — switch `apps/organiclever-be/Dockerfile.integration` from
  `FROM eclipse-temurin:25-jdk-alpine` → `FROM eclipse-temurin:25.0.3+9-jdk` (Ubuntu base).
  Rationale: Alpine layer has 2 unfixed High CVEs in binutils (CVE-2025-69649, CVE-2025-69650
  CVSS 7.5) and 8 unfixed Medium CVEs. Ubuntu base has 0 High/Critical. JDK itself
  patched in both.
  - Verify build: `cd apps/organiclever-be && docker build -f Dockerfile.integration -t organiclever-be-int .`
  - Note: image size increases (~200MB → ~450MB) but security tradeoff is justified per
    audit. Document in `apps/organiclever-be/README.md`.
- [ ] **4.8** Run lint: `npx nx run organiclever-be:lint`
- [ ] **4.9** Optional: enable OWASP dependency-check Maven plugin for ongoing scans:
  ```bash
  cd apps/organiclever-be
  mvn org.owasp:dependency-check-maven:check -DfailBuildOnCVSS=7
  ```
  Triage any HIGH/CRITICAL findings.

**Commit Phase 4:** `chore(deps): update Spring Boot parent and Maven deps in organiclever-be`

---

## Phase 5 — Playwright Update (all E2E apps; EXACT pins)

- [ ] **5.1** `apps/ayokoding-web-be-e2e/package.json`: `"@playwright/test": "^1.58.2"` → `"@playwright/test": "1.60.0"`
- [ ] **5.2** `apps/ayokoding-web-fe-e2e/package.json`: `"@playwright/test": "^1.58.2"` → `"@playwright/test": "1.60.0"`
- [ ] **5.3** `apps/ose-web-be-e2e/package.json`: `"@playwright/test": "^1.58.2"` → `"@playwright/test": "1.60.0"`
- [ ] **5.4** `apps/ose-web-fe-e2e/package.json`: `"@playwright/test": "^1.58.2"` → `"@playwright/test": "1.60.0"`
- [ ] **5.5** `apps/ose-app-web-e2e/package.json`: `"@playwright/test": "^1.52.0"` → `"@playwright/test": "1.60.0"`
- [ ] **5.6** `apps/ose-app-be-e2e/package.json`: `"@playwright/test": "^1.58.2"` → `"@playwright/test": "1.60.0"`
- [ ] **5.7** `apps/organiclever-web-e2e/package.json`: `"@playwright/test": "^1.52.0"` → `"@playwright/test": "1.60.0"`
- [ ] **5.8** `apps/organiclever-be-e2e/package.json`: `"@playwright/test": "^1.58.2"` → `"@playwright/test": "1.60.0"`
- [ ] **5.9** `apps/wahidyankf-web-fe-e2e/package.json`: `"@playwright/test": "^1.52.0"` → `"@playwright/test": "1.60.0"`
- [ ] **5.10** Update `@axe-core/playwright` from `"^4.10.1"` → `"4.10.1"` (exact pin; 4.11.x is post-cutoff per Version Table) in all 5 FE E2E apps that have it:
  - `apps/ayokoding-web-fe-e2e/package.json`
  - `apps/ose-web-fe-e2e/package.json`
  - `apps/ose-app-web-e2e/package.json`
  - `apps/organiclever-web-e2e/package.json`
  - `apps/wahidyankf-web-fe-e2e/package.json`
- [ ] **5.11** Run `npm install`
- [ ] **5.12** Verify no caret/tilde in any E2E package.json:
  ```bash
  grep -E '"\^|"~' apps/*-e2e/package.json && echo FAIL || echo OK
  ```
- [ ] **5.13** Run `npx playwright install` to update browser binaries
- [ ] **5.14** Smoke-test E2E (one suite): `npx nx run ayokoding-web-fe-e2e:test:e2e --headed=false`

**Commit Phase 5:** `chore(deps): align all E2E apps to @playwright/test 1.60.0 and @axe-core/playwright 4.10.1`

---

## Phase 6 — Next.js 16.2.6 + React 19.2.6 (Path C WAIVER — CVE patches required)

> **Waiver rationale**: Next.js 16.2.6 is post-cutoff (2026-05-08) but required for
> CVE-2026-29057 (HTTP smuggling), CVE-2026-27979 (DoS), CVE-2026-44578 (SSRF) and 10
> other May 2026 advisories. React 19.2.6 is post-cutoff (~2026-05-06) but required for
> CVE-2025-55182 (Critical RSC RCE) and follow-on patches. No pre-cutoff CVE-clean version
> exists. See `tech-docs.md §Security Waivers`.

Update each app's `package.json` individually, then run install once. All values are
literal strings WITHOUT carets.

- [ ] **6.1** `apps/ayokoding-web/package.json`:
  - `"next": "16.1.6"` → `"next": "16.2.6"` (Path C waiver)
  - `"react": "^19.0.0"` → `"react": "19.2.6"` (Path C waiver)
  - `"react-dom": "^19.0.0"` → `"react-dom": "19.2.6"` (Path C waiver)
  - `"@next/third-parties": "^16.0.0"` → `"@next/third-parties": "16.2.6"`
- [ ] **6.2** `apps/ose-web/package.json`:
  - `"next": "16.1.6"` → `"next": "16.2.6"`
  - `"react": "^19.0.0"` → `"react": "19.2.6"`
  - `"react-dom": "^19.0.0"` → `"react-dom": "19.2.6"`
- [ ] **6.3** `apps/organiclever-web/package.json`:
  - `"next": "16.1.6"` → `"next": "16.2.6"`
  - `"react": "^19.1.0"` → `"react": "19.2.6"`
  - `"react-dom": "^19.1.0"` → `"react-dom": "19.2.6"`
- [ ] **6.4** `apps/wahidyankf-web/package.json`:
  - `"next": "16.1.6"` → `"next": "16.2.6"`
  - `"react": "^19.0.0"` → `"react": "19.2.6"`
  - `"react-dom": "^19.0.0"` → `"react-dom": "19.2.6"`
  - `"@next/third-parties": "^16.0.0"` → `"@next/third-parties": "16.2.6"`
- [ ] **6.5** `apps/ose-app-web/package.json`:
  - `"next": "16.1.6"` → `"next": "16.2.6"`
  - `"react": "^19.1.0"` → `"react": "19.2.6"`
  - `"react-dom": "^19.1.0"` → `"react-dom": "19.2.6"`
- [ ] **6.5b** Pin `@types/react`, `@types/react-dom`, `@types/node` to exact versions in
  every Next.js `package.json` and in `libs/web-ui/package.json`:
  - `"@types/react": "19.2.14"`
  - `"@types/react-dom": "19.2.3"`
  - `"@types/node": "22.19.15"` (Path B; latest 22.x pre-cutoff 2026-03-06; v25 is post-cutoff and DEFERRED)
- [ ] **6.6** Run `npm install`
- [ ] **6.7** Build all Next.js apps:
  ```bash
  npx nx run-many -t build -p ayokoding-web ose-web organiclever-web wahidyankf-web ose-app-web
  ```
- [ ] **6.8** Run `test:quick` for all Next.js apps:
  ```bash
  npx nx run-many -t test:quick -p ayokoding-web ose-web organiclever-web wahidyankf-web ose-app-web
  ```
- [ ] **6.9** Spot-check dev server: `npx nx dev ayokoding-web` → loads on localhost:3101

**Commit Phase 6:** `chore(deps): next.js 16.1.6→16.2.6, react 19.2 — all Next.js apps`

---

## Phase 7 — Tailwind 4.2.1 + Vitest 4.1.0 + Storybook 10.2.10 + plugin-react 6.0.1 (eligible Path B)

### 7A — Tailwind CSS 4.2.1 (Path B; no bump from current ^4.2.1 — just remove carets)

- [ ] **7A.1** Root `package.json` `dependencies`: `"tailwindcss": "4.2.1"` (already in 2E.15)
- [ ] **7A.2** Each Next.js app `devDependencies`: pin `"tailwindcss": "4.2.1"` (no caret) in all 5 apps:
  - `apps/ayokoding-web/package.json`
  - `apps/ose-web/package.json`
  - `apps/organiclever-web/package.json`
  - `apps/wahidyankf-web/package.json`
  - `apps/ose-app-web/package.json`
- [ ] **7A.3** Each app's `@tailwindcss/postcss`: pin to exact `"4.2.1"` (no caret)
- [ ] **7A.4** `apps/organiclever-web/package.json`, `apps/ose-app-web/package.json`, `libs/web-ui/package.json`: pin `"@tailwindcss/vite": "4.2.1"`
- [ ] **7A.5** `apps/ayokoding-web/package.json` + `apps/ose-web/package.json`: pin `"@tailwindcss/typography": "0.5.16"` (Path B; verify on npm — use latest pre-cutoff)
- [ ] **7A.6** Confirm root `optionalDependencies` `@tailwindcss/oxide-*` are pinned to `4.2.1` (already in 2E.16)

### 7B — Vitest 4.1.0 (Path B; pre-cutoff 2026-03-12)

- [ ] **7B.1** All 5 Next.js apps — pin `"vitest": "4.1.0"` and `"@vitest/coverage-v8": "4.1.0"` (no caret; Path B, 2026-03-12; 4.1.6 post-cutoff)
- [ ] **7B.2** `libs/web-ui/package.json`: confirm already pinned to `"4.1.0"` from Phase 1D
- [ ] **7B.3** `apps/organiclever-web/package.json` + `apps/ose-app-web/package.json`: pin `"@effect/vitest": "0.29.0"` (no caret)
- [ ] **7B.4** All apps using `jsdom`: pin `"jsdom": "29.0.0"` (Path B; pre-cutoff 2026-03-15)
- [ ] **7B.5** All apps with `@testing-library/*` deps: pin to exact (verify pre-cutoff):
  - `"@testing-library/react": "16.3.2"`
  - `"@testing-library/jest-dom": "6.9.1"`
  - `"@testing-library/dom": "10.4.1"`
  - `"@testing-library/user-event": "14.6.1"`
- [ ] **7B.6** All apps with `@amiceli/vitest-cucumber`: pin `"6.3.0"` (Path B; pre-cutoff 2026-03-08)
- [ ] **7B.7** All apps with `vite-tsconfig-paths`: pin `"6.1.1"` (pre-cutoff)

### 7C — Storybook 10.2.10 (Path B; DOWNGRADE from current ^10.3.3)

> CVE-2026-27148 (High WebSocket hijacking) patched at 10.2.10 (2026-02-25, pre-cutoff).
> Storybook 10.3.x first released 2026-04-08 — POST-CUTOFF. The current pin ^10.3.3 must
> be DOWNGRADED to 10.2.10 to satisfy both 60-day rule and CVE clearance.

- [ ] **7C.1** `apps/organiclever-web/package.json`: pin all `@storybook/*` and `storybook` to exact `"10.2.10"` (DOWNGRADE)
- [ ] **7C.2** `apps/ose-app-web/package.json`: same pin to `"10.2.10"` (DOWNGRADE)
- [ ] **7C.3** `libs/web-ui/package.json`: same pin to `"10.2.10"` (DOWNGRADE)
- [ ] **7C.4** Run `npm install`
- [ ] **7C.5** Run Storybook build for web-ui: `npx nx run web-ui:build-storybook` — must succeed
- [ ] **7C.6** Run tests: `npx nx run-many -t test:quick -p web-ui organiclever-web ose-app-web`

### 7D — @vitejs/plugin-react 6.0.1 MAJOR (Path B; pre-cutoff 2026-03-13)

> v6 removes Babel as a dependency. 6.0.1 is the latest pre-cutoff version (6.0.0 → 6.0.1
> → 6.0.2; 6.0.2 is post-cutoff 2026-05-14).

- [ ] **7D.1** Identify consumers: `grep -l '"@vitejs/plugin-react"' apps/*/package.json libs/*/package.json`
- [ ] **7D.2** `apps/organiclever-web/package.json`: `"@vitejs/plugin-react": "^5.1.4"` → `"@vitejs/plugin-react": "6.0.1"`
- [ ] **7D.3** `apps/ose-app-web/package.json`: → `"6.0.1"`
- [ ] **7D.4** `apps/ayokoding-web/package.json`: → `"6.0.1"`
- [ ] **7D.5** `apps/ose-web/package.json`: → `"6.0.1"`
- [ ] **7D.6** `apps/wahidyankf-web/package.json`: → `"6.0.1"`
- [ ] **7D.7** `libs/web-ui/package.json`: → `"6.0.1"`
- [ ] **7D.8** Run `npm install`
- [ ] **7D.9** Audit any `vite.config.*` or `vitest.config.*` using custom Babel configs
  passed to the plugin — none expected, but verify via `grep -rn "babel:" apps/ libs/`
- [ ] **7D.10** Run `test:quick` for all consumers — must pass

### 7E — postcss (Path C WAIVER — CVE patch required)

> CVE-2026-41305 (XSS Medium 6.1) first patched at 8.5.10 (2026-04-15, post-cutoff).
> No pre-cutoff version is CVE-clean. Path C waiver applied.

- [ ] **7E.1** Each app + lib that has `postcss` direct dep: pin `"postcss": "8.5.10"` (no caret) — Path C waiver per `tech-docs.md §Security Waivers`

**Commit Phase 7:** `chore(deps): tailwind 4.2.1, vitest 4.1.0, storybook 10.2.10 (downgrade from ^10.3.3), @vitejs/plugin-react 6.0.1`

---

## Phase 8 — Effect ecosystem (Path B; eligible versions)

> CVE-2026-32887 (effect AsyncLocalStorage High 7.4) was patched at 3.20.0. Verify
> 3.20.0 release date — if pre-cutoff (2026-03-16), use 3.20.0 or earlier 3.20.x. If
> post-cutoff, this becomes Path C waiver.

- [ ] **8.1** `apps/organiclever-web/package.json`: `"@effect/platform": "^0.84.0"` → `"@effect/platform": "0.94.5"` (Path B; latest pre-cutoff 2026-02-14)
- [ ] **8.2** `apps/ose-app-web/package.json`: same as 8.1
- [ ] **8.3** Both apps: pin `"effect": "3.21.2"` (Path C WAIVER — CVE-2026-32887 patched at 3.20.0; latest pre-cutoff 3.x is 3.19.19 which is VULNERABLE; use 3.21.2 with waiver per `tech-docs.md §Security Waivers`)
- [ ] **8.4** Both apps: pin `"@effect/vitest": "0.29.0"` (no caret; pre-cutoff)
- [ ] **8.5** Both apps: pin `"xstate": "5.28.0"` (Path B; latest 5.x pre-cutoff 2026-02-12)
- [ ] **8.6** Both apps with `@electric-sql/pglite`: pin `"@electric-sql/pglite": "0.3.16"` (Path B; latest 0.x pre-cutoff 2026-03-10; 0.4.x first release was 2026-03-25 post-cutoff. NOTE: this is a DOWNGRADE from current ^0.4.5)
- [ ] **8.7** Run `npm install`
- [ ] **8.8** Run typecheck: `npx nx run-many -t typecheck -p organiclever-web ose-app-web`
- [ ] **8.9** Fix any type errors from @effect/platform 0.96 API changes
- [ ] **8.10** Run `test:quick`: `npx nx run-many -t test:quick -p organiclever-web ose-app-web`
- [ ] **8.11** Verify `lint-staged 16.4.0` hook still fires on pre-commit: stage a `.md` change and attempt commit

**Commit Phase 8:** `chore(deps): @effect/platform 0.94.5, effect 3.21.2 (Path C waiver), xstate 5.28.0, @electric-sql/pglite 0.3.16 in organiclever-web and ose-app-web`

---

## Phase 9 — TypeScript 5.8.3 stay-on-line (Path B; TS 6 DEFERRED to future plan)

> POLICY DECISION: TypeScript 6.0.3 (released 2026-03-23) is post-cutoff. Per Path B,
> stay on TypeScript 5 line. Latest pre-cutoff TS 5.x is 5.8.3 (2025-02-28).
> Migration to TS 6 deferred to a future plan once 6.x has 60+ days of soak.

### 9A — Pin TypeScript 5.8.3 in all packages (EXACT pins)

- [ ] **9A.1** Root `package.json` `devDependencies`: confirm `"tsx": "4.21.0"` (Path B; from 2E.4); compatible with TS 5.x
- [ ] **9A.2** `apps/ayokoding-web/package.json`: `"typescript": "^5.6.0"` → `"typescript": "5.8.3"`
- [ ] **9A.3** `apps/ose-web/package.json`: `"typescript": "^5.6.0"` → `"typescript": "5.8.3"`
- [ ] **9A.4** `apps/organiclever-web/package.json`: `"typescript": "^5"` → `"typescript": "5.8.3"`
- [ ] **9A.5** `apps/wahidyankf-web/package.json`: `"typescript": "^5.6.0"` → `"typescript": "5.8.3"`
- [ ] **9A.6** `apps/ose-app-web/package.json`: `"typescript": "^5"` → `"typescript": "5.8.3"`
- [ ] **9A.7** E2E apps with TypeScript pin (pin to 5.8.3; these may have carets remaining from original setup):
  - `apps/organiclever-web-e2e/package.json`: `"typescript": "5.8.3"`
  - `apps/ose-app-web-e2e/package.json`: `"typescript": "5.8.3"`
  - `apps/wahidyankf-web-fe-e2e/package.json`: `"typescript": "5.8.3"`
- [ ] **9A.8** Run `npm install`
- [ ] **9A.9** Verify TS version: `npx tsc --version` → `5.8.3` (exact)
- [ ] **9A.10** Verify no caret/tilde across all package.json files:
  ```bash
  find apps libs -name package.json -not -path "*/node_modules/*" -exec grep -lE '"\^|"~' {} \; && echo "FAIL: above files have caret/tilde" || echo "OK: all exact"
  ```
- [ ] **9A.11** Add tracking note to `plans/ideas.md`: "Future plan: TypeScript 6.0
  migration once TS 6.x has 60+ days of soak (eligible after ~2026-05-23)."

### 9B — Fix type errors per project (TS 5.8.3 from prior 5.6.0)

- [ ] **9B.1** RED: Run `npx nx run-many -t typecheck` and collect all errors — confirm
  typecheck fails (non-zero exit) for at least one project before applying any fixes.
  Save output to a scratch file: `npx nx run-many -t typecheck 2>&1 | tee /tmp/ts58-errors.txt`
- [ ] **9B.2** Fix type errors in `apps/ayokoding-web`:
  - Run `npx nx run ayokoding-web:typecheck 2>&1 | tee /tmp/ts58-ayokoding.txt` — review output
  - For each error: identify the file path (shown in error), open it, apply the fix
    (stricter inference → add explicit return type; index signature issues → use
    `Record<string, unknown>`)
  - Verify: `npx nx run ayokoding-web:typecheck` → exits 0
- [ ] **9B.3** Fix type errors in `apps/ose-web`:
  - Run `npx nx run ose-web:typecheck 2>&1 | tee /tmp/ts58-ose-web.txt` — review output
  - For each error: edit the identified file, apply fix
  - Verify: `npx nx run ose-web:typecheck` → exits 0
- [ ] **9B.4** Fix type errors in `apps/organiclever-web`:
  - Run `npx nx run organiclever-web:typecheck 2>&1 | tee /tmp/ts58-organiclever.txt` — review
  - For each error: edit the identified file, apply fix
  - Verify: `npx nx run organiclever-web:typecheck` → exits 0
- [ ] **9B.5** Fix type errors in `apps/wahidyankf-web`:
  - Run `npx nx run wahidyankf-web:typecheck 2>&1 | tee /tmp/ts58-wahidyankf.txt` — review
  - For each error: edit the identified file, apply fix
  - Verify: `npx nx run wahidyankf-web:typecheck` → exits 0
- [ ] **9B.6** Fix type errors in `apps/ose-app-web`:
  - Run `npx nx run ose-app-web:typecheck 2>&1 | tee /tmp/ts58-ose-app-web.txt` — review
  - For each error: edit the identified file, apply fix
  - Verify: `npx nx run ose-app-web:typecheck` → exits 0
- [ ] **9B.7** Fix type errors in `libs/web-ui`:
  - Run `npx nx run web-ui:typecheck 2>&1 | tee /tmp/ts58-web-ui.txt` — review
  - For each error: edit the identified file, apply fix
  - Verify: `npx nx run web-ui:typecheck` → exits 0
- [ ] **9B.8** Fix type errors in E2E apps:
  - Run `npx nx run-many -t typecheck -p organiclever-web-e2e ose-app-web-e2e wahidyankf-web-fe-e2e`
  - For each error: edit the identified file, apply fix
  - Verify: re-run the same command → exits 0
- [ ] **9B.9** GREEN: Confirm all typecheck passes: `npx nx run-many -t typecheck` → zero errors
  (exit code 0 for every project)

### 9C — Lint and test after TS 5.8.3 pin update

- [ ] **9C.1** Run `npx nx run-many -t lint` — record errors (ESLint 9.x with @typescript-eslint
  8.57.0 on TS 5.8.3 code); fix any errors in this phase
- [ ] **9C.2** Run `npx nx run-many -t test:quick` — fix any test failures from TS 5.8.3 upgrade

### 9D — ESLint 9.x stay-on-line + react-hooks 5.x stay (Path B; ESLint 10 DEFERRED)

> POLICY DECISION: ESLint 10.0.0 (~2026-02 release) is post-cutoff for some patches; v10.3
> is post-cutoff. Stay on ESLint 9 line + react-hooks 5 line + typescript-eslint 8.x.

- [ ] **9D.1** Identify all consumers: `grep -l '"eslint":' apps/*/package.json libs/*/package.json`
- [ ] **9D.2** `apps/organiclever-web/package.json` (EXACT pins; remove carets, stay on 9.x):
  - `"eslint": "^9.18.0"` → `"eslint": "9.39.4"` (Path B; latest 9.x pre-cutoff 2026-03-06)
  - `"eslint-plugin-react-hooks": "^5.1.0"` → `"eslint-plugin-react-hooks": "5.2.0"` (Path B; latest 5.x pre-cutoff 2025-02-28; v7 requires ESLint 10 — deferred)
  - `"@typescript-eslint/parser": "^8.20.0"` → `"@typescript-eslint/parser": "8.57.0"` (Path B; pre-cutoff 2026-03-09)
  - `"eslint-import-resolver-typescript": "^4.4.4"` → `"eslint-import-resolver-typescript": "4.4.4"` (remove caret)
  - `"eslint-plugin-boundaries": "^5.0.1"` → `"eslint-plugin-boundaries": "5.4.0"` (Path B; pre-cutoff 2026-02-02; v6 deferred)
  - `"eslint-plugin-import": "^2.31.0"` → `"eslint-plugin-import": "2.32.0"`
- [ ] **9D.3** `apps/ose-app-web/package.json`: same set of bumps as 9D.2
- [ ] **9D.4** Root `package.json` already pinned `eslint-plugin-jsx-a11y` to `6.10.2` in 2E.14 — confirm
- [ ] **9D.5** Add tracking note to `plans/ideas.md`: "Future plan: ESLint 10 + react-hooks
  7 migration once those versions have 60+ days of soak."
- [ ] **9D.6** Check other apps for `eslint` dep (ayokoding-web, ose-web, wahidyankf-web)
  and bump if present (stay on 9.x exact pins)
- [ ] **9D.7** Run `npm install`
- [ ] **9D.8** Verify versions: `npx eslint --version` → `9.x.x` (must NOT show v10)
- [ ] **9D.9** Run `npx nx run-many -t lint 2>&1 | tee /tmp/eslint9-errors.txt` — collect
  all errors. Confirm lint passes (exit 0) after pinning ESLint 9.x.
- [ ] **9D.10** Fix any lint errors per project:
  - For each project with lint errors: identify the file path from the error output
  - Verify `eslint.config.*` files work under ESLint 9.x (flat config)
  - For `rule 'X' not found` errors: check version compatibility of the plugin for ESLint 9
  - After each project fix: `npx nx run <project>:lint` → exits 0
- [ ] **9D.11** GREEN: Run `npx nx run-many -t lint` — must pass with zero errors (exit 0)
- [ ] **9D.12** Run `npx nx run-many -t test:quick` — must pass

**Commit Phase 9:** `chore(deps): typescript 5.8.3 pin (6.0 deferred), eslint 9.x stay-on-line, @typescript-eslint 8.57.0`

---

## Phase 10 — Shiki 4.0.2 Migration (Path B; pre-cutoff 2026-03-09 — eligible)

- [ ] **10.1** `apps/ayokoding-web/package.json`: `"shiki": "^1.0.0"` → `"shiki": "4.0.2"`
- [ ] **10.2** `apps/ose-web/package.json`: `"shiki": "^1.0.0"` → `"shiki": "4.0.2"`
- [ ] **10.3** Run `npm install`
- [ ] **10.4** Audit all Shiki usage: `grep -rn "from 'shiki'\|require('shiki')" apps/ayokoding-web apps/ose-web`
  — note every file path returned; these are the files to edit in 10.5 and 10.6.
- [ ] **10.5** RED: Run `npx nx run ayokoding-web:typecheck` — confirm it fails on Shiki 4.x
  type mismatches before applying fixes.
  Migrate `ayokoding-web` Shiki usage files (identified in 10.4) to 4.x API:
  - For each file: check `createHighlighter` import (unchanged in 4.x, no migration needed)
  - Check theme name references — if using named themes, compare against Shiki 4.x
    [bundled themes list](https://shiki.style/themes)
  - Fix any `codeToHtml` / `codeToTokens` signature mismatches shown by typecheck
  - Verify: `npx nx run ayokoding-web:typecheck` → exits 0
- [ ] **10.6** RED: Run `npx nx run ose-web:typecheck` — confirm it fails before fixes.
  Migrate `ose-web` Shiki usage files (identified in 10.4) using same approach as 10.5.
  Verify: `npx nx run ose-web:typecheck` → exits 0
- [ ] **10.7** Build both apps: `npx nx run-many -t build -p ayokoding-web ose-web`
- [ ] **10.8** Run `test:quick`: `npx nx run-many -t test:quick -p ayokoding-web ose-web`
- [ ] **10.9** Spot-check dev server syntax highlighting: `npx nx dev ayokoding-web` → open a code example page, confirm highlighting renders

**Commit Phase 10:** `chore(deps): shiki 1.x→4.x migration in ayokoding-web and ose-web`

---

## Phase 11 — Zod stay-on-3 (Path B; v4 DEFERRED to future plan)

> POLICY DECISION: Zod 4.x release date is post-cutoff per audit. Stay on Zod 3 line.
> Migration to Zod 4 deferred to a future plan once 4.x has 60+ days of soak.

- [ ] **11.1** `apps/ayokoding-web/package.json`: `"zod": "^3.23.0"` → `"zod": "3.25.76"` (Path B; latest 3.x release 2025-07-08; v4 post-cutoff)
- [ ] **11.2** `apps/ose-web/package.json`: same pin as 11.1
- [ ] **11.3** Run `npm install`
- [ ] **11.4** Audit all Zod usage: `grep -rn "from 'zod'" apps/ayokoding-web apps/ose-web`
  — note every file path returned; these are the files to check in 11.5 and 11.6.
- [ ] **11.5** Run `npx nx run ayokoding-web:typecheck` — confirm it passes after the 3.x pin
  update (stay-on-line; no API changes expected). If any type errors appear, review and fix
  (unlikely — this is a patch-level pin within Zod 3).
  Verify: `npx nx run ayokoding-web:typecheck` → exits 0
- [ ] **11.6** Run `npx nx run ose-web:typecheck` — same verification.
  Verify: `npx nx run ose-web:typecheck` → exits 0
- [ ] **11.7** GREEN: Run typecheck: `npx nx run-many -t typecheck -p ayokoding-web ose-web` → exits 0
- [ ] **11.8** Run `test:quick`: `npx nx run-many -t test:quick -p ayokoding-web ose-web`

**Commit Phase 11:** `chore(deps): zod 3.25.76 stay-on-3.x in ayokoding-web and ose-web (zod 4 deferred)`

---

## Phase 12 — lucide-react stay-on-0.x (Path B; 1.x DEFERRED)

> POLICY DECISION: lucide-react 1.0+ is post-cutoff (1.0.1 ~2026-03-23). Stay on 0.x.
> Latest pre-cutoff 0.x version is **0.577.0** (2026-03-04). v1 migration deferred.

- [ ] **12.1** `apps/ayokoding-web/package.json`: `"lucide-react": "^0.447.0"` → `"lucide-react": "0.577.0"`
- [ ] **12.2** `apps/ose-web/package.json`: `"lucide-react": "^0.447.0"` → `"lucide-react": "0.577.0"`
- [ ] **12.3** `apps/wahidyankf-web/package.json`: `"lucide-react": "^0.447.0"` → `"lucide-react": "0.577.0"`
- [ ] **12.4** `libs/web-ui/package.json`: `"lucide-react": "^0.447.0"` → `"lucide-react": "0.577.0"`
- [ ] **12.5** Run `npm install`
- [ ] **12.6** Audit all lucide-react icon imports and save the icon name list:
  ```bash
  grep -rn "from 'lucide-react'" apps/ayokoding-web apps/ose-web apps/wahidyankf-web libs/web-ui | tee /tmp/lucide-imports.txt
  ```
- [ ] **12.7** Cross-reference icon names in use against the lucide-react 0.577.0 changelog:
  - Fetch https://github.com/lucide-icons/lucide/releases and check entries between
    0.447.0 and 0.577.0 (within 0.x ONLY — NOT the 0.x → 1.x jump which is deferred)
  - Extract any renamed/removed icons in that range and compare against `/tmp/lucide-imports.txt`
  - Acceptance criterion: every icon name used in the repo exists in `lucide-react@0.577.0`
- [ ] **12.8** Run `npx nx run ayokoding-web:build 2>&1 | grep -i 'lucide\|module not found'`
  — confirm no renamed-icon build errors (stay-on-0.x; unlikely to have renames).
  If any icons renamed between 0.447.0 and 0.577.0: update the import in the identified file.
  Verify: `npx nx run ayokoding-web:build` → exits 0 with no lucide errors
- [ ] **12.9** Fix any renamed icons in `apps/ose-web` (same process as 12.8).
  Verify: `npx nx run ose-web:build` → exits 0 with no lucide errors
- [ ] **12.10** Fix any renamed icons in `apps/wahidyankf-web` (same process as 12.8).
  Verify: `npx nx run wahidyankf-web:build` → exits 0 with no lucide errors
- [ ] **12.11** Fix any renamed icons in `libs/web-ui` (same process as 12.8).
  Verify: `npx nx run web-ui:build` → exits 0 with no lucide errors
- [ ] **12.12** Build all affected: `npx nx run-many -t build -p ayokoding-web ose-web wahidyankf-web web-ui`
- [ ] **12.13** Run tests: `npx nx run-many -t test:quick -p ayokoding-web ose-web wahidyankf-web web-ui`
- [ ] **12.14** Run Storybook build to verify icon rendering: `npx nx run web-ui:build-storybook`

**Commit Phase 12:** `chore(deps): lucide-react 0.577.0 (latest pre-cutoff 0.x; 1.x deferred)`

---

## Phase 13 — @xstate/react stay-on-5 (Path B; v6 DEFERRED)

> POLICY DECISION: @xstate/react 6.0+ is post-cutoff. Stay on 5 line. v6 deferred.

- [ ] **13.1** `apps/organiclever-web/package.json`: `"@xstate/react": "^5.0.5"` → `"@xstate/react": "5.0.5"` (Path B; current pin already at the latest 5.x pre-cutoff version 2025-05-31; v6 post-cutoff)
- [ ] **13.2** `apps/ose-app-web/package.json`: same pin as 13.1
- [ ] **13.3** Run `npm install`
- [ ] **13.4** Audit XState React usage and save file list:
  ```bash
  grep -rn "from '@xstate/react'" apps/organiclever-web apps/ose-app-web | tee /tmp/xstate-imports.txt
  ```
- [ ] **13.5** Run `npx nx run organiclever-web:typecheck` — confirm it passes after the 5.x
  pin update (stay-on-line; no API changes expected). If any type errors appear, review and
  fix (unlikely — this is a patch-level pin within @xstate/react 5.x).
  Verify: `npx nx run organiclever-web:typecheck` → exits 0
- [ ] **13.6** Run `npx nx run ose-app-web:typecheck` — same verification.
  Verify: `npx nx run ose-app-web:typecheck` → exits 0
- [ ] **13.7** GREEN: Run typecheck: `npx nx run-many -t typecheck -p organiclever-web ose-app-web` → exits 0
- [ ] **13.8** Run tests: `npx nx run-many -t test:quick -p organiclever-web ose-app-web`
- [ ] **13.9** Build: `npx nx run-many -t build -p organiclever-web ose-app-web`

**Commit Phase 13:** `chore(deps): @xstate/react 5.x pin (latest pre-cutoff; 6.x deferred)`

---

## Phase 14 — Final Full CI Gate

- [ ] **14.1** Run `npm install` one final time (clean state)
- [ ] **14.2** Run `npm run doctor` — all tools must show OK; zero FAIL entries
- [ ] **14.3** Run full typecheck: `npx nx run-many -t typecheck` — zero errors
- [ ] **14.4** Run full lint: `npx nx run-many -t lint` — zero errors
- [ ] **14.5** Run full test:quick: `npx nx run-many -t test:quick` — all pass
- [ ] **14.6** Run full spec-coverage: `npx nx run-many -t spec-coverage` — all pass
- [ ] **14.7** Build all apps: `npx nx run-many -t build` — zero errors
- [ ] **14.8** Run markdown lint: `npm run lint:md` — zero errors
- [ ] **14.9** Check version table in `tech-docs.md` — confirm every "Current" column matches "Target"
- [ ] **14.10** Run `npm run doctor` one final time and copy output to confirm

---

## Phase 15 — Post-Merge CI Verification

> After merging to `main`, monitor CI.

- [ ] **15.1** Merge worktree branch to `main` via fast-forward:
  ```bash
  git checkout main && git merge --ff-only worktree/stack-update
  ```
- [ ] **15.2** Push to origin: `git push origin main`
- [ ] **15.3** Monitor GitHub Actions CI — check every 3–5 min until all checks pass
- [ ] **15.4** If any CI job fails: investigate root cause, fix, push, re-monitor
- [ ] **15.5** Confirm all jobs green before closing this plan

---

## Local Quality Gates (Before Push)

> Apply after each phase commit AND before the final push.

- [ ] Run affected typecheck: `npx nx affected -t typecheck` — exits 0
- [ ] Run affected linting: `npx nx affected -t lint` — exits 0
- [ ] Run affected quick tests: `npx nx affected -t test:quick` — all pass
- [ ] Run affected spec coverage: `npx nx affected -t spec-coverage` — all pass

> **Important**: Fix ALL failures found during quality gates, not just those caused by your
> changes. This follows the root cause orientation principle — proactively fix preexisting
> errors encountered during work. Do not defer or mention-and-skip existing issues.

---

## Manual UI Verification (Playwright MCP)

> Run after Phase 6 (Next.js), Phase 10 (Shiki), Phase 11 (Zod), Phase 12 (lucide-react).

For each affected Next.js app (`ayokoding-web`, `ose-web`, `organiclever-web`,
`wahidyankf-web`, `ose-app-web`), verify the app renders without JS errors:

- [ ] Start dev server: `npx nx dev <app-name>` (ports: 3101, 3100, 3200, 3201, 3300)
- [ ] Navigate to the home page via `browser_navigate http://localhost:<port>`
- [ ] Inspect DOM via `browser_snapshot` — verify correct rendering and no layout breaks
- [ ] Check for JS errors via `browser_console_messages` — must be zero errors
- [ ] For `ayokoding-web` / `ose-web` after Shiki (Phase 10): navigate to a code example
  page, take `browser_snapshot` — confirm syntax highlighting renders (colored tokens visible)
- [ ] For apps after Zod (Phase 11): use `mcp__plugin_playwright_playwright__browser_fill_form`
  to fill a form field with invalid input, then use
  `mcp__plugin_playwright_playwright__browser_click` to submit; take
  `mcp__plugin_playwright_playwright__browser_snapshot` — confirm validation messages render
  correctly; check `mcp__plugin_playwright_playwright__browser_console_messages` to confirm no
  Zod-related console errors
- [ ] For apps after lucide-react (Phase 12): navigate to a page with icons, take
  `browser_screenshot` — confirm icons render (no broken icon placeholders)
- [ ] Take screenshots via `browser_take_screenshot` for visual verification record

---

## Manual API Verification (curl)

> Run after Phase 4 (Spring Boot upgrade for organiclever-be).

- [ ] Start backend server: `npx nx dev organiclever-be` (port 8202)
- [ ] Verify health endpoint: `curl -s http://localhost:8202/actuator/health | jq .`
  — response must contain `{"status":"UP"}`
- [ ] Verify at least one business endpoint returns expected schema:
  `curl -s http://localhost:8202/api/v1/health | jq .` (or substitute a real endpoint from
  `apps/organiclever-be-e2e/` test files)
- [ ] Test error case with invalid payload — verify 4xx response with proper error body
- [ ] Document verification results in this checklist (note actual response received)

---

## Post-Push Verification

- [ ] Push changes to `main`: `git push origin main`
- [ ] Monitor GitHub Actions workflows for the push (check `gh run list` every 3–5 min)
- [ ] Verify all CI checks pass — specifically the `jvm` job in `pr-quality-gate.yml`
- [ ] If any CI check fails, fix immediately and push a follow-up commit
- [ ] Do NOT archive this plan until CI is green

---

## Plan Archive Checklist

- [ ] All delivery items above checked off
- [ ] CI green on `origin/main`
- [ ] Run from repo root: `git mv plans/in-progress/stack-update plans/done/2026-05-15__stack-update`
- [ ] Update `plans/in-progress/README.md` — remove the `stack-update` entry
- [ ] Update `plans/done/README.md` — add `stack-update` entry with completion date `2026-05-15`
- [ ] Commit: `git commit -m "chore(plans): move stack-update to done"`
