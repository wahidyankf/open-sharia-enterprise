# Business Requirements Document

## Problem

The `ose-public` monorepo has accumulated version drift across 11 non-E2E apps + 9 E2E
apps + 4 working libs (`web-ui`, `web-ui-token`, `golang-commons`, `hugo-commons`)
plus 2 placeholder libs (`ts-ui`, `clojure-openapi-codegen`). Specific issues:

- **Security risk**: Next.js 16.1.6 has known CVE patches addressed in 16.2.6
  (CVE-2026-29057, CVE-2026-27979, CVE-2026-44578 — see `tech-docs.md §Security Clearance
Status` for full CVE list already audited). Spring Boot 4.0.4 has
  8 CVE patches addressed in 4.0.6. [Web-cited: Spring Boot 4.0.6 released 2026-04-23
  patches 8 CVEs (CVE-2026-40976, 40973, 40972, 40970, 40971, 40974, 40975, 40977)
  per https://spring.io/blog/2026/04/23/spring-boot-4-0-6-available-now/, accessed
  2026-05-15.]
- **Stale runtimes**: Go 1.26.1 [Unverified: binary version not in config file; go.mod
  directive shows `go 1.26` only] (two patch releases behind: 1.26.3 latest
  [Web-cited: golang-announce 2026-05-07]), .NET SDK 10.0.103 [Repo-grounded:
  apps/ose-app-be/global.json] (SDK 10.0.300 released 2026-05-12 [Web-cited:
  dotnet.microsoft.com]), Erlang/OTP 27.3 [Repo-grounded: .tool-versions]
  (27.3.3 is the latest eligible pre-cutoff patch [Web-cited: erlang.org/news; OTP 27.3.3 released 2025-04-16 patches CVE-2025-32433 (Critical SSH RCE), accessed 2026-05-15]).
- **Inconsistent tooling**: playwright-bdd pins differ across apps (^8.2.0, ^8.4.2,
  ^8.5.0 — the last does not exist on npm) [Repo-grounded: E2E app package.json files].
  Playwright versions span 1.52.0–1.58.2 within the same repo (latest: 1.60.0
  [Web-cited: npmjs.com, accessed 2026-05-15]). `web-ui` lib runs Vitest 3.x while all
  apps run 4.x [Repo-grounded: libs/web-ui/package.json]. `@vitejs/plugin-react` is at
  ^4.0.0 in some apps and ^5.1.4 in others (latest is 6.0.1, which removes Babel — major
  [Web-cited: github.com/vitejs/vite-plugin-react/releases, accessed 2026-05-15]).
- **Major version lag**: TypeScript 6.0 (released 2026-03-23 [Web-cited:
  devblogs.microsoft.com/typescript/announcing-typescript-6-0]), ESLint 10.x [Web-cited:
  eslint.org/blog], eslint-plugin-react-hooks 7.x, @vitejs/plugin-react 6.x, Shiki 4.x,
  Zod 4.x, lucide-react 1.x, @xstate/react 6.x — all stable, none adopted.
- **Preexisting doc bug**: `AGENTS.md` / `CLAUDE.md` describe `organiclever-be` as
  "F#/Giraffe" when it is actually Java/Spring Boot 4.0.4.
- **Stale doctor paths**: `rhino-cli/internal/doctor/tools.go` references demo-app paths
  (`apps/a-demo-be-python-fastapi`, `apps/organiclever-be-jasb`, `apps/ose-grc-be`, etc.)
  that were extracted to `ose-primer` in 2026-04-18 and no longer exist. Also reads Java
  version from `apps/organiclever-be-jasb/pom.xml` when the real app is
  `apps/organiclever-be/pom.xml`, and .NET from `apps/ose-grc-be/global.json` when the
  real app is `apps/ose-app-be/global.json`.
- **golangci-lint hardcoded**: version `2.11.1` hardcoded in `tools.go`; eligible target is 2.11.3 (Path B; 2.12.2 is post-cutoff).
- **CRITICAL — Broken CI composite actions**: `.github/workflows/pr-quality-gate.yml`
  references 5 composite actions that do not exist on disk: `setup-jvm`, `setup-rust`,
  `setup-elixir`, `setup-clojure`, `setup-flutter`. The `jvm` job WILL fail in CI on
  every PR because `organiclever-be` carries `tag:lang:java`. The other 4 actions are
  also broken but currently unreached because the polyglot demo apps were extracted to
  `ose-primer` (no project carries `tag:lang:rust|elixir|clojure|dart` anymore).
- **Stale CI version pins**: `.github/workflows/_reusable-backend-e2e.yml` and
  `.github/workflows/pr-validate-links.yml` pin `go-version: "1.26.0"` — should be
  bumped to `"1.26.3"` or floating `"1.26"`.
- **Volta unmaintained**: last release Dec 2024 (v2.0.2); maintainers recommend `mise`.
  [Judgment call: based on GitHub activity as of 2026-05-15 — no official "deprecated"
  announcement found; classify as de-facto unmaintained pending verification.]
  Not blocking but flagged for a future migration plan.

## Goals

1. All runtimes and toolchains on policy-compliant versions per the
   [Dependency Bump Stability & Safety Policy](../../../repo-governance/development/workflow/dependency-bump-policy.md):
   either Path A (LTS) or Path B (≥60-day pre-cutoff stable + CVE-clean) or Path C
   (security-override waiver). Cutoff date: **2026-03-16**.
2. All `package.json` deps updated to latest compatible version (minor/patch) or migrated
   (major versions with breaking changes).
3. All Go modules updated via `go get -u ./...` + `go mod tidy`.
4. All Spring Boot / Maven deps updated in `organiclever-be/pom.xml`.
5. Machine installs match config-file pins.
6. All CI targets pass: `typecheck`, `lint`, `test:quick`, `spec-coverage` for affected
   projects.
7. Preexisting issues resolved: doctor paths, `playwright-bdd` alignment, `web-ui` Vitest
   mismatch, `AGENTS.md` `organiclever-be` description, missing CI composite actions
   (at minimum: create `setup-jvm`; remove or fix the 4 unreachable broken jobs in
   `pr-quality-gate.yml`).
8. `golangci-lint` pin updated and `tools.go` fixed to read from correct pom.xml path.
9. CI workflow `go-version: "1.26.0"` pins updated to `"1.25.8"` (Path B; 1.26.3 is post-cutoff).

## Non-Goals

- Volta → `mise` migration (separate future plan).
- TypeScript 6.0 migration (post-cutoff per 2026-03-16 policy; deferred to future plan
  once TS 6.x has 60+ days of soak — tracked in `plans/ideas.md`).
- ESLint 10 + react-hooks 7 migration (post-cutoff; deferred — tracked in `plans/ideas.md`).
- Zod 4.x migration (post-cutoff; deferred — tracked in `plans/ideas.md`).
- lucide-react 1.x migration (post-cutoff; deferred — tracked in `plans/ideas.md`).
- @xstate/react 6.x migration (post-cutoff; deferred — tracked in `plans/ideas.md`).
- Effect v4 beta adoption (v3 is production-recommended; stay on v3).
- Upgrading Erlang to OTP 28 (stay on maintained OTP 27 branch, patch to 27.3.3).

## Success Criteria

- `npm run doctor` passes with zero warnings for all required tools.
- `npx nx affected -t typecheck lint test:quick spec-coverage` passes on `main`.
- No CVEs in known-affected packages. [Web-cited: CVE-2026-27979 (DoS via unbounded
  request buffering in Next.js 16.0.1–16.1.6) and CVE-2026-29057 confirmed patched in
  Next.js 16.2.6 per https://nvd.nist.gov/vuln/detail/CVE-2026-27979 and
  https://nextjs.org/blog, accessed 2026-05-15. See `tech-docs.md §Security Clearance
  Status` for the complete CVE list already audited.]
- Version table in [tech-docs.md](./tech-docs.md) shows "Current" = "Target" for every row.

---

## Affected Roles

This plan is executed by a single maintainer wearing the following hats:

- **Developer** — applies code, config, and dependency changes; runs local quality gates
- **DevOps / CI operator** — monitors GitHub Actions after push; fixes CI failures
- **Security auditor** — verifies CVE patches applied; confirms no new vulnerabilities introduced

Agents that consume this plan:

- `plan-executor` — executes the delivery checklist step by step
- `plan-execution-checker` — validates completed execution matches expected outcomes

No sign-off or approval ceremonies required; code review via git history is the only gate.

---

## Business Impact

**Pain points addressed by this plan:**

1. **Security exposure**: Next.js 16.1.6 has two publicly disclosed DoS CVEs; Spring Boot
   4.0.4 has 8 CVEs patched in 4.0.6. Every day without the patch is a day of exposure.
2. **Developer friction**: Version scatter (playwright-bdd at three different pins, vitest
   mismatch in web-ui, @vitejs/plugin-react at three different versions) causes CI
   non-determinism and makes it harder to diagnose failures.
3. **Broken CI in production**: The missing `setup-jvm` composite action means every PR that
   touches `organiclever-be` (tagged `lang:java`) fails CI — blocking the Java app's
   development pipeline entirely.
4. **Stale doctor output**: rhino-cli's `npm run doctor` reads from non-existent paths, so
   the health tool reports false data and misleads the developer about runtime versions.
5. **Major version lag risk**: Waiting longer to adopt TypeScript 6, ESLint 10, and other
   major versions increases migration difficulty as the codebase grows.

**Expected benefits after completion:**

- Security CVEs resolved for all user-facing apps
- CI pipeline unblocked for Java backend development
- `npm run doctor` gives accurate, trustworthy runtime version data
- Developer toolchain consistency: one Playwright version, one Vitest version, one Shiki
  version across all apps
- Future major-version migrations easier from a known-good baseline

---

## Business Risks

| Risk                                                                                                      | Likelihood | Mitigation                                                                                                                         |
| --------------------------------------------------------------------------------------------------------- | ---------- | ---------------------------------------------------------------------------------------------------------------------------------- |
| Next.js 16.2.6 (Path C waiver, post-cutoff) introduces a regression                                       | LOW        | Phase 6 includes build + test:quick + dev-server spot-check; CVE patch is the driver; rollback = `git revert` Phase 6 commit       |
| React 19.2.6 (Path C waiver, post-cutoff) introduces a regression                                         | LOW        | Phase 6 includes typecheck + test:quick; CVE-2025-55182 makes staying on 19.1.x riskier than the waiver; rollback = revert Phase 6 |
| mermaid 11.15.0 (Path C waiver, post-cutoff) introduces a regression                                      | VERY LOW   | Security fix only (CSS injection CVEs); functional change risk low; rollback = revert Phase 2E                                     |
| Shiki 4.x API change breaks syntax highlighting in ayokoding-web / ose-web                                | LOW        | Phase 10 includes dev-server spot-check and Playwright MCP verification; rollback = `git revert` Phase 10 commit                   |
| Storybook 10.2.10 DOWNGRADE breaks web-ui component stories                                               | VERY LOW   | 10.2.10 is stable; DOWNGRADE is CVE-driven; rollback = revert Phase 7C                                                             |
| Spring Boot 4.0.6 introduces a regression                                                                 | VERY LOW   | 4.0.4 → 4.0.6 is a pure patch; Phase 4 includes unit tests and curl health check; rollback = revert pom.xml                        |
| Schedule risk: plan is large (15 phases) and may span multiple sessions                                   | HIGH       | Phases are independent commits; any phase can be completed and pushed independently; plan does not need to be done in one session  |
| DEFERRED major migrations (TS 6, ESLint 10, Zod 4, lucide-react 1.x, @xstate/react 6) tracked in ideas.md | LOW        | Policy-compliant deferral; each item has 60-day soak condition that governs the next bump plan                                     |
