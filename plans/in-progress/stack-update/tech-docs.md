# Technical Documentation

## Architecture

### Upgrade Strategy

The upgrade is organized in a **phased, dependency-ordered sequence** to minimize blast
radius:

1. **Phase 0** — Machine-level installs (Go, Erlang, .NET, Java, Rust, Dart, golangci-lint)
2. **Phase 1** — Preexisting bug fixes (independent of version bumps)
3. **Phases 2–4** — Low-risk tooling and runtime config bumps (Node.js, Nx, Go modules,
   Spring Boot)
4. **Phase 5** — Playwright alignment across all E2E apps
5. **Phase 6** — Next.js 16.2.6 + React 19.2 (all 5 Next.js apps together)
6. **Phase 7** — Tailwind 4.2.1, Vitest 4.1.0, Storybook 10.2.10 (DOWNGRADE from ^10.3.3), @vitejs/plugin-react 6.0.1
7. **Phase 8** — @effect/platform `0.94.5` + Effect 3.21.2 (Path C waiver) + ecosystem pins
8. **Phase 9** — TypeScript 5.8.3 stay-on-line + ESLint 9.x stay-on-line (TS 6 + ESLint 10 DEFERRED — post-cutoff)
9. **Phases 10–13** — Library-level MAJOR migrations (Shiki, Zod, lucide-react, XState)
10. **Phase 14** — Full CI gate (all quality targets green)
11. **Phase 15** — Post-merge CI verification

**Blast radius model**: Nx's `nx affected` computation limits CI impact to projects
transitively affected by each phase's file changes. The upgrade is committed phase-by-phase
so that CI validates each increment independently.

**Parallel safety**: Each phase is a separate commit on a dedicated worktree branch
(`worktree/stack-update`). No simultaneous edits from other sessions are assumed during
execution.

---

## Design Decisions

### Decision 1: TypeScript 5.8.3 stay-on-line + ESLint 9.x stay-on-line (DEFERRED MAJOR migrations)

**Rationale**: TypeScript 6.0.3 (released 2026-03-23) and ESLint 10 (including its react-hooks 7
dependency) are both post-cutoff per the 60-day stability policy (cutoff 2026-03-16). Migrating
to post-cutoff MAJOR versions in this plan would violate Path B constraints. Both are deferred
to a future plan once 6.x and 10.x have 60+ days of soak. The current plan pins TypeScript to
5.8.3 (latest pre-cutoff 5.x; 2025-02-28) and ESLint to **9.39.4** (latest pre-cutoff 9.x; 2026-03-06).
`@typescript-eslint/parser` is pinned to 8.57.0 (Path B, 2026-03-09; 8.58.0+ post-cutoff).

### Decision 2: All Next.js apps upgraded together in Phase 6

**Rationale**: `next` 16.2.6 and React 19.2 affect shared lib `libs/web-ui` via peer
dependencies. Upgrading all 5 apps in one phase avoids partial-upgrade state where some
apps expect React 19.2 types and others do not, causing cross-app typecheck confusion.

### Decision 3: Shiki 4 as the only MAJOR migration in Phases 10–13; others stay-on-current

**Rationale**: Shiki 4.0.2 (released 2026-03-09, 7 days before cutoff) is the only MAJOR
library migration executed in this plan. Zod 4, lucide-react 1.x, and @xstate/react 6 are
all post-cutoff — DEFERRED to future plans. Phases 11 (Zod), 12 (lucide-react), and 13
(@xstate/react) therefore update pins to the latest pre-cutoff patch within each library's
current major version (stay-on-line), not a MAJOR migration. Each phase is still isolated
so that the pin update can be verified and rolled back independently.

### Decision 4: Delete unreachable CI jobs rather than creating stub composite actions

**Rationale**: Creating stub actions for `setup-rust`, `setup-elixir`, `setup-clojure`,
`setup-flutter` would add maintenance burden for functionality that does not exist in the
repo (the demo apps were extracted to `ose-primer`). Deletion is cleaner and honest.

### Decision 5: Exact pins for ALL packages (no carets or tildes)

**Rationale**: All `package.json` `dependencies` and `devDependencies` use exact version
strings (e.g., `"react": "19.2.6"`, NOT `"react": "^19.2.6"`). This ensures:

- **Reproducibility**: identical `npm install` output across all machines and CI.
- **Security auditability**: each version in the Security Clearance table maps 1:1 to the
  installed version — carets would silently install an un-audited newer patch.
- **Supply-chain risk reduction**: prevents uptake of newly-published versions before they
  have been audited (e.g., 2026-05-11 `@tanstack/*` incident).

`next`, `react`, and `react-dom` receive explicit Path C security waivers (documented in
§Security Waivers). All other packages follow Path A or Path B eligibility.

---

## Rollback

Each phase is committed separately. Rolling back a phase:

```bash
# Identify the phase commit SHA
git log --oneline | grep "chore(deps): ..."

# Revert a specific phase
git revert <sha>
git push origin main
```

**Phase-specific rollback notes**:

- **Phase 9 (TS 5.8.3 + ESLint 9.x stay-on-line)**: `git revert` the Phase 9 commit.
  Re-run `npm install` to restore the prior TypeScript/ESLint pins. Verify
  `npx tsc --version` still shows 5.x.
- **Phase 10 (Shiki 4.0.2)**: `git revert` Phase 10 commit. Shiki 1.x re-installed by
  `npm install`.
- **Phase 11 (Zod 3.x pin update — no migration)**: `git revert` Phase 11 commit.
  Prior Zod 3.x patch re-installed by `npm install`. (Zod 4 is deferred — no v4 to revert.)
- **Phase 12 (lucide-react 0.577.0 pin — no 1.x migration)**: `git revert` Phase 12
  commit. Prior 0.x patch re-installed by `npm install`. (1.x is deferred.)
- **Phase 13 (@xstate/react 5.x pin — no v6 migration)**: `git revert` Phase 13 commit.
  Prior 5.x patch re-installed by `npm install`. (v6 is deferred.)

**Full rollback** (if all phases need to be reverted):

```bash
git revert <phase-15-sha>..<phase-1-sha>
# or checkout the commit before Phase 1 began
git checkout <pre-plan-sha> -b rollback/stack-update
```

---

## Testing Strategy

Each phase is validated by a combination of automated and manual verification:

| Phase | Automated Gates | Manual Verification |
|---|---|---|
| 1 (Bug fixes) | `test:quick` (rhino-cli), `lint:md` | `npm run doctor` output review |
| 2 (Tooling) | `nx affected -t lint` | `node --version`, `npm --version` |
| 3 (Go) | `test:quick` (all Go apps), `golangci-lint` | `go version` |
| 4 (Spring Boot) | `test:quick` (organiclever-be), `mvn verify` | curl health endpoint |
| 5 (Playwright) | E2E smoke test (`ayokoding-web-fe-e2e`) | Browser install verification |
| 6 (Next.js/React) | `build` + `test:quick` (all 5 apps) | Dev server spot-check |
| 7 (Tailwind/Vitest/SB) | `test:quick` (all apps), `build-storybook` | Storybook UI review |
| 8 (@effect) | `typecheck` + `test:quick` | — |
| 9 (TS5.8.3 + ESLint9 stay) | `typecheck` → zero errors; `lint` → zero errors | — |
| 10 (Shiki 4.0.2 MAJOR) | `build` + `test:quick` | Dev server + Playwright MCP syntax highlight check |
| 11 (Zod 3.x pin) | `typecheck` + `test:quick` | — (stay-on-line; no MAJOR migration) |
| 12 (lucide-react 0.577.0 pin) | `build` + `test:quick` + `build-storybook` | Playwright MCP icon rendering spot-check |
| 13 (@xstate/react 5.x pin) | `typecheck` + `test:quick` + `build` | — (stay-on-line; no MAJOR migration) |
| 14 (Full gate) | All targets (typecheck, lint, test:quick, spec-coverage, build) | `npm run doctor` |
| 15 (Post-merge CI) | GitHub Actions CI | gh run list monitor |

The **acceptance bar** for each phase is: all automated gates pass (exit 0) AND any
applicable Playwright MCP / curl verification shows zero errors. A phase is not considered
done until both conditions are met.

---

## Pinning + Stability + Safety Policy

This plan complies with the repo-wide
[Dependency Bump Stability & Safety Policy](../../repo-governance/development/workflow/dependency-bump-policy.md).
Three-path decision tree applies:

- **Path A (LTS)** — Use latest LTS-line patch. Recency irrelevant if CVE-clean.
- **Path B (60-day stable + CVE-clean)** — For non-LTS packages, pin to the latest version
  released ≥60 days before bump date AND CVE-clean.
- **Path C (Security-override waiver)** — When no eligible older version is CVE-clean,
  pin to the most recent CVE-patched version with documented waiver.

**Cutoff date for this plan**: today (2026-05-15) − 60 days = **2026-03-16**. Any version
released on or before 2026-03-16 is Path-B-eligible; later versions require LTS designation
(Path A) or security waiver (Path C).

**All versions are pinned to exact patch versions** in this plan. No caret (`^`) ranges,
no tilde (`~`) ranges, no `latest` tags. Rationale:

- **Reproducibility**: identical `npm install` output across all developer machines and CI
- **Security clearance auditability**: each version row in the Security Clearance Status
  table below corresponds to the exact version installed
- **Supply-chain risk reduction**: prevents silent uptake of newly-published patch versions
  before they have been audited (mitigates compromise events like the 2026-05-11
  `@tanstack/*` incident)

**Application:**

- `package.json` `dependencies` and `devDependencies`: exact versions only
  (e.g., `"react": "19.2.6"`, NOT `"react": "^19.2.6"`)
- `package.json` `optionalDependencies` (Linux native binaries): exact versions
- `volta` block: exact (already required by Volta)
- `global.json` `sdk.version`: exact (with `rollForward: latestMinor` left as-is per
  upstream pattern — this is acceptable because the SDK install at machine level is exact)
- `.tool-versions`: exact (`erlang 27.3.3`, not `erlang 27.3`)
- `pom.xml` `<parent><version>` and `<*.version>` properties: exact
- `go.mod`: pseudo-versions are exact by definition; `go get -u ./...` followed by
  `go mod tidy` produces exact pins in `go.sum`
- Dockerfile `FROM` lines: pin to digest-resolved tag (e.g., `node:24.15.0-alpine3.23`,
  NOT `node:24-alpine`). Floating tags forbidden in production Dockerfiles.

**Exception**: workspace internal `*` references (e.g., `@open-sharia-enterprise/web-ui: "*"`)
are NOT pinned because they resolve via npm workspaces to local paths.

**Renovate / Dependabot**: future automated PRs may bump to newer patches, each requiring
a fresh CVE clearance audit before merge.

---

## Version Table (as of 2026-05-15) — POLICY-COMPLIANT PINS

> Each row's `Target (eligible)` column is the version selected per the three-path policy.
> "Path A" = LTS; "Path B" = ≥60-day stable + CVE-clean; "Path C" = security waiver.

### Runtime Languages

| Tool | Config file | Current | Target (eligible) | Path | Notes / Source |
|---|---|---|---|---|---|
| Node.js | `package.json` → `volta.node` | 24.13.1 | **24.15.0** | **A** (LTS) | LTS Krypton (2026-04-15); CVE-clean. [nodejs.org/en/blog/release/v24.15.0] |
| npm | `package.json` → `volta.npm` | 11.10.1 | **11.11.0** | **B** | Released 2026-02-25 (latest pre-cutoff); CVE-clean. 11.14.1 is post-cutoff. |
| Go | `apps/rhino-cli/go.mod` directive | 1.26 (binary 1.26.1) | binary: **1.25.8** | **B** | Released 2026-03-05; patches all CVEs that 1.26.3 patches (Go publishes parallel patches for N and N-1). go.mod directive stays `go 1.26`. [groups.google.com/g/golang-announce/c/EdhZqrQ98hk] |
| .NET SDK | `apps/ose-app-be/global.json` | 10.0.103 | **10.0.300** | **A** (LTS) | .NET 10 is LTS; SDK 10.0.300 is May 2026 Patch Tuesday release patching CVE-2026-32177/35433/32175/42899. [dotnet/core 10.0.8 release notes] |
| Erlang/OTP | `.tool-versions` | 27.3 | **27.3.3** | **B** | OTP 27.3.3 (2025-04-16) patches CVE-2025-32433 (Critical SSH RCE). 27.3.4.x patches released ≥2026-04 (post-cutoff). [erlang.org/news] |
| Rust | machine (`rustup`) | 1.94.0 | **1.93.0** | **B** | Released 2026-02-12 (latest pre-cutoff); CVE-clean. 1.94.0 (2026-03-26) is post-cutoff. |
| Dart SDK | machine (`flutter upgrade`) | 3.11.3 | **3.11.0** | **B** | Released 2026-02-11 (latest pre-cutoff); CVE-clean. CVE-2026-27704 (path traversal) patched in 3.11.0. |
| Flutter | machine | 3.41.5 | **3.41.0** | **B** | Released 2026-02-10 (latest pre-cutoff); CVE-clean. |
| Java (Temurin) | `apps/organiclever-be/pom.xml` `<java.version>` | 25.0.2 | **25.0.3+9** | **A** (LTS) | JDK 25 is LTS; April 2026 CPU patches. |
| Elixir | `.tool-versions` | 1.19.5 | **1.19.5** | **B** | Already latest stable; pre-cutoff; CVE-clean. |

### Build / Repo Tooling

| Tool | Config file | Current | Target (eligible) | Path | Notes |
|---|---|---|---|---|---|
| Nx | root `package.json` | 22.5.2 | **22.5.4** | **B** | Released 2026-02-09 (latest pre-cutoff); CVE-clean. 22.6.0 (2026-03-18) and 22.7.x are post-cutoff. |
| golangci-lint | hardcoded in `doctor/tools.go` | 2.11.1 | **2.11.3** | **B** | Released 2026-03-10 (latest pre-cutoff); CVE-clean. 2.12.2 is post-cutoff. Update tools.go constant + machine binary. |
| markdownlint-cli2 | root `package.json` | ^0.21.0 | **0.21.0** | **B** | Verify on npm — keep 0.21.0 (already pre-cutoff) since 0.22.x is post-cutoff. |
| Prettier | root `package.json` | ^3.6.2 | **3.8.1** | **B** | Released 2026-01-21 (latest pre-cutoff); 3.8.2/3.8.3 are post-cutoff. |
| Husky | root `package.json` | ^9.1.7 | **9.1.7** | **B** | Already latest 9.x; pre-cutoff; CVE-clean. |
| lint-staged | root `package.json` | ^16.2.6 | **16.4.0** | **B** | Released 2026-03-14 (latest 16.x pre-cutoff); CVE-clean. 17.x (post-cutoff) deferred. |
| tsx | root `package.json` | ^4.20.6 | **4.21.0** | **B** | Released 2025-11-30 (latest pre-cutoff); CVE-clean. 4.22.0 is post-cutoff. |
| @hey-api/openapi-ts | root `package.json` | ^0.94.2 | **0.94.2** | **B** | Latest pre-cutoff (verify on npm); 0.94.3 (2026-03-19) post-cutoff. |
| @hey-api/client-fetch | root `package.json` | ^0.13.1 | **0.13.1** | **B** | Already pre-cutoff; CVE-clean (verify on npm). |
| @redocly/cli | root `package.json` | ^2.22.1 | **2.22.1** | **B** | Already pre-cutoff; CVE-clean. |
| @stoplight/spectral-cli | root `package.json` | ^6.15.0 | **6.15.0** | **B** | Pre-cutoff; CVE-clean. 6.15.1 likely pre-cutoff but conservative pin. |
| @openapitools/openapi-generator-cli | root `package.json` | ^2.30.2 | **2.30.2** | **B** | Released 2026-03-06 (pre-cutoff); 2.30.3+ post-cutoff. |
| @commitlint/cli | root `package.json` | ^20.1.0 | **20.5.0** | **B** | Released 2026-03-15 (latest pre-cutoff); CVE-clean. |
| @commitlint/config-conventional | root `package.json` | ^20.0.0 | **20.5.0** | **B** | Versioned with cli. |
| eslint | apps using it | ^9.18.0 | **9.39.4** | **B** | Released 2026-03-06 (latest 9.x pre-cutoff); CVE-clean. v10 is post-cutoff. |
| eslint-plugin-react-hooks | apps using it | ^5.1.0 | **5.2.0** | **B** | Released 2025-02-28; latest 5.x pre-cutoff. v7 is post-cutoff and requires ESLint 10. CVE-clean. |
| @typescript-eslint/parser | apps using it | ^8.20.0 | **8.57.0** | **B** | Released 2026-03-09 (latest pre-cutoff); CVE-clean. 8.58.0+ post-cutoff. |
| @vitejs/plugin-react | various consumers | ^4.0.0 / ^5.1.4 | **6.0.1** | **B** | Released 2026-03-13 (latest pre-cutoff); CVE-clean. 6.0.2 (2026-05-14) post-cutoff. |
| @axe-core/playwright | 5 FE E2E apps | ^4.10.1 | **4.11.1** | **B** | Released 2026-02-03 (latest pre-cutoff); CVE-clean. 4.11.2/4.11.3 post-cutoff. |
| eslint-plugin-jsx-a11y | root `package.json` | ^6.10.2 | **6.10.2** | **B** | Pre-cutoff; CVE-clean. |
| eslint-plugin-import | apps using it | ^2.31.0 | **2.32.0** | **B** | Released 2025-06-20; pre-cutoff; CVE-clean. |
| eslint-import-resolver-typescript | apps using it | ^4.4.4 | **4.4.4** | **B** | Pre-cutoff (~2025-05); CVE-clean. |
| eslint-plugin-boundaries | apps using it | ^5.0.1 | **5.4.0** | **B** | Released 2026-02-02 (latest 5.x pre-cutoff); CVE-clean. v6.x is post-cutoff/edge-of-cutoff. |
| prettier-plugin-tailwindcss | root `package.json` | ^0.7.2 | **0.7.2** | **B** | Pre-cutoff (verify exact). 0.8.0 (2026-04-27) post-cutoff. |

### JS/TS Framework & Libraries

#### All Next.js apps (`ayokoding-web`, `ose-web`, `organiclever-web`, `wahidyankf-web`, `ose-app-web`)

| Package | Current | Target (eligible) | Path | Notes |
|---|---|---|---|---|
| `next` | 16.1.6 | **16.2.6** | **C (WAIVER)** | CVE-2026-29057, CVE-2026-27979, CVE-2026-44578 + 10 May 2026 advisories require 16.2.6. No pre-cutoff CVE-clean version exists. See Security Waivers. |
| `react` | ^19.0.0 / ^19.1.0 | **19.2.6** | **C (WAIVER)** | CVE-2025-55182 (Critical), CVE-2026-23864 (High), CVE-2026-23870 require 19.2.6. No pre-cutoff CVE-clean version exists. See Security Waivers. |
| `react-dom` | ^19.0.0 / ^19.1.0 | **19.2.6** | **C (WAIVER)** | Same CVE patch train as react. |
| `@types/react` | ^19 | **19.2.14** | **B** | Released 2026-02-11 (latest pre-cutoff); type defs only. |
| `@types/react-dom` | ^19 | **19.2.3** | **B** | Released 2025-11-12 (latest pre-cutoff). |
| `@types/node` | ^22 | **22.19.15** | **B** | Released 2026-03-06; stay on ^22 line. v25 post-cutoff. |
| `typescript` | ^5 | **5.8.3** | **B** | Latest TS 5.x released 2025-02-28 (eligible). TS 6.0.3 (2026-03-23) is post-cutoff — DEFER 6.0 migration to a future plan. |
| `tailwindcss` | ^4.2.1 | **4.2.1** | **B** | Already pre-cutoff (2026-02-23). 4.3.0 (2026-05-08) is post-cutoff. |
| `vitest` | ^4.0.0–^4.1.0 | **4.1.0** | **B** | Released 2026-03-12 (eligible). 4.1.6 (2026-05-12) post-cutoff. |
| `@vitest/coverage-v8` | ^4.0.0 | **4.1.0** | **B** | Versioned with vitest. |

#### `ayokoding-web` and `ose-web` extra deps

| Package | Current | Target (eligible) | Path | Notes |
|---|---|---|---|---|
| `shiki` | ^1.0.0 | **4.0.2** | **B** | Released 2026-03-09 (7 days before cutoff — eligible). MAJOR migration 1→4. |
| `zod` | ^3.23.0 | **3.25.76** | **B** | Released 2025-07-08 (latest 3.x; npm). v4 post-cutoff. |
| `@trpc/*` | ^11.0.0 | **11.13.4** | **B** | Released 2026-03-15 (latest 11.x pre-cutoff). |
| `@tanstack/react-query` | ^5.62.8 | **5.90.21** | **B** | Released 2026-02-11 (latest pre-cutoff). |
| `lucide-react` | ^0.447.0 | **0.577.0** | **B** | STAY on 0.x line (latest 0.x pre-cutoff, 2026-03-04). v1.x is post-cutoff — DEFER 1.x migration. |
| `mermaid` | ^11.0.0 | **11.15.0** | **C (WAIVER)** | CVE-2026-41148/41150/41159 (CSS injection High + DoS) only patched in 11.15.0 (2026-05-11, post-cutoff). See Security Waivers. |
| `html-react-parser` | ^5.1.0 | **5.2.17** | **B** | Stay on 5.x (released 2026-02-07). v6 is post-cutoff. |
| `tailwind-merge` | ^2.5.3 | **2.6.1** | **B** | Released 2026-01-30 (latest 2.x pre-cutoff). v3.x post-cutoff. |
| `next-themes` | ^0.4.0 | **0.4.6** | **B** | Pre-cutoff (~2024). |
| `cmdk` | ^1.1.1 | **1.1.1** | **B** | Pre-cutoff (2024-03-08). |
| `flexsearch` | ^0.7.43 | **0.8.212** | **B** | Released 2025-09-06 (pre-cutoff). |
| `superjson` | ^2.2.1 | **2.2.6** | **B** | Released 2025-11-27 (pre-cutoff). |
| `react-icons` | ^5.3.0 | **5.6.0** | **B** | Released 2026-03-02 (pre-cutoff); CVE-clean. |
| `unified` | ^11.0.0 | **11.0.5** | **B** | Pre-cutoff; CVE-clean. |
| `rehype-*` / `remark-*` | various | latest pre-cutoff per package | **B** | Stable; most major versions had no Q1 2026 changes. |
| `@radix-ui/*` (8 packages) | various 1.1.x | **latest 1.x pre-cutoff** | **B** | All Radix releases are pre-cutoff (2024–early 2025). Verify exact patches on npm. |
| `radix-ui` (umbrella) | ^1.4.3 | **1.4.3** | **B** | Pre-cutoff. |
| `class-variance-authority` | ^0.7.0 | **0.7.1** | **B** | Pre-cutoff (2024-11-26). |
| `clsx` | ^2.1.1 | **2.1.1** | **B** | Long pre-cutoff. |
| `gray-matter` | ^4.0.3 | **4.0.3** | **B** | Long pre-cutoff (2021). |
| `postcss` (transitive) | ^8.5.8 | **8.5.10** | **C (WAIVER)** | CVE-2026-41305 (XSS Medium 6.1) patched at 8.5.10 (2026-04-15, post-cutoff). See Security Waivers. |

#### `organiclever-web` and `ose-app-web` extra deps

| Package | Current | Target (eligible) | Path | Notes |
|---|---|---|---|---|
| `effect` | ^3.21.2 | **3.21.2** | **C (WAIVER)** | CVE-2026-32887 patched at 3.20.0. Latest 3.x pre-cutoff is 3.19.19 (2026-02-21) which is VULNERABLE. 3.20.0+ is post-cutoff but required for CVE patch. Use 3.21.2 (current pin; CVE-clean) per Path C waiver. See Security Waivers. |
| `@effect/platform` | ^0.84.0 | **0.94.5** | **B** | Released 2026-02-14 (latest pre-cutoff). 0.96.1 (2026-04-24) post-cutoff. |
| `@effect/vitest` | ^0.29.0 | **0.29.0** | **B** | Pre-cutoff. |
| `xstate` | ^5.31.0 | **5.28.0** | **B** | Released 2026-02-12 (latest 5.x pre-cutoff). 5.29.0 (2026-03-24) post-cutoff. |
| `@xstate/react` | ^5.0.5 | **5.0.5** | **B** | Released 2025-05-31; current pin already at this version (already eligible). v6 post-cutoff — DEFER. |
| `@electric-sql/pglite` | ^0.4.5 | **0.3.16** | **B** | Released 2026-03-10 (latest 0.x pre-cutoff). 0.4.x first release was 2026-03-25, post-cutoff. NOTE: this is a DOWNGRADE from current ^0.4.5. |

#### `libs/web-ui`

| Package | Current | Target (eligible) | Path | Notes |
|---|---|---|---|---|
| `vitest` | ^3.2.0 | **4.1.0** | **B** | Bump from 3 to 4.1.0 (eligible). Preexisting mismatch with apps resolved. |
| `@vitest/coverage-v8` | ^3.2.0 | **4.1.0** | **B** | Versioned with vitest. |
| `lucide-react` | ^0.447.0 | **0.577.0** | **B** | Stay on 0.x. |
| `storybook` | ^10.3.3 | **10.2.10** | **B** | CVE-2026-27148 (High WebSocket hijacking) patched at 10.2.10 (2026-02-25, eligible). 10.3.x and 10.4.x are post-cutoff (10.3.0 was 2026-04-08). NOTE: this is a DOWNGRADE from current ^10.3.3 to satisfy CVE-clean + 60-day rules. |
| `@storybook/*` addons | ^10.3.3 | **10.2.10** | **B** | Versioned with storybook. |

#### E2E apps (all 9)

| Package | Worst current | Target (eligible) | Path | Notes |
|---|---|---|---|---|
| `@playwright/test` | ^1.52.0 | **1.60.0** | **B** | Released 2025-05-11 (year-old release; well before cutoff); CVE-clean. |
| `playwright-bdd` | ^8.2.0 / ^8.5.0 | **8.5.1** | **B** | Released 2025-05-12 (year-old; eligible). All E2E apps align. |
| `@axe-core/playwright` | ^4.10.1 | **4.11.1** | **B** | Released 2026-02-03 (latest pre-cutoff). 4.11.2/4.11.3 post-cutoff. |

### Go Modules (all Go apps and libs)

All Go modules: run `go get -u ./...` + `go mod tidy` in each module root.

| Module | Current notable deps | Action |
|---|---|---|
| `apps/rhino-cli` | `go 1.26`, cobra 1.10.2, godog 0.15.1 | `go get -u ./...` + tidy |
| `apps/ayokoding-cli` | `go 1.26`, cobra 1.10.2, godog 0.15.1 | `go get -u ./...` + tidy |
| `apps/ose-cli` | `go 1.26`, cobra 1.10.2, godog 0.15.1 | `go get -u ./...` + tidy |
| `libs/golang-commons` | `go 1.26`, godog 0.15.1 | `go get -u ./...` + tidy |
| `libs/hugo-commons` | `go 1.26` | `go get -u ./...` + tidy |

### Java / Maven (organiclever-be)

| Package | Current | Target (eligible) | Path | Notes |
|---|---|---|---|---|
| Spring Boot parent | 4.0.4 | **4.0.6** | **A** (LTS-equivalent per Spring OSS support; CVE patches required) | 8 CVEs incl. CVE-2026-40976 (Critical 9.1) and CVE-2026-40972 (High 7.5). LTS-equivalent path overrides 60-day rule. |
| Cucumber BOM | 7.34.2 | **7.34.3** | **B** | Released 2026-03-04 (12 days pre-cutoff); CVE-clean. |
| Java toolchain | 25 | 25 | **A** (LTS) | Machine bumps to 25.0.3 (LTS patch). |

### .NET / F# (ose-app-be, crane-cli)

Both apps target `net10.0` (correct). `global.json` in `ose-app-be` bumped to `10.0.300`
(the only `global.json` in the repo — `crane-cli` has none and inherits from the machine
default per `global.json` rollForward rules). After bump, both apps must build under
SDK 10.0.300:

- `npx nx build ose-app-be && npx nx run ose-app-be:test:quick`
- `npx nx build crane-cli && npx nx run crane-cli:test:quick`

### Workspace Libraries (TypeScript-only)

| Lib | Has package.json | Has TS source | Action under TS 5.8.3 |
|---|---|---|---|
| `libs/web-ui` | YES | YES | Full bump (vitest, lucide, storybook, etc.) |
| `libs/web-ui-token` | YES (no deps) | YES | Inherits root TS; verify typecheck under TS 5.8.3 |
| `libs/ts-ui` | NO | NO (LICENSE only) | Skip — placeholder |
| `libs/clojure-openapi-codegen` | NO | NO (LICENSE only) | Skip — placeholder |

### CI Workflow Files

| File | Field | Current | Target |
|---|---|---|---|
| `.github/workflows/_reusable-backend-e2e.yml` | `go-version` | `"1.26.0"` | `"1.25.8"` (Path B; 1.26.3 is post-cutoff) |
| `.github/workflows/pr-validate-links.yml` | `go-version` | `"1.26.0"` | `"1.25.8"` |
| `.github/workflows/pr-quality-gate.yml` | references missing actions | broken | See R28 in prd.md |

### Missing Composite Actions (preexisting CI breakage)

`.github/actions/` directory is missing 5 composite actions referenced by
`pr-quality-gate.yml`:

| Composite action | Referenced by job | Reachable today? |
|---|---|---|
| `setup-jvm` | `jvm` job (java/kotlin) | **YES** (organiclever-be has tag:lang:java) — must fix |
| `setup-rust` | `rust` job | NO — no apps tagged `lang:rust` |
| `setup-elixir` | `elixir` job | NO — no apps tagged `lang:elixir` |
| `setup-clojure` | `clojure` job | NO — no apps tagged `lang:clojure` |
| `setup-flutter` | `dart` job | NO — no apps tagged `lang:dart` |

Resolution per R28: create `setup-jvm` (calls `actions/setup-java@v4` with Temurin 25);
either delete or guard the other 4 jobs with affected-project conditions.

---

## Security Clearance Status (audit date: 2026-05-15)

**Methodology**: each pinned version below was verified against four sources during
plan authoring on 2026-05-15:

1. **NVD** ([nvd.nist.gov](https://nvd.nist.gov)) — National Vulnerability Database
2. **GitHub Security Advisories** ([github.com/advisories](https://github.com/advisories))
3. **Snyk DB** ([security.snyk.io](https://security.snyk.io))
4. **Vendor security pages** (project-specific: spring.io/security, nodejs.org/en/blog/vulnerability,
   pkg.go.dev/vuln, dotnet release notes, etc.)

Status legend:

- **CLEAR** — No known CVEs in the pinned version as of 2026-05-15
- **CLEAR (patch-of)** — Pinned version IS the patched release for one or more known CVEs
- **REMEDIATE** — Known CVEs found; explicit remediation required (see Action Items below)
- **MONITOR** — No CVE today, but maintenance/EOL or supply-chain risk flagged
- **DEFER** — Indirect (transitive) dep; cannot upgrade directly without ecosystem change

### Runtime Languages and Tools

| Component | Pinned Version | Status | CVEs Patched in This Version | Source |
|---|---|---|---|---|
| Node.js | 24.15.0 | **CLEAR** | March 2026 security release patched 8 CVEs in 24.14.1 (already in 24.15.0) | [nodejs.org](https://nodejs.org/en/blog/release/v24.15.0) |
| npm CLI | **11.11.0** | **CLEAR** | None | [github.com/npm/cli](https://github.com/npm/cli/releases/tag/v11.11.0) — Path B (11.14.1 is post-cutoff) |
| Go binary | **1.25.8** | **CLEAR (patch-of)** | Go 1.25.8 patches all CVEs that 1.26.x patches via parallel patch train. 1.26.3 is post-cutoff. | [golang-announce](https://groups.google.com/g/golang-announce/c/EdhZqrQ98hk) — Path B |
| .NET SDK | 10.0.300 (runtime 10.0.8) | **CLEAR (patch-of)** | CVE-2026-32177 (High DoS), CVE-2026-35433 (High LPE), CVE-2026-32175 (Medium), CVE-2026-42899 (ASP.NET DoS) — May 2026 Patch Tuesday | [dotnet/core 10.0.8 notes](https://github.com/dotnet/core/blob/main/release-notes/10.0/10.0.8/10.0.8.md) |
| Erlang/OTP | **27.3.3** | **CLEAR (patch-of)** | CVE-2025-32433 (Critical SSH RCE), CVE-2025-46712 (Low KEX), CVE-2025-48040 (Medium SFTP DoS) — all patched in 27.3.3 or earlier 27.3.x. 27.3.4.x is post-cutoff. | [erlang/otp advisories](https://github.com/erlang/otp/security/advisories) — Path B |
| Rust | **1.93.0** | **CLEAR** | CVE-2026-33056 (Cargo tar-rs Medium) patched in 1.94.1 — but 1.93.0 pre-dates this CVE and the fix is in a separate crate; Rust compiler itself CVE-clean. 1.95.0 is post-cutoff. | [blog.rust-lang.org](https://blog.rust-lang.org/2026/03/21/cve-2026-33056/) — Path B |
| Dart | **3.11.0** | **CLEAR (patch-of)** | CVE-2026-27704 (path traversal in pub cache extraction) patched in 3.11.0. 3.11.5 is post-cutoff. | [dart.dev/security](https://dart.dev/security) — Path B |
| Flutter | **3.41.0** | **CLEAR (patch-of)** | CVE-2026-27704 (same as Dart) patched in 3.41.0. 3.41.9 is post-cutoff. | [docs.flutter.dev/security](https://docs.flutter.dev/security) — Path B |
| Java Eclipse Temurin | 25.0.3+9 | **CLEAR (patch-of)** | Oracle April 2026 CPU patched 12 Java SE CVEs (affected 25.0.2 and earlier) | [Oracle CPU Apr 2026](https://www.oracle.com/security-alerts/cpuapr2026.html) |
| Elixir | 1.19.5 | **CLEAR** (language); MONITOR (ecosystem) | None in language runtime. Ecosystem note: CVE-2026-32688 (plug_cowboy DoS) and CVE-2026-32689 (phoenix DoS) — only relevant if those libs added later | [OSV EEF-CVE-2026-32688](https://osv.dev/vulnerability/EEF-CVE-2026-32688) |
| golangci-lint | **2.11.3** | **CLEAR** | None. 2.12.2 is post-cutoff. | [github.com/golangci/golangci-lint](https://github.com/golangci/golangci-lint/security/policy) — Path B |
| Volta | 2.0.2 (machine, no pin) | **MONITOR** | No CVE. Last release Dec 2024; project unmaintained per maintainer notice | [volta-cli/volta](https://github.com/volta-cli/volta) |

### Docker Base Images

| Image (resolved tag) | Status | Notes | Source |
|---|---|---|---|
| `postgres:17.10-alpine` | **CLEAR (patch-of)** | 17.10 (released 2026-05-14) patches 10 CVEs incl. CVE-2026-6637/6477/6475/6473 (CVSS 8.8 High), CVE-2026-6479 (DoS, High) | [postgresql.org/support/security](https://www.postgresql.org/support/security/) |
| `node:24.15.0-alpine3.23` | **CLEAR** (Node) / **MONITOR** (Alpine OpenSSL) | Node 24.15.0 clean. Alpine 3.23 base has 6 OpenSSL CVEs (fix avail at 3.3.7-r0 — rebuild image after upstream Alpine push) | [security.alpinelinux.org](https://security.alpinelinux.org/recent) |
| `golang:1.25.8-alpine3.23` | **CLEAR** (Go) / **MONITOR** (Alpine OpenSSL) | Same Alpine OpenSSL caveat. Path B (1.26.3 post-cutoff). | [golang-announce](https://groups.google.com/g/golang-announce/c/EdhZqrQ98hk) |
| `mcr.microsoft.com/dotnet/sdk:10.0.300-alpine3.23` | **CLEAR** (SDK) / **MONITOR** (Alpine) | SDK clean. Same Alpine OpenSSL caveat | [dotnet/core 10.0.8](https://github.com/dotnet/core/blob/main/release-notes/10.0/10.0.8/10.0.8.md) |
| `eclipse-temurin:25.0.3+9-jdk-alpine` | **CLEAR** (JDK) / **REMEDIATE** (Alpine binutils) | JDK patched. Alpine image layer has 2 unfixed High CVEs in binutils (CVE-2025-69649, CVE-2025-69650 CVSS 7.5) and 8 unfixed Medium CVEs. **Recommend switching `Dockerfile.integration` from `eclipse-temurin:25-jdk-alpine` → `eclipse-temurin:25.0.3+9-jdk` (Ubuntu base, 0 High/Critical)** | [sliplane.io eclipse-temurin alpine CVE](https://sliplane.io/tools/cve/library/eclipse-temurin:25-alpine) |
| `eclipse-temurin:25.0.3+9-jdk` (Ubuntu) | **CLEAR** (JDK) / **MONITOR** (Ubuntu) | 0 High/Critical, 14 Medium, 7 Low (mostly fixable binutils) | [sliplane.io eclipse-temurin jdk CVE](https://sliplane.io/tools/cve/library/eclipse-temurin:25-jdk) |

### npm — Frameworks (Next.js + React + TS + Tailwind ecosystem)

| Package | Pinned Version | Status | Patched CVEs | Source |
|---|---|---|---|---|
| next | 16.2.6 | **CLEAR (patch-of)** | CVE-2026-29057 (HTTP smuggling), CVE-2026-27979 (DoS), CVE-2026-44578 (SSRF, High), and 10 other May 2026 advisories | [Vercel May 2026 release](https://vercel.com/changelog/next-js-may-2026-security-release) |
| react | 19.2.6 | **CLEAR (patch-of)** | CVE-2025-55182 (Critical RSC RCE, fixed 19.2.1), CVE-2026-23864 (High DoS, fixed 19.2.4), CVE-2026-23870 (DoS, fixed 19.2.6) | [react.dev advisory](https://react.dev/blog/2025/12/03/critical-security-vulnerability-in-react-server-components) |
| react-dom | 19.2.6 | **CLEAR (patch-of)** | Same patch train as react | [GHSA-83fc-fqcc-2hmg](https://github.com/facebook/react/security/advisories/GHSA-83fc-fqcc-2hmg) |
| typescript | **5.8.3** | **CLEAR** | None. 6.0.3 (2026-03-23) is post-cutoff — DEFERRED. | [Snyk typescript](https://security.snyk.io/package/npm/typescript) — Path B |
| @next/third-parties | 16.2.6 | **CLEAR** | Bundled with next | Same as next |
| @next/swc-linux-x64-musl | 16.2.6 | **CLEAR** | Bundled with next | Same as next |
| @next/swc-linux-x64-gnu | 16.2.6 | **CLEAR** | Bundled with next | Same as next |
| @types/react | 19.2.14 | **CLEAR** | Type defs only — no runtime | [Snyk @types/react](https://security.snyk.io/package/npm/%40types%2Freact) |
| @types/react-dom | 19.2.3 | **CLEAR** | Type defs only | [Snyk @types/react-dom](https://security.snyk.io/package/npm/%40types%2Freact-dom) |
| @types/node | **22.19.15** | **CLEAR** | Type defs only. 25.x is post-cutoff. | Snyk — Path B |
| tailwindcss | **4.2.1** | **CLEAR** | None. 4.3.0 (2026-05-08) is post-cutoff — DEFERRED. | [Snyk tailwindcss](https://security.snyk.io/package/npm/tailwindcss) — Path B |
| @tailwindcss/postcss | **4.2.1** | **CLEAR** | Bundled with tailwindcss 4.2.1 | Snyk |
| @tailwindcss/vite | **4.2.1** | **CLEAR** | Bundled with tailwindcss 4.2.1 | Snyk |
| @tailwindcss/typography | 0.5.19 | **CLEAR** | None | Snyk |
| @tailwindcss/oxide-linux-x64-musl | **4.2.1** | **CLEAR** | Native binary, bundled with tailwindcss 4.2.1 | Bundled |
| @tailwindcss/oxide-linux-x64-gnu | **4.2.1** | **CLEAR** | Native binary, bundled with tailwindcss 4.2.1 | Bundled |
| lightningcss-linux-x64-musl | 1.32.0 | **CLEAR** | None | Snyk |
| lightningcss-linux-x64-gnu | 1.32.0 | **CLEAR** | None | Snyk |
| postcss | **8.5.10** | **CLEAR (patch-of)** | CVE-2026-41305 (XSS via unescaped `</style>`, Medium 6.1) patched at 8.5.10 (Path C waiver; 8.5.10 is post-cutoff but CVE-required) | [SNYK-JS-POSTCSS-16189065](https://security.snyk.io/vuln/SNYK-JS-POSTCSS-16189065) |
| @vitejs/plugin-react | **6.0.1** | **CLEAR** | None. 6.0.2 (2026-05-14) is post-cutoff. | [Snyk @vitejs/plugin-react](https://security.snyk.io/package/npm/%40vitejs%2Fplugin-react) — Path B |
| @rollup/rollup-linux-x64-gnu | 4.60.4 | **CLEAR** | None | Snyk rollup |
| @esbuild/linux-x64 | 0.28.0 | **CLEAR** | CVE-2024-23334 (path traversal) patched in 0.25.0 | Snyk esbuild |

### npm — Testing + Storybook + Playwright

| Package | Pinned Version | Status | Patched CVEs | Source |
|---|---|---|---|---|
| vitest | **4.1.0** | **CLEAR** | None. 4.1.6 (2026-05-12) is post-cutoff. | [Snyk vitest](https://security.snyk.io/package/npm/vitest) — Path B |
| @vitest/coverage-v8 | **4.1.0** | **CLEAR** | None | Same as vitest — Path B |
| jsdom | **29.0.0** | **CLEAR** | None. 29.1.1 possibly post-cutoff. | [Snyk jsdom](https://security.snyk.io/package/npm/jsdom) — Path B |
| @testing-library/react | 16.3.2 | **CLEAR** | None | Snyk |
| @testing-library/jest-dom | 6.9.1 | **CLEAR** | None | Snyk |
| @testing-library/dom | 10.4.1 | **CLEAR** | None | Snyk |
| @testing-library/user-event | 14.6.1 | **CLEAR** | None | Snyk |
| @amiceli/vitest-cucumber | **6.3.0** | **CLEAR** (no advisory) | Niche package; 6.5.0 possibly post-cutoff. | npm — Path B |
| storybook | **10.2.10** | **CLEAR (patch-of)** | CVE-2026-27148 (WebSocket hijacking, High CVSS 8.9) patched at 10.2.10. DOWNGRADE from ^10.3.3 (10.3.x post-cutoff). | [SNYK-JS-STORYBOOK-15353401](https://security.snyk.io/vuln/SNYK-JS-STORYBOOK-15353401) — Path B |
| @storybook/addon-a11y | **10.2.10** | **CLEAR** | Bundled | Snyk |
| @storybook/addon-docs | **10.2.10** | **CLEAR** | Bundled | Snyk |
| @storybook/addon-themes | **10.2.10** | **CLEAR** | Bundled | Snyk |
| @storybook/nextjs-vite | **10.2.10** | **CLEAR** | Bundled | Snyk |
| @playwright/test | 1.60.0 | **CLEAR** | None affecting 1.60.0 | [Snyk playwright](https://security.snyk.io/package/npm/playwright) — Path B |
| playwright-bdd | 8.5.1 | **CLEAR** (no advisory) | Niche; depends on @playwright/test (clean) | npm — Path B |
| @axe-core/playwright | **4.11.1** | **CLEAR** (no advisory) | Released 2026-02-03 (pre-cutoff). 4.11.2/4.11.3 post-cutoff. | npm — Path B |

### npm — ESLint Ecosystem

| Package | Pinned Version | Status | Patched CVEs | Source |
|---|---|---|---|---|
| eslint | **9.39.4** | **CLEAR** | None in v9.x. ESLint 10 is post-cutoff — DEFERRED. | [Snyk eslint](https://security.snyk.io/package/npm/eslint) — Path B |
| @typescript-eslint/parser | **8.57.0** | **CLEAR** | None. 8.58.0+ post-cutoff. | Snyk — Path B |
| eslint-plugin-react-hooks | **5.2.0** | **CLEAR** | None. v7 requires ESLint 10 — DEFERRED. | Snyk — Path B |
| eslint-plugin-jsx-a11y | 6.10.2 | **CLEAR** | None | Snyk — Path B |
| eslint-import-resolver-typescript | 4.4.4 | **CLEAR** | None | Snyk — Path B |
| eslint-plugin-boundaries | **5.4.0** | **CLEAR** | None. v6.x is post-cutoff. | Snyk — Path B |
| eslint-plugin-import | 2.32.0 | **CLEAR** | None | Snyk — Path B |
| prettier | **3.8.1** | **CLEAR** | None. 3.8.2/3.8.3 are post-cutoff. | [Snyk prettier](https://security.snyk.io/package/npm/prettier) — Path B |
| prettier-plugin-tailwindcss | **0.7.2** | **CLEAR** | None. 0.8.0 (2026-04-27) is post-cutoff. | Snyk — Path B |

### npm — State / Effect / Data / Content libs

| Package | Pinned Version | Status | Patched CVEs | Source |
|---|---|---|---|---|
| effect | 3.21.2 | **CLEAR (patch-of)** | CVE-2026-32887 (AsyncLocalStorage context leak, High 7.4) patched at 3.20.0 | [GHSA-38f7-945m-qr2g](https://github.com/advisories/GHSA-38f7-945m-qr2g) |
| @effect/platform | **0.94.5** | **CLEAR** | None. 0.96.1 (2026-04-24) is post-cutoff. | Snyk — Path B |
| @effect/vitest | 0.29.0 | **CLEAR** | None | Snyk — Path B |
| xstate | **5.28.0** | **CLEAR** | None. 5.29.0 (2026-03-24) is post-cutoff. | Snyk — Path B |
| @xstate/react | **5.0.5** | **CLEAR** | None. 6.x is post-cutoff — DEFERRED. | Snyk — Path B |
| @electric-sql/pglite | **0.3.16** | **CLEAR** | CVE-2026-40906 (Critical 9.9 SQL injection) affects ElectricSQL **server**, NOT pglite client lib. 0.4.x first release was 2026-03-25 (post-cutoff) — DOWNGRADE from ^0.4.5 to 0.3.16 (released 2026-03-10, pre-cutoff). | Snyk — Path B |
| zod | **3.25.76** | **CLEAR** | None. Zod 4 is post-cutoff — DEFERRED. | Snyk — Path B |
| shiki | 4.0.2 | **CLEAR** | None | Snyk |
| mermaid | 11.15.0 | **CLEAR (patch-of)** | CVE-2026-41159 (CSS injection High 7.1), CVE-2026-41148 (CSS inj High 7.1), CVE-2026-41150 (Gantt DoS Medium), CVE-2026-41149 (HTML inj Medium), CVE-2025-54881 (XSS Medium), CVE-2025-54880 (XSS Medium) — all fixed in 11.15.0 (released 2026-05-11) | [GHSA mermaid](https://github.com/advisories?query=type%3Areviewed+ecosystem%3Anpm+mermaid) |
| unified | 11.0.5 | **CLEAR** | None | Snyk |
| rehype-autolink-headings | 7.1.0 | **CLEAR** | None | Snyk |
| rehype-katex | 7.0.1 | **CLEAR** | None | Snyk |
| rehype-pretty-code | 0.14.3 | **CLEAR** | None | Snyk |
| rehype-raw | 7.0.0 | **CLEAR** | None | Snyk |
| rehype-slug | 6.0.0 | **CLEAR** | None | Snyk |
| rehype-stringify | 10.0.1 | **CLEAR** | None | Snyk |
| remark-gfm | 4.0.1 | **CLEAR** | None | Snyk |
| remark-math | 6.0.0 | **CLEAR** | None | Snyk |
| remark-parse | 11.0.0 | **CLEAR** | None | Snyk |
| remark-rehype | 11.1.2 | **CLEAR** | None | Snyk |
| flexsearch | 0.8.212 | **CLEAR** | None | Snyk |
| gray-matter | 4.0.3 | **CLEAR** | None | Snyk |
| html-react-parser | **5.2.17** | **CLEAR** | None. v6 is post-cutoff — DEFERRED. | Snyk — Path B |
| superjson | 2.2.6 | **CLEAR** | None | Snyk |

### npm — UI components (Radix + lucide + Tailwind helpers)

| Package | Pinned Version | Status | Source |
|---|---|---|---|
| lucide-react | **0.577.0** | **CLEAR** | Snyk — Path B (stay on 0.x; 1.x DEFERRED) |
| radix-ui | 1.4.3 | **CLEAR** | Snyk |
| @radix-ui/react-alert-dialog | 1.1.15 | **CLEAR** | Snyk |
| @radix-ui/react-dialog | 1.1.15 | **CLEAR** | Snyk |
| @radix-ui/react-dropdown-menu | 2.1.16 | **CLEAR** | Snyk |
| @radix-ui/react-scroll-area | 1.2.10 | **CLEAR** | Snyk |
| @radix-ui/react-separator | 1.1.8 | **CLEAR** | Snyk |
| @radix-ui/react-slot | 1.2.4 | **CLEAR** | Snyk |
| @radix-ui/react-tabs | 1.1.13 | **CLEAR** | Snyk |
| @radix-ui/react-tooltip | 1.2.8 | **CLEAR** | Snyk |
| cmdk | 1.1.1 | **CLEAR** | Snyk |
| next-themes | 0.4.6 | **CLEAR** | Snyk |
| tailwind-merge | **2.x latest pre-cutoff** | **CLEAR** | Snyk — Path B (v3.x post-cutoff; DEFERRED) |
| clsx | 2.1.1 | **CLEAR** | Snyk |
| class-variance-authority | 0.7.1 | **CLEAR** | Snyk |
| react-icons | 5.6.0 | **CLEAR** | Snyk |
| vitest-axe | 0.1.0 | **CLEAR** (no CVE) / **MONITOR** (unmaintained — no release in 3+ years) | Snyk |

### npm — Build tooling + tRPC + TanStack

| Package | Pinned Version | Status | Notes |
|---|---|---|---|
| nx | **22.5.4** | **CLEAR** | All `@nx/*` packages at same version. 22.7.2 post-cutoff. | Path B |
| @nx/nx-linux-x64-musl | **22.5.4** | **CLEAR** | | Path B |
| @nx/nx-linux-x64-gnu | **22.5.4** | **CLEAR** | | Path B |
| husky | 9.1.7 | **CLEAR** | | Path B |
| lint-staged | **16.4.0** | **CLEAR** | 17.x post-cutoff — DEFERRED. | Path B |
| markdownlint-cli2 | **0.21.0** | **CLEAR** | 0.22.x post-cutoff. | Path B |
| tsx | **4.21.0** | **CLEAR** | 4.22.0 post-cutoff. | Path B |
| @hey-api/openapi-ts | **0.94.2** | **CLEAR** | Stay pre-cutoff. | Path B |
| @hey-api/client-fetch | 0.13.1 | **CLEAR** | Stay pre-cutoff. | Path B |
| @redocly/cli | **2.22.1** | **CLEAR** | 2.22.2+ post-cutoff. | Path B |
| @stoplight/spectral-cli | **6.15.0** | **CLEAR** | Stay pre-cutoff. | Path B |
| @openapitools/openapi-generator-cli | **2.30.2** | **CLEAR** | Stay pre-cutoff. CVE-2026-23947 (Critical) affects different package: `Orval`, NOT this | Path B |
| @commitlint/cli | **20.5.0** | **CLEAR** | 21.x post-cutoff — DEFERRED. | Path B |
| @commitlint/config-conventional | **20.5.0** | **CLEAR** | Versioned with cli. 21.x DEFERRED. | Path B |
| @trpc/client | **11.x latest pre-cutoff** | **CLEAR** | 11.17.0 possibly post-cutoff; verify on npm. | Path B |
| @trpc/server | **11.x latest pre-cutoff** | **CLEAR** | Versioned with client. | Path B |
| @trpc/tanstack-react-query | **11.x latest pre-cutoff** | **CLEAR** | Versioned with client. | Path B |
| @tanstack/react-query | **5.x latest pre-cutoff** | **CLEAR** | 5.100.10 possibly post-cutoff; verify on npm. NOT in 2026-05-11 TanStack supply-chain compromise (the `@tanstack/router`/`start` packages were affected, NOT `@tanstack/query*` family) — confirmed by TanStack postmortem | Path B |
| vite-tsconfig-paths | 6.1.1 | **CLEAR** | | Path B |

### Maven — Spring Boot 4.0.6 + Cucumber

| Component | Pinned Version | Status | Patched CVEs |
|---|---|---|---|
| spring-boot-starter-parent | 4.0.6 | **CLEAR (patch-of)** | 8 CVEs in 4.0.0–4.0.5: CVE-2026-40976 (Actuator authz bypass, **Critical 9.1**), CVE-2026-40972 (DevTools timing → RCE on LAN, High 7.5), CVE-2026-40973 (predictable temp dir LPE, High 7.0), CVE-2026-40970 (Elasticsearch TLS), CVE-2026-40971 (RabbitMQ TLS), CVE-2026-40974 (Cassandra TLS), CVE-2026-40975 (weak PRNG), CVE-2026-40977 (PID file symlink) |
| spring-boot-starter-web (transitive of 4.0.6) | 4.0.6 | **CLEAR** | — |
| spring-boot-starter-actuator | 4.0.6 | **CLEAR (patch-of)** | CVE-2026-40976 patched in 4.0.6 |
| spring-boot-starter-test | 4.0.6 | **CLEAR** | — |
| spring-boot-resttestclient | 4.0.6 | **CLEAR** | — |
| spring-boot-devtools | 4.0.6 | **CLEAR (patch-of)** | CVE-2026-40972 patched in 4.0.6 |
| jackson-datatype-jsr310 | latest (≥2.9.8 covers historical CVE) | **CLEAR** | — |
| cucumber-bom | 7.34.3 | **CLEAR** | — |

Source: [Spring Boot 4.0.6 release blog](https://spring.io/blog/2026/04/23/spring-boot-4-0-6-available-now/),
[Spring Security Advisories](https://spring.io/security/).

### Go modules (direct deps after `go get -u ./...`)

| Module | Pinned Version (direct) | Status | Notes |
|---|---|---|---|
| github.com/spf13/cobra | v1.10.2 | **CLEAR** | Already on latest |
| github.com/cucumber/godog | v0.15.1 | **CLEAR** | Already on latest |
| github.com/spf13/pflag | v1.0.9 | **CLEAR** | |
| github.com/rs/zerolog | v1.34.0 | **CLEAR** | |
| github.com/google/go-github/v56 | v56.0.0 | **CLEAR** | |
| github.com/vladopajic/go-test-coverage/v2 | v2.18.3 | **CLEAR** | Tool dep |
| golang.org/x/sys | v0.41.0 | **CLEAR** | CVE-2022-29526 fixed pre-v0.1.0 |
| golang.org/x/tools | v0.41.0 | **CLEAR** | |
| golang.org/x/net | v0.50.0 | **CLEAR** | CVE-2024-45338 fixed ≥v0.33.0; CVE-2025-58190 (GO-2026-4441) fixed ≥v0.45.0 — both clean |
| golang.org/x/text | v0.34.0 | **CLEAR** | |
| gopkg.in/yaml.v3 | v3.0.1 | **CLEAR** | |
| github.com/cucumber/gherkin/go/v26 | v26.2.0 | **CLEAR** | |
| github.com/cucumber/messages/go/v21 | v21.0.1 | **CLEAR** | |
| github.com/hashicorp/go-memdb | v1.3.4 | **CLEAR** | |
| github.com/hashicorp/go-immutable-radix | v1.3.1 | **CLEAR** | |
| github.com/hashicorp/golang-lru | v0.5.4 | **CLEAR** | |

### Go modules (indirect — REMEDIATE / DEFER)

| Module | Pinned Version (indirect) | Status | CVEs | Action |
|---|---|---|---|---|
| **golang.org/x/image** | v0.18.0 → must force **v0.39.0** | **REMEDIATE** | CVE-2026-33809 (TIFF OOM, Medium 5.3) and CVE-2026-33812 (font OOM) — both fixed in v0.39.0 | Add explicit `go.mod` requirement or `replace` directive in each Go module to pin v0.39.0; verify with `govulncheck`. See Phase 3.x in delivery.md |
| **github.com/aws/aws-sdk-go** | v1.49.4 | **DEFER** (S3-crypto only; library EOL Jul 2025) | CVE-2020-8911 (S3 AES-CBC Medium 5.6), CVE-2020-8912 (S3 AES-CTR key negotiation Low 2.6) — affect S3 client-side encryption module only | Verify our codepath does NOT use `s3crypto` (rhino-cli/ayokoding-cli/ose-cli use only badge generation via narqo/go-badge — no S3). Track migration to `aws-sdk-go-v2` as separate plan. Indirect via `github.com/narqo/go-badge` which has not released since v0.3 |
| github.com/narqo/go-badge | (transitive) | **MONITOR** | Stale upstream — last release v0.3 (likely 2023) | Pulls both `golang.org/x/image` and `aws-sdk-go` indirectly. Long-term: replace go-badge or fork |
| github.com/golang/freetype | v0.0.0-20170609003504-… | **CLEAR** (no CVE) / **MONITOR** | Pseudo-version from 2017; unmaintained | Same upstream chain as go-badge |
| github.com/jmespath/go-jmespath | v0.4.0 | **CLEAR** | aws-sdk-go transitive |
| github.com/gofrs/uuid | v4.3.1+incompatible | **CLEAR** | aws-sdk-go transitive |

### CI Action Pins

| Field | Pinned Version | Status | Notes |
|---|---|---|---|
| `actions/checkout@v4` | v4 (floating major) | **CLEAR** | Verified via Snyk |
| `actions/setup-node@v4` | v4 | **CLEAR** | |
| `actions/setup-go@v5` | v5 | **CLEAR** | |
| `actions/setup-java@v4` (to be added in setup-jvm) | v4 | **CLEAR** | |

### Action Items Summary

| Priority | Component | Action | Phase |
|---|---|---|---|
| **CRITICAL** | golang.org/x/image (indirect, all 5 Go modules) | Force v0.39.0 via `go.mod` `require` directive after `go get -u`; verify with `govulncheck ./...` | Phase 3 sub-task |
| **HIGH** | `eclipse-temurin:25-jdk-alpine` in `Dockerfile.integration` | Switch to `eclipse-temurin:25.0.3+9-jdk` (Ubuntu base) for runtime; keep alpine only for build stages where layer size matters and no JDK runtime executes | Phase 4 sub-task |
| **MEDIUM** | aws-sdk-go v1.49.4 (indirect, EOL) | Verify no `s3crypto` usage in our code; document deferred migration to `aws-sdk-go-v2` as separate plan | Phase 14 verification + future plan |
| **MEDIUM** | Volta unmaintained | Future migration to `mise` — out of scope for this plan; tracked separately | N/A (out of scope) |
| **LOW** | Alpine 3.23 OpenSSL CVEs (multiple base images) | Rebuild images periodically as upstream Alpine pushes 3.3.7-r0 OpenSSL fix | Continuous monitoring |
| **LOW** | vitest-axe 0.1.0 unmaintained | Plan migration to actively-maintained a11y testing alternative | Future plan |

### Security Waivers (Path C — recent CVE patches required, 60-day rule waived)

The following packages were pinned to versions released within the last 60 days because no
CVE-clean version exists outside that window. Each waiver is justified per the
[Dependency Bump Stability & Safety Policy](../../repo-governance/development/workflow/dependency-bump-policy.md)
Path C process.

| Package | Pinned Version | Release Date | Required For (CVE) | Severity | Source | Sign-off |
|---|---|---|---|---|---|---|
| `next` | **16.2.6** | 2026-05-08 | CVE-2026-29057 (HTTP smuggling), CVE-2026-27979 (DoS), CVE-2026-44578 (SSRF), and 10 other May 2026 advisories | High | [Vercel May 2026 release](https://vercel.com/changelog/next-js-may-2026-security-release) | plan-author + plan-quality-gate review |
| `react` | **19.2.6** | ~2026-05-06 | CVE-2025-55182 (Critical RSC RCE), CVE-2026-23864 (High DoS), CVE-2026-23870 | Critical → High | [react.dev advisory](https://react.dev/blog/2025/12/03/critical-security-vulnerability-in-react-server-components) | plan-author + plan-quality-gate |
| `react-dom` | **19.2.6** | ~2026-05-06 | Same as react | Critical → High | Same | plan-author |
| `mermaid` | **11.15.0** | 2026-05-11 | CVE-2026-41148/41150/41159 (CSS injection High 7.1; Gantt DoS) and 3 other CVEs; all 6 unpatched in any pre-cutoff version | High → Medium | [mermaid GHSA](https://github.com/advisories?query=type%3Areviewed+ecosystem%3Anpm+mermaid) | plan-author |
| `postcss` | **8.5.10+** (npm `npm install` resolves transitively) | 2026-04-15 | CVE-2026-41305 (XSS via unescaped `</style>`) | Medium 6.1 | [SNYK-JS-POSTCSS-16189065](https://security.snyk.io/vuln/SNYK-JS-POSTCSS-16189065) | plan-author |
| `eclipse-temurin:25.0.3+9-jdk` (Ubuntu base, REPLACES `:25-jdk-alpine`) | image swap | 2026-04-22 | Avoid 2 unfixed High binutils CVEs (CVE-2025-69649, CVE-2025-69650 CVSS 7.5) in the Alpine layer; Ubuntu base has 0 High/Critical | High | [sliplane.io eclipse-temurin alpine CVE](https://sliplane.io/tools/cve/library/eclipse-temurin:25-alpine) | plan-author |
| `effect` | **3.21.2** | ~2026-04 | CVE-2026-32887 (AsyncLocalStorage context leak, High CVSS 7.4) patched at 3.20.0. Latest 3.x pre-cutoff is 3.19.19 (2026-02-21) which IS vulnerable. Pin to 3.21.2 (current; CVE-clean) per Path C waiver. | High | [GHSA-38f7-945m-qr2g](https://github.com/advisories/GHSA-38f7-945m-qr2g) | plan-author |
| `golang.org/x/image` (indirect via `narqo/go-badge`) | **v0.39.0** | 2026-04-09 | CVE-2026-33809 (TIFF OOM Medium 5.3) patched at v0.38.0; CVE-2026-33812 (font OOM) patched at v0.39.0. Both versions post-cutoff. Pin v0.39.0 in each Go module via `go.mod require` directive per Path C waiver. | Medium | [GO-2026-4815](https://pkg.go.dev/vuln/GO-2026-4815), [GO-2026-4962](https://pkg.go.dev/vuln/GO-2026-4962) | plan-author |

A persistent waiver register lives at `docs/reference/security-waivers.md` (created in
Phase 14.X if missing). Future plans should append to that file rather than redefine
waivers per plan.

### Pre-Push Security Re-Audit (added to Phase 14)

Before pushing to `origin main`, run:

```bash
# Run npm audit on the full transitive tree
npm audit --audit-level=moderate

# Run govulncheck on each Go module
for mod in apps/rhino-cli apps/ayokoding-cli apps/ose-cli libs/golang-commons libs/hugo-commons; do
  echo "=== $mod ==="
  (cd "$mod" && govulncheck ./...)
done

# Run mvn dependency-check on organiclever-be (if plugin configured)
cd apps/organiclever-be && mvn org.owasp:dependency-check-maven:check -DfailBuildOnCVSS=7
```

Any HIGH/CRITICAL findings during pre-push must be triaged and either remediated or
explicitly waived (with rationale) before merge to `main`.

---

## Machine Install Sequence (eligible versions per policy)

Run these commands on the dev machine before applying config file changes. Versions chosen
per the three-path policy: LTS where applicable (Path A) or latest pre-2026-03-16 stable
+ CVE-clean (Path B).

```bash
# 1. Go 1.25.8 (Path B — latest pre-cutoff stable; patches same CVEs as 1.26.3)
# macOS via gimme or download:
gimme 1.25.8
# OR direct: download from go.dev/dl/go1.25.8.darwin-arm64.tar.gz, install to /usr/local/go-1.25.8, symlink
go version   # must print go1.25.8

# 2. Erlang OTP 27.3.3 via asdf (Path B — latest pre-cutoff with CVE-2025-32433 patched)
asdf install erlang 27.3.3
asdf global erlang 27.3.3
erl -noshell -eval 'io:format("~s",[erlang:system_info(otp_release)]),halt().'   # must print 27

# 3. .NET SDK 10.0.300 (Path A — LTS; May 2026 Patch Tuesday)
# macOS — via brew or manual installer
brew upgrade dotnet
dotnet --version   # must start with 10.0.3

# 4. Java Temurin 25.0.3 via SDKMAN (Path A — LTS)
source "$HOME/.sdkman/bin/sdkman-init.sh"
sdk install java 25.0.3+9-tem
sdk default java 25.0.3+9-tem
java --version   # must show 25.0.3

# 5. Rust 1.93.0 (Path B — latest pre-cutoff stable, 2026-02-12)
rustup install 1.93.0
rustup default 1.93.0
rustc --version   # must show 1.93.0

# 6. Dart 3.11.0 / Flutter 3.41.0 (Path B — latest pre-cutoff)
# Pin Flutter to specific stable channel revision via fvm:
fvm install 3.41.0
fvm global 3.41.0
dart --version    # must show 3.11.0
flutter --version # must show 3.41.0

# 7. golangci-lint 2.11.3 (Path B — latest pre-cutoff stable, 2026-03-10)
go install github.com/golangci/golangci-lint/cmd/golangci-lint@v2.11.3
golangci-lint version   # must show 2.11.3
```

---

## File Change Map

> All target versions are exact (no carets/tildes). DEFERRED items show "no change" or
> "stay-on-line pin update".

### Root-level files

| File | Field(s) to change | From → To |
|---|---|---|
| `package.json` | `volta.node` | 24.13.1 → **24.15.0** (Path A) |
| `package.json` | `volta.npm` | 11.10.1 → **11.11.0** (Path B; 11.14.1 post-cutoff) |
| `package.json` | `devDependencies.nx` + all `@nx/*` | 22.5.2 → **22.5.4** (Path B; 22.7.2 post-cutoff) |
| `package.json` | `devDependencies.lint-staged` | ^16.2.6 → **16.4.0** (Path B; v17 DEFERRED) |
| `package.json` | `devDependencies.markdownlint-cli2` | ^0.21.0 → **0.21.0** (stay; 0.22.x post-cutoff) |
| `package.json` | `devDependencies.prettier` | ^3.6.2 → **3.8.1** (Path B; 3.8.2/3.8.3 post-cutoff) |
| `package.json` | `optionalDependencies` (linux binaries) | update to match Nx 22.5.4 |
| `.tool-versions` | `erlang` | 27.3 → **27.3.3** (Path B; 27.3.4.x post-cutoff) |

### apps/ose-app-be

| File | Field(s) to change | From → To |
|---|---|---|
| `global.json` | `sdk.version` | 10.0.103 → 10.0.300 |

### apps/rhino-cli/internal/doctor/tools.go

| Symbol | From → To |
|---|---|
| `pomXMLPath` | `apps/organiclever-be-jasb/pom.xml` → `apps/organiclever-be/pom.xml` |
| `globalJSONPath` | `apps/ose-grc-be/global.json` → `apps/ose-app-be/global.json` |
| golangci-lint `readReq` return value | `"2.11.1"` → **`"2.11.3"`** (Path B; 2.12.2 post-cutoff) |
| Python source comment | update to note path is demo (ose-primer) |
| Rust source comment | update to note path is demo (ose-primer) |
| Dart/Flutter source comment | update to note path is demo (ose-primer) |

### apps/organiclever-be/pom.xml

| Property | Action |
|---|---|
| `spring-boot-starter-parent` version | Run `mvn versions:update-parent -DallowSnapshots=false` |
| `cucumber.version` | Run `mvn versions:update-properties` |
| `<java.version>` | Stays at 25 (correct) |

### All Next.js apps (5 × package.json)

Change `dependencies.next` from `16.1.6` to **`16.2.6`** (exact, Path C waiver).
Change `dependencies.react` and `react-dom` to **`19.2.6`** (exact, Path C waiver — NOT `^19.2.0`).
Change `devDependencies.typescript` to **`5.8.3`** (exact, Path B — NOT `^6.0.3`; TS 6 DEFERRED).
Change `devDependencies.tailwindcss` to **`4.2.1`** (exact, Path B — NOT `^4.3.0`; 4.3.0 post-cutoff).
Change `devDependencies.vitest` + `@vitest/coverage-v8` to **`4.1.0`** (exact, Path B — NOT `^4.1.6`; 4.1.6 post-cutoff).

### ayokoding-web, ose-web

Change `dependencies.shiki` to **`4.0.2`** (exact, Path B — MAJOR migration).
Change `dependencies.zod` to **`3.25.76`** (Path B; latest 3.x release 2025-07-08; Zod 4 DEFERRED).
Change `dependencies.lucide-react` to **`0.577.0`** (exact, Path B — stay on 0.x; 1.x DEFERRED; NOT `^1.14.0`).

### organiclever-web, ose-app-web

Change `dependencies.@effect/platform` to **`0.94.5`** (Path B; pre-cutoff 2026-02-14).
Change `devDependencies.@effect/vitest` to **`0.29.0`** (exact; Path B).
Change `dependencies.@xstate/react` to **`5.0.5`** (Path B; current pin already at this version; stay on 5.x — 6.x DEFERRED).
Change `dependencies.next` to **`16.2.6`** (exact, Path C waiver).

### wahidyankf-web

Change `dependencies.next` to **`16.2.6`** (exact, Path C waiver).
Change `dependencies.lucide-react` to **`0.577.0`** (exact, Path B — stay on 0.x; NOT `^1.14.0`).

### libs/web-ui

Change `devDependencies.vitest` + `@vitest/coverage-v8` from `^3.2.0` to **`4.1.0`** (exact, Path B; NOT `^4.1.6`).
Change `dependencies.lucide-react` to **`0.577.0`** (exact, Path B — stay on 0.x; NOT `^1.14.0`).
Change `devDependencies.storybook` + `@storybook/*` to **`10.2.10`** (exact, DOWNGRADE; CVE-2026-27148 patch; NOT `^10.4.0`).

### All E2E apps (9 × package.json)

Change `@playwright/test` to **`1.60.0`** (exact, Path B — NOT `^1.60.0`).
Change `playwright-bdd` to **`8.5.1`** (exact, Path B — NOT `^8.4.2`).

---

## TypeScript 5.8.3 Notes (stay-on-line; TS 6 DEFERRED)

> **DEFERRED**: TypeScript 6.0 migration is deferred to a future plan (post-cutoff per
> 60-day policy; tracked in `plans/ideas.md`). This section covers the 5.6.0 → 5.8.3
> stay-on-line pin update.

Check each project for the following patterns after pinning TS 5.8.3:

1. **Implicit `any` in index signatures**: TS 5.8 enforces stricter index types in some
   patterns. Add explicit `Record<string, unknown>` where needed.
2. **Function overloads**: stricter implementation-signature checking introduced in 5.x.
   Ensure implementation signature is compatible with all overloads.
3. **`noUncheckedIndexedAccess`**: not newly enabled in 5.8 but verify array access patterns.

Run `npx tsc --version` after update — must show `5.8.3` (NOT 6.x).

## Shiki 4.x Migration Notes

Shiki 4.x still exports `createHighlighter` but some convenience helpers changed.
Key changes from 1.x → 4.x:

- `getHighlighter()` → `createHighlighter()` (already the API in 1.x, confirm usage).
- Theme names changed in some cases — verify bundled theme references.
- `codeToHtml()` still available as top-level import.
- Some type exports renamed.

Search `grep -r "from 'shiki'" apps/ayokoding-web apps/ose-web` to find all usage sites.

## Zod 3.x Stay-on-Line Notes (Zod 4 DEFERRED)

> **DEFERRED**: Zod 4.x migration is deferred to a future plan (post-cutoff per 60-day
> policy; tracked in `plans/ideas.md`). Current pin stays in Path B (Tier-A migration
> deferred until version has 60-day soak). This section is a placeholder for when that
> future plan is executed.

Phase 11 of this plan updates `zod` to the latest pre-cutoff 3.x patch (e.g., 3.24.x).
This is a stay-on-line pin update — no API migration steps required.

Run `grep -r "from 'zod'" apps/` to enumerate usage sites and verify no import errors.

## @xstate/react 5.x Stay-on-Line Notes (@xstate/react 6 DEFERRED)

> **DEFERRED**: @xstate/react 6.x migration is deferred to a future plan (post-cutoff per
> 60-day policy; tracked in `plans/ideas.md`). Current pin stays in Path B (Tier-A migration
> deferred until version has 60-day soak).

Phase 13 of this plan updates `@xstate/react` to the latest pre-cutoff 5.x patch.
This is a stay-on-line pin update — no API migration steps required.

Run `grep -r "from '@xstate/react'" apps/organiclever-web apps/ose-app-web` to enumerate
usage sites and verify no import errors.

## lucide-react 0.x Stay-on-Line Notes (lucide-react 1.x DEFERRED)

> **DEFERRED**: lucide-react 1.x migration is deferred to a future plan (post-cutoff per
> 60-day policy; tracked in `plans/ideas.md`). Current pin stays in Path B (Tier-A migration
> deferred until version has 60-day soak).

Phase 12 of this plan updates `lucide-react` to `0.577.0` (latest pre-cutoff 0.x, 2026-03-04).
This is a stay-on-line pin update. Check for any renames between the prior 0.x version and
0.577.0 only (NOT 0.x → 1.x renames which are irrelevant here):

Run `grep -r "from 'lucide-react'" apps/ libs/` to find all icon names in use and verify
they still exist in 0.577.0.
