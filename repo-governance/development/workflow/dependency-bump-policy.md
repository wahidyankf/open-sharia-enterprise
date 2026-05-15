---
title: "Dependency Bump Stability & Safety Policy"
description: Three-path decision tree (LTS, 60-day soak, security waiver) governing every dependency bump across the polyglot monorepo — npm, Go, Maven, .NET, Dockerfile, GitHub Actions
category: explanation
subcategory: development
tags:
  - dependencies
  - security
  - versioning
  - reproducibility
  - workflow
created: 2026-05-15
---

# Dependency Bump Stability & Safety Policy

Every dependency bump MUST satisfy three constraints before it is merged: (1) reproducibility via exact pinning, (2) stability via LTS-first or 60-day soak, and (3) security via CVE clearance. This rule prevents shipping fresh versions whose breakage profile is undiscovered while ensuring known vulnerabilities are patched.

## Principles Implemented/Respected

This practice implements/respects the following core principles:

- **[Reproducibility First](../../principles/software-engineering/reproducibility.md)**: All version specifications use exact pins — no caret, no tilde, no `latest`. Lockfiles are the sole source of truth for the resolved graph. Deterministic installs on every machine and CI runner.

- **[Explicit Over Implicit](../../principles/software-engineering/explicit-over-implicit.md)**: Every version is stated explicitly in manifests. Path classification (LTS / 60-day / waiver) is documented in writing. Cutoff dates are computed and recorded. No version is ever implicitly "latest".

- **[Automation Over Manual](../../principles/software-engineering/automation-over-manual.md)**: Verification commands (`grep`, `npm audit`, `govulncheck`, lockfile update commands) are prescribed steps in the application workflow so that correctness checks run mechanically rather than relying on reviewer memory.

- **[Root Cause Orientation](../../principles/general/root-cause-orientation.md)**: When a package has a CVE, the fix is to upgrade to the patched version (root cause resolved) rather than suppressing the audit warning or adding an exception comment.

- **[Deliberate Problem-Solving](../../principles/general/deliberate-problem-solving.md)**: The three-path decision tree forces explicit classification before a version is chosen. Engineers and agents state their reasoning in writing rather than picking the newest available tag reflexively.

## Conventions Implemented/Respected

This practice respects the following conventions:

- **[Reproducible Environments Convention](./reproducible-environments.md)**: Exact version pinning in `package.json`, Volta block, `go.mod`, `global.json`, `.tool-versions`, and Dockerfiles directly implements the reproducibility standards established there.

- **[Commit Message Convention](./commit-messages.md)**: Dependency bump commits use `chore(deps): bump <package> to <version>` or `fix(deps): patch CVE-YYYY-NNNNN in <package>` per Conventional Commits format.

## Scope

### What This Policy Covers

- All `package.json` `dependencies`, `devDependencies`, `peerDependencies`, `optionalDependencies` (npm)
- All `go.mod` `require` directives (Go)
- All `pom.xml` `<version>` properties and `<parent><version>` (Maven / Java / Spring Boot)
- All `global.json` `sdk.version` (.NET)
- All `.tool-versions` entries (Erlang, Elixir, and other asdf-managed runtimes)
- All `package.json` `volta` block (Node.js, npm)
- All `Dockerfile` `FROM` lines (base images)
- All GitHub Actions `uses:` references and inline version pins (CI workflow files)
- All composite-action input defaults (`.github/actions/*/action.yml`)

### What This Policy Does NOT Cover

- Workspace-internal `*` references (`@open-sharia-enterprise/web-ui: "*"` etc.) — these resolve via npm workspaces to local paths, not the registry
- Lockfiles (`package-lock.json`, `go.sum`, etc.) — managed by tooling after manifest changes
- Type-only dev deps where the security surface is provably zero (exact pinning still recommended, but lower enforcement priority)

## Three-Path Decision Tree

For every version bump, classify the package and apply the corresponding path.

### Path A — LTS Path (use latest LTS-line patch)

If the package or runtime has an officially designated LTS line, **use the latest LTS patch** regardless of recency, provided it is CVE-clean.

LTS-track packages and runtimes (non-exhaustive examples):

- Node.js (LTS lines: 22 "Jod", 24 "Krypton", etc.)
- Java / Eclipse Temurin (LTS releases: 8, 11, 17, 21, 25)
- .NET (even-numbered major versions: 6, 8, 10 are LTS)
- PostgreSQL (5-year support model — every major is effectively LTS)
- Spring Boot (3-year OSS support — current minor is LTS-equivalent)
- Erlang/OTP (each major has 2-year support — current major is LTS-equivalent)
- React (de facto LTS treatment for major versions)

Rationale: LTS lines have a soak and curation process built in by the upstream maintainer. Recent LTS patches inherit that soak.

### Path B — 60-Day Stable + CVE-Clean Path

If the package has no LTS designation, **use the latest version that satisfies BOTH**:

1. Released **at least 60 days** before the bump date (release date ≤ today − 60 days)
2. CVE-clean — zero known unpatched CVEs per NVD, GitHub Security Advisories, Snyk DB, and the project's own security page

Examples of non-LTS packages (most JavaScript libraries, Go, Rust, TypeScript, Tailwind, Vitest, Storybook, ESLint, Playwright, lucide-react, Zod, Shiki, mermaid, etc.).

Rationale: 60 days is the minimum soak window for the community to surface regression bugs and security issues. Most non-LTS upstreams cut patch releases monthly; 60 days catches the next-cycle fixes before the version is adopted here.

### Path C — Security-Override Waiver

When **no version satisfies BOTH the 60-day rule AND CVE-cleanness**, use the most recent CVE-patched version (or the security-recommended LTS) and document a waiver.

The waiver MUST include:

- Package name and version pinned
- The CVE(s) requiring the recent version (with NVD or GHSA URL)
- The CVE severity (Critical / High / Medium / Low)
- The release date of the pinned version
- Brief justification (e.g., "Critical RCE; no older patched version exists")
- Sign-off identity (the engineer or AI agent applying the waiver)

Waivers are documented in the plan that introduces the bump (in `tech-docs.md` under a "Security Waivers" subsection) and propagated to a long-lived `docs/reference/security-waivers.md` file (create if missing).

## Pinning Policy (Hard Rule)

All version specifications MUST be exact strings. No caret (`^`), no tilde (`~`), no `latest`, no `*` (except npm workspace-internal references).

| Manifest                                                | Required Form                                      | Example                                                                |
| ------------------------------------------------------- | -------------------------------------------------- | ---------------------------------------------------------------------- |
| `package.json` deps / devDeps                           | Exact string                                       | `"react": "19.2.6"` (NOT `"^19.2.6"`)                                  |
| `package.json` `volta` block                            | Exact (Volta enforces this)                        | `"node": "24.15.0"`                                                    |
| `package.json` `optionalDependencies` (native binaries) | Exact                                              | `"@next/swc-linux-x64-gnu": "16.2.6"`                                  |
| `go.mod` `require`                                      | Exact pseudo-version (Go enforces this)            | `golang.org/x/image v0.39.0`                                           |
| `pom.xml` `<parent><version>` and `<*.version>`         | Exact                                              | `<spring-boot.version>4.0.6</spring-boot.version>`                     |
| `global.json` `sdk.version`                             | Exact (`rollForward` allowed per upstream pattern) | `"version": "10.0.300"`                                                |
| `.tool-versions`                                        | Exact                                              | `erlang 27.3.4.11` (NOT `erlang 27.3`)                                 |
| Dockerfile `FROM`                                       | Exact tag (digest preferred for production)        | `FROM node:24.15.0-alpine3.23` (NOT `FROM node:24-alpine`)             |
| GitHub Actions `uses:`                                  | Pinned major OR exact SHA                          | `uses: actions/setup-node@v4` (acceptable for first-party) or `@<sha>` |
| Composite action input defaults                         | Exact                                              | `default: "1.26.3"` (NOT `default: "1.26"`)                            |

**Verification command** after every `package.json` edit:

```bash
grep -E '"\^|"~' <changed-file> && echo "FAIL: caret/tilde found" || echo "OK: all exact"
```

## CVE Clearance Process (Mandatory for Every Bump)

For every version selected (Path A, B, or C), verify CVE status against all four sources:

1. **NVD** ([nvd.nist.gov](https://nvd.nist.gov)) — National Vulnerability Database
2. **GitHub Security Advisories** ([github.com/advisories](https://github.com/advisories))
3. **Snyk DB** ([security.snyk.io](https://security.snyk.io))
4. **Project security page** (vendor-specific: `nodejs.org/en/blog/vulnerability`, `spring.io/security`, `pkg.go.dev/vuln`, dotnet release notes, etc.)

The result for every package MUST be one of:

- **CLEAR** — No known CVEs as of the bump date
- **CLEAR (patch-of)** — Pinned version IS the patched release for one or more known CVEs (document the CVE IDs)
- **WAIVER** — Path C applied (document waiver per the template above)

Audit findings go into the plan's `tech-docs.md` Security Clearance Status table, or for ad-hoc bumps outside a plan, into the PR description.

## Cutoff Date Computation

For every bump, the policy author MUST state the cutoff date in writing:

```
Today: <YYYY-MM-DD>
Cutoff: today − 60 days = <YYYY-MM-DD>
Eligible (Path B): versions released on or before <cutoff>
```

This ensures auditability when CVE or release dates are revisited.

## When the Plan Spans Many Days

If a plan with dependency bumps takes more than 60 days to merge, the cutoff drifts forward. Re-run the eligibility check before the final merge to catch newly-eligible versions or newly-disclosed CVEs.

## Examples

### Example 1: Path A (LTS, Recent Patch)

Node.js: `package.json` `volta.node` = `"24.15.0"`. Released 2026-04-15 (30 days before the bump on 2026-05-15). LTS Krypton confirmed. CVE-clean per NVD. **Decision: keep at 24.15.0** — LTS path overrides the 60-day rule.

### Example 2: Path B (Non-LTS, Eligible Older Version)

Tailwind CSS: latest is 4.3.0 (released 2026-05-08, 7 days old). Cutoff = 2026-03-16. Latest version released on or before 2026-03-16 is 4.2.1 (released 2026-02-23). 4.2.1 is CVE-clean. **Decision: bump to 4.2.1** — skip 4.3.0; it is not eligible until 60 days have elapsed.

### Example 3: Path C (Waiver)

mermaid: latest is 11.15.0 (released 2026-05-11, 4 days old). All versions below 11.15.0 have unpatched CVE-2026-41148 (CSS injection, High 7.1) and five related CVEs. No pre-cutoff CVE-clean version exists. **Decision: waiver — pin to 11.15.0.** Justification: required for active CVE patches; 60-day rule waived per Path C.

## Application Workflow

When proposing or executing a dependency bump, follow these steps in order:

1. List every package, runtime, and base image to be bumped
2. For each item: classify as Path A, B, or C
3. For Path A: identify the latest LTS patch and verify CVE clearance
4. For Path B: identify the latest version released on or before the cutoff and verify CVE clearance
5. For Path C: document the waiver per the template above
6. Convert all version specs to exact pins (remove carets and tildes)
7. Run lockfile updates: `npm install`, `go mod tidy`, `mvn versions:resolve-ranges`
8. Run security re-audit: `npm audit --audit-level=moderate`, `govulncheck ./...`, optionally `mvn org.owasp:dependency-check-maven:check`
9. Document the audit results and any waivers in the plan's `tech-docs.md`
10. Run quality gates for affected projects: typecheck, lint, test:quick, spec-coverage

## Tools and Automation

- **repo-rules-checker** — validates that any plan introducing dependency bumps includes a Security Clearance Status section and applies the three-path decision tree
- `npm audit --audit-level=moderate` — mandatory post-update security scan for npm packages
- `govulncheck ./...` — mandatory post-update security scan for Go modules
- `grep -E '"\^|"~'` — pin verification after any `package.json` edit
- Renovate / Dependabot — if configured, surface bump PRs but require human application of the three-path classification before merge

## References

**Related Development Practices:**

- [Reproducible Environments Convention](./reproducible-environments.md) — Runtime pinning and lockfile discipline that this policy extends
- [Trunk Based Development Convention](./trunk-based-development.md) — Bumps follow the same direct-to-main publish path
- [Native-First Toolchain Management](./native-first-toolchain.md) — Toolchain version management via `rhino-cli doctor`
- [CI Blocker Resolution Convention](../quality/ci-blocker-resolution.md) — CVE-related CI failures are resolved per root-cause discipline, not suppressed

**Related Principles:**

- [Reproducibility First](../../principles/software-engineering/reproducibility.md) — Foundational why for exact pinning
- [Explicit Over Implicit](../../principles/software-engineering/explicit-over-implicit.md) — Foundational why for written path classification and cutoff dates
- [Root Cause Orientation](../../principles/general/root-cause-orientation.md) — Foundational why for CVE clearance rather than suppression

**External References:**

- [NVD](https://nvd.nist.gov) — National Vulnerability Database
- [GitHub Security Advisories](https://github.com/advisories) — GitHub advisory database
- [Snyk DB](https://security.snyk.io) — Snyk vulnerability database
- [govulncheck](https://pkg.go.dev/golang.org/x/vuln/cmd/govulncheck) — Go vulnerability scanner
- [npm audit](https://docs.npmjs.com/cli/v10/commands/npm-audit) — npm vulnerability scanner

**Long-Lived Registers:**

- `docs/reference/security-waivers.md` — Waiver register (create if missing when the first Path C waiver is issued)
- Introducing plan's `tech-docs.md` — Security Clearance Status table for each bump
