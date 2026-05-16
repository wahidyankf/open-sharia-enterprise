---
title: "Security Waivers"
description: Persistent register of Path C (security-override) dependency waivers granted across the workspace
category: reference
tags:
  - reference
  - security
  - dependency
  - waiver
  - cve
created: 2026-05-16
---

# Security Waivers

Persistent register of **Path C** (security-override) waivers granted under the [Dependency Bump Stability & Safety Policy](../../repo-governance/development/workflow/dependency-bump-policy.md). Path C applies when no CVE-clean version exists outside the policy's 60-day soak window — the team waives the soak requirement to pull in the security patch.

> **Append, do not redefine.** Future plans must append waivers to this register rather than re-declaring them in their own `tech-docs.md`. Each entry records the plan that introduced (or last revalidated) the waiver, the package, the pinned version, the CVE(s) being addressed, the severity, the citation, and the sign-off.

## Active Waivers

| Package                                                            | Pinned Version | Release Date | CVE(s)                                                                                                                              | Severity        | Citation                                                                                                           | Introduced By                                                             | Sign-off                               |
| ------------------------------------------------------------------ | -------------- | ------------ | ----------------------------------------------------------------------------------------------------------------------------------- | --------------- | ------------------------------------------------------------------------------------------------------------------ | ------------------------------------------------------------------------- | -------------------------------------- |
| `next`                                                             | **16.2.6**     | 2026-05-08   | CVE-2026-29057 (HTTP smuggling), CVE-2026-27979 (DoS), CVE-2026-44578 (SSRF), and 10 other May 2026 advisories                      | High            | [Vercel May 2026 release](https://vercel.com/changelog/next-js-may-2026-security-release)                          | [`stack-update` (2026-05-15)](../../plans/done/2026-05-15__stack-update/) | plan-author + plan-quality-gate review |
| `react`                                                            | **19.2.6**     | ~2026-05-06  | CVE-2025-55182 (Critical RSC RCE), CVE-2026-23864 (High DoS), CVE-2026-23870                                                        | Critical → High | [react.dev advisory](https://react.dev/blog/2025/12/03/critical-security-vulnerability-in-react-server-components) | [`stack-update` (2026-05-15)](../../plans/done/2026-05-15__stack-update/) | plan-author + plan-quality-gate        |
| `react-dom`                                                        | **19.2.6**     | ~2026-05-06  | Same as `react`                                                                                                                     | Critical → High | Same as `react`                                                                                                    | [`stack-update` (2026-05-15)](../../plans/done/2026-05-15__stack-update/) | plan-author                            |
| `effect`                                                           | **3.21.2**     | ~2026-04     | CVE-2026-32887 (AsyncLocalStorage context leak) — patched at 3.20.0. Latest 3.x pre-cutoff is 3.19.19 (2026-02-21, vulnerable).     | High (CVSS 7.4) | [GHSA-38f7-945m-qr2g](https://github.com/advisories/GHSA-38f7-945m-qr2g)                                           | [`stack-update` (2026-05-15)](../../plans/done/2026-05-15__stack-update/) | plan-author                            |
| `golang.org/x/image` (indirect via `narqo/go-badge`)               | **v0.39.0**    | 2026-04-09   | CVE-2026-33809 (TIFF OOM, patched at v0.38.0); CVE-2026-33812 (font OOM, patched at v0.39.0). Both versions post-cutoff.            | Medium          | [GO-2026-4815](https://pkg.go.dev/vuln/GO-2026-4815), [GO-2026-4962](https://pkg.go.dev/vuln/GO-2026-4962)         | [`stack-update` (2026-05-15)](../../plans/done/2026-05-15__stack-update/) | plan-author                            |
| `postcss` (transitive)                                             | **8.5.10+**    | 2026-04-15   | CVE-2026-41305 (XSS via unescaped `</style>`). No pre-cutoff version is CVE-clean.                                                  | Medium 6.1      | [SNYK-JS-POSTCSS-16189065](https://security.snyk.io/vuln/SNYK-JS-POSTCSS-16189065)                                 | [`stack-update` (2026-05-15)](../../plans/done/2026-05-15__stack-update/) | plan-author                            |
| `eclipse-temurin:25.0.3+9-jdk` (Ubuntu base; replaces alpine base) | image swap     | 2026-04-22   | Avoid 2 unfixed High binutils CVEs (CVE-2025-69649, CVE-2025-69650; CVSS 7.5) in the Alpine layer. Ubuntu base has 0 High/Critical. | High            | [sliplane.io eclipse-temurin alpine CVE](https://sliplane.io/tools/cve/library/eclipse-temurin:25-alpine)          | [`stack-update` (2026-05-15)](../../plans/done/2026-05-15__stack-update/) | plan-author                            |
| `mermaid`                                                          | **11.15.0**    | 2026-05-11   | CVE-2026-41148/41150/41159 (CSS injection High 7.1; Gantt DoS) plus 3 other CVEs; all 6 unpatched in any pre-cutoff version.        | High → Medium   | [mermaid GHSA](https://github.com/advisories?query=type%3Areviewed+ecosystem%3Anpm+mermaid)                        | [`stack-update` (2026-05-15)](../../plans/done/2026-05-15__stack-update/) | plan-author                            |

## Process

1. **Trigger** — A dependency bump's CVE clearance step finds no pre-cutoff CVE-clean version.
2. **Justify** — The introducing plan's `tech-docs.md` documents the waiver with the CVE list, severity, and citation.
3. **Sign-off** — `plan-author` records the waiver here; `plan-quality-gate` reviews during the quality-gate workflow for Critical/High waivers.
4. **Append** — When the waiver is introduced (or revalidated by a later plan), add or update the entry in the table above with the introducing plan's link.
5. **Retire** — When a normally-eligible (Path A or Path B) version supersedes the waivered pin, move the entry to the "Retired" section below with the retirement date and the plan that retired it.

## Retired Waivers

_None yet._

## See Also

- [Dependency Bump Stability & Safety Policy](../../repo-governance/development/workflow/dependency-bump-policy.md) — the three-path decision tree (A/B/C) governing every dependency bump.
- [`stack-update` plan (2026-05-15)](../../plans/done/2026-05-15__stack-update/) — first plan to populate this register; introduced all 8 active waivers above.
