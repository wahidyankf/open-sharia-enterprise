# Plan: Migrate from MIT to FSL-1.1-MIT License

**Status**: In Progress
**Created**: 2026-04-04

## Overview

Relicense the open-sharia-enterprise repository from MIT to **FSL-1.1-MIT** (Functional Source
License 1.1 with MIT as the Change License). Under FSL-1.1-MIT:

- **Immediately**: Source code is publicly available. Anyone can use, modify, and distribute the
  software for any purpose **except** offering a competing commercial product or service.
- **After 2 years**: The code automatically converts to the MIT license with no restrictions.

This protects the project from competitors repackaging it as a competing Sharia-compliant enterprise
platform during the critical early growth period, while guaranteeing full open-source freedom after
the change date.

**Git Workflow**: Commit to `main` (Trunk Based Development)

## Quick Links

- [Requirements](./requirements.md) - Files to change, acceptance criteria, legal considerations
- [Technical Documentation](./tech-docs.md) - FSL-1.1-MIT specification, competing use definition,
  third-party code handling
- [Delivery Plan](./delivery.md) - Phased checklist and validation

## Scope Summary

### Files Requiring License Updates

| File                          | Current             | Change                                        |
| ----------------------------- | ------------------- | --------------------------------------------- |
| `LICENSE`                     | MIT full text       | Replace with FSL-1.1-MIT full text            |
| `package.json`                | `"license": "MIT"`  | Change to `"license": "FSL-1.1-MIT"`          |
| `README.md`                   | MIT License section | Update to describe FSL-1.1-MIT                |
| `CLAUDE.md`                   | `License: MIT` (x2) | Change to `License: FSL-1.1-MIT`              |
| `governance/vision/README.md` | `Open source (MIT)` | Update to reflect FSL-1.1-MIT with conversion |

### Files NOT Changed (Third-Party Code)

| File                                  | Copyright           | Why Unchanged                                  |
| ------------------------------------- | ------------------- | ---------------------------------------------- |
| `libs/elixir-cabbage/LICENSE`         | Matt Widmann (2017) | Third-party fork; retains original MIT license |
| `libs/elixir-gherkin/LICENSE`         | Matt Widmann (2018) | Third-party fork; retains original MIT license |
| `archived/ayokoding-web-hugo/LICENSE` | Xin (2023)          | Third-party fork; retains original MIT license |

## Context

### Why FSL-1.1-MIT

The project's [vision](../../../governance/vision/open-sharia-enterprise.md) aims to democratize
Shariah-compliant enterprise systems. FSL-1.1-MIT balances two goals:

1. **Protection**: Prevents competitors from taking the code and offering a competing commercial
   Sharia-compliant enterprise platform without contributing back
2. **Openness**: Source code is fully visible from day one, and converts to MIT after 2 years —
   guaranteeing eventual full freedom

### Dependency Compatibility

A full dependency audit (2026-04-04) of all **production** (non-demo) apps found:

- **0 GPL/AGPL** dependencies — clean
- **1 LGPL** dependency — `@img/sharp-libvips` (LGPL-3.0), transitive optional via Next.js →
  `sharp`. Affects `ayokoding-web`, `oseplatform-web`, and `organiclever-fe`. **Resolution**:
  set `images.unoptimized: true` in all 3 apps to eliminate sharp entirely (Vercel handles image
  optimization at the edge anyway)
- **MPL-2.0** — HashiCorp libs (`go-immutable-radix`, `go-memdb`, `golang-lru`), indirect deps
  via `godog` in Go CLI apps. File-level copyleft only — no conflict with FSL. No action needed.
- **All other deps** — MIT, Apache-2.0, BSD, ISC, PostgreSQL License (all permissive)

Demo apps (`a-demo-*`) are excluded from this audit — they are reference implementations only and
do not ship as products. See [tech-docs.md](./tech-docs.md) for the full audit and
[delivery.md](./delivery.md) for mitigation steps.

### Change Date and Per-Version Rolling Conversion

The FSL Change Date will be set to **2028-04-04** (2 years from the initial license change). FSL
converts to MIT on a **per-version (per-commit) rolling basis**: each commit becomes MIT-licensed
2 years after its first public distribution. The Change Date is the floor — the earliest any code
becomes MIT. Code committed after 2026-04-04 gets its own 2-year window (e.g., a commit from
2026-06-15 becomes MIT on 2028-06-15). See [tech-docs.md](./tech-docs.md) for the full
explanation.
