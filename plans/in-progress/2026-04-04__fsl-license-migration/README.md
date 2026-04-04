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

### Relationship to LGPL Dependency Work

A prior license audit (2026-03-26) identified 3 LGPL runtime dependencies that could conflict with
FSL's non-compete clause (LGPL Section 7 prohibits "further restrictions"). Those findings are
incorporated into [tech-docs.md](./tech-docs.md) and the LGPL mitigation steps are included in
the [delivery plan](./delivery.md).

### Change Date

The FSL Change Date will be set to **2028-04-04** (2 years from the license change date). After
this date, all code released under FSL-1.1-MIT automatically becomes MIT-licensed.
