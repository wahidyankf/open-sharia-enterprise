# Nx Targets Standardization

**Status**: In Progress
**Date**: 2026-02-23
**Priority**: High
**Standard**: [governance/development/infra/nx-targets.md](../../../governance/development/infra/nx-targets.md)

## Overview

Bring all 10 apps and the workspace `nx.json` into full compliance with the Nx Target Standards
convention. The convention defines canonical target names, mandatory targets per project type, and
caching rules. All project.json files were written before the standard was finalized — they use
non-standard names and are missing required targets.

**Scope**: `nx.json` + 10 `project.json` files in `apps/`

**No documentation changes needed**: READMEs were already updated by `repo-governance-maker` in a
prior session to reference canonical target names. Only `project.json` and `nx.json` files require
changes.

## Files

- [requirements.md](./requirements.md) — gap analysis per project with acceptance criteria
- [tech-docs.md](./tech-docs.md) — exact JSON changes needed for every file
- [delivery.md](./delivery.md) — ordered implementation checklist

## Gap Summary

| App                        | Type        | Existing Targets                                               | Missing / Non-Standard                                                                                                                                       |
| -------------------------- | ----------- | -------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `nx.json`                  | workspace   | `build`, `test` ⚠️, `lint` (targetDefaults)                    | Remove legacy `tasksRunnerOptions`; missing `test:quick`, `test:unit`, `typecheck`, `test:integration`, `test:e2e` defaults; has non-standard `test` default |
| `ayokoding-cli`            | Go CLI      | `build`, `test:quick`, `run`, `install`                        | Missing `lint`                                                                                                                                               |
| `rhino-cli`                | Go CLI      | `build`, `test:quick`, `run`, `install`                        | Missing `lint`                                                                                                                                               |
| `ayokoding-web`            | Hugo site   | `dev`, `build`, `clean`, `test:quick`, `run-pre-commit`        | Missing `lint`                                                                                                                                               |
| `oseplatform-web`          | Hugo site   | `dev`, `build`, `clean` ⚠️                                     | Missing `test:quick`, `lint`; `clean` incomplete                                                                                                             |
| `organiclever-web`         | Next.js     | `dev`, `build`, `start`, `lint` ⚠️                             | `lint`→oxlint (replaces `next lint`); missing `test:quick`, `typecheck`, `test:unit`, `test:integration`; add vitest + devDeps                               |
| `organiclever-be`          | Spring Boot | `build`, `serve` ⚠️, `test` ⚠️, `lint`                         | `serve`→`dev`, `test`→`test:unit`; missing `test:quick`, `start`, `outputs` on `build`                                                                       |
| `organiclever-app`         | Flutter     | `install`, `dev`, `build:web`, `test` ⚠️, `test:quick`, `lint` | `test`→`test:unit`; missing `typecheck`, `dependsOn` on `test:quick`                                                                                         |
| `organiclever-web-e2e`     | Playwright  | `install`, `e2e` ⚠️, `e2e:ui` ⚠️, `e2e:report` ⚠️              | `e2e`→`test:e2e`, `e2e:ui`→`test:e2e:ui`, `e2e:report`→`test:e2e:report`; missing `lint`, `test:quick`                                                       |
| `organiclever-be-e2e`      | Playwright  | `install`, `e2e` ⚠️, `e2e:ui` ⚠️, `e2e:report` ⚠️              | Same as `organiclever-web-e2e`                                                                                                                               |
| `organiclever-app-web-e2e` | Playwright  | `install`, `e2e` ⚠️, `e2e:ui` ⚠️, `e2e:report` ⚠️              | Same as `organiclever-web-e2e`                                                                                                                               |
| `.husky/pre-push`          | hook        | `nx affected -t test:quick` only                               | Add `nx affected -t typecheck` and `nx affected -t lint`; fixes diagram bug (lint shown but never blocked push)                                              |

## Critical Finding

`oseplatform-web` has **no `test:quick` target** — it is silently excluded from the pre-push hook
(`nx affected -t test:quick`) and the PR merge gate. This is the highest-priority fix.

`organiclever-web` also has **no `test:quick` target** — same consequence.

All three Playwright E2E projects use **`e2e`** instead of **`test:e2e`** — their existing tests
cannot be invoked via the workspace-level `nx affected -t test:e2e` cron pattern.

## Non-Goals

- Changing test frameworks or build tools
- Adding test coverage where none exists today
- Changing the logic of any existing command (only renaming and adding targets)
- Updating README or documentation files (already done)
