# Plan: CI/CD Standardization

**Status**: In Progress
**Created**: 2026-03-31

## Overview

This plan standardizes the entire CI/CD pipeline across the monorepo -- from local git hooks
through GitHub Actions workflows to Docker-based development and testing. The repository currently
has **22 GitHub Actions workflows**, **44 docker-compose files**, **38 Dockerfiles** (10 production, 13 integration, 13 dev, 1 CI, 1 other), a 9-step
pre-commit hook, and a 3-target pre-push gate spanning **13+ language runtimes**. Much of this
infrastructure grew organically as new backends and frameworks were added, resulting in significant
duplication, inconsistent patterns, and undocumented conventions.

**Git Workflow**: Commit to `main` (Trunk Based Development)

## Problem Statement

1. **Workflow duplication**: 11 near-identical backend test workflows (`test-a-demo-be-*.yml`)
   differ only in language setup and docker-compose paths. Adding a 12th backend means copying
   ~150 lines and making 5-10 substitutions.
2. **Monolithic PR quality gate**: A single GitHub Actions job installs 13+ language runtimes
   (Go, Java 21+25, .NET 10, Elixir, Python, Rust, Flutter, Clojure CLI, etc.) even when the PR
   only touches one app.
3. **No reusable workflow building blocks**: No composite actions or reusable workflows; every
   workflow is self-contained.
4. **Inconsistent Docker patterns**: Integration test compose files live in `apps/` while dev
   compose files live in `infra/dev/`. Dockerfiles vary in base image versions, layer ordering,
   and health check configuration.
5. **Missing CI coverage**: spec-coverage validation is implemented in rhino-cli but never invoked
   in CI. Fullstack app (`a-demo-fs-ts-nextjs`) lacks E2E workflow.
6. **Undocumented local dev workflow**: Docker Compose dev setups exist but lack a unified
   entrypoint or documentation for onboarding.
7. **No Docker layer caching in CI**: Every integration test workflow rebuilds Docker images from
   scratch.
8. **Coverage threshold rationale undocumented**: 90%/80%/75%/70% thresholds exist but the
   reasoning is implicit.

## Goals

1. **DRY CI workflows** via reusable workflows and composite actions
2. **Parallel, language-scoped PR quality gate** that only installs what's needed
3. **Standardized Docker templates** for Dockerfiles, docker-compose, and .dockerignore
4. **Spec-coverage validation** integrated into CI
5. **Documented local development** workflow with Docker Compose + autoreload
6. **Docker layer caching** in CI for faster integration/E2E test cycles
7. **Single source of truth** for CI conventions in governance docs

## Quick Links

- [Requirements](./requirements.md) -- Detailed requirements, gaps, and acceptance criteria
- [Technical Documentation](./tech-docs.md) -- Architecture, patterns, and implementation details
- [Delivery Plan](./delivery.md) -- Phased checklist with validation steps

## Scope

### In Scope

| Area                     | Description                                                                |
| ------------------------ | -------------------------------------------------------------------------- |
| Git hooks                | Pre-commit (9 steps), commit-msg, pre-push standardization                 |
| Local quality gates      | formatting, typecheck, lint, test:quick across all project types           |
| Three-level testing      | test:unit, test:integration, test:e2e for BE, FE, FS, CLI                  |
| Specs folder structure   | Directory layout, contract-driven development, Gherkin consumption         |
| GitHub Actions workflows | PR quality gate, backend/frontend test workflows, test-and-deploy, codecov |
| Local Docker development | docker-compose dev setups with autoreload for all app types                |
| CI Docker infrastructure | Integration test Dockerfiles, CI overlays, layer caching                   |
| Governance documentation | CI conventions doc in `repo-governance/development/`                       |
| Compliance enforcement   | ci-checker/ci-fixer agents, ci-quality-gate workflow, ci-standards skill   |

### Out of Scope

- Vercel deployment configuration (covered by existing deployer agents)
- Application-level code changes (only CI/build infrastructure)
- New testing frameworks or tools (standardize what exists)
- Kubernetes deployment (future initiative)
- Monitoring/observability (separate concern)

## Workstreams

| #   | Workstream                                                                        | Phase | Dependencies                                                   |
| --- | --------------------------------------------------------------------------------- | ----- | -------------------------------------------------------------- |
| W1  | [Governance & Documentation](#w1-governance--documentation)                       | 1     | None                                                           |
| W2  | [Git Hooks Standardization](#w2-git-hooks-standardization)                        | 1     | None                                                           |
| W3  | [GitHub Actions Composite Actions](#w3-github-actions-composite-actions)          | 2     | W1                                                             |
| W4  | [PR Quality Gate Optimization](#w4-pr-quality-gate-optimization)                  | 2     | W3                                                             |
| W5  | [Backend Test Workflow DRY-up](#w5-backend-test-workflow-dry-up)                  | 3     | W3                                                             |
| W6  | [Frontend & Fullstack Test Workflows](#w6-frontend--fullstack-test-workflows)     | 3     | W3                                                             |
| W7  | [Docker Standardization](#w7-docker-standardization)                              | 2     | W1                                                             |
| W8  | [Local Development with Docker](#w8-local-development-with-docker)                | 3     | W7                                                             |
| W9  | [CI Docker Caching & Optimization](#w9-ci-docker-caching--optimization)           | 4     | W5, W7                                                         |
| W10 | [Spec-Coverage Integration](#w10-spec-coverage-integration)                       | 4     | W5                                                             |
| W11 | [Gherkin Consumption Remediation](#w11-gherkin-consumption-remediation)           | 3     | W1                                                             |
| W12 | [Specs Folder Restructuring](#w12-specs-folder-restructuring)                     | 3     | W1                                                             |
| W13 | [CLI Docker Compose Setup](#w13-cli-docker-compose-setup)                         | 3     | W7                                                             |
| W15 | [Accessibility Testing Remediation](#w15-accessibility-testing-remediation)       | 3     | W1, W11                                                        |
| W16 | [Environment Variable Standardization](#w16-environment-variable-standardization) | 3     | W7                                                             |
| W17 | [CI Compliance Enforcement](#w17-ci-compliance-enforcement)                       | 4     | W1                                                             |
| W14 | [Governance Propagation](#w14-governance-propagation)                             | 4     | W1-W17 (intentionally last — depends on all other workstreams) |

## Context

### Current CI Architecture (As-Is)

```mermaid
flowchart TD
    local["Local Git Hooks\npre-commit (9 steps)\ncommit-msg + pre-push"]
    pr["PR Workflows\npr-quality-gate (monolithic)\npr-format + pr-validate-links"]
    scheduled["Scheduled Workflows\n11 BE + 3 FE + 1 FS tests\ntest-organiclever"]
    deploy["Test & Deploy\nayokoding-web\noseplatform-web"]
    codecov["Coverage Upload\n27 reports on push to main"]

    local --> pr --> scheduled --> deploy --> codecov
```

### Target CI Architecture (To-Be)

```mermaid
flowchart TD
    local["Local Git Hooks\npre-commit + commit-msg\npre-push (cacheable)"]
    actions["Reusable GH Actions\n11 composite setup-*\n3 reusable workflows"]
    pr["PR Workflows\nquality-gate (lang-scoped)\nformat + validate-links"]
    scheduled["Scheduled Workflows\n11 BE + 3 FE + 1 FS\ntest-organiclever"]
    deploy["Test & Deploy\nayokoding-web\noseplatform-web"]
    codecov_to["Coverage Upload\n27 reports on push"]

    actions --> pr
    local --> pr
    pr --> scheduled
    scheduled --> deploy
    deploy --> codecov_to
```

## Related Documentation

- [Three-Level Testing Standard](../../../repo-governance/development/quality/three-level-testing-standard.md)
- [Code Quality](../../../repo-governance/development/quality/code.md)
- [Markdown Quality](../../../repo-governance/development/quality/markdown.md)
- [Nx Targets](../../../repo-governance/development/infra/nx-targets.md)
- [Plans Organization Convention](../../../repo-governance/conventions/structure/plans.md)
