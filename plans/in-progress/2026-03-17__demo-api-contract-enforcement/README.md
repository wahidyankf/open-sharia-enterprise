# Enforce API Contracts Across Demo Apps

**Status**: In Progress

**Created**: 2026-03-17

**Delivery Type**: Multi-phase rollout

**Git Workflow**: Trunk Based Development (work on `main` branch)

## Overview

Establish a single, enforceable API contract specification in `specs/apps/demo/contracts/` that
governs request/response shapes between all `demo-be-*` backends (11 languages), `demo-fe-*`
frontends (3 frameworks), and `demo-*-e2e` test suites — with build-time or test-time enforcement
so drift is caught automatically.

### Goals

1. **Single source of truth** for every endpoint's request body, response body, query parameters,
   path parameters, headers, and status codes
2. **Automated enforcement** — backends and frontends must conform; violations fail CI
3. **Language-agnostic** — consumable by Go, Java, Kotlin, Python, Rust, Elixir, F#, C#, Clojure,
   TypeScript, and Dart
4. **Colocated with specs** — lives in `specs/apps/demo/contracts/` alongside existing Gherkin specs
5. **Complements Gherkin** — Gherkin defines behavior flows; contracts define structural shapes

### Context

- **Behavior specs exist** — 76 backend Gherkin scenarios + 92 frontend scenarios define _what_ the
  API does, but not the precise shape of every request/response field, type, or constraint
- **Types are duplicated** — each backend defines its own DTOs/structs, and each frontend maintains
  its own `types.ts` / Dart models. Nothing enforces sync.
- **Drift is invisible** — if `demo-be-rust-axum` returns `created_at` (snake_case) while others
  return `createdAt` (camelCase), only E2E tests catch it at runtime, and only if a scenario
  happens to assert that field
- **No machine-readable contract** — Gherkin specs are human-readable but cannot be used for
  automated schema validation or code generation

### Recommended Approach

**Alternative 6: OpenAPI 3.1 (Modular YAML) + Spectral Linting + Contract Tests** — chosen from
6 alternatives analyzed in [requirements.md](./requirements.md). Key reasons:

- Full HTTP semantics (paths, methods, status codes, headers, body schemas)
- Language-agnostic YAML authoring
- Modular domain-split files mirror existing Gherkin organization
- Spectral linting enforces API style conventions
- Mature validators exist for all 11 languages (web-verified)

## Plan Structure

- **[requirements.md](./requirements.md)** — Alternatives analysis, recommendation matrix,
  acceptance criteria
- **[tech-docs.md](./tech-docs.md)** — Contract file structure, enforcement architecture,
  per-language strategy, Nx integration, Spectral rules
- **[delivery.md](./delivery.md)** — 5-phase implementation plan with checklists, open questions
