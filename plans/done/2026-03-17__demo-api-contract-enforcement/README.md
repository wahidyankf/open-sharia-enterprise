# Enforce API Contracts Across Demo Apps

**Status**: Done

**Created**: 2026-03-17

**Completed**: 2026-03-18

**Delivery Type**: Multi-phase rollout

**Git Workflow**: Trunk Based Development (work on `main` branch)

## Overview

Establish a single, enforceable API contract specification in `specs/apps/demo/contracts/` that
governs request/response shapes between all `demo-be-*` backends (11 languages), `demo-fe-*`
frontends (3 frameworks), and `demo-*-e2e` test suites — with **compile-time enforcement** via
auto-generated type-safe code (encoders/decoders) in every app.

### Goals

1. **Single source of truth** — one OpenAPI 3.1 spec defines every endpoint's request body,
   response body, query parameters, path parameters, headers, and status codes
2. **Auto-generated code** — each app has a `generated-contracts/` folder (gitignored) containing
   language-specific types, encoders, and decoders generated from the contract
3. **Compile-time enforcement** — apps import generated types; mismatches fail `typecheck`/`build`
4. **Pre-push safety** — violations caught by `nx affected -t typecheck`, `lint`, and `test:quick`
   (already in pre-push hook and PR quality gate)
5. **Strict camelCase** — all JSON fields use camelCase (no snake_case or kebab-case), enforced
   by Spectral linting with zero exceptions
6. **API documentation** — auto-generated browsable API docs (Redoc) viewable by
   public/product/any team via `nx run demo-contracts:docs`
7. **Language-agnostic** — the contract is YAML; code generators produce Go structs, Java DTOs,
   Kotlin data classes, Python Pydantic models, Rust serde structs, Elixir structs, F#/C# records,
   Clojure Malli schemas, TypeScript types, and Dart classes

### Context

- **Behavior specs exist** — 78 backend + 92 frontend Gherkin scenarios define _what_ the API does,
  but not the exact shape of every field, type, or constraint
- **Types are duplicated** — each backend has its own DTOs/structs, each frontend has its own
  `types.ts` / Dart models. Nothing enforces sync.
- **Drift is invisible** — naming mismatches can only be caught by E2E tests, and only if a
  scenario asserts that specific field
- **No machine-readable contract** — Gherkin is human-readable but cannot drive code generation

### Enforcement Model

```
  specs/apps/demo/contracts/openapi.yaml
                    │
           ┌────────┼────────┐
           ▼        ▼        ▼
       codegen   codegen   codegen      ← Nx target per app
           │        │        │
           ▼        ▼        ▼
   Go structs   TS types  Dart classes  ← generated-contracts/ (gitignored)
           │        │        │
           ▼        ▼        ▼
     app imports generated types
     mismatch = compile error           ← caught by typecheck/build/test:quick
           │        │        │
           ▼        ▼        ▼
     pre-push hook & PR quality gate    ← nx affected -t typecheck/lint/test:quick
```

### Recommended Approach

**Alternative 3: OpenAPI 3.1 (Modular YAML) + Spectral Linting + Code Generation** — chosen from
10 alternatives analyzed across 5 categories (REST-native specs, data-shape-only formats,
language-specific source-of-truth, higher-level DSLs, non-REST paradigms) in
[requirements.md](./requirements.md). The only framework meeting all 10 hard requirements. Key
reasons:

- Full HTTP semantics (paths, methods, status codes, headers, body schemas)
- Language-agnostic YAML authoring — no new DSL to learn
- Modular domain-split files mirror existing Gherkin organization
- Spectral linting enforces API style conventions (camelCase, descriptions, examples)
- Mature code generators exist for all 11 languages (web-verified)
- Generated types include encoders/decoders for type-safe serialization
- Auto-generated browsable API documentation (Redoc) for product/public teams
- 100% open source and free — every tool is MIT or Apache 2.0 licensed

## Plan Structure

- **[requirements.md](./requirements.md)** — Problem statement, solution space, 10 alternatives
  analysis (with web research), language coverage matrix, hard requirements compliance, tool-specific
  comparisons, recommendation with rationale
- **[tech-docs.md](./tech-docs.md)** — Contract file structure, code generation strategy per
  language, Nx integration, gitignore setup, Spectral rules
- **[delivery.md](./delivery.md)** — 5-phase implementation plan with checklists, open questions
