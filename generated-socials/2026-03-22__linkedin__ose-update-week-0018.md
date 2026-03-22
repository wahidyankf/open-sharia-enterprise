Posted: Saturday, March 22, 2026
Platform: LinkedIn

---

OPEN SHARIA ENTERPRISE
Week 0018, Phase 1 Week 6

---

Phase 1 Week 6: API Contract, 3 Frontends, Nx Graph Audit

Last week: 11 backends, 1 frontend, no shared API contract. This week: OpenAPI contract with codegen for all 14 apps, 2 new frontends, and the Nx dependency graph fully audited.

**What changed since last week:**

API Contract: demo-contracts (NEW)

- OpenAPI 3.1 spec covering all demo endpoints (auth, users, expenses, admin, health).
- Codegen generates language-specific types for all 14 demo apps.
- Contract violations caught at compile time via typecheck dependency on codegen.

Frontends: 1 -> 3 (all Gherkin-compliant)

- demo-fe-ts-tanstack-start (NEW): TanStack Router SPA. Full auth, expenses, admin. 76% coverage.
- demo-fe-dart-flutterweb (NEW): Pure Dart Web app using package:web (no Flutter widgets in VM). In-memory ServiceClient for BDD tests. 89% coverage.
- demo-fe-ts-nextjs: Already compliant at 74% coverage. Unchanged.

Backend Unit Tests: HTTP -> Service-Layer

Refactored 4 backends (Go/Gin, Rust/Axum, Python/FastAPI, TypeScript/Effect) from HTTP calls to direct service function calls in unit tests. All 11 backends now match the three-level testing standard: mocked deps, no HTTP, Gherkin-driven.

Nx Graph: Unaudited -> 30 projects, 68 edges, 0 cycles

Deep audit found and fixed:

- demo-contracts (new this week) wired with 18 dependents via implicitDependencies.
- rhino-cli added to 10 projects that invoke it for coverage validation.
- Gherkin spec inputs added to all 3 frontends and 3 E2E projects.
- Codegen lib dependencies declared (elixir-openapi-codegen, clojure-openapi-codegen).
- Circular dependency resolved (golang-commons <-> rhino-cli).
- New: docs/reference/re\_\_project-dependency-graph.md with full Mermaid diagram.

C4 Docs: Updated all 5 diagrams with implementations, Gherkin coverage per component, CI pipelines.

CI: Added Codecov upload for TanStack Start and Flutter Web. Fixed E2E navigation race condition.

rhino-cli: v0.13.0 — new contracts commands (java-clean-imports, dart-scaffold). All codegen shell scripts replaced.

AyoKoding: 17 new by-example tutorials (Go, C, Java).

**Current state:**

- 11 demo backends (10 languages), all CI green, all >= 90% coverage
- 3 demo frontends, all Gherkin-compliant, all >= 70% coverage
- 30 Nx projects, 68 dependency edges, 0 circular dependencies
- 29 feature files (14 BE + 15 FE), 170 total scenarios
- OpenAPI 3.1 contract with codegen for all 14 demo apps

---

Phase 1 Goal: OrganicLever (productivity tracker)
Stack: Next.js + XState + Effect TS (frontend) + F#/Giraffe (backend), still evaluating
Timeline: Quality over deadlines, Insha Allah

---

Every commit is visible on GitHub.

---

Links

- GitHub: https://github.com/wahidyankf/open-sharia-enterprise
- All Updates: https://www.oseplatform.com/updates/
- Learning Content: https://www.ayokoding.com/
