Posted: Saturday, March 22, 2026
Platform: LinkedIn

---

OPEN SHARIA ENTERPRISE
Week 18 / Phase 1, Week 6

Phase 1 Week 6: API Contract, 3 Frontends, Nx Graph Audit

Last week: 11 backends, 1 frontend, no shared API contract.
This week: OpenAPI contract with codegen for all 14 apps, 2 new frontends, and the entire Nx dependency graph audited and fixed.

What changed since last week:

API Contract (NEW)
OpenAPI 3.1 spec covering all endpoints. Codegen auto-generates language-specific types for all 14 demo apps (11 backends + 3 frontends). Type mismatches caught at compile time.

Frontends: 1 to 3
All now fully Gherkin-compliant with shared BDD scenarios.

demo-fe-ts-tanstack-start (NEW) — TanStack Router SPA. Full auth, expenses, admin. 76% test coverage.
demo-fe-dart-flutterweb (NEW) — Pure Dart Web app using package:web. Custom in-memory ServiceClient for BDD tests. 89% coverage.
demo-fe-ts-nextjs — Already compliant at 74%. Unchanged.

Backend Unit Tests: HTTP to Service-Layer
Refactored 4 backends (Go/Gin, Rust/Axum, Python/FastAPI, TypeScript/Effect) from HTTP calls to direct service function calls. All 11 backends now match the three-level testing standard: mocked deps, no HTTP, Gherkin-driven.

Nx Dependency Graph: Fully Audited
Was missing many real dependencies. Audited all 30 projects and fixed 68 dependency edges. Found and resolved one circular dependency. Zero cycles remaining. Published full dependency diagram with Mermaid.

C4 Architecture Docs
Updated all 5 diagrams with current implementations, test coverage mapping per component, and CI pipelines.

CI Improvements
Added Codecov coverage upload for both new frontends. Fixed E2E navigation race condition.

rhino-cli v0.13.0
New contract post-processing commands. All demo codegen shell scripts replaced with CLI commands.

AyoKoding
17 new by-example tutorials in Go, C, and Java.

Current state:

11 demo backends (10 languages), all CI green, all at or above 90% coverage
3 demo frontends, all Gherkin-compliant, all at or above 70% coverage
30 Nx projects, 68 dependency edges, 0 circular dependencies
29 Gherkin feature files (14 BE + 15 FE), 170 total scenarios
OpenAPI 3.1 contract with codegen for all 14 demo apps

Phase 1 Goal: OrganicLever (productivity tracker)
Stack: Next.js + XState + Effect TS (frontend) + F#/Giraffe (backend), still evaluating
Timeline: Quality over deadlines, Insha Allah

Every commit is visible on GitHub.

GitHub: https://github.com/wahidyankf/open-sharia-enterprise
Updates: https://www.oseplatform.com/updates/
Learning: https://www.ayokoding.com/
