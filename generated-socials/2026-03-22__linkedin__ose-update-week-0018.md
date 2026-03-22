Posted: Saturday, March 22, 2026
Platform: LinkedIn

---

OPEN SHARIA ENTERPRISE
Week 18 / Phase 1, Week 6

Last week: 11 backends, 1 frontend, no shared API contract.
This week: OpenAPI contract with codegen for all 14 apps, 2 new frontends, and the Nx dependency graph fully audited.

What changed:

API Contract (NEW)
Introduced an OpenAPI 3.1 spec covering all demo endpoints. Codegen now auto-generates language-specific types for all 14 demo apps (11 backends + 3 frontends). Type mismatches are caught at compile time.

Frontends: 1 to 3
Two new frontends built and deployed, all three now fully Gherkin-compliant with shared BDD scenarios.

demo-fe-ts-tanstack-start (NEW) — TanStack Router SPA. Full auth, expenses, admin. 76% test coverage.

demo-fe-dart-flutterweb (NEW) — Pure Dart Web app using package:web. Custom in-memory ServiceClient for BDD tests. 89% coverage.

demo-fe-ts-nextjs — Already compliant at 74%. Unchanged.

Backend Unit Tests: HTTP to Service-Layer
Refactored 4 backends (Go/Gin, Rust/Axum, Python/FastAPI, TypeScript/Effect) from HTTP calls to direct service function calls. All 11 backends now match the three-level testing standard: mocked deps, no HTTP, Gherkin-driven.

Nx Dependency Graph: Fully Audited
The graph was missing many real dependencies. Audited all 30 projects, fixed 68 edges, resolved one circular dependency. Zero cycles remaining. Published a full dependency diagram.

Infrastructure and Tooling
Updated all 5 C4 architecture diagrams with current implementations and test coverage mapping. Added Codecov upload for both new frontends. Fixed an E2E navigation race condition. Released rhino-cli v0.13.0 with new contract post-processing commands, replacing all codegen shell scripts.

AyoKoding
17 new by-example tutorials in Go, C, and Java.

Current state:

11 demo backends (10 languages), all CI green, all at or above 90% coverage

3 demo frontends, all Gherkin-compliant, all at or above 70% coverage

30 Nx projects, 68 dependency edges, 0 circular dependencies

29 Gherkin feature files (14 BE + 15 FE), 170 total scenarios

OpenAPI 3.1 contract with codegen for all 14 demo apps

Phase 1 Goal: OrganicLever (productivity tracker)
Stack leaning: Next.js + XState + Effect TS (frontend), F#/Giraffe (backend) — still evaluating
Timeline: Quality over deadlines, Insha Allah

Every commit is visible on GitHub.

GitHub: https://github.com/wahidyankf/open-sharia-enterprise
Updates: https://www.oseplatform.com/updates/
Learning: https://www.ayokoding.com/
