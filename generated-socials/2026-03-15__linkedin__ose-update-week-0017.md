Posted: Saturday, March 15, 2026
Platform: LinkedIn

---

OPEN SHARIA ENTERPRISE
Week 0017, Phase 1 Week 5

---

🔥 Phase 1 Week 5: 11 Backends, 11 Languages, All Green

One REST API spec. Eleven implementations. Every CI pipeline green.

This week was the backend tech stack evaluation — building the same API in 11 different languages. Not just for learning, but to find the "middle path" in API design and architecture that stays compatible across stacks, so swapping tech in the future is a realistic option, not a hard rewrite effort. Plus real CI pipeline benchmarks across all 11 stacks.

**What shipped:**

11 Demo Backend Implementations (all CI green, ≥90% coverage):

- Java/Spring Boot, Java/Vert.x
- Elixir/Phoenix
- Go/Gin
- F#/Giraffe, C#/ASP.NET Core
- Python/FastAPI
- Rust/Axum
- Kotlin/Ktor
- TypeScript/Effect
- Clojure/Pedestal

9 new backends built this week. All 11 share the same Gherkin specs, same Playwright E2E suite, same PostgreSQL schema.

Three-Level Testing Standard:

- Unit: mocked dependencies, Gherkin-driven, ≥90% coverage
- Integration: real PostgreSQL via Docker, no HTTP calls, Gherkin-driven
- E2E: real HTTP via Playwright, Gherkin-driven
- All three levels consume the same specs — only step implementations differ

Specs Consolidation:

- Unified demo-be and demo-fe specs into specs/apps/demo/
- Added specs-validation workflow + checker/fixer/maker agents

CI/Infrastructure Cleanup:

- Simplified all workflow names (dropped verbose prefixes)
- Codecov flags for all 11 backends + frontend
- README overhaul: tables → lists, removed hardcoded counts and stale details

AI Agents & Workflows:

- 6 new developer agents (Dart, Kotlin, C#, F#, Clojure, Rust)
- 6 new programming language style guides
- 3 new specs agents (specs-checker, specs-fixer, specs-maker)
- specs-validation workflow for automated spec quality gates
- Agent workflow orchestration convention

**What's next:**

- Frontend demos: Flutter Web, Remix, TanStack Start
- Evaluate XState and Effect TS in React-based frontends
- Frontend and backend framework evaluations
- Standardize and tidy up CI for demo-be and demo-fe
- Sharpen specs, Gherkin, and tooling

---

Phase 1 Goal: OrganicLever (productivity tracker)
Stack: Frontend TBD + backend leaning toward F#/Giraffe, but still evaluating
Timeline: Quality over deadlines, Insha Allah

---

Every commit is visible on GitHub.

---

🔗 LINKS

- GitHub: https://github.com/wahidyankf/open-sharia-enterprise
- All Updates: https://www.oseplatform.com/updates/
- Learning Content: https://www.ayokoding.com/
