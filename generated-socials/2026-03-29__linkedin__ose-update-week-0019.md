Posted: Saturday, March 29, 2026
Platform: LinkedIn

---

OPEN SHARIA ENTERPRISE
Week 19 / Phase 1, Week 7

Last week: 14 demo apps, 2 Hugo websites, no database migrations.
This week: both websites rewritten to Next.js 16, migrations for all 12 backends, a full-stack demo app, shared UI libraries, and OrganicLever's stack finalized.

What changed:

Hugo to Next.js 16 (Both Websites)
ayokoding.com and oseplatform.com rewritten from Hugo to Next.js 16 with App Router, tRPC, and SSG. AyoKoding got repository pattern with DI, full-text search, Playwright BDD E2E tests, and oxlint. Zero Hugo sites remain.

Database Migrations (All 12 Backends)
Every backend now has idiomatic migration tooling: Flyway (Kotlin), Liquibase (Vert.x), Alembic (Python), goose (Go), DbUp (F#), Migratus (Clojure), EF Core (C#), @effect/sql (TypeScript), plus existing Ecto, Spring Boot, and Drizzle—PostgreSQL schema standardization—all CI green.

Fullstack Demo App (NEW)
Next.js 16 with 24 route handlers, Drizzle ORM, JWT auth. 170 E2E tests passing (78 BE + 92 FE). Integration tests hit real PostgreSQL.

Shared UI Libraries (NEW)
ts-ui and ts-ui-tokens: design tokens, Radix primitives, Tailwind, Storybook with visual tests. jsx-a11y linting is enabled across all frontends.

Repository Pattern (4 More Backends)
Idiomatic abstractions: trait objects (Rust), Protocol (Python), defprotocol (Clojure), function records (F#).

OrganicLever Fullstack Evolution
Google OAuth + JWT refresh tokens, BFF proxy, OpenAPI 3.1 contract shared between F# backend and Next.js frontend, codegen for both.

AyoKoding
16 new by-example tutorials: 9 database migration tools, 4 tRPC/Zod/Testing Library/Tailwind, 3 git worktree.

Current state:

- 12 demo backends (11 languages), all CI green, all at or above 90% coverage
- 4 demo frontends (3 languages), all Gherkin-compliant, all at or above 70% coverage
- 1 fullstack demo (Next.js 16 + Drizzle + PostgreSQL), 170 E2E tests
- 38 Nx projects, 81 Gherkin feature files, 388 scenarios
- 2 shared UI libraries with Storybook and visual tests
- 0 Hugo sites remaining

What's next:

Wrapping up CI exploration, then exploring CD for maximum iteration speed on OrganicLever.

Phase 1 Goal: OrganicLever (productivity tracker)
Stack decided: Next.js 16 + F#/Giraffe, Google OAuth, shared OpenAPI contract with codegen
Timeline: Quality over deadlines, Insha Allah

Every commit is visible on GitHub.

GitHub: https://github.com/wahidyankf/open-sharia-enterprise
Updates: https://www.oseplatform.com/updates/
Learning: https://www.ayokoding.com/
