Posted: Sunday, March 1, 2026
Platform: LinkedIn

---

OPEN SHARIA ENTERPRISE
Week 0015, Phase 1 Week 3

---

🏗️ The Infrastructure Marathon: 137 Commits, 2 Plans Closed, Full Stack Updated

Week 3 of Phase 1. This was the biggest week yet — two formal plans completed, every project touched.

**Week 0015 Progress:**

CI Standardization (Plan 1 — Closed):

- Canonical Nx targets enforced across all 11 projects: typecheck, lint, test:quick now consistent everywhere
- Added typecheck + lint gates to pre-push hook
- organiclever-web: oxlint + vitest + typecheck targets added
- organiclever-be: canonical target names, test:quick, start targets
- Go CLI apps: golangci-lint target added to all 5 projects
- Hugo sites: markdownlint lint target added
- Nx upgraded 22.1.3 → 22.5.2; defaultBase fixed for affected comparisons
- golangci-lint config consolidated to repo root

Dependency Sweep (Plan 2 — Closed):

- Next.js 14 → 16 (two major versions in one week)
- TailwindCSS v3 → v4
- Hugo themes: hextra v0.12.0, PaperMod latest
- Go toolchain normalized to 1.26 across all modules
- Flutter pub packages updated
- Spring Boot 4.0.2 → 4.0.3
- Gradle 9.1.0 + AGP 9.0.1 for Java 25 support
- Node.js Volta pin updated
- TypeScript: enabled noUncheckedIndexedAccess across all projects

Go Ecosystem Expansion:

- 80% test coverage enforced on all 5 Go projects (rhino-cli, javaproject-cli, ayokoding-cli, oseplatform-cli, golang-commons)
- New `golang-commons` lib: shared link checker and output utilities extracted
- New `hugo-commons` lib: Hugo-specific utilities extracted (link checker for Hugo sites)
- New `oseplatform-cli`: CLI tool with links check command for OSE Platform maintenance
- New `javaproject-cli`: standalone Java null-safety annotation validator (extracted from rhino-cli)
- `ayokoding-cli`: links check command added
- `rhino-cli`: validate-java-null-safety command added (v0.6.0), then extracted to dedicated CLI
- ayokoding-web: 96 broken internal links detected and fixed; links check added to test:quick

OrganicLever Hardening:

- Storybook 10 added to organiclever-web (nextjs-vite framework)
- 12 failing E2E tests fixed across Chromium, Firefox, and WebKit
- WebKit auth bug fixed (secure cookie detection by protocol)
- organiclever-web added to Docker Compose dev environment; CI updated to use Docker
- PR quality gate workflow added
- Scheduled E2E workflows for organiclever-web and organiclever-be (morning + evening WIB)
- organiclever-be: JSpecify + NullAway typecheck target (null safety enforcement)
- organiclever-be: package annotation check added to typecheck
- organiclever-app-web-e2e: removed and deferred to future phase

CI/CD:

- Scheduled deploy workflows for ayokoding-web and oseplatform-web
- Workflows renamed to "Test" and "Deploy" for clarity
- Nx implicit-dep cascade builds fixed across 4 projects

Experimental:

- apps-labs: hello-rust-be added (Actix Web + Tokio) — early Rust exploration

**Infrastructure Checklist:**

- ✅ Development environment (rhino-cli doctor)
- ✅ E2E test infrastructure (Playwright for 3 apps)
- ✅ Canonical Nx target standards (all 11 projects)
- ✅ Dependency sweep (full stack up to date)
- ✅ Go test coverage enforcement (≥80%, all 5 projects)
- ✅ CI/CD pipeline hardening + scheduled deployments
- ✅ Go tooling ecosystem (4 CLIs + 2 shared libs)
- 🔄 Feature development: not yet

---

Phase 1 Goal: Organic Lever (productivity tracker)
Stack: Next.js (web — prototype fast) + Flutter (Android/iOS — optimized UX)
Timeline: 12-16 weeks, Insha Allah

The infrastructure is done. No more excuses. Time to build.

---

Last week: E2E testing suite + governance hardening
This week: CI standardization + dependency sweep + Go ecosystem (137 commits, 2 plans closed)
Next week: Gherkin integration in integration + E2E tests, more CI/CD improvements (OrganicLever apps), Insha Allah

Every commit visible on GitHub. Updates every second Sunday.

---

🔗 LINKS

- Roadmap: https://github.com/wahidyankf/open-sharia-enterprise/blob/main/ROADMAP.md
- Phase 0 Retrospective: https://www.oseplatform.com/updates/2026-02-08-phase-0-end-of-phase-0/
- All Updates: https://www.oseplatform.com/updates/
- Learning Content: https://www.ayokoding.com/

---

# Golang #NextJS #CI #CodeQuality #Infrastructure #DependencyManagement #Testing #Storybook #Docker #BuildingInPublic #OpenSource #SoftwareEngineering #EnterpriseArchitecture #ProductivityTools #ShariahCompliant
