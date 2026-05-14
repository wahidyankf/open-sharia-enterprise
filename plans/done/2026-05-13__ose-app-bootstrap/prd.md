# PRD — ose-app Bootstrap

## Product Overview

This is an **infrastructure plan**. The "product" being delivered is a working four-project scaffold for OSE GRC, not a user-visible feature. Direct end-users (compliance officers, auditors) are out of scope; the immediate consumer is the next product engineer (or AI agent) tasked with implementing the first gap-analysis feature.

## Personas

### Product Engineer (immediate consumer)

Wants `nx dev ose-app-web` and `nx dev ose-app-be` to start on day 0 (the BE under `dotnet watch run`). Expects `nx affected -t typecheck lint test:quick spec-coverage` to behave consistently with how it behaves in `organiclever-*`. Expects DDD bounded contexts to be pre-declared so they don't get to invent that from scratch under feature-work pressure. Expects BDD `.feature` files to be findable in `specs/apps/ose-app/behavior/` and consumed by xUnit + TickSpec on the BE. Expects `OPENROUTER_API_KEY` to be a documented env var with `.env.example` placeholder and a typed F# settings record.

### Repo Governance (solo maintainer)

Wants the four projects to register against existing conventions — file naming, project tags (`type:app`, `type:e2e`, `platform:*`, `lang:*`, `domain:ose-app`), three-level testing, contracts codegen, and PR quality-gate language tag routing. Wants no special-casing in any cross-cutting tool (`rhino-cli`, `doctor`, PR quality gate detector). Wants AGENTS.md updated and in-progress README accurate.

## User Stories

| ID    | Story                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
| ----- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| US-1  | As a product engineer, I want `nx dev ose-app-web` to bind `http://localhost:3300` and serve a Next.js smoke page, so that I can confirm the FE scaffold compiles and renders before authoring any feature code.                                                                                                                                                                                                                                                                                                                                  |
| US-2  | As a product engineer, I want `nx dev ose-app-be` to bind `http://localhost:8302` and answer `GET /api/v1/health` with `{"status":"healthy"}`, so that I can confirm the F#/Giraffe scaffold compiles and serves the contract-declared endpoint.                                                                                                                                                                                                                                                                                                  |
| US-3  | As a product engineer, I want `nx run ose-app-be:test:quick` to pass with ≥ 90 % AltCover LCOV coverage on the scaffolded sample, so that the CI quality bar for the BE is wired from day 0 and won't have to be retrofit during the first feature plan.                                                                                                                                                                                                                                                                                          |
| US-4  | As a product engineer, I want `nx run ose-app-web:test:quick` to pass with ≥ 70 % coverage on the scaffolded sample, so that the CI quality bar for the FE is wired from day 0. _[Judgment call]: 70 % matches the conservative starting point used for `organiclever-web` (currently set to 70 % in its `test:quick` invocation despite a 75 % spec in AGENTS.md — `[Repo-grounded]` from `apps/organiclever-web/project.json`); raising to 75 % is deferred until ose-app-web grows enough surface area to hit it without cargo-culting tests._ |
| US-5  | As a product engineer, I want `npx nx affected -t typecheck lint test:quick spec-coverage` to exit 0 for all four ose-app projects after a no-op edit, so that the pre-push hook converges without per-project debugging.                                                                                                                                                                                                                                                                                                                         |
| US-6  | As a product engineer, I want four bounded contexts (`regulatory-source`, `internal-policy`, `gap-analysis`, `ai-orchestration`) pre-declared in `specs/apps/ose-app/ddd/bounded-contexts.yaml` with matching `ubiquitous-language/*.md` stubs, so that I don't invent DDD boundaries under feature-work pressure and the first feature plan extends, not redefines.                                                                                                                                                                              |
| US-7  | As a product engineer, I want `specs/apps/ose-app/behavior/be/gherkin/health.feature` consumed by `ose-app-be:test:unit` via xUnit + TickSpec, so that the BDD-test loop is exercised on a real `.feature` file from the first commit and `spec-coverage` validators report 100 %.                                                                                                                                                                                                                                                                |
| US-8  | As a product engineer, I want `specs/apps/ose-app/behavior/web/gherkin/smoke.feature` consumed by `ose-app-web-e2e:test:e2e`, so that the FE Playwright-BDD loop is exercised on a real `.feature` file from the first commit.                                                                                                                                                                                                                                                                                                                    |
| US-9  | As a product engineer, I want `infra/dev/ose-app/docker-compose.yml` to bring up `postgres` + `ose-app-be` + `ose-app-web` in one command, so that I can develop against a realistic stack without per-service manual provisioning.                                                                                                                                                                                                                                                                                                               |
| US-10 | As a product engineer, I want `.github/workflows/test-and-deploy-ose-app-web-development.yml` (mirroring the organiclever workflow) to exist and be dispatchable, so that the CI shape for ose-app is established the same day the projects land and adapting it for the first feature plan is mechanical.                                                                                                                                                                                                                                        |
| US-11 | As a product engineer, I want documented `OPENROUTER_API_KEY`, `OPENROUTER_MODEL`, `OPENROUTER_BASE_URL`, and `DATABASE_URL` placeholders in `apps/ose-app-be/.env.example` and `infra/dev/ose-app/.env.example`, so that the OpenRouter integration contract is committed in shape but not in secret value before the first feature plan calls it.                                                                                                                                                                                               |
| US-12 | As a product engineer, I want `npm run doctor` on a fresh worktree to provision the .NET 10 SDK pinned by `apps/ose-app-be/global.json` AND globally install `fantomas` + `fsharplint` AND restore the local-tool manifest's `altcover.global` + `fsharp-analyzers`, so that `nx run ose-app-be:lint` and `nx run ose-app-be:test:quick` run without manual setup.                                                                                                                                                                                |
| US-13 | As repo governance, I want all four `ose-app-*` projects registered in `AGENTS.md` (apps catalog + project structure tree) alongside the organiclever entries, so that catalog drift is prevented from the moment the projects exist.                                                                                                                                                                                                                                                                                                             |
| US-14 | As a product engineer, I want `apps/ose-app-web/src/app/page.tsx` to render at least one primitive imported from `@open-sharia-enterprise/web-ui` (e.g., `Button`), so that the shared component library wiring is verified end-to-end and feature work composes from it instead of forking a per-app component set.                                                                                                                                                                                                                              |

## Gherkin Acceptance Criteria

These scenarios are the executable gate for the plan. `plan-execution-checker` runs through them after delivery completes.

### AC-1: Frontend dev server boots

```gherkin
Given the repository is freshly cloned at the plan's HEAD
And `npm install` has completed
When I run `npx nx dev ose-app-web`
Then the process binds to port 3300 within 60 seconds
And `curl -sf http://localhost:3300` returns HTTP 200
```

### AC-2: Backend dev server boots and answers health

```gherkin
Given the repository is at the plan's HEAD
And PostgreSQL is running at the URL in `apps/ose-app-be/.env.example`
When I run `npx nx dev ose-app-be`
Then the `dotnet watch run` process starts and the Giraffe HTTP listener binds to port 8302 within 60 seconds
And `curl -sf http://localhost:8302/api/v1/health` returns HTTP 200
And the response body matches the `HealthResponse` schema in `specs/apps/ose-app/containers/contracts/schemas/health.yaml`
```

### AC-3: Affected quality gate passes

```gherkin
Given the repository is at the plan's HEAD on a fresh worktree
And `npm install && npm run doctor -- --fix` has completed successfully
When I run `npx nx run-many -t typecheck lint test:quick spec-coverage --projects=ose-app-web,ose-app-be,ose-app-web-e2e,ose-app-be-e2e`
Then every target exits 0
And `ose-app-be:test:quick` reports coverage ≥ 90% on `apps/ose-app-be/coverage/altcov.info`
And `ose-app-web:test:quick` reports coverage ≥ 70% on `apps/ose-app-web/coverage/lcov.info`
```

### AC-4: DDD validators pass for ose-app

```gherkin
Given `apps/rhino-cli/main.go` is the current rhino-cli build
When I run `CGO_ENABLED=0 go run -C apps/rhino-cli main.go ddd bc ose-app`
Then the validator exits 0
When I run `CGO_ENABLED=0 go run -C apps/rhino-cli main.go ddd ul ose-app`
Then the validator exits 0
And the YAML at `specs/apps/ose-app/ddd/bounded-contexts.yaml` lists exactly the contexts: `regulatory-source`, `internal-policy`, `gap-analysis`, `ai-orchestration`
And every context has a matching `specs/apps/ose-app/ddd/ubiquitous-language/<context>.md` file
```

### AC-5: BDD smoke specs match BE + FE E2E projects

```gherkin
Given `specs/apps/ose-app/behavior/be/gherkin/health.feature` exists
When I run `npx nx run ose-app-be:test:unit`
Then the BDD scenarios in that feature file execute as xUnit + TickSpec tests
And the suite exits 0
Given `specs/apps/ose-app/behavior/web/gherkin/smoke.feature` exists
When I run `npx nx run ose-app-web-e2e:test:e2e` with the dev stack running
Then the BDD scenarios execute as Playwright-BDD tests
And the suite exits 0
```

### AC-6: Contracts codegen produces typed clients/servers

```gherkin
Given `specs/apps/ose-app/containers/contracts/openapi.yaml` exists
When I run `npx nx run ose-app-contracts:bundle`
Then `specs/apps/ose-app/containers/contracts/generated/openapi-bundled.yaml` is produced
When I run `npx nx run ose-app-be:codegen`
Then `apps/ose-app-be/generated-contracts/` contains F# record types for `HealthResponse` and `ErrorResponse` under the `OseAppBe.Contracts` namespace
When I run `npx nx run ose-app-web:codegen`
Then `apps/ose-app-web/src/generated-contracts/` contains TypeScript types for `HealthResponse` and `ErrorResponse`
```

### AC-7: docker-compose dev stack starts and converges

```gherkin
Given `infra/dev/ose-app/docker-compose.yml` exists
When I run `docker compose -f infra/dev/ose-app/docker-compose.yml up --build -d`
Then a `postgres` service becomes healthy within 60 seconds
And a `ose-app-be` service binds port 8302 and `curl -sf http://localhost:8302/api/v1/health` returns 200 within 120 seconds
And a `ose-app-web` service binds port 3300 and `curl -sf http://localhost:3300` returns 200 within 120 seconds
And `docker compose -f infra/dev/ose-app/docker-compose.yml down -v` succeeds
```

### AC-8: PR quality gate auto-detects ose-app

```gherkin
Given a PR modifies `apps/ose-app-be/src/OseAppBe/Program.fs`
When the `pr-quality-gate.yml` workflow's `detect` job runs
Then `has-dotnet` is `true`
And the `dotnet` job is scheduled
Given a PR modifies `apps/ose-app-web/src/app/page.tsx`
When the `detect` job runs
Then `has-ts` is `true`
And the `typescript` job is scheduled
```

### AC-9: doctor handles ose-app-be .NET toolchain

```gherkin
Given a fresh worktree without the .NET 10 SDK installed
And `apps/rhino-cli/internal/doctor/tools.go` has been updated to anchor the F# SDK probe on `apps/ose-app-be/global.json` (replacing the dead `apps/a-demo-be-fsharp-giraffe/global.json` reference)
When I run `npm install && npm run doctor -- --fix`
Then the script provisions the .NET 10 SDK pinned by `apps/ose-app-be/global.json`
And `dotnet --version` reports a 10.x SDK
And `dotnet tool restore` from `apps/ose-app-be` installs `altcover.global` and `fsharp-analyzers` per `apps/ose-app-be/dotnet-tools.json` (matching the ose-primer `crud-be-fsharp-giraffe/dotnet-tools.json` declared set — `[Repo-grounded]`)
And `fantomas --version` and `dotnet fsharplint --version` succeed (provisioned globally by doctor, NOT via the local-tools manifest)
```

### AC-10: AGENTS.md catalog matches the four new projects

```gherkin
Given `AGENTS.md` is loaded
When I search for `ose-app-web`, `ose-app-be`, `ose-app-web-e2e`, `ose-app-be-e2e`
Then each project name appears at least once in the **Current Apps** list
And each project name appears at least once in the project structure tree
```

### AC-11: ose-app-web consumes shared @open-sharia-enterprise/web-ui

```gherkin
Given `apps/ose-app-web/package.json` is loaded
Then `dependencies` contains `"@open-sharia-enterprise/web-ui": "*"`
And `dependencies` contains `"@open-sharia-enterprise/web-ui-token": "*"`

Given `apps/ose-app-web/next.config.ts` is loaded
Then the `transpilePackages` array contains both `@open-sharia-enterprise/web-ui` and `@open-sharia-enterprise/web-ui-token`

Given `apps/ose-app-web/src/app/globals.css` is loaded
Then the file imports `@open-sharia-enterprise/web-ui-token/src/tokens.css`
And the file does NOT import `@open-sharia-enterprise/web-ui-token/src/organiclever.css`

Given `apps/ose-app-web/src/app/page.tsx` is loaded
Then at least one named export from `@open-sharia-enterprise/web-ui` is imported and rendered

Given `apps/ose-app-web/project.json` is loaded
Then `implicitDependencies` lists `web-ui` and `web-ui-token`

When I run `npx nx graph --file=/tmp/graph.json && jq '.graph.dependencies["ose-app-web"][].target' /tmp/graph.json`
Then the output contains both `web-ui` and `web-ui-token`
```

## Product Scope

### In Scope

- Create `apps/ose-app-web`, `apps/ose-app-be`, `apps/ose-app-web-e2e`, `apps/ose-app-be-e2e` directories with full skeleton.
- Create `specs/apps/ose-app/` mirror of organiclever's spec tree (behavior, components, containers, ddd, product, system-context).
- Create `specs/apps/ose-app/containers/contracts/` with health endpoint OpenAPI 3.1 + `ose-app-contracts` Nx project.
- Create `infra/dev/ose-app/` with docker-compose, Dockerfiles, env.example.
- Create `.github/workflows/test-and-deploy-ose-app-web-development.yml`. _[Judgment call]_ — staging/prod workflows _(`test-ose-app-web-staging.yml`, `deploy-ose-app-web-to-production.yml`)_ are scaffolded with stub steps so the file naming convention is established; activation deferred until first Vercel link.
- Update `AGENTS.md` — apps catalog, structure tree.
- Update `plans/in-progress/README.md` to add this plan entry.
- Register `global.json` + `dotnet-tools.json` for ose-app-be in doctor's polyglot detection (.NET 10 path).

### Out of Scope

- Document upload, parsing, OCR, or vector embedding logic
- LLM prompt design or evaluation
- Authentication, RBAC, audit logging
- Vercel project linking, domain registration, DNS
- Kubernetes manifests, Helm charts, production-grade Postgres
- Marketing site content on `oseplatform-web`

## Product Risks

| Risk                                                                                                                    | Severity | Mitigation                                                                                                                                                 |
| ----------------------------------------------------------------------------------------------------------------------- | -------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Engineers building first GRC feature pick wrong DDD context for a new aggregate.                                        | Medium   | Each bounded context's `ubiquitous-language/<name>.md` stub spells out the context's responsibility one paragraph long. Future plans extend, not redefine. |
| ose-app-web ends up with PGlite leftover from organiclever-web copy, creating dead code.                                | Medium   | `tech-docs.md §FE Scaffold` explicitly forbids PGlite. delivery.md verifies via `grep -r '@electric-sql/pglite' apps/ose-app-web` returns nothing.         |
| Bootstrap codegen for F# uses a stale `openapi-generator-cli -g fsharp-giraffe-server` template, blocking feature work. | Medium   | Mirror `ose-primer/apps/crud-be-fsharp-giraffe/project.json` codegen command verbatim; record divergence under [Judgment call] if needed.                  |
| OpenRouter API contract changes by feature-work time, making bootstrap env var names obsolete.                          | Low      | `.env.example` documents env vars as **placeholders, subject to feature-plan refinement**. Bootstrap commits to **placement**, not final interface.        |

## Naming Decision: BE E2E Project

User listed the four projects as `ose-app-web`, `ose-app-web-e2e`, `ose-app-be`, `ose-app-e2e`. This plan implements the BE e2e project as `ose-app-be-e2e` for two reasons:

1. **Consistency** — `organiclever-be-e2e` and `crud-be-e2e` (ose-primer) use the `-be-e2e` suffix; a bare `-e2e` would be the only outlier across the monorepo.
2. **Disambiguation** — without `-be-`, "is this the FE or BE e2e?" requires reading the project's source to answer.

If user prefers `ose-app-e2e` (single combined e2e project covering both FE and BE), the cost is significant: `infra/dev/ose-app/docker-compose.yml` needs to spin both up, Playwright config needs both base URLs, spec-coverage tooling needs adjustment. Renaming `-be-e2e` to `-e2e` (BE-only, just a rename) is mechanical — flag in feedback before delivery starts and the rename takes one `git mv` + 6–8 string substitutions.
