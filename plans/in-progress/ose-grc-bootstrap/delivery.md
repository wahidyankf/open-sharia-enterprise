# Delivery Checklist — ose-grc Bootstrap

All phases follow Red → Green → Refactor where code is being authored. Run `npx nx affected -t typecheck lint test:quick spec-coverage` at the end of every phase. Fix ALL failures found, including preexisting issues not caused by this plan (root cause orientation).

> **Reference Materials** (read-only):
>
> - F# pattern: `ose-primer/apps/crud-be-fsharp-giraffe/`
> - F# minimal baseline (prior `organiclever-be`): commit `c5a7058c8^` (see `git show c5a7058c8^:apps/organiclever-be/...`)
> - TS pattern: `apps/organiclever-web/`
> - E2E patterns: `apps/organiclever-web-e2e/`, `apps/organiclever-be-e2e/`
> - Contracts pattern: `specs/apps/organiclever/containers/contracts/`
> - Shared UI lib: `libs/web-ui/`, `libs/web-ui-token/`

## Worktree

Worktree path: `worktrees/ose-grc-bootstrap/`

Provision before execution (run from repo root):

```bash
claude --worktree ose-grc-bootstrap
```

See [Worktree Path Convention](../../../repo-governance/conventions/structure/worktree-path.md) and [Plans Organization Convention §Worktree Specification](../../../repo-governance/conventions/structure/plans.md#worktree-specification).

---

## Phase 0 — Environment Setup

- [ ] Provision worktree from repo root: `claude --worktree ose-grc-bootstrap` — verify `worktrees/ose-grc-bootstrap/` exists and is on branch `worktree/ose-grc-bootstrap`.

- [ ] Initialize the toolchain **in the root worktree** (`cd "$(git rev-parse --show-toplevel)"`), NOT inside the newly created subordinate worktree: run `npm install && npm run doctor -- --fix`. Verify `dotnet --version` reports 10.x and `npx nx --version` succeeds. (Doctor's polyglot lane will not yet provision ose-grc-be specifically — that happens once Phase 3 lands `global.json` AND Phase 3's rhino-cli edit registers `apps/ose-grc-be/global.json` in `tools.go`; rerun doctor after Phase 3 completes.)

- [ ] Confirm dev server starts for an existing app as smoke test: `npx nx dev organiclever-web` opens at `http://localhost:3200`. Ctrl-C to stop.

- [ ] Confirm existing tests pass: `npx nx run organiclever-web:test:quick` exits 0.

---

## Phase 1 — Contracts (`specs/apps/ose-grc/containers/contracts/`)

The contracts project is the contract source from which BE/FE codegen runs. Build it first so downstream Nx targets resolve correctly.

- [ ] Create directory tree:
      `specs/apps/ose-grc/containers/contracts/{paths,schemas,generated}` and `specs/apps/ose-grc/containers/contracts/README.md` mirroring `specs/apps/organiclever/containers/contracts/README.md`.
  - _Suggested executor: `specs-maker`_

- [ ] Author `specs/apps/ose-grc/containers/contracts/openapi.yaml` mirroring `specs/apps/organiclever/containers/contracts/openapi.yaml`. Replace title/description with OSE GRC; keep version `1.0.0`; set `servers[0].url` to `http://localhost:8302`. Reference exactly one path (`/api/v1/health`) and two schemas (`ErrorResponse`, `HealthResponse`).

- [ ] Create `specs/apps/ose-grc/containers/contracts/paths/health.yaml` and `schemas/{error.yaml,health.yaml}` by literal copy from `organiclever/containers/contracts/{paths,schemas}/` — these health/error schemas are domain-agnostic and reusable. Verify by `diff specs/apps/ose-grc/containers/contracts/schemas/health.yaml specs/apps/organiclever/containers/contracts/schemas/health.yaml` — output empty.

- [ ] Create `specs/apps/ose-grc/containers/contracts/.spectral.yaml` by literal copy from `organiclever/containers/contracts/.spectral.yaml`. Verify by `diff`.

- [ ] Create `specs/apps/ose-grc/containers/contracts/project.json` with `name: "ose-grc-contracts"`, mirroring `organiclever-contracts/project.json` with every path substituted from `organiclever` → `ose-grc`. Targets: `lint`, `bundle`, `docs`.

- [ ] Verify: `npx nx run ose-grc-contracts:bundle` — exits 0, produces `specs/apps/ose-grc/containers/contracts/generated/openapi-bundled.{yaml,json}`.

- [ ] Verify: `npx nx run ose-grc-contracts:lint` — exits 0 with no Spectral violations.

---

## Phase 2 — DDD + BDD spec tree (`specs/apps/ose-grc/`)

Sibling folders to `containers/`. Establishes the DDD bounded-contexts surface before any code references it.

- [ ] Create `specs/apps/ose-grc/README.md` by copying `specs/apps/organiclever/README.md` and substituting product name + bounded context list. Section headings should match.

- [ ] Create `specs/apps/ose-grc/ddd/{README.md,bounded-context-map.md,bounded-contexts.yaml}` and `specs/apps/ose-grc/ddd/ubiquitous-language/{README.md,regulatory-source.md,internal-policy.md,gap-analysis.md,ai-orchestration.md}`. Use `specs/apps/organiclever/ddd/` as the literal template; replace the four organiclever contexts with the four ose-grc contexts per `tech-docs.md §DD-5`.

- [ ] Write `bounded-contexts.yaml` content using the schema observed in `specs/apps/organiclever/ddd/bounded-contexts.yaml` (`version: 2`, `app: ose-grc`, `contexts: [...]`). Each context's `glossary` points to `specs/apps/ose-grc/ddd/ubiquitous-language/<context>.md` and `gherkin` points to `specs/apps/ose-grc/behavior/be/gherkin/<context>` or `specs/apps/ose-grc/behavior/web/gherkin/<context>` (use `be` for the four bootstrap contexts since data ownership is BE-side). `layers` per the table in `tech-docs.md §DD-5`.

- [ ] Each `ubiquitous-language/<context>.md` stub: ~15 lines: H1 title, "Responsibility" paragraph, "Canonical terms" table with 3-5 placeholder rows marked `_to be defined in feature plan_`, "Out of scope" bullet list.

- [ ] Verify: `CGO_ENABLED=0 go run -C apps/rhino-cli main.go ddd bc ose-grc` — exits 0 (validates `bounded-contexts.yaml` schema + presence).

- [ ] Verify: `CGO_ENABLED=0 go run -C apps/rhino-cli main.go ddd ul ose-grc` — exits 0 (validates ubiquitous-language file presence + 1:1 mapping to YAML contexts).

- [ ] Create `specs/apps/ose-grc/behavior/README.md`, `specs/apps/ose-grc/behavior/be/gherkin/`, `specs/apps/ose-grc/behavior/web/gherkin/` directories. README mirrors `specs/apps/organiclever/behavior/README.md`.

- [ ] Author `specs/apps/ose-grc/behavior/be/gherkin/health.feature` with **one** scenario:

  ```gherkin
  Feature: BE health endpoint
    As a system operator
    I want the BE to advertise liveness
    So that orchestrators can route traffic only to healthy instances

    Scenario: Health endpoint returns 200
      Given the ose-grc-be service is running
      When I send GET /api/v1/health
      Then the response status is 200
      And the response body has a "status" field equal to "healthy"
  ```

- [ ] Author `specs/apps/ose-grc/behavior/web/gherkin/smoke.feature` with **one** scenario:

  ```gherkin
  Feature: FE smoke load
    Scenario: Home page loads
      Given the ose-grc-web dev server is running
      When I navigate to "/"
      Then I see the heading "OSE GRC"
  ```

- [ ] Create the remaining stub folders so the tree mirrors organiclever's: `specs/apps/ose-grc/components/{README.md,.gitkeep}`, `specs/apps/ose-grc/product/{README.md,.gitkeep}`, `specs/apps/ose-grc/system-context/{README.md,.gitkeep}`. READMEs are one-liner placeholders pointing to future feature plans.

- [ ] Create `specs/apps/ose-grc/containers/{container.md,deployment.md}` — Mermaid stubs mirroring organiclever's docs. The C4 container diagram should show `ose-grc-web ↔ ose-grc-be ↔ postgres` and an outbound arrow `ose-grc-be → OpenRouter`.

- [ ] Verify spec tree shape matches organiclever's (excluding domain-specific files):
      `diff <(find specs/apps/ose-grc -type d | sed 's|specs/apps/ose-grc||' | sort) <(find specs/apps/organiclever -type d | sed 's|specs/apps/organiclever||' | sort)`
      Output empty.

---

## Phase 3 — ose-grc-be scaffold (F#/Giraffe)

Patterns mirror `ose-primer/apps/crud-be-fsharp-giraffe/` for project shape, packaging, and dotnet tooling. The pre-migration `apps/organiclever-be/` F# baseline (commit `c5a7058c8^`) is the minimal `Program.fs` reference.

- [ ] Create `apps/ose-grc-be/` directory tree per `tech-docs.md §apps/ose-grc-be/ structure`:
      `src/OseGrcBe/{Contracts,Domain,Infrastructure,Handlers}` and
      `tests/OseGrcBe.Tests/{Unit}` and
      `db/migrations/` (with `.gitkeep`).
  - _Suggested executor: `swe-fsharp-dev`_

- [ ] Author `apps/ose-grc-be/global.json` (literal copy from `ose-primer/apps/crud-be-fsharp-giraffe/global.json`; verify SDK pin is `10.x`).

- [ ] Author `apps/ose-grc-be/dotnet-tools.json` by literal copy from `ose-primer/apps/crud-be-fsharp-giraffe/dotnet-tools.json`. Verify the copied file declares `altcover.global` and `fsharp-analyzers` (the verified set per `cat ose-primer/apps/crud-be-fsharp-giraffe/dotnet-tools.json` — `[Repo-grounded]`). `fantomas` and `fsharplint` are NOT in this manifest by design — they are provisioned globally by the doctor step below.

- [ ] Author `apps/ose-grc-be/fsharplint.json` (literal copy from ose-primer crud-be-fsharp-giraffe).

- [ ] Author `apps/ose-grc-be/.editorconfig` (literal copy from ose-primer crud-be-fsharp-giraffe).

- [ ] Author `apps/ose-grc-be/.gitignore` — ignore `bin/`, `obj/`, `generated-contracts/`, `coverage/`, `.env`, `appsettings.Development.local.json`.

- [ ] Author `apps/ose-grc-be/.env.example` with placeholders:

  ```dotenv
  # OpenRouter (see tech-docs.md §DD-10) — placeholders only; never commit a real key
  OPENROUTER_API_KEY=
  OPENROUTER_MODEL=openrouter/auto
  OPENROUTER_BASE_URL=https://openrouter.ai/api/v1
  # PostgreSQL
  DATABASE_URL=postgresql://ose_grc:ose_grc@localhost:5432/ose_grc_dev
  ```

- [ ] Author `apps/ose-grc-be/src/OseGrcBe/OseGrcBe.fsproj` mirroring `ose-primer/apps/crud-be-fsharp-giraffe/src/DemoBeFsgi/DemoBeFsgi.fsproj` shape:
  - `Microsoft.NET.Sdk.Web`, `<TargetFramework>net10.0</TargetFramework>`, `<TreatWarningsAsErrors>true</TreatWarningsAsErrors>`, `<Nullable>enable</Nullable>`.
  - `<RootNamespace>OseGrcBe</RootNamespace>`, `<AssemblyName>OseGrcBe</AssemblyName>`.
  - Compile order: generated contracts (HealthResponse, ErrorResponse) → `Contracts/ContractWrappers.fs` → `Domain/Types.fs` → `Domain/RegulatorySource.fs` → `Domain/InternalPolicy.fs` → `Domain/GapAnalysis.fs` → `Domain/AiOrchestration.fs` → `Infrastructure/AppDbContext.fs` → `Infrastructure/Migrations.fs` → `Handlers/HealthHandler.fs` → `Program.fs`.
  - Package set per `tech-docs.md §Dependencies`: Giraffe 7, EFCore 10 + Npgsql.EntityFrameworkCore.PostgreSQL 10 + EFCore.NamingConventions 10, FSharp.SystemTextJson 1, dbup-core + dbup-postgresql 5, `G-Research.FSharp.Analyzers 0.*` + `FSharp.Analyzers.Build 0.*`.

- [ ] Author each F# source file with **minimal** content sufficient to compile + serve the health endpoint:
  - `Contracts/ContractWrappers.fs` — empty `module OseGrcBe.Contracts.Wrappers` (placeholder).
  - `Domain/Types.fs` — `module OseGrcBe.Domain.Types` with `type AppEnv = Dev | Staging | Prod` and `type AppError = | UnknownError of string`.
  - `Domain/{RegulatorySource,InternalPolicy,GapAnalysis}.fs` — each is an empty module with a doc comment naming the context.
  - `Domain/AiOrchestration.fs` — `type OpenRouterSettings = { ApiKey: string; Model: string; BaseUrl: string }` plus `module OseGrcBe.Domain.AiOrchestration` doc comment.
  - `Infrastructure/AppDbContext.fs` — EF Core `DbContext` class with no `DbSet<>` declarations; constructor takes `DbContextOptions<AppDbContext>`.
  - `Infrastructure/Migrations.fs` — DbUp engine factory: `let upgrade (connectionString: string) = DeployChanges.To.PostgresqlDatabase(connectionString).WithScriptsEmbeddedInAssembly(...)` — bootstrap version logs "no migrations yet" if `db/migrations/` is empty.
  - `Handlers/HealthHandler.fs` — Giraffe handler returning `{ status = "healthy" }` matching the OpenAPI `HealthResponse` schema.
  - `Program.fs` — Generic-host bootstrap: Giraffe pipeline with `route "/api/v1/health" >=> HealthHandler.handle`; options binding for `OpenRouter` config section; EF Core `AddDbContext` reading `DATABASE_URL`; DbUp runs at startup.

- [ ] Author `apps/ose-grc-be/tests/OseGrcBe.Tests/OseGrcBe.Tests.fsproj` mirroring ose-primer; packages: `xunit`, `xunit.runner.visualstudio`, `TickSpec`, `Microsoft.AspNetCore.Mvc.Testing`, `AltCover`. ProjectReference to `../../src/OseGrcBe/OseGrcBe.fsproj`.

- [ ] Author `apps/ose-grc-be/tests/OseGrcBe.Tests/{State.fs,TestFixture.fs,Unit/HealthSteps.fs,Unit/HealthTests.fs}` — minimal scaffold:
  - `TestFixture.fs` — `WebApplicationFactory<OseGrcBe.Program.Program>` wrapper with an in-memory or testcontainer-less Postgres-free profile (use `--in-memory` mode by overriding `AppDbContext` registration in the fixture).
  - `Unit/HealthTests.fs` — single `[<Fact>] [<Trait("Category","Unit")>] let ``Health endpoint returns 200 with healthy body`` () = ...` exercising `HealthHandler` directly.
  - `Unit/HealthSteps.fs` — TickSpec step bindings consuming `specs/apps/ose-grc/behavior/be/gherkin/health.feature`. Use `[<Given>]`, `[<When>]`, `[<Then>]` attributes; mutate `State` record.
  - `State.fs` — mutable state record shared across TickSpec steps (`statusCode: int`, `responseBody: string`).

- [ ] Author `apps/ose-grc-be/project.json` mirroring the prior-organiclever-be F# project.json (commit `c5a7058c8^`) with the following substitutions: `organiclever` → `ose-grc`, `OrganicLeverBe` → `OseGrcBe`. Replace `domain:organiclever` with `domain:ose-grc`. Targets: `codegen`, `build`, `dev`, `start`, `test:quick`, `test:unit`, `test:integration`, `lint`, `typecheck`, `spec-coverage`. Tags: `type:app`, `platform:giraffe`, `lang:fsharp`, `domain:ose-grc`. Implicit deps: `ose-grc-contracts`, `rhino-cli`.

- [ ] Author `apps/ose-grc-be/docker-compose.integration.yml` and `apps/ose-grc-be/Dockerfile.integration` by literal copy from `ose-primer/apps/crud-be-fsharp-giraffe/`, substituting names (`crud_be_fastapi` → `ose_grc`, `DemoBeFsgi` → `OseGrcBe`, `crud` → `ose-grc`).

- [ ] Author `apps/ose-grc-be/README.md` mirroring `apps/organiclever-be/README.md`: Quick Start, Commands table, Prerequisites (.NET 10), Env Vars, Tech Stack, Behavior & Architecture, Related links.

- [ ] Edit five rhino-cli files to replace every `apps/a-demo-be-fsharp-giraffe/global.json` reference with `apps/ose-grc-be/global.json` (`[Repo-grounded]` — locations verified via `grep -rn 'a-demo-be-fsharp-giraffe' apps/rhino-cli/`):
  - `apps/rhino-cli/internal/doctor/tools.go` line 37: `filepath.Join(repoRoot, "apps", "a-demo-be-fsharp-giraffe", "global.json")` → `filepath.Join(repoRoot, "apps", "ose-grc-be", "global.json")`
  - `apps/rhino-cli/internal/doctor/tools.go` line 222: `source: "apps/a-demo-be-fsharp-giraffe/global.json → sdk.version"` → `source: "apps/ose-grc-be/global.json → sdk.version"`
  - `apps/rhino-cli/cmd/doctor.go` line 37 (help-text): `apps/a-demo-be-fsharp-giraffe/global.json → sdk.version` → `apps/ose-grc-be/global.json → sdk.version`
  - `apps/rhino-cli/cmd/doctor.integration_test.go` lines 134 + 151: replace both `"apps/a-demo-be-fsharp-giraffe"` and `"apps/a-demo-be-fsharp-giraffe/global.json"` keys with `"apps/ose-grc-be"` and `"apps/ose-grc-be/global.json"`
  - `apps/rhino-cli/internal/doctor/checker_test.go` lines 637 + 652: same replacement pattern as `doctor.integration_test.go`
  - `apps/rhino-cli/internal/doctor/reporter_test.go` line 42: `Source: "apps/a-demo-be-fsharp-giraffe/global.json → sdk.version"` → `Source: "apps/ose-grc-be/global.json → sdk.version"`
  - _Suggested executor: `swe-golang-dev`_

- [ ] Verify all dead-path references are gone: `grep -rn 'a-demo-be-fsharp-giraffe' apps/rhino-cli/` returns ZERO hits (the verification confirms all six lines listed in the previous step were updated; line counts may shift after the edit, so use `grep` not absolute lines).

- [ ] Run `npx nx run rhino-cli:test:quick` — exits 0 with ≥ 90 % coverage (matches the existing rhino-cli threshold). Confirms the rhino-cli change in the previous steps did not regress unit, integration, reporter, or checker tests.

- [ ] If `fantomas` is not on `PATH`, install it globally: `dotnet tool install --global fantomas`. Verify with `fantomas --version`.

- [ ] If `dotnet-fsharplint` is not on `PATH`, install it globally: `dotnet tool install --global dotnet-fsharplint`. Verify with `dotnet fsharplint --version`. _[Judgment call]: ose-primer's lint target invokes `dotnet fsharplint` assuming it is available either as a global tool or via `DOTNET_ROLL_FORWARD=LatestMajor` resolution; the simplest portable choice is a global install._

- [ ] Re-run doctor to verify the dotnet stack is now detected: from root worktree (`cd "$(git rev-parse --show-toplevel)" && npm run doctor -- --fix`) — verify the dotnet/F# section reports green for `apps/ose-grc-be` and that doctor logs the SDK version pinned by the new `global.json`.

- [ ] **RED** — Run `npx nx run ose-grc-be:test:unit` — expect failure (no contract types compiled, no DbUp script, but xUnit infrastructure should compile).

- [ ] **GREEN** — Run `npx nx run ose-grc-contracts:bundle && npx nx run ose-grc-be:codegen` — produces `apps/ose-grc-be/generated-contracts/`. Re-run `npx nx run ose-grc-be:test:unit` — passes.

- [ ] **GREEN** — Run `npx nx run ose-grc-be:test:quick` — passes AND `apps/ose-grc-be/coverage/altcov.info` reports ≥ 90%.

- [ ] **GREEN** — Run `npx nx run ose-grc-be:typecheck` — passes (exits 0, no warnings escalated to errors).

- [ ] **GREEN** — Run `npx nx run ose-grc-be:lint` — passes (fantomas check, fsharplint, G-Research analyzers all clean).

- [ ] **Manual** — `npx nx dev ose-grc-be` and `curl -sf http://localhost:8302/api/v1/health` returns `{"status":"healthy"}`. Stop the server (Ctrl-C).

---

## Phase 4 — ose-grc-web scaffold (Next.js 16)

Patterns mirror `apps/organiclever-web/` exactly except: no PGlite, no `gen-migrations.mjs`.

- [ ] Create `apps/ose-grc-web/` directory tree per `tech-docs.md §apps/ose-grc-web/ structure`. Start with literal copy of `apps/organiclever-web/` and then delete PGlite-specific files (`scripts/gen-migrations.mjs`, `src/shared/pglite-*`, `migrations/`).
  - _Suggested executor: `swe-typescript-dev`_

- [ ] Edit `apps/ose-grc-web/package.json`:
  - `name: "ose-grc-web"`.
  - Strip PGlite dependencies (`@electric-sql/pglite`, `@electric-sql/pglite-react`, etc.).
  - Keep `"@open-sharia-enterprise/web-ui": "*"` and `"@open-sharia-enterprise/web-ui-token": "*"` (DD-3).
  - Keep tRPC, Next.js, React 19, Vitest, Storybook deps verbatim.

- [ ] Edit `apps/ose-grc-web/next.config.ts`: keep `transpilePackages: ["@open-sharia-enterprise/web-ui", "@open-sharia-enterprise/web-ui-token"]`. Set Next.js port via the Nx `dev`/`build` targets (port 3300).

- [ ] Edit `apps/ose-grc-web/src/app/globals.css`: import `@open-sharia-enterprise/web-ui-token/src/tokens.css` (base tokens). Remove any `organiclever.css` import. Verify: `grep -F 'organiclever.css' apps/ose-grc-web/src/app/globals.css` returns nothing.

- [ ] Edit `apps/ose-grc-web/src/app/page.tsx`:
  - Render an `<h1>OSE GRC</h1>` and at least one `<Button>` imported from `@open-sharia-enterprise/web-ui` (DD-3, AC-11). Body text: "Governance, Risk, and Compliance — bootstrap scaffold."

- [ ] Edit `apps/ose-grc-web/src/app/layout.tsx`: import `./globals.css`; set `<html lang="en">`. Strip any OrganicLever-specific provider wrappers.

- [ ] Strip OrganicLever-specific contexts: replace `src/contexts/{journal,workout-session,routine,stats,app-shell}` with `src/contexts/{regulatory-source,internal-policy,gap-analysis,ai-orchestration}` — each is a folder with a `README.md` placeholder pointing to the matching `specs/apps/ose-grc/ddd/ubiquitous-language/<context>.md`.

- [ ] Edit `apps/ose-grc-web/project.json`:
  - `name: "ose-grc-web"`.
  - `sourceRoot: "apps/ose-grc-web/src"`.
  - Replace every `organiclever` → `ose-grc` substring.
  - Remove the `gen-migrations.mjs` invocations from `dev` and `build` targets.
  - `dev` command: `next dev --port 3300`. `start` command: `next start --port 3300`.
  - `tags: ["type:app", "platform:nextjs", "lang:ts", "domain:ose-grc"]`.
  - `implicitDependencies: ["ose-grc-contracts", "rhino-cli", "web-ui", "web-ui-token"]`.

- [ ] Author `apps/ose-grc-web/Dockerfile`, `.dockerignore`, `eslint.config.mjs`, `oxlint.json`, `postcss.config.mjs`, `tsconfig.json`, `vitest.config.ts` by adapting from `apps/organiclever-web/` (substitute names; keep all rules).

- [ ] Author `apps/ose-grc-web/README.md` mirroring `apps/organiclever-web/README.md`: Quick Start, Commands, Env Vars, Project Layout, Tech Stack, Related.

- [ ] **Verification** — PGlite removed:
      `grep -rl '@electric-sql/pglite' apps/ose-grc-web/` returns no matches.

- [ ] **Verification** — web-ui wired:
      `grep -F '@open-sharia-enterprise/web-ui' apps/ose-grc-web/package.json apps/ose-grc-web/next.config.ts apps/ose-grc-web/src/app/page.tsx` returns ≥ 1 hit per file.

- [ ] **Verification** — Nx graph edge present:
      `npx nx graph --file=/tmp/graph.json && jq '.graph.dependencies["ose-grc-web"][].target' /tmp/graph.json | sort -u` includes `web-ui` and `web-ui-token` (AC-11).

- [ ] **RED** — Create `apps/ose-grc-web/src/app/page.test.tsx` BEFORE editing `page.tsx`. The test imports `Home` from `./page` and asserts: (a) renders `<h1>OSE GRC</h1>`, (b) renders at least one element matching `screen.getByRole('button')`. Run `npx nx run ose-grc-web:test:unit` — the test should FAIL with "Cannot find module './page'" (since `page.tsx` isn't authored yet — or fail with assertion errors if a placeholder copy from organiclever-web remains). Capture the failure output.

- [ ] Run `cd "$(git rev-parse --show-toplevel)" && npm install` so npm picks up the new workspace (`apps/ose-grc-web/package.json`). Verify with `ls node_modules/@open-sharia-enterprise/web-ui` — directory exists and resolves to the workspace symlink (not a published version).

- [ ] **GREEN** — `npx nx run ose-grc-contracts:bundle && npx nx run ose-grc-web:codegen` produces `apps/ose-grc-web/src/generated-contracts/`. Verify the directory contains TS types for `HealthResponse` and `ErrorResponse`.

- [ ] **GREEN** — `npx nx run ose-grc-web:typecheck` exits 0.

- [ ] **GREEN** — `npx nx run ose-grc-web:lint` exits 0.

- [ ] **GREEN** — `npx nx run ose-grc-web:test:unit` — the previously RED `page.test.tsx` now passes (heading + button assertions). Capture the green output to confirm closure of the Red→Green cycle.

- [ ] **GREEN** — `npx nx run ose-grc-web:test:quick` exits 0 with coverage ≥ 70 % (per US-4 `[Judgment call]`). _If the scaffold doesn't reach 70 % with only a smoke page, add 1-2 trivial unit tests on the page component — DO NOT lower the threshold._

- [ ] **Manual UI Verification** (Playwright MCP) — `npx nx dev ose-grc-web`:
  - `browser_navigate http://localhost:3300` returns 200.
  - `browser_snapshot` shows `<h1>OSE GRC</h1>` and a `<button>` rendered.
  - `browser_console_messages` returns zero error-level entries.

---

## Phase 5 — ose-grc-be-e2e and ose-grc-web-e2e scaffolds (Playwright-BDD)

- [ ] Create `apps/ose-grc-web-e2e/` by literal copy from `apps/organiclever-web-e2e/`. Substitute `organiclever` → `ose-grc`, port `3200` → `3300`. Strip any OrganicLever-specific step files; retain `steps/` folder with one `smoke.steps.ts` referencing `specs/apps/ose-grc/behavior/web/gherkin/smoke.feature`.
  - _Suggested executor: `swe-e2e-dev`_

- [ ] Create `apps/ose-grc-be-e2e/` by literal copy from `apps/organiclever-be-e2e/`. Substitute `organiclever` → `ose-grc`, port `8202` → `8302`. Strip OrganicLever-specific step files; retain `steps/` folder with one `health.steps.ts` referencing `specs/apps/ose-grc/behavior/be/gherkin/health.feature`.
  - _Suggested executor: `swe-e2e-dev`_

- [ ] Update both `playwright.config.ts` files: `WEB_BASE_URL=http://localhost:3300` (web-e2e) and `BASE_URL=http://localhost:8302` (be-e2e).

- [ ] Update both `project.json` files to use `tags: ["type:e2e", "platform:playwright", "lang:ts", "domain:ose-grc"]` and `implicitDependencies` pointing at the correct ose-grc projects.

- [ ] **RED** — Before authoring step bindings, run `npx nx run-many -t spec-coverage --projects=ose-grc-web-e2e,ose-grc-be-e2e` from `"$(git rev-parse --show-toplevel)"`. Expect FAILURE reporting < 100 % step coverage for `smoke.feature` and `health.feature` (because the literal-copy step files reference OrganicLever-domain Gherkin steps, not ose-grc ones). Capture the failure output.

- [ ] **GREEN** — Author the ose-grc-specific step bindings:
  - `apps/ose-grc-web-e2e/steps/smoke.steps.ts` — implements steps `Given the ose-grc-web dev server is running`, `When I navigate to "/"`, `Then I see the heading "OSE GRC"` per `specs/apps/ose-grc/behavior/web/gherkin/smoke.feature` (Phase 2).
  - `apps/ose-grc-be-e2e/steps/health.steps.ts` — implements steps `Given the ose-grc-be service is running`, `When I send GET /api/v1/health`, `Then the response status is 200`, `And the response body has a "status" field equal to "healthy"` per `specs/apps/ose-grc/behavior/be/gherkin/health.feature` (Phase 2).

- [ ] **GREEN** — Verify by running typecheck on both: `npx nx run-many -t typecheck --projects=ose-grc-web-e2e,ose-grc-be-e2e` — exits 0.

- [ ] **GREEN** — Run spec-coverage on both: `npx nx run-many -t spec-coverage --projects=ose-grc-web-e2e,ose-grc-be-e2e` — exits 0 with 100 % step coverage for `smoke.feature` and `health.feature` (closes the Red→Green cycle from the RED step above).

- [ ] Author `apps/ose-grc-web-e2e/README.md` and `apps/ose-grc-be-e2e/README.md` mirroring organiclever e2e READMEs.

---

## Phase 6 — infra/dev/ose-grc/ (docker-compose stack)

- [ ] Create `infra/dev/ose-grc/` directory tree by literal copy from `infra/dev/organiclever/`. Substitute names + ports throughout.

- [ ] Edit `infra/dev/ose-grc/docker-compose.yml`:
  - Service `postgres`: `postgres:17-alpine`, env `POSTGRES_DB=ose_grc_dev`, healthcheck `pg_isready -U ose_grc -d ose_grc_dev`.
  - Service `ose-grc-be`: builds via `Dockerfile.be.dev`, ports `8302:8302`, depends on `postgres` healthy, env `DATABASE_URL` + OpenRouter placeholders, mounts `../../../apps/ose-grc-be:/workspace:rw` and `../../../specs/apps/ose-grc/behavior/be/gherkin:/specs/apps/ose-grc/behavior/be/gherkin:ro`, healthcheck `curl -f http://localhost:8302/api/v1/health`.
  - Service `ose-grc-web`: builds via `apps/ose-grc-web/Dockerfile`, ports `3300:3300`, env `OSE_GRC_BE_URL=http://ose-grc-be:8302`, depends on `ose-grc-be` healthy.

- [ ] Edit `infra/dev/ose-grc/Dockerfile.be.dev` — base image `mcr.microsoft.com/dotnet/sdk:10.0-alpine`, install `npm` for nx; entrypoint runs `dotnet watch run --project src/OseGrcBe/OseGrcBe.fsproj`.

- [ ] Edit `infra/dev/ose-grc/Dockerfile.fe.dev` mirroring `infra/dev/organiclever/Dockerfile.fe.dev`.

- [ ] Edit `infra/dev/ose-grc/.env.example` with all placeholders documented (DATABASE*URL, OPENROUTER*\*).

- [ ] Edit `infra/dev/ose-grc/docker-compose.ci.yml` mirroring `infra/dev/organiclever/docker-compose.ci.yml` (CI override for prebuilt images if used).

- [ ] Edit `infra/dev/ose-grc/README.md` mirroring `infra/dev/organiclever/README.md`.

- [ ] **Manual verification** (AC-7):
      `docker compose -f infra/dev/ose-grc/docker-compose.yml down -v && docker compose -f infra/dev/ose-grc/docker-compose.yml up --build -d` - Within 60 s: `docker compose -f infra/dev/ose-grc/docker-compose.yml ps` shows `postgres` healthy. - Within 120 s: `curl -sf http://localhost:8302/api/v1/health` returns 200. - Within 120 s: `curl -sf http://localhost:3300` returns 200. - Teardown: `docker compose -f infra/dev/ose-grc/docker-compose.yml down -v` succeeds.

---

## Phase 7 — CI workflows

The PR quality gate (`.github/workflows/pr-quality-gate.yml`) already routes `lang:fsharp` → `has-dotnet` and `lang:ts` → `has-ts`, so no edit is required there (DD-7). Per-product workflows are net new.

- [ ] Create `.github/workflows/test-and-deploy-ose-grc-web-development.yml` by literal copy of `test-and-deploy-organiclever-web-development.yml`. Substitute `organiclever` → `ose-grc` and matching port references. Confirm jobs: `spec-coverage`, `fe-lint`, `be-integration`, `fe-integration`, `e2e`, `specs-gate`. The `deploy` job's force-push target becomes `stag-ose-grc-web` — leave it in place; first run will simply create the branch.
  - _Suggested executor: `repo-workflow-maker`_

- [ ] Create `.github/workflows/test-ose-grc-web-staging.yml` by literal copy of `test-organiclever-web-staging.yml`, substituting names. Trigger: `workflow_dispatch` only (no `schedule:`) until staging Vercel project exists.

- [ ] Create `.github/workflows/deploy-ose-grc-web-to-production.yml` by literal copy of `deploy-organiclever-web-to-production.yml`, substituting names. Trigger: `workflow_dispatch` only.

- [ ] Open each new workflow YAML and verify the `runs-on:` is `ubuntu-latest` (matches ose-public convention; ose-infra uses self-hosted).

- [ ] Locally lint the new workflow files (yamllint isn't required; just visually verify against the source files). Run `git diff --stat .github/workflows/` and confirm exactly three new files staged.

- [ ] **Manual smoke** — AC-8 (PR quality gate routes `lang:fsharp` → `has-dotnet=true`) is verified during the post-push CI run in Phase 9, **not** here. This repo uses Trunk Based Development (direct push to `main` per [`repo-practicing-trunk-based-development`](../../../repo-governance/development/workflow/trunk-based-development.md)); the PR-quality-gate workflow only triggers `on: pull_request`. Verification path: after Phase 9 push, run `gh workflow view pr-quality-gate.yml --ref main` to confirm the workflow exists and is callable; PR-triggered execution will land naturally when the first feature plan opens a PR. _If a PR is desired for this bootstrap itself, run `gh pr create --draft` from the worktree branch before merging; AC-8 then becomes observable in the PR-run logs._

---

## Phase 8 — AGENTS.md and plans/in-progress/README.md updates

- [ ] Edit `AGENTS.md`:
  - **Current Apps** list: append four entries for `ose-grc-web`, `ose-grc-be`, `ose-grc-web-e2e`, `ose-grc-be-e2e` — each one bullet line in the same shape as the organiclever entries.
  - **Project Structure** tree: insert four lines under `apps/` for the four new apps in alphabetical order, with the same indent and comment style.
  - **Web Sites** section: add an "ose-grc-web" subsection at the end (after `wahidyankf-web`) mirroring the organiclever-web subsection. Mark URL as "TBD (no Vercel project yet)".
  - _Suggested executor: `readme-maker`_

- [ ] Edit `plans/in-progress/README.md`: append a new bullet under "Active Plans" with the form:

  ```markdown
  - [ose-grc-bootstrap](./ose-grc-bootstrap/README.md) — Scaffold ose-grc-web, ose-grc-web-e2e, ose-grc-be, ose-grc-be-e2e (Next.js 16 FE + F#/Giraffe BE + Playwright-BDD E2E + DDD specs + CI workflows)
  ```

  The link resolves correctly when authored inside `plans/in-progress/README.md` itself.

- [ ] Verify: `grep -c 'ose-grc' AGENTS.md` returns ≥ 8 (apps catalog ×4 + project tree ×4 + web-sites section). `grep -c 'ose-grc-bootstrap' plans/in-progress/README.md` returns ≥ 1.

---

## Phase 9 — Final validation, push, and CI verification

### Local Quality Gates (Before Push)

- [ ] Run affected typecheck: `npx nx affected -t typecheck` — exits 0.
- [ ] Run affected linting: `npx nx affected -t lint` — exits 0.
- [ ] Run affected quick tests: `npx nx affected -t test:quick` — exits 0; ose-grc-be reports ≥ 90% coverage; ose-grc-web reports ≥ 70% coverage.
- [ ] Run affected spec coverage: `npx nx affected -t spec-coverage` — exits 0; reports 100% step coverage for `health.feature` and `smoke.feature`.
- [ ] Run repo-wide DDD validation: `CGO_ENABLED=0 go run -C apps/rhino-cli main.go ddd bc ose-grc && CGO_ENABLED=0 go run -C apps/rhino-cli main.go ddd ul ose-grc` — both exit 0.
- [ ] Run markdown lint: `npm run lint:md` — exits 0.
- [ ] **Fix ALL failures found, including preexisting issues not caused by this plan** (root cause orientation per AGENTS.md §Conventions).
- [ ] Verify no real OpenRouter key was committed: `grep -r 'sk-or-' apps/ose-grc-* infra/dev/ose-grc/ 2>/dev/null` returns nothing.

### Commit Guidelines

- [ ] Commit changes thematically. Suggested commit grouping:
  1. `feat(ose-grc-contracts): scaffold OpenAPI 3.1 contracts project with health endpoint`
  2. `feat(specs/ose-grc): add DDD bounded-contexts and ubiquitous-language stubs`
  3. `feat(specs/ose-grc): add BDD smoke features for BE health and FE smoke load`
  4. `feat(ose-grc-be): scaffold F#/Giraffe service with PostgreSQL + DbUp migrations`
  5. `feat(ose-grc-web): scaffold Next.js 16 frontend with web-ui integration`
  6. `feat(ose-grc-{be,web}-e2e): scaffold Playwright-BDD e2e suites`
  7. `feat(infra): add ose-grc dev docker-compose stack`
  8. `feat(ci): add ose-grc-web dev/staging/prod workflows`
  9. `docs(agents): catalog ose-grc apps and structure tree`
  10. `chore(plans): track ose-grc-bootstrap in in-progress`
- [ ] Each commit uses Conventional Commits format: `<type>(<scope>): <description>` per AGENTS.md §Git Workflow.
- [ ] No commit bundles unrelated changes.

### Publish

- [ ] Merge worktree branch into `main` via fast-forward (Trunk Based Development default per Standard 14 of subrepo worktree workflow). Push to `origin main`.

### Post-Push Verification

- [ ] Monitor GitHub Actions workflows for the push: `gh run list --limit 10 --branch main`.
- [ ] Verify `pr-quality-gate.yml` doesn't run (no PR — direct push to main); other on-push workflows run green.
- [ ] If any CI check fails: fix immediately and push a follow-up commit. Do NOT proceed to archival.

### Manual Behavioral Re-Verification (Full Stack)

- [ ] `docker compose -f infra/dev/ose-grc/docker-compose.yml up --build -d` from a clean state.
- [ ] `curl -sf http://localhost:8302/api/v1/health` returns 200 with `{"status":"healthy"}`.
- [ ] `curl -sf http://localhost:3300/` returns 200; `curl -s http://localhost:3300/ | grep -F 'OSE GRC'` returns at least one hit.
- [ ] `docker compose -f infra/dev/ose-grc/docker-compose.yml down -v` cleans up.

### Plan Archival

- [ ] Verify ALL delivery checklist items above are ticked.
- [ ] Verify ALL local + CI quality gates pass.
- [ ] Move plan folder: `git mv plans/in-progress/ose-grc-bootstrap plans/done/$(date +%Y-%m-%d)__ose-grc-bootstrap`.
- [ ] Update `plans/in-progress/README.md` — remove the ose-grc-bootstrap entry.
- [ ] Update `plans/done/README.md` — add the new entry with completion date.
- [ ] Update `README.md` of the plan folder itself with `**Status**: Completed` and the completion date.
- [ ] Commit: `chore(plans): move ose-grc-bootstrap to done`.
- [ ] Push to `origin main`.
