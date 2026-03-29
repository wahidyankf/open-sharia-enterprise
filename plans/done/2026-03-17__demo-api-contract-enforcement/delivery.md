# Delivery Plan

## Implementation Phases

### Phase 1: Contract Authoring + Infrastructure

**Goal**: Write the OpenAPI 3.1 modular spec, set up Spectral linting, bundling, and gitignore.

**Implementation Steps**:

- [x] Create `specs/apps/a-demo/contracts/` directory structure (paths/, schemas/, examples/)
- [x] Write `README.md` with purpose, usage guide, and contribution rules
- [x] Write root `openapi.yaml` with server config, security schemes, and `$ref` path mappings
- [x] Write all schema files:
  - [x] `schemas/auth.yaml` — LoginRequest, RegisterRequest, RefreshRequest, AuthTokens
  - [x] `schemas/user.yaml` — User, UpdateProfileRequest, ChangePasswordRequest
  - [x] `schemas/expense.yaml` — Expense, CreateExpenseRequest, UpdateExpenseRequest
  - [x] `schemas/expense-list.yaml` — ExpenseListResponse (uses pagination.yaml)
  - [x] `schemas/report.yaml` — PLReport, CategoryBreakdown, ExpenseSummary
  - [x] `schemas/attachment.yaml` — Attachment
  - [x] `schemas/token.yaml` — TokenClaims, JwksResponse, JwkKey
  - [x] `schemas/admin.yaml` — DisableRequest, PasswordResetResponse, UserListResponse
  - [x] `schemas/pagination.yaml` — reusable pagination envelope
  - [x] `schemas/error.yaml` — standardized error response
  - [x] `schemas/health.yaml` — HealthResponse
- [x] Write all path files:
  - [x] `paths/health.yaml` — GET /health
  - [x] `paths/auth.yaml` — POST login, register, refresh, logout, logout-all
  - [x] `paths/users.yaml` — GET/PATCH /me, POST password, POST deactivate
  - [x] `paths/expenses.yaml` — CRUD + summary
  - [x] `paths/attachments.yaml` — POST/GET/DELETE attachments
  - [x] `paths/reports.yaml` — GET /api/v1/reports/pl
  - [x] `paths/admin.yaml` — GET users, POST disable/enable/unlock/force-password-reset
  - [x] `paths/tokens.yaml` — GET claims, GET /.well-known/jwks.json
  - [x] `paths/test-support.yaml` — POST reset-db, POST promote-admin
- [x] Write `.spectral.yaml` with strict camelCase (zero exceptions), description, and example
      rules
- [x] Write `redocly.yaml` with docs theme config and `x-test-only` filtering
- [x] Add `a-demo-contracts` as Nx project (`project.json`) with `lint`, `bundle`, and `docs` targets
- [x] Add `**/generated-contracts/`, `**/generated_contracts/` (Python), and
      `specs/apps/a-demo/contracts/generated/` to root `.gitignore`
- [x] Verify Spectral lint passes with zero errors (includes camelCase enforcement)
- [x] Verify Redocly CLI bundle resolves all `$ref`s into `generated/openapi-bundled.yaml` and
      `generated/openapi-bundled.json` (JSON needed for ajv in E2E tests)
- [x] Verify Redocly CLI `build-docs` generates browsable HTML at `generated/docs/index.html`
- [x] Verify test-only endpoints (`/api/v1/test/*`) are excluded from generated docs
- [x] Write example files for major endpoints

**Validation**:

- `npx @stoplight/spectral-cli lint openapi.yaml` exits 0
- `npx @redocly/cli bundle openapi.yaml` produces valid bundled output (YAML + JSON)
- `npx @redocly/cli build-docs` produces browsable HTML documentation
- All endpoints from Gherkin specs are covered
- `.gitignore` patterns correctly exclude generated folders
- Generated docs show all public endpoints grouped by domain, with schemas and examples

---

### Phase 2: Code Generation for Statically Typed Apps

**Goal**: Set up `codegen` Nx targets producing types + encoders/decoders in `generated-contracts/`
for all statically typed apps. Wire `codegen` as dependency of `typecheck`/`build`/`test:unit`.

**Implementation Steps**:

- [x] **a-demo-be-golang-gin**: Install `oapi-codegen`, create config (`oapi-codegen.yaml` with
      strict-server + gin output), add `codegen` target, update handlers to implement generated
      strict server interface, verify `go build` passes
- [x] **a-demo-be-java-springboot**: Add `openapi-generator-maven-plugin` to `pom.xml`, configure
      Spring generator, add `codegen` target, update controllers to use generated DTOs with Jackson
      annotations, verify `mvn compile` passes
- [x] **a-demo-be-java-vertx**: Add `openapi-generator-maven-plugin`, configure Java generator, add
      `codegen` target, update handlers to use generated DTOs, verify `mvn compile` passes
- [x] **a-demo-be-kotlin-ktor**: Add `openapi-generator-gradle-plugin` to `build.gradle.kts`,
      configure Kotlin generator with kotlinx.serialization, add `codegen` target, update routes to
      use generated data classes, verify `./gradlew build` passes
- [x] **a-demo-be-rust-axum**: Add `openapi-generator` Rust generator via build script, add `codegen`
      target, update handlers to use generated serde structs, verify `cargo build` passes
- [x] **a-demo-be-fsharp-giraffe**: Add `openapi-generator` with `fsharp-giraffe-server` generator
      (beta), configure to generate F# model types only, add `codegen` target, update handlers to use
      generated types, verify `dotnet build` passes
- [x] **a-demo-be-csharp-aspnetcore**: Add `NSwag.MSBuild` NuGet package, configure C# class
      generation, add `codegen` target, update controllers to use generated classes, verify
      `dotnet build` passes
- [x] **a-demo-be-ts-effect**: Add `@hey-api/openapi-ts` dev dependency, add `codegen` target
      generating TS types + Effect Schema definitions, update handlers to use generated types with
      `Schema.decode`/`Schema.encode`, verify `tsc` passes
- [x] **a-demo-fe-ts-nextjs**: Add `@hey-api/openapi-ts` with Zod plugin, add `codegen` target
      generating TS types + Zod schemas + SDK client, replace hand-written `types.ts` with generated
      types, use generated Zod schemas as runtime decoders for API responses, verify `tsc` passes
- [x] **a-demo-fe-ts-tanstack-start**: Same as a-demo-fe-ts-nextjs — `@hey-api/openapi-ts` + Zod
      plugin, add `codegen` target, replace types, use Zod runtime decoders, verify `tsc` passes
- [x] **a-demo-fe-dart-flutterweb**: Add `openapi-generator` Dart generator, add `codegen` target,
      replace hand-written models with generated classes using `json_serializable`, verify
      `dart analyze` passes
- [x] Wire `codegen` as dependency of `typecheck`/`build`/`test:unit` in each app's `project.json`
- [x] Verify `nx run-many -t typecheck --projects=a-demo-fe-*` passes
- [x] Verify `nx run-many -t build --projects=a-demo-be-golang-gin,a-demo-be-rust-axum` passes

**Validation**:

- Each statically typed app's `generated-contracts/` is populated by `codegen`
- `nx affected -t typecheck` catches TS/Dart mismatches
- `nx affected -t build` catches Go/Java/Kotlin/Rust/F#/C# mismatches
- Intentionally breaking a field name in the contract and re-running codegen causes compile failure

---

### Phase 3: Codegen Libs + Dynamically Typed Apps

**Goal**: Create `libs/elixir-openapi-codegen` and `libs/clojure-openapi-codegen` as Nx library
projects with full test suites and coverage. Set up `codegen` Nx targets for Python, Elixir, and
Clojure. Enforcement via `test:unit` (part of `test:quick`).

**Implementation Steps — Codegen Libs**:

- [x] **libs/elixir-openapi-codegen**: Create Elixir Nx library project
  - [x] Set up `mix.exs` with `yaml_elixir` dependency
  - [x] Implement OpenAPI YAML parser (reads bundled spec, extracts component schemas)
  - [x] Implement Elixir struct generator (emits `defstruct` + `@enforce_keys` + `@type` typespecs)
  - [x] Add `project.json` with `build`, `test:unit`, `test:quick`, `lint` targets
  - [x] Write unit tests: given sample OpenAPI schemas, assert generated Elixir code has correct
        struct fields, enforce_keys, and typespecs
  - [x] Write integration tests: generate structs from the actual demo contract, compile them,
        verify they accept valid data and reject invalid data
  - [x] Enforce ≥90% line coverage via `rhino-cli test-coverage validate`
  - [x] Verify `nx run elixir-openapi-codegen:test:quick` passes
- [x] **libs/clojure-openapi-codegen**: Create Clojure Nx library project
  - [x] Set up `deps.edn` with `clj-yaml` dependency
  - [x] Implement OpenAPI YAML parser (reads bundled spec, extracts component schemas)
  - [x] Implement Malli schema generator (emits Malli `[:map ...]` definitions per schema)
  - [x] Add `project.json` with `build`, `test:unit`, `test:quick`, `lint` targets
  - [x] Write unit tests: given sample OpenAPI schemas, assert generated Malli schemas have correct
        keys, types, and required/optional markers
  - [x] Write integration tests: generate schemas from the actual demo contract, validate sample
        JSON payloads against generated schemas (accept valid, reject invalid)
  - [x] Enforce ≥90% line coverage via `rhino-cli test-coverage validate`
  - [x] Verify `nx run clojure-openapi-codegen:test:quick` passes

**Implementation Steps — App Integration**:

- [x] **a-demo-be-python-fastapi**: Add `datamodel-code-generator` dev dependency, add `codegen`
      target generating Pydantic v2 models into `generated_contracts/`, update FastAPI route handlers
      to use generated models as `response_model`, verify `pytest` passes
- [x] **a-demo-be-elixir-phoenix**: Add `codegen` target that invokes
      `libs/elixir-openapi-codegen` to generate structs into `generated-contracts/`, update
      controllers to return generated structs, verify `mix test` passes
- [x] **a-demo-be-clojure-pedestal**: Add `codegen` target that invokes
      `libs/clojure-openapi-codegen` to generate Malli schemas into `generated_contracts/`, add
      middleware validating responses against generated schemas, verify `lein test` passes
- [x] Wire `codegen` as dependency of `test:unit` in each app's `project.json`
- [x] Verify `nx run-many -t test:quick --projects=elixir-openapi-codegen,clojure-openapi-codegen,a-demo-be-python-fastapi,a-demo-be-elixir-phoenix,a-demo-be-clojure-pedestal` passes

**Validation**:

- `libs/elixir-openapi-codegen` passes `test:quick` with ≥90% coverage
- `libs/clojure-openapi-codegen` passes `test:quick` with ≥90% coverage
- Each dynamically typed app's `generated-contracts/` (or `generated_contracts/`) is populated
- `test:unit` validates responses against generated schemas/models
- `nx affected -t test:quick` catches violations before push
- Intentionally removing a required field causes `test:unit` failure
- `nx graph` shows correct dependency: app → codegen lib → a-demo-contracts

---

### Phase 4: E2E Runtime Validation (Safety Net)

**Goal**: Add `ajv`-based response validation to E2E tests as an additional safety layer.

**Implementation Steps**:

- [x] Add `ajv` + `ajv-formats` to `a-demo-be-e2e` dev dependencies
- [x] Create `a-demo-be-e2e/tests/utils/contract-validator.ts`
- [x] Integrate validator into existing backend E2E step definitions
- [x] Add same validator to `a-demo-fe-e2e` test utilities
- [x] Run full E2E suites — fix any discovered drift

**Validation**:

- `nx run a-demo-be-e2e:test:e2e` passes with contract validation enabled
- `nx run a-demo-fe-e2e:test:e2e` passes with contract validation enabled

---

### Phase 5: Documentation + Postinstall

**Goal**: Update documentation, add postinstall hook, verify end-to-end workflow.

**Implementation Steps**:

- [x] Add postinstall script to `package.json`: `npx nx run-many -t codegen --projects=demo-*`
- [x] Verify root `.gitignore` has `**/generated-contracts/` and `**/generated_contracts/` patterns
- [x] Update `specs/apps/a-demo/README.md` — add contracts section with link to
      `specs/apps/a-demo/contracts/README.md`
- [x] Update `specs/apps/a-demo/contracts/README.md` — document codegen workflow, how to modify
      contract, how generated code flows to each app, how to generate/view docs
- [x] Update `libs/README.md` — add `elixir-openapi-codegen` and `clojure-openapi-codegen` entries
- [x] Update `CLAUDE.md`:
  - Add `a-demo-contracts` to Current Apps list
  - Document `codegen` and `docs` Nx targets and dependency chain
  - Document `generated-contracts/` gitignore pattern
  - Add note about contract enforcement in Three-Level Testing section
- [x] Update `governance/development/infra/nx-targets.md` — add `codegen` and `docs` as standard
      targets for demo apps
- [x] Verify fresh clone workflow: `git clone` → `npm install` → `nx affected -t typecheck` passes
- [x] Verify contract change workflow: modify schema → `nx affected -t typecheck` catches all apps
- [x] Verify docs workflow: `nx run a-demo-contracts:docs` → open `generated/docs/index.html` in
      browser → verify all endpoints visible, test-only excluded

**Validation**:

- Fresh clone: `npm install` triggers codegen, `typecheck`/`build` succeeds
- Contract change: `nx affected` shows all demo apps, codegen re-runs, compile catches mismatches
- Pre-push: `git push` triggers `typecheck` + `lint` + `test:quick`, all pass
- PR: quality gate runs same checks, all pass
- Docs: `nx run a-demo-contracts:docs` produces browsable HTML with all public endpoints

---

## Open Questions

1. **Should `codegen` be cacheable?**
   Yes — if the bundled spec hasn't changed, the generated code is identical. Nx caching skips
   redundant regeneration. Cache key includes `openapi-bundled.yaml` hash.

2. **What if a code generator doesn't support all OpenAPI features?**
   Fall back to simpler generation (e.g., just types without full server interface) and add
   runtime validation for unsupported features. Document per-language limitations.

3. **Should the postinstall codegen run on CI?**
   Yes — `npm ci` in CI triggers postinstall, which runs codegen. This ensures CI has all
   generated code before running typecheck/build/test.

4. **What about custom codegen for Elixir/Clojure?**
   Implemented as Nx library projects in `libs/elixir-openapi-codegen` and
   `libs/clojure-openapi-codegen`. Each gets its own `project.json`, test suite (≥90% coverage),
   and Nx dependency tracking — following the same pattern as `libs/elixir-cabbage` and
   `libs/elixir-gherkin`.

---

## Risks and Mitigations

| Risk                                                          | Impact | Mitigation                                                             |
| ------------------------------------------------------------- | ------ | ---------------------------------------------------------------------- |
| Code generator produces incompatible output                   | High   | Test each generator against existing app code before migrating         |
| Custom codegen libs for Elixir/Clojure are fragile            | Medium | Nx libs with ≥90% test coverage; generate only structs/schemas         |
| Postinstall codegen slows npm install                         | Low    | Nx caching ensures codegen only runs when spec changes                 |
| oapi-codegen strict mode conflicts with existing Gin handlers | Medium | Incremental migration: generate types first, then strict interface     |
| Generated code has merge conflicts across branches            | N/A    | Generated code is gitignored — no merge conflicts possible             |
| Fresh clone fails if codegen tool not installed               | Medium | All generators are npm/Go/Maven dependencies, installed by npm install |

---

## Dependencies

- **npm packages**: `@stoplight/spectral-cli`, `@redocly/cli`, `@hey-api/openapi-ts`, `zod`,
  `ajv`
- **Go**: `oapi-codegen` (go install)
- **Java/Kotlin**: `openapi-generator-maven-plugin` / `openapi-generator-gradle-plugin`
- **Python**: `datamodel-code-generator` (pip)
- **Rust**: `openapi-generator` CLI or `progenitor` crate
- **C#**: `NSwag.MSBuild` NuGet package
- **F#**: `openapi-generator` CLI with `fsharp-giraffe-server` generator (beta)
- **Dart**: `openapi-generator` CLI (via Java)
- **Elixir**: `libs/elixir-openapi-codegen` (deps: `yaml_elixir`)
- **Clojure**: `libs/clojure-openapi-codegen` (deps: `clj-yaml`)
