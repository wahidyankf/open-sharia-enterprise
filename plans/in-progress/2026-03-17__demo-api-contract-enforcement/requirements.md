# Requirements

## Objectives

**Primary Objectives**:

1. Create a machine-readable API contract covering all demo application endpoints
2. Enforce contract compliance across all 11 backend implementations at test time
3. Enforce contract compliance across all 3 frontend implementations at build/typecheck time
4. Enforce contract compliance in E2E test suites at runtime
5. Integrate contract validation into Nx dependency graph and CI pipeline

**Secondary Objectives**:

1. Generate API documentation (Swagger UI / Redoc) from the contract for free
2. Enable future code generation for client SDKs or server stubs
3. Establish API style conventions (camelCase fields, consistent error format) via linting
4. Provide example request/response pairs as documentation and test fixtures

## User Stories

**Story 1: Backend Developer Adding a New Field**

```gherkin
Feature: Contract prevents unnoticed API drift
  As a backend developer
  I want the contract to catch when my response shape doesn't match
  So that all implementations stay in sync

Scenario: Adding a field without updating the contract
  Given the OpenAPI contract defines the Expense response schema
  And the schema does not include a "tags" field
  When I add a "tags" field to demo-be-golang-gin's expense response
  And I run test:unit
  Then the contract validator should flag an unexpected field
  And the test should fail until I update the contract

Scenario: Removing a required field
  Given the OpenAPI contract requires "currency" in the Expense response
  When I remove "currency" from demo-be-python-fastapi's response
  And I run test:unit
  Then the contract validator should flag the missing required field
```

**Story 2: Frontend Developer Consuming API Types**

```gherkin
Feature: Frontend types stay in sync with backend contract
  As a frontend developer
  I want types generated from the contract
  So that I never use stale type definitions

Scenario: Contract change updates frontend types
  Given the OpenAPI contract adds an optional "tags" field to Expense
  When I regenerate types with openapi-typescript
  Then the generated Expense type should include "tags?: string[]"
  And TypeScript compilation should succeed

Scenario: Frontend code references non-existent field
  Given the generated types do not include a "notes" field on Expense
  When I reference expense.notes in demo-fe-ts-nextjs
  Then tsc should produce a compile error
```

**Story 3: E2E Test Validating Response Shape**

```gherkin
Feature: E2E tests validate API responses against contract
  As a QA engineer
  I want every API response validated against the contract during E2E runs
  So that shape mismatches are caught end-to-end

Scenario: Response passes contract validation
  Given the demo-be-e2e test sends POST /api/v1/auth/login
  When the backend returns a 200 response with accessToken and refreshToken
  Then the ajv validator should confirm the response matches the contract schema
  And the test should pass

Scenario: Response violates contract
  Given the backend returns a 200 response with "access_token" instead of "accessToken"
  Then the ajv validator should flag a schema violation
  And the E2E test should fail
```

**Story 4: Contract Change Triggers Affected Projects**

```gherkin
Feature: Nx dependency graph includes contract
  As a developer
  I want contract changes to trigger re-testing of all consumers
  So that no project silently falls out of compliance

Scenario: Modifying a schema triggers all backends
  Given I modify specs/apps/demo/contracts/schemas/expense.yaml
  When Nx computes affected projects
  Then all demo-be-* projects should be in the affected list
  And all demo-fe-* projects should be in the affected list
```

## Alternatives Analysis

### Alternative 1: OpenAPI 3.1 (Single YAML) + Language-Specific Validators

**Approach**: Write a single `openapi.yaml` in `specs/apps/demo/contracts/` that fully describes
every endpoint. Each project validates against it at test time.

**Enforcement per project type**:

| Project Type         | Enforcement Mechanism                                                                       | When        |
| -------------------- | ------------------------------------------------------------------------------------------- | ----------- |
| Go backends          | `kin-openapi` middleware or test helper — validates every request/response against the spec | `test:unit` |
| Java/Kotlin backends | `openapi-diff` or `swagger-request-validator` (Atlassian)                                   | `test:unit` |
| Python backends      | `openapi-core` validator                                                                    | `test:unit` |
| Rust backends        | Custom validator or `utoipa` compile-time check                                             | `test:unit` |
| Elixir backends      | `open_api_spex` cast/validate                                                               | `test:unit` |
| F# / C# backends     | `NSwag` or `Microsoft.OpenApi` reader + validator                                           | `test:unit` |
| Clojure backends     | `metosin/scjsv` (JSON Schema) or `luposlip/json-schema`                                     | `test:unit` |
| TS Effect backend    | `zod-openapi` or `openapi-typescript` generated types                                       | `typecheck` |
| TypeScript frontends | `openapi-typescript` → generates `types.ts`; `tsc` fails on drift                           | `typecheck` |
| Dart frontend        | `openapi_generator_cli` → generates models; compiler fails on drift                         | `build`     |
| E2E tests            | `ajv` (JSON Schema extracted from OpenAPI) validates every response                         | `test:e2e`  |

**Pros**:

- Industry standard — massive tooling ecosystem
- Human-readable YAML, easy to review in PRs
- Can generate interactive docs (Swagger UI / Redoc) for free
- Supports `$ref` for reusable schemas (DRY)
- JSON Schema 2020-12 compatible (OpenAPI 3.1)
- Most languages have mature, battle-tested validators
- Can generate client SDKs and server stubs (optional, not required)

**Cons**:

- Large single file can be hard to review (mitigated by splitting into `$ref` files)
- Some languages (Elixir, Clojure, F#) have thinner tooling
- Spec authoring requires OpenAPI knowledge
- Runtime validation adds overhead (acceptable in tests, debatable in prod)

**Estimated effort**: Medium

---

### Alternative 2: JSON Schema (Standalone) + Test-Time Validation

**Approach**: Write JSON Schema files per endpoint (request + response) in
`specs/apps/demo/contracts/schemas/`. Each project loads the schema and validates against it in
tests.

**Contract location**:

```
specs/apps/demo/contracts/
├── README.md
└── schemas/
    ├── auth/
    │   ├── login-request.json
    │   ├── login-response.json
    │   └── ...
    ├── expenses/
    │   ├── create-expense-request.json
    │   ├── expense-list-response.json
    │   └── ...
    └── common/
        ├── pagination.json
        └── error.json
```

**Pros**:

- Simpler than OpenAPI — just schemas, no path/method/parameter overhead
- Every language has a JSON Schema library (even Elixir: `ex_json_schema`)
- Schemas are independently testable
- Easy to understand — one file per shape

**Cons**:

- Does NOT capture endpoints, HTTP methods, status codes, headers, or query params — only body
  shapes. A backend could return the right body on the wrong endpoint.
- More files to maintain (~40-50 schema files)
- No standard way to tie schemas to endpoints (need a separate mapping file)
- Cannot generate API docs or client code
- Duplicates some information already in Gherkin (field names, types)

**Estimated effort**: Medium

---

### Alternative 3: TypeScript Types as Source of Truth + Cross-Language Validation

**Approach**: Maintain canonical TypeScript interfaces in `specs/apps/demo/contracts/types.ts`.
Use `typescript-json-schema` to generate JSON Schemas. Other languages validate against generated
schemas.

**Pros**:

- Developers already know TypeScript — lower authoring friction
- The types already exist in `demo-fe-ts-nextjs/src/lib/api/types.ts`
- TypeScript's type system is expressive (unions, optional fields, string literals)
- JSON Schema generation is well-supported (`ts-json-schema-generator`, `zod-to-json-schema`)

**Cons**:

- TypeScript-centric — feels wrong for a polyglot repo with 11 languages
- Requires a build step to generate JSON Schemas
- TypeScript type system doesn't capture HTTP semantics (methods, paths, status codes)
- Generated schemas are a secondary artifact — debugging requires tracing back to TS
- Non-TS developers must understand TypeScript to modify contracts

**Estimated effort**: Low-Medium

---

### Alternative 4: Protocol Buffers (Protobuf) + gRPC-Gateway Style

**Approach**: Define the API contract in `.proto` files. Use `protoc` with JSON-mapping plugins to
generate JSON Schema or language-specific types. The actual API remains REST/JSON but the contract
is defined in Protobuf.

**Pros**:

- Extremely precise type definitions (no ambiguity)
- First-class code generation for Go, Java, Python, Rust, C#, Dart, Kotlin
- Well-established in enterprise environments
- Schema evolution rules built in (field numbers)

**Cons**:

- Heavy tooling overhead (`protoc` + plugins for each language)
- The API is REST/JSON, not gRPC — Protobuf adds conceptual mismatch
- Protobuf JSON mapping has quirks (camelCase default, wrapper types for nullable)
- No native Elixir, F#, or Clojure Protobuf support (requires plugins/wrappers)
- Developers must learn Protobuf syntax
- Overkill for a REST API that doesn't use gRPC

**Estimated effort**: High

---

### Alternative 5: Zod Schemas + `zod-to-openapi` Bridge

**Approach**: Write Zod schemas in TypeScript as the source of truth. Generate OpenAPI spec from
Zod via `zod-to-openapi`. Non-TypeScript projects validate against the generated OpenAPI spec.

**Pros**:

- Zod provides runtime validation + TypeScript inference (single definition)
- TypeScript projects get compile-time safety AND runtime validation
- OpenAPI output enables the full ecosystem (docs, validators, generators)
- Best of both worlds: developer-friendly authoring + industry-standard output

**Cons**:

- Same TypeScript-centricity issue as Alternative 3
- Adds a Zod dependency and build step
- Two layers of abstraction (Zod → OpenAPI → JSON Schema → validation)
- Non-TS developers must read Zod to understand contracts
- Generated OpenAPI may not be as clean as hand-written

**Estimated effort**: Medium

---

### Alternative 6: OpenAPI 3.1 (Modular YAML) + Spectral Linting + Contract Tests

**Approach**: Same as Alternative 1 but with a modular file structure (split by domain using
`$ref`) and Spectral linting to enforce API style rules. Add a dedicated `contracts:validate`
Nx target.

**Contract location**:

```
specs/apps/demo/contracts/
├── README.md
├── openapi.yaml              # Root spec (references domain files)
├── .spectral.yaml            # API style rules
├── paths/
│   ├── auth.yaml             # /api/v1/auth/* paths
│   ├── users.yaml            # /api/v1/users/* paths
│   ├── expenses.yaml         # /api/v1/expenses/* paths
│   ├── admin.yaml            # /api/v1/admin/* paths
│   ├── reports.yaml          # /api/v1/reports/* paths
│   ├── tokens.yaml           # /api/v1/tokens/* paths
│   ├── health.yaml           # /health, /.well-known/* paths
│   └── test-support.yaml     # /api/v1/test/* paths
├── schemas/
│   ├── auth.yaml             # Auth request/response schemas
│   ├── user.yaml             # User schemas
│   ├── expense.yaml          # Expense schemas
│   ├── report.yaml           # Report schemas
│   ├── attachment.yaml       # Attachment schemas
│   ├── token.yaml            # Token/JWKS schemas
│   ├── admin.yaml            # Admin schemas
│   ├── pagination.yaml       # Shared pagination envelope
│   └── error.yaml            # Shared error response
└── examples/
    ├── auth-login.yaml       # Example request/response pairs
    └── ...
```

**Enforcement**:

1. **Spectral CI lint** — ensures the OpenAPI spec follows style rules (consistent naming, required
   fields, proper `$ref` usage)
2. **Per-backend contract test** — a test helper reads `openapi.yaml`, intercepts every HTTP
   response, and validates it against the spec
3. **Per-frontend type generation** — `openapi-typescript` generates types; `tsc` catches drift
4. **E2E response validation** — middleware in Playwright validates every API response against the
   spec
5. **Nx target `contracts:validate`** — bundles the modular YAML into a single resolved spec and
   validates it

**Pros**:

- All pros of Alternative 1
- Modular structure — easy to review domain-by-domain in PRs
- Spectral linting catches API style issues (inconsistent naming, missing descriptions)
- Example files serve as documentation AND test fixtures
- `$ref` structure maps naturally to the domain organization in Gherkin specs
- Scales well as API grows

**Cons**:

- More files than a single OpenAPI YAML (but each file is small and focused)
- Spectral adds another tool to the CI pipeline
- Requires `$ref` resolution step (standard tooling handles this)

**Estimated effort**: Medium-High

---

## Recommendation Matrix

| Criterion                         | Alt 1: OpenAPI Single | Alt 2: JSON Schema | Alt 3: TS Types | Alt 4: Protobuf | Alt 5: Zod Bridge | Alt 6: OpenAPI Modular |
| --------------------------------- | --------------------- | ------------------ | --------------- | --------------- | ----------------- | ---------------------- |
| Captures full HTTP semantics      | Yes                   | No                 | Partial         | Partial         | Yes (via gen)     | Yes                    |
| Language-agnostic authoring       | Yes                   | Yes                | No              | Yes             | No                | Yes                    |
| Tooling maturity (all 11 langs)   | High                  | High               | Medium          | Low             | Medium            | High                   |
| File organization / reviewability | Medium                | High               | Medium          | Medium          | Medium            | High                   |
| Code generation capability        | Yes                   | No                 | Partial         | Yes             | Yes               | Yes                    |
| API documentation generation      | Yes                   | No                 | No              | No              | Yes               | Yes                    |
| Style/lint enforcement            | Manual                | No                 | No              | No              | No                | Yes (Spectral)         |
| Learning curve                    | Medium                | Low                | Low (TS devs)   | High            | Medium            | Medium                 |
| Conceptual fit (REST/JSON API)    | Perfect               | Good               | OK              | Poor            | Good              | Perfect                |
| Maintenance burden                | Medium                | High (many files)  | Low             | High            | Medium            | Medium                 |
| Complements existing Gherkin      | Yes                   | Partial            | Partial         | No              | Yes               | Yes                    |

## Recommended Approach: Alternative 6

**Why Alternative 6 — OpenAPI 3.1 Modular + Spectral**:

1. **Full HTTP semantics** — paths, methods, parameters, headers, status codes, and body schemas in
   one specification. No separate mapping file needed.
2. **Language-agnostic** — YAML is readable by all 11 backend languages and both frontend
   ecosystems. No team member needs TypeScript or Protobuf knowledge.
3. **Modular structure** — domain-split files mirror the existing Gherkin domain organization,
   making PRs reviewable and ownership clear.
4. **Spectral linting** — enforces API naming conventions (camelCase fields, consistent error
   format, required descriptions) beyond just schema correctness.
5. **Ecosystem leverage** — OpenAPI 3.1 is JSON Schema 2020-12 compatible, so every JSON Schema
   validator works on extracted schemas. Code generation, documentation, and mock servers are all
   available.
6. **Complements Gherkin** — Gherkin says "when I POST this, I get 201 with an id". OpenAPI says
   "the POST body must have these exact fields with these exact types, and the 201 response has
   this exact shape". They cover orthogonal concerns.

## Acceptance Criteria

```gherkin
Feature: API contract enforcement

  Scenario: Contract spec exists and is valid
    Given the file specs/apps/demo/contracts/openapi.yaml exists
    When Spectral lints the OpenAPI specification
    Then there should be zero errors
    And the spec should cover all endpoints from the Gherkin features

  Scenario: Contract changes trigger affected project tests
    Given a developer modifies specs/apps/demo/contracts/schemas/expense.yaml
    When Nx computes affected projects
    Then all demo-be-* and demo-fe-* projects should be affected

  Scenario: Backend response matches contract
    Given the OpenAPI spec defines POST /api/v1/expenses response as 201
    And the 201 response schema requires fields "id", "amount", "currency"
    When demo-be-golang-gin handles a create expense request in test:unit
    Then the response body must validate against the contract schema
    And a missing or extra field should fail the test

  Scenario: Frontend types are generated from contract
    Given the OpenAPI spec defines the Expense schema
    When openapi-typescript generates types for demo-fe-ts-nextjs
    Then the generated Expense type should match the contract exactly
    And any drift from hand-written types should cause tsc to fail

  Scenario: E2E tests validate responses against contract
    Given demo-be-e2e runs a Playwright test hitting POST /api/v1/auth/login
    When the backend returns a 200 response
    Then the response body should be validated against the OpenAPI spec
    And a schema violation should fail the E2E test

  Scenario: All 11 backends pass contract validation
    Given each demo-be-* project has a contract validation test helper
    When nx run-many -t test:unit --projects=demo-be-*
    Then all backends should pass with zero contract violations

  Scenario: All frontends compile with generated types
    Given each demo-fe-* project uses generated types from the contract
    When nx run-many -t typecheck --projects=demo-fe-*
    Then all frontends should compile successfully
```

## Constraints

1. **Must not break existing tests** — contract enforcement is additive; existing Gherkin-based
   tests continue to work unchanged
2. **Must not require runtime dependencies in production** — validation is test-time only
3. **Must use existing Nx infrastructure** — new targets, implicit dependencies, affected graph
4. **Contract lives in `specs/`** — not inside any individual app
5. **Trunk Based Development** — all work on main branch

## Out of Scope

1. Replacing Gherkin specs with OpenAPI — they serve different purposes
2. Runtime validation in production (only test-time enforcement)
3. Generating server stubs from the contract (manual implementation stays)
4. WebSocket or streaming API contracts (REST/JSON only)
5. Authentication token content validation (JWT claims are covered by Gherkin)
