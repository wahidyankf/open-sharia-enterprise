# Delivery Plan

## Implementation Phases

### Phase 1: Contract Authoring

**Goal**: Write the complete OpenAPI 3.1 modular specification and validate it with Spectral.

**Implementation Steps**:

- [ ] Create `specs/apps/demo/contracts/` directory structure (paths/, schemas/, examples/)
- [ ] Write `README.md` with purpose, usage guide, and contribution rules
- [ ] Write root `openapi.yaml` with server config, security schemes, and `$ref` path mappings
- [ ] Write `schemas/auth.yaml` — LoginRequest, LoginResponse, RegisterRequest, RegisterResponse,
      RefreshRequest, RefreshResponse
- [ ] Write `schemas/user.yaml` — User, UpdateProfileRequest, ChangePasswordRequest
- [ ] Write `schemas/expense.yaml` — Expense, CreateExpenseRequest, UpdateExpenseRequest
- [ ] Write `schemas/expense-list.yaml` — ExpenseListResponse (uses pagination.yaml)
- [ ] Write `schemas/report.yaml` — PLReport, CategoryBreakdown, ExpenseSummary
- [ ] Write `schemas/attachment.yaml` — Attachment
- [ ] Write `schemas/token.yaml` — TokenClaims, JwksResponse, JwkKey
- [ ] Write `schemas/admin.yaml` — DisableRequest, PasswordResetResponse
- [ ] Write `schemas/pagination.yaml` — reusable pagination envelope
- [ ] Write `schemas/error.yaml` — standardized error response
- [ ] Write `schemas/health.yaml` — HealthResponse
- [ ] Write `paths/health.yaml` — GET /health
- [ ] Write `paths/auth.yaml` — POST login, register, refresh, logout, logout-all
- [ ] Write `paths/users.yaml` — GET/PATCH /me, POST password, POST deactivate
- [ ] Write `paths/expenses.yaml` — CRUD + summary
- [ ] Write `paths/attachments.yaml` — POST/GET/DELETE attachments
- [ ] Write `paths/reports.yaml` — GET /api/v1/reports/pl
- [ ] Write `paths/admin.yaml` — GET users, POST disable/enable/unlock/force-password-reset
- [ ] Write `paths/tokens.yaml` — GET claims, GET /.well-known/jwks.json
- [ ] Write `paths/test-support.yaml` — POST reset-db, POST promote-admin
- [ ] Write `.spectral.yaml` with camelCase, description, and example rules
- [ ] Add `demo-contracts` as Nx project with `lint` and `bundle` targets
- [ ] Verify Spectral lint passes with zero errors
- [ ] Verify Redocly CLI bundle resolves all `$ref`s correctly
- [ ] Write example files for major endpoints (auth-login, expense-create, user-register)

**Validation**:

- `npx @stoplight/spectral-cli lint openapi.yaml` exits 0
- `npx @redocly/cli bundle openapi.yaml` produces valid bundled output
- All endpoints from Gherkin specs are covered in the contract

---

### Phase 2: Frontend Enforcement

**Goal**: Replace hand-written frontend types with types generated from the contract.

**Implementation Steps**:

- [ ] Add `openapi-typescript` as dev dependency to workspace
- [ ] Create codegen script: `openapi-typescript specs/apps/demo/contracts/generated/openapi-bundled.yaml -o <app>/src/lib/api/generated-types.d.ts`
- [ ] Update `demo-fe-ts-nextjs` to import generated types instead of hand-written `types.ts`
- [ ] Update `demo-fe-ts-tanstack-start` to import generated types similarly
- [ ] Set up `openapi_generator_cli` for `demo-fe-dart-flutterweb` (generates Dart models)
- [ ] Add `codegen` Nx target to each frontend that regenerates types from the contract
- [ ] Verify `typecheck` passes for `demo-fe-ts-nextjs`
- [ ] Verify `typecheck` passes for `demo-fe-ts-tanstack-start`
- [ ] Verify `build` passes for `demo-fe-dart-flutterweb`

**Validation**:

- `nx run demo-fe-ts-nextjs:typecheck` passes
- `nx run demo-fe-ts-tanstack-start:typecheck` passes
- `nx run demo-fe-dart-flutterweb:build` passes
- Intentionally breaking a type in the contract causes compilation failure

---

### Phase 3: E2E Enforcement

**Goal**: Add runtime response validation against the contract in E2E test suites.

**Implementation Steps**:

- [ ] Add `ajv` + `@apidevtools/json-schema-ref-parser` to `demo-be-e2e` dev dependencies
- [ ] Create shared response validator utility in `demo-be-e2e/tests/utils/contract-validator.ts`
- [ ] Integrate validator into existing backend E2E step definitions (validate every API response)
- [ ] Add same validator to `demo-fe-e2e` test utilities
- [ ] Run full `demo-be-e2e` E2E suite — fix any discovered drift
- [ ] Run full `demo-fe-e2e` E2E suite — fix any discovered drift

**Validation**:

- `nx run demo-be-e2e:test:e2e` passes with contract validation enabled
- `nx run demo-fe-e2e:test:e2e` passes with contract validation enabled
- Intentionally returning wrong field name in a backend causes E2E test failure

---

### Phase 4: Backend Enforcement (per backend)

**Goal**: Add contract validation test helpers to all 11 backend implementations.

**Implementation Steps**:

- [ ] **demo-be-golang-gin**: Add `getkin/kin-openapi` dependency, create `contract_test.go` helper
      that validates response bodies against the spec in unit tests
- [ ] **demo-be-java-springboot**: Add `com.atlassian.oai:swagger-request-validator-spring-webmvc`,
      create MockMvc assertion wrapper
- [ ] **demo-be-java-vertx**: Add `com.atlassian.oai:swagger-request-validator-core`, create custom
      test validator
- [ ] **demo-be-kotlin-ktor**: Add `com.atlassian.oai:swagger-request-validator-core`, create test
      helper
- [ ] **demo-be-python-fastapi**: Add `openapi-core` to test dependencies, create pytest fixture
      wrapping TestClient with validation
- [ ] **demo-be-rust-axum**: Add `jsonschema` crate to dev-dependencies, create test helper that
      validates serialized responses
- [ ] **demo-be-elixir-phoenix**: Add `ex_json_schema` dependency, create ConnCase helper for
      response validation
- [ ] **demo-be-fsharp-giraffe**: Add `NJsonSchema` NuGet package, create test helper
- [ ] **demo-be-csharp-aspnetcore**: Add `NJsonSchema` NuGet package, create WebApplicationFactory
      validator
- [ ] **demo-be-clojure-pedestal**: Add `metosin/scjsv` dependency, create test middleware for
      response validation
- [ ] **demo-be-ts-effect**: Add `ajv` dev dependency, create test helper for response validation
- [ ] Run `nx run-many -t test:unit --projects=demo-be-*` — verify all pass

**Validation**:

- Each backend's `test:unit` validates response shapes against the contract
- `nx run-many -t test:unit --projects=demo-be-*` passes with zero contract violations
- Intentionally removing a required field in a backend causes `test:unit` failure

---

### Phase 5: CI Integration

**Goal**: Wire contract validation into the Nx dependency graph and CI pipeline.

**Implementation Steps**:

- [ ] Add `contracts:lint` to pre-push affected targets
- [ ] Add implicit dependency from all `demo-*` projects to `demo-contracts` in `nx.json`
- [ ] Verify `nx affected -t test:unit` includes backends when contract files change
- [ ] Verify `nx affected -t typecheck` includes frontends when contract files change
- [ ] Update `specs/apps/demo/README.md` to reference contracts directory
- [ ] Update `CLAUDE.md` to document contract enforcement workflow

**Validation**:

- Modifying `specs/apps/demo/contracts/schemas/expense.yaml` and running `nx affected` shows all
  demo-be-\* and demo-fe-\* projects as affected
- Pre-push hook runs contract lint for affected changes
- Full CI pipeline passes with all contract enforcement active

---

## Open Questions

1. **Should generated frontend types replace or coexist with hand-written types?**
   Replacing is cleaner but means frontend devs must regenerate on every contract change.
   Coexisting means a CI check that generated matches hand-written.
   **Recommendation**: Replace — add a `codegen` Nx target that runs before `typecheck`.

2. **Should backend contract validation run in `test:unit` or `test:integration`?**
   `test:unit` is faster and cacheable, but uses mocked responses. `test:integration` tests real
   responses but is slower.
   **Recommendation**: Both — unit tests validate response _construction_, integration tests
   validate end-to-end _correctness_.

3. **How to handle test-only endpoints (`/api/v1/test/*`)?**
   They're part of the API surface but shouldn't appear in production docs.
   **Recommendation**: Use `x-test-only: true` OpenAPI extension. Spectral rule warns if test
   endpoints lack this extension. Documentation generators filter them out.

4. **Should the bundled OpenAPI be committed or generated in CI?**
   Committing makes it easy to review the resolved spec. Generating avoids staleness.
   **Recommendation**: Generate in CI via `bundle` target. Commit the bundled file for convenience
   but validate it's up-to-date in CI.

---

## Risks and Mitigations

| Risk                                                  | Impact | Mitigation                                                                   |
| ----------------------------------------------------- | ------ | ---------------------------------------------------------------------------- |
| OpenAPI spec doesn't match actual implementation      | High   | Phase 3 (E2E) catches any drift against real backends                        |
| Some languages have weak OpenAPI validation libraries | Medium | Fall back to JSON Schema extraction + language-native JSON Schema validators |
| Large number of projects to update (15+)              | Medium | Phased rollout: contract first, then frontends, E2E, backends                |
| Spectral rules too strict or too lenient              | Low    | Start with minimal rules, expand based on actual drift patterns              |
| Generated types cause breaking changes in frontends   | Medium | Generated types committed to repo; diff visible in PR review                 |

---

## Dependencies

- **npm packages**: `@stoplight/spectral-cli`, `@redocly/cli`, `openapi-typescript`, `ajv`
- **Per-language libraries**: See [tech-docs.md](./tech-docs.md) Per-Language Enforcement Strategy
- **Existing infrastructure**: Nx workspace, existing Gherkin specs, existing frontend types
- **No external services**: All validation is local/CI — no hosted API required
