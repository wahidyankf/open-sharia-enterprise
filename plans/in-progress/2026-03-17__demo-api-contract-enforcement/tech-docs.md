# Technical Documentation

## Contract File Structure

```
specs/apps/demo/contracts/
├── README.md                 # Purpose, usage, how to add/modify
├── openapi.yaml              # Root OpenAPI 3.1 document
├── .spectral.yaml            # Spectral linting rules
├── paths/
│   ├── health.yaml           # GET /health
│   ├── auth.yaml             # POST /api/v1/auth/{login,register,refresh,logout,logout-all}
│   ├── users.yaml            # GET/PATCH /api/v1/users/me, POST password/deactivate
│   ├── expenses.yaml         # CRUD /api/v1/expenses, summary
│   ├── attachments.yaml      # /api/v1/expenses/{id}/attachments
│   ├── reports.yaml          # GET /api/v1/reports/pl
│   ├── admin.yaml            # /api/v1/admin/users/*
│   ├── tokens.yaml           # GET /api/v1/tokens/claims, /.well-known/jwks.json
│   └── test-support.yaml     # POST /api/v1/test/{reset-db,promote-admin}
├── schemas/
│   ├── auth.yaml             # LoginRequest, LoginResponse, RegisterRequest, etc.
│   ├── user.yaml             # User, UpdateProfileRequest, ChangePasswordRequest
│   ├── expense.yaml          # Expense, CreateExpenseRequest, UpdateExpenseRequest
│   ├── expense-list.yaml     # ExpenseListResponse (uses pagination.yaml)
│   ├── report.yaml           # PLReport, CategoryBreakdown
│   ├── attachment.yaml       # Attachment
│   ├── token.yaml            # TokenClaims, JwksResponse, JwkKey
│   ├── admin.yaml            # DisableRequest, PasswordResetResponse, UserListResponse
│   ├── pagination.yaml       # Reusable pagination envelope
│   ├── error.yaml            # Standardized error response
│   └── health.yaml           # HealthResponse
└── examples/
    ├── auth-login.yaml       # Login request/response example pair
    ├── expense-create.yaml   # Create expense example pair
    └── ...                   # One per major endpoint
```

## Root OpenAPI Document

```yaml
# specs/apps/demo/contracts/openapi.yaml
openapi: "3.1.0"
info:
  title: Demo Application API
  version: "1.0.0"
  description: >
    Canonical API contract for the demo full-stack application.
    Consumed by 11 backend implementations, 3 frontend implementations,
    and 2 E2E test suites.
servers:
  - url: http://localhost:{port}
    variables:
      port:
        default: "8080"
        description: Backend server port
paths:
  /health:
    $ref: "./paths/health.yaml#/health"
  /api/v1/auth/login:
    $ref: "./paths/auth.yaml#/login"
  /api/v1/auth/register:
    $ref: "./paths/auth.yaml#/register"
  # ... all paths reference domain files
components:
  securitySchemes:
    bearerAuth:
      type: http
      scheme: bearer
      bearerFormat: JWT
  schemas:
    $ref: "./schemas/" # Bundled during resolution
```

## Enforcement Architecture

```
┌─────────────────────────────────────────────────────┐
│              specs/apps/demo/contracts/              │
│                   openapi.yaml                       │
│              (Single Source of Truth)                │
└──────────┬──────────┬──────────┬────────────────────┘
           │          │          │
     ┌─────▼────┐ ┌───▼────┐ ┌──▼──────────────┐
     │ Spectral │ │  Type  │ │  Schema          │
     │  Lint    │ │  Gen   │ │  Validators      │
     │ (CI)     │ │ (FE)   │ │  (BE tests)      │
     └─────┬────┘ └───┬────┘ └──┬──────────────┘
           │          │          │
     ┌─────▼────┐ ┌───▼────┐ ┌──▼──────────────┐
     │  Style   │ │ TS/Dart│ │ Go,Java,Kotlin,  │
     │  Rules   │ │ compile│ │ Python,Rust,     │
     │  Pass?   │ │ check  │ │ Elixir,F#,C#,    │
     │          │ │        │ │ Clojure,TS       │
     └──────────┘ └────────┘ └─────────────────┘
```

## Per-Language Enforcement Strategy

### Backends — Test-Time Response Validation

Each backend adds a **contract test helper** that:

1. Reads the bundled `openapi.yaml` (resolved to a single file)
2. On every HTTP handler test, validates the response body against the corresponding schema for
   that endpoint + status code
3. Fails the test if the response doesn't match

| Language            | Library                                                     | Integration Point                       |
| ------------------- | ----------------------------------------------------------- | --------------------------------------- |
| Go                  | `getkin/kin-openapi`                                        | Test middleware or helper in `_test.go` |
| Java (Spring Boot)  | `com.atlassian.oai:swagger-request-validator-spring-webmvc` | MockMvc assertion                       |
| Java (Vert.x)       | `com.atlassian.oai:swagger-request-validator-core`          | Custom test validator                   |
| Kotlin (Ktor)       | `com.atlassian.oai:swagger-request-validator-core`          | Test helper                             |
| Python (FastAPI)    | `openapi-core`                                              | Pytest fixture wrapping TestClient      |
| Rust (Axum)         | Custom via `serde_json` + `jsonschema` crate                | Test helper                             |
| Elixir (Phoenix)    | `open_api_spex` or `ex_json_schema`                         | ConnCase helper                         |
| F# (Giraffe)        | `NSwag.Core` or `NJsonSchema`                               | Test helper                             |
| C# (ASP.NET Core)   | `NSwag.Core` or Verify.Http                                 | WebApplicationFactory                   |
| Clojure (Pedestal)  | `metosin/scjsv` (wraps networknt/json-schema-validator)     | Test middleware                         |
| TypeScript (Effect) | `ajv` with OpenAPI schema extraction                        | Test helper                             |

**Library verification** (web-researched 2026-03-17):

- **getkin/kin-openapi** — OpenAPI 3.0/3.1 Go library with `openapi3filter` package for
  request/response validation. Actively maintained (2025 releases).
- **Atlassian swagger-request-validator** — Java library validating HTTP requests/responses against
  OpenAPI specs. Supports Spring 6+ WebMVC. Available on Maven Central.
- **openapi-core** — Python library (v0.23.0, Mar 2026) supporting OpenAPI 3.0/3.1/3.2. Has
  first-class FastAPI middleware integration via `FastAPIOpenAPIMiddleware`.
- **jsonschema crate** — High-performance Rust JSON Schema validator. Requires Rust 1.83.0+.
  Supports draft 4/6/7/2019-09/2020-12.
- **ex_json_schema** — Elixir JSON Schema validator (v0.11.2) supporting draft 4/6/7.
- **NJsonSchema** — .NET library (v11.5.2) for JSON Schema validation. Works with F# via
  `FSharp.Data.JsonSchema`. Integrates with NSwag for OpenAPI.
- **metosin/scjsv** — Simple Clojure JSON Schema Validator, wraps
  `networknt/json-schema-validator` (Java). Runs on JVM natively.

### Frontends — Type Generation + Compile Check

| Framework           | Tool                           | Workflow                                                                    |
| ------------------- | ------------------------------ | --------------------------------------------------------------------------- |
| Next.js (TS)        | `openapi-typescript`           | Generates `types.d.ts` → imported by API client → `tsc` fails on mismatch   |
| TanStack Start (TS) | `openapi-typescript`           | Same as Next.js                                                             |
| Flutter (Dart)      | `openapi_generator_cli` (Dart) | Generates models → imported by API client → Dart analyzer fails on mismatch |

**Library verification** (web-researched 2026-03-17):

- **openapi-typescript** — Generates TypeScript types from OpenAPI 3.0 & 3.1 specs. Runtime-free
  types, millisecond generation. Requires Node.js 20+. Available at `openapi-ts.dev`.
- **openapi_generator_cli** — Dart/Flutter wrapper around openapi-generator. Generates Dart models
  and API clients from OpenAPI specs. Available on pub.dev.

### E2E Tests — Runtime Response Validation

Both `demo-be-e2e` and `demo-fe-e2e` (Playwright/TypeScript) add an `ajv`-based helper that
validates every API response against the OpenAPI spec during test runs.

**Implementation pattern**:

```typescript
// demo-be-e2e/tests/utils/contract-validator.ts
import Ajv from "ajv";
import { readFileSync } from "fs";

const spec = JSON.parse(readFileSync("specs/apps/demo/contracts/generated/openapi-bundled.json", "utf8"));
const ajv = new Ajv({ allErrors: true });

export function validateResponse(path: string, method: string, statusCode: number, body: unknown): void {
  const schema = spec.paths[path]?.[method]?.responses?.[statusCode]?.content?.["application/json"]?.schema;
  if (!schema) throw new Error(`No schema for ${method.toUpperCase()} ${path} ${statusCode}`);
  const valid = ajv.validate(schema, body);
  if (!valid) throw new Error(`Contract violation: ${ajv.errorsText()}`);
}
```

## Nx Integration

### New Nx Project: `demo-contracts`

Added via `specs/apps/demo/contracts/project.json`:

```json
{
  "name": "demo-contracts",
  "root": "specs/apps/demo/contracts",
  "targets": {
    "lint": {
      "command": "npx @stoplight/spectral-cli lint specs/apps/demo/contracts/openapi.yaml --ruleset specs/apps/demo/contracts/.spectral.yaml"
    },
    "bundle": {
      "command": "npx @redocly/cli bundle specs/apps/demo/contracts/openapi.yaml -o specs/apps/demo/contracts/generated/openapi-bundled.yaml",
      "dependsOn": ["lint"]
    },
    "validate": {
      "dependsOn": ["lint", "bundle"]
    }
  }
}
```

**Library verification** (web-researched 2026-03-17):

- **@stoplight/spectral-cli** — OpenAPI linter supporting 3.0/3.1. Adopted by 40% of Fortune 500
  API teams (2025). Supports custom rulesets via `.spectral.yaml`.
- **@redocly/cli** — OpenAPI CLI tool with `bundle` command that resolves `$ref` references into a
  single output file. Supports YAML and JSON output.

### Implicit Dependencies

All `demo-be-*` and `demo-fe-*` projects declare an implicit dependency on `demo-contracts` in
`nx.json`:

```json
{
  "targetDefaults": {
    "test:unit": {
      "dependsOn": ["^validate"]
    }
  },
  "projects": {
    "demo-be-golang-gin": { "implicitDependencies": ["demo-contracts"] },
    "demo-be-java-springboot": { "implicitDependencies": ["demo-contracts"] },
    "demo-fe-ts-nextjs": { "implicitDependencies": ["demo-contracts"] }
  }
}
```

This ensures changes to `specs/apps/demo/contracts/**` trigger re-testing of all consumer projects
via `nx affected`.

## Spectral Rules

```yaml
# specs/apps/demo/contracts/.spectral.yaml
extends: ["spectral:oas"]
rules:
  # Enforce camelCase for all schema properties
  oas3-schema-properties-camelCase:
    severity: error
    given: "$.components.schemas..properties.*~"
    then:
      function: casing
      functionOptions:
        type: camel

  # Require descriptions on all schemas
  oas3-schema-description:
    severity: warn
    given: "$.components.schemas.*"
    then:
      field: description
      function: truthy

  # Require examples on all endpoints
  oas3-operation-examples:
    severity: warn
    given: "$.paths.*.*.responses.*.content.application/json"
    then:
      field: example
      function: truthy
```

## API Surface Summary

The contract covers all endpoints from the existing Gherkin specs:

### Authentication & Token Management

| Method | Path                    | Request Body    | Success Code | Response Body |
| ------ | ----------------------- | --------------- | ------------ | ------------- |
| POST   | /api/v1/auth/register   | RegisterRequest | 201          | User          |
| POST   | /api/v1/auth/login      | LoginRequest    | 200          | AuthTokens    |
| POST   | /api/v1/auth/refresh    | RefreshRequest  | 200          | AuthTokens    |
| POST   | /api/v1/auth/logout     | —               | 200          | —             |
| POST   | /api/v1/auth/logout-all | —               | 200          | —             |

### User Account Management

| Method | Path                        | Request Body          | Success Code | Response Body |
| ------ | --------------------------- | --------------------- | ------------ | ------------- |
| GET    | /api/v1/users/me            | —                     | 200          | User          |
| PATCH  | /api/v1/users/me            | UpdateProfileRequest  | 200          | User          |
| POST   | /api/v1/users/me/password   | ChangePasswordRequest | 200          | —             |
| POST   | /api/v1/users/me/deactivate | —                     | 200          | —             |

### Expense Management

| Method | Path                     | Request Body         | Success Code | Response Body       |
| ------ | ------------------------ | -------------------- | ------------ | ------------------- |
| POST   | /api/v1/expenses         | CreateExpenseRequest | 201          | Expense             |
| GET    | /api/v1/expenses         | —                    | 200          | ExpenseListResponse |
| GET    | /api/v1/expenses/{id}    | —                    | 200          | Expense             |
| PUT    | /api/v1/expenses/{id}    | UpdateExpenseRequest | 200          | Expense             |
| DELETE | /api/v1/expenses/{id}    | —                    | 204          | —                   |
| GET    | /api/v1/expenses/summary | —                    | 200          | ExpenseSummary[]    |

### Attachments

| Method | Path                                    | Request Body   | Success Code | Response Body |
| ------ | --------------------------------------- | -------------- | ------------ | ------------- |
| POST   | /api/v1/expenses/{id}/attachments       | multipart/form | 201          | Attachment    |
| GET    | /api/v1/expenses/{id}/attachments       | —              | 200          | Attachment[]  |
| DELETE | /api/v1/expenses/{id}/attachments/{aid} | —              | 204          | —             |

### Reporting

| Method | Path               | Query Params       | Success Code | Response Body |
| ------ | ------------------ | ------------------ | ------------ | ------------- |
| GET    | /api/v1/reports/pl | from, to, currency | 200          | PLReport      |

### Admin Operations

| Method | Path                                          | Request Body   | Success Code | Response Body         |
| ------ | --------------------------------------------- | -------------- | ------------ | --------------------- |
| GET    | /api/v1/admin/users                           | —              | 200          | UserListResponse      |
| POST   | /api/v1/admin/users/{id}/disable              | DisableRequest | 200          | —                     |
| POST   | /api/v1/admin/users/{id}/enable               | —              | 200          | —                     |
| POST   | /api/v1/admin/users/{id}/unlock               | —              | 200          | —                     |
| POST   | /api/v1/admin/users/{id}/force-password-reset | —              | 200          | PasswordResetResponse |

### Token & Health

| Method | Path                   | Success Code | Response Body  |
| ------ | ---------------------- | ------------ | -------------- |
| GET    | /health                | 200          | HealthResponse |
| GET    | /.well-known/jwks.json | 200          | JwksResponse   |
| GET    | /api/v1/tokens/claims  | 200          | TokenClaims    |

### Test Support (x-test-only)

| Method | Path                       | Success Code | Response Body |
| ------ | -------------------------- | ------------ | ------------- |
| POST   | /api/v1/test/reset-db      | 200          | —             |
| POST   | /api/v1/test/promote-admin | 200          | —             |

## Schema Definitions

Based on the canonical types from `demo-fe-ts-nextjs/src/lib/api/types.ts`:

### Core Schemas

**AuthTokens**: `{ accessToken: string, refreshToken: string, token_type: string }`

**User**: `{ id: string, username: string, email: string, displayName: string, status: string, roles: string[], createdAt: string, updatedAt: string }`

**Expense**: `{ id: string, amount: string, currency: string, category: string, description: string, date: string, type: string, quantity?: number, unit?: string, userId: string, createdAt: string, updatedAt: string }`

**ExpenseSummary**: `{ currency: string, totalIncome: string, totalExpense: string, net: string, categories: CategoryBreakdown[] }`

**Pagination Envelope**: `{ content: T[], totalElements: number, totalPages: number, page: number, size: number }`

### Request Schemas

**RegisterRequest**: `{ username: string, email: string, password: string }`

**LoginRequest**: `{ username: string, password: string }`

**RefreshRequest**: `{ refreshToken: string }`

**CreateExpenseRequest**: `{ amount: string, currency: string, category: string, description: string, date: string, type: string, quantity?: number, unit?: string }`

**UpdateExpenseRequest**: All fields optional version of CreateExpenseRequest

**UpdateProfileRequest**: `{ displayName: string }`

**ChangePasswordRequest**: `{ oldPassword: string, newPassword: string }`

**DisableRequest**: `{ reason: string }`

### Response-Only Schemas

**PLReport**: `{ startDate: string, endDate: string, currency: string, totalIncome: string, totalExpense: string, net: string, incomeBreakdown: CategoryBreakdown[], expenseBreakdown: CategoryBreakdown[] }`

**CategoryBreakdown**: `{ category: string, type: string, total: string }`

**Attachment**: `{ id: string, filename: string, contentType: string, size: number, createdAt: string }`

**TokenClaims**: `{ sub: string, iss: string, exp: number, iat: number, roles: string[] }`

**JwksResponse**: `{ keys: JwkKey[] }` where JwkKey = `{ kty: string, kid: string, use: string, n: string, e: string }`

**HealthResponse**: `{ status: string }`

**PasswordResetResponse**: `{ token: string }`

**ErrorResponse**: `{ error: string, message: string }` (standardized)

## Design Decisions

### Decision 1: Modular YAML Over Single File

**Context**: OpenAPI specs can be written as a single monolithic file or split into modules.

**Decision**: Use modular YAML with `$ref` references, split by domain.

**Rationale**:

- Domain-split files mirror existing Gherkin spec organization (`be/gherkin/authentication/`,
  `be/gherkin/expenses/`, etc.)
- Smaller files are easier to review in PRs
- Multiple developers can work on different domains without merge conflicts
- Redocly CLI handles `$ref` resolution into a single bundled file for consumers

**Alternatives Considered**:

- Single `openapi.yaml` — rejected, too large (~800-1000 lines) for easy review
- Per-endpoint files — rejected, too granular (30+ files just for paths)

### Decision 2: Test-Time Only Enforcement

**Context**: Contract validation can run at build time, test time, or runtime in production.

**Decision**: Enforce contracts only at test time (unit, integration, E2E). No production runtime
validation.

**Rationale**:

- Test-time catches all drift before merge
- No performance overhead in production
- No additional production dependencies
- Consistent with existing three-level testing standard

### Decision 3: Generated Types for Frontends

**Context**: Frontend types can be hand-written or generated from the contract.

**Decision**: Generate types from the contract using `openapi-typescript` (TS) and
`openapi_generator_cli` (Dart). Generated types replace hand-written types.

**Rationale**:

- Eliminates the possibility of hand-written types drifting from the contract
- Single source of truth (contract) rather than two sources (contract + hand-written types)
- Generation is fast (milliseconds for `openapi-typescript`)
- Generated types are committed to the repo for easy review

### Decision 4: Test-Only Endpoints Use OpenAPI Extension

**Context**: `/api/v1/test/*` endpoints exist only for testing and shouldn't appear in production
API documentation.

**Decision**: Mark test-only endpoints with `x-test-only: true` OpenAPI extension. Spectral rule
warns if test endpoints lack this extension.

**Rationale**:

- Keeps all endpoints in a single spec (no separate test spec)
- Documentation generators can filter out `x-test-only` endpoints
- Contract validation still covers test endpoints
