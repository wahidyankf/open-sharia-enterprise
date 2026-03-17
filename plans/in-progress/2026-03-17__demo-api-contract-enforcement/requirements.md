# Requirements

## Problem

### What Is Breaking

This monorepo has 11 backend implementations (Go, Java x2, Kotlin, Python, Rust, Elixir, F#, C#,
TypeScript, Dart, Clojure), 3 frontends (Next.js, TanStack Start, Flutter Web), and 2 E2E test
suites — all implementing the same REST API. There is **no machine-readable contract** defining the
exact shape of every request and response.

**Symptoms**:

1. **Types are duplicated 14 times** — each backend has its own DTOs/structs, each frontend has its
   own `types.ts` / Dart models. Nothing enforces they are identical.
2. **Drift is invisible** — a naming mismatch (e.g., `token_type` vs `tokenType`) can only be
   caught by E2E tests, and only if a Gherkin scenario asserts that specific field.
3. **Gherkin specs define behavior, not shape** — 76 backend + 92 frontend scenarios specify _what_
   the API does, but not the exact field names, types, nullability, or constraints of every payload.
4. **No documentation** — there is no browsable reference showing all endpoints, their schemas, and
   examples. Product and stakeholders must read code or ask developers.
5. **Adding a field requires updating 14 places** — and forgetting one is undetectable until an E2E
   test exercises that path (if one exists).

### Impact

- API drift between backends causes frontend failures that surface late (in E2E, or worse, in
  production)
- Onboarding new backends/frontends requires reverse-engineering the API shape from existing code
- No single source of truth for what the API looks like

---

## Solution Space

### What a Solution Must Do

Any viable solution must satisfy these **hard requirements**:

1. **Machine-readable contract** — a specification that tools can parse, not just documentation
2. **Code generation** — auto-generate types + encoders/decoders in each app's language so that
   mismatches are caught at compile time (or test time for dynamic languages)
3. **All 11 backend languages covered** — Go, Java, Kotlin, Python, Rust, Elixir, F#, C#,
   TypeScript, Dart, Clojure
4. **Full HTTP semantics** — the contract must express methods, paths, status codes, headers, and
   request/response bodies (not just data shapes)
5. **Fits existing CI** — violations caught by `nx affected -t typecheck`, `lint`, `test:quick`
   (pre-push hook + PR quality gate). No new workflows or paradigm shifts.
6. **Generated code is gitignored** — contract is the sole source of truth
7. **Open source and free** — all tools in the critical path must have OSI-approved licenses.
   No commercial dependencies or paid tiers required.
8. **Browsable documentation** — HTML docs viewable by product/stakeholders/public
9. **Strict camelCase** — all JSON field names use camelCase, zero exceptions
10. **No API paradigm change** — must work with the existing REST/JSON API, not require migration
    to GraphQL, gRPC, or another protocol

### What a Solution Should Do (nice-to-have)

1. Language-agnostic authoring — the contract should not privilege one language
2. No new DSL to learn — leverage formats the team already knows
3. Example request/response pairs as documentation and test fixtures
4. Cacheable code generation via Nx
5. Minimal runtime overhead in generated code

---

## Solution Alternatives

This section evaluates 10 API contract frameworks. Research conducted 2026-03-17 via web search
and official documentation review.

### Category A: REST-Native Specification Formats

These frameworks were designed specifically to describe REST/JSON APIs.

---

#### Alternative 1: OpenAPI 3.1 (Single YAML) + Runtime Validators

**Approach**: Single monolithic `openapi.yaml` with runtime validators in each project's tests.
No code generation — each app manually maintains its own types and validates against the spec at
test time using language-specific JSON Schema validators.

**Current status**: OpenAPI 3.2.0 released September 2025. Fully backward-compatible with 3.1.
Adds streaming, tag hierarchy, QUERY method. Actively maintained by the OpenAPI Initiative (Linux
Foundation). Dominant industry standard.

**Pros**:

- Industry standard with the largest tooling ecosystem of any API specification format
- Human-readable YAML; JSON Schema 2020-12 compatible (since 3.1)
- Every language has a JSON Schema validation library (ajv, gojsonschema, etc.)
- Massive community: 20K+ GitHub stars on openapi-generator, millions of users

**Cons**:

- Large single file becomes unmanageable at scale (hundreds of endpoints = thousands of lines)
- Runtime-only validation — types are still hand-written in each app, no compile-time safety
- No generated encoders/decoders — each app maintains its own serialization code
- Drift between hand-written types and the spec can go undetected until test execution

**Fails hard requirement**: #2 (no code generation), #6 (types still hand-written)

**Estimated effort**: Medium | **License**: Apache 2.0 (all tools open source)

---

#### Alternative 2: RAML (RESTful API Modeling Language)

**Approach**: Define REST APIs using RAML's YAML-based syntax with traits, resource types, and
type system.

**Current status**: RAML 1.0 released in 2016. **Specification repository archived February 2024.**
No active development. Surviving only within MuleSoft's Anypoint platform (Salesforce).

**Pros**:

- Full REST HTTP semantics (methods, paths, status codes, headers, bodies)
- Clean YAML syntax with type system and traits

**Cons**:

- **Dead specification** — archived in 2024, no active maintenance
- Declining community — most users have migrated to OpenAPI
- Limited code generation ecosystem compared to OpenAPI
- Vendor lock-in risk (MuleSoft/Salesforce is the sole maintainer)

**Fails hard requirement**: #7 (effectively unmaintained; no viable ecosystem)

**Estimated effort**: N/A (not recommended) | **License**: Apache 2.0

---

#### Alternative 3: OpenAPI 3.1 (Modular YAML) + Spectral Linting + Code Generation

**Approach**: Modular OpenAPI spec split by domain using `$ref`. Language-specific code generators
produce types + encoders/decoders in each app's `generated-contracts/` folder. Apps import
generated types; compiler catches mismatches. Spectral lints the spec for style. Redocly generates
browsable documentation.

**Current status**: All tools actively maintained and open source:

- OpenAPI 3.2.0 (September 2025), backward-compatible with 3.1
- Spectral (MIT, ~2.4K GitHub stars) — most widely adopted OpenAPI linter
- Redocly CLI (MIT community edition) — bundling, linting, documentation generation
- Redoc (MIT, ~25K GitHub stars) — most popular OpenAPI documentation renderer
- openapi-generator (Apache 2.0, ~22K GitHub stars) — multi-language code generation
- oapi-codegen (Apache 2.0, ~6.3K GitHub stars) — Go-specific, most popular Go generator
- @hey-api/openapi-ts (MIT) — modern TS codegen used by Vercel, PayPal
- NSwag (MIT) — .NET/F# code generation
- datamodel-code-generator (MIT) — Python Pydantic model generation

**Pros**:

- All HTTP semantics in one spec (paths, methods, status codes, body schemas)
- Modular structure mirrors Gherkin domain organization
- Code generation produces compile-time-safe types in all 11 languages
- Generated encoders/decoders handle serialization type-safely
- Spectral linting enforces naming conventions (camelCase, descriptions, examples)
- `generated-contracts/` gitignored — contract is sole source of truth
- Violations caught by existing pre-push hook and PR quality gate
- Browsable API documentation via Redoc (no separate tooling needed)
- **Every tool in the critical path is open source and free** (MIT or Apache 2.0)
- Largest ecosystem: most tutorials, Stack Overflow answers, IDE plugins
- No new DSL — YAML is already used throughout this repo

**Cons**:

- More files than single YAML (but each is small and domain-focused)
- Code generator per language (9 off-the-shelf + 2 custom libs in `libs/`)
- Dynamic languages (Elixir, Clojure) enforce at test time rather than compile time
- openapi-generator has 4,500+ open issues; generated code quality varies by language target
- Raw YAML authoring is more verbose than TypeSpec or Zod DSLs

**Meets all hard requirements**: Yes

**Estimated effort**: Medium-High | **License**: All MIT or Apache 2.0

---

### Category B: Data-Shape-Only Formats

These frameworks define data shapes but lack HTTP semantics (methods, paths, status codes).

---

#### Alternative 4: JSON Schema (Standalone) + Test-Time Validation

**Approach**: JSON Schema 2020-12 files per endpoint, validated at test time using ajv (JS),
jsonschema (Python), json-schema (Go), etc. No OpenAPI wrapper — just data shape definitions.

**Current status**: JSON Schema 2020-12 is the latest stable draft. Actively maintained. GSoC 2025
projects improving tooling. Foundational technology underlying OpenAPI 3.1+.

**Pros**:

- Simpler than OpenAPI — focuses purely on data shapes
- Every language has a JSON Schema library
- Quicktype can generate types for 8 of 11 languages
- Well-understood, widely adopted standard

**Cons**:

- No HTTP semantics — cannot express methods, paths, status codes, headers
- No API documentation generation (only data model docs)
- No code generation for Elixir, F#, or Clojure (3 of 11 languages missing)
- Requires a separate mechanism to map schemas to endpoints
- Many files with no standard organizational structure

**Fails hard requirements**: #3 (missing 3 languages), #4 (no HTTP semantics), #8 (no API docs)

**Estimated effort**: Medium | **License**: Public domain (spec); open source (tools)

---

### Category C: Language-Specific Source of Truth

These frameworks use one programming language as the canonical definition and generate artifacts
for other languages. They privilege one language over others.

---

#### Alternative 5: TypeScript Types + `typescript-json-schema`

**Approach**: Canonical TypeScript interfaces as the single source of truth.
`typescript-json-schema` or `ts-json-schema-generator` generates JSON Schemas from TS types,
which other languages consume for validation.

**Current status**: typescript-json-schema (v0.65.1) and ts-json-schema-generator (v2.4.0) are
actively maintained.

**Pros**:

- TypeScript types already exist in `demo-fe-ts-nextjs` — low authoring friction
- TypeScript is expressive (unions, intersections, generics, mapped types)
- Generated JSON Schema bridges to other languages

**Cons**:

- TypeScript-centric — wrong source of truth for a polyglot repo where most backends aren't TS
- No HTTP semantics (paths, methods, status codes, headers)
- Requires a build step (TS compilation) before other languages can consume the schema
- Non-TS developers must understand TypeScript syntax to modify the contract
- Only generates types, not encoders/decoders for non-TS languages
- Creates a false hierarchy where one language "owns" the contract

**Fails hard requirements**: #4 (no HTTP semantics); also fails nice-to-have #1 (not
language-agnostic)

**Estimated effort**: Low-Medium | **License**: MIT/Apache 2.0 (all open source)

---

#### Alternative 6: Zod Schemas + `zod-to-openapi` Bridge

**Approach**: Define API schemas in Zod (TypeScript). Use `@asteasolutions/zod-to-openapi` or
`zod-openapi` to generate an OpenAPI spec. Other languages consume the generated OpenAPI spec
via standard code generators.

**Current status**: Zod v4 (2025) is one of the most popular TypeScript validation libraries
(~35K GitHub stars). `zod-to-openapi` is actively maintained.

**Pros**:

- Zod gives runtime + compile-time safety for TypeScript projects
- Single definition produces both validation logic AND OpenAPI spec
- OpenAPI output enables the full ecosystem of code generators for other languages
- Runtime validation (z.parse) + static type inference (z.infer) from one schema

**Cons**:

- TypeScript-centric — non-TS devs must read Zod syntax to modify the contract
- Two layers of abstraction: Zod → OpenAPI → per-language codegen
- Zod → OpenAPI conversion can lose semantics (some Zod features don't map cleanly)
- Not language-neutral: creates a privileged position for TypeScript
- Requires TypeScript build step before any other language can regenerate

**Fails nice-to-have**: #1 (not language-agnostic), #2 (new DSL — Zod syntax)

**Estimated effort**: Medium | **License**: MIT (all open source)

---

### Category D: Higher-Level DSLs That Compile to REST Specs

These frameworks provide a custom language that compiles to OpenAPI, protobuf, or other formats.

---

#### Alternative 7: TypeSpec (Microsoft) → OpenAPI

**Approach**: Write API definitions in TypeSpec's TypeScript-like DSL. TypeSpec compiles to
OpenAPI 3.0/3.1 (and optionally protobuf, JSON Schema). Use the generated OpenAPI spec with
standard per-language code generators.

**Current status**: TypeSpec 1.0 GA (stable core). Actively maintained by Microsoft. AutoRest
deprecated effective July 2026 in favor of TypeSpec. Powers all Azure API definitions internally.
~4.5K GitHub stars.

**Native code generation** (direct from TypeSpec, no OpenAPI intermediate):

| Language   | Client  | Server | Status  |
| ---------- | ------- | ------ | ------- |
| C#/.NET    | Preview | Yes    | Preview |
| Java       | Preview | No     | Preview |
| Python     | Preview | No     | Preview |
| TypeScript | Preview | No     | Preview |
| Go         | No      | No     | Planned |
| Others     | No      | No     | N/A     |

For 7 of 11 languages (Go, Kotlin, Rust, Elixir, F#, Dart, Clojure), TypeSpec emits OpenAPI
which then feeds into the same code generators as Alternative 3.

**Pros**:

- Excellent developer experience: TypeScript-like syntax is more readable than YAML for complex
  schemas (generics, templates, decorators)
- Built-in linter with configurable rules; VS Code extension with real-time feedback
- Compiles to multiple formats: OpenAPI 3.0/3.1, JSON Schema 2020-12, Protobuf
- Growing rapidly; backed by Microsoft
- Custom emitters can be written for specialized output

**Cons**:

- Direct code generation only for 4 languages (C#, Java, Python, TS) and all in preview
- For 7 of 11 languages, you still go through OpenAPI — adding an extra compilation step with no
  benefit over writing OpenAPI directly
- Microsoft/Azure-centric ecosystem — most examples and plugins target Azure patterns
- Adds a build step before other tools can consume the spec (TypeSpec → OpenAPI → codegen)
- Relatively new; smaller community than OpenAPI (4.5K vs 20K+ stars)
- Team must learn a new DSL; OpenAPI YAML is more universally understood

**Meets hard requirements**: Yes (via OpenAPI output), but adds unnecessary complexity
**Fails nice-to-have**: #2 (new DSL to learn)

**Estimated effort**: Medium-High | **License**: MIT (all open source)

---

#### Alternative 8: Smithy (AWS) + Code Generation

**Approach**: Define API models in Smithy's IDL (Interface Definition Language). Use
smithy-build to generate clients and servers. Protocol-agnostic — supports REST/JSON, AWS
protocols, and custom protocols.

**Current status**: Smithy 2.0, actively maintained by AWS. Powers all AWS SDKs and CLI tools
since 2018. ~2.5K GitHub stars. Growing adoption outside AWS (Disney+).

**Code generation coverage**:

| Language    | Client | Server | Status                    |
| ----------- | ------ | ------ | ------------------------- |
| Go          | Yes    | No     | Stable (AWS SDK)          |
| Java        | Yes    | Yes    | Stable (AWS SDK)          |
| Kotlin      | Yes    | No     | Stable                    |
| Python      | Yes    | No     | Stable                    |
| Rust        | Yes    | Yes    | Stable (AWS SDK)          |
| TypeScript  | Yes    | Yes    | Stable                    |
| **Elixir**  | No     | No     | **Not available**         |
| **F#**      | No     | No     | **Not available**         |
| **C#**      | No     | No     | **Not available (Dafny)** |
| **Dart**    | No     | No     | **Not available**         |
| **Clojure** | No     | No     | **Not available**         |

**Pros**:

- Most sophisticated code generation — produces production-grade SDKs with retry logic, auth,
  pagination
- Protocol-agnostic: same model supports REST, gRPC, MQTT, custom protocols
- Schema evolution and breaking change detection built into the toolchain
- Used at massive scale (all AWS services)

**Cons**:

- **Missing 5 of 11 languages** (Elixir, F#, C#, Dart, Clojure) — disqualifying
- AWS-centric ecosystem — most examples and plugins target AWS patterns
- Steep learning curve: Smithy's IDL and trait system are more complex than OpenAPI YAML
- Java dependency for the build toolchain (Gradle/Maven-based)
- Niche community outside AWS — harder to find help and third-party tools
- Overkill for a demo expense tracker API

**Fails hard requirement**: #3 (missing 5 of 11 languages)

**Estimated effort**: High | **License**: Apache 2.0 (all open source)

---

### Category E: Non-REST Paradigms

These frameworks define API contracts using a fundamentally different paradigm than REST.

---

#### Alternative 9: Protocol Buffers (Protobuf) + gRPC-Gateway

**Approach**: `.proto` files define message types and service interfaces. `protoc` with
language-specific plugins generates types + serialization code. `gRPC-Gateway` or `Connect-RPC`
exposes REST/JSON endpoints from protobuf definitions.

**Current status**: Protobuf v3 (proto3 syntax) actively maintained by Google. gRPC-Gateway v2.x
maintained by community. Buf.build provides modern tooling (linting, breaking change detection).
Connect-RPC (by Buf) provides native HTTP/JSON without a gateway proxy.

**Code generation coverage** (via `protoc` + plugins):

| Language   | Support   | Tool                               |
| ---------- | --------- | ---------------------------------- |
| Go         | Official  | protoc-gen-go                      |
| Java       | Official  | protoc-gen-java                    |
| Kotlin     | Official  | protoc-gen-java (via Java interop) |
| Python     | Official  | protoc-gen-python                  |
| Rust       | Community | tonic + prost                      |
| C#         | Official  | protoc-gen-csharp                  |
| TypeScript | Official  | protobuf-es (Buf), Connect-RPC     |
| Dart       | Official  | protoc-gen-dart                    |
| Elixir     | Community | protobuf-elixir, elixir-grpc       |
| F#         | Community | FSharp.GrpcCodeGenerator           |
| Clojure    | Community | Protojure                          |

**Pros**:

- Covers all 11 languages (Elixir, F#, Clojure via community plugins)
- Precise type system with strict schema evolution rules (field numbering)
- Generated code includes full binary AND JSON serialization/deserialization
- Enterprise-grade: used by Google, Netflix, Uber, Square
- Buf CLI provides excellent linting, formatting, and breaking change detection

**Cons**:

- Fundamental conceptual mismatch with REST/JSON — protobuf is an RPC/message format
- Does NOT natively express HTTP concepts (paths, status codes, headers) — requires annotations
- Heavy tooling: `protoc` compiler + per-language plugins + gateway proxy
- Binary-first serialization — JSON support is secondary
- Would require rewriting the API layer (not a contract overlay)
- Community plugins for Elixir/F#/Clojure are less mature than official ones

**Fails hard requirements**: #5 (doesn't fit existing CI — needs gRPC infra), #10 (requires
paradigm change)

**Estimated effort**: High | **License**: BSD-3-Clause (protobuf); Apache 2.0 (gRPC, Buf)

---

#### Alternative 10: GraphQL Schema + Code Generation

**Approach**: Define the API as a GraphQL schema (SDL). Use GraphQL code generators to produce
typed clients, resolvers, and query builders. Fundamentally replaces REST with GraphQL.

**Current status**: GraphQL spec October 2021 (with September 2025 refresh adding `OneOf`).
Maintained by the GraphQL Foundation (Linux Foundation). ~50% enterprise adoption.

**Code generation coverage**:

| Language   | Client | Server | Key Tool                    |
| ---------- | ------ | ------ | --------------------------- |
| Go         | Yes    | Yes    | gqlgen (schema-first)       |
| Java       | Yes    | Yes    | DGS Framework, graphql-java |
| Kotlin     | Yes    | Yes    | GraphQL Kotlin (Expedia)    |
| Python     | Yes    | Yes    | Strawberry, Ariadne         |
| Rust       | Yes    | Yes    | juniper, async-graphql      |
| C#         | Yes    | Yes    | Hot Chocolate               |
| TypeScript | Yes    | Yes    | GraphQL Code Generator      |
| Dart       | Yes    | Yes    | Ferry, Artemis              |
| Elixir     | Yes    | Yes    | Absinthe                    |
| Clojure    | Yes    | Yes    | Lacinia                     |
| F#         | No     | No     | Uses C# libraries via .NET  |

**Pros**:

- Strong type system with introspection — the schema IS the contract
- Covers 10 of 11 languages with mature code generation
- Excellent for frontend-driven APIs with complex data requirements
- GraphiQL provides interactive API exploration

**Cons**:

- **Fundamentally different paradigm from REST** — single endpoint (POST /graphql), no HTTP
  methods/status codes/paths. Not a contract overlay for REST APIs.
- Would require rewriting all 11 backends from REST to GraphQL
- Existing Gherkin specs are REST-based (POST /api/v1/auth/login, etc.)
- HTTP caching doesn't work (all requests are POST to same endpoint)
- Server complexity: N+1 queries, query depth limits, query cost analysis
- F# support requires going through C# libraries via .NET interop

**Fails hard requirements**: #5 (doesn't fit existing CI), #10 (requires paradigm migration)

**Estimated effort**: Very High | **License**: MIT (spec and all major tools)

---

### Eliminated Without Full Evaluation

**API Blueprint** (Apiary): **Effectively dead.** Apiary shut down mid-2025. GitHub repo
unmaintained. No active tooling. Not viable for any new project.

**AsyncAPI** (v3.1.0, January 2026): Designed for event-driven/asynchronous APIs (Kafka, MQTT,
WebSockets, AMQP). Explicitly complements OpenAPI for REST rather than replacing it. Not relevant
for REST API contract enforcement.

---

## Analysis

### Language Coverage Matrix

Coverage of all 11 backend languages by each framework's code generation tooling:

| Language   | Alt 1: OAS Single  | Alt 3: OAS Modular       | Alt 4: JSON Schema | Alt 5: TS Types | Alt 6: Zod   | Alt 7: TypeSpec | Alt 8: Smithy | Alt 9: Protobuf | Alt 10: GraphQL |
| ---------- | ------------------ | ------------------------ | ------------------ | --------------- | ------------ | --------------- | ------------- | --------------- | --------------- |
| Go         | Validate           | oapi-codegen             | quicktype          | via JSON Schema | via OAS      | via OAS         | Stable        | Official        | gqlgen          |
| Java       | Validate           | openapi-generator        | quicktype          | via JSON Schema | via OAS      | Preview         | Stable        | Official        | DGS             |
| Kotlin     | Validate           | openapi-generator        | quicktype          | via JSON Schema | via OAS      | via OAS         | Stable        | Official        | GraphQL Kotlin  |
| Python     | Validate           | datamodel-code-generator | quicktype          | via JSON Schema | via OAS      | Preview         | Stable        | Official        | Strawberry      |
| Rust       | Validate           | openapi-generator        | quicktype          | via JSON Schema | via OAS      | via OAS         | Stable        | Community       | juniper         |
| Elixir     | Validate           | Custom lib               | **No**             | **No**          | via OAS      | via OAS         | **No**        | Community       | Absinthe        |
| F#         | Validate           | NSwag                    | **No**             | **No**          | via OAS      | via OAS         | **No**        | Community       | **No**          |
| C#         | Validate           | NSwag                    | quicktype          | via JSON Schema | via OAS      | Preview         | **No**        | Official        | Hot Chocolate   |
| TypeScript | Validate           | @hey-api/openapi-ts      | quicktype          | Native          | Native       | Preview         | Stable        | Official        | GQL Codegen     |
| Dart       | Validate           | openapi-generator        | quicktype          | via JSON Schema | via OAS      | via OAS         | **No**        | Official        | Ferry           |
| Clojure    | Validate           | Custom lib               | **No**             | **No**          | via OAS      | via OAS         | **No**        | Community       | Lacinia         |
| **Total**  | 11 (validate only) | **11/11**                | 8/11               | 8/11            | 11 (via OAS) | 11 (via OAS)    | **6/11**      | 11/11           | **10/11**       |

**Key**: "Validate" = runtime validation only (no codegen). "via OAS" = must go through OpenAPI as
intermediate format (two-step). "Custom lib" = requires `libs/*-openapi-codegen`. **Bold "No"** =
no support.

### Hard Requirements Compliance

| Hard Requirement             | Alt 1 | Alt 2 | Alt 3     | Alt 4 | Alt 5 | Alt 6 | Alt 7    | Alt 8 | Alt 9 | Alt 10 |
| ---------------------------- | ----- | ----- | --------- | ----- | ----- | ----- | -------- | ----- | ----- | ------ |
| 1. Machine-readable contract | Yes   | Yes   | **Yes**   | Yes   | Yes   | Yes   | Yes      | Yes   | Yes   | Yes    |
| 2. Code generation + enc/dec | No    | No    | **Yes**   | No    | No    | Yes\* | Yes      | Yes   | Yes   | Yes    |
| 3. All 11 languages          | Yes†  | Yes†  | **Yes**   | No    | No    | Yes\* | **No**   | Yes\* | Yes\* | No     |
| 4. Full HTTP semantics       | Yes   | Yes   | **Yes**   | No    | No    | Yes   | Yes      | Yes   | No    | No     |
| 5. Fits existing CI          | No    | No    | **Yes**   | No    | Yes   | Yes   | No       | No    | No    | No     |
| 6. Gitignored generated code | N/A   | N/A   | **Yes**   | N/A   | Yes   | Yes   | Yes      | Yes   | Yes   | Yes    |
| 7. Open source and free      | Yes   | Yes   | **Yes**   | Yes   | Yes   | Yes   | Yes      | Yes   | Yes   | Yes    |
| 8. Browsable documentation   | No    | No    | **Yes**   | No    | No    | Yes\* | No       | No    | No    | Yes‡   |
| 9. Strict camelCase enforced | No    | No    | **Yes**   | No    | No    | Yes   | Yes      | Yes   | Yes   | No     |
| 10. No paradigm change       | Yes   | Yes   | **Yes**   | Yes   | Yes   | Yes   | Yes      | Yes   | No    | No     |
| **Hard requirements met**    | 6/10  | 6/10  | **10/10** | 4/10  | 5/10  | 9/10  | **6/10** | 7/10  | 6/10  | 5/10   |

\* Via OpenAPI as intermediate format (two-step process).
† Validate only — no codegen.
‡ Via GraphiQL, not REST API docs.

**Only Alternative 3 meets all 10 hard requirements natively.**

### Recommendation Matrix (Qualitative)

| Criterion                    | Alt 1: OAS Single | Alt 3: OAS Modular   | Alt 4: JSON Schema | Alt 6: Zod | Alt 7: TypeSpec  | Alt 8: Smithy  | Alt 9: Protobuf | Alt 10: GraphQL |
| ---------------------------- | ----------------- | -------------------- | ------------------ | ---------- | ---------------- | -------------- | --------------- | --------------- |
| Compile-time enforcement     | No                | **All static langs** | No                 | TS only    | 4 langs direct   | 6 langs        | All 11          | 10 langs        |
| Generated encoders/decoders  | No                | **Yes**              | Partial            | TS only    | 4 langs direct   | Yes            | Yes             | Yes             |
| Language-agnostic authoring  | Yes               | **Yes (YAML)**       | Yes                | No         | New DSL          | New IDL        | Yes             | New SDL         |
| Works with existing REST CI  | New checks        | **Yes**              | New checks         | Partial    | Extra build step | No (new infra) | No (gRPC)       | No (paradigm)   |
| Complements existing Gherkin | Yes               | **Yes**              | Partial            | Yes        | Yes              | No             | No              | No              |
| No new DSL/paradigm to learn | Yes               | **Yes**              | Yes                | No (Zod)   | No (TypeSpec)    | No (Smithy)    | No (proto3)     | No (GraphQL)    |
| Browsable API docs           | Manual            | **Redoc (built-in)** | No                 | Via OAS    | Via OAS          | Limited        | Limited         | GraphiQL        |
| Ecosystem maturity (2026)    | High              | **Highest**          | High               | Medium     | Growing          | Niche          | High            | High            |

**Eliminated from matrix**: Alt 2 (RAML) — dead spec. Alt 5 (TS Types) — no HTTP semantics.
API Blueprint — dead. AsyncAPI — wrong domain.

### Tool-Specific Comparisons (for Alternative 3)

#### TypeScript OpenAPI Code Generators

| Tool                | License | GitHub Stars | Weekly Downloads | Runtime Decoders | SDK Client | TanStack Query | Key Strength                         |
| ------------------- | ------- | ------------ | ---------------- | ---------------- | ---------- | -------------- | ------------------------------------ |
| @hey-api/openapi-ts | MIT     | ~4K          | Growing fast     | Zod plugin       | Yes        | Plugin         | 20+ plugins, used by Vercel/PayPal   |
| openapi-typescript  | MIT     | ~8K          | ~2.5M            | No (types only)  | Separate   | No             | Lightweight, minimal runtime         |
| Orval               | MIT     | ~5.5K        | ~837K            | Zod output       | Yes        | Built-in       | Simplest TanStack Query integration  |
| swagger-codegen     | Apache  | Legacy       | Declining        | No               | Yes        | No             | Legacy; openapi-generator supersedes |

**Recommendation**: `@hey-api/openapi-ts` — best plugin ecosystem (Zod + TanStack Query for both
Next.js and TanStack Start frontends), production-proven, actively maintained.

#### Go OpenAPI Code Generators

| Tool              | License | GitHub Stars | Server Frameworks               | Key Strength                                           |
| ----------------- | ------- | ------------ | ------------------------------- | ------------------------------------------------------ |
| oapi-codegen      | Apache  | ~6.3K        | Gin, Chi, Echo, Fiber, net/http | Most popular, broadest framework support               |
| ogen              | Apache  | ~1.6K        | stdlib (net/http)               | Fastest, strongest type safety, OpenTelemetry built-in |
| openapi-generator | Apache  | ~22K (all)   | Various                         | Same tool for all languages                            |

**Recommendation**: `oapi-codegen` — broadest framework support (Gin is our framework), most
popular in the Go ecosystem, no Java dependency (unlike openapi-generator).

#### OpenAPI Linters

| Tool        | License | Language | Performance                 | Key Strength                                            |
| ----------- | ------- | -------- | --------------------------- | ------------------------------------------------------- |
| Spectral    | MIT     | Node.js  | Slower on large docs        | Most adopted, highly configurable, custom functions     |
| Vacuum      | MIT     | Go       | ~3x faster than Spectral    | Fastest, zero deps (single binary), Spectral-compatible |
| Redocly CLI | MIT\*   | Node.js  | Between Spectral and Vacuum | Combined linting + bundling + docs in one tool          |

\* Redocly CLI community edition is MIT; some advanced features require paid license but are NOT
needed for our use case (linting, bundling, and `build-docs` are all free).

**Recommendation**: `Spectral` for linting (most configurable, custom camelCase rules) +
`Redocly CLI` for bundling and documentation generation. Both are free and open source for our
needs.

#### API Documentation Renderers

| Tool               | License | GitHub Stars | Style                      | Key Strength                           |
| ------------------ | ------- | ------------ | -------------------------- | -------------------------------------- |
| Redoc              | MIT     | ~25K         | Three-panel "Stripe-like"  | Most polished, handles complex schemas |
| Swagger UI         | Apache  | ~26K         | Interactive "try it out"   | Live API requests from browser         |
| Stoplight Elements | Apache  | ~1.6K        | Web components, embeddable | Stalled since SmartBear acquisition    |

**Recommendation**: `Redoc` via `@redocly/cli build-docs` — best visual quality, MIT licensed,
generates static HTML (no hosting infrastructure needed), same tool already used for bundling.

---

## Solution

### Recommended Approach: Alternative 3 — OpenAPI 3.1 Modular + Spectral + Code Generation

Alternative 3 is the **only framework that meets all 10 hard requirements** without requiring
paradigm changes, new DSLs, or intermediate compilation steps.

**Why Alternative 3**:

1. **Only framework covering all 11 languages with codegen** — Protobuf also covers all 11 but
   requires a paradigm shift to gRPC. OpenAPI is the only option that works with our existing REST
   APIs and all 11 backend languages.

2. **Compile-time + runtime safety** — generated types catch field name mismatches at build time;
   generated runtime decoders (Zod for TS frontends, Effect Schema for TS Effect backend, Pydantic
   for Python, etc.) catch shape/type mismatches at runtime. Defense in depth.

3. **Encoders/decoders included** — generated code handles JSON serialization/deserialization
   type-safely (Zod `z.parse()` for TS, Jackson for Java, serde for Rust, Pydantic for Python).

4. **Fits existing CI** — `nx affected -t typecheck` and `test:quick` already run in pre-push hook
   and PR quality gate. No new CI steps needed. No paradigm shift.

5. **No new DSL to learn** — OpenAPI YAML is the most widely understood API specification format.
   TypeSpec, Smithy, Zod, and GraphQL all require learning a new language. YAML is already used
   throughout this repo (Nx configs, GitHub Actions, Hugo frontmatter, docker-compose).

6. **Gitignored** — `generated-contracts/` is not committed. The OpenAPI spec is the sole source of
   truth. Generated code is a build artifact.

7. **Browsable documentation** — Redoc generates polished, responsive HTML documentation from the
   same spec that drives code generation. Always in sync.

8. **100% open source and free** — every tool in the critical path is MIT or Apache 2.0 licensed.
   No commercial dependencies, no paid tiers needed.

9. **Minimal runtime overhead** — Zod adds ~12kb to TS frontends (acceptable for API validation).
   Non-TS languages use their native serialization (zero additional overhead).

### Why Not the Runners-Up

**Why not Protobuf (Alt 9)?** — Protobuf covers all 11 languages but is fundamentally an
RPC/message format, not a REST API description. Using it for existing REST endpoints requires
gRPC-Gateway or Connect-RPC, adding complexity without benefit. Our Gherkin specs test REST
endpoints; protobuf would require rewriting the API layer.

**Why not TypeSpec (Alt 7)?** — TypeSpec offers better authoring DX than raw YAML, but only
generates code directly for 4 of 11 languages (all in preview). For the other 7, you still go
through OpenAPI — adding an extra compilation step with no benefit over writing OpenAPI directly.
If the team later finds YAML authoring burdensome, TypeSpec can be adopted as an authoring layer
on top of Alternative 3 without changing the downstream toolchain.

**Why not GraphQL (Alt 10)?** — GraphQL is a fundamentally different API paradigm (single endpoint,
queries/mutations, no HTTP verbs). Adopting it would require rewriting all 11 backends and all
Gherkin specs. This plan is about contract enforcement for existing REST APIs, not API paradigm
migration.

**Why not Smithy (Alt 8)?** — Missing 5 of 11 languages (Elixir, F#, C#, Dart, Clojure) with no
community plugins available. Disqualifying for this repo.

### Selected Toolchain

| Role                  | Tool                         | License    | Why                                          |
| --------------------- | ---------------------------- | ---------- | -------------------------------------------- |
| Spec format           | OpenAPI 3.1                  | Apache 2.0 | Industry standard, all 11 languages          |
| Linting               | Spectral                     | MIT        | Most configurable, custom camelCase rules    |
| Bundling              | Redocly CLI                  | MIT        | Resolves `$ref`, produces YAML + JSON        |
| Documentation         | Redoc                        | MIT        | Best visual quality, static HTML             |
| Go codegen            | oapi-codegen                 | Apache 2.0 | Gin support, most popular Go generator       |
| Java/Kotlin/Rust/Dart | openapi-generator            | Apache 2.0 | Multi-language, mature                       |
| F#/C# codegen         | NSwag                        | MIT        | Best .NET/F# support                         |
| TS codegen            | @hey-api/openapi-ts          | MIT        | Zod + TanStack Query plugins, used by Vercel |
| Python codegen        | datamodel-code-generator     | MIT        | Pydantic v2 models                           |
| Elixir codegen        | libs/elixir-openapi-codegen  | Custom     | No off-the-shelf tool exists                 |
| Clojure codegen       | libs/clojure-openapi-codegen | Custom     | No off-the-shelf tool exists                 |
| E2E validation        | ajv                          | MIT        | JSON Schema validation in Playwright tests   |

---

## Objectives

**Primary Objectives**:

1. Create a machine-readable OpenAPI 3.1 contract covering all demo application endpoints
2. Auto-generate type-safe code (types + encoders/decoders) into each app's `generated-contracts/`
   folder from the contract
3. Apps import generated types; mismatches fail at compile time (`typecheck`/`build`)
4. Generated folders are gitignored — code is regenerated via `nx run <app>:codegen`
5. Violations caught by existing PR quality gate (`nx affected -t typecheck`, `lint`, `test:quick`)
6. All JSON fields use **strict camelCase** — no snake_case, no kebab-case, zero exceptions
7. Generate browsable API documentation (Redoc) viewable by public/product/any team

**Secondary Objectives**:

1. Provide example request/response pairs as documentation and test fixtures
2. Enable future deployment of API docs to a public URL

---

## User Stories

**Story 1: Backend Developer Adding a New Field**

```gherkin
Feature: Contract prevents unnoticed API drift
  As a backend developer
  I want generated types to enforce the contract shape
  So that all implementations stay in sync

Scenario: Adding a field without updating the contract
  Given the OpenAPI contract defines the Expense response schema
  And the generated Go struct does not include a "tags" field
  When I try to set response.Tags in demo-be-golang-gin
  Then the Go compiler should fail because the field does not exist
  And I must update the contract and re-run codegen to add it

Scenario: Removing a required field
  Given the generated Python Pydantic model requires "currency"
  When I remove "currency" from demo-be-python-fastapi's handler return
  Then Pydantic validation should fail in test:unit
  And pre-push hook catches this via test:quick
```

**Story 2: Frontend Developer Consuming API Types**

```gherkin
Feature: Frontend types are auto-generated from contract
  As a frontend developer
  I want generated types with encoders/decoders
  So that my API calls are type-safe

Scenario: Contract change updates frontend types
  Given the OpenAPI contract adds an optional "tags" field to Expense
  When I run nx run demo-fe-ts-nextjs:codegen
  Then the generated types include "tags?: string[]"
  And the generated Zod schema includes the new field for runtime validation
  And TypeScript compilation succeeds

Scenario: Frontend code references non-existent field
  Given the generated types do not include a "notes" field on Expense
  When I reference expense.notes in demo-fe-ts-nextjs
  Then tsc should produce a compile error
  And pre-push hook blocks the push via typecheck
```

**Story 3: Contract Change Triggers Regeneration**

```gherkin
Feature: Nx dependency graph triggers codegen cascade
  As a developer
  I want contract changes to regenerate all consumer code
  So that no project silently falls out of compliance

Scenario: Modifying a schema triggers codegen for all apps
  Given I modify specs/apps/demo/contracts/schemas/expense.yaml
  When Nx computes affected projects
  Then demo-contracts:bundle runs first
  Then all demo-be-* and demo-fe-* codegen targets run
  Then typecheck/build/test:quick runs against the new generated code
  And any mismatch fails the PR quality gate
```

**Story 4: Generated Code is Not Committed**

```gherkin
Feature: Generated contracts are gitignored
  As a developer
  I want generated-contracts/ folders to be gitignored
  So that the repo stays clean and the contract is the only source of truth

Scenario: Fresh clone regenerates all contract code
  Given a developer clones the repository
  When they run npm install (which triggers postinstall codegen)
  Then all generated-contracts/ folders are populated
  And typecheck/build succeeds immediately

Scenario: Generated code is excluded from git
  Given apps/demo-be-golang-gin/generated-contracts/ exists locally
  When I run git status
  Then generated-contracts/ should not appear as untracked
```

**Story 5: Product Team Views API Documentation**

```gherkin
Feature: Browsable API documentation from the contract
  As a product manager or stakeholder
  I want to browse the API documentation
  So that I can understand available endpoints without reading code

Scenario: Generate API documentation locally
  Given the OpenAPI contract is valid
  When I run nx run demo-contracts:docs
  Then a browsable HTML page is generated at specs/apps/demo/contracts/generated/docs/index.html
  And it shows all endpoints with request/response schemas
  And it includes example request/response pairs
  And test-only endpoints (/api/v1/test/*) are excluded

Scenario: Documentation reflects latest contract
  Given I add a new "tags" field to the Expense schema
  When I run nx run demo-contracts:docs
  Then the Expense section in the docs shows the new "tags" field
```

---

## Acceptance Criteria

```gherkin
Feature: API contract enforcement via code generation

  Scenario: Contract spec exists and is valid
    Given the file specs/apps/demo/contracts/openapi.yaml exists
    When Spectral lints the OpenAPI specification
    Then there should be zero errors
    And the spec should cover all endpoints from the Gherkin features

  Scenario: Each app has a codegen target
    Given every demo-be-* and demo-fe-* project has a "codegen" Nx target
    When nx run <app>:codegen runs
    Then a generated-contracts/ folder is created with language-specific types
    And the folder contains encoders and decoders for each schema

  Scenario: Generated folders are gitignored
    Given the root .gitignore includes **/generated-contracts/ and **/generated_contracts/
    When a developer runs git status after codegen
    Then no generated-contracts/ folder appears as untracked

  Scenario: Backend compile-time enforcement (statically typed)
    Given demo-be-golang-gin uses generated Go structs as handler return types
    When the contract changes and codegen re-runs
    Then any handler returning the old shape fails compilation
    And this is caught by nx affected -t test:quick before push

  Scenario: Backend test-time enforcement (dynamically typed)
    Given demo-be-elixir-phoenix uses generated structs with @enforce_keys
    When the contract changes and codegen re-runs
    Then any handler returning the old shape fails in test:unit
    And this is caught by nx affected -t test:quick before push

  Scenario: Frontend compile-time enforcement
    Given demo-fe-ts-nextjs imports generated TypeScript types
    When the contract changes and codegen re-runs
    Then any component using old field names fails tsc
    And this is caught by nx affected -t typecheck before push

  Scenario: PR quality gate catches violations
    Given a PR modifies specs/apps/demo/contracts/schemas/expense.yaml
    When the PR quality gate runs
    Then nx affected -t typecheck runs (catches TS/Dart mismatches)
    And nx affected -t test:quick runs (catches all language mismatches)
    And the PR fails if any app doesn't match the new contract

  Scenario: Fresh clone works after npm install
    Given a developer clones the repository
    When they run npm install
    Then postinstall triggers codegen for all demo apps
    And typecheck/build succeeds immediately

  Scenario: API documentation is generated
    Given the OpenAPI specification is valid
    When nx run demo-contracts:docs runs
    Then a browsable HTML page is generated
    And it shows all endpoints grouped by domain
    And it includes request/response schemas with examples
    And test-only endpoints are excluded from the documentation

  Scenario: All tools are open source and free
    Given the contract enforcement toolchain
    Then every tool in the critical path has an OSI-approved open source license
    And no commercial license or paid tier is required for any functionality used
```

---

## Constraints

1. **Must not break existing tests** — contract enforcement is additive
2. **Generated code must be gitignored** — only the OpenAPI spec is committed
3. **Must use existing CI pipeline** — no new GitHub Actions workflows; leverage `typecheck`,
   `lint`, `test:quick` which already run in PR quality gate
4. **Contract lives in `specs/`** — not inside any individual app
5. **Trunk Based Development** — all work on main branch
6. **Generated code must include encoders AND decoders** — not just types but full
   serialization/deserialization support
7. **Strict camelCase** — all JSON field names use camelCase, zero exceptions
8. **Browsable documentation** — the contract must produce HTML documentation viewable by
   non-developers (product, stakeholders, public)
9. **Open source only** — all tools in the critical path must be open source and free to use with
   OSI-approved licenses (MIT, Apache 2.0, BSD, etc.). No commercial dependencies or paid tiers
   required for any functionality used in this plan

## Out of Scope

1. Replacing Gherkin specs with OpenAPI — they serve different purposes
2. Runtime validation in production (only compile-time/test-time enforcement)
3. Generating full server stubs (only types + encoders/decoders, not routing/handlers)
4. WebSocket or streaming API contracts (REST/JSON only)
5. API paradigm migration (no GraphQL, gRPC, or protocol changes)
