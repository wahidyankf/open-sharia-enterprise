# Delivery Plan

## Critical Context

Most backends build responses as **untyped maps** (`gin.H{}`, `JsonObject`, `mapOf()`, inline
objects). Wiring generated types requires replacing these with typed generated structs for BOTH
request parsing AND response construction.

## Implementation Phases

### Phase 0: Evaluate Missing Spec Types

**Goal**: Determine if types that exist locally but not in the OpenAPI spec need to be added before
adoption can proceed.

- [x] **Audit types not in spec**
  - [x] `RegisterResponse` (used by Java-SB, Rust) — **Decision: NO spec change**. Spec already
        returns `User` on 201. Backends returning partial fields should be fixed to return full
        `User`. Local `RegisterResponse` types replaced with generated `User`.
  - [x] `AttachmentListResponse` (used by Java-SB, Python) — **Decision: NO spec change**. Spec
        returns bare array. Backends wrapping in `{attachments:[]}` should match spec. Local
        `AttachmentListResponse` types removed; use generated `Attachment[]`.
  - [x] `LogoutRequest` (used by Kotlin) — **Decision: NO spec change**. Spec is correct: logout
        uses Authorization header, not request body. Kotlin's local `LogoutRequest` is vestigial
        and stays local (test-only).
  - [x] `PromoteAdminRequest` (used by Kotlin, Python) — **Decision: keep local**. This is a
        test-only endpoint (`x-test-only: true`). Test-only types don't need contract enforcement.
- [x] **Decision**: Spec is the source of truth. No spec changes needed. Backends that disagree
      with the spec are fixed to match during their respective phases.
- [x] **No spec changes needed** — skip lint/bundle

**Validation**: All missing type decisions are documented. Spec changes (if any) pass lint.

---

### Phase 1: Verify Codegen for Elixir, Clojure, and Dart

**Goal**: Confirm that `codegen` Nx targets actually produce usable output for the three apps where
generation was planned but never confirmed.

- [x] **a-demo-be-elixir-phoenix codegen verification**
  - [x] Run `nx run a-demo-be-elixir-phoenix:codegen` — exits 0
  - [x] 23 `.ex` files in `generated-contracts/generated_schemas/`
  - [x] Each struct has `defstruct`, `@enforce_keys`, and `@type` typespecs
  - [x] Module namespace confirmed: `GeneratedSchemas.User` etc.
  - [x] `elixir-openapi-codegen:test:quick` passes (92.2% coverage)
- [x] **a-demo-be-clojure-pedestal codegen verification**
  - [x] Run `nx run a-demo-be-clojure-pedestal:codegen` — exits 0 (required `mkdir -p classes/`)
  - [x] 23 `.clj` files in `generated_contracts/`
  - [x] Each schema is a valid Malli `[:map ...]` definition
  - [x] Namespace confirmed: `openapi-codegen.schemas.user` etc.
  - [x] `clojure-openapi-codegen:test:quick` passes
- [x] **a-demo-fe-dart-flutterweb codegen verification**
  - [x] `codegen` target exists in project.json
  - [x] Run `nx run a-demo-fe-dart-flutterweb:codegen` — exits 0
  - [x] 23 Dart model files in `generated-contracts/lib/model/`
  - [x] Generated classes have `fromJson`/`toJson` methods
  - [x] No codegen fixes needed

**Validation**:

- All three codegen targets exit 0
- Generated files exist and contain valid code
- No empty or malformed output files

---

### Phase 2: Wire a-demo-be-ts-effect (TypeScript/Effect)

**Goal**: Wire the TypeScript backend — request body type annotations + response type annotations.

- [x] **Create re-export layer**
  - [x] Create `src/lib/api/types.ts` (create `src/lib/api/` directories first) mirroring
        `a-demo-fe-ts-nextjs` pattern
  - [x] Re-export all primary domain types from `../../generated-contracts/types.gen`
  - [x] Note: `types.gen.ts` has 97+ exports; the re-export layer selects the 23 primary domain
        types used by route handlers
  - [x] Use `PlReport as PLReport` alias (the generated name is `PlReport`, conventional usage
        is `PLReport`; see `a-demo-fe-ts-nextjs/src/lib/api/types.ts` for the exact alias)
- [x] **Wire `src/routes/auth.ts`** (request + response)
  - [x] Import `LoginRequest`, `RegisterRequest`, `RefreshRequest` (request types)
  - [x] Import `AuthTokens`, `User` (response types)
  - [x] Type-annotate login request body as `LoginRequest`
  - [x] Type-annotate register request body as `RegisterRequest`
  - [x] Type-annotate refresh request body as `RefreshRequest`
  - [x] Type-annotate login response as `AuthTokens`
  - [x] Type-annotate register response as `User`
  - [x] Type-annotate refresh response as `AuthTokens`
- [x] **Wire `src/routes/expense.ts`** (request + response)
  - [x] Import `CreateExpenseRequest`, `UpdateExpenseRequest` (request types)
  - [x] Import `Expense`, `ExpenseListResponse` (response types)
  - [x] Type-annotate create expense body as `CreateExpenseRequest`
  - [x] Type-annotate update expense body as `UpdateExpenseRequest`
  - [x] Type-annotate expense responses as `Expense`
  - [x] Type-annotate list response as `ExpenseListResponse`
- [x] **Wire `src/routes/user.ts`** (request + response)
  - [x] Import `UpdateProfileRequest`, `ChangePasswordRequest` (request types)
  - [x] Import `User` (response type)
  - [x] Type-annotate request bodies
  - [x] Type-annotate user profile responses as `User`
- [x] **Wire `src/routes/attachment.ts`** (response only — upload is multipart)
  - [x] Import `Attachment` (response type)
  - [x] Type-annotate attachment responses as `Attachment`
- [x] **Wire `src/routes/report.ts`** (response only — query params)
  - [x] Import `PLReport` (response type)
  - [x] Type-annotate P&L report response as `PLReport`
- [x] **Wire `src/routes/admin.ts`** (request + response)
  - [x] Import `DisableRequest` (request type)
  - [x] Import `User`, `UserListResponse`, `PasswordResetResponse` (response types)
  - [x] Type-annotate disable request body as `DisableRequest`
  - [x] Type-annotate admin responses with generated types
- [x] **Wire `src/routes/token.ts`** (response only)
  - [x] Import `TokenClaims`, `JwksResponse` (response types)
  - [x] Type-annotate token endpoint responses
- [x] **Wire `src/routes/health.ts`** (response only)
  - [x] Import `HealthResponse` (response type)
  - [x] Type-annotate health endpoint response
- [x] **Verify** `nx run a-demo-be-ts-effect:typecheck` passes
- [x] **Verify** `nx run a-demo-be-ts-effect:test:quick` passes with >=90% coverage

---

### Phase 3: Wire a-demo-be-golang-gin (Go/Gin)

**Goal**: Replace local request structs with `contracts.*` imports and replace all `gin.H{}`
response maps with typed generated structs.

- [x] **Wire `internal/handler/auth.go`** (request + response)
  - [x] Add import for `contracts` package
  - [x] Remove local `RegisterRequest` struct definition
  - [x] Remove local `LoginRequest` struct definition
  - [x] Use `contracts.RegisterRequest` for register body binding
  - [x] Use `contracts.LoginRequest` for login body binding
  - [x] Use `contracts.RefreshRequest` for refresh body (replaces `map[string]string`)
  - [x] Replace `gin.H{}` login response with `contracts.AuthTokens{...}`
  - [x] Replace `gin.H{}` register response with `contracts.User{...}`
  - [x] Replace `gin.H{}` refresh response with `contracts.AuthTokens{...}`
- [x] **Wire `internal/handler/user.go`** (request + response)
  - [x] Remove local `ChangePasswordRequest` struct definition
  - [x] Use `contracts.ChangePasswordRequest` for password change body
  - [x] Use `contracts.UpdateProfileRequest` for profile update (replaces `map[string]string`)
  - [x] Replace `gin.H{}` user profile response with `contracts.User{...}`
  - [x] Replace `gin.H{}` password change response (message only — verify)
- [x] **Wire `internal/handler/expense.go`** (request + response)
  - [x] Remove local `ExpenseRequest` struct definition
  - [x] Use `contracts.CreateExpenseRequest` for create expense body
  - [x] Use `contracts.UpdateExpenseRequest` for update expense body
  - [x] Replace `gin.H{}` expense responses with `contracts.Expense{...}`
  - [x] Replace `gin.H{}` expense list response with `contracts.ExpenseListResponse{...}`
- [x] **Wire `internal/handler/report.go`** (response only)
  - [x] Replace `gin.H{}` P&L report response with `contracts.PLReport{...}`
- [x] **Wire `internal/handler/attachment.go`** (response only)
  - [x] Replace `gin.H{}` attachment responses with `contracts.Attachment{...}`
- [x] **Wire `internal/handler/admin.go`** (request + response)
  - [x] Use `contracts.DisableRequest` for disable body (replaces raw body parsing)
  - [x] Replace `gin.H{}` user list response with `contracts.UserListResponse{...}`
  - [x] Replace `gin.H{}` password reset response with `contracts.PasswordResetResponse{...}`
- [x] **Wire `internal/handler/token.go`** (response only)
  - [x] Replace `gin.H{}` token claims response with `contracts.TokenClaims{...}`
  - [x] Replace `gin.H{}` JWKS response with `contracts.JwksResponse{...}`
- [x] **Wire `internal/handler/health.go`** (response only)
  - [x] Replace `gin.H{}` health response with `contracts.HealthResponse{...}`
- [x] **Verify** `nx run a-demo-be-golang-gin:build` passes (`go build ./...`)
- [x] **Verify** `nx run a-demo-be-golang-gin:test:quick` passes with >=90% coverage
- [x] **Verify** no local request/response structs remain (grep for `type.*struct` in handlers)

---

### Phase 4: Wire a-demo-be-java-springboot (Java/Spring Boot)

**Goal**: Replace 18 local DTO classes with generated `contracts.*` imports. Resolve name
mismatches.

- [x] **Add** `generated-contracts/src/main/java` as Maven source root to `pom.xml` (use
      `build-helper-maven-plugin` `add-source` execution — this configuration does not exist yet;
      see tech-docs.md Java section for the required XML snippet)
- [x] **Wire auth DTOs** (7 request + 2 response)
  - [x] Delete `auth/dto/LoginRequest.java` — replace with `contracts.LoginRequest`
  - [x] Delete `auth/dto/RegisterRequest.java` — replace with `contracts.RegisterRequest`
  - [x] Delete `auth/dto/RefreshRequest.java` — replace with `contracts.RefreshRequest`
  - [x] Delete `auth/dto/AuthResponse.java` — replace with `contracts.AuthTokens`
  - [x] Delete `auth/dto/RegisterResponse.java` — replace with `contracts.User`
  - [x] Update `AuthController` imports to use `com.aademobejasb.contracts.*`
  - [x] Update all service and test files referencing deleted DTOs
- [x] **Wire user DTOs** (2 request + 1 response)
  - [x] Delete `user/dto/ChangePasswordRequest.java` — replace with `contracts.ChangePasswordRequest`
  - [x] Delete `user/dto/UpdateProfileRequest.java` — replace with `contracts.UpdateProfileRequest`
  - [x] Delete `user/dto/UserProfileResponse.java` — replace with `contracts.User`
  - [x] Update `UserController` and related files
- [x] **Wire expense DTOs** (1 request + 2 response)
  - [x] Delete `expense/dto/ExpenseRequest.java` — replace with `contracts.CreateExpenseRequest`
        (and `contracts.UpdateExpenseRequest` for updates)
  - [x] Delete `expense/dto/ExpenseResponse.java` — replace with `contracts.Expense`
  - [x] Delete `expense/dto/ExpenseListResponse.java` — replace with `contracts.ExpenseListResponse`
  - [x] Update `ExpenseController` and related files
- [x] **Wire admin DTOs** (1 request + 3 response)
  - [x] Delete `admin/dto/DisableUserRequest.java` — replace with `contracts.DisableRequest`
  - [x] Delete `admin/dto/AdminUserResponse.java` — replace with `contracts.User`
  - [x] Delete `admin/dto/AdminUserListResponse.java` — replace with `contracts.UserListResponse`
  - [x] Delete `admin/dto/AdminPasswordResetResponse.java` — replace with
        `contracts.PasswordResetResponse`
  - [x] Update `AdminController` and related files
- [x] **Wire attachment DTOs** (1 response)
  - [x] Delete `attachment/dto/AttachmentResponse.java` — replace with `contracts.Attachment`
  - [x] Evaluate `AttachmentListResponse` — keep local or add to spec
  - [x] Update `AttachmentController` and related files
- [x] **Wire report DTOs** (1 response)
  - [x] Delete `report/dto/PlReportResponse.java` — replace with `contracts.PLReport`
  - [x] Update `ReportController` and related files
- [x] **Update all tests** referencing deleted DTOs to use generated types
- [x] **Verify** `nx run a-demo-be-java-springboot:build` passes
- [x] **Verify** `nx run a-demo-be-java-springboot:test:quick` passes with >=90% coverage

---

### Phase 5: Wire a-demo-be-java-vertx (Java/Vert.x)

**Goal**: Refactor handlers from raw `JsonObject` to use generated contract types for BOTH request
parsing and response serialization. This is the most invasive backend change.

- [x] **Add** `generated-contracts/src/main/java` as Maven source root to `pom.xml` (same as
      Phase 4 — use `build-helper-maven-plugin` `add-source`; Vert.x model package is
      `com.aademobejavx.contracts`, verified from `project.json`'s `--model-package` argument)
- [x] **Wire auth handlers** (`AuthHandler.java`)
  - [x] Replace `body.getString("username")` pattern with deserialization into
        `contracts.LoginRequest` / `contracts.RegisterRequest` / `contracts.RefreshRequest`
  - [x] Replace `new JsonObject().put("accessToken", ...)` with `contracts.AuthTokens` construction
  - [x] Replace register response `JsonObject` with `contracts.User` serialization
  - [x] Use `ctx.json()` or `Jackson.encode()` for typed response serialization
- [x] **Wire user handlers** (`UserHandler.java`)
  - [x] Replace request parsing with `contracts.UpdateProfileRequest` /
        `contracts.ChangePasswordRequest`
  - [x] Replace response `JsonObject` with `contracts.User` serialization
- [x] **Wire expense handlers** (`ExpenseHandler.java`)
  - [x] Replace request parsing with `contracts.CreateExpenseRequest` /
        `contracts.UpdateExpenseRequest`
  - [x] Replace response `JsonObject` with `contracts.Expense` / `contracts.ExpenseListResponse`
- [x] **Wire admin handlers** (`AdminHandler.java`)
  - [x] Replace request parsing with `contracts.DisableRequest`
  - [x] Replace response `JsonObject` with `contracts.User` / `contracts.UserListResponse` /
        `contracts.PasswordResetResponse`
- [x] **Wire attachment handlers** (`AttachmentHandler.java`)
  - [x] Replace response `JsonObject` with `contracts.Attachment`
- [x] **Wire report handlers** (`ReportHandler.java`)
  - [x] Replace response `JsonObject` with `contracts.PLReport`
- [x] **Wire token handlers** (`TokenHandler.java`)
  - [x] Replace response `JsonObject` with `contracts.TokenClaims` / `contracts.JwksResponse`
- [x] **Wire health handler** (`HealthHandler.java`)
  - [x] Replace response `JsonObject` with `contracts.HealthResponse`
- [x] **Update all tests** to use generated types instead of `JsonObject` assertions
- [x] **Verify** `nx run a-demo-be-java-vertx:build` passes
- [x] **Verify** `nx run a-demo-be-java-vertx:test:quick` passes with >=90% coverage

---

### Phase 6: Wire a-demo-be-kotlin-ktor (Kotlin/Ktor)

**Goal**: Replace 9 inline data classes with generated imports. Convert `mapOf()` responses to
generated type instances.

- [x] **Add** `sourceSets.main { kotlin.srcDirs("generated-contracts/src/main/kotlin") }` to
      `build.gradle.kts` (Kotlin DSL syntax; this configuration does not exist yet — see
      tech-docs.md Kotlin section for the correct syntax)
- [x] **Wire `AuthRoutes.kt`** (request + response)
  - [x] Remove local `RegisterRequest` data class — import `contracts.RegisterRequest`
  - [x] Remove local `LoginRequest` data class — import `contracts.LoginRequest`
  - [x] Remove local `RefreshRequest` data class — import `contracts.RefreshRequest`
  - [x] Keep local `LogoutRequest` (not in spec)
  - [x] Replace `call.respond(mapOf(...))` login response with `call.respond(contracts.AuthTokens(...))`
  - [x] Replace `call.respond(mapOf(...))` register response with `call.respond(contracts.User(...))`
  - [x] Replace `call.respond(mapOf(...))` refresh response with `call.respond(contracts.AuthTokens(...))`
- [x] **Wire `UserRoutes.kt`** (request + response)
  - [x] Remove local `UpdateDisplayNameRequest` — import `contracts.UpdateProfileRequest`
  - [x] Remove local `ChangePasswordRequest` — import `contracts.ChangePasswordRequest`
  - [x] Replace `call.respond(mapOf(...))` user responses with `call.respond(contracts.User(...))`
- [x] **Wire `ExpenseRoutes.kt`** (request + response)
  - [x] Remove local `CreateExpenseDto` — import `contracts.CreateExpenseRequest`
  - [x] Replace `call.respond(mapOf(...))` expense responses with `call.respond(contracts.Expense(...))`
  - [x] Replace expense list response with `call.respond(contracts.ExpenseListResponse(...))`
- [x] **Wire `AdminRoutes.kt`** (request + response)
  - [x] Remove local `DisableUserRequest` — import `contracts.DisableRequest`
  - [x] Replace admin responses with generated types (`User`, `UserListResponse`,
        `PasswordResetResponse`)
- [x] **Wire `AttachmentRoutes.kt`** (response only)
  - [x] Replace attachment responses with `contracts.Attachment(...)`
- [x] **Wire `ReportRoutes.kt`** (response only)
  - [x] Replace P&L report response with `contracts.PLReport(...)`
- [x] **Wire `TokenRoutes.kt`** (response only)
  - [x] Replace token responses with `contracts.TokenClaims(...)` / `contracts.JwksResponse(...)`
- [x] **Wire `HealthRoutes.kt`** (response only)
  - [x] Replace health response with `contracts.HealthResponse(...)`
- [x] **Wire `TestRoutes.kt`**
  - [x] Keep local `PromoteAdminRequest` (test-only, not in spec)
- [x] **Update all tests** referencing removed data classes
- [x] **Verify** `nx run a-demo-be-kotlin-ktor:build` passes
- [x] **Verify** `nx run a-demo-be-kotlin-ktor:test:quick` passes with >=90% coverage

---

### Phase 7: Wire a-demo-be-rust-axum (Rust/Axum)

**Goal**: Replace 12 local structs with generated model imports. Add generated-contracts as crate
dependency.

- [x] **Create crate scaffolding** (generated-contracts/ has no Cargo.toml — must be created)
  - [x] Create `generated-contracts/Cargo.toml` with `[package] name = "a-demo-contracts"` and
        `edition = "2021"`
  - [x] Create `generated-contracts/src/lib.rs` with `pub mod models;`
  - [x] Create `generated-contracts/src/models/mod.rs` re-exporting all generated model files
  - [x] Add `a-demo-contracts = { path = "generated-contracts" }` to main `Cargo.toml`
        (crate name is `a-demo-contracts`, not `generated-contracts`)
- [x] **Wire `src/handlers/auth.rs`** (request + response)
  - [x] Remove local `RegisterRequest`, `LoginRequest`, `RefreshRequest` structs
  - [x] Remove local `RegisterResponse`, `LoginResponse` structs
  - [x] Import generated types: `use a_demo_contracts::models::{RegisterRequest, LoginRequest, ...}`
  - [x] Replace `RegisterResponse` with `a_demo_contracts::models::User`
  - [x] Replace `LoginResponse` with `a_demo_contracts::models::AuthTokens`
- [x] **Wire `src/handlers/user.rs`** (request + response)
  - [x] Remove local `UpdateProfileRequest`, `ChangePasswordRequest` structs
  - [x] Remove local `UserProfile` struct
  - [x] Import generated types
  - [x] Replace `UserProfile` with `models::User`
- [x] **Wire `src/handlers/expense.rs`** (request + response)
  - [x] Remove local `CreateExpenseRequest` struct
  - [x] Import generated types including `Expense`, `ExpenseListResponse`
  - [x] Type-annotate expense responses with generated types
- [x] **Wire `src/handlers/admin.rs`** (request + response)
  - [x] Remove local `DisableUserRequest`, `UserSummary`, `ListUsersResponse` structs
  - [x] Import `models::DisableRequest`, `models::User`, `models::UserListResponse`
  - [x] Replace `UserSummary` with `models::User`
  - [x] Replace `ListUsersResponse` with `models::UserListResponse`
- [x] **Wire `src/handlers/attachment.rs`** (response only)
  - [x] Import `models::Attachment`
  - [x] Type-annotate attachment responses
- [x] **Wire `src/handlers/report.rs`** (response only)
  - [x] Import `models::PLReport`
  - [x] Type-annotate P&L report response
- [x] **Wire `src/handlers/token.rs`** (response only)
  - [x] Import `models::TokenClaims`, `models::JwksResponse`
  - [x] Type-annotate token responses
- [x] **Wire `src/handlers/health.rs`** (response only)
  - [x] Import `models::HealthResponse`
  - [x] Type-annotate health response
- [x] **Update all tests** referencing removed structs
- [x] **Verify** `nx run a-demo-be-rust-axum:build` passes
- [x] **Verify** `nx run a-demo-be-rust-axum:test:quick` passes with >=90% coverage

---

### Phase 8: Wire a-demo-be-python-fastapi (Python/FastAPI)

**Goal**: Replace 22 local Pydantic models (8 request-type + 14 response-type) with generated
imports. Update `response_model=` parameters to use generated types.

- [x] **Verify** `from generated_contracts import LoginRequest` works from app source root
- [x] Note: All router files are under `src/a_demo_be_python_fastapi/routers/` (e.g.,
      `src/a_demo_be_python_fastapi/routers/auth.py`)
- [x] **Wire `src/a_demo_be_python_fastapi/routers/auth.py`** (request + response)
  - [x] Remove local `RegisterRequest` class — import from `generated_contracts`
  - [x] Remove local `LoginRequest` class — import from `generated_contracts`
  - [x] Remove local `RefreshRequest` class — import from `generated_contracts`
  - [x] Remove local `TokenResponse` class — import `AuthTokens` from `generated_contracts`
  - [x] Remove local `RegisterResponse` class — evaluate (use `User` or keep)
  - [x] Update `response_model=` parameters to use generated types
- [x] **Wire `src/a_demo_be_python_fastapi/routers/users.py`** (request + response)
  - [x] Remove local `UpdateProfileRequest` — import from `generated_contracts`
  - [x] Remove local `ChangePasswordRequest` — import from `generated_contracts`
  - [x] Remove local `UserProfileResponse` — import `User` from `generated_contracts`
  - [x] Update `response_model=` to use `User`
- [x] **Wire `src/a_demo_be_python_fastapi/routers/expenses.py`** (request + response)
  - [x] Remove local `ExpenseRequest` — import `CreateExpenseRequest` from `generated_contracts`
  - [x] Add import for `UpdateExpenseRequest` (may not exist locally)
  - [x] Remove local `ExpenseResponse` — import `Expense` from `generated_contracts`
  - [x] Remove local `ExpenseListResponse` — import from `generated_contracts`
  - [x] Update `response_model=` parameters
- [x] **Wire `src/a_demo_be_python_fastapi/routers/admin.py`** (request + response)
  - [x] Remove local `DisableRequest` — import from `generated_contracts`
  - [x] Remove local `UserSummary` — import `User` from `generated_contracts`
  - [x] Remove local `UserListResponse` — import from `generated_contracts`
  - [x] Update `response_model=` parameters
- [x] **Wire `src/a_demo_be_python_fastapi/routers/attachments.py`** (response only)
  - [x] Remove local `AttachmentResponse` — import `Attachment` from `generated_contracts`
  - [x] Evaluate `AttachmentListResponse` (not in spec — keep or add)
  - [x] Update `response_model=`
- [x] **Wire `src/a_demo_be_python_fastapi/routers/reports.py`** (response only)
  - [x] Remove local `BreakdownItem` — import `CategoryBreakdown` from `generated_contracts`
  - [x] Remove local `PLResponse` — import `PLReport` from `generated_contracts`
  - [x] Update `response_model=`
- [x] **Wire `src/a_demo_be_python_fastapi/routers/tokens.py`** (response only)
  - [x] Remove local `ClaimsResponse` — import `TokenClaims` from `generated_contracts`
  - [x] Update `response_model=`
- [x] **Wire `src/a_demo_be_python_fastapi/routers/health.py`** (response only)
  - [x] Remove local `HealthResponse` — import from `generated_contracts`
  - [x] Update `response_model=`
- [x] **Update all tests** referencing removed models
- [x] **Verify** `nx run a-demo-be-python-fastapi:test:quick` passes with >=90% coverage

---

### Phase 9: Wire a-demo-be-fsharp-giraffe and a-demo-be-csharp-aspnetcore (.NET)

**Goal**: Add source inclusion of generated `.fs`/`.cs` files to app projects. Replace inline
records/classes with generated types for both request parsing and response construction.

**a-demo-be-fsharp-giraffe**:

- [x] **Add source inclusion** in main app's `.fsproj` (no `.fsproj` in `generated-contracts/`)
  - [x] Add `<Compile Include="../generated-contracts/OpenAPI/src/ADemoBeFsgi.Contracts/*.fs" />`
        to main `.fsproj` before any files that reference generated types
- [x] **Create `[<CLIMutable>]` wrapper records** for request binding
  - [x] Generated F# records do NOT carry `[<CLIMutable>]`; Giraffe's `bindJsonAsync<T>()` fails
        without it
  - [x] Create thin `[<CLIMutable>]` wrapper records that map to/from generated types for each
        request type: `RegisterRequest`, `LoginRequest`, `RefreshRequest`, `UpdateProfileRequest`,
        `ChangePasswordRequest`, `CreateExpenseRequest`, `UpdateExpenseRequest`, `DisableRequest`
  - [x] Use generated types directly for response construction (no `[<CLIMutable>]` needed)
- [x] **Wire `AuthHandler.fs`** (request + response)
  - [x] Remove local `RegisterRequest`, `LoginRequest`, `RefreshRequest` records
  - [x] Open `OpenAPI.ADemoBeFsgi.Contracts` namespace
  - [x] Use thin `[<CLIMutable>]` wrapper records for request deserialization
  - [x] Map wrapper records to generated types for business logic
  - [x] Use generated types for response construction (`AuthTokens`, `User`)
- [x] **Wire `UserHandler.fs`** (request + response)
  - [x] Remove local `UpdateProfileRequest`, `ChangePasswordRequest` records
  - [x] Use generated types for request and response
- [x] **Wire `ExpenseHandler.fs`** (request + response)
  - [x] Remove local `CreateExpenseRequest`, `UpdateExpenseRequest` records
  - [x] Use generated types for request and response (`Expense`, `ExpenseListResponse`)
- [x] **Wire `AdminHandler.fs`** (request + response)
  - [x] Remove local `DisableRequest` record
  - [x] Use generated types for request and response (`User`, `UserListResponse`,
        `PasswordResetResponse`)
- [x] **Wire `AttachmentHandler.fs`** (response only)
  - [x] Use generated `Attachment` for response
- [x] **Wire `ReportHandler.fs`** (response only)
  - [x] Use generated `PLReport` for response
- [x] **Wire `TokenHandler.fs`** (response only)
  - [x] Use generated `TokenClaims`, `JwksResponse` for response
- [x] **Wire `HealthHandler.fs`** (response only)
  - [x] Use generated `HealthResponse` for response
- [x] **Update all tests** referencing removed records
- [x] **Verify** `nx run a-demo-be-fsharp-giraffe:build` passes
- [x] **Verify** `nx run a-demo-be-fsharp-giraffe:test:quick` passes with >=90% coverage

**a-demo-be-csharp-aspnetcore**:

- [x] **Add source inclusion** in main app's `.csproj` (no `.csproj` in `generated-contracts/`)
  - [x] Add `<Compile Include="../generated-contracts/src/Org.OpenAPITools/ADemoBeCsas.Contracts/*.cs" />`
        to main `.csproj`
- [x] **Wire `AuthEndpoints.cs`** (request + response)
  - [x] Remove local `RegisterRequest`, `LoginRequest`, `RefreshRequest` sealed records
  - [x] Add `using Org.OpenAPITools.ADemoBeCsas.Contracts;` (actual namespace — not `ADemoBeCsas.Contracts`)
  - [x] Use generated types for request binding and response (`AuthTokens`, `User`)
- [x] **Wire `UserEndpoints.cs`** (request + response)
  - [x] Remove local `PatchMeRequest` (replace with `UpdateProfileRequest`)
  - [x] Remove local `ChangePasswordRequest`
  - [x] Use generated types for request and response
- [x] **Wire `ExpenseEndpoints.cs`** (request + response)
  - [x] Remove local `ExpenseRequest` (replace with `CreateExpenseRequest` + `UpdateExpenseRequest`)
  - [x] Use generated types for request and response
- [x] **Wire `AdminEndpoints.cs`** (request + response)
  - [x] Use generated types for disable request and responses
- [x] **Wire `AttachmentEndpoints.cs`** (response only)
  - [x] Use generated `Attachment` for response
- [x] **Wire `ReportEndpoints.cs`** (response only)
  - [x] Use generated `PLReport` for response
- [x] **Wire `TokenEndpoints.cs`** (response only)
  - [x] Use generated `TokenClaims`, `JwksResponse` for response
- [x] **Wire `HealthEndpoints.cs`** (response only)
  - [x] Use generated `HealthResponse` for response
- [x] **Update all tests** referencing removed records
- [x] **Verify** `nx run a-demo-be-csharp-aspnetcore:build` passes
- [x] **Verify** `nx run a-demo-be-csharp-aspnetcore:test:quick` passes with >=90% coverage

---

### Phase 10: Wire a-demo-be-elixir-phoenix and a-demo-be-clojure-pedestal (Dynamic Languages)

**Goal**: Wire Elixir and Clojure backends. Enforcement is at test time via struct construction
(Elixir) and schema validation (Clojure).

**a-demo-be-elixir-phoenix**:

- [x] **Add generated-contracts to Mix source paths** in `mix.exs`
- [x] **Verify module namespace from Phase 1 output** — expected `GeneratedSchemas.*` (the codegen
      defaults to `@default_namespace "GeneratedSchemas"`; no namespace override is passed in
      `project.json`). All struct references below use `GeneratedSchemas.*` as the expected prefix.
      Update if Phase 1 reveals a different namespace.
- [x] **Wire `AuthController`** (request validation + response struct construction)
  - [x] Alias generated `GeneratedSchemas.LoginRequest`, `GeneratedSchemas.RegisterRequest`,
        `GeneratedSchemas.RefreshRequest` modules
  - [x] Validate incoming params against generated struct fields
  - [x] Construct `%GeneratedSchemas.AuthTokens{}` for login/refresh responses
  - [x] Construct `%GeneratedSchemas.User{}` for register/profile responses
- [x] **Wire `UserController`** (request + response)
  - [x] Alias generated `GeneratedSchemas.UpdateProfileRequest`,
        `GeneratedSchemas.ChangePasswordRequest`
  - [x] Validate incoming params
  - [x] Construct `%GeneratedSchemas.User{}` for user profile response
- [x] **Wire `ExpenseController`** (request + response)
  - [x] Alias generated `GeneratedSchemas.CreateExpenseRequest`,
        `GeneratedSchemas.UpdateExpenseRequest`
  - [x] Validate incoming params
  - [x] Construct `%GeneratedSchemas.Expense{}` / `%GeneratedSchemas.ExpenseListResponse{}` for
        responses
- [x] **Wire `AdminController`** (request + response)
  - [x] Alias generated `GeneratedSchemas.DisableRequest`
  - [x] Construct `%GeneratedSchemas.User{}` / `%GeneratedSchemas.UserListResponse{}` /
        `%GeneratedSchemas.PasswordResetResponse{}` for responses
- [x] **Wire `AttachmentController`** (response only)
  - [x] Construct `%GeneratedSchemas.Attachment{}` for responses
- [x] **Wire `ReportController`** (response only)
  - [x] Construct `%GeneratedSchemas.PLReport{}` for response
- [x] **Wire `TokenController`** (response only)
  - [x] Construct `%GeneratedSchemas.TokenClaims{}` / `%GeneratedSchemas.JwksResponse{}` for
        responses
- [x] **Wire `HealthController`** (response only)
  - [x] Construct `%GeneratedSchemas.HealthResponse{}` for response
- [x] **Add struct construction tests** — at least one test per generated struct verifying
      `@enforce_keys` catches missing required fields
- [x] **Verify** `nx run a-demo-be-elixir-phoenix:test:quick` passes

**a-demo-be-clojure-pedestal**:

- [x] **Add generated schemas to classpath** in `deps.edn`
- [x] **Verify namespace from Phase 1 output** — generated schemas use namespace
      `openapi-codegen.schemas.<kebab-name>` (e.g., `openapi-codegen.schemas.auth-tokens`).
      Confirm exact names by inspecting the Phase 1 codegen output.
- [x] **Create contract validation helper**
  - [x] Create `contracts.clj` namespace that requires all generated schemas, e.g.:
        `(:require [openapi-codegen.schemas.auth-tokens :as auth-tokens-schema] ...)`
  - [x] Add `validate-response` function using `m/validate`
- [x] **Wire auth handlers** (request + response validation)
  - [x] Require `openapi-codegen.schemas.login-request`, `openapi-codegen.schemas.register-request`,
        `openapi-codegen.schemas.refresh-request` for request validation
  - [x] Add `validate-response` calls on login/register/refresh responses against
        `openapi-codegen.schemas.auth-tokens` / `openapi-codegen.schemas.user` generated schemas
- [x] **Wire user handlers** (request + response validation)
  - [x] Validate user profile response against `openapi-codegen.schemas.user` schema
- [x] **Wire expense handlers** (request + response validation)
  - [x] Validate expense responses against `openapi-codegen.schemas.expense` /
        `openapi-codegen.schemas.expense-list-response` schemas
- [x] **Wire admin handlers** (request + response validation)
  - [x] Validate admin responses against `openapi-codegen.schemas.user` /
        `openapi-codegen.schemas.user-list-response` /
        `openapi-codegen.schemas.password-reset-response` schemas
- [x] **Wire attachment handlers** (response validation)
  - [x] Validate against `openapi-codegen.schemas.attachment` schema
- [x] **Wire report handlers** (response validation)
  - [x] Validate against `openapi-codegen.schemas.pl-report` schema
- [x] **Wire token handlers** (response validation)
  - [x] Validate against `openapi-codegen.schemas.token-claims` /
        `openapi-codegen.schemas.jwks-response` schemas
- [x] **Wire health handler** (response validation)
  - [x] Validate against `openapi-codegen.schemas.health-response` schema
- [x] **Add schema validation tests** — at least one test per generated schema verifying
      validation catches missing required fields
- [x] **Verify** `nx run a-demo-be-clojure-pedestal:test:quick` passes

---

### Phase 11: Wire a-demo-fe-dart-flutterweb (Dart/Flutter Web)

**Goal**: Replace 20+ hand-written model classes with generated Dart classes.

- [x] **Add generated package as path dependency** in `pubspec.yaml`
- [x] **Run `flutter pub get`** to install
- [x] **Wire `lib/models/auth.dart`**
  - [x] Replace local `LoginRequest`, `RegisterRequest`, `AuthTokens` with re-exports from
        generated package
- [x] **Wire `lib/models/user.dart`**
  - [x] Replace local `User`, `UserListResponse`, `UpdateProfileRequest`,
        `ChangePasswordRequest`, `DisableRequest`, `PasswordResetResponse` with generated types
- [x] **Wire `lib/models/expense.dart`**
  - [x] Replace local `Expense`, `ExpenseListResponse`, `CreateExpenseRequest`,
        `UpdateExpenseRequest` with generated types
- [x] **Wire `lib/models/attachment.dart`**
  - [x] Replace local `Attachment` with generated type
- [x] **Wire `lib/models/token.dart`**
  - [x] Replace local `TokenClaims`, `JwkKey`, `JwksResponse` with generated types
- [x] **Wire `lib/models/report.dart`**
  - [x] Replace local `CategoryBreakdown`, `ExpenseSummary`, `PLReport` with generated types
- [x] **Wire `lib/models/health.dart`**
  - [x] Replace local `HealthResponse` with generated type
- [x] **Update `lib/services/*.dart`** to compile against updated model imports
- [x] **Verify** `dart analyze` passes with no errors
- [x] **Verify** `nx run a-demo-fe-dart-flutterweb:test:quick` passes with >=70% coverage

---

### Phase 12: Wire E2E Contract Validation (a-demo-be-e2e + a-demo-fe-e2e)

**Goal**: Activate the existing `validateResponseAgainstContract` function in all E2E step
definitions.

**a-demo-be-e2e** (15 step files):

- [x] **Wire `tests/steps/auth/auth.steps.ts`**
  - [x] Import `validateResponseAgainstContract`
  - [x] Add contract validation after every 2xx response
- [x] **Wire `tests/steps/auth/token-lifecycle.steps.ts`**
  - [x] Add contract validation after every 2xx response
- [x] **Wire `tests/steps/expenses/expenses.steps.ts`**
  - [x] Add contract validation after every 2xx response
- [x] **Wire `tests/steps/expenses/units.steps.ts`**
  - [x] Add contract validation after every 2xx response
- [x] **Wire `tests/steps/expenses/currency.steps.ts`**
  - [x] Add contract validation after every 2xx response
- [x] **Wire `tests/steps/expenses/attachments.steps.ts`**
  - [x] Add contract validation after every 2xx response
- [x] **Wire `tests/steps/expenses/reporting.steps.ts`**
  - [x] Add contract validation after every 2xx response
- [x] **Wire `tests/steps/user/user-lifecycle.steps.ts`**
  - [x] Add contract validation after every 2xx response
- [x] **Wire `tests/steps/admin/admin.steps.ts`**
  - [x] Add contract validation after every 2xx response
- [x] **Wire `tests/steps/token-management/tokens.steps.ts`**
  - [x] Add contract validation after every 2xx response
- [x] **Wire `tests/steps/health/health.steps.ts`**
  - [x] Add contract validation after health check response
- [x] **Wire `tests/steps/security/security.steps.ts`**
  - [x] Add contract validation after every 2xx response
- [x] **Wire `tests/steps/common.steps.ts`**
  - [x] Add contract validation for any shared response handling (validates in status code check
        step — covers all 2xx responses centrally)
- [x] **Wire `tests/steps/common-setup.steps.ts`**
  - [x] Add contract validation if it handles responses
- [x] **Wire `tests/steps/test-support/test-api.steps.ts`**
  - [x] Add contract validation for test-support responses (if applicable)

**a-demo-fe-e2e** (16 step files):

- [x] **Wire `tests/steps/authentication/login.steps.ts`**
  - [x] Import `validateResponseAgainstContract`
  - [x] Add contract validation after every 2xx response
- [x] **Wire `tests/steps/authentication/session.steps.ts`**
  - [x] Add contract validation after every 2xx response
- [x] **Wire `tests/steps/user-lifecycle/user-profile.steps.ts`**
  - [x] Add contract validation after every 2xx response
- [x] **Wire `tests/steps/user-lifecycle/registration.steps.ts`**
  - [x] Add contract validation after every 2xx response
- [x] **Wire `tests/steps/expenses/expense-management.steps.ts`**
  - [x] Add contract validation after every 2xx response
- [x] **Wire `tests/steps/expenses/attachments.steps.ts`**
  - [x] Add contract validation after every 2xx response
- [x] **Wire `tests/steps/expenses/reporting.steps.ts`**
  - [x] Add contract validation after every 2xx response
- [x] **Wire `tests/steps/expenses/currency-handling.steps.ts`**
  - [x] Add contract validation after every 2xx response
- [x] **Wire `tests/steps/expenses/unit-handling.steps.ts`**
  - [x] Add contract validation after every 2xx response
- [x] **Wire `tests/steps/admin/admin-panel.steps.ts`**
  - [x] Add contract validation after every 2xx response
- [x] **Wire `tests/steps/token-management/tokens.steps.ts`**
  - [x] Add contract validation after every 2xx response
- [x] **Wire `tests/steps/health/health.steps.ts`**
  - [x] Add contract validation after health check response
- [x] **Wire `tests/steps/security/security.steps.ts`**
  - [x] Add contract validation after every 2xx response
- [x] **Wire `tests/steps/layout/responsive.steps.ts`**
  - [x] Add contract validation if it handles API responses
- [x] **Wire `tests/steps/layout/accessibility.steps.ts`**
  - [x] Add contract validation if it handles API responses
- [x] **Wire `tests/steps/common.steps.ts`**
  - [x] Add contract validation for shared response handling
- [x] **Verify** grep confirms `validateResponseAgainstContract` is imported in all step files

**Validation**:

- [x] **Verify** `nx run a-demo-be-e2e:test:e2e` passes (against a running backend)
- [x] **Verify** `nx run a-demo-fe-e2e:test:e2e` passes (against running frontend + backend)

---

### Phase 13: End-to-End Verification

**Goal**: Verify the full enforcement model works. All apps pass. Contract changes cause failures.

- [x] **Run full test suite**
  - [x] `nx run-many -t test:quick --projects=demo-*` — all 16 demo apps pass
        (14 newly wired + 2 previously wired by 2026-03-17 plan)
- [x] **Run builds for compiled backends**
  - [x] `nx run-many -t build --projects=a-demo-be-golang-gin,a-demo-be-java-springboot,a-demo-be-java-vertx,a-demo-be-kotlin-ktor,a-demo-be-rust-axum,a-demo-be-fsharp-giraffe,a-demo-be-csharp-aspnetcore`
- [x] **Run typechecks for TypeScript and Dart**
  - [x] `nx run-many -t typecheck --projects=a-demo-be-ts-effect,a-demo-fe-dart-flutterweb`
- [x] **Enforcement smoke test** (do NOT commit)
  - [x] Rename `accessToken` to `token` in `specs/apps/a-demo/contracts/schemas/auth.yaml`
  - [x] Run `nx run a-demo-contracts:bundle`
  - [x] Run `nx run a-demo-be-golang-gin:codegen && nx run a-demo-be-golang-gin:build` — expect failure
  - [x] Run `nx run a-demo-be-ts-effect:codegen && nx run a-demo-be-ts-effect:typecheck` — expect
        failure
  - [x] Run `nx run a-demo-be-python-fastapi:codegen && nx run a-demo-be-python-fastapi:test:unit` —
        expect failure
  - [x] Revert the rename in `specs/apps/a-demo/contracts/schemas/auth.yaml`
  - [x] Run `nx run a-demo-contracts:bundle`
  - [x] Run `nx run a-demo-be-golang-gin:codegen && nx run a-demo-be-ts-effect:codegen && nx run a-demo-be-python-fastapi:codegen`
  - [x] Verify builds/typechecks pass again (confirm clean revert)
- [x] **Verify pre-push hook**: Stage and push a minor change; confirm hook runs `test:quick` for
      affected projects and passes
- [x] **Update `CLAUDE.md`** — note that all 16 demo apps now import from generated-contracts/
      for both request and response types (14 wired in this plan, 2 wired in 2026-03-17 plan)
- [x] **Update `specs/apps/a-demo/contracts/README.md`** — update adoption status to reflect all
      apps are wired
- [x] **Trigger all E2E CI workflows manually** — verify all 14 newly-wired apps pass

**Validation**:

- `nx run-many -t test:quick --projects=demo-*` exits 0 (all 16 apps: 14 newly wired + 2
  previously wired by 2026-03-17 plan)
- Enforcement smoke test confirms contract change causes failures in at least one statically typed
  and one dynamically typed app before revert
- Pre-push hook passes on clean working tree
- All 14 newly-wired app E2E CI workflows pass
- Documentation updated

---

## Open Questions

1. ~~**RegisterResponse**~~: **RESOLVED** — Spec returns `User` on 201. Backends standardize on
   returning full `User`. Local `RegisterResponse` types replaced with generated `User`.

2. ~~**AttachmentListResponse**~~: **RESOLVED** — Spec returns bare array. Local wrapper types
   removed. Backends match spec.

3. **Java Vert.x refactoring scope**: The JsonObject-to-typed-object refactoring is invasive. Should
   we prioritize compile-time safety (full refactor) or take a lighter approach (type assertions in
   tests)?

4. **Generated type field compatibility**: Some generated types may use different field types than
   local ones (e.g., `openapi_types.Date` vs `string`, `Optional<>` vs nullable). Verify
   compatibility during implementation.

5. **Kotlin `@Serializable`**: Verify generated Kotlin data classes carry `@Serializable` annotation
   needed for Ktor's `ContentNegotiation`.

6. **F# `[<CLIMutable>]`**: Verified — generated F# records do NOT carry `[<CLIMutable>]`.
   Giraffe's `bindJsonAsync<T>()` and ASP.NET Core model binding require a default constructor,
   which immutable F# records lack without this attribute. Mitigation: create thin
   `[<CLIMutable>]` wrapper records for request binding (see Phase 9). Use generated types
   directly for response construction.

7. **Dart generator output format**: Verify generated Dart classes have `fromJson`/`toJson` methods
   compatible with existing service layer expectations.

---

## Risks and Mitigations

| Risk                                                                 | Impact | Mitigation                                                                        |
| -------------------------------------------------------------------- | ------ | --------------------------------------------------------------------------------- |
| Generated types have incompatible field types (Date vs string, etc.) | High   | Verify generated vs local field types in Phase 0; adjust codegen config if needed |
| Java Vert.x refactoring breaks many tests                            | High   | Phase 5 is isolated; can fall back to lighter approach if too invasive            |
| Elixir/Clojure codegen never ran; may have bugs                      | High   | Phase 1 catches issues early; fix before wiring                                   |
| Dart codegen produces incompatible output                            | Medium | Phase 1 verification; use re-export layer if names differ                         |
| Generated Java DTOs use Optional instead of nullable                 | Medium | Check generated field types before replacing; adjust if needed                    |
| Kotlin generated types lack @Serializable                            | Medium | Verify in Phase 6; add thin wrapper if missing                                    |
| F# generated records lack [<CLIMutable>]                             | Medium | Verify in Phase 9; add thin wrapper if missing                                    |
| Coverage drops after removing local types and their dedicated tests  | Low    | Tests must be updated to use generated types; coverage maintained                 |
| E2E tests fail due to contract gaps (endpoint not in spec)           | Low    | Validator returns null for unknown paths; existing behavior preserved             |
| Name mapping creates import confusion for future developers          | Low    | Document all mappings in tech-docs.md; re-export layers provide stable names      |

---

## Completion Status

- [x] Phase 0: Evaluate Missing Spec Types
- [x] Phase 1: Verify Codegen (Elixir, Clojure, Dart)
- [x] Phase 2: Wire a-demo-be-ts-effect
- [x] Phase 3: Wire a-demo-be-golang-gin
- [x] Phase 4: Wire a-demo-be-java-springboot
- [x] Phase 5: Wire a-demo-be-java-vertx
- [x] Phase 6: Wire a-demo-be-kotlin-ktor
- [x] Phase 7: Wire a-demo-be-rust-axum
- [x] Phase 8: Wire a-demo-be-python-fastapi
- [x] Phase 9: Wire a-demo-be-fsharp-giraffe + a-demo-be-csharp-aspnetcore
- [x] Phase 10: Wire a-demo-be-elixir-phoenix + a-demo-be-clojure-pedestal
- [x] Phase 11: Wire a-demo-fe-dart-flutterweb
- [x] Phase 12: Wire E2E Contract Validation (common.steps.ts validates all 2xx responses centrally)
- [x] Phase 13: End-to-End Verification (all 14 E2E workflows pass)
