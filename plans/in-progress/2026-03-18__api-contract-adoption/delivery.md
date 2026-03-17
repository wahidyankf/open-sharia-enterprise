# Delivery Plan

## Critical Context

Most backends build responses as **untyped maps** (`gin.H{}`, `JsonObject`, `mapOf()`, inline
objects). Wiring generated types requires replacing these with typed generated structs for BOTH
request parsing AND response construction.

## Implementation Phases

### Phase 0: Evaluate Missing Spec Types

**Goal**: Determine if types that exist locally but not in the OpenAPI spec need to be added before
adoption can proceed.

- [ ] **Audit types not in spec**
  - [ ] `RegisterResponse` (used by Java-SB, Rust) — evaluate: should register return `User` type
        or a separate RegisterResponse? Check what frontends expect
  - [ ] `AttachmentListResponse` (used by Java-SB, Python) — evaluate: is this a real API response
        or just a wrapper? Check OpenAPI paths for list-attachments endpoint
  - [ ] `LogoutRequest` (used by Kotlin) — evaluate: does the logout endpoint accept a body?
        Check OpenAPI spec paths/auth.yaml
  - [ ] `PromoteAdminRequest` (used by Kotlin, Python) — confirm this is test-only and should stay
        local
- [ ] **Decision**: For each missing type, either (a) add it to the OpenAPI spec + regenerate, or
      (b) keep it as a local-only type. Document the decision.
- [ ] **If spec changes needed**: Run `nx run demo-contracts:lint` and
      `nx run demo-contracts:bundle` after changes

**Validation**: All missing type decisions are documented. Spec changes (if any) pass lint.

---

### Phase 1: Verify Codegen for Elixir, Clojure, and Dart

**Goal**: Confirm that `codegen` Nx targets actually produce usable output for the three apps where
generation was planned but never confirmed.

- [ ] **demo-be-elixir-phoenix codegen verification**
  - [ ] Run `nx run demo-be-elixir-phoenix:codegen` and capture output
  - [ ] Verify `.ex` struct files appear in `apps/demo-be-elixir-phoenix/generated-contracts/`
  - [ ] Verify each struct has `defstruct`, `@enforce_keys`, and `@type` typespecs
  - [ ] Verify module namespace matches expected pattern (e.g., `DemoBeExph.Contracts.User`)
  - [ ] If generation fails, debug `libs/elixir-openapi-codegen` and fix
  - [ ] Run `nx run elixir-openapi-codegen:test:quick` to confirm lib is healthy
- [ ] **demo-be-clojure-pedestal codegen verification**
  - [ ] Run `nx run demo-be-clojure-pedestal:codegen` and capture output
  - [ ] Verify `.clj` schema files appear in `apps/demo-be-clojure-pedestal/generated_contracts/`
  - [ ] Verify each schema is a valid Malli `[:map ...]` definition
  - [ ] Verify namespace naming convention matches expected pattern
  - [ ] If generation fails, debug `libs/clojure-openapi-codegen` and fix
  - [ ] Run `nx run clojure-openapi-codegen:test:quick` to confirm lib is healthy
- [ ] **demo-fe-dart-flutterweb codegen verification**
  - [ ] Check `apps/demo-fe-dart-flutterweb/project.json` for `codegen` target
  - [ ] Run `nx run demo-fe-dart-flutterweb:codegen` and capture output
  - [ ] Verify Dart classes appear in `generated-contracts/` with `fromJson`/`toJson`
  - [ ] Run `dart analyze` on generated code to verify validity
  - [ ] If codegen target missing or broken, implement/fix it

**Validation**:

- All three codegen targets exit 0
- Generated files exist and contain valid code
- No empty or malformed output files

---

### Phase 2: Wire demo-be-ts-effect (TypeScript/Effect)

**Goal**: Wire the TypeScript backend — request body type annotations + response type annotations.

- [ ] **Create re-export layer**
  - [ ] Create `src/lib/api/types.ts` mirroring `demo-fe-ts-nextjs` pattern
  - [ ] Re-export ALL 23 types from `../../generated-contracts/types.gen`
- [ ] **Wire `src/routes/auth.ts`** (request + response)
  - [ ] Import `LoginRequest`, `RegisterRequest`, `RefreshRequest` (request types)
  - [ ] Import `AuthTokens`, `User` (response types)
  - [ ] Type-annotate login request body as `LoginRequest`
  - [ ] Type-annotate register request body as `RegisterRequest`
  - [ ] Type-annotate refresh request body as `RefreshRequest`
  - [ ] Type-annotate login response as `AuthTokens`
  - [ ] Type-annotate register response as `User`
  - [ ] Type-annotate refresh response as `AuthTokens`
- [ ] **Wire `src/routes/expense.ts`** (request + response)
  - [ ] Import `CreateExpenseRequest`, `UpdateExpenseRequest` (request types)
  - [ ] Import `Expense`, `ExpenseListResponse` (response types)
  - [ ] Type-annotate create expense body as `CreateExpenseRequest`
  - [ ] Type-annotate update expense body as `UpdateExpenseRequest`
  - [ ] Type-annotate expense responses as `Expense`
  - [ ] Type-annotate list response as `ExpenseListResponse`
- [ ] **Wire `src/routes/user.ts`** (request + response)
  - [ ] Import `UpdateProfileRequest`, `ChangePasswordRequest` (request types)
  - [ ] Import `User` (response type)
  - [ ] Type-annotate request bodies
  - [ ] Type-annotate user profile responses as `User`
- [ ] **Wire `src/routes/attachment.ts`** (response only — upload is multipart)
  - [ ] Import `Attachment` (response type)
  - [ ] Type-annotate attachment responses as `Attachment`
- [ ] **Wire `src/routes/report.ts`** (response only — query params)
  - [ ] Import `PLReport` (response type)
  - [ ] Type-annotate P&L report response as `PLReport`
- [ ] **Wire `src/routes/admin.ts`** (request + response)
  - [ ] Import `DisableRequest` (request type)
  - [ ] Import `User`, `UserListResponse`, `PasswordResetResponse` (response types)
  - [ ] Type-annotate disable request body as `DisableRequest`
  - [ ] Type-annotate admin responses with generated types
- [ ] **Wire `src/routes/token.ts`** (response only)
  - [ ] Import `TokenClaims`, `JwksResponse` (response types)
  - [ ] Type-annotate token endpoint responses
- [ ] **Wire `src/routes/health.ts`** (response only)
  - [ ] Import `HealthResponse` (response type)
  - [ ] Type-annotate health endpoint response
- [ ] **Verify** `nx run demo-be-ts-effect:typecheck` passes
- [ ] **Verify** `nx run demo-be-ts-effect:test:quick` passes with >=90% coverage

---

### Phase 3: Wire demo-be-golang-gin (Go/Gin)

**Goal**: Replace local request structs with `contracts.*` imports and replace all `gin.H{}`
response maps with typed generated structs.

- [ ] **Wire `internal/handler/auth.go`** (request + response)
  - [ ] Add import for `contracts` package
  - [ ] Remove local `RegisterRequest` struct definition
  - [ ] Remove local `LoginRequest` struct definition
  - [ ] Use `contracts.RegisterRequest` for register body binding
  - [ ] Use `contracts.LoginRequest` for login body binding
  - [ ] Use `contracts.RefreshRequest` for refresh body (replaces `map[string]string`)
  - [ ] Replace `gin.H{}` login response with `contracts.AuthTokens{...}`
  - [ ] Replace `gin.H{}` register response with `contracts.User{...}`
  - [ ] Replace `gin.H{}` refresh response with `contracts.AuthTokens{...}`
- [ ] **Wire `internal/handler/user.go`** (request + response)
  - [ ] Remove local `ChangePasswordRequest` struct definition
  - [ ] Use `contracts.ChangePasswordRequest` for password change body
  - [ ] Use `contracts.UpdateProfileRequest` for profile update (replaces `map[string]string`)
  - [ ] Replace `gin.H{}` user profile response with `contracts.User{...}`
  - [ ] Replace `gin.H{}` password change response (message only — verify)
- [ ] **Wire `internal/handler/expense.go`** (request + response)
  - [ ] Remove local `ExpenseRequest` struct definition
  - [ ] Use `contracts.CreateExpenseRequest` for create expense body
  - [ ] Use `contracts.UpdateExpenseRequest` for update expense body
  - [ ] Replace `gin.H{}` expense responses with `contracts.Expense{...}`
  - [ ] Replace `gin.H{}` expense list response with `contracts.ExpenseListResponse{...}`
- [ ] **Wire `internal/handler/report.go`** (response only)
  - [ ] Replace `gin.H{}` P&L report response with `contracts.PLReport{...}`
- [ ] **Wire `internal/handler/attachment.go`** (response only)
  - [ ] Replace `gin.H{}` attachment responses with `contracts.Attachment{...}`
- [ ] **Wire `internal/handler/admin.go`** (request + response)
  - [ ] Use `contracts.DisableRequest` for disable body (replaces raw body parsing)
  - [ ] Replace `gin.H{}` user list response with `contracts.UserListResponse{...}`
  - [ ] Replace `gin.H{}` password reset response with `contracts.PasswordResetResponse{...}`
- [ ] **Wire `internal/handler/token.go`** (response only)
  - [ ] Replace `gin.H{}` token claims response with `contracts.TokenClaims{...}`
  - [ ] Replace `gin.H{}` JWKS response with `contracts.JwksResponse{...}`
- [ ] **Wire `internal/handler/health.go`** (response only)
  - [ ] Replace `gin.H{}` health response with `contracts.HealthResponse{...}`
- [ ] **Verify** `nx run demo-be-golang-gin:build` passes (`go build ./...`)
- [ ] **Verify** `nx run demo-be-golang-gin:test:quick` passes with >=90% coverage
- [ ] **Verify** no local request/response structs remain (grep for `type.*struct` in handlers)

---

### Phase 4: Wire demo-be-java-springboot (Java/Spring Boot)

**Goal**: Replace 18 local DTO classes with generated `contracts.*` imports. Resolve name
mismatches.

- [ ] **Verify** `pom.xml` has `generated-contracts/src/main/java` as Maven source root
- [ ] **Wire auth DTOs** (7 request + 2 response)
  - [ ] Delete `auth/dto/LoginRequest.java` — replace with `contracts.LoginRequest`
  - [ ] Delete `auth/dto/RegisterRequest.java` — replace with `contracts.RegisterRequest`
  - [ ] Delete `auth/dto/RefreshRequest.java` — replace with `contracts.RefreshRequest`
  - [ ] Delete `auth/dto/AuthResponse.java` — replace with `contracts.AuthTokens`
  - [ ] Delete `auth/dto/RegisterResponse.java` — replace with `contracts.User`
  - [ ] Update `AuthController` imports to use `com.demobejasb.contracts.*`
  - [ ] Update all service and test files referencing deleted DTOs
- [ ] **Wire user DTOs** (2 request + 1 response)
  - [ ] Delete `user/dto/ChangePasswordRequest.java` — replace with `contracts.ChangePasswordRequest`
  - [ ] Delete `user/dto/UpdateProfileRequest.java` — replace with `contracts.UpdateProfileRequest`
  - [ ] Delete `user/dto/UserProfileResponse.java` — replace with `contracts.User`
  - [ ] Update `UserController` and related files
- [ ] **Wire expense DTOs** (1 request + 2 response)
  - [ ] Delete `expense/dto/ExpenseRequest.java` — replace with `contracts.CreateExpenseRequest`
        (and `contracts.UpdateExpenseRequest` for updates)
  - [ ] Delete `expense/dto/ExpenseResponse.java` — replace with `contracts.Expense`
  - [ ] Delete `expense/dto/ExpenseListResponse.java` — replace with `contracts.ExpenseListResponse`
  - [ ] Update `ExpenseController` and related files
- [ ] **Wire admin DTOs** (1 request + 3 response)
  - [ ] Delete `admin/dto/DisableUserRequest.java` — replace with `contracts.DisableRequest`
  - [ ] Delete `admin/dto/AdminUserResponse.java` — replace with `contracts.User`
  - [ ] Delete `admin/dto/AdminUserListResponse.java` — replace with `contracts.UserListResponse`
  - [ ] Delete `admin/dto/AdminPasswordResetResponse.java` — replace with
        `contracts.PasswordResetResponse`
  - [ ] Update `AdminController` and related files
- [ ] **Wire attachment DTOs** (1 response)
  - [ ] Delete `attachment/dto/AttachmentResponse.java` — replace with `contracts.Attachment`
  - [ ] Evaluate `AttachmentListResponse` — keep local or add to spec
  - [ ] Update `AttachmentController` and related files
- [ ] **Wire report DTOs** (1 response)
  - [ ] Delete `report/dto/PlReportResponse.java` — replace with `contracts.PLReport`
  - [ ] Update `ReportController` and related files
- [ ] **Update all tests** referencing deleted DTOs to use generated types
- [ ] **Verify** `nx run demo-be-java-springboot:build` passes
- [ ] **Verify** `nx run demo-be-java-springboot:test:quick` passes with >=90% coverage

---

### Phase 5: Wire demo-be-java-vertx (Java/Vert.x)

**Goal**: Refactor handlers from raw `JsonObject` to use generated contract types for BOTH request
parsing and response serialization. This is the most invasive backend change.

- [ ] **Verify** `pom.xml` has `generated-contracts/src/main/java` as Maven source root
- [ ] **Wire auth handlers** (`AuthHandler.java`)
  - [ ] Replace `body.getString("username")` pattern with deserialization into
        `contracts.LoginRequest` / `contracts.RegisterRequest` / `contracts.RefreshRequest`
  - [ ] Replace `new JsonObject().put("accessToken", ...)` with `contracts.AuthTokens` construction
  - [ ] Replace register response `JsonObject` with `contracts.User` serialization
  - [ ] Use `ctx.json()` or `Jackson.encode()` for typed response serialization
- [ ] **Wire user handlers** (`UserHandler.java`)
  - [ ] Replace request parsing with `contracts.UpdateProfileRequest` /
        `contracts.ChangePasswordRequest`
  - [ ] Replace response `JsonObject` with `contracts.User` serialization
- [ ] **Wire expense handlers** (`ExpenseHandler.java`)
  - [ ] Replace request parsing with `contracts.CreateExpenseRequest` /
        `contracts.UpdateExpenseRequest`
  - [ ] Replace response `JsonObject` with `contracts.Expense` / `contracts.ExpenseListResponse`
- [ ] **Wire admin handlers** (`AdminHandler.java`)
  - [ ] Replace request parsing with `contracts.DisableRequest`
  - [ ] Replace response `JsonObject` with `contracts.User` / `contracts.UserListResponse` /
        `contracts.PasswordResetResponse`
- [ ] **Wire attachment handlers** (`AttachmentHandler.java`)
  - [ ] Replace response `JsonObject` with `contracts.Attachment`
- [ ] **Wire report handlers** (`ReportHandler.java`)
  - [ ] Replace response `JsonObject` with `contracts.PLReport`
- [ ] **Wire token handlers** (`TokenHandler.java`)
  - [ ] Replace response `JsonObject` with `contracts.TokenClaims` / `contracts.JwksResponse`
- [ ] **Wire health handler** (`HealthHandler.java`)
  - [ ] Replace response `JsonObject` with `contracts.HealthResponse`
- [ ] **Update all tests** to use generated types instead of `JsonObject` assertions
- [ ] **Verify** `nx run demo-be-java-vertx:build` passes
- [ ] **Verify** `nx run demo-be-java-vertx:test:quick` passes with >=90% coverage

---

### Phase 6: Wire demo-be-kotlin-ktor (Kotlin/Ktor)

**Goal**: Replace 9 inline data classes with generated imports. Convert `mapOf()` responses to
generated type instances.

- [ ] **Verify** `build.gradle.kts` includes `generated-contracts/src/main/kotlin` in source set
- [ ] **Wire `AuthRoutes.kt`** (request + response)
  - [ ] Remove local `RegisterRequest` data class — import `contracts.RegisterRequest`
  - [ ] Remove local `LoginRequest` data class — import `contracts.LoginRequest`
  - [ ] Remove local `RefreshRequest` data class — import `contracts.RefreshRequest`
  - [ ] Keep local `LogoutRequest` (not in spec)
  - [ ] Replace `call.respond(mapOf(...))` login response with `call.respond(contracts.AuthTokens(...))`
  - [ ] Replace `call.respond(mapOf(...))` register response with `call.respond(contracts.User(...))`
  - [ ] Replace `call.respond(mapOf(...))` refresh response with `call.respond(contracts.AuthTokens(...))`
- [ ] **Wire `UserRoutes.kt`** (request + response)
  - [ ] Remove local `UpdateDisplayNameRequest` — import `contracts.UpdateProfileRequest`
  - [ ] Remove local `ChangePasswordRequest` — import `contracts.ChangePasswordRequest`
  - [ ] Replace `call.respond(mapOf(...))` user responses with `call.respond(contracts.User(...))`
- [ ] **Wire `ExpenseRoutes.kt`** (request + response)
  - [ ] Remove local `CreateExpenseDto` — import `contracts.CreateExpenseRequest`
  - [ ] Replace `call.respond(mapOf(...))` expense responses with `call.respond(contracts.Expense(...))`
  - [ ] Replace expense list response with `call.respond(contracts.ExpenseListResponse(...))`
- [ ] **Wire `AdminRoutes.kt`** (request + response)
  - [ ] Remove local `DisableUserRequest` — import `contracts.DisableRequest`
  - [ ] Replace admin responses with generated types (`User`, `UserListResponse`,
        `PasswordResetResponse`)
- [ ] **Wire `AttachmentRoutes.kt`** (response only)
  - [ ] Replace attachment responses with `contracts.Attachment(...)`
- [ ] **Wire `ReportRoutes.kt`** (response only)
  - [ ] Replace P&L report response with `contracts.PLReport(...)`
- [ ] **Wire `TokenRoutes.kt`** (response only)
  - [ ] Replace token responses with `contracts.TokenClaims(...)` / `contracts.JwksResponse(...)`
- [ ] **Wire `HealthRoutes.kt`** (response only)
  - [ ] Replace health response with `contracts.HealthResponse(...)`
- [ ] **Wire `TestRoutes.kt`**
  - [ ] Keep local `PromoteAdminRequest` (test-only, not in spec)
- [ ] **Update all tests** referencing removed data classes
- [ ] **Verify** `nx run demo-be-kotlin-ktor:build` passes
- [ ] **Verify** `nx run demo-be-kotlin-ktor:test:quick` passes with >=90% coverage

---

### Phase 7: Wire demo-be-rust-axum (Rust/Axum)

**Goal**: Replace 12 local structs with generated model imports. Add generated-contracts as crate
dependency.

- [ ] **Add crate dependency**
  - [ ] Verify `generated-contracts/Cargo.toml` crate name
  - [ ] Add path dependency to main `Cargo.toml`
- [ ] **Wire `src/handlers/auth.rs`** (request + response)
  - [ ] Remove local `RegisterRequest`, `LoginRequest`, `RefreshRequest` structs
  - [ ] Remove local `RegisterResponse`, `LoginResponse` structs
  - [ ] Import generated types: `use contracts::models::{RegisterRequest, LoginRequest, ...}`
  - [ ] Replace `RegisterResponse` with `models::User`
  - [ ] Replace `LoginResponse` with `models::AuthTokens`
- [ ] **Wire `src/handlers/user.rs`** (request + response)
  - [ ] Remove local `UpdateProfileRequest`, `ChangePasswordRequest` structs
  - [ ] Remove local `UserProfile` struct
  - [ ] Import generated types
  - [ ] Replace `UserProfile` with `models::User`
- [ ] **Wire `src/handlers/expense.rs`** (request + response)
  - [ ] Remove local `CreateExpenseRequest` struct
  - [ ] Import generated types including `Expense`, `ExpenseListResponse`
  - [ ] Type-annotate expense responses with generated types
- [ ] **Wire `src/handlers/admin.rs`** (request + response)
  - [ ] Remove local `DisableUserRequest`, `UserSummary`, `ListUsersResponse` structs
  - [ ] Import `models::DisableRequest`, `models::User`, `models::UserListResponse`
  - [ ] Replace `UserSummary` with `models::User`
  - [ ] Replace `ListUsersResponse` with `models::UserListResponse`
- [ ] **Wire `src/handlers/attachment.rs`** (response only)
  - [ ] Import `models::Attachment`
  - [ ] Type-annotate attachment responses
- [ ] **Wire `src/handlers/report.rs`** (response only)
  - [ ] Import `models::PLReport`
  - [ ] Type-annotate P&L report response
- [ ] **Wire `src/handlers/token.rs`** (response only)
  - [ ] Import `models::TokenClaims`, `models::JwksResponse`
  - [ ] Type-annotate token responses
- [ ] **Wire `src/handlers/health.rs`** (response only)
  - [ ] Import `models::HealthResponse`
  - [ ] Type-annotate health response
- [ ] **Update all tests** referencing removed structs
- [ ] **Verify** `nx run demo-be-rust-axum:build` passes
- [ ] **Verify** `nx run demo-be-rust-axum:test:quick` passes with >=90% coverage

---

### Phase 8: Wire demo-be-python-fastapi (Python/FastAPI)

**Goal**: Replace 18 local Pydantic models with generated imports. Update `response_model=`
parameters to use generated types.

- [ ] **Verify** `from generated_contracts import LoginRequest` works from app source root
- [ ] **Wire `routers/auth.py`** (request + response)
  - [ ] Remove local `RegisterRequest` class — import from `generated_contracts`
  - [ ] Remove local `LoginRequest` class — import from `generated_contracts`
  - [ ] Remove local `RefreshRequest` class — import from `generated_contracts`
  - [ ] Remove local `TokenResponse` class — import `AuthTokens` from `generated_contracts`
  - [ ] Remove local `RegisterResponse` class — evaluate (use `User` or keep)
  - [ ] Update `response_model=` parameters to use generated types
- [ ] **Wire `routers/users.py`** (request + response)
  - [ ] Remove local `UpdateProfileRequest` — import from `generated_contracts`
  - [ ] Remove local `ChangePasswordRequest` — import from `generated_contracts`
  - [ ] Remove local `UserProfileResponse` — import `User` from `generated_contracts`
  - [ ] Update `response_model=` to use `User`
- [ ] **Wire `routers/expenses.py`** (request + response)
  - [ ] Remove local `ExpenseRequest` — import `CreateExpenseRequest` from `generated_contracts`
  - [ ] Add import for `UpdateExpenseRequest` (may not exist locally)
  - [ ] Remove local `ExpenseResponse` — import `Expense` from `generated_contracts`
  - [ ] Remove local `ExpenseListResponse` — import from `generated_contracts`
  - [ ] Update `response_model=` parameters
- [ ] **Wire `routers/admin.py`** (request + response)
  - [ ] Remove local `DisableRequest` — import from `generated_contracts`
  - [ ] Remove local `UserSummary` — import `User` from `generated_contracts`
  - [ ] Remove local `UserListResponse` — import from `generated_contracts`
  - [ ] Update `response_model=` parameters
- [ ] **Wire `routers/attachments.py`** (response only)
  - [ ] Remove local `AttachmentResponse` — import `Attachment` from `generated_contracts`
  - [ ] Evaluate `AttachmentListResponse` (not in spec — keep or add)
  - [ ] Update `response_model=`
- [ ] **Wire `routers/reports.py`** (response only)
  - [ ] Remove local `BreakdownItem` — import `CategoryBreakdown` from `generated_contracts`
  - [ ] Remove local `PLResponse` — import `PLReport` from `generated_contracts`
  - [ ] Update `response_model=`
- [ ] **Wire `routers/tokens.py`** (response only)
  - [ ] Remove local `ClaimsResponse` — import `TokenClaims` from `generated_contracts`
  - [ ] Update `response_model=`
- [ ] **Wire `routers/health.py`** (response only)
  - [ ] Remove local `HealthResponse` — import from `generated_contracts`
  - [ ] Update `response_model=`
- [ ] **Update all tests** referencing removed models
- [ ] **Verify** `nx run demo-be-python-fastapi:test:quick` passes with >=90% coverage

---

### Phase 9: Wire demo-be-fsharp-giraffe and demo-be-csharp-aspnetcore (.NET)

**Goal**: Add ProjectReference to generated projects. Replace inline records/classes with
generated types for both request parsing and response construction.

**demo-be-fsharp-giraffe**:

- [ ] **Add ProjectReference** to generated `.fsproj` in main app's `.fsproj`
- [ ] **Wire `AuthHandler.fs`** (request + response)
  - [ ] Remove local `RegisterRequest`, `LoginRequest`, `RefreshRequest` records
  - [ ] Open `OpenAPI.DemoBeFsgi.Contracts` namespace
  - [ ] Use generated types for request deserialization
  - [ ] Use generated types for response construction (`AuthTokens`, `User`)
- [ ] **Wire `UserHandler.fs`** (request + response)
  - [ ] Remove local `UpdateProfileRequest`, `ChangePasswordRequest` records
  - [ ] Use generated types for request and response
- [ ] **Wire `ExpenseHandler.fs`** (request + response)
  - [ ] Remove local `CreateExpenseRequest`, `UpdateExpenseRequest` records
  - [ ] Use generated types for request and response (`Expense`, `ExpenseListResponse`)
- [ ] **Wire `AdminHandler.fs`** (request + response)
  - [ ] Remove local `DisableRequest` record
  - [ ] Use generated types for request and response (`User`, `UserListResponse`,
        `PasswordResetResponse`)
- [ ] **Wire `AttachmentHandler.fs`** (response only)
  - [ ] Use generated `Attachment` for response
- [ ] **Wire `ReportHandler.fs`** (response only)
  - [ ] Use generated `PLReport` for response
- [ ] **Wire `TokenHandler.fs`** (response only)
  - [ ] Use generated `TokenClaims`, `JwksResponse` for response
- [ ] **Wire `HealthHandler.fs`** (response only)
  - [ ] Use generated `HealthResponse` for response
- [ ] **Update all tests** referencing removed records
- [ ] **Verify** `nx run demo-be-fsharp-giraffe:build` passes
- [ ] **Verify** `nx run demo-be-fsharp-giraffe:test:quick` passes with >=90% coverage

**demo-be-csharp-aspnetcore**:

- [ ] **Add ProjectReference** to generated `.csproj` in main app's `.csproj`
- [ ] **Wire `AuthEndpoints.cs`** (request + response)
  - [ ] Remove local `RegisterRequest`, `LoginRequest`, `RefreshRequest` sealed records
  - [ ] Add `using DemoBeCsas.Contracts;`
  - [ ] Use generated types for request binding and response (`AuthTokens`, `User`)
- [ ] **Wire `UserEndpoints.cs`** (request + response)
  - [ ] Remove local `PatchMeRequest` (replace with `UpdateProfileRequest`)
  - [ ] Remove local `ChangePasswordRequest`
  - [ ] Use generated types for request and response
- [ ] **Wire `ExpenseEndpoints.cs`** (request + response)
  - [ ] Remove local `ExpenseRequest` (replace with `CreateExpenseRequest` + `UpdateExpenseRequest`)
  - [ ] Use generated types for request and response
- [ ] **Wire `AdminEndpoints.cs`** (request + response)
  - [ ] Use generated types for disable request and responses
- [ ] **Wire `AttachmentEndpoints.cs`** (response only)
  - [ ] Use generated `Attachment` for response
- [ ] **Wire `ReportEndpoints.cs`** (response only)
  - [ ] Use generated `PLReport` for response
- [ ] **Wire `TokenEndpoints.cs`** (response only)
  - [ ] Use generated `TokenClaims`, `JwksResponse` for response
- [ ] **Wire `HealthEndpoints.cs`** (response only)
  - [ ] Use generated `HealthResponse` for response
- [ ] **Update all tests** referencing removed records
- [ ] **Verify** `nx run demo-be-csharp-aspnetcore:build` passes
- [ ] **Verify** `nx run demo-be-csharp-aspnetcore:test:quick` passes with >=90% coverage

---

### Phase 10: Wire demo-be-elixir-phoenix and demo-be-clojure-pedestal (Dynamic Languages)

**Goal**: Wire Elixir and Clojure backends. Enforcement is at test time via struct construction
(Elixir) and schema validation (Clojure).

**demo-be-elixir-phoenix**:

- [ ] **Add generated-contracts to Mix source paths** in `mix.exs`
- [ ] **Wire `AuthController`** (request validation + response struct construction)
  - [ ] Alias generated `LoginRequest`, `RegisterRequest`, `RefreshRequest` modules
  - [ ] Validate incoming params against generated struct fields
  - [ ] Construct `%AuthTokens{}` for login/refresh responses
  - [ ] Construct `%User{}` for register/profile responses
- [ ] **Wire `UserController`** (request + response)
  - [ ] Alias generated `UpdateProfileRequest`, `ChangePasswordRequest`
  - [ ] Validate incoming params
  - [ ] Construct `%User{}` for user profile response
- [ ] **Wire `ExpenseController`** (request + response)
  - [ ] Alias generated `CreateExpenseRequest`, `UpdateExpenseRequest`
  - [ ] Validate incoming params
  - [ ] Construct `%Expense{}` / `%ExpenseListResponse{}` for responses
- [ ] **Wire `AdminController`** (request + response)
  - [ ] Alias generated `DisableRequest`
  - [ ] Construct `%User{}` / `%UserListResponse{}` / `%PasswordResetResponse{}` for responses
- [ ] **Wire `AttachmentController`** (response only)
  - [ ] Construct `%Attachment{}` for responses
- [ ] **Wire `ReportController`** (response only)
  - [ ] Construct `%PLReport{}` for response
- [ ] **Wire `TokenController`** (response only)
  - [ ] Construct `%TokenClaims{}` / `%JwksResponse{}` for responses
- [ ] **Wire `HealthController`** (response only)
  - [ ] Construct `%HealthResponse{}` for response
- [ ] **Add struct construction tests** — at least one test per generated struct verifying
      `@enforce_keys` catches missing required fields
- [ ] **Verify** `nx run demo-be-elixir-phoenix:test:quick` passes

**demo-be-clojure-pedestal**:

- [ ] **Add generated schemas to classpath** in `deps.edn`
- [ ] **Create contract validation helper**
  - [ ] Create `contracts.clj` namespace that requires all generated schemas
  - [ ] Add `validate-response` function using `m/validate`
- [ ] **Wire auth handlers** (request + response validation)
  - [ ] Require generated request schemas for validation
  - [ ] Add `validate-response` calls on login/register/refresh responses against
        `auth-tokens` / `user` generated schemas
- [ ] **Wire user handlers** (request + response validation)
  - [ ] Validate user profile response against `user` schema
- [ ] **Wire expense handlers** (request + response validation)
  - [ ] Validate expense responses against `expense` / `expense-list-response` schemas
- [ ] **Wire admin handlers** (request + response validation)
  - [ ] Validate admin responses against `user` / `user-list-response` /
        `password-reset-response` schemas
- [ ] **Wire attachment handlers** (response validation)
  - [ ] Validate against `attachment` schema
- [ ] **Wire report handlers** (response validation)
  - [ ] Validate against `pl-report` schema
- [ ] **Wire token handlers** (response validation)
  - [ ] Validate against `token-claims` / `jwks-response` schemas
- [ ] **Wire health handler** (response validation)
  - [ ] Validate against `health-response` schema
- [ ] **Add schema validation tests** — at least one test per generated schema verifying
      validation catches missing required fields
- [ ] **Verify** `nx run demo-be-clojure-pedestal:test:quick` passes

---

### Phase 11: Wire demo-fe-dart-flutterweb (Dart/Flutter Web)

**Goal**: Replace 20+ hand-written model classes with generated Dart classes.

- [ ] **Add generated package as path dependency** in `pubspec.yaml`
- [ ] **Run `flutter pub get`** to install
- [ ] **Wire `lib/models/auth.dart`**
  - [ ] Replace local `LoginRequest`, `RegisterRequest`, `AuthTokens` with re-exports from
        generated package
- [ ] **Wire `lib/models/user.dart`**
  - [ ] Replace local `User`, `UserListResponse`, `UpdateProfileRequest`,
        `ChangePasswordRequest`, `DisableRequest`, `PasswordResetResponse` with generated types
- [ ] **Wire `lib/models/expense.dart`**
  - [ ] Replace local `Expense`, `ExpenseListResponse`, `CreateExpenseRequest`,
        `UpdateExpenseRequest` with generated types
- [ ] **Wire `lib/models/attachment.dart`**
  - [ ] Replace local `Attachment` with generated type
- [ ] **Wire `lib/models/token.dart`**
  - [ ] Replace local `TokenClaims`, `JwkKey`, `JwksResponse` with generated types
- [ ] **Wire `lib/models/report.dart`**
  - [ ] Replace local `CategoryBreakdown`, `ExpenseSummary`, `PLReport` with generated types
- [ ] **Wire `lib/models/health.dart`**
  - [ ] Replace local `HealthResponse` with generated type
- [ ] **Update `lib/services/*.dart`** to compile against updated model imports
- [ ] **Verify** `dart analyze` passes with no errors
- [ ] **Verify** `nx run demo-fe-dart-flutterweb:test:quick` passes with >=70% coverage

---

### Phase 12: Wire E2E Contract Validation (demo-be-e2e + demo-fe-e2e)

**Goal**: Activate the existing `validateResponseAgainstContract` function in all E2E step
definitions.

**demo-be-e2e**:

- [ ] **Wire auth step definitions** (`tests/steps/auth/`)
  - [ ] Import `validateResponseAgainstContract`
  - [ ] Add contract validation after every 2xx response in login, register, refresh, logout steps
- [ ] **Wire expense step definitions** (`tests/steps/expenses/`)
  - [ ] Add contract validation after every 2xx response in CRUD expense steps
- [ ] **Wire user step definitions** (`tests/steps/user/` or `tests/steps/users/`)
  - [ ] Add contract validation after every 2xx response in profile, password change steps
- [ ] **Wire admin step definitions** (`tests/steps/admin/`)
  - [ ] Add contract validation after every 2xx response in user management steps
- [ ] **Wire attachment step definitions** (`tests/steps/attachments/`)
  - [ ] Add contract validation after every 2xx response
- [ ] **Wire report step definitions** (`tests/steps/reports/`)
  - [ ] Add contract validation after every 2xx response
- [ ] **Wire token step definitions** (`tests/steps/token-management/` or `tests/steps/tokens/`)
  - [ ] Add contract validation after every 2xx response
- [ ] **Wire health step definitions** (if exists)
  - [ ] Add contract validation after health check response
- [ ] **Wire common/shared steps** (`tests/steps/common.steps.ts` or similar)
  - [ ] Add contract validation for any shared response handling

**demo-fe-e2e**:

- [ ] **Wire all step definition files** (same pattern as demo-be-e2e)
  - [ ] Import `validateResponseAgainstContract` in each step file
  - [ ] Add contract validation after every 2xx HTTP response
- [ ] **Verify** grep confirms `validateResponseAgainstContract` is imported in all step files

**Validation**:

- [ ] **Verify** `nx run demo-be-e2e:test:e2e` passes (against a running backend)
- [ ] **Verify** `nx run demo-fe-e2e:test:e2e` passes (against running frontend + backend)

---

### Phase 13: End-to-End Verification

**Goal**: Verify the full enforcement model works. All apps pass. Contract changes cause failures.

- [ ] **Run full test suite**
  - [ ] `nx run-many -t test:quick --projects=demo-*` — all 16 demo apps pass
- [ ] **Run builds for compiled backends**
  - [ ] `nx run-many -t build --projects=demo-be-golang-gin,demo-be-java-springboot,demo-be-java-vertx,demo-be-kotlin-ktor,demo-be-rust-axum,demo-be-fsharp-giraffe,demo-be-csharp-aspnetcore`
- [ ] **Run typechecks for TypeScript and Dart**
  - [ ] `nx run-many -t typecheck --projects=demo-be-ts-effect,demo-fe-dart-flutterweb`
- [ ] **Enforcement smoke test** (do NOT commit)
  - [ ] Rename `accessToken` to `token` in `specs/apps/demo/contracts/schemas/auth.yaml`
  - [ ] Run `nx run demo-contracts:bundle`
  - [ ] Run `nx run demo-be-golang-gin:codegen && nx run demo-be-golang-gin:build` — expect failure
  - [ ] Run `nx run demo-be-ts-effect:codegen && nx run demo-be-ts-effect:typecheck` — expect
        failure
  - [ ] Run `nx run demo-be-python-fastapi:codegen && nx run demo-be-python-fastapi:test:unit` —
        expect failure
  - [ ] Revert the rename in `schemas/auth.yaml` and rebundle
- [ ] **Verify pre-push hook**: Stage and push a minor change; confirm hook runs `test:quick` for
      affected projects and passes
- [ ] **Update `CLAUDE.md`** — note that all 16 demo apps now import from generated-contracts/
      for both request and response types
- [ ] **Update `specs/apps/demo/contracts/README.md`** — update adoption status to reflect all
      apps are wired
- [ ] **Trigger all E2E CI workflows manually** — verify all 14 pass

**Validation**:

- `nx run-many -t test:quick --projects=demo-*` exits 0
- Enforcement smoke test confirms contract change causes failures in at least one statically typed
  and one dynamically typed app before revert
- Pre-push hook passes on clean working tree
- All 14 E2E CI workflows pass
- Documentation updated

---

## Open Questions

1. **RegisterResponse**: Several apps return a different shape for registration than the `User` type.
   Should we add `RegisterResponse` to the OpenAPI spec or standardize on returning `User`?

2. **AttachmentListResponse**: Used locally in Java-SB and Python but not in the spec. Should we
   add a list endpoint to the spec or keep local wrappers?

3. **Java Vert.x refactoring scope**: The JsonObject-to-typed-object refactoring is invasive. Should
   we prioritize compile-time safety (full refactor) or take a lighter approach (type assertions in
   tests)?

4. **Generated type field compatibility**: Some generated types may use different field types than
   local ones (e.g., `openapi_types.Date` vs `string`, `Optional<>` vs nullable). Verify
   compatibility during implementation.

5. **Kotlin `@Serializable`**: Verify generated Kotlin data classes carry `@Serializable` annotation
   needed for Ktor's `ContentNegotiation`.

6. **F# `[<CLIMutable>]`**: Verify generated F# records carry `[<CLIMutable>]` for request types.

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

- [ ] Phase 0: Evaluate Missing Spec Types
- [ ] Phase 1: Verify Codegen (Elixir, Clojure, Dart)
- [ ] Phase 2: Wire demo-be-ts-effect
- [ ] Phase 3: Wire demo-be-golang-gin
- [ ] Phase 4: Wire demo-be-java-springboot
- [ ] Phase 5: Wire demo-be-java-vertx
- [ ] Phase 6: Wire demo-be-kotlin-ktor
- [ ] Phase 7: Wire demo-be-rust-axum
- [ ] Phase 8: Wire demo-be-python-fastapi
- [ ] Phase 9: Wire demo-be-fsharp-giraffe + demo-be-csharp-aspnetcore
- [ ] Phase 10: Wire demo-be-elixir-phoenix + demo-be-clojure-pedestal
- [ ] Phase 11: Wire demo-fe-dart-flutterweb
- [ ] Phase 12: Wire E2E Contract Validation
- [ ] Phase 13: End-to-End Verification
