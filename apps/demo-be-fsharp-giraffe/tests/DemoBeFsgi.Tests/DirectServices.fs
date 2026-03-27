/// Direct service layer for integration tests.
///
/// Each function mirrors a Giraffe handler but works entirely via repository function records —
/// no HTTP, no HttpContext, no WebApplicationFactory. Each function returns a
/// (status: int * body: string) pair that the step definitions treat as a
/// simulated HTTP response, preserving the HTTP-oriented Gherkin language.
module DemoBeFsgi.Tests.DirectServices

open System
open System.Text.Json
open DemoBeFsgi.Infrastructure.AppDbContext
open DemoBeFsgi.Infrastructure.Repositories.RepositoryTypes
open DemoBeFsgi.Infrastructure.PasswordHasher
open DemoBeFsgi.Domain.Types
open DemoBeFsgi.Domain.User
open DemoBeFsgi.Domain.Expense
open DemoBeFsgi.Domain.Attachment
open DemoBeFsgi.Auth.JwtService

// ─────────────────────────────────────────────────────────────────────────────
// Helpers
// ─────────────────────────────────────────────────────────────────────────────

let private opts = JsonSerializerOptions(PropertyNameCaseInsensitive = true)

let private ok (payload: obj) =
    200, JsonSerializer.Serialize(payload, opts)

let private created (payload: obj) =
    201, JsonSerializer.Serialize(payload, opts)

let private noContent () = 204, ""

let private badRequest (message: string) =
    400,
    JsonSerializer.Serialize(
        {| error = "Bad Request"
           message = message |},
        opts
    )

let private validationError (field: string) (message: string) =
    400,
    JsonSerializer.Serialize(
        {| error = "Validation Error"
           field = field
           message = message |},
        opts
    )

let private unauthorized (message: string) =
    401,
    JsonSerializer.Serialize(
        {| error = "Unauthorized"
           message = message |},
        opts
    )

let private forbidden (message: string) =
    403,
    JsonSerializer.Serialize(
        {| error = "Forbidden"
           message = message |},
        opts
    )

let private notFound (message: string) =
    404,
    JsonSerializer.Serialize(
        {| error = "Not Found"
           message = message |},
        opts
    )

let private conflict (message: string) =
    409,
    JsonSerializer.Serialize(
        {| error = "Conflict"
           message = message |},
        opts
    )

let private unsupportedMediaType (field: string) (message: string) =
    415,
    JsonSerializer.Serialize(
        {| error = "Unsupported Media Type"
           field = field
           message = message |},
        opts
    )

let private fileTooLarge (limit: int64) =
    413,
    JsonSerializer.Serialize(
        {| error = "File Too Large"
           message = $"File exceeds maximum size of {limit} bytes" |},
        opts
    )

let private parseAmount (s: string) =
    if String.IsNullOrEmpty(s) then
        Error(ValidationError("amount", "Amount is required"))
    else
        match Decimal.TryParse(s, Globalization.NumberStyles.Any, Globalization.CultureInfo.InvariantCulture) with
        | true, v -> Ok v
        | _ -> Error(ValidationError("amount", "Invalid amount format"))

// ─────────────────────────────────────────────────────────────────────────────
// Token auth — resolves a JWT to a UserId (replaces requireAuth middleware)
// ─────────────────────────────────────────────────────────────────────────────

/// Validates the JWT token string against the repositories.
/// Returns Ok userId or Error (status, body).
let resolveAuth
    (userRepo: UserRepository)
    (tokenRepo: TokenRepository)
    (token: string option)
    : Async<Result<Guid, int * string>> =
    async {
        match token with
        | None -> return Error(unauthorized "Missing or invalid Authorization header")
        | Some t ->
            let principal = validateToken t

            match principal with
            | None -> return Error(unauthorized "Invalid or expired token")
            | Some claims ->
                let jti = getTokenJti t

                let! isRevoked =
                    match jti with
                    | None -> async { return true }
                    | Some j -> tokenRepo.ExistsJti j |> Async.AwaitTask

                if isRevoked then
                    return Error(unauthorized "Token has been revoked")
                else
                    let sub =
                        claims.FindFirst(fun c ->
                            c.Type = "http://schemas.xmlsoap.org/ws/2005/05/identity/claims/nameidentifier")

                    let sub2 =
                        if sub = null then
                            claims.FindFirst(fun c -> c.Type = "sub")
                        else
                            sub

                    if sub2 = null then
                        return Error(unauthorized "Invalid token claims")
                    else
                        let userId =
                            try
                                Guid.Parse(sub2.Value) |> Some
                            with _ ->
                                None

                        match userId with
                        | None -> return Error(unauthorized "Invalid user ID in token")
                        | Some uid ->
                            let! userOpt = userRepo.FindById uid |> Async.AwaitTask

                            match userOpt with
                            | None -> return Error(unauthorized "User not found")
                            | Some user when user.Status = statusToString Locked ->
                                return Error(unauthorized "Account is locked after too many failed attempts")
                            | Some user when user.Status = statusToString Inactive ->
                                return Error(unauthorized "Account has been deactivated")
                            | Some user when user.Status = statusToString Disabled ->
                                return Error(unauthorized "Account has been disabled by an administrator")
                            | Some user when user.Status <> statusToString Active ->
                                return Error(unauthorized "Account is not active")
                            | Some _ -> return Ok uid
    }

/// Validates the JWT token string and additionally checks that the user is admin.
let resolveAdmin
    (userRepo: UserRepository)
    (tokenRepo: TokenRepository)
    (token: string option)
    : Async<Result<Guid, int * string>> =
    async {
        let! authResult = resolveAuth userRepo tokenRepo token

        match authResult with
        | Error e -> return Error e
        | Ok uid ->
            let! userOpt = userRepo.FindById uid |> Async.AwaitTask

            match userOpt with
            | None -> return Error(forbidden "User not found")
            | Some user when user.Role <> roleToString Admin -> return Error(forbidden "Admin role required")
            | Some _ -> return Ok uid
    }

// ─────────────────────────────────────────────────────────────────────────────
// Health
// ─────────────────────────────────────────────────────────────────────────────

let health () : int * string = ok {| status = "UP" |}

// ─────────────────────────────────────────────────────────────────────────────
// Auth
// ─────────────────────────────────────────────────────────────────────────────

let register (userRepo: UserRepository) (username: string) (email: string) (password: string) : Async<int * string> =
    async {
        let usernameResult = validateUsername (if username = null then "" else username)
        let emailResult = validateEmail (if email = null then "" else email)
        let passwordResult = validatePassword (if password = null then "" else password)

        match usernameResult, emailResult, passwordResult with
        | Error(ValidationError(f, m)), _, _ -> return validationError f m
        | _, Error(ValidationError(f, m)), _ -> return validationError f m
        | _, _, Error(ValidationError(f, m)) -> return validationError f m
        | Ok _, Ok _, Ok _ ->
            let! existing = userRepo.FindByUsername username |> Async.AwaitTask

            if existing.IsSome then
                return conflict "Username already exists"
            else
                let now = DateTime.UtcNow
                let userId = Guid.NewGuid()

                let entity: UserEntity =
                    { Id = userId
                      Username = username
                      Email = email
                      DisplayName = username
                      PasswordHash = hashPassword password
                      Role = roleToString User
                      Status = statusToString Active
                      FailedLoginAttempts = 0
                      CreatedAt = now
                      UpdatedAt = now }

                let! _ = userRepo.Create entity |> Async.AwaitTask

                return
                    created
                        {| id = userId
                           username = entity.Username
                           email = entity.Email
                           displayName = entity.DisplayName |}
        | _ -> return badRequest "Validation failed"
    }

let private maxFailedAttempts = 5

let login
    (userRepo: UserRepository)
    (rtRepo: RefreshTokenRepository)
    (username: string)
    (password: string)
    : Async<int * string> =
    async {
        let! userOpt = userRepo.FindByUsername username |> Async.AwaitTask

        match userOpt with
        | None -> return unauthorized "Invalid credentials"
        | Some user when user.Status = statusToString Locked ->
            return unauthorized "Account is locked after too many failed attempts"
        | Some user when user.Status = statusToString Inactive -> return unauthorized "Account has been deactivated"
        | Some user when user.Status = statusToString Disabled -> return unauthorized "Account has been disabled"
        | Some user when not (verifyPassword password user.PasswordHash) ->
            let newAttempts = user.FailedLoginAttempts + 1

            let newStatus =
                if newAttempts >= maxFailedAttempts then
                    statusToString Locked
                else
                    user.Status

            let updated =
                { user with
                    FailedLoginAttempts = newAttempts
                    Status = newStatus
                    UpdatedAt = DateTime.UtcNow }

            let! _ = userRepo.Update updated |> Async.AwaitTask

            if newAttempts >= maxFailedAttempts then
                return unauthorized "Account is locked after too many failed attempts"
            else
                return unauthorized "Invalid credentials"
        | Some user ->
            let updated =
                { user with
                    FailedLoginAttempts = 0
                    UpdatedAt = DateTime.UtcNow }

            let! _ = userRepo.Update updated |> Async.AwaitTask

            let accessToken = generateAccessToken user.Id user.Username user.Email user.Role
            let refreshTokenStr = generateRefreshToken user.Id
            let now = DateTime.UtcNow

            let rtEntity: RefreshTokenEntity =
                { Id = Guid.NewGuid()
                  UserId = user.Id
                  TokenHash = refreshTokenStr
                  ExpiresAt = now.AddDays(7.0)
                  CreatedAt = now
                  Revoked = false }

            let! _ = rtRepo.Create rtEntity |> Async.AwaitTask

            return
                ok
                    {| accessToken = accessToken
                       refreshToken = refreshTokenStr
                       tokenType = "Bearer" |}
    }

let refresh
    (userRepo: UserRepository)
    (rtRepo: RefreshTokenRepository)
    (refreshTokenStr: string)
    : Async<int * string> =
    async {
        let! rtEntityOpt = rtRepo.FindActiveByHash refreshTokenStr |> Async.AwaitTask

        match rtEntityOpt with
        | None -> return unauthorized "Invalid or already used token"
        | Some rtEntity when rtEntity.ExpiresAt < DateTime.UtcNow -> return unauthorized "Token has expired"
        | Some rtEntity ->
            let! userOpt = userRepo.FindById rtEntity.UserId |> Async.AwaitTask

            match userOpt with
            | None -> return unauthorized "User not found"
            | Some user when user.Status <> statusToString Active -> return unauthorized "Account has been deactivated"
            | Some user ->
                let revokedRt = { rtEntity with Revoked = true }
                let! _ = rtRepo.Update revokedRt |> Async.AwaitTask

                let accessToken = generateAccessToken user.Id user.Username user.Email user.Role
                let newRefreshToken = generateRefreshToken user.Id
                let now = DateTime.UtcNow

                let newRtEntity: RefreshTokenEntity =
                    { Id = Guid.NewGuid()
                      UserId = user.Id
                      TokenHash = newRefreshToken
                      ExpiresAt = now.AddDays(7.0)
                      CreatedAt = now
                      Revoked = false }

                let! _ = rtRepo.Create newRtEntity |> Async.AwaitTask

                return
                    ok
                        {| accessToken = accessToken
                           refreshToken = newRefreshToken
                           tokenType = "Bearer" |}
    }

let logout (tokenRepo: TokenRepository) (token: string option) : Async<int * string> =
    async {
        let tokenStr = token |> Option.defaultValue ""
        let jti = getTokenJti tokenStr

        match jti with
        | Some j ->
            let! exists = tokenRepo.ExistsJti j |> Async.AwaitTask

            if not exists then
                // Resolve userId from token (best-effort; use Guid.Empty if not available)
                let userId =
                    try
                        let handler = System.IdentityModel.Tokens.Jwt.JwtSecurityTokenHandler()
                        let jwt = handler.ReadJwtToken(tokenStr)

                        let sub =
                            jwt.Claims
                            |> Seq.tryFind (fun c ->
                                c.Type = "http://schemas.xmlsoap.org/ws/2005/05/identity/claims/nameidentifier"
                                || c.Type = "sub")

                        sub
                        |> Option.map (fun c -> Guid.Parse(c.Value))
                        |> Option.defaultValue Guid.Empty
                    with _ ->
                        Guid.Empty

                let revokedEntity: RevokedTokenEntity =
                    { Id = Guid.NewGuid()
                      Jti = j
                      UserId = userId
                      RevokedAt = DateTime.UtcNow }

                let! _ = tokenRepo.Create revokedEntity |> Async.AwaitTask
                ()
        | None -> ()

        return ok {| message = "Logged out successfully" |}
    }

let logoutAll
    (userRepo: UserRepository)
    (tokenRepo: TokenRepository)
    (rtRepo: RefreshTokenRepository)
    (token: string option)
    : Async<int * string> =
    async {
        let! authResult = resolveAuth userRepo tokenRepo token

        match authResult with
        | Error e -> return e
        | Ok userId ->
            let tokenStr = token |> Option.defaultValue ""
            let jti = getTokenJti tokenStr

            match jti with
            | Some j ->
                let! exists = tokenRepo.ExistsJti j |> Async.AwaitTask

                if not exists then
                    let revokedEntity: RevokedTokenEntity =
                        { Id = Guid.NewGuid()
                          Jti = j
                          UserId = userId
                          RevokedAt = DateTime.UtcNow }

                    let! _ = tokenRepo.Create revokedEntity |> Async.AwaitTask
                    ()
            | None -> ()

            let! activeTokens = rtRepo.ListActiveByUser userId |> Async.AwaitTask

            for rt in activeTokens do
                let! _ = rtRepo.Update { rt with Revoked = true } |> Async.AwaitTask
                ()

            return ok {| message = "All sessions logged out" |}
    }

// ─────────────────────────────────────────────────────────────────────────────
// User profile
// ─────────────────────────────────────────────────────────────────────────────

let getProfile (userRepo: UserRepository) (tokenRepo: TokenRepository) (token: string option) : Async<int * string> =
    async {
        let! authResult = resolveAuth userRepo tokenRepo token

        match authResult with
        | Error e -> return e
        | Ok userId ->
            let! userOpt = userRepo.FindById userId |> Async.AwaitTask

            match userOpt with
            | None -> return notFound "User not found"
            | Some user ->
                return
                    ok
                        {| id = user.Id
                           username = user.Username
                           email = user.Email
                           displayName = user.DisplayName
                           role = user.Role
                           status = user.Status |}
    }

let updateProfile
    (userRepo: UserRepository)
    (tokenRepo: TokenRepository)
    (token: string option)
    (displayName: string)
    : Async<int * string> =
    async {
        let! authResult = resolveAuth userRepo tokenRepo token

        match authResult with
        | Error e -> return e
        | Ok userId ->
            let! userOpt = userRepo.FindById userId |> Async.AwaitTask

            match userOpt with
            | None -> return notFound "User not found"
            | Some user ->
                let updated =
                    { user with
                        DisplayName =
                            if displayName <> null then
                                displayName
                            else
                                user.DisplayName
                        UpdatedAt = DateTime.UtcNow }

                let! saved = userRepo.Update updated |> Async.AwaitTask

                return
                    ok
                        {| id = saved.Id
                           username = saved.Username
                           email = saved.Email
                           displayName = saved.DisplayName |}
    }

let changePassword
    (userRepo: UserRepository)
    (tokenRepo: TokenRepository)
    (token: string option)
    (oldPassword: string)
    (newPassword: string)
    : Async<int * string> =
    async {
        let! authResult = resolveAuth userRepo tokenRepo token

        match authResult with
        | Error e -> return e
        | Ok userId ->
            let! userOpt = userRepo.FindById userId |> Async.AwaitTask

            match userOpt with
            | None -> return notFound "User not found"
            | Some user when not (verifyPassword oldPassword user.PasswordHash) ->
                return unauthorized "Invalid credentials"
            | Some user ->
                let updated =
                    { user with
                        PasswordHash = hashPassword newPassword
                        UpdatedAt = DateTime.UtcNow }

                let! _ = userRepo.Update updated |> Async.AwaitTask
                return ok {| message = "Password changed successfully" |}
    }

let deactivate (userRepo: UserRepository) (tokenRepo: TokenRepository) (token: string option) : Async<int * string> =
    async {
        let! authResult = resolveAuth userRepo tokenRepo token

        match authResult with
        | Error e -> return e
        | Ok userId ->
            let! userOpt = userRepo.FindById userId |> Async.AwaitTask

            match userOpt with
            | None -> return notFound "User not found"
            | Some user ->
                let updated =
                    { user with
                        Status = statusToString Inactive
                        UpdatedAt = DateTime.UtcNow }

                let! _ = userRepo.Update updated |> Async.AwaitTask
                return ok {| message = "Account deactivated" |}
    }

// ─────────────────────────────────────────────────────────────────────────────
// Admin
// ─────────────────────────────────────────────────────────────────────────────

let listUsers
    (userRepo: UserRepository)
    (tokenRepo: TokenRepository)
    (token: string option)
    (page: int)
    (size: int)
    (emailFilter: string option)
    : Async<int * string> =
    async {
        let! authResult = resolveAdmin userRepo tokenRepo token

        match authResult with
        | Error e -> return e
        | Ok _ ->
            let p = Math.Max(1, page)
            let s = Math.Max(1, size)

            let! total = userRepo.CountByFilter emailFilter |> Async.AwaitTask
            let! users = userRepo.ListByFilter emailFilter p s |> Async.AwaitTask

            let userData =
                users
                |> List.map (fun u ->
                    {| id = u.Id
                       username = u.Username
                       email = u.Email
                       displayName = u.DisplayName
                       role = u.Role
                       status = u.Status |})
                |> List.toArray

            return
                ok
                    {| content = userData
                       totalElements = total
                       page = p
                       size = s |}
    }

let disableUser
    (userRepo: UserRepository)
    (tokenRepo: TokenRepository)
    (token: string option)
    (targetUserId: Guid)
    : Async<int * string> =
    async {
        let! authResult = resolveAdmin userRepo tokenRepo token

        match authResult with
        | Error e -> return e
        | Ok _ ->
            let! userOpt = userRepo.FindById targetUserId |> Async.AwaitTask

            match userOpt with
            | None -> return notFound "User not found"
            | Some user ->
                let updated =
                    { user with
                        Status = statusToString Disabled
                        UpdatedAt = DateTime.UtcNow }

                let! _ = userRepo.Update updated |> Async.AwaitTask

                return
                    ok
                        {| message = "User disabled"
                           id = targetUserId
                           status = statusToString Disabled |}
    }

let enableUser
    (userRepo: UserRepository)
    (tokenRepo: TokenRepository)
    (token: string option)
    (targetUserId: Guid)
    : Async<int * string> =
    async {
        let! authResult = resolveAdmin userRepo tokenRepo token

        match authResult with
        | Error e -> return e
        | Ok _ ->
            let! userOpt = userRepo.FindById targetUserId |> Async.AwaitTask

            match userOpt with
            | None -> return notFound "User not found"
            | Some user ->
                let updated =
                    { user with
                        Status = statusToString Active
                        UpdatedAt = DateTime.UtcNow }

                let! _ = userRepo.Update updated |> Async.AwaitTask

                return
                    ok
                        {| message = "User enabled"
                           id = targetUserId
                           status = statusToString Active |}
    }

let unlockUser
    (userRepo: UserRepository)
    (tokenRepo: TokenRepository)
    (token: string option)
    (targetUserId: Guid)
    : Async<int * string> =
    async {
        let! authResult = resolveAdmin userRepo tokenRepo token

        match authResult with
        | Error e -> return e
        | Ok _ ->
            let! userOpt = userRepo.FindById targetUserId |> Async.AwaitTask

            match userOpt with
            | None -> return notFound "User not found"
            | Some user ->
                let updated =
                    { user with
                        Status = statusToString Active
                        FailedLoginAttempts = 0
                        UpdatedAt = DateTime.UtcNow }

                let! _ = userRepo.Update updated |> Async.AwaitTask

                return
                    ok
                        {| message = "User unlocked"
                           id = targetUserId
                           status = statusToString Active |}
    }

let forcePasswordReset
    (userRepo: UserRepository)
    (tokenRepo: TokenRepository)
    (token: string option)
    (targetUserId: Guid)
    : Async<int * string> =
    async {
        let! authResult = resolveAdmin userRepo tokenRepo token

        match authResult with
        | Error e -> return e
        | Ok _ ->
            let! userOpt = userRepo.FindById targetUserId |> Async.AwaitTask

            match userOpt with
            | None -> return notFound "User not found"
            | Some _ ->
                let resetToken = Guid.NewGuid().ToString("N")

                return
                    ok
                        {| message = "Password reset token generated"
                           token = resetToken |}
    }

/// Test-only: set a user's role to ADMIN without authentication.
let setAdminRole (userRepo: UserRepository) (username: string) : Async<int * string> =
    async {
        let! userOpt = userRepo.FindByUsername username |> Async.AwaitTask

        match userOpt with
        | None -> return notFound "User not found"
        | Some user ->
            let updated = { user with Role = roleToString Admin }
            let! _ = userRepo.Update updated |> Async.AwaitTask
            return ok {| message = "Role set to admin" |}
    }

// ─────────────────────────────────────────────────────────────────────────────
// Expenses
// ─────────────────────────────────────────────────────────────────────────────

let createExpense
    (userRepo: UserRepository)
    (tokenRepo: TokenRepository)
    (expenseRepo: ExpenseRepository)
    (token: string option)
    (amount: string)
    (currency: string)
    (category: string)
    (description: string)
    (date: string)
    (entryType: string)
    (quantity: float option)
    (unit: string option)
    : Async<int * string> =
    async {
        let! authResult = resolveAuth userRepo tokenRepo token

        match authResult with
        | Error e -> return e
        | Ok userId ->
            let currencyResult = parseCurrency (if currency = null then "" else currency)

            match currencyResult with
            | Error(ValidationError(f, m)) -> return validationError f m
            | Error _ -> return validationError "currency" "Invalid currency"
            | Ok _ ->
                let amountResult = parseAmount amount

                match amountResult with
                | Error(ValidationError(f, m)) -> return validationError f m
                | Error _ -> return validationError "amount" "Invalid amount"
                | Ok amt ->
                    let amountValidation = validateAmount amt

                    match amountValidation with
                    | Error(ValidationError(f, m)) -> return validationError f m
                    | Error _ -> return validationError "amount" "Invalid amount"
                    | Ok validAmount ->
                        let unitOpt = if unit = None || unit = Some "" then None else unit
                        let unitResult = validateUnit unitOpt

                        match unitResult with
                        | Error(ValidationError(f, m)) -> return validationError f m
                        | Error _ -> return validationError "unit" "Invalid unit"
                        | Ok validUnit ->
                            let dateVal =
                                match DateTime.TryParse(date) with
                                | true, d -> DateTime.SpecifyKind(d, DateTimeKind.Utc)
                                | _ -> DateTime.UtcNow

                            let now = DateTime.UtcNow
                            let expenseId = Guid.NewGuid()

                            let entity: ExpenseEntity =
                                { Id = expenseId
                                  UserId = userId
                                  Amount = validAmount
                                  Currency = currency.ToUpperInvariant()
                                  Category = if category = null then "" else category
                                  Description = if description = null then "" else description
                                  Date = dateVal
                                  Type =
                                    if entryType = null then
                                        "EXPENSE"
                                    else
                                        entryType.ToUpperInvariant()
                                  Quantity =
                                    match quantity with
                                    | Some q -> Nullable(decimal q)
                                    | None -> Nullable()
                                  Unit =
                                    match validUnit with
                                    | Some u -> u
                                    | None -> null
                                  CreatedAt = now
                                  UpdatedAt = now }

                            let! _ = expenseRepo.Create entity |> Async.AwaitTask

                            let formattedAmount =
                                match currency.ToUpperInvariant() with
                                | "IDR" -> validAmount.ToString("0")
                                | _ -> validAmount.ToString("0.00")

                            return
                                created
                                    {| id = expenseId
                                       amount = formattedAmount
                                       currency = entity.Currency
                                       category = entity.Category
                                       description = entity.Description
                                       date = entity.Date.ToString("yyyy-MM-dd")
                                       ``type`` = entity.Type.ToLowerInvariant() |}
    }

let listExpenses
    (userRepo: UserRepository)
    (tokenRepo: TokenRepository)
    (expenseRepo: ExpenseRepository)
    (token: string option)
    (page: int)
    (size: int)
    : Async<int * string> =
    async {
        let! authResult = resolveAuth userRepo tokenRepo token

        match authResult with
        | Error e -> return e
        | Ok userId ->
            let p = Math.Max(1, page)
            let s = Math.Max(1, size)

            let! total, expenses = expenseRepo.ListByUser userId p s |> Async.AwaitTask

            let data =
                expenses
                |> List.map (fun e ->
                    let formattedAmount =
                        match e.Currency with
                        | "IDR" -> e.Amount.ToString("0")
                        | _ -> e.Amount.ToString("0.00")

                    let qtyOpt =
                        if e.Quantity.HasValue then
                            Some(float e.Quantity.Value)
                        else
                            None

                    {| id = e.Id
                       amount = formattedAmount
                       currency = e.Currency
                       category = e.Category
                       description = e.Description
                       date = e.Date.ToString("yyyy-MM-dd")
                       ``type`` = e.Type.ToLowerInvariant()
                       quantity = qtyOpt
                       unit = if e.Unit = null then None else Some e.Unit |})
                |> List.toArray

            return
                ok
                    {| content = data
                       totalElements = total
                       page = p |}
    }

let getExpenseById
    (userRepo: UserRepository)
    (tokenRepo: TokenRepository)
    (expenseRepo: ExpenseRepository)
    (token: string option)
    (expenseId: Guid)
    : Async<int * string> =
    async {
        let! authResult = resolveAuth userRepo tokenRepo token

        match authResult with
        | Error e -> return e
        | Ok userId ->
            let! expenseOpt = expenseRepo.FindById expenseId |> Async.AwaitTask

            match expenseOpt with
            | None -> return notFound "Expense not found"
            | Some expense when expense.UserId <> userId -> return forbidden "Access denied"
            | Some expense ->
                let formattedAmount =
                    match expense.Currency with
                    | "IDR" -> expense.Amount.ToString("0")
                    | _ -> expense.Amount.ToString("0.00")

                let qtyOpt =
                    if expense.Quantity.HasValue then
                        Some(float expense.Quantity.Value)
                    else
                        None

                return
                    ok
                        {| id = expense.Id
                           amount = formattedAmount
                           currency = expense.Currency
                           category = expense.Category
                           description = expense.Description
                           date = expense.Date.ToString("yyyy-MM-dd")
                           ``type`` = expense.Type.ToLowerInvariant()
                           quantity = qtyOpt
                           unit = if expense.Unit = null then None else Some expense.Unit |}
    }

let updateExpense
    (userRepo: UserRepository)
    (tokenRepo: TokenRepository)
    (expenseRepo: ExpenseRepository)
    (token: string option)
    (expenseId: Guid)
    (amount: string)
    (currency: string)
    (category: string)
    (description: string)
    (date: string)
    (entryType: string)
    : Async<int * string> =
    async {
        let! authResult = resolveAuth userRepo tokenRepo token

        match authResult with
        | Error e -> return e
        | Ok userId ->
            let! expenseOpt = expenseRepo.FindById expenseId |> Async.AwaitTask

            match expenseOpt with
            | None -> return notFound "Expense not found"
            | Some expense when expense.UserId <> userId -> return forbidden "Access denied"
            | Some expense ->
                let amountResult = parseAmount amount

                match amountResult with
                | Error(ValidationError(f, m)) -> return validationError f m
                | Error _ -> return validationError "amount" "Invalid amount"
                | Ok amt ->
                    let dateVal =
                        match DateTime.TryParse(date) with
                        | true, d -> DateTime.SpecifyKind(d, DateTimeKind.Utc)
                        | _ -> expense.Date

                    let updated =
                        { expense with
                            Amount = amt
                            Currency =
                                if currency <> null then
                                    currency.ToUpperInvariant()
                                else
                                    expense.Currency
                            Category = if category <> null then category else expense.Category
                            Description =
                                if description <> null then
                                    description
                                else
                                    expense.Description
                            Date = dateVal
                            Type =
                                if entryType <> null then
                                    entryType.ToUpperInvariant()
                                else
                                    expense.Type
                            UpdatedAt = DateTime.UtcNow }

                    let! saved = expenseRepo.Update updated |> Async.AwaitTask

                    let formattedAmount =
                        match saved.Currency with
                        | "IDR" -> saved.Amount.ToString("0")
                        | _ -> saved.Amount.ToString("0.00")

                    return
                        ok
                            {| id = saved.Id
                               amount = formattedAmount
                               currency = saved.Currency
                               category = saved.Category
                               description = saved.Description
                               date = saved.Date.ToString("yyyy-MM-dd")
                               ``type`` = saved.Type.ToLowerInvariant() |}
    }

let deleteExpense
    (userRepo: UserRepository)
    (tokenRepo: TokenRepository)
    (expenseRepo: ExpenseRepository)
    (token: string option)
    (expenseId: Guid)
    : Async<int * string> =
    async {
        let! authResult = resolveAuth userRepo tokenRepo token

        match authResult with
        | Error e -> return e
        | Ok userId ->
            let! expenseOpt = expenseRepo.FindById expenseId |> Async.AwaitTask

            match expenseOpt with
            | None -> return notFound "Expense not found"
            | Some expense when expense.UserId <> userId -> return forbidden "Access denied"
            | Some expense ->
                do! expenseRepo.Delete expense |> Async.AwaitTask
                return noContent ()
    }

let expenseSummary
    (userRepo: UserRepository)
    (tokenRepo: TokenRepository)
    (expenseRepo: ExpenseRepository)
    (token: string option)
    : Async<int * string> =
    async {
        let! authResult = resolveAuth userRepo tokenRepo token

        match authResult with
        | Error e -> return e
        | Ok userId ->
            let! expenses = expenseRepo.ListSummaryByUser userId |> Async.AwaitTask

            let grouped =
                expenses
                |> List.groupBy (fun e -> e.Currency)
                |> List.map (fun (currency, items) ->
                    let total = items |> List.sumBy (fun e -> e.Amount)

                    let formattedTotal =
                        match currency with
                        | "IDR" -> total.ToString("0")
                        | _ -> total.ToString("0.00")

                    currency, formattedTotal)
                |> Map.ofList

            return ok grouped
    }

// ─────────────────────────────────────────────────────────────────────────────
// Attachments
// ─────────────────────────────────────────────────────────────────────────────

let uploadAttachment
    (userRepo: UserRepository)
    (tokenRepo: TokenRepository)
    (expenseRepo: ExpenseRepository)
    (attachmentRepo: AttachmentRepository)
    (token: string option)
    (expenseId: Guid)
    (filename: string)
    (contentType: string)
    (data: byte[])
    : Async<int * string> =
    async {
        let! authResult = resolveAuth userRepo tokenRepo token

        match authResult with
        | Error e -> return e
        | Ok userId ->
            let! expenseOpt = expenseRepo.FindById expenseId |> Async.AwaitTask

            match expenseOpt with
            | None -> return notFound "Expense not found"
            | Some expense when expense.UserId <> userId -> return forbidden "Access denied"
            | Some _ ->
                match validateContentType contentType with
                | Error(UnsupportedMediaType m) -> return unsupportedMediaType "file" m
                | Error _ -> return unsupportedMediaType "file" "Unsupported content type"
                | Ok _ ->
                    match validateFileSize (int64 data.Length) with
                    | Error(FileTooLarge limit) -> return fileTooLarge limit
                    | Error _ -> return fileTooLarge maxFileSize
                    | Ok _ ->
                        let attachmentId = Guid.NewGuid()
                        let now = DateTime.UtcNow
                        let url = $"/api/v1/expenses/{expenseId}/attachments/{attachmentId}/file"

                        let entity: AttachmentEntity =
                            { Id = attachmentId
                              ExpenseId = expenseId
                              Filename = filename
                              ContentType = contentType
                              Size = int64 data.Length
                              Data = data
                              CreatedAt = now }

                        let! _ = attachmentRepo.Create entity |> Async.AwaitTask

                        return
                            created
                                {| id = attachmentId
                                   filename = entity.Filename
                                   contentType = entity.ContentType
                                   size = entity.Size
                                   url = url |}
    }

let listAttachments
    (userRepo: UserRepository)
    (tokenRepo: TokenRepository)
    (expenseRepo: ExpenseRepository)
    (attachmentRepo: AttachmentRepository)
    (token: string option)
    (expenseId: Guid)
    : Async<int * string> =
    async {
        let! authResult = resolveAuth userRepo tokenRepo token

        match authResult with
        | Error e -> return e
        | Ok userId ->
            let! expenseOpt = expenseRepo.FindById expenseId |> Async.AwaitTask

            match expenseOpt with
            | None -> return notFound "Expense not found"
            | Some expense when expense.UserId <> userId -> return forbidden "Access denied"
            | Some _ ->
                let! attachments = attachmentRepo.ListByExpense expenseId |> Async.AwaitTask

                let data =
                    attachments
                    |> List.map (fun a ->
                        {| id = a.Id
                           filename = a.Filename
                           contentType = a.ContentType
                           size = a.Size
                           url = sprintf "/api/v1/expenses/%O/attachments/%O/download" expenseId a.Id |})
                    |> List.toArray

                return ok {| attachments = data |}
    }

let deleteAttachment
    (userRepo: UserRepository)
    (tokenRepo: TokenRepository)
    (expenseRepo: ExpenseRepository)
    (attachmentRepo: AttachmentRepository)
    (token: string option)
    (expenseId: Guid)
    (attachmentId: Guid)
    : Async<int * string> =
    async {
        let! authResult = resolveAuth userRepo tokenRepo token

        match authResult with
        | Error e -> return e
        | Ok userId ->
            let! expenseOpt = expenseRepo.FindById expenseId |> Async.AwaitTask

            match expenseOpt with
            | None -> return notFound "Expense not found"
            | Some expense when expense.UserId <> userId -> return forbidden "Access denied"
            | Some _ ->
                let! attachmentOpt = attachmentRepo.FindById attachmentId expenseId |> Async.AwaitTask

                match attachmentOpt with
                | None -> return notFound "Attachment not found"
                | Some attachment ->
                    do! attachmentRepo.Delete attachment |> Async.AwaitTask
                    return noContent ()
    }

// ─────────────────────────────────────────────────────────────────────────────
// Reports
// ─────────────────────────────────────────────────────────────────────────────

let profitAndLoss
    (userRepo: UserRepository)
    (tokenRepo: TokenRepository)
    (expenseRepo: ExpenseRepository)
    (token: string option)
    (fromDate: string)
    (toDate: string)
    (currency: string)
    : Async<int * string> =
    async {
        let! authResult = resolveAuth userRepo tokenRepo token

        match authResult with
        | Error e -> return e
        | Ok userId ->
            let from =
                match DateTime.TryParse(fromDate) with
                | true, d -> DateTime.SpecifyKind(d, DateTimeKind.Utc)
                | _ -> DateTime.SpecifyKind(DateTime.MinValue, DateTimeKind.Utc)

            let ``to`` =
                match DateTime.TryParse(toDate) with
                | true, d -> DateTime.SpecifyKind(d.AddDays(1.0).AddSeconds(-1.0), DateTimeKind.Utc)
                | _ -> DateTime.SpecifyKind(DateTime.MaxValue, DateTimeKind.Utc)

            let curr = currency.ToUpperInvariant()

            let! entries = expenseRepo.ListByUserAndFilter userId curr from ``to`` |> Async.AwaitTask

            let incomeEntries = entries |> List.filter (fun e -> e.Type = "INCOME")
            let expenseEntries = entries |> List.filter (fun e -> e.Type = "EXPENSE")

            let incomeTotal = incomeEntries |> List.sumBy (fun e -> e.Amount)
            let expenseTotal = expenseEntries |> List.sumBy (fun e -> e.Amount)
            let net = incomeTotal - expenseTotal

            let formatAmount (a: decimal) =
                match curr with
                | "IDR" -> a.ToString("0")
                | _ -> a.ToString("0.00")

            let incomeBreakdown =
                incomeEntries
                |> List.groupBy (fun e -> e.Category)
                |> List.map (fun (cat, items) ->
                    {| category = cat
                       ``type`` = "income"
                       total = formatAmount (items |> List.sumBy (fun e -> e.Amount)) |})
                |> List.toArray

            let expenseBreakdown =
                expenseEntries
                |> List.groupBy (fun e -> e.Category)
                |> List.map (fun (cat, items) ->
                    {| category = cat
                       ``type`` = "expense"
                       total = formatAmount (items |> List.sumBy (fun e -> e.Amount)) |})
                |> List.toArray

            return
                ok
                    {| totalIncome = formatAmount incomeTotal
                       totalExpense = formatAmount expenseTotal
                       net = formatAmount net
                       currency = curr
                       incomeBreakdown = incomeBreakdown
                       expenseBreakdown = expenseBreakdown |}
    }

// ─────────────────────────────────────────────────────────────────────────────
// Tokens
// ─────────────────────────────────────────────────────────────────────────────

let getTokenClaims (token: string option) : int * string =
    let tokenStr = token |> Option.defaultValue ""
    let handler = System.IdentityModel.Tokens.Jwt.JwtSecurityTokenHandler()

    let claimsData =
        try
            let jwt = handler.ReadJwtToken(tokenStr)
            jwt.Claims |> Seq.map (fun c -> c.Type, c.Value) |> Map.ofSeq |> Some
        with _ ->
            None

    match claimsData with
    | None -> badRequest "Cannot decode token"
    | Some claimsMap -> ok claimsMap

let getJwks () : int * string =
    ok (DemoBeFsgi.Auth.JwtService.getJwks ())
