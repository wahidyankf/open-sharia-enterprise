module DemoBeFsgi.Tests.Unit.HandlerCoverageTests

open System
open System.IdentityModel.Tokens.Jwt
open System.Security.Claims
open System.Text
open System.Text.Json
open Microsoft.IdentityModel.Tokens
open Xunit
open DemoBeFsgi.Domain.Types
open DemoBeFsgi.Domain.Expense
open DemoBeFsgi.Tests.TestFixture
open DemoBeFsgi.Tests.DirectServices

// ─────────────────────────────────────────────────────────────────────────────
// Pure function branch coverage
// ─────────────────────────────────────────────────────────────────────────────

[<Trait("Category", "Unit")>]
type TypesModuleTests() =

    [<Fact>]
    member _.``currencyToString returns USD``() =
        Assert.Equal("USD", currencyToString USD)

    [<Fact>]
    member _.``currencyToString returns IDR``() =
        Assert.Equal("IDR", currencyToString IDR)

    [<Fact>]
    member _.``roleToString returns USER``() = Assert.Equal("USER", roleToString User)

    [<Fact>]
    member _.``roleToString returns ADMIN``() =
        Assert.Equal("ADMIN", roleToString Admin)

    [<Fact>]
    member _.``statusToString returns ACTIVE``() =
        Assert.Equal("ACTIVE", statusToString Active)

    [<Fact>]
    member _.``statusToString returns INACTIVE``() =
        Assert.Equal("INACTIVE", statusToString Inactive)

    [<Fact>]
    member _.``statusToString returns DISABLED``() =
        Assert.Equal("DISABLED", statusToString Disabled)

    [<Fact>]
    member _.``statusToString returns LOCKED``() =
        Assert.Equal("LOCKED", statusToString Locked)

    [<Fact>]
    member _.``parseStatus returns Active for ACTIVE``() =
        Assert.Equal(Some Active, parseStatus "ACTIVE")

    [<Fact>]
    member _.``parseStatus returns Inactive for INACTIVE``() =
        Assert.Equal(Some Inactive, parseStatus "INACTIVE")

    [<Fact>]
    member _.``parseStatus returns Disabled for DISABLED``() =
        Assert.Equal(Some Disabled, parseStatus "DISABLED")

    [<Fact>]
    member _.``parseStatus returns Locked for LOCKED``() =
        Assert.Equal(Some Locked, parseStatus "LOCKED")

    [<Fact>]
    member _.``parseStatus returns None for unknown``() =
        Assert.Equal(None, parseStatus "unknown")

    [<Fact>]
    member _.``parseCurrency case insensitive lowercase``() =
        Assert.Equal(Ok USD, parseCurrency "usd")

    [<Fact>]
    member _.``parseCurrency case insensitive mixed``() =
        Assert.Equal(Ok IDR, parseCurrency "Idr")

    [<Fact>]
    member _.``supportedUnits contains expected units``() =
        Assert.True(supportedUnits.Contains("liter"))
        Assert.True(supportedUnits.Contains("kg"))
        Assert.True(supportedUnits.Contains("hour"))

[<Trait("Category", "Unit")>]
type ExpenseModuleTests() =

    [<Fact>]
    member _.``validateCurrencyPrecision USD too many decimals fails``() =
        let result = validateCurrencyPrecision "USD" 10.123m
        Assert.True(Result.isError result)

    [<Fact>]
    member _.``validateCurrencyPrecision IDR with decimals fails``() =
        let result = validateCurrencyPrecision "IDR" 100.5m
        Assert.True(Result.isError result)

    [<Fact>]
    member _.``validateCurrencyPrecision unknown currency fails``() =
        let result = validateCurrencyPrecision "EUR" 10.00m
        Assert.True(Result.isError result)

    [<Fact>]
    member _.``validateCurrencyPrecision USD valid passes``() =
        let result = validateCurrencyPrecision "USD" 10.00m
        Assert.True(Result.isOk result)

    [<Fact>]
    member _.``validateCurrencyPrecision IDR valid passes``() =
        let result = validateCurrencyPrecision "IDR" 100000m
        Assert.True(Result.isOk result)

    [<Fact>]
    member _.``validateUnit empty string passes``() =
        Assert.True(Result.isOk (validateUnit (Some "")))

    [<Fact>]
    member _.``validateUnit uppercase unit normalizes``() =
        let result = validateUnit (Some "LITER")
        Assert.Equal(Ok(Some "liter"), result)

// ─────────────────────────────────────────────────────────────────────────────
// JwtService branch coverage
// ─────────────────────────────────────────────────────────────────────────────

[<Trait("Category", "Unit")>]
type JwtServiceTests() =

    [<Fact>]
    member _.``validateToken returns None for invalid token``() =
        let result = DemoBeFsgi.Auth.JwtService.validateToken "not-a-valid-token"
        Assert.Equal(None, result)

    [<Fact>]
    member _.``getTokenJti returns None for invalid token``() =
        let result = DemoBeFsgi.Auth.JwtService.getTokenJti "not-a-valid-token"
        Assert.Equal(None, result)

    [<Fact>]
    member _.``getTokenExpiry returns None for invalid token``() =
        let result = DemoBeFsgi.Auth.JwtService.getTokenExpiry "not-a-valid-token"
        Assert.Equal(None, result)

    [<Fact>]
    member _.``generateAccessToken produces a parseable JWT``() =
        let userId = Guid.NewGuid()

        let token =
            DemoBeFsgi.Auth.JwtService.generateAccessToken userId "alice" "alice@example.com" "user"

        Assert.False(String.IsNullOrEmpty(token))
        let jti = DemoBeFsgi.Auth.JwtService.getTokenJti token
        Assert.True(jti.IsSome)

    [<Fact>]
    member _.``generateRefreshToken produces a parseable JWT``() =
        let userId = Guid.NewGuid()
        let token = DemoBeFsgi.Auth.JwtService.generateRefreshToken userId
        Assert.False(String.IsNullOrEmpty(token))
        let expiry = DemoBeFsgi.Auth.JwtService.getTokenExpiry token
        Assert.True(expiry.IsSome)

    [<Fact>]
    member _.``getJwks returns keys array``() =
        let jwks = DemoBeFsgi.Auth.JwtService.getJwks ()
        let json = JsonSerializer.Serialize(jwks)
        Assert.Contains("keys", json)

    [<Fact>]
    member _.``generateAccessToken uses environment variable when set``() =
        let original = Environment.GetEnvironmentVariable("APP_JWT_SECRET")

        try
            Environment.SetEnvironmentVariable("APP_JWT_SECRET", "test-secret-long-enough-for-hmac-sha256-minimum")

            let userId = Guid.NewGuid()

            let token =
                DemoBeFsgi.Auth.JwtService.generateAccessToken userId "bob" "bob@example.com" "user"

            Assert.False(String.IsNullOrEmpty(token))
        finally
            if original = null then
                Environment.SetEnvironmentVariable("APP_JWT_SECRET", null)
            else
                Environment.SetEnvironmentVariable("APP_JWT_SECRET", original)

// ─────────────────────────────────────────────────────────────────────────────
// Handler coverage via direct service calls
// ─────────────────────────────────────────────────────────────────────────────

let private jwtSecret = "dev-jwt-secret-at-least-32-characters-long-for-hmac"

let private makeCustomToken (claimsArr: Claim array) (includeJti: bool) =
    let key = SymmetricSecurityKey(Encoding.UTF8.GetBytes(jwtSecret))
    let signingCreds = SigningCredentials(key, SecurityAlgorithms.HmacSha256)
    let now = DateTime.UtcNow

    let allClaims =
        if includeJti then
            Array.append claimsArr [| Claim(JwtRegisteredClaimNames.Jti, Guid.NewGuid().ToString()) |]
        else
            claimsArr

    let token =
        JwtSecurityToken(
            issuer = "demo-be-fsharp-giraffe",
            audience = "demo-be-fsharp-giraffe",
            claims = allClaims,
            notBefore = now,
            expires = now.AddMinutes(15.0),
            signingCredentials = signingCreds
        )

    JwtSecurityTokenHandler().WriteToken(token)

let private shortId () =
    let raw = Guid.NewGuid().ToString("N")
    raw.Substring(0, 8)

let private registerAndLogin (db: DemoBeFsgi.Infrastructure.AppDbContext.AppDbContext) (username: string) =
    let email = $"{username}@example.com"
    register db username email "Str0ng#Pass1!" |> Async.RunSynchronously |> ignore
    let status, body = login db username "Str0ng#Pass1!" |> Async.RunSynchronously

    if status = 200 then
        let doc = JsonDocument.Parse(body)
        doc.RootElement.GetProperty("access_token").GetString()
    else
        failwith $"Login failed for {username}: {status} {body}"

let private createExpenseForUser (db: DemoBeFsgi.Infrastructure.AppDbContext.AppDbContext) (token: string) =
    let status, body =
        createExpense db (Some token) "10.00" "USD" "food" "test" "2024-01-01" "expense" None None
        |> Async.RunSynchronously

    if status = 201 then
        let doc = JsonDocument.Parse(body)
        doc.RootElement.GetProperty("id").GetString()
    else
        failwith $"Create expense failed: {status} {body}"

// ─────────────────────────────────────────────────────────────────────────────
// Program handler coverage
// ─────────────────────────────────────────────────────────────────────────────

[<Trait("Category", "Unit")>]
type ProgramHandlerCoverageTests() =

    [<Fact>]
    member _.``setAdminRole with nonexistent user returns 404``() =
        let db, cleanup = createDb ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let fakeUsername = $"nobody_{shortId ()}"
        let status, _ = setAdminRole db fakeUsername |> Async.RunSynchronously
        Assert.Equal(404, status)

// ─────────────────────────────────────────────────────────────────────────────
// Auth handler
// ─────────────────────────────────────────────────────────────────────────────

[<Trait("Category", "Unit")>]
type AuthHandlerCoverageTests() =

    [<Fact>]
    member _.``register with empty username returns 400``() =
        let db, cleanup = createDb ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let status, _ = register db "" "a@example.com" "Str0ng#Pass1!" |> Async.RunSynchronously
        Assert.Equal(400, status)

    [<Fact>]
    member _.``register with empty email returns 400``() =
        let db, cleanup = createDb ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let status, _ = register db "alice" "" "Str0ng#Pass1!" |> Async.RunSynchronously
        Assert.Equal(400, status)

    [<Fact>]
    member _.``register with empty password returns 400``() =
        let db, cleanup = createDb ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let status, _ = register db "alice" "a@example.com" "" |> Async.RunSynchronously
        Assert.Equal(400, status)

    [<Fact>]
    member _.``register with null username returns 400``() =
        let db, cleanup = createDb ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let status, _ = register db null "a@example.com" "Str0ng#Pass1!" |> Async.RunSynchronously
        Assert.Equal(400, status)

    [<Fact>]
    member _.``login with inactive account returns 401``() =
        let db, cleanup = createDb ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let username = $"ina_{shortId ()}"
        let token = registerAndLogin db username
        deactivate db (Some token) |> Async.RunSynchronously |> ignore
        let status, _ = login db username "Str0ng#Pass1!" |> Async.RunSynchronously
        Assert.Equal(401, status)

    [<Fact>]
    member _.``login with disabled account returns 401``() =
        let db, cleanup = createDb ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let username = $"dis_{shortId ()}"

        let email = $"{username}@example.com"
        let _s, regBody = register db username email "Str0ng#Pass1!" |> Async.RunSynchronously
        let userId = JsonDocument.Parse(regBody).RootElement.GetProperty("id").GetString()

        let adminName = $"adm_{shortId ()}"
        let adminEmail = $"{adminName}@example.com"
        register db adminName adminEmail "Str0ng#Pass1!" |> Async.RunSynchronously |> ignore
        setAdminRole db adminName |> Async.RunSynchronously |> ignore
        let adminToken, _ = Some(registerAndLogin db adminName), None
        disableUser db adminToken (Guid.Parse(userId)) |> Async.RunSynchronously |> ignore

        let status, _ = login db username "Str0ng#Pass1!" |> Async.RunSynchronously
        Assert.Equal(401, status)

    [<Fact>]
    member _.``login account gets locked after 5 failed attempts``() =
        let db, cleanup = createDb ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let username = $"lck_{shortId ()}"
        let email = $"{username}@example.com"
        register db username email "Str0ng#Pass1!" |> Async.RunSynchronously |> ignore

        for _ in 1..4 do
            let status, _ = login db username "WrongPass1!" |> Async.RunSynchronously
            Assert.Equal(401, status)

        let status, _ = login db username "WrongPass1!" |> Async.RunSynchronously
        Assert.Equal(401, status)

    [<Fact>]
    member _.``refresh with invalid token returns 401``() =
        let db, cleanup = createDb ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let status, _ = refresh db "nonexistent" |> Async.RunSynchronously
        Assert.Equal(401, status)

// ─────────────────────────────────────────────────────────────────────────────
// Token handler
// ─────────────────────────────────────────────────────────────────────────────

[<Trait("Category", "Unit")>]
type TokenHandlerCoverageTests() =

    [<Fact>]
    member _.``claims endpoint with valid token returns 200``() =
        let db, cleanup = createDb ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let username = $"tok_{shortId ()}"
        let token = registerAndLogin db username
        let status, body = getTokenClaims (Some token)
        Assert.Equal(200, status)
        Assert.Contains("sub", body)

    [<Fact>]
    member _.``claims endpoint without token returns 400``() =
        let status, _ = getTokenClaims None
        // No token → cannot decode → 400
        Assert.Equal(400, status)

    [<Fact>]
    member _.``jwks endpoint returns keys``() =
        let status, body = getJwks ()
        Assert.Equal(200, status)
        Assert.Contains("keys", body)

// ─────────────────────────────────────────────────────────────────────────────
// User handler
// ─────────────────────────────────────────────────────────────────────────────

[<Trait("Category", "Unit")>]
type UserHandlerCoverageTests() =

    [<Fact>]
    member _.``changePassword with wrong old password returns 401``() =
        let db, cleanup = createDb ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let username = $"pwd_{shortId ()}"
        let token = registerAndLogin db username
        let status, _ = changePassword db (Some token) "WrongPass1!" "NewStr0ng#Pass1!" |> Async.RunSynchronously
        Assert.Equal(401, status)

    [<Fact>]
    member _.``updateProfile with null display name uses existing``() =
        let db, cleanup = createDb ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let username = $"prf_{shortId ()}"
        let token = registerAndLogin db username
        let status, _ = updateProfile db (Some token) null |> Async.RunSynchronously
        Assert.Equal(200, status)

// ─────────────────────────────────────────────────────────────────────────────
// Admin handler
// ─────────────────────────────────────────────────────────────────────────────

[<Trait("Category", "Unit")>]
type AdminHandlerCoverageTests() =

    let createAdminContext () =
        let db, cleanup = createDb ()
        let adminName = $"adm_{shortId ()}"
        let adminEmail = $"{adminName}@example.com"
        register db adminName adminEmail "Str0ng#Pass1!" |> Async.RunSynchronously |> ignore
        setAdminRole db adminName |> Async.RunSynchronously |> ignore
        let adminToken = registerAndLogin db adminName
        db, cleanup, adminToken

    [<Fact>]
    member _.``disableUser with nonexistent user returns 404``() =
        let db, cleanup, adminToken = createAdminContext ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let fakeId = Guid.NewGuid()
        let status, _ = disableUser db (Some adminToken) fakeId |> Async.RunSynchronously
        Assert.Equal(404, status)

    [<Fact>]
    member _.``enableUser with nonexistent user returns 404``() =
        let db, cleanup, adminToken = createAdminContext ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let fakeId = Guid.NewGuid()
        let status, _ = enableUser db (Some adminToken) fakeId |> Async.RunSynchronously
        Assert.Equal(404, status)

    [<Fact>]
    member _.``unlockUser with nonexistent user returns 404``() =
        let db, cleanup, adminToken = createAdminContext ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let fakeId = Guid.NewGuid()
        let status, _ = unlockUser db (Some adminToken) fakeId |> Async.RunSynchronously
        Assert.Equal(404, status)

    [<Fact>]
    member _.``forcePasswordReset with nonexistent user returns 404``() =
        let db, cleanup, adminToken = createAdminContext ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let fakeId = Guid.NewGuid()
        let status, _ = forcePasswordReset db (Some adminToken) fakeId |> Async.RunSynchronously
        Assert.Equal(404, status)

    [<Fact>]
    member _.``listUsers with email filter returns filtered results``() =
        let db, cleanup, adminToken = createAdminContext ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let status, _ = listUsers db (Some adminToken) 1 20 (Some "notexists@example.com") |> Async.RunSynchronously
        Assert.Equal(200, status)

    [<Fact>]
    member _.``non-admin user gets 403 on admin endpoint``() =
        let db, cleanup = createDb ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let username = $"nonadm_{shortId ()}"
        let token = registerAndLogin db username
        let status, _ = listUsers db (Some token) 1 20 None |> Async.RunSynchronously
        Assert.Equal(403, status)

// ─────────────────────────────────────────────────────────────────────────────
// Expense handler
// ─────────────────────────────────────────────────────────────────────────────

[<Trait("Category", "Unit")>]
type ExpenseHandlerCoverageTests() =

    let setupUser () =
        let db, cleanup = createDb ()
        let username = $"exp_{shortId ()}"
        let token = registerAndLogin db username
        db, cleanup, token

    [<Fact>]
    member _.``create expense with invalid currency returns 400``() =
        let db, cleanup, token = setupUser ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }

        let status, _ =
            createExpense db (Some token) "10.00" "EUR" "food" "test" "2024-01-01" "expense" None None
            |> Async.RunSynchronously

        Assert.Equal(400, status)

    [<Fact>]
    member _.``create expense with empty amount returns 400``() =
        let db, cleanup, token = setupUser ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }

        let status, _ =
            createExpense db (Some token) "" "USD" "food" "test" "2024-01-01" "expense" None None
            |> Async.RunSynchronously

        Assert.Equal(400, status)

    [<Fact>]
    member _.``create expense with invalid amount format returns 400``() =
        let db, cleanup, token = setupUser ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }

        let status, _ =
            createExpense db (Some token) "not-a-number" "USD" "food" "test" "2024-01-01" "expense" None None
            |> Async.RunSynchronously

        Assert.Equal(400, status)

    [<Fact>]
    member _.``create expense with negative amount returns 400``() =
        let db, cleanup, token = setupUser ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }

        let status, _ =
            createExpense db (Some token) "-5.00" "USD" "food" "test" "2024-01-01" "expense" None None
            |> Async.RunSynchronously

        Assert.Equal(400, status)

    [<Fact>]
    member _.``create expense with invalid unit returns 400``() =
        let db, cleanup, token = setupUser ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }

        let status, _ =
            createExpense
                db
                (Some token)
                "10.00"
                "USD"
                "food"
                "test"
                "2024-01-01"
                "expense"
                None
                (Some "fathom")
            |> Async.RunSynchronously

        Assert.Equal(400, status)

    [<Fact>]
    member _.``create expense with IDR currency formats correctly``() =
        let db, cleanup, token = setupUser ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }

        let status, respBody =
            createExpense db (Some token) "150000" "IDR" "food" "test" "2024-01-01" "expense" None None
            |> Async.RunSynchronously

        Assert.Equal(201, status)
        Assert.Contains("150000", respBody)

    [<Fact>]
    member _.``create expense with invalid date defaults to now``() =
        let db, cleanup, token = setupUser ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }

        let status, _ =
            createExpense db (Some token) "10.00" "USD" "food" "test" "not-a-date" "expense" None None
            |> Async.RunSynchronously

        Assert.Equal(201, status)

    [<Fact>]
    member _.``create expense with null category and description``() =
        let db, cleanup, token = setupUser ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }

        let status, _ =
            createExpense db (Some token) "10.00" "USD" null null "2024-01-01" null None None
            |> Async.RunSynchronously

        Assert.Equal(201, status)

    [<Fact>]
    member _.``create expense with quantity and unit``() =
        let db, cleanup, token = setupUser ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }

        let status, _ =
            createExpense db (Some token) "10.00" "USD" "food" "test" "2024-01-01" "expense" (Some 2.5) (Some "kg")
            |> Async.RunSynchronously

        Assert.Equal(201, status)

    [<Fact>]
    member _.``getById returns 404 for nonexistent expense``() =
        let db, cleanup, token = setupUser ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let status, _ = getExpenseById db (Some token) (Guid.NewGuid()) |> Async.RunSynchronously
        Assert.Equal(404, status)

    [<Fact>]
    member _.``getById returns 403 when accessing another user's expense``() =
        let db, cleanup, token1 = setupUser ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let username2 = $"exp_{shortId ()}"
        let token2 = registerAndLogin db username2
        let expId = createExpenseForUser db token1
        let status, _ = getExpenseById db (Some token2) (Guid.Parse(expId)) |> Async.RunSynchronously
        Assert.Equal(403, status)

    [<Fact>]
    member _.``getById with unit returns unit in response``() =
        let db, cleanup, token = setupUser ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }

        let _s, body =
            createExpense db (Some token) "10.00" "USD" "food" "test" "2024-01-01" "expense" (Some 2.0) (Some "kg")
            |> Async.RunSynchronously

        let expId = Guid.Parse(JsonDocument.Parse(body).RootElement.GetProperty("id").GetString())
        let status, respBody = getExpenseById db (Some token) expId |> Async.RunSynchronously
        Assert.Equal(200, status)
        Assert.Contains("kg", respBody)

    [<Fact>]
    member _.``getById with IDR currency formats correctly``() =
        let db, cleanup, token = setupUser ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }

        let _s, body =
            createExpense db (Some token) "150000" "IDR" "food" "test" "2024-01-01" "expense" None None
            |> Async.RunSynchronously

        let expId = Guid.Parse(JsonDocument.Parse(body).RootElement.GetProperty("id").GetString())
        let status, respBody = getExpenseById db (Some token) expId |> Async.RunSynchronously
        Assert.Equal(200, status)
        Assert.Contains("150000", respBody)

    [<Fact>]
    member _.``update expense returns 404 for nonexistent``() =
        let db, cleanup, token = setupUser ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }

        let status, _ =
            updateExpense db (Some token) (Guid.NewGuid()) "20.00" "USD" "food" "updated" "2024-01-01" "expense"
            |> Async.RunSynchronously

        Assert.Equal(404, status)

    [<Fact>]
    member _.``update expense returns 403 for another user's expense``() =
        let db, cleanup, token1 = setupUser ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let username2 = $"exp_{shortId ()}"
        let token2 = registerAndLogin db username2
        let expId = createExpenseForUser db token1

        let status, _ =
            updateExpense
                db
                (Some token2)
                (Guid.Parse(expId))
                "20.00"
                "USD"
                "food"
                "test"
                "2024-01-01"
                "expense"
            |> Async.RunSynchronously

        Assert.Equal(403, status)

    [<Fact>]
    member _.``update expense with invalid amount returns 400``() =
        let db, cleanup, token = setupUser ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let expId = createExpenseForUser db token

        let status, _ =
            updateExpense db (Some token) (Guid.Parse(expId)) "bad" "USD" "food" "test" "2024-01-01" "expense"
            |> Async.RunSynchronously

        Assert.Equal(400, status)

    [<Fact>]
    member _.``update expense with IDR currency formats correctly``() =
        let db, cleanup, token = setupUser ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }

        let _s, body =
            createExpense db (Some token) "150000" "IDR" "food" "test" "2024-01-01" "expense" None None
            |> Async.RunSynchronously

        let expId = Guid.Parse(JsonDocument.Parse(body).RootElement.GetProperty("id").GetString())

        let status, respBody =
            updateExpense db (Some token) expId "200000" "IDR" "food" "updated" "2024-01-02" "expense"
            |> Async.RunSynchronously

        Assert.Equal(200, status)
        Assert.Contains("200000", respBody)

    [<Fact>]
    member _.``update expense with null optional fields uses existing values``() =
        let db, cleanup, token = setupUser ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let expId = createExpenseForUser db token

        let status, _ =
            updateExpense db (Some token) (Guid.Parse(expId)) "15.00" null null null "not-a-date" null
            |> Async.RunSynchronously

        Assert.Equal(200, status)

    [<Fact>]
    member _.``delete expense returns 404 for nonexistent``() =
        let db, cleanup, token = setupUser ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let status, _ = deleteExpense db (Some token) (Guid.NewGuid()) |> Async.RunSynchronously
        Assert.Equal(404, status)

    [<Fact>]
    member _.``delete expense returns 403 for another user's expense``() =
        let db, cleanup, token1 = setupUser ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let username2 = $"exp_{shortId ()}"
        let token2 = registerAndLogin db username2
        let expId = createExpenseForUser db token1
        let status, _ = deleteExpense db (Some token2) (Guid.Parse(expId)) |> Async.RunSynchronously
        Assert.Equal(403, status)

    [<Fact>]
    member _.``list expenses with IDR currency formats correctly``() =
        let db, cleanup, token = setupUser ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        createExpense db (Some token) "150000" "IDR" "food" "test" "2024-01-01" "expense" None None |> Async.RunSynchronously |> ignore
        let status, respBody = listExpenses db (Some token) 1 20 |> Async.RunSynchronously
        Assert.Equal(200, status)
        Assert.Contains("150000", respBody)

    [<Fact>]
    member _.``list expenses with quantity``() =
        let db, cleanup, token = setupUser ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        createExpense db (Some token) "10.00" "USD" "food" "test" "2024-01-01" "expense" (Some 1.5) (Some "kg") |> Async.RunSynchronously |> ignore
        let status, respBody = listExpenses db (Some token) 1 20 |> Async.RunSynchronously
        Assert.Equal(200, status)
        Assert.Contains("1.5", respBody)

    [<Fact>]
    member _.``summary with IDR expenses formats correctly``() =
        let db, cleanup, token = setupUser ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        createExpense db (Some token) "150000" "IDR" "food" "test" "2024-01-01" "expense" None None |> Async.RunSynchronously |> ignore
        let status, respBody = expenseSummary db (Some token) |> Async.RunSynchronously
        Assert.Equal(200, status)
        Assert.Contains("IDR", respBody)

// ─────────────────────────────────────────────────────────────────────────────
// Attachment handler
// ─────────────────────────────────────────────────────────────────────────────

[<Trait("Category", "Unit")>]
type AttachmentHandlerCoverageTests() =

    let setupUser () =
        let db, cleanup = createDb ()
        let username = $"att_{shortId ()}"
        let token = registerAndLogin db username
        db, cleanup, token

    [<Fact>]
    member _.``upload attachment to nonexistent expense returns 404``() =
        let db, cleanup, token = setupUser ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let fakeId = Guid.NewGuid()
        let data = [| 0uy; 1uy; 2uy |]
        let status, _ = uploadAttachment db (Some token) fakeId "test.jpg" "image/jpeg" data |> Async.RunSynchronously
        Assert.Equal(404, status)

    [<Fact>]
    member _.``upload attachment to another user's expense returns 403``() =
        let db, cleanup, token1 = setupUser ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let username2 = $"att_{shortId ()}"
        let token2 = registerAndLogin db username2
        let expId = createExpenseForUser db token1
        let data = [| 0uy; 1uy; 2uy |]
        let status, _ = uploadAttachment db (Some token2) (Guid.Parse(expId)) "test.jpg" "image/jpeg" data |> Async.RunSynchronously
        Assert.Equal(403, status)

    [<Fact>]
    member _.``upload attachment with unsupported content type returns 415``() =
        let db, cleanup, token = setupUser ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let expId = createExpenseForUser db token
        let data = [| 0uy; 1uy; 2uy |]
        let status, _ = uploadAttachment db (Some token) (Guid.Parse(expId)) "test.bin" "application/octet-stream" data |> Async.RunSynchronously
        Assert.Equal(415, status)

    [<Fact>]
    member _.``list attachments for nonexistent expense returns 404``() =
        let db, cleanup, token = setupUser ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let status, _ = listAttachments db (Some token) (Guid.NewGuid()) |> Async.RunSynchronously
        Assert.Equal(404, status)

    [<Fact>]
    member _.``list attachments for another user's expense returns 403``() =
        let db, cleanup, token1 = setupUser ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let username2 = $"att_{shortId ()}"
        let token2 = registerAndLogin db username2
        let expId = createExpenseForUser db token1
        let status, _ = listAttachments db (Some token2) (Guid.Parse(expId)) |> Async.RunSynchronously
        Assert.Equal(403, status)

    [<Fact>]
    member _.``delete attachment for nonexistent expense returns 404``() =
        let db, cleanup, token = setupUser ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let fakeId = Guid.NewGuid()
        let fakeAttId = Guid.NewGuid()
        let status, _ = deleteAttachment db (Some token) fakeId fakeAttId |> Async.RunSynchronously
        Assert.Equal(404, status)

    [<Fact>]
    member _.``delete attachment for another user's expense returns 403``() =
        let db, cleanup, token1 = setupUser ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let username2 = $"att_{shortId ()}"
        let token2 = registerAndLogin db username2
        let expId = createExpenseForUser db token1
        let fakeAttId = Guid.NewGuid()
        let status, _ = deleteAttachment db (Some token2) (Guid.Parse(expId)) fakeAttId |> Async.RunSynchronously
        Assert.Equal(403, status)

    [<Fact>]
    member _.``delete nonexistent attachment on own expense returns 404``() =
        let db, cleanup, token = setupUser ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let expId = createExpenseForUser db token
        let fakeAttId = Guid.NewGuid()
        let status, _ = deleteAttachment db (Some token) (Guid.Parse(expId)) fakeAttId |> Async.RunSynchronously
        Assert.Equal(404, status)

// ─────────────────────────────────────────────────────────────────────────────
// Auth / JWT middleware coverage via resolveAuth
// ─────────────────────────────────────────────────────────────────────────────

[<Trait("Category", "Unit")>]
type JwtMiddlewareCoverageTests() =

    [<Fact>]
    member _.``request with no token returns 401``() =
        let db, cleanup = createDb ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let status, _ = getProfile db None |> Async.RunSynchronously
        Assert.Equal(401, status)

    [<Fact>]
    member _.``request with invalid JWT returns 401``() =
        let db, cleanup = createDb ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let status, _ = getProfile db (Some "invalid.jwt.token") |> Async.RunSynchronously
        Assert.Equal(401, status)

    [<Fact>]
    member _.``request with revoked token returns 401``() =
        let db, cleanup = createDb ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let username = $"rev_{shortId ()}"
        let token = registerAndLogin db username
        logout db (Some token) |> Async.RunSynchronously |> ignore
        let status, _ = getProfile db (Some token) |> Async.RunSynchronously
        Assert.Equal(401, status)

    [<Fact>]
    member _.``request with token having no jti treated as revoked returns 401``() =
        let db, cleanup = createDb ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let userId = Guid.NewGuid()
        let claimsArr = [| Claim(JwtRegisteredClaimNames.Sub, userId.ToString()) |]
        let token = makeCustomToken claimsArr false
        let status, _ = getProfile db (Some token) |> Async.RunSynchronously
        Assert.Equal(401, status)

    [<Fact>]
    member _.``request with token with non-guid sub returns 401``() =
        let db, cleanup = createDb ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let claimsArr = [| Claim(JwtRegisteredClaimNames.Sub, "not-a-guid") |]
        let token = makeCustomToken claimsArr true
        let status, _ = getProfile db (Some token) |> Async.RunSynchronously
        Assert.Equal(401, status)

    [<Fact>]
    member _.``request with valid guid sub but non-existent user returns 401``() =
        let db, cleanup = createDb ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let nonExistentGuid = Guid.NewGuid()

        let claimsArr =
            [| Claim(
                   "http://schemas.xmlsoap.org/ws/2005/05/identity/claims/nameidentifier",
                   nonExistentGuid.ToString()
               ) |]

        let token = makeCustomToken claimsArr true
        let status, _ = getProfile db (Some token) |> Async.RunSynchronously
        Assert.Equal(401, status)

    [<Fact>]
    member _.``request with deactivated user account returns 401``() =
        let db, cleanup = createDb ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let username = $"dact_{shortId ()}"
        let token = registerAndLogin db username
        deactivate db (Some token) |> Async.RunSynchronously |> ignore
        let status, body = getProfile db (Some token) |> Async.RunSynchronously
        Assert.Equal(401, status)
        Assert.Contains("deactivated", body)

    [<Fact>]
    member _.``request with locked user account returns 401 with locked message``() =
        let db, cleanup = createDb ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let username = $"lkm_{shortId ()}"
        let email = $"{username}@example.com"
        register db username email "Str0ng#Pass1!" |> Async.RunSynchronously |> ignore
        let _s, loginResp = login db username "Str0ng#Pass1!" |> Async.RunSynchronously
        let doc = JsonDocument.Parse(loginResp)
        let token = doc.RootElement.GetProperty("access_token").GetString()

        for _ in 1..5 do
            login db username "WrongPass1!" |> Async.RunSynchronously |> ignore

        let status, body = getProfile db (Some token) |> Async.RunSynchronously
        Assert.Equal(401, status)
        Assert.Contains("locked", body)

// ─────────────────────────────────────────────────────────────────────────────
// Report handler
// ─────────────────────────────────────────────────────────────────────────────

[<Trait("Category", "Unit")>]
type ReportHandlerCoverageTests() =

    let setupUser () =
        let db, cleanup = createDb ()
        let username = $"rpt_{shortId ()}"
        let token = registerAndLogin db username
        db, cleanup, token

    [<Fact>]
    member _.``profit and loss with IDR currency returns formatted results``() =
        let db, cleanup, token = setupUser ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        createExpense db (Some token) "500000" "IDR" "salary" "income" "2024-01-15" "income" None None |> Async.RunSynchronously |> ignore
        createExpense db (Some token) "100000" "IDR" "food" "expense" "2024-01-15" "expense" None None |> Async.RunSynchronously |> ignore
        let status, respBody = profitAndLoss db (Some token) "" "" "IDR" |> Async.RunSynchronously
        Assert.Equal(200, status)
        Assert.Contains("500000", respBody)

    [<Fact>]
    member _.``profit and loss with invalid date params uses defaults``() =
        let db, cleanup, token = setupUser ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let status, _ = profitAndLoss db (Some token) "notadate" "notadate" "USD" |> Async.RunSynchronously
        Assert.Equal(200, status)

    [<Fact>]
    member _.``profit and loss with valid date range filters correctly``() =
        let db, cleanup, token = setupUser ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let status, _ = profitAndLoss db (Some token) "2024-01-01" "2024-12-31" "USD" |> Async.RunSynchronously
        Assert.Equal(200, status)

    [<Fact>]
    member _.``profit and loss with no date params returns all``() =
        let db, cleanup, token = setupUser ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let status, _ = profitAndLoss db (Some token) "" "" "USD" |> Async.RunSynchronously
        Assert.Equal(200, status)

// ─────────────────────────────────────────────────────────────────────────────
// Additional auth handler coverage
// ─────────────────────────────────────────────────────────────────────────────

[<Trait("Category", "Unit")>]
type AuthHandlerAdditionalTests() =

    [<Fact>]
    member _.``refresh with inactive user returns 401``() =
        let db, cleanup = createDb ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let username = $"rfi_{shortId ()}"
        let email = $"{username}@example.com"
        register db username email "Str0ng#Pass1!" |> Async.RunSynchronously |> ignore
        let _s, loginResp = login db username "Str0ng#Pass1!" |> Async.RunSynchronously
        let doc = JsonDocument.Parse(loginResp)
        let token = doc.RootElement.GetProperty("access_token").GetString()
        let rt = doc.RootElement.GetProperty("refresh_token").GetString()
        deactivate db (Some token) |> Async.RunSynchronously |> ignore
        let status, _ = refresh db rt |> Async.RunSynchronously
        Assert.Equal(401, status)

    [<Fact>]
    member _.``logout already logged out token is idempotent``() =
        let db, cleanup = createDb ()
        use _ = { new IDisposable with member _.Dispose() = cleanup () }
        let username = $"dbl_{shortId ()}"
        let token = registerAndLogin db username
        logout db (Some token) |> Async.RunSynchronously |> ignore
        // Second logout with same token — logout is safe to call twice
        let status, _ = logout db (Some token) |> Async.RunSynchronously
        // logout returns 200 regardless; the token is already revoked in DB
        Assert.Equal(200, status)
