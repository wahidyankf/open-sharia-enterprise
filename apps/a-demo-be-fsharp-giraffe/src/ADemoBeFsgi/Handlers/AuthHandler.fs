module ADemoBeFsgi.Handlers.AuthHandler

open System
open System.Text.Json
open Giraffe
open ADemoBeFsgi.Infrastructure.AppDbContext
open ADemoBeFsgi.Infrastructure.PasswordHasher
open ADemoBeFsgi.Infrastructure.Repositories.RepositoryTypes
open ADemoBeFsgi.Domain.User
open ADemoBeFsgi.Domain.Types
open ADemoBeFsgi.Auth.JwtService
open ADemoBeFsgi.Contracts.ContractWrappers

let private maxFailedAttempts = 5

let register: HttpHandler =
    fun _next ctx ->
        task {
            let! body = ctx.ReadBodyFromRequestAsync()

            let req =
                try
                    JsonSerializer.Deserialize<RegisterRequest>(
                        body,
                        JsonSerializerOptions(PropertyNameCaseInsensitive = true)
                    )
                    |> Some
                with _ ->
                    None

            match req with
            | None ->
                ctx.Response.StatusCode <- 400

                return!
                    json
                        {| error = "Bad Request"
                           message = "Invalid request body" |}
                        earlyReturn
                        ctx
            | Some r ->
                let usernameResult = validateUsername (if r.username = null then "" else r.username)
                let emailResult = validateEmail (if r.email = null then "" else r.email)
                let passwordResult = validatePassword (if r.password = null then "" else r.password)

                match usernameResult, emailResult, passwordResult with
                | Error(ValidationError(f, m)), _, _ ->
                    ctx.Response.StatusCode <- 400

                    return!
                        json
                            {| error = "Validation Error"
                               field = f
                               message = m |}
                            earlyReturn
                            ctx
                | _, Error(ValidationError(f, m)), _ ->
                    ctx.Response.StatusCode <- 400

                    return!
                        json
                            {| error = "Validation Error"
                               field = f
                               message = m |}
                            earlyReturn
                            ctx
                | _, _, Error(ValidationError(f, m)) ->
                    ctx.Response.StatusCode <- 400

                    return!
                        json
                            {| error = "Validation Error"
                               field = f
                               message = m |}
                            earlyReturn
                            ctx
                | Ok _, Ok _, Ok _ ->
                    let userRepo = ctx.GetService<UserRepository>()

                    let! existingUser = userRepo.FindByUsername r.username

                    if existingUser.IsSome then
                        ctx.Response.StatusCode <- 409

                        return!
                            json
                                {| error = "Conflict"
                                   message = "Username already exists" |}
                                earlyReturn
                                ctx
                    else
                        let now = DateTime.UtcNow
                        let userId = Guid.NewGuid()

                        let entity: UserEntity =
                            { Id = userId
                              Username = r.username
                              Email = r.email
                              DisplayName = r.username
                              PasswordHash = hashPassword r.password
                              Role = roleToString User
                              Status = statusToString Active
                              FailedLoginAttempts = 0
                              CreatedAt = now
                              UpdatedAt = now }

                        let! _ = userRepo.Create entity

                        ctx.Response.StatusCode <- 201

                        return!
                            json
                                {| id = userId
                                   username = entity.Username
                                   email = entity.Email
                                   displayName = entity.DisplayName |}
                                earlyReturn
                                ctx
                | _ ->
                    ctx.Response.StatusCode <- 400

                    return!
                        json
                            {| error = "Bad Request"
                               message = "Validation failed" |}
                            earlyReturn
                            ctx
        }

let login: HttpHandler =
    fun _next ctx ->
        task {
            let! body = ctx.ReadBodyFromRequestAsync()

            let req =
                try
                    JsonSerializer.Deserialize<LoginRequest>(
                        body,
                        JsonSerializerOptions(PropertyNameCaseInsensitive = true)
                    )
                    |> Some
                with _ ->
                    None

            match req with
            | None ->
                ctx.Response.StatusCode <- 400

                return!
                    json
                        {| error = "Bad Request"
                           message = "Invalid request body" |}
                        earlyReturn
                        ctx
            | Some r ->
                let userRepo = ctx.GetService<UserRepository>()
                let rtRepo = ctx.GetService<RefreshTokenRepository>()

                let! userOpt = userRepo.FindByUsername r.username

                match userOpt with
                | None ->
                    ctx.Response.StatusCode <- 401

                    return!
                        json
                            {| error = "Unauthorized"
                               message = "Invalid credentials" |}
                            earlyReturn
                            ctx
                | Some user when user.Status = statusToString Locked ->
                    ctx.Response.StatusCode <- 401

                    return!
                        json
                            {| error = "Unauthorized"
                               message = "Account is locked after too many failed attempts" |}
                            earlyReturn
                            ctx
                | Some user when user.Status = statusToString Inactive ->
                    ctx.Response.StatusCode <- 401

                    return!
                        json
                            {| error = "Unauthorized"
                               message = "Account has been deactivated" |}
                            earlyReturn
                            ctx
                | Some user when user.Status = statusToString Disabled ->
                    ctx.Response.StatusCode <- 401

                    return!
                        json
                            {| error = "Unauthorized"
                               message = "Account has been disabled" |}
                            earlyReturn
                            ctx
                | Some user when not (verifyPassword r.password user.PasswordHash) ->
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

                    let! _ = userRepo.Update updated

                    if newAttempts >= maxFailedAttempts then
                        ctx.Response.StatusCode <- 401

                        return!
                            json
                                {| error = "Unauthorized"
                                   message = "Account is locked after too many failed attempts" |}
                                earlyReturn
                                ctx
                    else
                        ctx.Response.StatusCode <- 401

                        return!
                            json
                                {| error = "Unauthorized"
                                   message = "Invalid credentials" |}
                                earlyReturn
                                ctx
                | Some user ->
                    let updated =
                        { user with
                            FailedLoginAttempts = 0
                            UpdatedAt = DateTime.UtcNow }

                    let! _ = userRepo.Update updated

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

                    let! _ = rtRepo.Create rtEntity

                    return!
                        json
                            {| accessToken = accessToken
                               refreshToken = refreshTokenStr
                               tokenType = "Bearer" |}
                            earlyReturn
                            ctx
        }

let refresh: HttpHandler =
    fun _next ctx ->
        task {
            let! body = ctx.ReadBodyFromRequestAsync()

            let req =
                try
                    JsonSerializer.Deserialize<RefreshRequest>(
                        body,
                        JsonSerializerOptions(PropertyNameCaseInsensitive = true)
                    )
                    |> Some
                with _ ->
                    None

            match req with
            | None ->
                ctx.Response.StatusCode <- 400

                return!
                    json
                        {| error = "Bad Request"
                           message = "Invalid request body" |}
                        earlyReturn
                        ctx
            | Some r ->
                let userRepo = ctx.GetService<UserRepository>()
                let rtRepo = ctx.GetService<RefreshTokenRepository>()

                let! rtEntityOpt = rtRepo.FindActiveByHash r.refreshToken

                match rtEntityOpt with
                | None ->
                    ctx.Response.StatusCode <- 401

                    return!
                        json
                            {| error = "Unauthorized"
                               message = "Invalid or already used token" |}
                            earlyReturn
                            ctx
                | Some rtEntity when rtEntity.ExpiresAt < DateTime.UtcNow ->
                    ctx.Response.StatusCode <- 401

                    return!
                        json
                            {| error = "Unauthorized"
                               message = "Token has expired" |}
                            earlyReturn
                            ctx
                | Some rtEntity ->
                    let! userOpt = userRepo.FindById rtEntity.UserId

                    match userOpt with
                    | None ->
                        ctx.Response.StatusCode <- 401

                        return!
                            json
                                {| error = "Unauthorized"
                                   message = "User not found" |}
                                earlyReturn
                                ctx
                    | Some user when user.Status <> statusToString Active ->
                        ctx.Response.StatusCode <- 401

                        return!
                            json
                                {| error = "Unauthorized"
                                   message = "Account has been deactivated" |}
                                earlyReturn
                                ctx
                    | Some user ->
                        let revokedRt = { rtEntity with Revoked = true }
                        let! _ = rtRepo.Update revokedRt

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

                        let! _ = rtRepo.Create newRtEntity

                        return!
                            json
                                {| accessToken = accessToken
                                   refreshToken = newRefreshToken
                                   tokenType = "Bearer" |}
                                earlyReturn
                                ctx
        }

let logout: HttpHandler =
    fun next ctx ->
        task {
            let authHeader = ctx.Request.Headers["Authorization"].ToString()

            let token =
                if authHeader.StartsWith("Bearer ", StringComparison.Ordinal) then
                    authHeader.Substring(7)
                else
                    ""

            let tokenRepo = ctx.GetService<TokenRepository>()
            let jti = getTokenJti token

            match jti with
            | Some j ->
                let! exists = tokenRepo.ExistsJti j

                if not exists then
                    let userId =
                        if ctx.Items.ContainsKey("UserId") then
                            ctx.Items["UserId"] :?> Guid
                        else
                            Guid.Empty

                    let revokedEntity: RevokedTokenEntity =
                        { Id = Guid.NewGuid()
                          Jti = j
                          UserId = userId
                          RevokedAt = DateTime.UtcNow }

                    let! _ = tokenRepo.Create revokedEntity
                    ()
            | None -> ()

            return! json {| message = "Logged out successfully" |} next ctx
        }

let logoutAll: HttpHandler =
    fun next ctx ->
        task {
            let tokenRepo = ctx.GetService<TokenRepository>()
            let rtRepo = ctx.GetService<RefreshTokenRepository>()
            let authHeader = ctx.Request.Headers["Authorization"].ToString()

            let token =
                if authHeader.StartsWith("Bearer ", StringComparison.Ordinal) then
                    authHeader.Substring(7)
                else
                    ""

            let jti = getTokenJti token

            let userId =
                if ctx.Items.ContainsKey("UserId") then
                    ctx.Items["UserId"] :?> Guid
                else
                    Guid.Empty

            match jti with
            | Some j ->
                let! exists = tokenRepo.ExistsJti j

                if not exists then
                    let revokedEntity: RevokedTokenEntity =
                        { Id = Guid.NewGuid()
                          Jti = j
                          UserId = userId
                          RevokedAt = DateTime.UtcNow }

                    let! _ = tokenRepo.Create revokedEntity
                    ()
            | None -> ()

            let! activeTokens = rtRepo.ListActiveByUser userId

            for rt in activeTokens do
                let! _ = rtRepo.Update { rt with Revoked = true }
                ()

            return! json {| message = "All sessions logged out" |} next ctx
        }
