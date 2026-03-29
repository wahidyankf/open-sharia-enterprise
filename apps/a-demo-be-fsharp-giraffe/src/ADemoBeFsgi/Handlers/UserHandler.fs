module ADemoBeFsgi.Handlers.UserHandler

open System
open System.Text.Json
open Giraffe
open ADemoBeFsgi.Infrastructure.AppDbContext
open ADemoBeFsgi.Infrastructure.PasswordHasher
open ADemoBeFsgi.Infrastructure.Repositories.RepositoryTypes
open ADemoBeFsgi.Domain.Types
open ADemoBeFsgi.Contracts.ContractWrappers

let getProfile: HttpHandler =
    fun next ctx ->
        task {
            let userId = ctx.Items["UserId"] :?> Guid
            let userRepo = ctx.GetService<UserRepository>()

            let! userOpt = userRepo.FindById userId

            match userOpt with
            | None ->
                ctx.Response.StatusCode <- 404

                return!
                    json
                        {| error = "Not Found"
                           message = "User not found" |}
                        earlyReturn
                        ctx
            | Some user ->
                return!
                    json
                        {| id = user.Id
                           username = user.Username
                           email = user.Email
                           displayName = user.DisplayName
                           role = user.Role
                           status = user.Status |}
                        next
                        ctx
        }

let updateProfile: HttpHandler =
    fun next ctx ->
        task {
            let! body = ctx.ReadBodyFromRequestAsync()

            let req =
                try
                    JsonSerializer.Deserialize<UpdateProfileRequest>(
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
                let userId = ctx.Items["UserId"] :?> Guid
                let userRepo = ctx.GetService<UserRepository>()

                let! userOpt = userRepo.FindById userId

                match userOpt with
                | None ->
                    ctx.Response.StatusCode <- 404

                    return!
                        json
                            {| error = "Not Found"
                               message = "User not found" |}
                            earlyReturn
                            ctx
                | Some user ->
                    let updated =
                        { user with
                            DisplayName =
                                if r.displayName <> null then
                                    r.displayName
                                else
                                    user.DisplayName
                            UpdatedAt = DateTime.UtcNow }

                    let! saved = userRepo.Update updated

                    return!
                        json
                            {| id = saved.Id
                               username = saved.Username
                               email = saved.Email
                               displayName = saved.DisplayName |}
                            next
                            ctx
        }

let changePassword: HttpHandler =
    fun next ctx ->
        task {
            let! body = ctx.ReadBodyFromRequestAsync()

            let req =
                try
                    JsonSerializer.Deserialize<ChangePasswordRequest>(
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
                let userId = ctx.Items["UserId"] :?> Guid
                let userRepo = ctx.GetService<UserRepository>()

                let! userOpt = userRepo.FindById userId

                match userOpt with
                | None ->
                    ctx.Response.StatusCode <- 404

                    return!
                        json
                            {| error = "Not Found"
                               message = "User not found" |}
                            earlyReturn
                            ctx
                | Some user when not (verifyPassword r.oldPassword user.PasswordHash) ->
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
                            PasswordHash = hashPassword r.newPassword
                            UpdatedAt = DateTime.UtcNow }

                    let! _ = userRepo.Update updated

                    return! json {| message = "Password changed successfully" |} next ctx
        }

let deactivate: HttpHandler =
    fun next ctx ->
        task {
            let userId = ctx.Items["UserId"] :?> Guid
            let userRepo = ctx.GetService<UserRepository>()

            let! userOpt = userRepo.FindById userId

            match userOpt with
            | None ->
                ctx.Response.StatusCode <- 404

                return!
                    json
                        {| error = "Not Found"
                           message = "User not found" |}
                        earlyReturn
                        ctx
            | Some user ->
                let updated =
                    { user with
                        Status = statusToString Inactive
                        UpdatedAt = DateTime.UtcNow }

                let! _ = userRepo.Update updated

                return! json {| message = "Account deactivated" |} next ctx
        }
