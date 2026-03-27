module DemoBeFsgi.Handlers.AdminHandler

open System
open System.Text.Json
open Giraffe
open DemoBeFsgi.Infrastructure.AppDbContext
open DemoBeFsgi.Infrastructure.Repositories.RepositoryTypes
open DemoBeFsgi.Domain.Types
open DemoBeFsgi.Contracts.ContractWrappers

let listUsers: HttpHandler =
    fun next ctx ->
        task {
            let userRepo = ctx.GetService<UserRepository>()
            let pageParam = ctx.TryGetQueryStringValue("page") |> Option.defaultValue "1"
            let sizeParam = ctx.TryGetQueryStringValue("size") |> Option.defaultValue "20"
            let searchFilter = ctx.TryGetQueryStringValue("search")

            let page =
                Math.Max(
                    1,
                    try
                        int pageParam
                    with _ ->
                        1
                )

            let size =
                Math.Max(
                    1,
                    try
                        int sizeParam
                    with _ ->
                        20
                )

            let! total = userRepo.CountByFilter searchFilter
            let! users = userRepo.ListByFilter searchFilter page size

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

            return!
                json
                    {| content = userData
                       totalElements = total
                       page = page
                       size = size |}
                    next
                    ctx
        }

let disableUser (userId: Guid) : HttpHandler =
    fun next ctx ->
        task {
            let! body = ctx.ReadBodyFromRequestAsync()

            let _req =
                try
                    JsonSerializer.Deserialize<DisableRequest>(
                        body,
                        JsonSerializerOptions(PropertyNameCaseInsensitive = true)
                    )
                    |> Some
                with _ ->
                    None

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
                        Status = statusToString Disabled
                        UpdatedAt = DateTime.UtcNow }

                let! _ = userRepo.Update updated

                return!
                    json
                        {| message = "User disabled"
                           id = userId
                           status = statusToString Disabled |}
                        next
                        ctx
        }

let enableUser (userId: Guid) : HttpHandler =
    fun next ctx ->
        task {
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
                        Status = statusToString Active
                        UpdatedAt = DateTime.UtcNow }

                let! _ = userRepo.Update updated

                return!
                    json
                        {| message = "User enabled"
                           id = userId
                           status = statusToString Active |}
                        next
                        ctx
        }

let unlockUser (userId: Guid) : HttpHandler =
    fun next ctx ->
        task {
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
                        Status = statusToString Active
                        FailedLoginAttempts = 0
                        UpdatedAt = DateTime.UtcNow }

                let! _ = userRepo.Update updated

                return!
                    json
                        {| message = "User unlocked"
                           id = userId
                           status = statusToString Active |}
                        next
                        ctx
        }

let forcePasswordReset (userId: Guid) : HttpHandler =
    fun next ctx ->
        task {
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
            | Some _ ->
                let resetToken = Guid.NewGuid().ToString("N")

                return!
                    json
                        {| message = "Password reset token generated"
                           token = resetToken |}
                        next
                        ctx
        }
