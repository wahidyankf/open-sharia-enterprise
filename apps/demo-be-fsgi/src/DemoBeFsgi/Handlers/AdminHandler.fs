module DemoBeFsgi.Handlers.AdminHandler

open System
open System.Linq
open System.Text.Json
open Giraffe
open Microsoft.AspNetCore.Http
open Microsoft.EntityFrameworkCore
open DemoBeFsgi.Infrastructure.AppDbContext
open DemoBeFsgi.Domain.Types

[<CLIMutable>]
type DisableRequest = { reason: string }

let listUsers: HttpHandler =
    fun next ctx ->
        task {
            let db = ctx.GetService<AppDbContext>()
            let pageParam = ctx.TryGetQueryStringValue("page") |> Option.defaultValue "1"
            let sizeParam = ctx.TryGetQueryStringValue("size") |> Option.defaultValue "20"
            let emailFilter = ctx.TryGetQueryStringValue("email")

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

            let query =
                match emailFilter with
                | Some email -> db.Users.Where(fun u -> u.Email = email)
                | None -> db.Users :> IQueryable<UserEntity>

            let total = query.CountAsync() |> Async.AwaitTask |> Async.RunSynchronously
            let offset = (page - 1) * size

            let users =
                query.Skip(offset).Take(size).ToListAsync()
                |> Async.AwaitTask
                |> Async.RunSynchronously

            let userData =
                users
                |> Seq.map (fun u ->
                    {| id = u.Id
                       username = u.Username
                       email = u.Email
                       display_name = u.DisplayName
                       role = u.Role
                       status = u.Status |})
                |> Seq.toArray

            return!
                json
                    {| data = userData
                       total = total
                       page = page
                       size = size |}
                    next
                    ctx
        }

let disableUser (userId: Guid) : HttpHandler =
    fun next ctx ->
        task {
            let! body = ctx.ReadBodyFromRequestAsync()

            let req =
                try
                    JsonSerializer.Deserialize<DisableRequest>(
                        body,
                        JsonSerializerOptions(PropertyNameCaseInsensitive = true)
                    )
                    |> Some
                with _ ->
                    None

            let db = ctx.GetService<AppDbContext>()

            let user =
                db.Users.AsNoTracking().FirstOrDefaultAsync(fun u -> u.Id = userId)
                |> Async.AwaitTask
                |> Async.RunSynchronously

            if obj.ReferenceEquals(user, null) then
                ctx.Response.StatusCode <- 404

                return!
                    json
                        {| error = "Not Found"
                           message = "User not found" |}
                        earlyReturn
                        ctx
            else
                let updated =
                    { user with
                        Status = statusToString Disabled
                        UpdatedAt = DateTime.UtcNow }

                db.Users.Update(updated) |> ignore
                db.SaveChangesAsync() |> Async.AwaitTask |> Async.RunSynchronously |> ignore

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
            let db = ctx.GetService<AppDbContext>()

            let user =
                db.Users.AsNoTracking().FirstOrDefaultAsync(fun u -> u.Id = userId)
                |> Async.AwaitTask
                |> Async.RunSynchronously

            if obj.ReferenceEquals(user, null) then
                ctx.Response.StatusCode <- 404

                return!
                    json
                        {| error = "Not Found"
                           message = "User not found" |}
                        earlyReturn
                        ctx
            else
                let updated =
                    { user with
                        Status = statusToString Active
                        UpdatedAt = DateTime.UtcNow }

                db.Users.Update(updated) |> ignore
                db.SaveChangesAsync() |> Async.AwaitTask |> Async.RunSynchronously |> ignore

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
            let db = ctx.GetService<AppDbContext>()

            let user =
                db.Users.AsNoTracking().FirstOrDefaultAsync(fun u -> u.Id = userId)
                |> Async.AwaitTask
                |> Async.RunSynchronously

            if obj.ReferenceEquals(user, null) then
                ctx.Response.StatusCode <- 404

                return!
                    json
                        {| error = "Not Found"
                           message = "User not found" |}
                        earlyReturn
                        ctx
            else
                let updated =
                    { user with
                        Status = statusToString Active
                        FailedLoginAttempts = 0
                        UpdatedAt = DateTime.UtcNow }

                db.Users.Update(updated) |> ignore
                db.SaveChangesAsync() |> Async.AwaitTask |> Async.RunSynchronously |> ignore

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
            let db = ctx.GetService<AppDbContext>()

            let user =
                db.Users.AsNoTracking().FirstOrDefaultAsync(fun u -> u.Id = userId)
                |> Async.AwaitTask
                |> Async.RunSynchronously

            if obj.ReferenceEquals(user, null) then
                ctx.Response.StatusCode <- 404

                return!
                    json
                        {| error = "Not Found"
                           message = "User not found" |}
                        earlyReturn
                        ctx
            else
                let resetToken = Guid.NewGuid().ToString("N")

                return!
                    json
                        {| message = "Password reset token generated"
                           reset_token = resetToken |}
                        next
                        ctx
        }
