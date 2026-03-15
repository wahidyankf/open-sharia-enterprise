module DemoBeFsgi.Handlers.TestHandler

open System
open System.Text.Json
open Giraffe
open Microsoft.EntityFrameworkCore
open DemoBeFsgi.Infrastructure.AppDbContext

let resetDb: HttpHandler =
    fun next ctx ->
        task {
            let db = ctx.GetService<AppDbContext>()

            let! _ = db.Database.ExecuteSqlRawAsync("DELETE FROM attachments")
            let! _ = db.Database.ExecuteSqlRawAsync("DELETE FROM expenses")
            let! _ = db.Database.ExecuteSqlRawAsync("DELETE FROM refresh_tokens")
            let! _ = db.Database.ExecuteSqlRawAsync("DELETE FROM revoked_tokens")
            let! _ = db.Database.ExecuteSqlRawAsync("DELETE FROM users")

            return! json {| message = "Database reset successful" |} next ctx
        }

[<CLIMutable>]
type PromoteAdminRequest = { username: string }

let promoteAdmin: HttpHandler =
    fun next ctx ->
        task {
            let! body = ctx.ReadBodyFromRequestAsync()

            let req =
                try
                    JsonSerializer.Deserialize<PromoteAdminRequest>(
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
                let db = ctx.GetService<AppDbContext>()
                let username = if r.username = null then "" else r.username

                let! user = db.Users.AsNoTracking().FirstOrDefaultAsync(fun u -> u.Username = username)

                if obj.ReferenceEquals(user, null) then
                    ctx.Response.StatusCode <- 404

                    return!
                        json
                            {| error = "Not Found"
                               message = "User not found" |}
                            earlyReturn
                            ctx
                else
                    let updated = { user with Role = "ADMIN" }
                    db.Users.Update(updated) |> ignore
                    let! _ = db.SaveChangesAsync()

                    return! json {| message = sprintf "User %s promoted to ADMIN" username |} next ctx
        }
