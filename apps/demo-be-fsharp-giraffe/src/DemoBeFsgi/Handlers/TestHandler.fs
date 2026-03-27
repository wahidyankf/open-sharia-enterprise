module DemoBeFsgi.Handlers.TestHandler

open System
open System.Text.Json
open Giraffe
open Microsoft.EntityFrameworkCore
open DemoBeFsgi.Infrastructure.AppDbContext
open DemoBeFsgi.Infrastructure.Repositories.RepositoryTypes
open DemoBeFsgi.Contracts.ContractWrappers

let private testApiEnabled () =
    Environment.GetEnvironmentVariable("ENABLE_TEST_API") = "true"

let resetDb: HttpHandler =
    fun next ctx ->
        if not (testApiEnabled ()) then
            RequestErrors.notFound (json {| error = "Not Found" |}) next ctx
        else
            task {
                let db = ctx.GetService<AppDbContext>()

                let! _ = db.Database.ExecuteSqlRawAsync("DELETE FROM attachments")
                let! _ = db.Database.ExecuteSqlRawAsync("DELETE FROM expenses")
                let! _ = db.Database.ExecuteSqlRawAsync("DELETE FROM refresh_tokens")
                let! _ = db.Database.ExecuteSqlRawAsync("DELETE FROM revoked_tokens")
                let! _ = db.Database.ExecuteSqlRawAsync("DELETE FROM users")

                return! json {| message = "Database reset successful" |} next ctx
            }

let promoteAdmin: HttpHandler =
    fun next ctx ->
        if not (testApiEnabled ()) then
            RequestErrors.notFound (json {| error = "Not Found" |}) next ctx
        else
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
                    let userRepo = ctx.GetService<UserRepository>()
                    let username = if r.username = null then "" else r.username

                    let! userOpt = userRepo.FindByUsername username

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
                        let updated = { user with Role = "ADMIN" }
                        let! _ = userRepo.Update updated

                        return! json {| message = sprintf "User %s promoted to ADMIN" username |} next ctx
            }
