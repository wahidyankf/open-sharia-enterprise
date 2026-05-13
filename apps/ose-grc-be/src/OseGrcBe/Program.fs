module OseGrcBe.Program

open System
open Microsoft.AspNetCore.Builder
open Microsoft.EntityFrameworkCore
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Configuration
open Giraffe
open OseGrcBe.Infrastructure.AppDbContext

// Marker type for WebApplicationFactory<> in tests — must be accessible from test assembly.
type Marker = class end

let webApp: HttpHandler =
    choose
        [ GET >=> route "/api/v1/health" >=> Handlers.HealthHandler.handle
          RequestErrors.NOT_FOUND "Not Found" ]

[<EntryPoint>]
let main _ =
    let builder = WebApplication.CreateBuilder()

    // OpenRouter options
    builder.Services.Configure<OseGrcBe.Domain.AiOrchestration.OpenRouterSettings>(
        builder.Configuration.GetSection("OpenRouter")
    )
    |> ignore

    // EF Core — read DATABASE_URL
    let connectionString =
        let env = Environment.GetEnvironmentVariable("DATABASE_URL")

        if String.IsNullOrEmpty(env) then
            builder.Configuration.GetConnectionString("Default")
            |> Option.ofObj
            |> Option.defaultValue "Host=localhost;Database=ose_grc_dev;Username=ose_grc;Password=ose_grc"
        else
            env

    builder.Services.AddDbContext<AppDbContext>(fun options ->
        options.UseNpgsql(connectionString).UseSnakeCaseNamingConvention() |> ignore)
    |> ignore

    builder.Services.AddGiraffe() |> ignore

    let app = builder.Build()

    // DbUp migrations at startup (skip if no DB connection available)
    try
        let result = OseGrcBe.Infrastructure.Migrations.upgrade connectionString

        if not result.Successful then
            eprintfn "DbUp migration failed: %s" result.Error.Message
    with ex ->
        eprintfn "DbUp migration skipped (no DB): %s" ex.Message

    app.UseGiraffe(webApp)
    app.Run()
    0
