module DemoBeFsgi.Tests.TestFixture

open System
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Mvc.Testing
open Microsoft.Data.Sqlite
open Microsoft.EntityFrameworkCore
open Microsoft.Extensions.DependencyInjection
open DemoBeFsgi.Infrastructure.AppDbContext

/// Returns true when a real PostgreSQL DATABASE_URL is present in the environment.
/// Used to switch between SQLite in-memory (unit/test:quick) and PostgreSQL (docker-compose integration).
let private usePostgres =
    not (String.IsNullOrEmpty(Environment.GetEnvironmentVariable("DATABASE_URL")))

/// WebApplicationFactory used for both unit-level tests (SQLite in-memory, no external services)
/// and integration tests (real PostgreSQL, configured via DATABASE_URL environment variable).
///
/// When DATABASE_URL is not set (unit/test:quick mode), replaces the production EF Core
/// registration with SQLite in-memory using a single shared connection so all DbContext
/// instances in a scenario see the same data.
///
/// When DATABASE_URL is set (docker-compose integration mode), delegates to the production
/// PostgreSQL registration wired in Program.fs without overriding it.
type TestWebAppFactory() =
    inherit WebApplicationFactory<DemoBeFsgi.Program.Marker>()

    // Open a persistent in-memory SQLite connection — only used when DATABASE_URL is absent.
    let connection =
        if usePostgres then
            None
        else
            let c = new SqliteConnection("DataSource=:memory:")
            c.Open()
            Some c

    let mutable schemaCreated = false

    override _.ConfigureWebHost(builder) =
        builder.UseEnvironment("Test") |> ignore

        if not usePostgres then
            // Unit mode: replace production DB registration with SQLite in-memory.
            builder.ConfigureServices(fun services ->
                let descriptor =
                    services
                    |> Seq.tryFind (fun d -> d.ServiceType = typeof<DbContextOptions<AppDbContext>>)

                match descriptor with
                | Some d -> services.Remove(d) |> ignore
                | None -> ()

                let dbDescriptor =
                    services |> Seq.tryFind (fun d -> d.ServiceType = typeof<AppDbContext>)

                match dbDescriptor with
                | Some d -> services.Remove(d) |> ignore
                | None -> ()

                // Register DbContext using the shared open connection
                match connection with
                | Some conn ->
                    services.AddDbContext<AppDbContext>(fun options -> options.UseSqlite(conn) |> ignore)
                    |> ignore
                | None -> ())
            |> ignore
    // When usePostgres is true, Program.fs already registered Npgsql from DATABASE_URL.
    // No override needed — EnsureCreated in Program.fs startup creates the schema.

    member this.CreateClient() =
        // Ensure schema is created exactly once per factory instance (SQLite unit mode only).
        if not usePostgres && not schemaCreated then
            use scope = this.Services.CreateScope()
            let db = scope.ServiceProvider.GetRequiredService<AppDbContext>()
            db.Database.EnsureCreated() |> ignore
            schemaCreated <- true

        (this :> WebApplicationFactory<DemoBeFsgi.Program.Marker>).CreateClient()

    override _.Dispose(disposing) =
        if disposing then
            connection |> Option.iter (fun c -> c.Dispose())

        base.Dispose(disposing)
