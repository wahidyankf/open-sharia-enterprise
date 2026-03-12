module DemoBeFsgi.Tests.TestFixture

open System
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Mvc.Testing
open Microsoft.Data.Sqlite
open Microsoft.EntityFrameworkCore
open Microsoft.Extensions.DependencyInjection
open DemoBeFsgi.Infrastructure.AppDbContext

/// Each TestWebAppFactory instance uses a single shared SqliteConnection
/// (kept open for the lifetime of the factory) so that in-memory data persists
/// across multiple HTTP requests within one scenario.
type TestWebAppFactory() =
    inherit WebApplicationFactory<DemoBeFsgi.Program.Marker>()

    // Open a persistent in-memory SQLite connection
    let connection =
        let c = new SqliteConnection("DataSource=:memory:")
        c.Open()
        c

    let mutable schemaCreated = false

    override _.ConfigureWebHost(builder) =
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
            services.AddDbContext<AppDbContext>(fun options -> options.UseSqlite(connection) |> ignore)
            |> ignore)
        |> ignore

        builder.UseEnvironment("Test") |> ignore

    member this.CreateClient() =
        // Ensure schema is created exactly once per factory instance
        if not schemaCreated then
            use scope = this.Services.CreateScope()
            let db = scope.ServiceProvider.GetRequiredService<AppDbContext>()
            db.Database.EnsureCreated() |> ignore
            schemaCreated <- true

        (this :> WebApplicationFactory<DemoBeFsgi.Program.Marker>).CreateClient()

    override _.Dispose(disposing) =
        if disposing then
            connection.Dispose()

        base.Dispose(disposing)
