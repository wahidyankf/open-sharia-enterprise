module DemoBeFsgi.Tests.HttpTestFixture

open System
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Mvc.Testing
open Microsoft.Data.Sqlite
open Microsoft.EntityFrameworkCore
open Microsoft.Extensions.DependencyInjection
open DemoBeFsgi.Infrastructure.AppDbContext

/// WebApplicationFactory that replaces the database with an in-memory SQLite instance.
/// Used exclusively in HandlerCoverageTests to exercise the HTTP handler pipeline
/// and obtain AltCover coverage for handler code, without requiring any external services.
///
/// Each factory instance uses a unique in-memory SQLite connection so test classes
/// are fully isolated from each other.
type TestWebAppFactory() =
    inherit WebApplicationFactory<DemoBeFsgi.Program.Marker>()

    let conn = new SqliteConnection("DataSource=:memory:")

    do conn.Open()

    override _.ConfigureWebHost(builder: IWebHostBuilder) =
        builder.ConfigureServices(fun services ->
            // Remove the existing DbContext registration
            let descriptor =
                services
                |> Seq.tryFind (fun d -> d.ServiceType = typeof<DbContextOptions<AppDbContext>>)

            match descriptor with
            | Some d -> services.Remove(d) |> ignore
            | None -> ()

            // Add SQLite in-memory with the open connection so schema is preserved
            services.AddDbContext<AppDbContext>(fun opts ->
                opts.UseSqlite(conn).UseSnakeCaseNamingConvention() |> ignore)
            |> ignore)
        |> ignore

        builder.UseEnvironment("Testing") |> ignore

    /// Creates the schema and returns an HttpClient ready to send requests.
    member this.CreateClientWithDb() =
        use scope = this.Services.CreateScope()
        let db = scope.ServiceProvider.GetRequiredService<AppDbContext>()
        db.Database.EnsureCreated() |> ignore
        this.CreateClient()

    interface IDisposable with
        member this.Dispose() =
            base.Dispose()
            conn.Dispose()
