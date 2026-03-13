module DemoBeFsgi.Tests.TestFixture

open System
open Microsoft.Data.Sqlite
open Microsoft.EntityFrameworkCore
open DemoBeFsgi.Infrastructure.AppDbContext

/// Returns true when a real PostgreSQL DATABASE_URL is present in the environment.
/// Integration tests (docker-compose) always set DATABASE_URL.
/// Unit/test:quick mode runs without DATABASE_URL and uses SQLite in-memory.
let private usePostgres =
    not (String.IsNullOrEmpty(Environment.GetEnvironmentVariable("DATABASE_URL")))

/// Creates a fresh, isolated AppDbContext per scenario.
///
/// PostgreSQL mode (DATABASE_URL set): uses Npgsql with the supplied connection string.
/// The schema must already exist (EnsureCreated is called once at startup by docker-compose).
///
/// SQLite in-memory mode (no DATABASE_URL): creates a shared in-memory connection per call
/// and calls EnsureCreated so each scenario starts with a clean schema.
let createDb () : AppDbContext * (unit -> unit) =
    if usePostgres then
        let connStr = Environment.GetEnvironmentVariable("DATABASE_URL")

        let options =
            DbContextOptionsBuilder<AppDbContext>().UseNpgsql(connStr).UseSnakeCaseNamingConvention().Options

        let db = new AppDbContext(options)

        // Ensure schema exists (idempotent)
        db.Database.EnsureCreated() |> ignore

        // Cleanup: wipe all rows in reverse-dependency order after each scenario
        let cleanup () =
            db.Database.ExecuteSqlRaw("DELETE FROM attachments") |> ignore
            db.Database.ExecuteSqlRaw("DELETE FROM expenses") |> ignore
            db.Database.ExecuteSqlRaw("DELETE FROM refresh_tokens") |> ignore
            db.Database.ExecuteSqlRaw("DELETE FROM revoked_tokens") |> ignore
            db.Database.ExecuteSqlRaw("DELETE FROM users") |> ignore
            db.Dispose()

        db, cleanup
    else
        // SQLite in-memory: each call gets its own connection → isolated schema
        let conn = new SqliteConnection("DataSource=:memory:")
        conn.Open()

        let options = DbContextOptionsBuilder<AppDbContext>().UseSqlite(conn).Options

        let db = new AppDbContext(options)
        db.Database.EnsureCreated() |> ignore

        let cleanup () =
            db.Dispose()
            conn.Dispose()

        db, cleanup
