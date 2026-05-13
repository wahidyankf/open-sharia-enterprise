module OseGrcBe.Tests.TestFixture

open Microsoft.AspNetCore.Mvc.Testing
open Microsoft.EntityFrameworkCore
open Microsoft.Extensions.DependencyInjection
open OseGrcBe.Infrastructure.AppDbContext

type OseGrcBeFactory() =
    inherit WebApplicationFactory<OseGrcBe.Program.Marker>()

    override _.ConfigureWebHost(builder) =
        builder.ConfigureServices(fun services ->
            // Remove the registered DbContext and replace with in-memory
            let descriptor =
                services
                |> Seq.tryFind (fun d -> d.ServiceType = typeof<DbContextOptions<AppDbContext>>)

            match descriptor with
            | Some d -> services.Remove(d) |> ignore
            | None -> ()

            services.AddDbContext<AppDbContext>(fun options -> options.UseInMemoryDatabase("TestDb") |> ignore)
            |> ignore)
        |> ignore
