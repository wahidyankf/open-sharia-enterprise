module OseGrcBe.Infrastructure.Migrations

open System.Reflection
open DbUp

let upgrade (connectionString: string) =
    let upgrader =
        DeployChanges.To
            .PostgresqlDatabase(connectionString)
            .WithScriptsEmbeddedInAssembly(Assembly.GetExecutingAssembly())
            .LogToConsole()
            .Build()

    let result = upgrader.PerformUpgrade()
    result
