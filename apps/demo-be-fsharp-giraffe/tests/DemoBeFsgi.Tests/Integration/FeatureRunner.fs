module DemoBeFsgi.Tests.Integration.FeatureRunner

open System
open System.IO
open System.Reflection
open TickSpec
open Xunit
open DemoBeFsgi.Tests.TestFixture
open DemoBeFsgi.Tests.State

/// xUnit collection that forces all integration test classes to run sequentially.
/// Required because integration tests share a single PostgreSQL database.
[<CollectionDefinition("IntegrationDb", DisableParallelization = true)>]
type IntegrationDbCollection() = class end

let private assembly = Assembly.GetExecutingAssembly()

let private specsDir =
    let assemblyDir = Path.GetDirectoryName(assembly.Location)
    Path.Combine(assemblyDir, "specs")

let private getFeatureFile (namePart: string) =
    if Directory.Exists(specsDir) then
        Directory.GetFiles(specsDir, "*.feature", SearchOption.AllDirectories)
        |> Array.tryFind (fun f -> f.Contains(namePart))
    else
        None

/// Each scenario gets its own isolated AppDbContext (fresh database state).
/// The service provider injects a StepState seeded with that context.
type private ScenarioServiceProvider(db: DemoBeFsgi.Infrastructure.AppDbContext.AppDbContext) =
    interface IServiceProvider with
        member _.GetService(serviceType: Type) =
            if serviceType = typeof<StepState> then
                empty db :> obj
            else
                null

/// Read a feature file but preserve inline '#' characters by replacing them with
/// a temporary placeholder HASH_SIGN before TickSpec's Gherkin parser strips them.
/// Step definitions receive HASH_SIGN and call decode() to restore '#' before service calls.
let private preprocessFeatureLines (path: string) : string[] =
    File.ReadAllLines(path)
    |> Array.map (fun line ->
        let trimmed = line.TrimStart()

        if trimmed.StartsWith("#") then
            line // actual Gherkin comment line — leave as-is
        else
            line.Replace("#", "HASH_SIGN"))

let private buildScenarioData (namePart: string) : seq<obj[]> =
    match getFeatureFile namePart with
    | Some path ->
        let defs = StepDefinitions(assembly)

        defs.ServiceProviderFactory <-
            fun () ->
                let db, _cleanup = createDb ()
                // Note: cleanup happens when the AppDbContext is disposed at scenario end.
                // For PostgreSQL integration mode the cleanup lambda deletes all rows;
                // for SQLite in-memory the connection is dropped. Both are safe.
                ScenarioServiceProvider(db) :> IServiceProvider

        let lines = preprocessFeatureLines path
        let feature = defs.GenerateFeature(path, lines)
        feature.Scenarios |> Seq.map (fun scenario -> [| scenario :> obj |])
    | None -> Seq.empty

[<Collection("IntegrationDb")>]
[<Trait("Category", "Integration")>]
type HealthFeatureTests() =
    static member Scenarios() : seq<obj[]> =
        buildScenarioData "health-check" |> Seq.toList :> seq<_>

    [<Theory>]
    [<MemberData("Scenarios")>]
    member this.``Health Check``(scenario: Scenario) = scenario.Action.Invoke()

[<Collection("IntegrationDb")>]
[<Trait("Category", "Integration")>]
type RegistrationFeatureTests() =
    static member Scenarios() : seq<obj[]> =
        buildScenarioData "registration" |> Seq.toList :> seq<_>

    [<Theory>]
    [<MemberData("Scenarios")>]
    member this.``Registration``(scenario: Scenario) = scenario.Action.Invoke()

[<Collection("IntegrationDb")>]
[<Trait("Category", "Integration")>]
type PasswordLoginFeatureTests() =
    static member Scenarios() : seq<obj[]> =
        buildScenarioData "password-login" |> Seq.toList :> seq<_>

    [<Theory>]
    [<MemberData("Scenarios")>]
    member this.``Password Login``(scenario: Scenario) = scenario.Action.Invoke()

[<Collection("IntegrationDb")>]
[<Trait("Category", "Integration")>]
type TokenLifecycleFeatureTests() =
    static member Scenarios() : seq<obj[]> =
        buildScenarioData "token-lifecycle" |> Seq.toList :> seq<_>

    [<Theory>]
    [<MemberData("Scenarios")>]
    member this.``Token Lifecycle``(scenario: Scenario) = scenario.Action.Invoke()

[<Collection("IntegrationDb")>]
[<Trait("Category", "Integration")>]
type TokensFeatureTests() =
    static member Scenarios() : seq<obj[]> =
        buildScenarioData "tokens" |> Seq.toList :> seq<_>

    [<Theory>]
    [<MemberData("Scenarios")>]
    member this.``Tokens``(scenario: Scenario) = scenario.Action.Invoke()

[<Collection("IntegrationDb")>]
[<Trait("Category", "Integration")>]
type UserAccountFeatureTests() =
    static member Scenarios() : seq<obj[]> =
        buildScenarioData "user-account" |> Seq.toList :> seq<_>

    [<Theory>]
    [<MemberData("Scenarios")>]
    member this.``User Account``(scenario: Scenario) = scenario.Action.Invoke()

[<Collection("IntegrationDb")>]
[<Trait("Category", "Integration")>]
type SecurityFeatureTests() =
    static member Scenarios() : seq<obj[]> =
        buildScenarioData "security" |> Seq.toList :> seq<_>

    [<Theory>]
    [<MemberData("Scenarios")>]
    member this.``Security``(scenario: Scenario) = scenario.Action.Invoke()

[<Collection("IntegrationDb")>]
[<Trait("Category", "Integration")>]
type AdminFeatureTests() =
    static member Scenarios() : seq<obj[]> =
        buildScenarioData "admin" |> Seq.toList :> seq<_>

    [<Theory>]
    [<MemberData("Scenarios")>]
    member this.``Admin``(scenario: Scenario) = scenario.Action.Invoke()

[<Collection("IntegrationDb")>]
[<Trait("Category", "Integration")>]
type ExpenseManagementFeatureTests() =
    static member Scenarios() : seq<obj[]> =
        buildScenarioData "expense-management" |> Seq.toList :> seq<_>

    [<Theory>]
    [<MemberData("Scenarios")>]
    member this.``Expense Management``(scenario: Scenario) = scenario.Action.Invoke()

[<Collection("IntegrationDb")>]
[<Trait("Category", "Integration")>]
type CurrencyHandlingFeatureTests() =
    static member Scenarios() : seq<obj[]> =
        buildScenarioData "currency-handling" |> Seq.toList :> seq<_>

    [<Theory>]
    [<MemberData("Scenarios")>]
    member this.``Currency Handling``(scenario: Scenario) = scenario.Action.Invoke()

[<Collection("IntegrationDb")>]
[<Trait("Category", "Integration")>]
type UnitHandlingFeatureTests() =
    static member Scenarios() : seq<obj[]> =
        buildScenarioData "unit-handling" |> Seq.toList :> seq<_>

    [<Theory>]
    [<MemberData("Scenarios")>]
    member this.``Unit Handling``(scenario: Scenario) = scenario.Action.Invoke()

[<Collection("IntegrationDb")>]
[<Trait("Category", "Integration")>]
type ReportingFeatureTests() =
    static member Scenarios() : seq<obj[]> =
        buildScenarioData "reporting" |> Seq.toList :> seq<_>

    [<Theory>]
    [<MemberData("Scenarios")>]
    member this.``Reporting``(scenario: Scenario) = scenario.Action.Invoke()

[<Collection("IntegrationDb")>]
[<Trait("Category", "Integration")>]
type AttachmentsFeatureTests() =
    static member Scenarios() : seq<obj[]> =
        buildScenarioData "attachments" |> Seq.toList :> seq<_>

    [<Theory>]
    [<MemberData("Scenarios")>]
    member this.``Attachments``(scenario: Scenario) = scenario.Action.Invoke()
