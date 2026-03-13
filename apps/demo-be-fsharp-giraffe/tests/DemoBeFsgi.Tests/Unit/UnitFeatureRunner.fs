module DemoBeFsgi.Tests.Unit.UnitFeatureRunner

open System
open System.IO
open System.Reflection
open TickSpec
open Xunit
open DemoBeFsgi.Tests.TestFixture
open DemoBeFsgi.Tests.State

/// Unit-level BDD runner.
///
/// Consumes the same shared Gherkin feature files as the integration runner but
/// marks every scenario with [Trait("Category", "Unit")] so they are picked up by
/// `dotnet test --filter Category=Unit`.
///
/// Uses a fresh SQLite in-memory AppDbContext per scenario (via createDb) so no
/// external services are required. Step definitions are shared with the integration
/// runner: all TickSpec [Given]/[When]/[Then] functions in the Integration.Steps.*
/// modules are discovered from the executing assembly and resolve identically here.

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

type private UnitScenarioServiceProvider(db: DemoBeFsgi.Infrastructure.AppDbContext.AppDbContext) =
    interface IServiceProvider with
        member _.GetService(serviceType: Type) =
            if serviceType = typeof<StepState> then empty db :> obj else null

/// Preserve inline '#' characters by replacing them with a temporary placeholder
/// before TickSpec's Gherkin parser strips them as comments.
let private preprocessFeatureLines (path: string) : string[] =
    File.ReadAllLines(path)
    |> Array.map (fun line ->
        let trimmed = line.TrimStart()

        if trimmed.StartsWith("#") then
            line
        else
            line.Replace("#", "HASH_SIGN"))

let private buildScenarioData (namePart: string) : seq<obj[]> =
    match getFeatureFile namePart with
    | Some path ->
        let defs = StepDefinitions(assembly)

        defs.ServiceProviderFactory <-
            fun () ->
                let db, _cleanup = createDb ()
                UnitScenarioServiceProvider(db) :> IServiceProvider

        let lines = preprocessFeatureLines path
        let feature = defs.GenerateFeature(path, lines)
        feature.Scenarios |> Seq.map (fun scenario -> [| scenario :> obj |])
    | None -> Seq.empty

[<Trait("Category", "Unit")>]
type UnitHealthFeatureTests() =
    static member Scenarios() : seq<obj[]> =
        buildScenarioData "health-check" |> Seq.toList :> seq<_>

    [<Theory>]
    [<MemberData("Scenarios")>]
    member _.``Health Check (unit)``(scenario: Scenario) = scenario.Action.Invoke()

[<Trait("Category", "Unit")>]
type UnitRegistrationFeatureTests() =
    static member Scenarios() : seq<obj[]> =
        buildScenarioData "registration" |> Seq.toList :> seq<_>

    [<Theory>]
    [<MemberData("Scenarios")>]
    member _.``Registration (unit)``(scenario: Scenario) = scenario.Action.Invoke()

[<Trait("Category", "Unit")>]
type UnitPasswordLoginFeatureTests() =
    static member Scenarios() : seq<obj[]> =
        buildScenarioData "password-login" |> Seq.toList :> seq<_>

    [<Theory>]
    [<MemberData("Scenarios")>]
    member _.``Password Login (unit)``(scenario: Scenario) = scenario.Action.Invoke()

[<Trait("Category", "Unit")>]
type UnitTokenLifecycleFeatureTests() =
    static member Scenarios() : seq<obj[]> =
        buildScenarioData "token-lifecycle" |> Seq.toList :> seq<_>

    [<Theory>]
    [<MemberData("Scenarios")>]
    member _.``Token Lifecycle (unit)``(scenario: Scenario) = scenario.Action.Invoke()

[<Trait("Category", "Unit")>]
type UnitTokensFeatureTests() =
    static member Scenarios() : seq<obj[]> =
        buildScenarioData "tokens" |> Seq.toList :> seq<_>

    [<Theory>]
    [<MemberData("Scenarios")>]
    member _.``Tokens (unit)``(scenario: Scenario) = scenario.Action.Invoke()

[<Trait("Category", "Unit")>]
type UnitUserAccountFeatureTests() =
    static member Scenarios() : seq<obj[]> =
        buildScenarioData "user-account" |> Seq.toList :> seq<_>

    [<Theory>]
    [<MemberData("Scenarios")>]
    member _.``User Account (unit)``(scenario: Scenario) = scenario.Action.Invoke()

[<Trait("Category", "Unit")>]
type UnitSecurityFeatureTests() =
    static member Scenarios() : seq<obj[]> =
        buildScenarioData "security" |> Seq.toList :> seq<_>

    [<Theory>]
    [<MemberData("Scenarios")>]
    member _.``Security (unit)``(scenario: Scenario) = scenario.Action.Invoke()

[<Trait("Category", "Unit")>]
type UnitAdminFeatureTests() =
    static member Scenarios() : seq<obj[]> =
        buildScenarioData "admin" |> Seq.toList :> seq<_>

    [<Theory>]
    [<MemberData("Scenarios")>]
    member _.``Admin (unit)``(scenario: Scenario) = scenario.Action.Invoke()

[<Trait("Category", "Unit")>]
type UnitExpenseManagementFeatureTests() =
    static member Scenarios() : seq<obj[]> =
        buildScenarioData "expense-management" |> Seq.toList :> seq<_>

    [<Theory>]
    [<MemberData("Scenarios")>]
    member _.``Expense Management (unit)``(scenario: Scenario) = scenario.Action.Invoke()

[<Trait("Category", "Unit")>]
type UnitCurrencyHandlingFeatureTests() =
    static member Scenarios() : seq<obj[]> =
        buildScenarioData "currency-handling" |> Seq.toList :> seq<_>

    [<Theory>]
    [<MemberData("Scenarios")>]
    member _.``Currency Handling (unit)``(scenario: Scenario) = scenario.Action.Invoke()

[<Trait("Category", "Unit")>]
type UnitUnitHandlingFeatureTests() =
    static member Scenarios() : seq<obj[]> =
        buildScenarioData "unit-handling" |> Seq.toList :> seq<_>

    [<Theory>]
    [<MemberData("Scenarios")>]
    member _.``Unit Handling (unit)``(scenario: Scenario) = scenario.Action.Invoke()

[<Trait("Category", "Unit")>]
type UnitReportingFeatureTests() =
    static member Scenarios() : seq<obj[]> =
        buildScenarioData "reporting" |> Seq.toList :> seq<_>

    [<Theory>]
    [<MemberData("Scenarios")>]
    member _.``Reporting (unit)``(scenario: Scenario) = scenario.Action.Invoke()

[<Trait("Category", "Unit")>]
type UnitAttachmentsFeatureTests() =
    static member Scenarios() : seq<obj[]> =
        buildScenarioData "attachments" |> Seq.toList :> seq<_>

    [<Theory>]
    [<MemberData("Scenarios")>]
    member _.``Attachments (unit)``(scenario: Scenario) = scenario.Action.Invoke()
