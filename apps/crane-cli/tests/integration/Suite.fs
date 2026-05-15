module CraneCli.Tests.Integration.Suite

open System.IO
open System.Reflection
open TickSpec
open Xunit

let private assembly = Assembly.GetExecutingAssembly()

let private gherkinRoot =
    match System.Environment.GetEnvironmentVariable("GHERKIN_ROOT") with
    | null -> Path.Combine(__SOURCE_DIRECTORY__, "../../../../specs/apps/crane/gherkin")
    | root -> root

let private buildScenarioData () : seq<obj[]> =
    if Directory.Exists(gherkinRoot) then
        let files =
            Directory.GetFiles(gherkinRoot, "*.feature", SearchOption.AllDirectories)

        let defs = StepDefinitions(assembly)

        files
        |> Seq.collect (fun path ->
            try
                let feature = defs.GenerateFeature(path)
                feature.Scenarios |> Seq.map (fun scenario -> [| scenario :> obj |])
            with _ ->
                Seq.empty)
    else
        Seq.empty

type CraneCliIntegrationSuite() =
    static member Scenarios() : seq<obj[]> =
        buildScenarioData () |> Seq.toList :> seq<_>

    [<Theory>]
    [<MemberData("Scenarios")>]
    member _.``Crane integration scenarios``(scenario: Scenario) = scenario.Action.Invoke()
