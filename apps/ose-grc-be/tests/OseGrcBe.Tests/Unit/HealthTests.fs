module OseGrcBe.Tests.Unit.HealthTests

open Xunit
open TickSpec
open System.IO
open System.Reflection
open OseGrcBe.Tests.TestFixture

[<Fact>]
[<Trait("Category", "Unit")>]
let healthEndpointReturns200 () =
    use factory = new OseGrcBeFactory()
    let client = factory.CreateClient()
    let response = client.GetAsync("/api/v1/health").Result
    let body = response.Content.ReadAsStringAsync().Result
    Assert.Equal(System.Net.HttpStatusCode.OK, response.StatusCode)
    Assert.Contains("\"healthy\"", body)

let private assembly = Assembly.GetExecutingAssembly()

let private specsDir =
    let assemblyDir = Path.GetDirectoryName(assembly.Location)
    Path.Combine(assemblyDir, "specs")

let private buildScenarioData (namePart: string) : seq<obj[]> =
    if Directory.Exists(specsDir) then
        let files =
            Directory.GetFiles(specsDir, "*.feature", SearchOption.AllDirectories)
            |> Array.tryFind (fun f -> f.Contains(namePart))

        match files with
        | Some path ->
            let defs = StepDefinitions(assembly)
            let feature = defs.GenerateFeature(path)
            feature.Scenarios |> Seq.map (fun scenario -> [| scenario :> obj |])
        | None -> Seq.empty
    else
        Seq.empty

[<Trait("Category", "Unit")>]
type HealthFeatureTests() =
    static member Scenarios() : seq<obj[]> =
        buildScenarioData "health" |> Seq.toList :> seq<_>

    [<Theory>]
    [<MemberData("Scenarios")>]
    member _.``Health scenarios``(scenario: Scenario) = scenario.Action.Invoke()
