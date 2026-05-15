module CraneCli.Tests.Unit.Steps.HeadingSteps

open TickSpec
open Xunit
open CraneCli.Commands.HeadingCommands
open CraneCli.Tests.Unit.Steps.BddState

// ---- BDD shared state ----
let mutable private pdfText: string = ""
let mutable private mdText: string = ""
let mutable private inferText: string = ""

// ---- BDD Given steps ----

[<Given>]
let ``a PDF fixture where heading "([^"]*)" implies depth (\d+)`` (heading: string) (_depth: int) = pdfText <- heading

[<Given>]
let ``the Markdown has that heading at depth (\d+)`` (depth: int) =
    let hashes = System.String('#', depth)
    mdText <- sprintf "%s Title" hashes

[<Given>]
let ``the text "([^"]*)"`` (text: string) = inferText <- text

// ---- BDD When steps ----

[<When>]
let ``I run "crane heading check" on the pair`` () =
    RunWithWriter(fun w -> runCheck pdfText mdText w)

[<When>]
let ``I run "crane heading infer" on that text`` () =
    RunWithWriter(fun w -> runInfer inferText w)

// ---- BDD Then steps ----

[<Then>]
let ``a finding with criticality "([^"]*)" is returned`` (expected: string) =
    let doc = System.Text.Json.JsonDocument.Parse(LastOutput)
    Assert.Equal(System.Text.Json.JsonValueKind.Array, doc.RootElement.ValueKind)
    Assert.True(doc.RootElement.GetArrayLength() > 0)
    let first = doc.RootElement.EnumerateArray() |> Seq.head
    Assert.Equal(expected, first.GetProperty("criticality").GetString())

[<Then>]
let ``the finding states expected_depth (\d+) and found_depth (\d+)`` (expectedDepth: int) (foundDepth: int) =
    let doc = System.Text.Json.JsonDocument.Parse(LastOutput)
    let first = doc.RootElement.EnumerateArray() |> Seq.head
    let description = first.GetProperty("description").GetString()
    Assert.Contains(sprintf "H%d" expectedDepth, description)
    Assert.Contains(sprintf "H%d" foundDepth, description)

[<Then>]
let ``the JSON output shows depth (\d+) and confidence "([^"]*)"`` (depth: int) (confidence: string) =
    let doc = System.Text.Json.JsonDocument.Parse(LastOutput)
    Assert.Equal(depth, doc.RootElement.GetProperty("depth").GetInt32())
    Assert.Equal(confidence, doc.RootElement.GetProperty("confidence").GetString())
