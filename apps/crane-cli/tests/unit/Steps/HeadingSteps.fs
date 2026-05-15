module CraneCli.Tests.Unit.Steps.HeadingSteps

open TickSpec
open Xunit
open CraneCli.Core.HeadingChecker
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

// ---- xUnit [<Fact>] unit tests ----

[<Fact>]
let ``TestUnitInferDepth_SingleNumber`` () =
    match inferDepthFromNumbering "1. Title" with
    | Some(depth, _) -> Assert.Equal(2, depth)
    | None -> Assert.Fail("Expected Some depth")

[<Fact>]
let ``TestUnitInferDepth_TwoComponents`` () =
    match inferDepthFromNumbering "2.3 Title" with
    | Some(depth, _) -> Assert.Equal(3, depth)
    | None -> Assert.Fail("Expected Some depth")

[<Fact>]
let ``TestUnitInferDepth_ThreeComponents`` () =
    match inferDepthFromNumbering "2.3.1 Title" with
    | Some(depth, _) -> Assert.Equal(4, depth)
    | None -> Assert.Fail("Expected Some depth")

[<Fact>]
let ``TestUnitInferDepth_NoNumber`` () =
    let result = inferDepthFromNumbering "Introduction"
    Assert.Equal(None, result)

[<Fact>]
let ``TestUnitWrongDepth_IsHigh`` () =
    // 2.3.1 → depth 4; ## Overview → H2, diff = 2 → HIGH
    let pdf2 = "2.3.1 Overview"
    let md2 = "## Overview"
    let findings2 = checkHeadings pdf2 md2
    Assert.NotEmpty(findings2)
    Assert.Equal("HIGH", (List.head findings2).Criticality)

[<Fact>]
let ``TestUnitCorrectDepth_NoFinding`` () =
    // 2.3 → depth 3; ### Overview → H3 → no finding
    let pdf = "2.3 Overview"
    let md = "### Overview"
    let findings = checkHeadings pdf md
    Assert.Empty(findings)

[<Fact>]
let ``HeadingCommands_runInfer_NoSectionNumber_ReturnsNoneConfidence`` () =
    use sw = new System.IO.StringWriter()
    let code = runInfer "Introduction" sw
    let json = System.Text.Json.JsonDocument.Parse(sw.ToString().Trim())
    Assert.Equal(0, code)
    Assert.Equal("NONE", json.RootElement.GetProperty("confidence").GetString())
