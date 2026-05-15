module CraneCli.Tests.Unit.Steps.TextSteps

open TickSpec
open Xunit
open CraneCli.Adapters.PdfAdapter
open CraneCli.Commands.TextCommands
open CraneCli.Tests.Unit.Steps.BddState

// ---- BDD shared state ----
let mutable private pdfText: string = ""
let mutable private mdText: string = ""

// ---- BDD Given steps ----

[<Given>]
let ``a PDF fixture and its complete Markdown pair`` () =
    pdfText <- "Hello world this is section one content here and more text"
    mdText <- "Hello world this is section one content here and more text"

[<Given>]
let ``a PDF fixture and a Markdown missing one section`` () =
    // Short chunk between 11-49 chars → CRITICAL when missing (normalize length < 50)
    pdfText <- "Missing section here"
    mdText <- "completely different content with no overlap at all"

[<Given>]
let ``a PDF with multiple consecutive spaces and its normalized Markdown`` () =
    pdfText <- "hello   world   text   content"
    mdText <- "hello world text content"

[<Given>]
let ``a PDF with "Organisation" and a Markdown with "Organization"`` () =
    pdfText <- "Organisation"
    mdText <- "Organization"

// ---- BDD When steps ----

[<When>]
let ``I run "crane text check" on the pair`` () =
    let fakeAdapter = FakePdfAdapter(pdfText, 1, 1024L) :> IPdfAdapter
    RunWithWriter(fun w -> runCheck fakeAdapter "fake.pdf" mdText w)

// ---- BDD Then steps ----

[<Then>]
let ``the JSON output is an empty array`` () =
    let doc = System.Text.Json.JsonDocument.Parse(LastOutput)
    Assert.Equal(System.Text.Json.JsonValueKind.Array, doc.RootElement.ValueKind)
    Assert.Equal(0, doc.RootElement.GetArrayLength())

[<Then>]
let ``the JSON output contains a finding`` () =
    let doc = System.Text.Json.JsonDocument.Parse(LastOutput)
    Assert.Equal(System.Text.Json.JsonValueKind.Array, doc.RootElement.ValueKind)
    Assert.True(doc.RootElement.GetArrayLength() > 0)

[<Then>]
let ``the finding criticality is "([^"]*)"`` (expected: string) =
    let doc = System.Text.Json.JsonDocument.Parse(LastOutput)
    let first = doc.RootElement.EnumerateArray() |> Seq.head
    Assert.Equal(expected, first.GetProperty("criticality").GetString())

[<Then>]
let ``the finding category is "([^"]*)"`` (expected: string) =
    let doc = System.Text.Json.JsonDocument.Parse(LastOutput)
    let first = doc.RootElement.EnumerateArray() |> Seq.head
    Assert.Equal(expected, first.GetProperty("category").GetString())

[<Then>]
let ``no CRITICAL or HIGH finding is raised for that word`` () =
    let doc = System.Text.Json.JsonDocument.Parse(LastOutput)
    let findings = doc.RootElement.EnumerateArray() |> Seq.toList

    let hasCriticalOrHigh =
        findings
        |> List.exists (fun f ->
            let crit = f.GetProperty("criticality").GetString()
            crit = "CRITICAL" || crit = "HIGH")

    Assert.False(hasCriticalOrHigh)
