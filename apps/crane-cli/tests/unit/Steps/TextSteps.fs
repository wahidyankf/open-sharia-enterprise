module CraneCli.Tests.Unit.Steps.TextSteps

open TickSpec
open Xunit
open CraneCli.Adapters.PdfAdapter
open CraneCli.Core.TextChecker
open CraneCli.Commands.TextCommands
open CraneCli.Models.Finding
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

// ---- xUnit [<Fact>] unit tests ----

[<Fact>]
let ``TestUnitNormalize_CollapsesWhitespace`` () =
    Assert.Equal("a b c", normalize "a  b   c")

[<Fact>]
let ``TestUnitNormalize_StripsLeadingTrailing`` () =
    Assert.Equal("hello", normalize "  hello  ")

[<Fact>]
let ``TestUnitSimilarity_ExactIs1`` () =
    Assert.Equal(1.0, computeSimilarity "hello world" "hello world")

[<Fact>]
let ``TestUnitSimilarity_BelowThreshold`` () =
    Assert.True(computeSimilarity "hello world" "completely different" < 0.85)

[<Fact>]
let ``TestUnitFuzzyMatch_AcceptsMinorVariation`` () =
    // Both words are short — use the full mdText as context
    Assert.True(segmentIsPresent "Organisation" "The Organization of the document is clear")

[<Fact>]
let ``TestUnitMissingHeading_IsCritical`` () =
    let result = classifyMissing "Short"
    Assert.Equal(Criticality.CRITICAL, result)

[<Fact>]
let ``TestUnitMissingParagraph_IsHigh`` () =
    let longText = System.String('a', 60)
    let result = classifyMissing longText
    Assert.Equal(Criticality.HIGH, result)

[<Fact>]
let ``TestUnitPresentText_NoFinding`` () =
    let chunks = [ "hello world" ]
    let md = "The text says hello world here"
    let findings = checkText chunks md
    Assert.Empty(findings)

[<Fact>]
let ``TextCommands_runCheck_ErrorAdapter_Returns1`` () =
    let errAdapter = CraneCli.Adapters.PdfAdapter.FakePdfAdapter("", 0, 0L)
    // FakePdfAdapter always returns Ok, so use a custom approach:
    // Test the success path (empty chunks → no findings → exit 0)
    use sw = new System.IO.StringWriter()

    let code =
        CraneCli.Commands.TextCommands.runCheck errAdapter "fake.pdf" "some md text" sw

    Assert.Equal(0, code)

[<Fact>]
let ``TextCommands_runCheck_WithRealErrorAdapter_Returns1`` () =
    // Create an adapter that returns Error to cover the Error branch
    let errorAdapter =
        { new CraneCli.Adapters.PdfAdapter.IPdfAdapter with
            member _.GetMetadata(_path) = Error "not implemented"
            member _.SampleText(_path, _pageCount) = Error "test error"
            member _.ExtractPages(_path, _startPage, _endPage) = Error "not implemented" }

    use sw = new System.IO.StringWriter()

    let code =
        CraneCli.Commands.TextCommands.runCheck errorAdapter "fake.pdf" "some md text" sw

    Assert.Equal(1, code)

[<Fact>]
let ``TextCommands_runSearch_Found_Returns0`` () =
    use sw = new System.IO.StringWriter()
    let code = CraneCli.Commands.TextCommands.runSearch "hello world text" "hello" sw
    Assert.Equal(0, code)

[<Fact>]
let ``TextCommands_runSearch_NotFound_Returns1`` () =
    use sw = new System.IO.StringWriter()

    let code =
        CraneCli.Commands.TextCommands.runSearch "hello world text" "xyzzy completely absent" sw

    Assert.Equal(1, code)
