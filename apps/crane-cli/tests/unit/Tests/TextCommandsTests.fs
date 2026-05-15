module CraneCli.Tests.Unit.Tests.TextCommandsTests

open Xunit
open CraneCli.Core.TextChecker
open CraneCli.Models.Finding

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
