module CraneCli.Tests.Unit.Tests.OcrCommandsTests

open Xunit
open CraneCli.Core.OcrAssessor
open CraneCli.Commands.OcrCommands

[<Fact>]
let ``TestUnitNoOcrTags_ReturnsEmpty`` () =
    let md = "# Heading\n\nSome text"
    Assert.Empty(checkOCRQuality md)

[<Fact>]
let ``TestUnitCleanOcr_ReturnsEmpty`` () =
    let content =
        "The quick brown fox jumps over the lazy dog. "
        |> Seq.replicate 4
        |> String.concat ""

    let md = sprintf "<!-- OCR: %s -->" content
    Assert.Empty(checkOCRQuality md)

[<Fact>]
let ``TestUnitHighErrorRate_IsCritical`` () =
    let nonAscii = System.String('é', 20)
    let ascii = System.String('a', 80)
    let md = sprintf "<!-- OCR: %s%s -->" nonAscii ascii
    let findings = checkOCRQuality md
    Assert.NotEmpty(findings)
    Assert.Equal("CRITICAL", (List.head findings).Criticality)

[<Fact>]
let ``TestUnitModerateErrorRate_IsHigh`` () =
    // ~7% error rate: 7 non-ASCII out of ~100 chars
    // Use realistic words so the [a-zA-Z]{30,} pattern does not trigger
    let words = "the quick fox jumped over lazy dogs "
    let content = words + "éééé " + words + "ééé " + words
    let md = sprintf "<!-- OCR: %s -->" content
    let findings = checkOCRQuality md
    Assert.NotEmpty(findings)
    Assert.Equal("HIGH", (List.head findings).Criticality)

[<Fact>]
let ``TestUnitExtractOcrSections_FindsTaggedContent`` () =
    let md = "<!-- OCR: some content here -->"
    let sections = extractOCRSections md
    Assert.Equal(1, sections.Length)
    Assert.Equal("ocr-comment", sections.[0].Tag)

[<Fact>]
let ``TestUnitExtractOcrSections_EmptyMarkdown_ReturnsEmpty`` () =
    let md = "# Just normal markdown"
    let sections = extractOCRSections md
    Assert.Empty(sections)

[<Fact>]
let ``TestUnitEstimateOcrRate_CleanText_IsZero`` () =
    let rate = estimateOCRErrorRate "hello world"
    Assert.Equal(0.0, rate)

[<Fact>]
let ``TestUnitOcrCommands_runExtract_ReturnsJson`` () =
    use sw = new System.IO.StringWriter()
    let code = runExtract "<!-- OCR: test content -->" sw
    let json = System.Text.Json.JsonDocument.Parse(sw.ToString().Trim())
    Assert.Equal(0, code)
    Assert.Equal(System.Text.Json.JsonValueKind.Array, json.RootElement.ValueKind)
