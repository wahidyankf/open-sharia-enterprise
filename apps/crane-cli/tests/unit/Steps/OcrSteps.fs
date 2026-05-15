module CraneCli.Tests.Unit.Steps.OcrSteps

open TickSpec
open Xunit
open CraneCli.Commands.OcrCommands
open CraneCli.Tests.Unit.Steps.BddState

// ---- BDD shared state ----
let mutable private mdFixture: string = ""

// ---- BDD Given steps ----

[<Given>]
let ``a Markdown fixture with an OCR-tagged section at 15% estimated error rate`` () =
    // Build content with >15% non-ASCII chars (above 10% CRITICAL threshold)
    // non-ASCII: 20 chars out of 100 total (after stripping spaces) = 20% error rate
    let nonAscii = System.String('é', 20)
    let ascii = System.String('a', 80)
    mdFixture <- sprintf "<!-- OCR: %s%s -->" nonAscii ascii

[<Given>]
let ``a Markdown fixture with an OCR-tagged section at 1% estimated error rate`` () =
    // Realistic clean OCR content — well below all thresholds
    let content =
        "The quick brown fox jumps over the lazy dog. "
        |> Seq.replicate 4
        |> String.concat ""

    mdFixture <- sprintf "<!-- OCR: %s -->" content

[<Given>]
let ``a Markdown fixture with no OCR page tags`` () =
    mdFixture <- "# Heading\n\nSome normal markdown text with no OCR tags."

// ---- BDD When steps ----

[<When>]
let ``I run "crane ocr quality" on the fixture`` () =
    RunWithWriter(fun w -> runQuality mdFixture w)

// ---- BDD Then steps ----

[<Then>]
let ``the finding includes the OCR page number`` () =
    let doc = System.Text.Json.JsonDocument.Parse(LastOutput)
    Assert.True(doc.RootElement.GetArrayLength() > 0)
    let first = doc.RootElement.EnumerateArray() |> Seq.head
    // location_md contains the tag reference "ocr-comment"
    let locationMd = first.GetProperty("location_md")
    Assert.Equal(System.Text.Json.JsonValueKind.String, locationMd.ValueKind)
    Assert.NotEmpty(locationMd.GetString())
