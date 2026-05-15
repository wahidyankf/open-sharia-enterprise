module CraneCli.Tests.Unit.Steps.PdfSteps

open TickSpec
open Xunit
open CraneCli.Adapters.PdfAdapter
open CraneCli.Commands.PdfCommands
open CraneCli.Tests.Unit.Steps.BddState

let mutable private currentAdapter: IPdfAdapter =
    FakePdfAdapter("Sample text with many words for testing purposes right here and more", 5, 10240L)

[<Given>]
let ``a text-based PDF fixture with a known page count`` () =
    currentAdapter <-
        FakePdfAdapter("Sample text content with many words for testing purposes and more content here", 5, 10240L)

[<Given>]
let ``a text-based PDF fixture exists`` () =
    currentAdapter <-
        FakePdfAdapter("Sample text content with many words for testing purposes and more content here", 5, 10240L)

[<Given>]
let ``an image-only PDF fixture exists`` () =
    currentAdapter <- FakePdfAdapter("", 1, 512L)

[<When>]
let ``I run "crane pdf info" on the fixture`` () =
    RunWithWriter(fun w -> runInfo currentAdapter w "fake.pdf")

[<When>]
let ``I run "crane pdf type" on the fixture`` () =
    RunWithWriter(fun w -> runType currentAdapter w "fake.pdf")

[<Then>]
let ``the JSON output is valid`` () =
    let doc = System.Text.Json.JsonDocument.Parse(LastOutput)
    Assert.NotNull(doc)

[<Then>]
let ``the JSON field "pages" matches the known page count`` () =
    let doc = System.Text.Json.JsonDocument.Parse(LastOutput)
    Assert.Equal(5, doc.RootElement.GetProperty("pages").GetInt32())

[<Then>]
let ``the JSON field "size_bytes" is greater than 0`` () =
    let doc = System.Text.Json.JsonDocument.Parse(LastOutput)
    Assert.True(doc.RootElement.GetProperty("size_bytes").GetInt64() > 0L)

[<Then>]
let ``the JSON output contains type "([^"]*)"`` (expected: string) =
    let doc = System.Text.Json.JsonDocument.Parse(LastOutput)
    Assert.Equal(expected, doc.RootElement.GetProperty("type").GetString())

[<Then>]
let ``the exit code is (\d+)`` (expected: int) = Assert.Equal(expected, LastExitCode)
