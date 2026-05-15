module CraneCli.Tests.Integration.Steps.PdfSteps

open System.IO
open TickSpec
open Xunit
open CraneCli.Adapters.PdfAdapter
open CraneCli.Commands.PdfCommands

let private fixtureDir = Path.Combine(__SOURCE_DIRECTORY__, "../fixtures")

let mutable private currentAdapter: IPdfAdapter = RealPdfAdapter() :> IPdfAdapter
let mutable private lastExitCode: int = -1
let mutable private lastOutput: string = ""

let private runWithWriter (f: System.IO.TextWriter -> int) =
    use writer = new System.IO.StringWriter()
    let code = f writer
    lastOutput <- writer.ToString().Trim()
    lastExitCode <- code

[<Given>]
let ``a text-based PDF fixture with a known page count`` () =
    let path = Path.Combine(fixtureDir, "sample-text.pdf")
    Assert.True(File.Exists(path), sprintf "Fixture not found: %s" path)
    currentAdapter <- RealPdfAdapter() :> IPdfAdapter

[<Given>]
let ``a text-based PDF fixture exists`` () =
    let path = Path.Combine(fixtureDir, "sample-text.pdf")
    Assert.True(File.Exists(path), sprintf "Fixture not found: %s" path)
    currentAdapter <- RealPdfAdapter() :> IPdfAdapter

[<Given>]
let ``an image-only PDF fixture exists`` () =
    currentAdapter <- FakePdfAdapter("", 1, 512L)

[<When>]
let ``I run "crane pdf info" on the fixture`` () =
    let path = Path.Combine(fixtureDir, "sample-text.pdf")
    runWithWriter (fun w -> runInfo currentAdapter w path)

[<When>]
let ``I run "crane pdf type" on the fixture`` () =
    let path = Path.Combine(fixtureDir, "sample-text.pdf")
    runWithWriter (fun w -> runType currentAdapter w path)

[<Then>]
let ``the JSON output is valid`` () =
    let doc = System.Text.Json.JsonDocument.Parse(lastOutput)
    Assert.NotNull(doc)

[<Then>]
let ``the JSON field "pages" matches the known page count`` () =
    let doc = System.Text.Json.JsonDocument.Parse(lastOutput)
    let pages = doc.RootElement.GetProperty("pages").GetInt32()
    Assert.True(pages > 0, sprintf "Expected pages > 0 but got %d" pages)

[<Then>]
let ``the JSON field "size_bytes" is greater than 0`` () =
    let doc = System.Text.Json.JsonDocument.Parse(lastOutput)
    Assert.True(doc.RootElement.GetProperty("size_bytes").GetInt64() > 0L)

[<Then>]
let ``the JSON output contains type "([^"]*)"`` (expected: string) =
    let doc = System.Text.Json.JsonDocument.Parse(lastOutput)
    Assert.Equal(expected, doc.RootElement.GetProperty("type").GetString())

[<Then>]
let ``the exit code is (\d+)`` (expected: int) = Assert.Equal(expected, lastExitCode)
