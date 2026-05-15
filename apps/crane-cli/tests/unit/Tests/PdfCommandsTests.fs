module CraneCli.Tests.Unit.Tests.PdfCommandsTests

open Xunit
open CraneCli.Adapters.PdfAdapter
open CraneCli.Commands.PdfCommands

let private runWithWriter (f: System.IO.TextWriter -> int) : int * string =
    use writer = new System.IO.StringWriter()
    let code = f writer
    code, writer.ToString().Trim()

let private textAdapter =
    FakePdfAdapter("This document contains more than ten words for testing text detection", 3, 2048L) :> IPdfAdapter

let private imageAdapter = FakePdfAdapter("", 1, 512L) :> IPdfAdapter

let private errorAdapter =
    { new IPdfAdapter with
        member _.GetMetadata(_) = Error "test error from GetMetadata"
        member _.SampleText(_, _) = Error "test error from SampleText"
        member _.ExtractPages(_, _, _) = Error "test error from ExtractPages" }

[<Fact>]
let ``runInfo returns 0 and JSON on success`` () =
    let code, out = runWithWriter (fun w -> runInfo textAdapter w "test.pdf")
    Assert.Equal(0, code)
    let doc = System.Text.Json.JsonDocument.Parse(out)
    Assert.Equal(3, doc.RootElement.GetProperty("pages").GetInt32())
    Assert.Equal(2048L, doc.RootElement.GetProperty("size_bytes").GetInt64())

[<Fact>]
let ``runInfo returns 1 on adapter failure`` () =
    let code, _ = runWithWriter (fun w -> runInfo errorAdapter w "test.pdf")
    Assert.Equal(1, code)

[<Fact>]
let ``runType returns 0 for text-based PDF`` () =
    let code, out = runWithWriter (fun w -> runType textAdapter w "test.pdf")
    Assert.Equal(0, code)
    let doc = System.Text.Json.JsonDocument.Parse(out)
    Assert.Equal("text", doc.RootElement.GetProperty("type").GetString())

[<Fact>]
let ``runType returns 1 for image-based PDF`` () =
    let code, out = runWithWriter (fun w -> runType imageAdapter w "test.pdf")
    Assert.Equal(1, code)
    let doc = System.Text.Json.JsonDocument.Parse(out)
    Assert.Equal("image", doc.RootElement.GetProperty("type").GetString())

[<Fact>]
let ``runType returns 1 on adapter failure`` () =
    let code, _ = runWithWriter (fun w -> runType errorAdapter w "test.pdf")
    Assert.Equal(1, code)

[<Fact>]
let ``runExtract returns 0 and writes text to writer when no output path`` () =
    let code, out =
        runWithWriter (fun w -> runExtract textAdapter w "test.pdf" 1 3 None)

    Assert.Equal(0, code)
    Assert.NotEmpty(out)

[<Fact>]
let ``runExtract returns 0 and writes to file when output path specified`` () =
    let tempFile = System.IO.Path.GetTempFileName()

    try
        use writer = new System.IO.StringWriter()

        let code = runExtract textAdapter writer "test.pdf" 1 3 (Some tempFile)

        Assert.Equal(0, code)
        Assert.True(System.IO.File.Exists(tempFile))
        let content = System.IO.File.ReadAllText(tempFile)
        Assert.NotEmpty(content)
    finally
        if System.IO.File.Exists(tempFile) then
            System.IO.File.Delete(tempFile)

[<Fact>]
let ``runExtract returns 1 on adapter failure`` () =
    let code, _ = runWithWriter (fun w -> runExtract errorAdapter w "test.pdf" 1 3 None)

    Assert.Equal(1, code)

[<Fact>]
let ``FakePdfAdapter SampleText returns configured text`` () =
    let adapter = FakePdfAdapter("hello world text", 2, 1024L)
    let result = (adapter :> IPdfAdapter).SampleText("any.pdf", 2)
    Assert.Equal(Ok "hello world text", result)

[<Fact>]
let ``FakePdfAdapter ExtractPages returns configured text`` () =
    let adapter = FakePdfAdapter("extracted content", 5, 5000L)
    let result = (adapter :> IPdfAdapter).ExtractPages("any.pdf", 1, 3)
    Assert.Equal(Ok "extracted content", result)

[<Fact>]
let ``FakePdfAdapter GetMetadata returns correct metadata`` () =
    let adapter = FakePdfAdapter("text", 7, 8192L)
    let result = (adapter :> IPdfAdapter).GetMetadata("my-file.pdf")

    match result with
    | Ok meta ->
        Assert.Equal(7, meta.Pages)
        Assert.Equal(8192L, meta.SizeBytes)
        Assert.Equal("my-file.pdf", meta.File)
        Assert.Equal(Some "Fake Document", meta.Title)
        Assert.Equal(None, meta.Author)
    | Error msg -> Assert.Fail(sprintf "Expected Ok but got Error: %s" msg)
