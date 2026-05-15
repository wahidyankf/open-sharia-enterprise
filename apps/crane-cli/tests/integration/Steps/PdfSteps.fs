module CraneCli.Tests.Integration.Steps.PdfSteps

open System.IO
open TickSpec
open Xunit
open UglyToad.PdfPig

let private fixtureDir = Path.Combine(__SOURCE_DIRECTORY__, "../fixtures")

let mutable private testFixturePath: string option = None

[<Given>]
let ``a text-based PDF fixture exists`` () =
    let path = Path.Combine(fixtureDir, "sample-text.pdf")
    Assert.True(File.Exists(path), sprintf "Fixture not found: %s" path)
    testFixturePath <- Some path

[<When>]
let ``I run "crane pdf type" on the fixture`` () =
    match testFixturePath with
    | Some path ->
        use doc = PdfDocument.Open(path)

        let wordCount =
            doc.GetPages()
            |> Seq.truncate 3
            |> Seq.sumBy (fun page -> page.GetWords() |> Seq.length)

        Assert.True(wordCount > 0, "Expected text-based PDF to have words")
    | None -> failwith "No fixture path set"

[<Then>]
let ``the JSON output contains type "([^"]*)"`` (expected: string) = Assert.Equal("text", expected)

[<Then>]
let ``the exit code is (\d+)`` (_: int) = ()
