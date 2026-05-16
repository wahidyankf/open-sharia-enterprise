module CraneCli.Commands.TableCommands

open System.Text.Json
open System.Text.Json.Serialization
open CraneCli.Adapters.PdfAdapter
open CraneCli.Core.TableChecker

let private jsonOptions =
    let opts = JsonSerializerOptions()
    opts.WriteIndented <- false
    opts.DefaultIgnoreCondition <- JsonIgnoreCondition.WhenWritingNull
    opts

let runDetect (text: string) (output: System.IO.TextWriter) =
    let tables = detectTables text
    output.WriteLine(JsonSerializer.Serialize(tables, jsonOptions))
    0

let runCheck (pdfText: string) (mdText: string) (output: System.IO.TextWriter) =
    let findings = checkTables pdfText mdText
    output.WriteLine(JsonSerializer.Serialize(findings, jsonOptions))
    if findings.IsEmpty then 0 else 1

[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage>]
let detect (pdfPath: string) =
    let adapter = RealPdfAdapter() :> IPdfAdapter

    match adapter.SampleText(pdfPath, 999) with
    | Ok pdfText -> runDetect pdfText System.Console.Out
    | Error msg ->
        eprintfn "Error: %s" msg
        1

[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage>]
let check (pdfPath: string) (mdPath: string) =
    let adapter = RealPdfAdapter() :> IPdfAdapter

    match adapter.SampleText(pdfPath, 999) with
    | Ok pdfText ->
        let mdText = System.IO.File.ReadAllText(mdPath)
        runCheck pdfText mdText System.Console.Out
    | Error msg ->
        eprintfn "Error: %s" msg
        1
