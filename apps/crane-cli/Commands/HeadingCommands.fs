module CraneCli.Commands.HeadingCommands

open System.Text.Json
open System.Text.Json.Serialization
open CraneCli.Adapters.PdfAdapter
open CraneCli.Core.HeadingChecker

let private jsonOptions =
    let opts = JsonSerializerOptions()
    opts.WriteIndented <- false
    opts.DefaultIgnoreCondition <- JsonIgnoreCondition.WhenWritingNull
    opts

let runInfer (text: string) (output: System.IO.TextWriter) =
    match inferDepthFromNumbering text with
    | Some(depth, confidence) ->
        output.WriteLine(
            JsonSerializer.Serialize(
                {| depth = depth
                   confidence = confidence |},
                jsonOptions
            )
        )

        0
    | None ->
        output.WriteLine(
            JsonSerializer.Serialize(
                {| depth = (None: int option)
                   confidence = "NONE" |},
                jsonOptions
            )
        )

        0

let runCheck (pdfText: string) (mdText: string) (output: System.IO.TextWriter) =
    let findings = checkHeadings pdfText mdText
    output.WriteLine(JsonSerializer.Serialize(findings, jsonOptions))
    if findings.IsEmpty then 0 else 1

[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage>]
let infer (text: string) = runInfer text System.Console.Out

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
