module CraneCli.Commands.TextCommands

open System.Text.Json
open System.Text.Json.Serialization
open CraneCli.Adapters.PdfAdapter
open CraneCli.Core.TextChecker

let private jsonOptions =
    let opts = JsonSerializerOptions()
    opts.WriteIndented <- false
    opts.DefaultIgnoreCondition <- JsonIgnoreCondition.WhenWritingNull
    opts

let runCheck (adapter: IPdfAdapter) (pdfPath: string) (mdText: string) (output: System.IO.TextWriter) =
    match adapter.SampleText(pdfPath, 999) with
    | Ok pdfText ->
        let chunks =
            pdfText.Split([| '\n' |], System.StringSplitOptions.RemoveEmptyEntries)
            |> Array.filter (fun s -> s.Trim().Length > 10)
            |> Array.toList

        let findings = checkText chunks mdText
        output.WriteLine(JsonSerializer.Serialize(findings, jsonOptions))
        if findings.IsEmpty then 0 else 1
    | Error msg ->
        eprintfn "Error: %s" msg
        1

let runSearch (mdText: string) (segment: string) (output: System.IO.TextWriter) =
    let found = segmentIsPresent segment mdText

    output.WriteLine(
        JsonSerializer.Serialize(
            {| found = found
               similarity = computeSimilarity segment mdText |},
            jsonOptions
        )
    )

    if found then 0 else 1

[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage>]
let check (pdfPath: string) (mdPath: string) =
    let mdText = System.IO.File.ReadAllText(mdPath)
    runCheck (RealPdfAdapter()) pdfPath mdText System.Console.Out

[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage>]
let search (mdPath: string) (segment: string) =
    let mdText = System.IO.File.ReadAllText(mdPath)
    runSearch mdText segment System.Console.Out
