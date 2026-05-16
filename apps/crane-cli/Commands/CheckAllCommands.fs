module CraneCli.Commands.CheckAllCommands

open System.Text.Json
open System.Text.Json.Serialization
open CraneCli.Adapters.PdfAdapter
open CraneCli.Core.TextChecker
open CraneCli.Core.HeadingChecker
open CraneCli.Core.NestingChecker
open CraneCli.Core.TableChecker
open CraneCli.Core.FigureChecker
open CraneCli.Core.MermaidValidator

let private jsonOptions =
    let opts = JsonSerializerOptions()
    opts.WriteIndented <- false
    opts.DefaultIgnoreCondition <- JsonIgnoreCondition.WhenWritingNull
    opts

let private toChunks (pdfText: string) =
    pdfText.Split([| '\n' |], System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.filter (fun s -> s.Trim().Length > 10)
    |> Array.toList

let runCheckAll (adapter: IPdfAdapter) (pdfPath: string) (mdText: string) (output: System.IO.TextWriter) =
    match adapter.SampleText(pdfPath, 999) with
    | Ok pdfText ->
        let chunks = toChunks pdfText

        let findings =
            [ yield! checkText chunks mdText
              yield! checkHeadings pdfText mdText
              yield! checkNesting pdfText mdText
              yield! checkTables pdfText mdText
              yield! checkFigures pdfText mdText
              yield! validateMd mdText ]

        output.WriteLine(JsonSerializer.Serialize(findings, jsonOptions))
        if findings.IsEmpty then 0 else 1
    | Error msg ->
        eprintfn "Error: %s" msg
        1

[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage>]
let checkAll (pdfPath: string) (mdPath: string) (cacheDir: string option) =
    let baseAdapter = RealPdfAdapter() :> IPdfAdapter

    let adapter =
        match cacheDir with
        | Some dir -> CraneCli.Core.PdfExtractionCache.wrap baseAdapter dir
        | None -> baseAdapter

    let mdText = System.IO.File.ReadAllText(mdPath)
    runCheckAll adapter pdfPath mdText System.Console.Out
