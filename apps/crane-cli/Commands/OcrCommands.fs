module CraneCli.Commands.OcrCommands

open System.Text.Json
open System.Text.Json.Serialization
open CraneCli.Core.OcrAssessor

let private jsonOptions =
    let opts = JsonSerializerOptions()
    opts.WriteIndented <- false
    opts.DefaultIgnoreCondition <- JsonIgnoreCondition.WhenWritingNull
    opts

let runQuality (mdText: string) (output: System.IO.TextWriter) =
    let findings = checkOCRQuality mdText
    output.WriteLine(JsonSerializer.Serialize(findings, jsonOptions))
    if findings.IsEmpty then 0 else 1

let runExtract (mdText: string) (output: System.IO.TextWriter) =
    let sections = extractOCRSections mdText
    output.WriteLine(JsonSerializer.Serialize(sections, jsonOptions))
    0

[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage>]
let quality (mdPath: string) =
    let mdText = System.IO.File.ReadAllText(mdPath)
    runQuality mdText System.Console.Out

[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage>]
let extract (mdPath: string) =
    let mdText = System.IO.File.ReadAllText(mdPath)
    runExtract mdText System.Console.Out
