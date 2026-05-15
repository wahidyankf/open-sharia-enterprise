module CraneCli.Commands.FigureCommands

open System.Text.Json
open System.Text.Json.Serialization
open CraneCli.Core.FigureChecker

let private jsonOptions =
    let opts = JsonSerializerOptions()
    opts.WriteIndented <- false
    opts.DefaultIgnoreCondition <- JsonIgnoreCondition.WhenWritingNull
    opts

let runDetect (text: string) (output: System.IO.TextWriter) =
    let figures = detectFigures text
    output.WriteLine(JsonSerializer.Serialize(figures, jsonOptions))
    0

let runCheck (pdfText: string) (mdText: string) (output: System.IO.TextWriter) =
    let findings = checkFigures pdfText mdText
    output.WriteLine(JsonSerializer.Serialize(findings, jsonOptions))
    if findings.IsEmpty then 0 else 1

[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage>]
let detect (pdfPath: string) =
    let text = System.IO.File.ReadAllText(pdfPath)
    runDetect text System.Console.Out

[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage>]
let check (pdfPath: string) (mdPath: string) =
    let pdfText = System.IO.File.ReadAllText(pdfPath)
    let mdText = System.IO.File.ReadAllText(mdPath)
    runCheck pdfText mdText System.Console.Out
