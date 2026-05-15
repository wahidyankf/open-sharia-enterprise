module CraneCli.Commands.MermaidCommands

open System.Text.Json
open System.Text.Json.Serialization
open CraneCli.Core.MermaidValidator

let private jsonOptions =
    let opts = JsonSerializerOptions()
    opts.WriteIndented <- false
    opts.DefaultIgnoreCondition <- JsonIgnoreCondition.WhenWritingNull
    opts

let runValidate (mdText: string) (output: System.IO.TextWriter) =
    let findings = validateMd mdText
    output.WriteLine(JsonSerializer.Serialize(findings, jsonOptions))
    if findings.IsEmpty then 0 else 1

[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage>]
let validate (mdPath: string) =
    let mdText = System.IO.File.ReadAllText(mdPath)
    runValidate mdText System.Console.Out
