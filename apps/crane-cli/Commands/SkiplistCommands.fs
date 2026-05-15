module CraneCli.Commands.SkiplistCommands

open System.Text.Json
open System.Text.Json.Serialization
open CraneCli.Core.SkiplistManager

let private jsonOptions =
    let opts = JsonSerializerOptions()
    opts.WriteIndented <- false
    opts.DefaultIgnoreCondition <- JsonIgnoreCondition.WhenWritingNull
    opts

let runAdd (mdBasename: string) (category: string) (description: string) (output: System.IO.TextWriter) =
    match add mdBasename category description with
    | Ok added ->
        output.WriteLine(JsonSerializer.Serialize({| added = added |}, jsonOptions))
        0
    | Error msg ->
        eprintfn "Error: %s" msg
        1

let runCheck (mdBasename: string) (category: string) (description: string) (output: System.IO.TextWriter) =
    match check mdBasename category description with
    | Ok found ->
        output.WriteLine(JsonSerializer.Serialize({| ``match`` = found |}, jsonOptions))

        if found then 0 else 1
    | Error msg ->
        eprintfn "Error: %s" msg
        1

let runList (mdBasename: string) (output: System.IO.TextWriter) =
    match list mdBasename with
    | Ok entries ->
        output.WriteLine(JsonSerializer.Serialize(entries, jsonOptions))
        0
    | Error msg ->
        eprintfn "Error: %s" msg
        1

[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage>]
let add' (mdBasename: string) (category: string) (description: string) =
    runAdd mdBasename category description System.Console.Out

[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage>]
let check' (mdBasename: string) (category: string) (description: string) =
    runCheck mdBasename category description System.Console.Out

[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage>]
let list' (mdBasename: string) = runList mdBasename System.Console.Out
