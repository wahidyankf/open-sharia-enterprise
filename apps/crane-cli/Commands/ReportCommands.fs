module CraneCli.Commands.ReportCommands

open System.Text.Json
open System.Text.Json.Serialization
open CraneCli.Core.ReportManager

let private jsonOptions =
    let opts = JsonSerializerOptions()
    opts.WriteIndented <- false
    opts.DefaultIgnoreCondition <- JsonIgnoreCondition.WhenWritingNull
    opts

let runInit (scope: string) (pdf: string) (md: string) (output: System.IO.TextWriter) =
    match initReport scope pdf md with
    | Ok path ->
        output.WriteLine(JsonSerializer.Serialize({| path = path |}, jsonOptions))
        0
    | Error msg ->
        eprintfn "Error: %s" msg
        1

let runFinalize (reportPath: string) (status: string) (output: System.IO.TextWriter) =
    match finalizeReport reportPath status with
    | Ok() ->
        output.WriteLine(JsonSerializer.Serialize({| status = status; path = reportPath |}, jsonOptions))
        0
    | Error msg ->
        eprintfn "Error: %s" msg
        1

[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage>]
let init (scope: string) (pdf: string) (md: string) = runInit scope pdf md System.Console.Out

[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage>]
let finalize (reportPath: string) (status: string) =
    runFinalize reportPath status System.Console.Out
