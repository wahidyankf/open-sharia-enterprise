module CraneCli.Commands.PdfCommands

open System
open System.IO
open System.Text.Json
open System.Text.Json.Serialization
open CraneCli.Adapters.PdfAdapter

let private jsonOptions =
    let opts = JsonSerializerOptions()
    opts.WriteIndented <- false
    opts.DefaultIgnoreCondition <- JsonIgnoreCondition.WhenWritingNull
    opts

let private outputJson (writer: TextWriter) value =
    writer.WriteLine(JsonSerializer.Serialize(value, jsonOptions))

let runInfo (adapter: IPdfAdapter) (writer: TextWriter) (path: string) =
    match adapter.GetMetadata(path) with
    | Ok meta ->
        outputJson writer meta
        0
    | Error msg ->
        writer.Flush()
        eprintfn "Error: %s" msg
        1

let runType (adapter: IPdfAdapter) (writer: TextWriter) (path: string) =
    match adapter.SampleText(path, 3) with
    | Ok text ->
        let wordCount =
            text.Split([| ' '; '\n'; '\t' |], StringSplitOptions.RemoveEmptyEntries).Length

        let docType = if wordCount > 10 then "text" else "image"
        outputJson writer {| ``type`` = docType |}
        if docType = "text" then 0 else 1
    | Error msg ->
        writer.Flush()
        eprintfn "Error: %s" msg
        1

let runExtract
    (adapter: IPdfAdapter)
    (writer: TextWriter)
    (path: string)
    (startPage: int)
    (endPage: int)
    (output: string option)
    =
    match adapter.ExtractPages(path, startPage, endPage) with
    | Ok text ->
        match output with
        | Some outPath -> File.WriteAllText(outPath, text)
        | None -> writer.WriteLine(text)

        0
    | Error msg ->
        writer.Flush()
        eprintfn "Error: %s" msg
        1
