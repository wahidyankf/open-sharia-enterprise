module CraneCli.Core.ReportManager

open System
open System.IO

let private chainWindowSeconds = 30L
let private utc7Offset = TimeSpan.FromHours(7.0)

let getOrExtendChain (scope: string) : string =
    let chainFile = sprintf ".execution-chain-%s" scope
    let newId = Guid.NewGuid().ToString("N").Substring(0, 6)

    let existingChain =
        if File.Exists(chainFile) then
            let parts = File.ReadAllText(chainFile).Trim().Split(' ', 2)

            if parts.Length = 2 then
                match Int64.TryParse(parts.[0]) with
                | true, ts when DateTimeOffset.UtcNow.ToUnixTimeSeconds() - ts < chainWindowSeconds ->
                    Some(parts.[1] + "__" + newId)
                | _ -> None
            else
                None
        else
            None

    let chain = existingChain |> Option.defaultValue newId
    File.WriteAllText(chainFile, sprintf "%d %s" (DateTimeOffset.UtcNow.ToUnixTimeSeconds()) chain)
    chain

let utc7Timestamp () : string =
    DateTimeOffset.UtcNow.ToOffset(utc7Offset).ToString("yyyy-MM-dd--HH-mm")

let initReport (scope: string) (pdf: string) (md: string) : Result<string, string> =
    try
        let chain = getOrExtendChain scope
        let ts = utc7Timestamp ()
        let reportPath = sprintf "generated-reports/%s__%s__%s__audit.md" scope chain ts
        let dir = Path.GetDirectoryName(reportPath)

        if not (Directory.Exists(dir)) then
            Directory.CreateDirectory(dir) |> ignore

        let header =
            sprintf "# Audit Report\n\nScope: %s\nPDF: %s\nMD: %s\nStatus: IN_PROGRESS\n" scope pdf md

        File.WriteAllText(reportPath, header)
        Ok reportPath
    with ex ->
        Error(sprintf "Failed to init report: %s" ex.Message)

let finalizeReport (reportPath: string) (status: string) : Result<unit, string> =
    try
        if not (File.Exists(reportPath)) then
            Error(sprintf "Report not found: %s" reportPath)
        else
            let content = File.ReadAllText(reportPath)
            let updated = content.Replace("Status: IN_PROGRESS", sprintf "Status: %s" status)
            File.WriteAllText(reportPath, updated)
            Ok()
    with ex ->
        Error(sprintf "Failed to finalize report: %s" ex.Message)
