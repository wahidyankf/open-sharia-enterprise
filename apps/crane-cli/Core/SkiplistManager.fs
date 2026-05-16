module CraneCli.Core.SkiplistManager

open System
open System.IO
open System.Text
open CraneCli.Models.Report

[<Literal>]
let private DefaultPath = "generated-reports/.known-false-positives.md"

[<Literal>]
let private FalsePositivePrefix = "## FALSE_POSITIVE:"

[<Literal>]
let private DefaultReason = "Auto-accepted via crane skiplist --add"

/// Resolve the skip-list path. CRANE_SKIPLIST_PATH overrides for tests; otherwise
/// the canonical repo-wide global markdown file is used.
let resolveSkiplistPath () : string =
    match Environment.GetEnvironmentVariable("CRANE_SKIPLIST_PATH") with
    | null
    | "" -> DefaultPath
    | overridePath -> overridePath

let stableKey (mdBasename: string) (category: string) (description: string) : string =
    let combined = sprintf "%s|%s|%s" mdBasename category description
    let bytes = Encoding.UTF8.GetBytes(combined)
    let hash = System.Security.Cryptography.SHA256.HashData(bytes)
    BitConverter.ToString(hash).[..15].Replace("-", "").ToLowerInvariant()

let private nowTimestamp () =
    DateTime.Now.ToString("yyyy-MM-dd--HH-mm")

let private parseHeading (line: string) : (string * string * string) option =
    // Heading format: "## FALSE_POSITIVE: {category} | {md-basename} | {description}"
    let body = line.Substring(FalsePositivePrefix.Length).TrimStart()
    let parts = body.Split([| " | " |], StringSplitOptions.None)

    if parts.Length = 3 then
        Some(parts.[0].Trim(), parts.[1].Trim(), parts.[2].Trim())
    else
        None

let private parseMetadata (block: string list) : Map<string, string> =
    block
    |> List.choose (fun line ->
        let trimmed = line.TrimStart()

        if trimmed.StartsWith("**") then
            let endMarker = trimmed.IndexOf("**:", 2)

            if endMarker > 0 then
                let key = trimmed.Substring(2, endMarker - 2).Trim()
                let value = trimmed.Substring(endMarker + 3).Trim()
                Some(key, value)
            else
                None
        else
            None)
    |> Map.ofList

let private parseEntries (path: string) : SkipListEntry list =
    if not (File.Exists(path)) then
        []
    else
        let lines = File.ReadAllLines(path) |> Array.toList

        let rec walk (acc: SkipListEntry list) (remaining: string list) =
            match remaining with
            | [] -> List.rev acc
            | line :: rest when line.StartsWith(FalsePositivePrefix) ->
                // Capture metadata block until next "## " heading or "---" separator
                let metaLines =
                    rest |> List.takeWhile (fun l -> not (l.StartsWith("## ")) && l.Trim() <> "---")

                let nextRest = rest |> List.skip metaLines.Length

                match parseHeading line with
                | Some(category, mdBasename, description) ->
                    let meta = parseMetadata metaLines
                    let accepted = meta |> Map.tryFind "Accepted" |> Option.defaultValue ""
                    let reason = meta |> Map.tryFind "Reason" |> Option.defaultValue ""

                    let key =
                        meta
                        |> Map.tryFind "Key"
                        |> Option.defaultWith (fun () -> stableKey mdBasename category description)

                    let entry =
                        { MdBasename = mdBasename
                          Category = category
                          Description = description
                          Key = key
                          Accepted = accepted
                          Reason = reason }

                    walk (entry :: acc) nextRest
                | None ->
                    // Malformed heading — skip this section, continue parsing
                    eprintfn "Warning: skipping malformed FALSE_POSITIVE heading: %s" line
                    walk acc nextRest
            | _ :: rest -> walk acc rest

        walk [] lines

let private renderEntry (entry: SkipListEntry) : string =
    let sb = StringBuilder()

    sb
        .Append("## FALSE_POSITIVE: ")
        .Append(entry.Category)
        .Append(" | ")
        .Append(entry.MdBasename)
        .Append(" | ")
        .Append(entry.Description)
        .AppendLine()
        .AppendLine()
        .Append("**Accepted**: ")
        .Append(entry.Accepted)
        .AppendLine()
        .Append("**Category**: ")
        .Append(entry.Category)
        .AppendLine()
        .Append("**File**: ")
        .Append(entry.MdBasename)
        .AppendLine()
        .Append("**Finding**: ")
        .Append(entry.Description)
        .AppendLine()
        .Append("**Key**: ")
        .Append(entry.Key)
        .AppendLine()
        .Append("**Reason**: ")
        .Append(entry.Reason)
        .AppendLine()
        .AppendLine()
        .AppendLine("---")
        .AppendLine()
    |> ignore

    sb.ToString()

let private appendEntry (path: string) (entry: SkipListEntry) =
    let text = renderEntry entry

    if File.Exists(path) then
        let existing = File.ReadAllText(path)
        let needsBlankLine = not (existing.EndsWith("\n\n")) && existing.Length > 0
        let prefix = if needsBlankLine then "\n" else ""
        File.AppendAllText(path, prefix + text)
    else
        match path |> Path.GetDirectoryName |> Option.ofObj with
        | Some dir when dir.Length > 0 && not (Directory.Exists(dir)) -> Directory.CreateDirectory(dir) |> ignore
        | _ -> ()

        File.WriteAllText(path, text)

let add (mdBasename: string) (category: string) (description: string) : Result<bool, string> =
    try
        let path = resolveSkiplistPath ()
        let key = stableKey mdBasename category description
        let existing = parseEntries path

        if existing |> List.exists (fun e -> e.Key = key) then
            Ok false // duplicate
        else
            let entry =
                { MdBasename = mdBasename
                  Category = category
                  Description = description
                  Key = key
                  Accepted = nowTimestamp ()
                  Reason = DefaultReason }

            appendEntry path entry
            Ok true
    with ex ->
        Error(sprintf "Failed to add entry: %s" ex.Message)

let check (mdBasename: string) (category: string) (description: string) : Result<bool, string> =
    let path = resolveSkiplistPath ()
    let key = stableKey mdBasename category description
    let existing = parseEntries path
    Ok(existing |> List.exists (fun e -> e.Key = key))

let list (mdBasename: string) : Result<SkipListEntry list, string> =
    let path = resolveSkiplistPath ()
    let all = parseEntries path
    Ok(all |> List.filter (fun e -> e.MdBasename = mdBasename))
