module CraneCli.Core.SkiplistManager

open System
open System.IO
open System.Text.Json
open CraneCli.Models.Report

let private skiplistPath (mdBasename: string) =
    sprintf ".crane-skiplist-%s.json" mdBasename

let stableKey (mdBasename: string) (category: string) (description: string) : string =
    let combined = sprintf "%s|%s|%s" mdBasename category description
    let bytes = System.Text.Encoding.UTF8.GetBytes(combined)
    let hash = System.Security.Cryptography.SHA256.HashData(bytes)
    BitConverter.ToString(hash).[..15].Replace("-", "").ToLowerInvariant()

let private loadEntries (mdBasename: string) : SkipListEntry list =
    let path = skiplistPath mdBasename

    if File.Exists(path) then
        try
            JsonSerializer.Deserialize<SkipListEntry list>(File.ReadAllText(path))
        with _ ->
            []
    else
        []

let private saveEntries (mdBasename: string) (entries: SkipListEntry list) =
    let path = skiplistPath mdBasename
    let opts = JsonSerializerOptions()
    opts.WriteIndented <- true
    File.WriteAllText(path, JsonSerializer.Serialize(entries, opts))

let add (mdBasename: string) (category: string) (description: string) : Result<bool, string> =
    try
        let key = stableKey mdBasename category description
        let existing = loadEntries mdBasename

        if existing |> List.exists (fun e -> e.Key = key) then
            Ok false // duplicate
        else
            let entry =
                { MdBasename = mdBasename
                  Category = category
                  Description = description
                  Key = key }

            saveEntries mdBasename (existing @ [ entry ])
            Ok true
    with ex ->
        Error(sprintf "Failed to add entry: %s" ex.Message)

let check (mdBasename: string) (category: string) (description: string) : Result<bool, string> =
    try
        let key = stableKey mdBasename category description
        let existing = loadEntries mdBasename
        Ok(existing |> List.exists (fun e -> e.Key = key))
    with ex ->
        Error(sprintf "Failed to check entry: %s" ex.Message)

let list (mdBasename: string) : Result<SkipListEntry list, string> =
    try
        Ok(loadEntries mdBasename)
    with ex ->
        Error(sprintf "Failed to list entries: %s" ex.Message)
