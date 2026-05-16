module CraneCli.Core.PdfExtractionCache

open System
open System.IO
open System.Security.Cryptography
open System.Text.Json
open CraneCli.Adapters.PdfAdapter

[<Literal>]
let private CacheSubdir = "extract"

let private pdfSha256 (path: string) : string =
    use stream = File.OpenRead(path)
    let hash = SHA256.HashData(stream)
    BitConverter.ToString(hash).Replace("-", "").ToLowerInvariant()

let private cacheEntryPath (cacheDir: string) (kind: string) (sha: string) =
    Path.Combine(cacheDir, CacheSubdir, sprintf "%s-%s.json" kind (sha.Substring(0, 16)))

type private CachedExtraction =
    { [<System.Text.Json.Serialization.JsonPropertyName("pdfSha")>]
      PdfSha: string
      [<System.Text.Json.Serialization.JsonPropertyName("kind")>]
      Kind: string
      [<System.Text.Json.Serialization.JsonPropertyName("extractedAt")>]
      ExtractedAt: string
      [<System.Text.Json.Serialization.JsonPropertyName("fullText")>]
      FullText: string }

let private tryRead (path: string) : string option =
    if File.Exists(path) then
        try
            let json = File.ReadAllText(path)

            match JsonSerializer.Deserialize<CachedExtraction>(json) with
            | null -> None
            | entry -> Some entry.FullText
        with _ ->
            None
    else
        None

let private writeAtomic (path: string) (sha: string) (kind: string) (text: string) =
    match Path.GetDirectoryName(path) with
    | null
    | "" -> ()
    | dir when not (Directory.Exists dir) -> Directory.CreateDirectory(dir) |> ignore
    | _ -> ()

    let entry =
        { PdfSha = sha
          Kind = kind
          ExtractedAt = DateTime.UtcNow.ToString("o")
          FullText = text }

    let tempPath = path + ".tmp"
    let json = JsonSerializer.Serialize(entry)
    File.WriteAllText(tempPath, json)
    File.Move(tempPath, path, overwrite = true)

type private CachingAdapter(inner: IPdfAdapter, cacheDir: string) =
    let cachedSampleText (path: string) (pageCount: int) =
        try
            let sha = pdfSha256 path
            let kind = sprintf "sample-%d" pageCount
            let cachePath = cacheEntryPath cacheDir kind sha

            match tryRead cachePath with
            | Some text -> Ok text
            | None ->
                match inner.SampleText(path, pageCount) with
                | Ok text ->
                    try
                        writeAtomic cachePath sha kind text
                    with _ ->
                        ()

                    Ok text
                | Error msg -> Error msg
        with ex ->
            inner.SampleText(path, pageCount)
            |> Result.mapError (fun innerMsg -> sprintf "Cache failed (%s), inner also failed: %s" ex.Message innerMsg)

    let cachedExtractPages (path: string) (startPage: int) (endPage: int) =
        try
            let sha = pdfSha256 path
            let kind = sprintf "pages-%d-%d" startPage endPage
            let cachePath = cacheEntryPath cacheDir kind sha

            match tryRead cachePath with
            | Some text -> Ok text
            | None ->
                match inner.ExtractPages(path, startPage, endPage) with
                | Ok text ->
                    try
                        writeAtomic cachePath sha kind text
                    with _ ->
                        ()

                    Ok text
                | Error msg -> Error msg
        with ex ->
            inner.ExtractPages(path, startPage, endPage)
            |> Result.mapError (fun innerMsg -> sprintf "Cache failed (%s), inner also failed: %s" ex.Message innerMsg)

    interface IPdfAdapter with
        member _.GetMetadata(path) = inner.GetMetadata(path)
        member _.SampleText(path, pageCount) = cachedSampleText path pageCount

        member _.ExtractPages(path, startPage, endPage) =
            cachedExtractPages path startPage endPage

let wrap (inner: IPdfAdapter) (cacheDir: string) : IPdfAdapter =
    CachingAdapter(inner, cacheDir) :> IPdfAdapter

let defaultCacheDir () =
    match Environment.GetEnvironmentVariable("XDG_CACHE_HOME") with
    | null
    | "" -> Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), ".cache", "crane")
    | xdg -> Path.Combine(xdg, "crane")
