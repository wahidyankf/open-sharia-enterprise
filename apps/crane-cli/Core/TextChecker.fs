module CraneCli.Core.TextChecker

open System
open System.Text.RegularExpressions
open F23.StringSimilarity
open CraneCli.Models.Finding

let private fuzzyThreshold = 0.85
let private wsPattern = Regex(@"\s+", RegexOptions.Compiled)
let private levenshtein = NormalizedLevenshtein()

let normalize (text: string) = wsPattern.Replace(text.Trim(), " ")

let computeSimilarity (a: string) (b: string) =
    let na = (normalize a).ToLowerInvariant()
    let nb = (normalize b).ToLowerInvariant()
    if na = nb then 1.0 else levenshtein.Similarity(na, nb)

let private wordFuzzyMatch (seg: string) (text: string) : bool =
    let normSeg = (normalize seg).ToLowerInvariant()
    let words = (normalize text).ToLowerInvariant().Split(' ')

    words
    |> Array.exists (fun w -> levenshtein.Similarity(normSeg, w) >= fuzzyThreshold)

let private windowMatch (seg: string) (text: string) : bool =
    let normSeg = normalize seg
    let normText = normalize text

    if normText.Contains(normSeg, StringComparison.OrdinalIgnoreCase) then
        true
    elif computeSimilarity normSeg normText >= fuzzyThreshold then
        true
    else
        // For short single-word segments, also check word-by-word fuzzy match
        let segWords = normSeg.Split(' ')
        segWords.Length = 1 && wordFuzzyMatch normSeg normText

let segmentIsPresent (segment: string) (mdText: string) : bool = windowMatch segment mdText

let classifyMissing (segment: string) : Criticality =
    let norm = normalize segment

    if norm.Length < 50 then
        Criticality.CRITICAL
    else
        Criticality.HIGH

let checkText (pdfChunks: string list) (mdText: string) : Finding list =
    pdfChunks
    |> List.choose (fun chunk ->
        if String.IsNullOrWhiteSpace(chunk) then
            None
        elif segmentIsPresent chunk mdText then
            None
        else
            let criticality = classifyMissing chunk

            Some
                { Category = "text-completeness"
                  Criticality = string criticality
                  Confidence = "HIGH"
                  LocationPdf = None
                  LocationMd = None
                  Description = sprintf "Missing text: %s" (chunk.Substring(0, min 50 chunk.Length))
                  PdfText = Some chunk
                  FixSuggestion = Some "Add the missing section to the Markdown"
                  AutoFixable = false })
