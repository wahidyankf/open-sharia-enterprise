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

/// Match a normalized lowercase segment against a normalized lowercase MD text.
/// Substring is the fast path. When the segment is a single word and not present
/// verbatim, we fall back to per-word fuzzy similarity to catch spelling variants
/// (e.g. "Organisation" vs "organization"). Whole-document similarity is NOT
/// checked: when |text| >> |segment|, NormalizedLevenshtein is structurally
/// bounded near 0 and never reaches fuzzyThreshold, so the branch was dead
/// compute that scaled O(N×M) per chunk.
let private matchNormalized (normSegLower: string) (normMdLower: string) : bool =
    if normMdLower.Contains(normSegLower, StringComparison.Ordinal) then
        true
    else
        let segWords = normSegLower.Split(' ')

        if segWords.Length = 1 then
            normMdLower.Split(' ')
            |> Array.exists (fun w -> levenshtein.Similarity(normSegLower, w) >= fuzzyThreshold)
        else
            false

let segmentIsPresent (segment: string) (mdText: string) : bool =
    let normSeg = (normalize segment).ToLowerInvariant()
    let normMd = (normalize mdText).ToLowerInvariant()
    matchNormalized normSeg normMd

let classifyMissing (segment: string) : Criticality =
    let norm = normalize segment

    if norm.Length < 50 then
        Criticality.CRITICAL
    else
        Criticality.HIGH

let checkText (pdfChunks: string list) (mdText: string) : Finding list =
    // Normalize the MD once; reusing it across N chunks avoids ~O(N × |mdText|)
    // allocations that previously stalled large-PDF runs.
    let normMd = (normalize mdText).ToLowerInvariant()

    pdfChunks
    |> List.choose (fun chunk ->
        if String.IsNullOrWhiteSpace(chunk) then
            None
        else
            let normSeg = (normalize chunk).ToLowerInvariant()

            if matchNormalized normSeg normMd then
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
