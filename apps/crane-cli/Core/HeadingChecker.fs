module CraneCli.Core.HeadingChecker

open System
open System.Text.RegularExpressions
open CraneCli.Models.Finding

let private sectionNumPattern =
    Regex(@"^(\d+|\w)(\.\d+|\.\w)*\.?\s", RegexOptions.Compiled)

type HeadingEntry = { Depth: int; Text: string }

let inferDepthFromNumbering (heading: string) : (int * string) option =
    let heading = heading.Trim()
    let m = sectionNumPattern.Match(heading)

    if not m.Success then
        None
    else
        let numPart = m.Value.TrimEnd(' ', '\t')
        let dots = numPart |> Seq.filter ((=) '.') |> Seq.length

        let depth = if numPart.EndsWith('.') then dots + 1 else dots + 2

        Some(min 5 depth, "HIGH")

let extractMdHeadings (mdText: string) : HeadingEntry list =
    mdText.Split('\n')
    |> Array.choose (fun line ->
        let trimmed = line.TrimStart()

        if trimmed.StartsWith("#") then
            let depth = trimmed |> Seq.takeWhile ((=) '#') |> Seq.length
            let text = trimmed.TrimStart('#').Trim()
            Some { Depth = depth; Text = text }
        else
            None)
    |> Array.toList

let checkHeadings (pdfLayoutText: string) (mdText: string) : Finding list =
    let mdHeadings = extractMdHeadings mdText

    pdfLayoutText.Split('\n')
    |> Array.choose (fun line ->
        match inferDepthFromNumbering line with
        | None -> None
        | Some(expectedDepth, _) ->
            let headingText = sectionNumPattern.Replace(line.Trim(), "").Trim()

            let mdMatch =
                mdHeadings
                |> List.tryFind (fun h ->
                    h.Text.Contains(headingText, StringComparison.OrdinalIgnoreCase)
                    || headingText.Contains(h.Text, StringComparison.OrdinalIgnoreCase))

            match mdMatch with
            | None -> None
            | Some mdH when mdH.Depth <> expectedDepth ->
                let diff = abs (mdH.Depth - expectedDepth)
                let criticality = if diff >= 1 then "HIGH" else "MEDIUM"

                Some
                    { Category = "heading-depth"
                      Criticality = criticality
                      Confidence = "HIGH"
                      LocationPdf = Some line
                      LocationMd = Some(sprintf "H%d: %s" mdH.Depth mdH.Text)
                      Description = sprintf "Expected H%d, found H%d for '%s'" expectedDepth mdH.Depth headingText
                      PdfText = Some line
                      FixSuggestion = Some(sprintf "Change heading to H%d" expectedDepth)
                      AutoFixable = false }
            | _ -> None)
    |> Array.toList
