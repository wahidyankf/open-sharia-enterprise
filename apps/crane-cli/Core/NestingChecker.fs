module CraneCli.Core.NestingChecker

open CraneCli.Models.Finding

type NestingItem = { Level: int; Text: string }

let extractNestingLevels (layoutText: string) : NestingItem list =
    layoutText.Split('\n')
    |> Array.choose (fun line ->
        let trimmed = line.TrimStart()

        if trimmed.StartsWith("-") || trimmed.StartsWith("*") || trimmed.StartsWith("•") then
            let indent = line.Length - line.TrimStart().Length
            let level = indent / 2 + 1
            let text = trimmed.TrimStart('-', '*', '•', ' ')
            Some { Level = level; Text = text.Trim() }
        else
            None)
    |> Array.toList

let checkNesting (pdfLayoutText: string) (mdText: string) : Finding list =
    let pdfItems = extractNestingLevels pdfLayoutText
    let mdItems = extractNestingLevels mdText

    pdfItems
    |> List.choose (fun pdfItem ->
        let mdMatch =
            mdItems
            |> List.tryFind (fun m ->
                m.Text.Contains(pdfItem.Text, System.StringComparison.OrdinalIgnoreCase)
                || pdfItem.Text.Contains(m.Text, System.StringComparison.OrdinalIgnoreCase))

        match mdMatch with
        | None -> None
        | Some mdItem when mdItem.Level <> pdfItem.Level ->
            let isInverted = mdItem.Level < pdfItem.Level
            let criticality = if isInverted then "HIGH" else "MEDIUM"

            Some
                { Category = "content-nesting"
                  Criticality = criticality
                  Confidence = "MEDIUM"
                  LocationPdf = None
                  LocationMd = None
                  Description =
                    sprintf
                        "Nesting mismatch: PDF level %d, MD level %d for '%s'"
                        pdfItem.Level
                        mdItem.Level
                        pdfItem.Text
                  PdfText = Some pdfItem.Text
                  FixSuggestion = Some(sprintf "Adjust nesting to level %d" pdfItem.Level)
                  AutoFixable = false }
        | _ -> None)
