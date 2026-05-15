module CraneCli.Core.FigureChecker

open System
open System.Text.RegularExpressions
open CraneCli.Models.Finding

type FigureRef = { Label: string; Number: string }

let private figurePattern =
    Regex(@"(?i)(figure|fig\.?)\s*(\d+)", RegexOptions.Compiled)

let private mermaidPattern = Regex(@"```mermaid", RegexOptions.Compiled)

let private placeholderPattern =
    Regex(@"\[FIGURE\s*\d+", RegexOptions.Compiled ||| RegexOptions.IgnoreCase)

let detectFigures (text: string) : FigureRef list =
    figurePattern.Matches(text)
    |> Seq.cast<Match>
    |> Seq.map (fun m ->
        { Label = m.Value
          Number = m.Groups.[2].Value })
    |> Seq.toList

let private figureIsCovered (figureNum: string) (mdText: string) : bool =
    let hasMermaid = mermaidPattern.IsMatch(mdText)

    let hasPlaceholder =
        placeholderPattern.IsMatch(mdText)
        && mdText.Contains(figureNum, StringComparison.OrdinalIgnoreCase)

    let hasFigLabel =
        figurePattern.IsMatch(mdText)
        && mdText.Contains(figureNum, StringComparison.OrdinalIgnoreCase)

    hasMermaid || hasPlaceholder || hasFigLabel

let checkFigures (pdfText: string) (mdText: string) : Finding list =
    detectFigures pdfText
    |> List.choose (fun fig ->
        if figureIsCovered fig.Number mdText then
            None
        else
            Some
                { Category = "figure-coverage"
                  Criticality = "HIGH"
                  Confidence = "HIGH"
                  LocationPdf = Some fig.Label
                  LocationMd = None
                  Description = sprintf "Figure %s has no representation in Markdown" fig.Number
                  PdfText = Some fig.Label
                  FixSuggestion = Some(sprintf "Add Mermaid block or [FIGURE %s: description] placeholder" fig.Number)
                  AutoFixable = false })
