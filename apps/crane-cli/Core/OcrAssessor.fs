module CraneCli.Core.OcrAssessor

open System.Text.RegularExpressions
open CraneCli.Models.Finding

let private ocrErrorPatterns =
    [| Regex(@"[^\x00-\x7F]{3,}", RegexOptions.Compiled)
       Regex(@"\b[lI1]{5,}\b", RegexOptions.Compiled)
       Regex(@"\b[0Oo]{5,}\b", RegexOptions.Compiled)
       Regex(@"[a-zA-Z]{30,}", RegexOptions.Compiled) |]

type OCRSection = { Tag: string; Content: string }

let estimateOCRErrorRate (text: string) : float =
    let clean = text.Replace(" ", "").Replace("\n", "")
    let total = clean.Length

    if total = 0 then
        0.0
    else
        let errorChars =
            ocrErrorPatterns
            |> Array.sumBy (fun p -> p.Matches(text) |> Seq.cast<Match> |> Seq.sumBy (fun m -> m.Length))

        let rate = float errorChars / float total
        min 1.0 rate

let extractOCRSections (mdText: string) : OCRSection list =
    let ocrTagPattern =
        Regex(@"<!--\s*OCR:\s*(.*?)\s*-->", RegexOptions.Singleline ||| RegexOptions.Compiled)

    ocrTagPattern.Matches(mdText)
    |> Seq.cast<Match>
    |> Seq.map (fun m ->
        { Tag = "ocr-comment"
          Content = m.Groups.[1].Value })
    |> Seq.toList

let checkOCRQuality (mdText: string) : Finding list =
    let sections = extractOCRSections mdText

    if sections.IsEmpty then
        []
    else
        sections
        |> List.choose (fun section ->
            let rate = estimateOCRErrorRate section.Content

            if rate > 0.10 then
                Some
                    { Category = "ocr-quality"
                      Criticality = "CRITICAL"
                      Confidence = "HIGH"
                      LocationPdf = None
                      LocationMd = Some section.Tag
                      Description = sprintf "OCR error rate %.1f%% exceeds 10%% threshold" (rate * 100.0)
                      PdfText = None
                      FixSuggestion = Some "Manual review of OCR section required"
                      AutoFixable = false }
            elif rate > 0.05 then
                Some
                    { Category = "ocr-quality"
                      Criticality = "HIGH"
                      Confidence = "HIGH"
                      LocationPdf = None
                      LocationMd = Some section.Tag
                      Description = sprintf "OCR error rate %.1f%% exceeds 5%% threshold" (rate * 100.0)
                      PdfText = None
                      FixSuggestion = Some "Review OCR section for errors"
                      AutoFixable = false }
            elif rate > 0.02 then
                Some
                    { Category = "ocr-quality"
                      Criticality = "MEDIUM"
                      Confidence = "MEDIUM"
                      LocationPdf = None
                      LocationMd = Some section.Tag
                      Description = sprintf "OCR error rate %.1f%% exceeds 2%% threshold" (rate * 100.0)
                      PdfText = None
                      FixSuggestion = Some "Minor OCR cleanup may be needed"
                      AutoFixable = false }
            else
                None)
