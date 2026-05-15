module CraneCli.Models.Finding

open System.Text.Json.Serialization

type Criticality =
    | CRITICAL
    | HIGH
    | MEDIUM
    | LOW

type Confidence =
    | HIGH
    | MEDIUM
    | FALSE_POSITIVE

type Category =
    | TextCompleteness
    | TextAccuracy
    | HeadingLevelAccuracy
    | ContentNestingAccuracy
    | TableIntegrity
    | FigureCoverage
    | MermaidSyntax
    | OcrQuality
    | Structure

type Finding =
    { [<JsonPropertyName("category")>]
      Category: string
      [<JsonPropertyName("criticality")>]
      Criticality: string
      [<JsonPropertyName("confidence")>]
      Confidence: string
      [<JsonPropertyName("location_pdf")>]
      LocationPdf: string option
      [<JsonPropertyName("location_md")>]
      LocationMd: string option
      [<JsonPropertyName("description")>]
      Description: string
      [<JsonPropertyName("pdf_text")>]
      PdfText: string option
      [<JsonPropertyName("fix_suggestion")>]
      FixSuggestion: string option
      [<JsonPropertyName("auto_fixable")>]
      AutoFixable: bool }
