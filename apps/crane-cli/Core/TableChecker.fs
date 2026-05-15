module CraneCli.Core.TableChecker

open System.Text.RegularExpressions
open CraneCli.Models.Finding

type TableSpec =
    { RowCount: int
      ColCount: int
      HeaderRow: string }

let private pipePattern = Regex(@"\|[^|]+", RegexOptions.Compiled)

let private isSeparatorLine (line: string) =
    line.Contains("---") || line.Contains("===")

let private isTableRow (line: string) = pipePattern.Matches(line).Count >= 2

let detectTables (layoutText: string) : TableSpec list =
    let lines = layoutText.Split('\n')
    let mutable results = []
    let mutable i = 0

    while i < lines.Length - 1 do
        let line = lines.[i]
        let next = lines.[i + 1]
        let cols = pipePattern.Matches(line).Count

        if cols >= 2 && isSeparatorLine next then
            // Count data rows following the separator
            let mutable dataRows = 0
            let mutable j = i + 2

            while j < lines.Length && isTableRow lines.[j] do
                dataRows <- dataRows + 1
                j <- j + 1

            results <-
                { RowCount = dataRows + 1 // header counts as 1
                  ColCount = cols
                  HeaderRow = line }
                :: results

            i <- j
        else
            i <- i + 1

    List.rev results

let checkTables (pdfLayoutText: string) (mdText: string) : Finding list =
    let pdfTables = detectTables pdfLayoutText
    let mdTables = detectTables mdText

    pdfTables
    |> List.choose (fun pdfTable ->
        let mdMatch = mdTables |> List.tryFind (fun t -> t.ColCount = pdfTable.ColCount)

        match mdMatch with
        | None ->
            Some
                { Category = "table-integrity"
                  Criticality = "CRITICAL"
                  Confidence = "HIGH"
                  LocationPdf = Some pdfTable.HeaderRow
                  LocationMd = None
                  Description = sprintf "Missing table with %d columns" pdfTable.ColCount
                  PdfText = Some pdfTable.HeaderRow
                  FixSuggestion = Some "Add the missing table to the Markdown"
                  AutoFixable = false }
        | Some mdTable when mdTable.RowCount <> pdfTable.RowCount && pdfTable.RowCount > 1 ->
            Some
                { Category = "table-integrity"
                  Criticality = "MEDIUM"
                  Confidence = "MEDIUM"
                  LocationPdf = Some pdfTable.HeaderRow
                  LocationMd = Some mdTable.HeaderRow
                  Description = sprintf "Table row count mismatch: PDF %d, MD %d" pdfTable.RowCount mdTable.RowCount
                  PdfText = None
                  FixSuggestion = Some "Verify row count matches source PDF"
                  AutoFixable = false }
        | _ -> None)
