module CraneCli.Core.MermaidValidator

open System.Text.RegularExpressions
open CraneCli.Models.Finding

let private validTypes =
    Set.ofList
        [ "graph"
          "flowchart"
          "sequenceDiagram"
          "stateDiagram"
          "stateDiagram-v2"
          "classDiagram"
          "gantt"
          "pie"
          "erDiagram"
          "journey"
          "gitGraph"
          "mindmap"
          "timeline"
          "quadrantChart"
          "xychart-beta"
          "sankey-beta"
          "block-beta"
          "architecture-beta" ]

type MermaidBlock = { LineNumber: int; Content: string }

let validateBlock (content: string) : Result<unit, string> =
    let lines = content.Trim().Split('\n')

    if lines.Length = 0 || lines.[0].Trim() = "" then
        Error "empty Mermaid block"
    else
        let diagramType = lines.[0].Trim().Split(' ').[0]

        if not (Set.contains diagramType validTypes) then
            Error(sprintf "unknown diagram type: %s" diagramType)
        elif
            (content |> Seq.filter ((=) '[') |> Seq.length)
            <> (content |> Seq.filter ((=) ']') |> Seq.length)
        then
            Error "unmatched brackets"
        elif
            (content |> Seq.filter ((=) '(') |> Seq.length)
            <> (content |> Seq.filter ((=) ')') |> Seq.length)
        then
            Error "unmatched parentheses"
        else
            Ok()

let extractBlocks (mdText: string) : MermaidBlock list =
    let lines = mdText.Split('\n')
    let mutable blocks = []
    let mutable inBlock = false
    let mutable blockStart = 0
    let mutable blockContent = System.Text.StringBuilder()

    for i in 0 .. lines.Length - 1 do
        let line = lines.[i]

        if not inBlock && line.Trim() = "```mermaid" then
            inBlock <- true
            blockStart <- i + 1
            blockContent <- System.Text.StringBuilder()
        elif inBlock && line.Trim() = "```" then
            blocks <-
                { LineNumber = blockStart
                  Content = blockContent.ToString() }
                :: blocks

            inBlock <- false
        elif inBlock then
            blockContent.AppendLine(line) |> ignore

    List.rev blocks

let validateMd (mdText: string) : Finding list =
    extractBlocks mdText
    |> List.choose (fun block ->
        match validateBlock block.Content with
        | Ok() -> None
        | Error msg ->
            Some
                { Category = "mermaid-syntax"
                  Criticality = "HIGH"
                  Confidence = "HIGH"
                  LocationPdf = None
                  LocationMd = Some(sprintf "line %d" block.LineNumber)
                  Description = msg
                  PdfText = None
                  FixSuggestion = Some "Fix Mermaid diagram syntax"
                  AutoFixable = false })
