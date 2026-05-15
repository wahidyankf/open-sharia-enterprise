module CraneCli.Program

open Argu

type PdfArgs =
    | Info of pdf: string
    | Type of pdf: string
    | Extract of pdf: string

    interface IArgParserTemplate with
        member a.Usage =
            match a with
            | Info _ -> "Get PDF metadata as JSON"
            | Type _ -> "Detect if PDF is text-based or image-based"
            | Extract _ -> "Extract text from PDF pages"

type TextArgs =
    | Check of pdf: string * md: string
    | Search of md: string * segment: string

    interface IArgParserTemplate with
        member a.Usage =
            match a with
            | Check _ -> "Check text completeness between PDF and MD"
            | Search _ -> "Search for a segment in MD"

type HeadingArgs =
    | Infer of pdf: string
    | Check of pdf: string * md: string

    interface IArgParserTemplate with
        member a.Usage =
            match a with
            | Infer _ -> "Infer heading depth from PDF numbering"
            | Check _ -> "Check heading consistency between PDF and MD"

type NestingArgs =
    | Infer of pdf: string
    | Check of pdf: string * md: string

    interface IArgParserTemplate with
        member a.Usage =
            match a with
            | Infer _ -> "Infer nesting levels from PDF"
            | Check _ -> "Check nesting consistency"

type TableArgs =
    | Detect of pdf: string
    | Check of pdf: string * md: string

    interface IArgParserTemplate with
        member a.Usage =
            match a with
            | Detect _ -> "Detect tables in PDF"
            | Check _ -> "Check table integrity"

type FigureArgs =
    | Detect of pdf: string
    | Check of pdf: string * md: string

    interface IArgParserTemplate with
        member a.Usage =
            match a with
            | Detect _ -> "Detect figures in PDF"
            | Check _ -> "Check figure coverage"

type MermaidArgs =
    | Validate of md: string

    interface IArgParserTemplate with
        member a.Usage =
            match a with
            | Validate _ -> "Validate Mermaid diagram syntax in MD"

type OcrArgs =
    | Quality of md: string
    | Extract of pdf: string

    interface IArgParserTemplate with
        member a.Usage =
            match a with
            | Quality _ -> "Assess OCR quality in MD"
            | Extract _ -> "Extract OCR sections from PDF"

type ReportArgs =
    | Init of scope: string * pdf: string * md: string
    | Finalize of report: string * status: string

    interface IArgParserTemplate with
        member a.Usage =
            match a with
            | Init _ -> "Initialize a new audit report"
            | Finalize _ -> "Finalize an audit report with status"

type SkiplistArgs =
    | Add of md: string * category: string * description: string
    | Check of md: string * category: string * description: string
    | List of md: string

    interface IArgParserTemplate with
        member a.Usage =
            match a with
            | Add _ -> "Add entry to skip list"
            | Check _ -> "Check if entry is in skip list"
            | List _ -> "List all skip list entries"

type CraneArgs =
    | [<CliPrefix(CliPrefix.None)>] Pdf of ParseResults<PdfArgs>
    | [<CliPrefix(CliPrefix.None)>] Text of ParseResults<TextArgs>
    | [<CliPrefix(CliPrefix.None)>] Heading of ParseResults<HeadingArgs>
    | [<CliPrefix(CliPrefix.None)>] Nesting of ParseResults<NestingArgs>
    | [<CliPrefix(CliPrefix.None)>] Table of ParseResults<TableArgs>
    | [<CliPrefix(CliPrefix.None)>] Figure of ParseResults<FigureArgs>
    | [<CliPrefix(CliPrefix.None)>] Mermaid of ParseResults<MermaidArgs>
    | [<CliPrefix(CliPrefix.None)>] Ocr of ParseResults<OcrArgs>
    | [<CliPrefix(CliPrefix.None)>] Report of ParseResults<ReportArgs>
    | [<CliPrefix(CliPrefix.None)>] Skiplist of ParseResults<SkiplistArgs>

    interface IArgParserTemplate with
        member a.Usage =
            match a with
            | Pdf _ -> "PDF operations (info, type, extract)"
            | Text _ -> "Text completeness checking"
            | Heading _ -> "Heading depth inference and checking"
            | Nesting _ -> "List nesting analysis"
            | Table _ -> "Table detection and checking"
            | Figure _ -> "Figure coverage checking"
            | Mermaid _ -> "Mermaid diagram validation"
            | Ocr _ -> "OCR quality assessment"
            | Report _ -> "Audit report management"
            | Skiplist _ -> "Skip list management"

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<CraneArgs>(programName = "crane")

    try
        let results = parser.ParseCommandLine(argv)

        match results.GetSubCommand() with
        | Pdf _ ->
            printfn "pdf subcommand (not yet implemented)"
            0
        | Text _ ->
            printfn "text subcommand (not yet implemented)"
            0
        | Heading _ ->
            printfn "heading subcommand (not yet implemented)"
            0
        | Nesting _ ->
            printfn "nesting subcommand (not yet implemented)"
            0
        | Table _ ->
            printfn "table subcommand (not yet implemented)"
            0
        | Figure _ ->
            printfn "figure subcommand (not yet implemented)"
            0
        | Mermaid _ ->
            printfn "mermaid subcommand (not yet implemented)"
            0
        | Ocr _ ->
            printfn "ocr subcommand (not yet implemented)"
            0
        | Report _ ->
            printfn "report subcommand (not yet implemented)"
            0
        | Skiplist _ ->
            printfn "skiplist subcommand (not yet implemented)"
            0
    with :? ArguParseException as ex ->
        printfn "%s" ex.Message
        1
