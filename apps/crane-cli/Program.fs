module CraneCli.Program

open System
open Argu
open CraneCli.Adapters.PdfAdapter
open CraneCli.Commands.PdfCommands

type PdfArgs =
    | [<AltCommandLine("-f")>] Info of pdf: string
    | [<AltCommandLine("-t")>] Type of pdf: string
    | [<AltCommandLine("-e")>] Extract of pdf: string
    | [<AltCommandLine("-s")>] Start_Page of int
    | [<AltCommandLine("-n")>] End_Page of int
    | [<AltCommandLine("-o")>] Output of string

    interface IArgParserTemplate with
        member a.Usage =
            match a with
            | Info _ -> "Get PDF metadata as JSON"
            | Type _ -> "Detect if PDF is text-based or image-based"
            | Extract _ -> "Extract text from PDF pages"
            | Start_Page _ -> "Start page (default 1)"
            | End_Page _ -> "End page (default: last page)"
            | Output _ -> "Output file path (default: stdout)"

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

type CheckAllArgs =
    | [<MainCommand; ExactlyOnce; Last>] Pair of pdf: string * md: string
    | [<AltCommandLine("-c")>] Cache_Dir of dir: string

    interface IArgParserTemplate with
        member a.Usage =
            match a with
            | Pair _ -> "PDF and MD pair to check across all dimensions"
            | Cache_Dir _ -> "Directory to cache PDF extractions, keyed by SHA256(pdf-bytes)"

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
    | [<CliPrefix(CliPrefix.None); CustomCommandLine("check-all")>] Check_All of ParseResults<CheckAllArgs>

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
            | Check_All _ -> "Run all check dimensions in one pass (text, heading, nesting, table, figure, mermaid)"

let private assemblyVersion () =
    let asm = Reflection.Assembly.GetExecutingAssembly()

    match asm.GetName().Version with
    | null -> "0.0.0"
    | v -> v.ToString()

let private buildAdapter (cacheDir: string option) : IPdfAdapter =
    let realAdapter = RealPdfAdapter() :> IPdfAdapter

    match cacheDir with
    | Some dir -> CraneCli.Core.PdfExtractionCache.wrap realAdapter dir
    | None -> realAdapter

[<EntryPoint>]
let main argv =
    if Array.exists (fun a -> a = "--version" || a = "-V") argv then
        printfn "%s" (assemblyVersion ())
        0
    else

        let parser = ArgumentParser.Create<CraneArgs>(programName = "crane")

        try
            let results = parser.ParseCommandLine(argv)

            match results.GetSubCommand() with
            | Pdf subArgs ->
                let allArgs = subArgs.GetAllResults()

                let subCommand =
                    allArgs
                    |> List.tryPick (function
                        | PdfArgs.Info p -> Some("info", p)
                        | PdfArgs.Type p -> Some("type", p)
                        | PdfArgs.Extract p -> Some("extract", p)
                        | _ -> None)

                let realAdapter = RealPdfAdapter() :> IPdfAdapter

                match subCommand with
                | Some("info", pdf) -> runInfo realAdapter Console.Out pdf
                | Some("type", pdf) -> runType realAdapter Console.Out pdf
                | Some("extract", pdf) ->
                    let startPage = subArgs.GetResult(PdfArgs.Start_Page, defaultValue = 1)
                    let endPage = subArgs.GetResult(PdfArgs.End_Page, defaultValue = 999)
                    let output = subArgs.TryGetResult(PdfArgs.Output)
                    runExtract realAdapter Console.Out pdf startPage endPage output
                | _ ->
                    printfn "%s" (subArgs.Parser.PrintUsage())
                    1
            | Text subArgs ->
                let allArgs = subArgs.GetAllResults()
                let realAdapter = RealPdfAdapter() :> IPdfAdapter

                let action =
                    allArgs
                    |> List.tryPick (function
                        | TextArgs.Check(pdf, md) -> Some(Choice1Of2(pdf, md))
                        | TextArgs.Search(md, seg) -> Some(Choice2Of2(md, seg)))

                match action with
                | Some(Choice1Of2(pdf, md)) ->
                    let mdText = IO.File.ReadAllText(md)
                    CraneCli.Commands.TextCommands.runCheck realAdapter pdf mdText Console.Out
                | Some(Choice2Of2(md, seg)) ->
                    let mdText = IO.File.ReadAllText(md)
                    CraneCli.Commands.TextCommands.runSearch mdText seg Console.Out
                | None ->
                    printfn "%s" (subArgs.Parser.PrintUsage())
                    1
            | Heading subArgs ->
                let allArgs = subArgs.GetAllResults()
                let realAdapter = RealPdfAdapter() :> IPdfAdapter

                let action =
                    allArgs
                    |> List.tryPick (function
                        | HeadingArgs.Infer pdf -> Some(Choice1Of2 pdf)
                        | HeadingArgs.Check(pdf, md) -> Some(Choice2Of2(pdf, md)))

                match action with
                | Some(Choice1Of2 pdf) ->
                    match realAdapter.SampleText(pdf, 999) with
                    | Ok text -> CraneCli.Commands.HeadingCommands.runInfer text Console.Out
                    | Error msg ->
                        eprintfn "Error: %s" msg
                        1
                | Some(Choice2Of2(pdf, md)) ->
                    match realAdapter.SampleText(pdf, 999) with
                    | Ok pdfText ->
                        let mdText = IO.File.ReadAllText(md)
                        CraneCli.Commands.HeadingCommands.runCheck pdfText mdText Console.Out
                    | Error msg ->
                        eprintfn "Error: %s" msg
                        1
                | None ->
                    printfn "%s" (subArgs.Parser.PrintUsage())
                    1
            | Nesting subArgs ->
                let allArgs = subArgs.GetAllResults()
                let realAdapter = RealPdfAdapter() :> IPdfAdapter

                let action =
                    allArgs
                    |> List.tryPick (function
                        | NestingArgs.Infer pdf -> Some(Choice1Of2 pdf)
                        | NestingArgs.Check(pdf, md) -> Some(Choice2Of2(pdf, md)))

                match action with
                | Some(Choice1Of2 pdf) ->
                    match realAdapter.SampleText(pdf, 999) with
                    | Ok text -> CraneCli.Commands.NestingCommands.runInfer text Console.Out
                    | Error msg ->
                        eprintfn "Error: %s" msg
                        1
                | Some(Choice2Of2(pdf, md)) ->
                    match realAdapter.SampleText(pdf, 999) with
                    | Ok pdfText ->
                        let mdText = IO.File.ReadAllText(md)
                        CraneCli.Commands.NestingCommands.runCheck pdfText mdText Console.Out
                    | Error msg ->
                        eprintfn "Error: %s" msg
                        1
                | None ->
                    printfn "%s" (subArgs.Parser.PrintUsage())
                    1
            | Table subArgs ->
                let allArgs = subArgs.GetAllResults()
                let realAdapter = RealPdfAdapter() :> IPdfAdapter

                let action =
                    allArgs
                    |> List.tryPick (function
                        | TableArgs.Detect pdf -> Some(Choice1Of2 pdf)
                        | TableArgs.Check(pdf, md) -> Some(Choice2Of2(pdf, md)))

                match action with
                | Some(Choice1Of2 pdf) ->
                    match realAdapter.SampleText(pdf, 999) with
                    | Ok text -> CraneCli.Commands.TableCommands.runDetect text Console.Out
                    | Error msg ->
                        eprintfn "Error: %s" msg
                        1
                | Some(Choice2Of2(pdf, md)) ->
                    match realAdapter.SampleText(pdf, 999) with
                    | Ok pdfText ->
                        let mdText = IO.File.ReadAllText(md)
                        CraneCli.Commands.TableCommands.runCheck pdfText mdText Console.Out
                    | Error msg ->
                        eprintfn "Error: %s" msg
                        1
                | None ->
                    printfn "%s" (subArgs.Parser.PrintUsage())
                    1
            | Figure subArgs ->
                let allArgs = subArgs.GetAllResults()
                let realAdapter = RealPdfAdapter() :> IPdfAdapter

                let action =
                    allArgs
                    |> List.tryPick (function
                        | FigureArgs.Detect pdf -> Some(Choice1Of2 pdf)
                        | FigureArgs.Check(pdf, md) -> Some(Choice2Of2(pdf, md)))

                match action with
                | Some(Choice1Of2 pdf) ->
                    match realAdapter.SampleText(pdf, 999) with
                    | Ok text -> CraneCli.Commands.FigureCommands.runDetect text Console.Out
                    | Error msg ->
                        eprintfn "Error: %s" msg
                        1
                | Some(Choice2Of2(pdf, md)) ->
                    match realAdapter.SampleText(pdf, 999) with
                    | Ok pdfText ->
                        let mdText = IO.File.ReadAllText(md)
                        CraneCli.Commands.FigureCommands.runCheck pdfText mdText Console.Out
                    | Error msg ->
                        eprintfn "Error: %s" msg
                        1
                | None ->
                    printfn "%s" (subArgs.Parser.PrintUsage())
                    1
            | Mermaid subArgs ->
                let allArgs = subArgs.GetAllResults()

                let action =
                    allArgs
                    |> List.tryPick (function
                        | MermaidArgs.Validate md -> Some md)

                match action with
                | Some md ->
                    let mdText = IO.File.ReadAllText(md)
                    CraneCli.Commands.MermaidCommands.runValidate mdText Console.Out
                | None ->
                    printfn "%s" (subArgs.Parser.PrintUsage())
                    1
            | Ocr subArgs ->
                let allArgs = subArgs.GetAllResults()
                let realAdapter = RealPdfAdapter() :> IPdfAdapter

                let action =
                    allArgs
                    |> List.tryPick (function
                        | OcrArgs.Quality md -> Some(Choice1Of2 md)
                        | OcrArgs.Extract pdf -> Some(Choice2Of2 pdf))

                match action with
                | Some(Choice1Of2 md) ->
                    let mdText = IO.File.ReadAllText(md)
                    CraneCli.Commands.OcrCommands.runQuality mdText Console.Out
                | Some(Choice2Of2 pdf) ->
                    match realAdapter.SampleText(pdf, 999) with
                    | Ok pdfText -> CraneCli.Commands.OcrCommands.runExtract pdfText Console.Out
                    | Error msg ->
                        eprintfn "Error: %s" msg
                        1
                | None ->
                    printfn "%s" (subArgs.Parser.PrintUsage())
                    1
            | Report subArgs ->
                let allArgs = subArgs.GetAllResults()

                let action =
                    allArgs
                    |> List.tryPick (function
                        | ReportArgs.Init(scope, pdf, md) -> Some(Choice1Of2(scope, pdf, md))
                        | ReportArgs.Finalize(report, status) -> Some(Choice2Of2(report, status)))

                match action with
                | Some(Choice1Of2(scope, pdf, md)) -> CraneCli.Commands.ReportCommands.runInit scope pdf md Console.Out
                | Some(Choice2Of2(report, status)) ->
                    CraneCli.Commands.ReportCommands.runFinalize report status Console.Out
                | None ->
                    printfn "%s" (subArgs.Parser.PrintUsage())
                    1
            | Skiplist subArgs ->
                let allArgs = subArgs.GetAllResults()

                let action =
                    allArgs
                    |> List.tryPick (function
                        | SkiplistArgs.Add(md, cat, desc) -> Some(Choice1Of3(md, cat, desc))
                        | SkiplistArgs.Check(md, cat, desc) -> Some(Choice2Of3(md, cat, desc))
                        | SkiplistArgs.List md -> Some(Choice3Of3 md))

                match action with
                | Some(Choice1Of3(md, cat, desc)) -> CraneCli.Commands.SkiplistCommands.runAdd md cat desc Console.Out
                | Some(Choice2Of3(md, cat, desc)) -> CraneCli.Commands.SkiplistCommands.runCheck md cat desc Console.Out
                | Some(Choice3Of3 md) -> CraneCli.Commands.SkiplistCommands.runList md Console.Out
                | None ->
                    printfn "%s" (subArgs.Parser.PrintUsage())
                    1
            | Check_All subArgs ->
                let pair = subArgs.TryGetResult(CheckAllArgs.Pair)
                let cacheDir = subArgs.TryGetResult(CheckAllArgs.Cache_Dir)

                match pair with
                | Some(pdf, md) ->
                    let adapter = buildAdapter cacheDir
                    let mdText = IO.File.ReadAllText(md)
                    CraneCli.Commands.CheckAllCommands.runCheckAll adapter pdf mdText Console.Out
                | None ->
                    printfn "%s" (subArgs.Parser.PrintUsage())
                    1
        with :? ArguParseException as ex ->
            printfn "%s" ex.Message
            1
