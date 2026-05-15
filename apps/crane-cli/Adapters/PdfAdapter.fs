module CraneCli.Adapters.PdfAdapter

open System.Diagnostics.CodeAnalysis
open System.IO
open UglyToad.PdfPig
open CraneCli.Models.PdfMetadata

type IPdfAdapter =
    abstract member GetMetadata: path: string -> Result<PdfMetadata, string>
    abstract member SampleText: path: string * pageCount: int -> Result<string, string>
    abstract member ExtractPages: path: string * startPage: int * endPage: int -> Result<string, string>

[<ExcludeFromCodeCoverage(Justification = "Integration-tested against real PDF files via crane-cli-integration-tests")>]
type RealPdfAdapter() =
    let nullableToOption (s: string | null) =
        if System.String.IsNullOrEmpty(s) then
            None
        else
            Some(s |> string)

    interface IPdfAdapter with
        member _.GetMetadata(path) =
            try
                use doc = PdfDocument.Open(path)
                let pages = doc.NumberOfPages
                let info = doc.Information
                let fileSize = FileInfo(path).Length

                Ok
                    { Pages = pages
                      Title = nullableToOption info.Title
                      Author = nullableToOption info.Author
                      File = path
                      SizeBytes = fileSize }
            with ex ->
                Error(sprintf "Failed to read PDF: %s" ex.Message)

        member _.SampleText(path, pageCount) =
            try
                use doc = PdfDocument.Open(path)

                let sampled =
                    doc.GetPages()
                    |> Seq.truncate pageCount
                    |> Seq.collect (fun page -> page.GetWords() |> Seq.map (fun w -> w.Text))
                    |> String.concat " "

                Ok sampled
            with ex ->
                Error(sprintf "Failed to sample PDF text: %s" ex.Message)

        member _.ExtractPages(path, startPage, endPage) =
            try
                use doc = PdfDocument.Open(path)
                let total = doc.NumberOfPages
                let start = max 1 startPage
                let finish = min total endPage

                let text =
                    [ start..finish ]
                    |> List.map (fun pageNum ->
                        let page = doc.GetPage(pageNum)
                        page.GetWords() |> Seq.map (fun w -> w.Text) |> String.concat " ")
                    |> String.concat "\n"

                Ok text
            with ex ->
                Error(sprintf "Failed to extract pages: %s" ex.Message)

type FakePdfAdapter(text: string, pages: int, sizeBytes: int64) =
    interface IPdfAdapter with
        member _.GetMetadata(path) =
            Ok
                { Pages = pages
                  Title = Some "Fake Document"
                  Author = None
                  File = path
                  SizeBytes = sizeBytes }

        member _.SampleText(_path, _pageCount) = Ok text
        member _.ExtractPages(_path, _startPage, _endPage) = Ok text
