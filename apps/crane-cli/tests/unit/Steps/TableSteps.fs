module CraneCli.Tests.Unit.Steps.TableSteps

open TickSpec
open Xunit
open CraneCli.Commands.TableCommands
open CraneCli.Tests.Unit.Steps.BddState

// ---- BDD shared state ----
let mutable private pdfText: string = ""
let mutable private mdText: string = ""
let mutable private detectText: string = ""

// helpers
let private makeTable (cols: int) (dataRows: int) =
    let header =
        [ 1..cols ] |> List.map (fun i -> sprintf " Col%d " i) |> String.concat "|"

    let headerRow = sprintf "|%s|" header

    let separator =
        [ 1..cols ] |> List.map (fun _ -> "---|") |> String.concat "" |> sprintf "|%s"

    let dataRow =
        [ 1..cols ]
        |> List.map (fun i -> sprintf " val%d " i)
        |> String.concat "|"
        |> sprintf "|%s|"

    let rows = [ 0 .. dataRows - 1 ] |> List.map (fun _ -> dataRow)
    (headerRow :: separator :: rows) |> String.concat "\n"

// ---- BDD Given steps ----

[<Given>]
let ``a PDF fixture with a 3-column table`` () = pdfText <- makeTable 3 1

[<Given>]
let ``its Markdown conversion with a matching 3-column table`` () = mdText <- makeTable 3 1

[<Given>]
let ``a PDF fixture with a table`` () = pdfText <- makeTable 3 1

[<Given>]
let ``a Markdown missing that table entirely`` () = mdText <- "No table here, just prose."

[<Given>]
let ``a PDF fixture with a 5-row table`` () =
    // 5 data rows → RowCount = 6 (header counts as 1 + 5 data rows)
    pdfText <- makeTable 3 5

[<Given>]
let ``a Markdown with a matching header but only 3 rows`` () =
    // 3 data rows → RowCount = 4
    mdText <- makeTable 3 3

[<Given>]
let ``layout text containing a 3-column columnar table`` () = detectText <- makeTable 3 1

// ---- BDD When steps ----

[<When>]
let ``I run "crane table check" on the pair`` () =
    RunWithWriter(fun w -> runCheck pdfText mdText w)

[<When>]
let ``I run "crane table detect" on the text`` () =
    RunWithWriter(fun w -> runDetect detectText w)

// ---- BDD Then steps ----

[<Then>]
let ``the JSON output lists one table with col_count (\d+)`` (expected: int) =
    let doc = System.Text.Json.JsonDocument.Parse(LastOutput)
    Assert.Equal(System.Text.Json.JsonValueKind.Array, doc.RootElement.ValueKind)
    Assert.Equal(1, doc.RootElement.GetArrayLength())
    let first = doc.RootElement.EnumerateArray() |> Seq.head
    Assert.Equal(expected, first.GetProperty("ColCount").GetInt32())
