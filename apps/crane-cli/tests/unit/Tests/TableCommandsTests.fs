module CraneCli.Tests.Unit.Tests.TableCommandsTests

open Xunit
open CraneCli.Core.TableChecker
open CraneCli.Commands.TableCommands

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

[<Fact>]
let ``TestUnitDetect3ColTable_ReturnsOne`` () =
    let text = "| Col1 | Col2 | Col3 |\n|---|---|---|\n| v1 | v2 | v3 |"
    let tables = detectTables text
    Assert.Equal(1, tables.Length)
    Assert.Equal(3, tables.[0].ColCount)

[<Fact>]
let ``TestUnitDetectProse_ReturnsEmpty`` () =
    let text = "This is just some prose text."
    let tables = detectTables text
    Assert.Empty(tables)

[<Fact>]
let ``TestUnitMissingTable_IsCritical`` () =
    let pdf = "| A | B | C |\n|---|---|---|\n| 1 | 2 | 3 |"
    let md = "No table here"
    let findings = checkTables pdf md
    Assert.NotEmpty(findings)
    Assert.Equal("CRITICAL", (List.head findings).Criticality)

[<Fact>]
let ``TestUnitPresentTable_NoFinding`` () =
    let pdf = "| A | B | C |\n|---|---|---|\n| 1 | 2 | 3 |"
    let md = "| A | B | C |\n|---|---|---|\n| 1 | 2 | 3 |"
    let findings = checkTables pdf md
    Assert.Empty(findings)

[<Fact>]
let ``TestUnitWrongRowCount_IsMedium`` () =
    // PDF table with 5 data rows (RowCount=6), MD with 3 data rows (RowCount=4)
    let pdf = makeTable 3 5
    let md = makeTable 3 3
    let findings = checkTables pdf md
    Assert.NotEmpty(findings)
    Assert.Equal("MEDIUM", (List.head findings).Criticality)

[<Fact>]
let ``TableCommands_runDetect_ReturnsJson`` () =
    use sw = new System.IO.StringWriter()

    let code =
        CraneCli.Commands.TableCommands.runDetect "| A | B | C |\n|---|---|---|" sw

    let json = System.Text.Json.JsonDocument.Parse(sw.ToString().Trim())
    Assert.Equal(0, code)
    Assert.Equal(System.Text.Json.JsonValueKind.Array, json.RootElement.ValueKind)
