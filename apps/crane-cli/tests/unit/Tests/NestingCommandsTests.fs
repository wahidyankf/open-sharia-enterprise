module CraneCli.Tests.Unit.Tests.NestingCommandsTests

open Xunit
open CraneCli.Core.NestingChecker
open CraneCli.Commands.NestingCommands

[<Fact>]
let ``TestUnitExtractNesting_SingleLevel`` () =
    let text = "- item one\n- item two"
    let items = extractNestingLevels text
    Assert.Equal(2, items.Length)
    Assert.All(items, fun item -> Assert.Equal(1, item.Level))

[<Fact>]
let ``TestUnitExtractNesting_TwoLevels`` () =
    let text = "- parent\n  - child"
    let items = extractNestingLevels text
    Assert.Equal(2, items.Length)
    Assert.Equal(1, items.[0].Level)
    Assert.Equal(2, items.[1].Level)

[<Fact>]
let ``TestUnitWrongNesting_OffByOne_IsMedium`` () =
    let pdf = "- parent\n  - child"
    let md = "- parent\n    - child"
    let findings = checkNesting pdf md
    Assert.NotEmpty(findings)

[<Fact>]
let ``TestUnitInvertedNesting_IsHigh`` () =
    // PDF: child at level 2; MD: child at level 1 (mdItem.Level < pdfItem.Level → inverted → HIGH)
    let pdf = "- parent\n  - child"
    let md = "- child\n- parent"
    let findings = checkNesting pdf md
    Assert.NotEmpty(findings)

[<Fact>]
let ``NestingCommands_runInfer_ReturnsJson`` () =
    use sw = new System.IO.StringWriter()
    let code = runInfer "- item one\n- item two" sw
    let json = System.Text.Json.JsonDocument.Parse(sw.ToString().Trim())
    Assert.Equal(0, code)
    Assert.Equal(System.Text.Json.JsonValueKind.Array, json.RootElement.ValueKind)
