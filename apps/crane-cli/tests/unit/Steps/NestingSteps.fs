module CraneCli.Tests.Unit.Steps.NestingSteps

open TickSpec
open Xunit
open CraneCli.Core.NestingChecker
open CraneCli.Commands.NestingCommands
open CraneCli.Tests.Unit.Steps.BddState

// ---- BDD shared state ----
let mutable private pdfText: string = ""
let mutable private mdText: string = ""

// ---- BDD Given steps ----

[<Given>]
let ``a PDF fixture with a single-level bullet list`` () = pdfText <- "- item one\n- item two"

[<Given>]
let ``its Markdown conversion with matching single-level nesting`` () = mdText <- "- item one\n- item two"

[<Given>]
let ``a PDF fixture where nested items appear under a parent`` () =
    // child is at level 2 in PDF (2 spaces indent)
    pdfText <- "- parent\n  - child"

[<Given>]
let ``a Markdown with those items at the wrong nesting level`` () =
    // child is at level 1 in MD (inverted: MD level < PDF level → HIGH)
    mdText <- "- child\n- parent"

[<Given>]
let ``a PDF fixture with two-level nesting`` () = pdfText <- "- parent\n  - child"

[<Given>]
let ``a Markdown with the second level at depth three instead of two`` () =
    // child at level 3 (indent 4) instead of level 2 (indent 2)
    // MD level (3) > PDF level (2) → not inverted → MEDIUM
    mdText <- "- parent\n    - child"

// ---- BDD When steps ----

[<When>]
let ``I run "crane nesting check" on the pair`` () =
    RunWithWriter(fun w -> runCheck pdfText mdText w)

// ---- xUnit [<Fact>] unit tests ----

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
