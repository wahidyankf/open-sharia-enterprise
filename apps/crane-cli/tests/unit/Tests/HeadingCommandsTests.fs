module CraneCli.Tests.Unit.Tests.HeadingCommandsTests

open Xunit
open CraneCli.Core.HeadingChecker
open CraneCli.Commands.HeadingCommands

[<Fact>]
let ``TestUnitInferDepth_SingleNumber`` () =
    match inferDepthFromNumbering "1. Title" with
    | Some(depth, _) -> Assert.Equal(2, depth)
    | None -> Assert.Fail("Expected Some depth")

[<Fact>]
let ``TestUnitInferDepth_TwoComponents`` () =
    match inferDepthFromNumbering "2.3 Title" with
    | Some(depth, _) -> Assert.Equal(3, depth)
    | None -> Assert.Fail("Expected Some depth")

[<Fact>]
let ``TestUnitInferDepth_ThreeComponents`` () =
    match inferDepthFromNumbering "2.3.1 Title" with
    | Some(depth, _) -> Assert.Equal(4, depth)
    | None -> Assert.Fail("Expected Some depth")

[<Fact>]
let ``TestUnitInferDepth_NoNumber`` () =
    let result = inferDepthFromNumbering "Introduction"
    Assert.Equal(None, result)

[<Fact>]
let ``TestUnitWrongDepth_IsHigh`` () =
    // 2.3.1 → depth 4; ## Overview → H2, diff = 2 → HIGH
    let pdf2 = "2.3.1 Overview"
    let md2 = "## Overview"
    let findings2 = checkHeadings pdf2 md2
    Assert.NotEmpty(findings2)
    Assert.Equal("HIGH", (List.head findings2).Criticality)

[<Fact>]
let ``TestUnitCorrectDepth_NoFinding`` () =
    // 2.3 → depth 3; ### Overview → H3 → no finding
    let pdf = "2.3 Overview"
    let md = "### Overview"
    let findings = checkHeadings pdf md
    Assert.Empty(findings)

[<Fact>]
let ``HeadingCommands_runInfer_NoSectionNumber_ReturnsNoneConfidence`` () =
    use sw = new System.IO.StringWriter()
    let code = runInfer "Introduction" sw
    let json = System.Text.Json.JsonDocument.Parse(sw.ToString().Trim())
    Assert.Equal(0, code)
    Assert.Equal("NONE", json.RootElement.GetProperty("confidence").GetString())
