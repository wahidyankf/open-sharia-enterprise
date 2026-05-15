module CraneCli.Tests.Unit.Tests.FigureCommandsTests

open Xunit
open CraneCli.Core.FigureChecker
open CraneCli.Commands.FigureCommands

[<Fact>]
let ``TestUnitDetectFigureN_Pattern`` () =
    let refs = detectFigures "See Figure 1 for details"
    Assert.Equal(1, refs.Length)
    Assert.Equal("1", refs.[0].Number)

[<Fact>]
let ``TestUnitDetectFigDotN_Pattern`` () =
    let refs = detectFigures "As shown in Fig. 3"
    Assert.Equal(1, refs.Length)
    Assert.Equal("3", refs.[0].Number)

[<Fact>]
let ``TestUnitNoFigures_ReturnsEmpty`` () =
    let refs = detectFigures "This document has no figures"
    Assert.Empty(refs)

[<Fact>]
let ``TestUnitMissingFigure_IsHigh`` () =
    let findings = checkFigures "See Figure 1 for details" "No figures here"
    Assert.NotEmpty(findings)
    Assert.Equal("HIGH", (List.head findings).Criticality)

[<Fact>]
let ``TestUnitPlaceholder_SatisfiesCoverage`` () =
    let findings = checkFigures "See Figure 2" "[FIGURE 2: description]"
    Assert.Empty(findings)

[<Fact>]
let ``TestUnitMermaidBlock_SatisfiesCoverage`` () =
    let findings = checkFigures "See Figure 3" "```mermaid\ngraph TD\n```"
    Assert.Empty(findings)

[<Fact>]
let ``TestUnitRunDetect_ReturnsJson`` () =
    use sw = new System.IO.StringWriter()
    let code = runDetect "See Figure 1 and Figure 2 for details" sw
    let json = System.Text.Json.JsonDocument.Parse(sw.ToString().Trim())
    Assert.Equal(0, code)
    Assert.Equal(System.Text.Json.JsonValueKind.Array, json.RootElement.ValueKind)
