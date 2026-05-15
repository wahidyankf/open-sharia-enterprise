module CraneCli.Tests.Unit.Tests.MermaidCommandsTests

open Xunit
open CraneCli.Core.MermaidValidator

[<Fact>]
let ``TestUnitValidGraphTD_NoFinding`` () =
    let md = "```mermaid\ngraph TD\n A-->B\n```"
    Assert.Empty(validateMd md)

[<Fact>]
let ``TestUnitAllKnownTypes_Accepted`` () =
    let knownTypes = [ "graph"; "flowchart"; "sequenceDiagram"; "stateDiagram" ]

    knownTypes
    |> List.iter (fun t ->
        let md = sprintf "```mermaid\n%s\n```" t
        Assert.Empty(validateMd md))

[<Fact>]
let ``TestUnitUnknownType_IsHigh`` () =
    let md = "```mermaid\nxyz some content\n```"
    let findings = validateMd md
    Assert.NotEmpty(findings)
    Assert.Equal("HIGH", (List.head findings).Criticality)

[<Fact>]
let ``TestUnitEmptyBlock_IsHigh`` () =
    let md = "```mermaid\n\n```"
    let findings = validateMd md
    Assert.NotEmpty(findings)

[<Fact>]
let ``TestUnitUnmatchedBracket_IsHigh`` () =
    let md = "```mermaid\ngraph TD\n A[ unclosed\n```"
    let findings = validateMd md
    Assert.NotEmpty(findings)
    Assert.Equal("HIGH", (List.head findings).Criticality)

[<Fact>]
let ``TestUnitUnmatchedParen_IsHigh`` () =
    let md = "```mermaid\ngraph TD\n A((unclosed\n```"
    let findings = validateMd md
    Assert.NotEmpty(findings)
    Assert.Equal("HIGH", (List.head findings).Criticality)

[<Fact>]
let ``TestUnitFinding_IncludesLineNumber`` () =
    let md = "```mermaid\nxyz invalid\n```"
    let findings = validateMd md
    Assert.NotEmpty(findings)
    Assert.NotNull((List.head findings).LocationMd)
