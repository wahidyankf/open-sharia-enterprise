module CraneCli.Tests.Unit.Steps.MermaidSteps

open TickSpec
open Xunit
open CraneCli.Core.MermaidValidator
open CraneCli.Commands.MermaidCommands
open CraneCli.Tests.Unit.Steps.BddState

// ---- BDD shared state ----
let mutable private mdFixture: string = ""

// ---- BDD Given steps ----

[<Given>]
let ``a Markdown fixture with a syntactically valid "([^"]*)" block`` (blockType: string) =
    mdFixture <- sprintf "```mermaid\n%s\n A-->B\n```" blockType

[<Given>]
let ``a Markdown fixture with a Mermaid block starting with "([^"]*)"`` (keyword: string) =
    mdFixture <- sprintf "```mermaid\n%s content\n```" keyword

[<Given>]
let ``a Markdown fixture with a Mermaid block containing unbalanced "\["`` () =
    mdFixture <- "```mermaid\ngraph TD\n A[ unclosed\n```"

[<Given>]
let ``a Markdown fixture with one block per known diagram type`` () =
    let blocks =
        [ "graph"
          "flowchart"
          "sequenceDiagram"
          "stateDiagram"
          "classDiagram"
          "gantt"
          "pie"
          "erDiagram"
          "journey"
          "gitGraph"
          "mindmap"
          "timeline" ]
        |> List.map (fun t -> sprintf "```mermaid\n%s\n```" t)
        |> String.concat "\n\n"

    mdFixture <- blocks

// ---- BDD When steps ----

[<When>]
let ``I run "crane mermaid validate" on the fixture`` () =
    RunWithWriter(fun w -> runValidate mdFixture w)

// ---- BDD Then steps ----

[<Then>]
let ``the finding description mentions "([^"]*)"`` (keyword: string) =
    let doc = System.Text.Json.JsonDocument.Parse(LastOutput)
    let first = doc.RootElement.EnumerateArray() |> Seq.head
    Assert.Contains(keyword, first.GetProperty("description").GetString())

[<Then>]
let ``a finding with criticality "([^"]*)" and category "([^"]*)" is returned`` (crit: string) (cat: string) =
    let doc = System.Text.Json.JsonDocument.Parse(LastOutput)
    Assert.True(doc.RootElement.GetArrayLength() > 0)
    let first = doc.RootElement.EnumerateArray() |> Seq.head
    Assert.Equal(crit, first.GetProperty("criticality").GetString())
    Assert.Equal(cat, first.GetProperty("category").GetString())

// ---- xUnit [<Fact>] unit tests ----

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
