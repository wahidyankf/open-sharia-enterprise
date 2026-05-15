module CraneCli.Tests.Unit.Steps.MermaidSteps

open TickSpec
open Xunit
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
