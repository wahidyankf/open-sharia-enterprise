module CraneCli.Tests.Unit.Steps.SkiplistSteps

open System.IO
open TickSpec
open Xunit
open CraneCli.Core.SkiplistManager
open CraneCli.Commands.SkiplistCommands
open CraneCli.Tests.Unit.Steps.BddState

// ---- BDD shared state ----
let mutable private currentMdBasename: string = "nist-sp-800-53"

// ---- BDD Given steps ----

[<Given>]
let ``no existing skip list for "([^"]*)"`` (mdBasename: string) =
    currentMdBasename <- mdBasename
    let path = sprintf ".crane-skiplist-%s.json" mdBasename

    if File.Exists(path) then
        File.Delete(path)

[<Given>]
let ``a skip list for "([^"]*)" already containing the entry for text-completeness "([^"]*)"``
    (mdBasename: string)
    (description: string)
    =
    currentMdBasename <- mdBasename
    let path = sprintf ".crane-skiplist-%s.json" mdBasename

    if File.Exists(path) then
        File.Delete(path)

    add mdBasename "text-completeness" description |> ignore

[<Given>]
let ``a skip list containing "([^|]*)\| ([^|]*)\| ([^"]*)"``
    (category: string)
    (mdBasename: string)
    (description: string)
    =
    currentMdBasename <- mdBasename.Trim()
    let path = sprintf ".crane-skiplist-%s.json" (mdBasename.Trim())

    if File.Exists(path) then
        File.Delete(path)

    add (mdBasename.Trim()) (category.Trim()) (description.Trim()) |> ignore

// ---- BDD When steps ----

[<When>]
let ``I run "crane skiplist add nist-sp-800-53 text-completeness '([^']*)'"`` (description: string) =
    RunWithWriter(fun w -> runAdd currentMdBasename "text-completeness" description w)

[<When>]
let ``I run "crane skiplist add" with the same arguments`` () =
    RunWithWriter(fun w -> runAdd currentMdBasename "text-completeness" "Page header on p.3" w)

[<When>]
let ``I run "crane skiplist check nist-sp-800-53 mermaid-syntax '([^']*)'"`` (description: string) =
    RunWithWriter(fun w -> runCheck currentMdBasename "mermaid-syntax" description w)

[<When>]
let ``I run "crane skiplist check nist-sp-800-53 text-completeness '([^']*)'"`` (description: string) =
    RunWithWriter(fun w -> runCheck currentMdBasename "text-completeness" description w)

// ---- BDD Then steps ----

[<Then>]
let ``the skip list file is created`` () =
    let path = sprintf ".crane-skiplist-%s.json" currentMdBasename
    Assert.True(File.Exists(path), sprintf "Skip list file should exist: %s" path)

[<Then>]
let ``it contains one entry with category "([^"]*)"`` (category: string) =
    let path = sprintf ".crane-skiplist-%s.json" currentMdBasename
    Assert.True(File.Exists(path))
    let content = File.ReadAllText(path)
    Assert.Contains(category, content)
    // Cleanup
    File.Delete(path)

[<Then>]
let ``the skip list file contains exactly one matching entry`` () =
    match list currentMdBasename with
    | Ok entries -> Assert.Equal(1, entries.Length)
    | Error msg -> Assert.Fail(msg)
    // Cleanup
    let path = sprintf ".crane-skiplist-%s.json" currentMdBasename

    if File.Exists(path) then
        File.Delete(path)

[<Then>]
let ``the JSON output contains match true`` () =
    let doc = System.Text.Json.JsonDocument.Parse(LastOutput)
    Assert.True(doc.RootElement.GetProperty("match").GetBoolean())

[<Then>]
let ``the JSON output contains match false`` () =
    let doc = System.Text.Json.JsonDocument.Parse(LastOutput)
    Assert.False(doc.RootElement.GetProperty("match").GetBoolean())
