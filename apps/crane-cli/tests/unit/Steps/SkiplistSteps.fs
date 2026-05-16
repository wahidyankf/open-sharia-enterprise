module CraneCli.Tests.Unit.Steps.SkiplistSteps

open System
open System.IO
open TickSpec
open Xunit
open CraneCli.Core.SkiplistManager
open CraneCli.Commands.SkiplistCommands
open CraneCli.Tests.Unit.Steps.BddState

// ---- BDD shared state ----
let mutable private currentMdBasename: string = "nist-sp-800-53"
let mutable private currentTempPath: string = ""

let private resetTempPath () =
    // Each scenario gets its own temp markdown file via CRANE_SKIPLIST_PATH so the
    // global generated-reports/.known-false-positives.md is never touched.
    currentTempPath <-
        Path.Combine(Path.GetTempPath(), sprintf "crane-skiplist-bdd-%s.md" (Guid.NewGuid().ToString("N").[..11]))

    Environment.SetEnvironmentVariable("CRANE_SKIPLIST_PATH", currentTempPath)

    if File.Exists(currentTempPath) then
        File.Delete(currentTempPath)

// ---- BDD Given steps ----

[<Given>]
let ``no existing skip list for "([^"]*)"`` (mdBasename: string) =
    currentMdBasename <- mdBasename
    resetTempPath ()

[<Given>]
let ``a skip list for "([^"]*)" already containing the entry for text-completeness "([^"]*)"``
    (mdBasename: string)
    (description: string)
    =
    currentMdBasename <- mdBasename
    resetTempPath ()
    add mdBasename "text-completeness" description |> ignore

[<Given>]
let ``a skip list containing "([^|]*)\| ([^|]*)\| ([^"]*)"``
    (category: string)
    (mdBasename: string)
    (description: string)
    =
    currentMdBasename <- mdBasename.Trim()
    resetTempPath ()
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
    Assert.True(File.Exists(currentTempPath), sprintf "Skip list file should exist: %s" currentTempPath)

[<Then>]
let ``it contains one entry with category "([^"]*)"`` (category: string) =
    Assert.True(File.Exists(currentTempPath))
    let content = File.ReadAllText(currentTempPath)
    Assert.Contains(sprintf "## FALSE_POSITIVE: %s |" category, content)
    // Cleanup
    File.Delete(currentTempPath)

[<Then>]
let ``the skip list file contains exactly one matching entry`` () =
    match list currentMdBasename with
    | Ok entries -> Assert.Equal(1, entries.Length)
    | Error msg -> Assert.Fail(msg)

    if File.Exists(currentTempPath) then
        File.Delete(currentTempPath)

[<Then>]
let ``the JSON output contains match true`` () =
    let doc = System.Text.Json.JsonDocument.Parse(LastOutput)
    Assert.True(doc.RootElement.GetProperty("match").GetBoolean())

[<Then>]
let ``the JSON output contains match false`` () =
    let doc = System.Text.Json.JsonDocument.Parse(LastOutput)
    Assert.False(doc.RootElement.GetProperty("match").GetBoolean())
