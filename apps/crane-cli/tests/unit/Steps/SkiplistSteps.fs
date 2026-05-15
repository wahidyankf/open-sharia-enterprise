module CraneCli.Tests.Unit.Steps.SkiplistSteps

open System
open System.IO
open TickSpec
open Xunit
open CraneCli.Core.SkiplistManager
open CraneCli.Commands.SkiplistCommands
open CraneCli.Tests.Unit.Steps.BddState

// ---- Helper ----

let private uniqueBase () =
    sprintf "test-%s" (Guid.NewGuid().ToString("N").[..7])

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

// ---- xUnit [<Fact>] unit tests ----

[<Fact>]
let ``TestUnitStableKey_IsDeterministic`` () =
    let key1 = stableKey "doc" "category" "description"
    let key2 = stableKey "doc" "category" "description"
    Assert.Equal(key1, key2)

[<Fact>]
let ``TestUnitStableKey_DiffersForDifferentInputs`` () =
    let key1 = stableKey "doc" "cat1" "desc"
    let key2 = stableKey "doc" "cat2" "desc"
    Assert.NotEqual<string>(key1, key2)

[<Fact>]
let ``TestUnitAdd_ReturnsTrueForNewEntry`` () =
    let basename = uniqueBase ()
    let result = add basename "category" "description"
    let path = sprintf ".crane-skiplist-%s.json" basename

    if File.Exists(path) then
        File.Delete(path)

    Assert.Equal(Ok true, result)

[<Fact>]
let ``TestUnitAdd_ReturnsFalseForDuplicate`` () =
    let basename = uniqueBase ()
    add basename "category" "description" |> ignore
    let result = add basename "category" "description"
    let path = sprintf ".crane-skiplist-%s.json" basename

    if File.Exists(path) then
        File.Delete(path)

    Assert.Equal(Ok false, result)

[<Fact>]
let ``TestUnitCheck_ReturnsTrueForExistingEntry`` () =
    let basename = uniqueBase ()
    add basename "cat" "desc" |> ignore
    let result = check basename "cat" "desc"
    let path = sprintf ".crane-skiplist-%s.json" basename

    if File.Exists(path) then
        File.Delete(path)

    Assert.Equal(Ok true, result)

[<Fact>]
let ``TestUnitCheck_ReturnsFalseForMissingEntry`` () =
    let basename = uniqueBase ()
    let result = check basename "cat" "desc-not-added"
    Assert.Equal(Ok false, result)

[<Fact>]
let ``TestUnitList_ReturnsAllEntries`` () =
    let basename = uniqueBase ()
    add basename "cat1" "desc1" |> ignore
    add basename "cat2" "desc2" |> ignore
    let result = list basename
    let path = sprintf ".crane-skiplist-%s.json" basename

    if File.Exists(path) then
        File.Delete(path)

    match result with
    | Ok entries -> Assert.Equal(2, entries.Length)
    | Error msg -> Assert.Fail(msg)

[<Fact>]
let ``TestUnitList_ReturnsEmptyForMissingFile`` () =
    let basename = uniqueBase ()
    let path = sprintf ".crane-skiplist-%s.json" basename

    if File.Exists(path) then
        File.Delete(path)

    let result = list basename
    Assert.Equal(Ok [], result)

[<Fact>]
let ``TestUnitRunCheck_ExitCode1ForMissingEntry`` () =
    let basename = uniqueBase ()

    use sw = new System.IO.StringWriter()
    let code = runCheck basename "cat" "not-added" sw
    Assert.Equal(1, code)

[<Fact>]
let ``TestUnitRunList_ReturnsJsonArray`` () =
    let basename = uniqueBase ()
    add basename "cat1" "desc1" |> ignore
    add basename "cat2" "desc2" |> ignore

    use sw = new System.IO.StringWriter()
    let code = runList basename sw
    let path = sprintf ".crane-skiplist-%s.json" basename

    if File.Exists(path) then
        File.Delete(path)

    Assert.Equal(0, code)
    let json = System.Text.Json.JsonDocument.Parse(sw.ToString().Trim())
    Assert.Equal(System.Text.Json.JsonValueKind.Array, json.RootElement.ValueKind)
    Assert.Equal(2, json.RootElement.GetArrayLength())

[<Fact>]
let ``TestUnitRunAdd_ExitCode0ForNew`` () =
    let basename = uniqueBase ()

    use sw = new System.IO.StringWriter()
    let code = runAdd basename "cat" "desc" sw
    let path = sprintf ".crane-skiplist-%s.json" basename

    if File.Exists(path) then
        File.Delete(path)

    Assert.Equal(0, code)

[<Fact>]
let ``TestUnitRunCheck_ExitCode0ForExistingEntry`` () =
    let basename = uniqueBase ()
    add basename "cat" "desc" |> ignore

    use sw = new System.IO.StringWriter()
    let code = runCheck basename "cat" "desc" sw
    let path = sprintf ".crane-skiplist-%s.json" basename

    if File.Exists(path) then
        File.Delete(path)

    Assert.Equal(0, code)

[<Fact>]
let ``TestUnitLoadEntries_MalformedJson_ReturnsEmpty`` () =
    // Write malformed JSON to the skiplist file and call list
    // This triggers the JsonDeserialize exception path (with _ -> [])
    let basename = uniqueBase ()
    let path = sprintf ".crane-skiplist-%s.json" basename
    File.WriteAllText(path, "not-valid-json{{{")
    let result = list basename

    if File.Exists(path) then
        File.Delete(path)

    // Should return Ok [] when JSON is malformed (graceful fallback)
    Assert.Equal(Ok [], result)

[<Fact>]
let ``TestUnitAdd_ErrorWhenFileIsDirectory`` () =
    // Create a directory where the skiplist file should be
    // This causes File.WriteAllText to throw when saving entries
    let basename = uniqueBase ()
    let path = sprintf ".crane-skiplist-%s.json" basename

    // Create dir at the path so saveEntries fails
    if not (Directory.Exists(path)) then
        Directory.CreateDirectory(path) |> ignore

    let result = add basename "cat" "desc"

    // Cleanup
    if Directory.Exists(path) then
        Directory.Delete(path)

    match result with
    | Error _ -> () // Expected: Error from IO exception
    | Ok _ -> Assert.Fail("Expected Error but got Ok")

[<Fact>]
let ``TestUnitRunAdd_ErrorPath_Returns1`` () =
    // Create a directory where the skiplist file should be, causing add to fail
    let basename = uniqueBase ()
    let path = sprintf ".crane-skiplist-%s.json" basename

    if not (Directory.Exists(path)) then
        Directory.CreateDirectory(path) |> ignore

    use sw = new System.IO.StringWriter()
    let code = runAdd basename "cat" "desc" sw

    if Directory.Exists(path) then
        Directory.Delete(path)

    Assert.Equal(1, code)
