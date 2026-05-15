module CraneCli.Tests.Unit.Tests.SkiplistCommandsTests

open System
open System.IO
open Xunit
open CraneCli.Core.SkiplistManager
open CraneCli.Commands.SkiplistCommands

let private uniqueBase () =
    sprintf "test-%s" (Guid.NewGuid().ToString("N").[..7])

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
