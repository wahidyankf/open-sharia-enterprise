module CraneCli.Tests.Unit.Tests.SkiplistCommandsTests

open System
open System.IO
open Xunit
open CraneCli.Core.SkiplistManager
open CraneCli.Commands.SkiplistCommands

let private uniqueBase () =
    sprintf "test-%s" (Guid.NewGuid().ToString("N").[..7])

/// Each test gets its own temp markdown file via CRANE_SKIPLIST_PATH so the
/// global generated-reports/.known-false-positives.md is never touched.
let private withTempSkiplist (action: string -> unit) =
    let path =
        Path.Combine(Path.GetTempPath(), sprintf "crane-skiplist-test-%s.md" (Guid.NewGuid().ToString("N").[..11]))

    let prior = Environment.GetEnvironmentVariable("CRANE_SKIPLIST_PATH")
    Environment.SetEnvironmentVariable("CRANE_SKIPLIST_PATH", path)

    try
        action path
    finally
        Environment.SetEnvironmentVariable("CRANE_SKIPLIST_PATH", prior)

        if File.Exists(path) then
            File.Delete(path)

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
    withTempSkiplist (fun _ ->
        let basename = uniqueBase ()
        let result = add basename "category" "description"
        Assert.Equal(Ok true, result))

[<Fact>]
let ``TestUnitAdd_ReturnsFalseForDuplicate`` () =
    withTempSkiplist (fun _ ->
        let basename = uniqueBase ()
        add basename "category" "description" |> ignore
        let result = add basename "category" "description"
        Assert.Equal(Ok false, result))

[<Fact>]
let ``TestUnitAdd_WritesMarkdownSectionHeading`` () =
    withTempSkiplist (fun path ->
        let basename = uniqueBase ()
        add basename "text-completeness" "Page header on p.3" |> ignore
        let content = File.ReadAllText(path)
        Assert.Contains(sprintf "## FALSE_POSITIVE: text-completeness | %s | Page header on p.3" basename, content))

[<Fact>]
let ``TestUnitAdd_WritesAllMetadataFields`` () =
    withTempSkiplist (fun path ->
        let basename = uniqueBase ()
        add basename "text-completeness" "Page header on p.3" |> ignore
        let content = File.ReadAllText(path)
        Assert.Contains("**Accepted**:", content)
        Assert.Contains("**Category**: text-completeness", content)
        Assert.Contains(sprintf "**File**: %s" basename, content)
        Assert.Contains("**Finding**: Page header on p.3", content)
        Assert.Contains("**Key**:", content)
        Assert.Contains("**Reason**:", content)
        Assert.Contains("---", content))

[<Fact>]
let ``TestUnitAdd_AppendsToExistingFileWithoutClobbering`` () =
    withTempSkiplist (fun path ->
        // Pre-seed with an unrelated entry from another tool
        File.WriteAllText(
            path,
            "## FALSE_POSITIVE: Other Tool | other/file.md | unrelated\n\n**Accepted**: 2026-01-01--00-00\n**Key**: deadbeefcafe1234\n\n---\n"
        )

        let basename = uniqueBase ()
        add basename "text-completeness" "new entry" |> ignore
        let content = File.ReadAllText(path)
        // Original entry survives
        Assert.Contains("Other Tool | other/file.md | unrelated", content)
        // New entry appended
        Assert.Contains(sprintf "text-completeness | %s | new entry" basename, content))

[<Fact>]
let ``TestUnitCheck_ReturnsTrueForExistingEntry`` () =
    withTempSkiplist (fun _ ->
        let basename = uniqueBase ()
        add basename "cat" "desc" |> ignore
        let result = check basename "cat" "desc"
        Assert.Equal(Ok true, result))

[<Fact>]
let ``TestUnitCheck_ReturnsFalseForMissingEntry`` () =
    withTempSkiplist (fun _ ->
        let basename = uniqueBase ()
        let result = check basename "cat" "desc-not-added"
        Assert.Equal(Ok false, result))

[<Fact>]
let ``TestUnitCheck_DoesNotMatchAcrossDifferentBasenames`` () =
    withTempSkiplist (fun _ ->
        let basenameA = uniqueBase ()
        let basenameB = uniqueBase ()
        add basenameA "cat" "shared description" |> ignore
        let result = check basenameB "cat" "shared description"
        Assert.Equal(Ok false, result))

[<Fact>]
let ``TestUnitList_ReturnsAllEntriesForBasename`` () =
    withTempSkiplist (fun _ ->
        let basename = uniqueBase ()
        add basename "cat1" "desc1" |> ignore
        add basename "cat2" "desc2" |> ignore

        match list basename with
        | Ok entries -> Assert.Equal(2, entries.Length)
        | Error msg -> Assert.Fail(msg))

[<Fact>]
let ``TestUnitList_FiltersByBasename`` () =
    withTempSkiplist (fun _ ->
        let basenameA = uniqueBase ()
        let basenameB = uniqueBase ()
        add basenameA "cat" "for A" |> ignore
        add basenameB "cat" "for B" |> ignore

        match list basenameA with
        | Ok entries ->
            Assert.Equal(1, entries.Length)
            Assert.Equal("for A", entries.[0].Description)
        | Error msg -> Assert.Fail(msg))

[<Fact>]
let ``TestUnitList_ReturnsEmptyForMissingFile`` () =
    withTempSkiplist (fun _ ->
        let basename = uniqueBase ()
        let result = list basename
        Assert.Equal(Ok [], result))

[<Fact>]
let ``TestUnitList_IgnoresNonFalsePositiveHeadings`` () =
    withTempSkiplist (fun path ->
        let basename = uniqueBase ()

        File.WriteAllText(
            path,
            "## Some Other Heading\n\nNot a skip-list entry\n\n---\n\n## FALSE_POSITIVE: cat | "
            + basename
            + " | real entry\n\n**Accepted**: 2026-01-01--00-00\n**Category**: cat\n**File**: "
            + basename
            + "\n**Finding**: real entry\n**Key**: aabbccddeeff0011\n**Reason**: test\n\n---\n"
        )

        match list basename with
        | Ok entries ->
            Assert.Equal(1, entries.Length)
            Assert.Equal("real entry", entries.[0].Description)
        | Error msg -> Assert.Fail(msg))

[<Fact>]
let ``TestUnitList_ToleratesMalformedHeadings`` () =
    withTempSkiplist (fun path ->
        let basename = uniqueBase ()

        // First section has malformed heading (only 2 pipes worth, not 3 segments) — should be skipped, not crash
        File.WriteAllText(
            path,
            "## FALSE_POSITIVE: only one segment\n\n**Accepted**: 2026-01-01--00-00\n\n---\n\n## FALSE_POSITIVE: cat | "
            + basename
            + " | real\n\n**Accepted**: 2026-01-01--00-00\n**Category**: cat\n**File**: "
            + basename
            + "\n**Finding**: real\n**Key**: aabbccddeeff0011\n**Reason**: test\n\n---\n"
        )

        match list basename with
        | Ok entries -> Assert.Equal(1, entries.Length)
        | Error msg -> Assert.Fail(msg))

[<Fact>]
let ``TestUnitLoadEntries_MalformedFile_ReturnsEmpty`` () =
    withTempSkiplist (fun path ->
        // Garbage that has no FALSE_POSITIVE headings at all
        File.WriteAllText(path, "this is not a skip list at all\njust prose\n")
        let basename = uniqueBase ()
        let result = list basename
        Assert.Equal(Ok [], result))

[<Fact>]
let ``TestUnitRunCheck_ExitCode1ForMissingEntry`` () =
    withTempSkiplist (fun _ ->
        let basename = uniqueBase ()
        use sw = new System.IO.StringWriter()
        let code = runCheck basename "cat" "not-added" sw
        Assert.Equal(1, code))

[<Fact>]
let ``TestUnitRunCheck_ExitCode0ForExistingEntry`` () =
    withTempSkiplist (fun _ ->
        let basename = uniqueBase ()
        add basename "cat" "desc" |> ignore
        use sw = new System.IO.StringWriter()
        let code = runCheck basename "cat" "desc" sw
        Assert.Equal(0, code))

[<Fact>]
let ``TestUnitRunAdd_ExitCode0ForNew`` () =
    withTempSkiplist (fun _ ->
        let basename = uniqueBase ()
        use sw = new System.IO.StringWriter()
        let code = runAdd basename "cat" "desc" sw
        Assert.Equal(0, code))

[<Fact>]
let ``TestUnitRunList_ReturnsJsonArray`` () =
    withTempSkiplist (fun _ ->
        let basename = uniqueBase ()
        add basename "cat1" "desc1" |> ignore
        add basename "cat2" "desc2" |> ignore
        use sw = new System.IO.StringWriter()
        let code = runList basename sw

        Assert.Equal(0, code)
        let json = System.Text.Json.JsonDocument.Parse(sw.ToString().Trim())
        Assert.Equal(System.Text.Json.JsonValueKind.Array, json.RootElement.ValueKind)
        Assert.Equal(2, json.RootElement.GetArrayLength()))

[<Fact>]
let ``TestUnitAdd_ErrorWhenPathIsDirectory`` () =
    let dirPath =
        Path.Combine(Path.GetTempPath(), sprintf "crane-skiplist-dir-%s" (Guid.NewGuid().ToString("N").[..11]))

    let prior = Environment.GetEnvironmentVariable("CRANE_SKIPLIST_PATH")
    Environment.SetEnvironmentVariable("CRANE_SKIPLIST_PATH", dirPath)
    Directory.CreateDirectory(dirPath) |> ignore

    try
        let basename = uniqueBase ()
        let result = add basename "cat" "desc"

        match result with
        | Error _ -> () // expected: IO error because path is a directory
        | Ok _ -> Assert.Fail("Expected Error but got Ok")
    finally
        Environment.SetEnvironmentVariable("CRANE_SKIPLIST_PATH", prior)

        if Directory.Exists(dirPath) then
            Directory.Delete(dirPath, true)

[<Fact>]
let ``TestUnitRunAdd_ErrorPath_Returns1`` () =
    let dirPath =
        Path.Combine(Path.GetTempPath(), sprintf "crane-skiplist-dir-%s" (Guid.NewGuid().ToString("N").[..11]))

    let prior = Environment.GetEnvironmentVariable("CRANE_SKIPLIST_PATH")
    Environment.SetEnvironmentVariable("CRANE_SKIPLIST_PATH", dirPath)
    Directory.CreateDirectory(dirPath) |> ignore

    try
        let basename = uniqueBase ()
        use sw = new System.IO.StringWriter()
        let code = runAdd basename "cat" "desc" sw
        Assert.Equal(1, code)
    finally
        Environment.SetEnvironmentVariable("CRANE_SKIPLIST_PATH", prior)

        if Directory.Exists(dirPath) then
            Directory.Delete(dirPath, true)

[<Fact>]
let ``TestUnitDefaultPath_IsRepoRelativeGlobalMarkdown`` () =
    // When CRANE_SKIPLIST_PATH is unset, the resolver returns the canonical
    // generated-reports/.known-false-positives.md global path.
    let prior = Environment.GetEnvironmentVariable("CRANE_SKIPLIST_PATH")
    Environment.SetEnvironmentVariable("CRANE_SKIPLIST_PATH", null)

    try
        let resolved = resolveSkiplistPath ()
        Assert.Equal("generated-reports/.known-false-positives.md", resolved)
    finally
        Environment.SetEnvironmentVariable("CRANE_SKIPLIST_PATH", prior)

[<Fact>]
let ``TestUnitAdd_CreatesParentDirectoryWhenMissing`` () =
    let nestedDir =
        Path.Combine(Path.GetTempPath(), sprintf "crane-skiplist-nested-%s" (Guid.NewGuid().ToString("N").[..11]))

    let nestedPath = Path.Combine(nestedDir, "subdir", "skiplist.md")
    let prior = Environment.GetEnvironmentVariable("CRANE_SKIPLIST_PATH")
    Environment.SetEnvironmentVariable("CRANE_SKIPLIST_PATH", nestedPath)

    try
        let basename = uniqueBase ()
        let result = add basename "cat" "desc"
        Assert.Equal(Ok true, result)
        Assert.True(File.Exists(nestedPath), sprintf "Expected file at %s" nestedPath)
    finally
        Environment.SetEnvironmentVariable("CRANE_SKIPLIST_PATH", prior)

        if Directory.Exists(nestedDir) then
            Directory.Delete(nestedDir, true)

[<Fact>]
let ``TestUnitResolveSkiplistPath_HonorsEnvVar`` () =
    let prior = Environment.GetEnvironmentVariable("CRANE_SKIPLIST_PATH")
    let overridden = "/tmp/crane-test-override.md"
    Environment.SetEnvironmentVariable("CRANE_SKIPLIST_PATH", overridden)

    try
        let resolved = resolveSkiplistPath ()
        Assert.Equal(overridden, resolved)
    finally
        Environment.SetEnvironmentVariable("CRANE_SKIPLIST_PATH", prior)
