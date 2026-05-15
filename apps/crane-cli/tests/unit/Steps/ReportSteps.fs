module CraneCli.Tests.Unit.Steps.ReportSteps

open System
open System.IO
open TickSpec
open Xunit
open CraneCli.Core.ReportManager
open CraneCli.Commands.ReportCommands
open CraneCli.Tests.Unit.Steps.BddState

// ---- BDD shared state ----
let mutable private lastReportPath: string = ""
let mutable private currentScope: string = "pdf-to-md"

// ---- BDD Given steps ----

[<Given>]
let ``no existing chain file for scope "([^"]*)"`` (scope: string) =
    currentScope <- scope
    let chainFile = sprintf ".execution-chain-%s" scope

    if File.Exists(chainFile) then
        File.Delete(chainFile)

[<Given>]
let ``a chain file for "([^"]*)" created (\d+) seconds ago with UUID "([^"]*)"``
    (scope: string)
    (seconds: int)
    (uuid: string)
    =
    currentScope <- scope
    let chainFile = sprintf ".execution-chain-%s" scope
    let ts = DateTimeOffset.UtcNow.ToUnixTimeSeconds() - int64 seconds
    File.WriteAllText(chainFile, sprintf "%d %s" ts uuid)

// ---- BDD When steps ----

[<When>]
let ``I run "crane report init" with scope "([^"]*)"`` (scope: string) =
    currentScope <- scope

    RunWithWriter(fun w ->
        let code = runInit scope "test.pdf" "test.md" w
        lastReportPath <- LastOutput.TrimEnd() |> fun s -> if s.Length > 0 then s else ""
        // Parse path from JSON
        try
            let doc = System.Text.Json.JsonDocument.Parse(w.ToString().Trim())
            lastReportPath <- doc.RootElement.GetProperty("path").GetString()
        with _ ->
            ()

        code)

// ---- BDD Then steps ----

[<Then>]
let ``a report file is created in "([^"]*)"`` (dir: string) =
    Assert.True(File.Exists(lastReportPath), sprintf "Report file should exist: %s" lastReportPath)
    Assert.True(lastReportPath.StartsWith(dir), sprintf "Path should start with %s" dir)

[<Then>]
let ``the filename matches the pattern "([^"]*)"`` (_pattern: string) =
    // Verify the path contains the scope, 6-hex UUID segment, and timestamp
    let filename = Path.GetFileName(lastReportPath)
    Assert.True(filename.EndsWith("__audit.md"), sprintf "Filename should end with __audit.md: %s" filename)
    Assert.True(filename.StartsWith(currentScope + "__"), sprintf "Filename should start with scope: %s" filename)
    let parts = filename.Replace("__audit.md", "").Split("__")
    Assert.True(parts.Length >= 3, sprintf "Filename should have at least 3 parts: %s" filename)
    // Last part before audit is timestamp, second part is chain
    let chain = parts.[1]
    Assert.Matches(@"^[0-9a-f]{6}$", chain)

[<Then>]
let ``the JSON output contains the report path`` () =
    let doc = System.Text.Json.JsonDocument.Parse(LastOutput)
    let path = doc.RootElement.GetProperty("path").GetString()
    Assert.NotEmpty(path)

[<Then>]
let ``the report filename contains "([^"]*)" followed by a new 6-hex UUID`` (prefix: string) =
    let filename = Path.GetFileName(lastReportPath)
    Assert.True(filename.Contains(prefix), sprintf "Filename should contain '%s': %s" prefix filename)

[<Then>]
let ``the report filename contains only the new 6-hex UUID .no "([^"]*)".`` (uuid: string) =
    let filename = Path.GetFileName(lastReportPath)
    Assert.DoesNotContain(uuid, filename)
    // Cleanup
    if File.Exists(lastReportPath) then
        File.Delete(lastReportPath)

    let chainFile = sprintf ".execution-chain-%s" currentScope

    if File.Exists(chainFile) then
        File.Delete(chainFile)

// ---- xUnit [<Fact>] unit tests ----

[<Fact>]
let ``TestUnitNewChain_Is6HexChars`` () =
    let chainFile = ".execution-chain-test-new"

    if File.Exists(chainFile) then
        File.Delete(chainFile)

    let chain = getOrExtendChain "test-new"
    File.Delete(chainFile)
    Assert.Matches("^[0-9a-f]{6}$", chain)

[<Fact>]
let ``TestUnitChain_ExtendsWhenFresh`` () =
    let chainFile = ".execution-chain-test-fresh"
    let ts = DateTimeOffset.UtcNow.ToUnixTimeSeconds()
    File.WriteAllText(chainFile, sprintf "%d abc123" ts)
    let chain = getOrExtendChain "test-fresh"
    File.Delete(chainFile)
    Assert.StartsWith("abc123__", chain)

[<Fact>]
let ``TestUnitChain_ResetsWhenStale`` () =
    let chainFile = ".execution-chain-test-stale"
    let ts = DateTimeOffset.UtcNow.ToUnixTimeSeconds() - 60L
    File.WriteAllText(chainFile, sprintf "%d abc123" ts)
    let chain = getOrExtendChain "test-stale"
    File.Delete(chainFile)
    Assert.DoesNotContain("abc123", chain)

[<Fact>]
let ``TestUnitUTC7Timestamp_Format`` () =
    let ts = utc7Timestamp ()
    Assert.Matches(@"^\d{4}-\d{2}-\d{2}--\d{2}-\d{2}$", ts)

[<Fact>]
let ``TestUnitInitReport_CreatesFileInGeneratedReports`` () =
    let result = initReport "test-init" "test.pdf" "test.md"

    match result with
    | Ok path ->
        Assert.True(File.Exists(path))
        File.Delete(path)
        let chainFile = ".execution-chain-test-init"

        if File.Exists(chainFile) then
            File.Delete(chainFile)
    | Error msg -> Assert.Fail(msg)

[<Fact>]
let ``TestUnitFinalizeReport_ReplacesInProgressWithPass`` () =
    let result = initReport "test-finalize" "a.pdf" "a.md"

    match result with
    | Ok path ->
        let finalResult = finalizeReport path "PASS"

        match finalResult with
        | Ok() ->
            let content = File.ReadAllText(path)
            Assert.Contains("Status: PASS", content)
            File.Delete(path)
            let chainFile = ".execution-chain-test-finalize"

            if File.Exists(chainFile) then
                File.Delete(chainFile)
        | Error msg -> Assert.Fail(msg)
    | Error msg -> Assert.Fail(msg)

[<Fact>]
let ``TestUnitFinalizeReport_ErrorsOnMissingFile`` () =
    let result = finalizeReport "nonexistent-report.md" "PASS"
    Assert.Equal(Error(sprintf "Report not found: %s" "nonexistent-report.md"), result)

[<Fact>]
let ``TestUnitRunFinalize_SuccessPath`` () =
    // Init a report first, then finalize it via the command layer
    let initResult = initReport "test-cmd-finalize" "b.pdf" "b.md"

    match initResult with
    | Ok path ->
        use sw = new System.IO.StringWriter()
        let code = runFinalize path "PASS" sw
        Assert.Equal(0, code)
        let json = System.Text.Json.JsonDocument.Parse(sw.ToString().Trim())
        Assert.Equal("PASS", json.RootElement.GetProperty("status").GetString())
        File.Delete(path)
        let chainFile = ".execution-chain-test-cmd-finalize"

        if File.Exists(chainFile) then
            File.Delete(chainFile)
    | Error msg -> Assert.Fail(msg)

[<Fact>]
let ``TestUnitRunInit_ReturnsJsonPath`` () =
    use sw = new System.IO.StringWriter()
    let code = runInit "test-cmd-init" "c.pdf" "c.md" sw
    Assert.Equal(0, code)
    let json = System.Text.Json.JsonDocument.Parse(sw.ToString().Trim())
    let path = json.RootElement.GetProperty("path").GetString()
    Assert.NotEmpty(path)

    if File.Exists(path) then
        File.Delete(path)

    let chainFile = ".execution-chain-test-cmd-init"

    if File.Exists(chainFile) then
        File.Delete(chainFile)

[<Fact>]
let ``TestUnitRunFinalize_ErrorsOnMissingFile`` () =
    use sw = new System.IO.StringWriter()
    let code = runFinalize "nonexistent-cmd-report.md" "PASS" sw
    Assert.Equal(1, code)

[<Fact>]
let ``TestUnitChain_MalformedContent_RestartsChain`` () =
    // Write a chain file with malformed content (not 2 space-separated parts)
    let chainFile = ".execution-chain-test-malformed"
    File.WriteAllText(chainFile, "malformed-no-space")
    let chain = getOrExtendChain "test-malformed"
    File.Delete(chainFile)
    // Should be a fresh 6-hex chain (not contain the malformed content)
    Assert.DoesNotContain("malformed", chain)

[<Fact>]
let ``TestUnitInitReport_CreatesDirectoryWhenMissing`` () =
    // Run initReport from a temp directory so generated-reports/ doesn't exist
    let tempDir = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N").[..7])
    Directory.CreateDirectory(tempDir) |> ignore
    let origDir = Directory.GetCurrentDirectory()

    try
        Directory.SetCurrentDirectory(tempDir)
        let result = initReport "test-dir-create" "x.pdf" "x.md"

        match result with
        | Ok path ->
            Assert.True(File.Exists(path))
            Assert.True(path.StartsWith("generated-reports/"))
        | Error msg -> Assert.Fail(msg)
    finally
        Directory.SetCurrentDirectory(origDir)
        // Clean up temp dir
        if Directory.Exists(tempDir) then
            Directory.Delete(tempDir, true)
        // Clean up chain file that may have been created in origDir
        let chainFile = ".execution-chain-test-dir-create"

        if File.Exists(chainFile) then
            File.Delete(chainFile)
