module CraneCli.Tests.Unit.Steps.BddState

/// Shared mutable state for BDD scenario steps.
/// All steps in all modules read/write through these refs so
/// When-step output is visible to Then-steps regardless of which
/// module defines them.
let mutable LastExitCode: int = -1
let mutable LastOutput: string = ""

let RunWithWriter (f: System.IO.TextWriter -> int) =
    use writer = new System.IO.StringWriter()
    let code = f writer
    LastOutput <- writer.ToString().Trim()
    LastExitCode <- code
