module OrganicLeverBe.Tests.Integration.Steps.CommonSteps

open TickSpec
open Xunit
open OrganicLeverBe.Tests.State

// ─────────────────────────────────────────────────────────────────────────────
// Helpers
// ─────────────────────────────────────────────────────────────────────────────

/// Helper to apply a direct service result to StepState.
let internal applyResult (status: int) (body: string) (state: StepState) : StepState =
    { state with
        Response = Some { Status = status; Body = body }
        ResponseBody = Some body }

// ─────────────────────────────────────────────────────────────────────────────
// Shared background steps
// ─────────────────────────────────────────────────────────────────────────────

[<Given>]
let ``the API is running`` (state: StepState) = state

[<Then>]
let ``the response status code should be (\d+)`` (code: int) (state: StepState) =
    let actual = state.Response.Value.Status
    Assert.Equal(code, actual)
    state
