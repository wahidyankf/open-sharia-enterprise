module ADemoBeFsgi.Tests.Integration.Steps.HealthSteps

open System.Text.Json
open TickSpec
open Xunit
open ADemoBeFsgi.Tests.State
open ADemoBeFsgi.Tests.DirectServices

[<When>]
let ``an operations engineer sends GET /health`` (state: StepState) =
    let status, body = health ()

    { state with
        Response = Some { Status = status; Body = body }
        ResponseBody = Some body }

[<When>]
let ``an unauthenticated engineer sends GET /health`` (state: StepState) =
    let status, body = health ()

    { state with
        Response = Some { Status = status; Body = body }
        ResponseBody = Some body }

[<Then>]
let ``the health status should be "(.+)"`` (status: string) (state: StepState) =
    let doc = JsonDocument.Parse(state.ResponseBody.Value)
    let actual = doc.RootElement.GetProperty("status").GetString()
    Assert.Equal(status, actual)
    state

[<Then>]
let ``the response should not include detailed component health information`` (state: StepState) =
    let doc = JsonDocument.Parse(state.ResponseBody.Value)
    let mutable hasComponents = false

    for prop in doc.RootElement.EnumerateObject() do
        if prop.Name = "components" || prop.Name = "details" then
            hasComponents <- true

    Assert.False(hasComponents, "Response should not include component details")
    state
