module DemoBeFsgi.Tests.Integration.Steps.HealthSteps

open TickSpec
open Xunit
open System.Text.Json
open DemoBeFsgi.Tests.State

[<When>]
let ``an operations engineer sends GET /health`` (state: StepState) =
    let response =
        state.Client.GetAsync("/health") |> Async.AwaitTask |> Async.RunSynchronously

    let body =
        response.Content.ReadAsStringAsync()
        |> Async.AwaitTask
        |> Async.RunSynchronously

    { state with
        Response = Some response
        ResponseBody = Some body }

[<When>]
let ``an unauthenticated engineer sends GET /health`` (state: StepState) =
    let response =
        state.Client.GetAsync("/health") |> Async.AwaitTask |> Async.RunSynchronously

    let body =
        response.Content.ReadAsStringAsync()
        |> Async.AwaitTask
        |> Async.RunSynchronously

    { state with
        Response = Some response
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
