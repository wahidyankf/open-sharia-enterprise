module DemoBeFsgi.Tests.Integration.Steps.AdminSteps

open System.Text.Json
open TickSpec
open Xunit
open DemoBeFsgi.Tests.State
open DemoBeFsgi.Tests.Integration.Steps.CommonSteps
open DemoBeFsgi.Tests.Integration.Steps.TokenManagementSteps

[<Given>]
let ``users "(.+)", "(.+)", and "(.+)" are registered`` (u1: string) (u2: string) (u3: string) (state: StepState) =
    let body1 = registerUser state.Client u1 $"{u1}@example.com" "Str0ng#Pass1"
    registerUser state.Client u2 $"{u2}@example.com" "Str0ng#Pass2" |> ignore
    registerUser state.Client u3 $"{u3}@example.com" "Str0ng#Pass3" |> ignore
    // Store alice (u1) id so admin steps can reference it via state.UserId
    let aliceId = getStringProp body1 "id"
    { state with UserId = aliceId }

[<When>]
let ``the admin sends GET /api/v1/admin/users`` (state: StepState) =
    let adminToken = state.ExtraData |> Map.tryFind "adminToken"
    let response, body = sendGet state.Client "/api/v1/admin/users" adminToken

    { state with
        Response = Some response
        ResponseBody = Some body }

[<When>]
let ``the admin sends GET /api/v1/admin/users\?email=(.+)`` (email: string) (state: StepState) =
    let adminToken = state.ExtraData |> Map.tryFind "adminToken"

    let response, body =
        sendGet state.Client $"/api/v1/admin/users?email={email}" adminToken

    { state with
        Response = Some response
        ResponseBody = Some body }

[<Then>]
let ``the response body should contain at least one user with "(.+)" equal to "(.+)"``
    (field: string)
    (expected: string)
    (state: StepState)
    =
    let body = state.ResponseBody.Value

    try
        let doc = JsonDocument.Parse(body)
        let dataEl = doc.RootElement.GetProperty("data")
        let found = ref false

        for item in dataEl.EnumerateArray() do
            try
                let el = item.GetProperty(field)

                if el.GetString() = expected then
                    found.Value <- true
            with _ ->
                ()

        Assert.True(found.Value, $"Expected at least one user with '{field}' = '{expected}' in: {body}")
    with ex ->
        Assert.Fail($"Could not parse response body: {body}. Error: {ex.Message}")

    state

[<When>]
let ``the admin sends POST /api/v1/admin/users/\{alice_id\}/disable with body (.+)`` (body: string) (state: StepState) =
    let adminToken = state.ExtraData |> Map.tryFind "adminToken"
    let aliceId = state.UserId |> Option.defaultValue ""

    let response, responseBody =
        sendPost state.Client $"/api/v1/admin/users/{aliceId}/disable" body adminToken

    { state with
        Response = Some response
        ResponseBody = Some responseBody }

[<Given>]
let ``alice's account has been disabled by the admin`` (state: StepState) =
    let adminToken = state.ExtraData |> Map.tryFind "adminToken"
    let aliceId = state.UserId |> Option.defaultValue ""

    sendPost state.Client $"/api/v1/admin/users/{aliceId}/disable" """{ "reason": "Test disable" }""" adminToken
    |> ignore

    state

[<Given>]
let ``alice's account has been disabled`` (state: StepState) =
    let adminToken = state.ExtraData |> Map.tryFind "adminToken"
    let aliceId = state.UserId |> Option.defaultValue ""

    sendPost state.Client $"/api/v1/admin/users/{aliceId}/disable" """{ "reason": "Test disable" }""" adminToken
    |> ignore

    state

[<When>]
let ``the admin sends POST /api/v1/admin/users/\{alice_id\}/enable`` (state: StepState) =
    let adminToken = state.ExtraData |> Map.tryFind "adminToken"
    let aliceId = state.UserId |> Option.defaultValue ""

    let response, responseBody =
        sendPost state.Client $"/api/v1/admin/users/{aliceId}/enable" "" adminToken

    { state with
        Response = Some response
        ResponseBody = Some responseBody }

[<When>]
let ``the admin sends POST /api/v1/admin/users/\{alice_id\}/force-password-reset`` (state: StepState) =
    let adminToken = state.ExtraData |> Map.tryFind "adminToken"
    let aliceId = state.UserId |> Option.defaultValue ""

    let response, responseBody =
        sendPost state.Client $"/api/v1/admin/users/{aliceId}/force-password-reset" "" adminToken

    { state with
        Response = Some response
        ResponseBody = Some responseBody }

// Note: alice's account status step is handled by SecuritySteps.``alice's account status should be "(.+)"``
