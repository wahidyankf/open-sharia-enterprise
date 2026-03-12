module DemoBeFsgi.Tests.Integration.Steps.SecuritySteps

open TickSpec
open Xunit
open DemoBeFsgi.Tests.State
open DemoBeFsgi.Tests.Integration.Steps.CommonSteps
open DemoBeFsgi.Tests.Integration.Steps.TokenManagementSteps

[<Given>]
let ``"(.+)" has had the maximum number of failed login attempts`` (username: string) (state: StepState) =
    // Send 5 failed login attempts with a wrong password
    for _ in 1..5 do
        sendPost
            state.Client
            "/api/v1/auth/login"
            $"""{{ "username": "{username}", "password": "WrongPass1234!" }}"""
            None
        |> ignore

    // Get alice's user ID via admin
    registerUser state.Client "tempAdmin_fail" "tempAdmin_fail@example.com" "Str0ng#Admin1"
    |> ignore

    sendPost state.Client "/test/set-admin-role/tempAdmin_fail" "" None |> ignore
    let adminToken, _ = loginUser state.Client "tempAdmin_fail" "Str0ng#Admin1"
    let email = $"{username}@example.com"

    let response, body =
        sendGet state.Client $"/api/v1/admin/users?email={email}" adminToken

    let doc = System.Text.Json.JsonDocument.Parse(body)
    let dataEl = doc.RootElement.GetProperty("data")
    let userId = ref ""

    for item in dataEl.EnumerateArray() do
        try
            let idEl = item.GetProperty("id")
            userId.Value <- idEl.GetString()
        with _ ->
            ()

    { state with
        UserId = Some userId.Value }

[<Then>]
let ``alice's account status should be "(.+)"`` (expectedStatus: string) (state: StepState) =
    // We need to check the account status. We can try to call an admin endpoint or
    // check the error message from login.
    // For "locked": a locked account returns 401 with locked message.
    // The response we have should indicate locked status.
    let body = state.ResponseBody.Value

    match expectedStatus with
    | "locked" ->
        // The response should have an error about locked account or we check status via admin
        Assert.True(
            body.ToLower().Contains("lock") || body.ToLower().Contains("attempt"),
            $"Expected locked account message but got: {body}"
        )
    | "disabled" ->
        Assert.True(
            body.ToLower().Contains("disabl") || int state.Response.Value.StatusCode = 401,
            $"Expected disabled account message but got: {body}"
        )
    | "active" ->
        // Check that we got a successful response or token
        let statusCode = int state.Response.Value.StatusCode

        Assert.True(
            statusCode = 200 || statusCode = 201,
            $"Expected active account (200/201) but got {statusCode}: {body}"
        )
    | _ -> Assert.True(body.Length > 0)

    state

[<Given>]
let ``a user "(.+)" is registered and locked after too many failed logins`` (username: string) (state: StepState) =
    let email = $"{username}@example.com"
    registerUser state.Client username email "Str0ng#Pass1" |> ignore

    // Trigger lockout with wrong password
    for _ in 1..5 do
        sendPost
            state.Client
            "/api/v1/auth/login"
            $"""{{ "username": "{username}", "password": "WrongPass123!" }}"""
            None
        |> ignore

    // Get alice's ID by registering a temp admin and listing users
    registerUser state.Client "tempAdmin_lock" "tempAdmin_lock@example.com" "Str0ng#Admin1"
    |> ignore

    sendPost state.Client "/test/set-admin-role/tempAdmin_lock" "" None |> ignore
    let adminToken, _ = loginUser state.Client "tempAdmin_lock" "Str0ng#Admin1"

    let response, body =
        sendGet state.Client $"/api/v1/admin/users?email={email}" adminToken

    let doc = System.Text.Json.JsonDocument.Parse(body)
    let dataEl = doc.RootElement.GetProperty("data")
    let aliceId = ref ""

    for item in dataEl.EnumerateArray() do
        try
            let idEl = item.GetProperty("id")
            aliceId.Value <- idEl.GetString()
        with _ ->
            ()

    { state with
        UserId = Some aliceId.Value }

[<Given>]
let ``an admin has unlocked alice's account`` (state: StepState) =
    // Register admin and unlock alice
    registerUser state.Client "testadmin_sec" "testadmin_sec@example.com" "Str0ng#Admin1"
    |> ignore

    sendPost state.Client "/test/set-admin-role/testadmin_sec" "" None |> ignore
    let adminToken, _ = loginUser state.Client "testadmin_sec" "Str0ng#Admin1"

    // Find alice's user ID
    let email = "alice@example.com"

    let response, body =
        sendGet state.Client $"/api/v1/admin/users?email={email}" adminToken

    let doc = System.Text.Json.JsonDocument.Parse(body)
    let dataEl = doc.RootElement.GetProperty("data")
    let aliceId = ref ""

    for item in dataEl.EnumerateArray() do
        try
            let idEl = item.GetProperty("id")
            aliceId.Value <- idEl.GetString()
        with _ ->
            ()

    if aliceId.Value <> "" then
        sendPost state.Client $"/api/v1/admin/users/{aliceId.Value}/unlock" "" adminToken
        |> ignore

    { state with
        UserId = Some aliceId.Value }

[<When>]
let ``the admin sends POST /api/v1/admin/users/\{alice_id\}/unlock`` (state: StepState) =
    let adminToken = state.ExtraData |> Map.tryFind "adminToken"
    let aliceId = state.UserId |> Option.defaultValue ""

    let response, responseBody =
        sendPost state.Client $"/api/v1/admin/users/{aliceId}/unlock" "" adminToken

    { state with
        Response = Some response
        ResponseBody = Some responseBody }
