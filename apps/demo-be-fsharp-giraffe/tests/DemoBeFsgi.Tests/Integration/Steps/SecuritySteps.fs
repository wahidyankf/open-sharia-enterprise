module DemoBeFsgi.Tests.Integration.Steps.SecuritySteps

open TickSpec
open Xunit
open DemoBeFsgi.Tests.State
open DemoBeFsgi.Tests.DirectServices
open DemoBeFsgi.Tests.Integration.Steps.CommonSteps
open DemoBeFsgi.Tests.Integration.Steps.TokenManagementSteps

[<Given>]
let ``"(.+)" has had the maximum number of failed login attempts`` (username: string) (state: StepState) =
    // Send 5 failed login attempts with a wrong password
    for _ in 1..5 do
        login state.Db username "WrongPass1234!" |> Async.RunSynchronously |> ignore

    // Get alice's user ID via admin: register a temp admin, set admin role, list users
    registerUser state "tempAdmin_fail" "tempAdmin_fail@example.com" "Str0ng#Admin1"
    |> ignore

    setAdminRole state.Db "tempAdmin_fail" |> Async.RunSynchronously |> ignore
    let adminToken, _ = loginUser state "tempAdmin_fail" "Str0ng#Admin1"

    let email = $"{username}@example.com"

    let _status, body =
        listUsers state.Db adminToken 1 20 (Some email) |> Async.RunSynchronously

    let userId = ref ""

    try
        let doc = System.Text.Json.JsonDocument.Parse(body)
        let dataEl = doc.RootElement.GetProperty("content")

        for item in dataEl.EnumerateArray() do
            try
                let idEl = item.GetProperty("id")
                userId.Value <- idEl.GetString()
            with _ ->
                ()
    with _ ->
        ()

    { state with
        UserId = Some userId.Value }

[<Then>]
let ``alice's account status should be "(.+)"`` (expectedStatus: string) (state: StepState) =
    let body = state.ResponseBody.Value

    match expectedStatus with
    | "locked" ->
        Assert.True(
            body.ToLower().Contains("lock") || body.ToLower().Contains("attempt"),
            $"Expected locked account message but got: {body}"
        )
    | "disabled" ->
        Assert.True(
            body.ToLower().Contains("disabl") || state.Response.Value.Status = 401,
            $"Expected disabled account message but got: {body}"
        )
    | "active" ->
        let statusCode = state.Response.Value.Status

        Assert.True(
            statusCode = 200 || statusCode = 201,
            $"Expected active account (200/201) but got {statusCode}: {body}"
        )
    | _ -> Assert.True(body.Length > 0)

    state

[<Given>]
let ``a user "(.+)" is registered and locked after too many failed logins`` (username: string) (state: StepState) =
    let email = $"{username}@example.com"
    registerUser state username email "Str0ng#Pass1" |> ignore

    // Trigger lockout with wrong password
    for _ in 1..5 do
        login state.Db username "WrongPass123!" |> Async.RunSynchronously |> ignore

    // Get alice's ID by registering a temp admin and listing users
    registerUser state "tempAdmin_lock" "tempAdmin_lock@example.com" "Str0ng#Admin1"
    |> ignore

    setAdminRole state.Db "tempAdmin_lock" |> Async.RunSynchronously |> ignore
    let adminToken, _ = loginUser state "tempAdmin_lock" "Str0ng#Admin1"

    let _status, body =
        listUsers state.Db adminToken 1 20 (Some email) |> Async.RunSynchronously

    let aliceId = ref ""

    try
        let doc = System.Text.Json.JsonDocument.Parse(body)
        let dataEl = doc.RootElement.GetProperty("content")

        for item in dataEl.EnumerateArray() do
            try
                let idEl = item.GetProperty("id")
                aliceId.Value <- idEl.GetString()
            with _ ->
                ()
    with _ ->
        ()

    { state with
        UserId = Some aliceId.Value }

[<Given>]
let ``an admin has unlocked alice's account`` (state: StepState) =
    // Register admin and unlock alice
    registerUser state "testadmin_sec" "testadmin_sec@example.com" "Str0ng#Admin1"
    |> ignore

    setAdminRole state.Db "testadmin_sec" |> Async.RunSynchronously |> ignore
    let adminToken, _ = loginUser state "testadmin_sec" "Str0ng#Admin1"

    // Find alice's user ID
    let email = "alice@example.com"

    let _status, body =
        listUsers state.Db adminToken 1 20 (Some email) |> Async.RunSynchronously

    let aliceId = ref ""

    try
        let doc = System.Text.Json.JsonDocument.Parse(body)
        let dataEl = doc.RootElement.GetProperty("content")

        for item in dataEl.EnumerateArray() do
            try
                let idEl = item.GetProperty("id")
                aliceId.Value <- idEl.GetString()
            with _ ->
                ()
    with _ ->
        ()

    if aliceId.Value <> "" then
        let aliceGuid =
            try
                Some(System.Guid.Parse(aliceId.Value))
            with _ ->
                None

        match aliceGuid with
        | Some id -> unlockUser state.Db adminToken id |> Async.RunSynchronously |> ignore
        | None -> ()

    { state with
        UserId = Some aliceId.Value }

[<When>]
let ``the admin sends POST /api/v1/admin/users/\{alice_id\}/unlock`` (state: StepState) =
    let adminToken = state.ExtraData |> Map.tryFind "adminToken"

    let aliceGuid =
        state.UserId
        |> Option.bind (fun s ->
            try
                Some(System.Guid.Parse(s))
            with _ ->
                None)

    match aliceGuid with
    | Some id ->
        let status, body = unlockUser state.Db adminToken id |> Async.RunSynchronously

        { state with
            Response = Some { Status = status; Body = body }
            ResponseBody = Some body }
    | None ->
        { state with
            Response =
                Some
                    { Status = 404
                      Body = """{"error":"Not Found"}""" }
            ResponseBody = Some """{"error":"Not Found"}""" }
