module DemoBeFsgi.Tests.Integration.Steps.AdminSteps

open System
open System.Text.Json
open TickSpec
open Xunit
open DemoBeFsgi.Tests.State
open DemoBeFsgi.Tests.DirectServices
open DemoBeFsgi.Tests.Integration.Steps.CommonSteps
open DemoBeFsgi.Tests.Integration.Steps.TokenManagementSteps

[<Given>]
let ``users "(.+)", "(.+)", and "(.+)" are registered`` (u1: string) (u2: string) (u3: string) (state: StepState) =
    let body1 = registerUser state u1 $"{u1}@example.com" "Str0ng#Pass1"
    registerUser state u2 $"{u2}@example.com" "Str0ng#Pass2" |> ignore
    registerUser state u3 $"{u3}@example.com" "Str0ng#Pass3" |> ignore
    // Store alice (u1) id so admin steps can reference it via state.UserId
    let aliceId = getStringProp body1 "id"
    { state with UserId = aliceId }

[<When>]
let ``the admin sends GET /api/v1/admin/users`` (state: StepState) =
    let adminToken = state.ExtraData |> Map.tryFind "adminToken"
    let status, body = listUsers state.Db adminToken 1 20 None |> Async.RunSynchronously

    { state with
        Response = Some { Status = status; Body = body }
        ResponseBody = Some body }

[<When>]
let ``the admin sends GET /api/v1/admin/users\?search=(.+)`` (email: string) (state: StepState) =
    let adminToken = state.ExtraData |> Map.tryFind "adminToken"

    let status, body =
        listUsers state.Db adminToken 1 20 (Some email) |> Async.RunSynchronously

    { state with
        Response = Some { Status = status; Body = body }
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
        let dataEl = doc.RootElement.GetProperty("content")
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
let ``the admin sends POST /api/v1/admin/users/\{alice_id\}/disable with body (.+)``
    (bodyStr: string)
    (state: StepState)
    =
    let adminToken = state.ExtraData |> Map.tryFind "adminToken"

    let aliceGuid =
        state.UserId
        |> Option.bind (fun s ->
            try
                Some(Guid.Parse(s))
            with _ ->
                None)

    match aliceGuid with
    | Some id ->
        let status, body = disableUser state.Db adminToken id |> Async.RunSynchronously

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

[<Given>]
let ``alice's account has been disabled by the admin`` (state: StepState) =
    let adminToken = state.ExtraData |> Map.tryFind "adminToken"

    let aliceGuid =
        state.UserId
        |> Option.bind (fun s ->
            try
                Some(Guid.Parse(s))
            with _ ->
                None)

    match aliceGuid with
    | Some id -> disableUser state.Db adminToken id |> Async.RunSynchronously |> ignore
    | None -> ()

    state

[<Given>]
let ``alice's account has been disabled`` (state: StepState) =
    let adminToken = state.ExtraData |> Map.tryFind "adminToken"

    let aliceGuid =
        state.UserId
        |> Option.bind (fun s ->
            try
                Some(Guid.Parse(s))
            with _ ->
                None)

    match aliceGuid with
    | Some id -> disableUser state.Db adminToken id |> Async.RunSynchronously |> ignore
    | None -> ()

    state

[<When>]
let ``the admin sends POST /api/v1/admin/users/\{alice_id\}/enable`` (state: StepState) =
    let adminToken = state.ExtraData |> Map.tryFind "adminToken"

    let aliceGuid =
        state.UserId
        |> Option.bind (fun s ->
            try
                Some(Guid.Parse(s))
            with _ ->
                None)

    match aliceGuid with
    | Some id ->
        let status, body = enableUser state.Db adminToken id |> Async.RunSynchronously

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

[<When>]
let ``the admin sends POST /api/v1/admin/users/\{alice_id\}/force-password-reset`` (state: StepState) =
    let adminToken = state.ExtraData |> Map.tryFind "adminToken"

    let aliceGuid =
        state.UserId
        |> Option.bind (fun s ->
            try
                Some(Guid.Parse(s))
            with _ ->
                None)

    match aliceGuid with
    | Some id ->
        let status, body =
            forcePasswordReset state.Db adminToken id |> Async.RunSynchronously

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

// Note: alice's account status step is handled by SecuritySteps.``alice's account status should be "(.+)"``
