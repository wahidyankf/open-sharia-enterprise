module DemoBeFsgi.Tests.Integration.Steps.TokenManagementSteps

open System.IdentityModel.Tokens.Jwt
open System.Text.Json
open TickSpec
open Xunit
open DemoBeFsgi.Tests.State
open DemoBeFsgi.Tests.DirectServices
open DemoBeFsgi.Tests.Integration.Steps.CommonSteps

[<When>]
let ``alice decodes her access token payload`` (state: StepState) =
    let token = state.AccessToken |> Option.defaultValue ""
    let handler = JwtSecurityTokenHandler()

    let claims =
        try
            let jwt = handler.ReadJwtToken(token)
            jwt.Claims |> Seq.map (fun c -> c.Type, c.Value) |> Map.ofSeq |> Some
        with _ ->
            None

    let claimsJson =
        claims |> Option.map (fun m -> JsonSerializer.Serialize(m))

    { state with ResponseBody = claimsJson }

[<Then>]
let ``the token should contain a non-null "(.+)" claim`` (claim: string) (state: StepState) =
    let body = state.ResponseBody.Value

    let doc =
        try
            JsonDocument.Parse(body) |> Some
        with _ ->
            None

    match doc with
    | None -> Assert.Fail($"Could not parse claims body: {body}")
    | Some d ->
        let hasProperty =
            d.RootElement.EnumerateObject() |> Seq.exists (fun p -> p.Name = claim)

        if not hasProperty then
            let aliases =
                match claim with
                | "sub" ->
                    [ "http://schemas.xmlsoap.org/ws/2005/05/identity/claims/nameidentifier"
                      "sub" ]
                | "iss" -> [ "iss" ]
                | _ -> [ claim ]

            let found =
                aliases
                |> List.exists (fun a -> d.RootElement.EnumerateObject() |> Seq.exists (fun p -> p.Name = a))

            Assert.True(found, $"Claim '{claim}' not found in token. Claims: {body}")

    state

[<Then>]
let ``the response body should contain at least one key in the "keys" array`` (state: StepState) =
    let body = state.ResponseBody.Value

    try
        let doc = JsonDocument.Parse(body)
        let keysEl = doc.RootElement.GetProperty("keys")
        let count = keysEl.GetArrayLength()
        Assert.True(count > 0, $"Expected at least one key in 'keys' array, got {count}. Body: {body}")
    with ex ->
        Assert.Fail($"Could not find 'keys' array in response: {body}. Error: {ex.Message}")

    state

[<Then>]
let ``alice's access token should be recorded as revoked`` (state: StepState) =
    // Attempt to use the token — it should be rejected because it was revoked
    let status, _body = getProfile state.Db state.AccessToken |> Async.RunSynchronously
    Assert.Equal(401, status)
    state

[<Given>]
let ``alice has logged out and her access token is blacklisted`` (state: StepState) =
    logout state.Db state.AccessToken |> Async.RunSynchronously |> ignore
    state

[<Given>]
let ``an admin user "(.+)" is registered and logged in`` (adminName: string) (state: StepState) =
    let email = $"{adminName}@example.com"
    registerUser state adminName email "Str0ng#Admin1" |> ignore

    // Use direct service to set admin role (test-only)
    setAdminRole state.Db adminName |> Async.RunSynchronously |> ignore

    let accessToken, _ = loginUser state adminName "Str0ng#Admin1"

    // Get user ID
    let _status, body = getProfile state.Db accessToken |> Async.RunSynchronously
    let userId = getStringProp body "id" |> Option.defaultValue ""

    { state with
        ExtraData =
            state.ExtraData
            |> Map.add "adminToken" (accessToken |> Option.defaultValue "")
            |> Map.add "adminUserId" userId }

[<Given>]
let ``the admin has disabled alice's account via POST /api/v1/admin/users/\{alice_id\}/disable``
    (state: StepState)
    =
    let aliceId =
        state.UserId
        |> Option.bind (fun s ->
            try
                Some(System.Guid.Parse(s))
            with _ ->
                None)

    let adminToken = state.ExtraData |> Map.tryFind "adminToken"

    match aliceId with
    | Some id -> disableUser state.Db adminToken id |> Async.RunSynchronously |> ignore
    | None -> ()

    state
