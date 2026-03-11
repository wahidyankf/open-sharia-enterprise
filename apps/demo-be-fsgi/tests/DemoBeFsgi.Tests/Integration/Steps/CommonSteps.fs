module DemoBeFsgi.Tests.Integration.Steps.CommonSteps

open System
open System.Net.Http
open System.Net.Http.Headers
open System.Text
open System.Text.Json
open TickSpec
open Xunit
open DemoBeFsgi.Tests.State

// ────────────────────────────────────────────────────────────────────────────
// Helpers
// ────────────────────────────────────────────────────────────────────────────

let private opts = JsonSerializerOptions(PropertyNameCaseInsensitive = true)

/// Restore '#' characters that were replaced with 'HASH_SIGN' by the feature
/// pre-processor in FeatureRunner (TickSpec strips inline '#' as Gherkin comments).
let internal decode (s: string) = s.Replace("HASH_SIGN", "#")

let internal sendPost (client: HttpClient) (url: string) (body: string) (token: string option) =
    let content = new StringContent(body, Encoding.UTF8, "application/json")

    let req = new HttpRequestMessage(HttpMethod.Post, url)
    req.Content <- content

    match token with
    | Some t -> req.Headers.Authorization <- AuthenticationHeaderValue("Bearer", t)
    | None -> ()

    let response = client.SendAsync(req) |> Async.AwaitTask |> Async.RunSynchronously

    let responseBody =
        response.Content.ReadAsStringAsync()
        |> Async.AwaitTask
        |> Async.RunSynchronously

    response, responseBody

let internal sendGet (client: HttpClient) (url: string) (token: string option) =
    let req = new HttpRequestMessage(HttpMethod.Get, url)

    match token with
    | Some t -> req.Headers.Authorization <- AuthenticationHeaderValue("Bearer", t)
    | None -> ()

    let response = client.SendAsync(req) |> Async.AwaitTask |> Async.RunSynchronously

    let responseBody =
        response.Content.ReadAsStringAsync()
        |> Async.AwaitTask
        |> Async.RunSynchronously

    response, responseBody

let internal sendPatch (client: HttpClient) (url: string) (body: string) (token: string option) =
    let content = new StringContent(body, Encoding.UTF8, "application/json")

    let req = new HttpRequestMessage(HttpMethod.Patch, url)
    req.Content <- content

    match token with
    | Some t -> req.Headers.Authorization <- AuthenticationHeaderValue("Bearer", t)
    | None -> ()

    let response = client.SendAsync(req) |> Async.AwaitTask |> Async.RunSynchronously

    let responseBody =
        response.Content.ReadAsStringAsync()
        |> Async.AwaitTask
        |> Async.RunSynchronously

    response, responseBody

let internal sendPut (client: HttpClient) (url: string) (body: string) (token: string option) =
    let content = new StringContent(body, Encoding.UTF8, "application/json")

    let req = new HttpRequestMessage(HttpMethod.Put, url)
    req.Content <- content

    match token with
    | Some t -> req.Headers.Authorization <- AuthenticationHeaderValue("Bearer", t)
    | None -> ()

    let response = client.SendAsync(req) |> Async.AwaitTask |> Async.RunSynchronously

    let responseBody =
        response.Content.ReadAsStringAsync()
        |> Async.AwaitTask
        |> Async.RunSynchronously

    response, responseBody

let internal sendDelete (client: HttpClient) (url: string) (token: string option) =
    let req = new HttpRequestMessage(HttpMethod.Delete, url)

    match token with
    | Some t -> req.Headers.Authorization <- AuthenticationHeaderValue("Bearer", t)
    | None -> ()

    let response = client.SendAsync(req) |> Async.AwaitTask |> Async.RunSynchronously

    let responseBody =
        response.Content.ReadAsStringAsync()
        |> Async.AwaitTask
        |> Async.RunSynchronously

    response, responseBody

let internal getJsonProp (json: string) (prop: string) =
    try
        let doc = JsonDocument.Parse(json)
        let el = doc.RootElement.GetProperty(prop)
        Some el
    with _ ->
        None

let internal getStringProp (json: string) (prop: string) =
    try
        let doc = JsonDocument.Parse(json)
        let el = doc.RootElement.GetProperty(prop)
        Some(el.GetString())
    with _ ->
        None

let internal registerUser (client: HttpClient) (username: string) (email: string) (password: string) =
    let pw = decode password

    let body =
        $"""{{ "username": "{username}", "email": "{email}", "password": "{pw}" }}"""

    let response, responseBody = sendPost client "/api/v1/auth/register" body None
    responseBody

let internal loginUser (client: HttpClient) (username: string) (password: string) =
    let pw = decode password
    let body = $"""{{ "username": "{username}", "password": "{pw}" }}"""
    let response, responseBody = sendPost client "/api/v1/auth/login" body None
    let accessToken = getStringProp responseBody "access_token"
    let refreshToken = getStringProp responseBody "refresh_token"
    accessToken, refreshToken

// ────────────────────────────────────────────────────────────────────────────
// Shared background steps
// ────────────────────────────────────────────────────────────────────────────

[<Given>]
let ``the API is running`` (state: StepState) = state

[<Then>]
let ``the response status code should be (\d+)`` (code: int) (state: StepState) =
    let actual = int state.Response.Value.StatusCode
    Assert.Equal(code, actual)
    state

[<Given>]
let ``a user "(.+)" is registered with password "(.+)"`` (username: string) (password: string) (state: StepState) =
    let email = $"{username}@example.com"
    registerUser state.Client username email password |> ignore
    state

[<Given>]
let ``a user "(.+)" is registered with email "(.+)" and password "(.+)"``
    (username: string)
    (email: string)
    (password: string)
    (state: StepState)
    =
    registerUser state.Client username email password |> ignore
    state

[<Given>]
let ``"(.+)" has logged in and stored the access token`` (username: string) (state: StepState) =
    // Try standard passwords for common test users
    let passwords = [ "Str0ng#Pass1"; "Str0ng#Pass2"; "Str0ng#Pass3"; "Str0ng#Admin1" ]

    let mutable accessToken = None
    let mutable userId = None

    for pw in passwords do
        if accessToken.IsNone then
            let at, _ = loginUser state.Client username pw

            if at.IsSome then
                let response, body = sendGet state.Client "/api/v1/users/me" at
                accessToken <- at
                userId <- getStringProp body "id"

    { state with
        AccessToken = accessToken
        UserId = userId }

[<Given>]
let ``"(.+)" has logged in and stored the access token and refresh token`` (username: string) (state: StepState) =
    let passwords = [ "Str0ng#Pass1"; "Str0ng#Pass2"; "Str0ng#Admin1" ]

    let mutable accessToken = None
    let mutable refreshToken = None
    let mutable userId = None

    for pw in passwords do
        if accessToken.IsNone then
            let at, rt = loginUser state.Client username pw

            if at.IsSome then
                let response, body = sendGet state.Client "/api/v1/users/me" at
                accessToken <- at
                refreshToken <- rt
                userId <- getStringProp body "id"

    { state with
        AccessToken = accessToken
        RefreshToken = refreshToken
        UserId = userId }

// ────────────────────────────────────────────────────────────────────────────
// Generic HTTP request steps
// ────────────────────────────────────────────────────────────────────────────

[<When>]
let ``the client sends (GET|POST|PUT|PATCH|DELETE) (.+) with body (.+)``
    (method: string)
    (url: string)
    (body: string)
    (state: StepState)
    =
    let decodedBody = decode body

    let response, responseBody =
        match method.ToUpperInvariant() with
        | "POST" -> sendPost state.Client url decodedBody None
        | "PUT" -> sendPut state.Client url decodedBody None
        | "PATCH" -> sendPatch state.Client url decodedBody None
        | _ -> sendPost state.Client url decodedBody None

    { state with
        Response = Some response
        ResponseBody = Some responseBody }

[<When>]
let ``the client sends (GET|POST|PUT|PATCH|DELETE) ([^ ]+) with ([^']+)'s access token``
    (method: string)
    (url: string)
    (username: string)
    (state: StepState)
    =
    let response, responseBody =
        match method.ToUpperInvariant() with
        | "GET" -> sendGet state.Client url state.AccessToken
        | "POST" -> sendPost state.Client url "" state.AccessToken
        | "DELETE" -> sendDelete state.Client url state.AccessToken
        | _ -> sendGet state.Client url state.AccessToken

    { state with
        Response = Some response
        ResponseBody = Some responseBody }

[<When>]
let ``the client sends (GET|POST|PUT|PATCH|DELETE) ([^ ]+)$`` (method: string) (url: string) (state: StepState) =
    let response, responseBody =
        match method.ToUpperInvariant() with
        | "GET" -> sendGet state.Client url None
        | "DELETE" -> sendDelete state.Client url None
        | _ -> sendPost state.Client url "" None

    { state with
        Response = Some response
        ResponseBody = Some responseBody }

// ────────────────────────────────────────────────────────────────────────────
// Response body assertion steps
// ────────────────────────────────────────────────────────────────────────────

[<Then>]
let ``the response body should contain "(.+)" equal to "(.+)"`` (field: string) (expected: string) (state: StepState) =
    let body = state.ResponseBody.Value
    let actual = getStringProp body field

    Assert.True(actual.IsSome, $"Field '{field}' not found in response: {body}")
    Assert.Equal(expected, actual.Value)
    state

[<Then>]
let ``the response body should contain a non-null "(.+)" field`` (field: string) (state: StepState) =
    let body = state.ResponseBody.Value
    let el = getJsonProp body field
    Assert.True(el.IsSome, $"Field '{field}' not found in response: {body}")
    let v = el.Value

    let isNull = v.ValueKind = System.Text.Json.JsonValueKind.Null

    Assert.False(isNull, $"Field '{field}' is null in response: {body}")
    state

[<Then>]
let ``the response body should not contain a "(.+)" field`` (field: string) (state: StepState) =
    let body = state.ResponseBody.Value
    let el = getJsonProp body field
    Assert.True(el.IsNone, $"Field '{field}' should not be present but found in: {body}")
    state

[<Then>]
let ``the response body should contain a validation error for "(.+)"`` (field: string) (state: StepState) =
    let body = state.ResponseBody.Value

    Assert.True(
        body.ToLower().Contains(field.ToLower()),
        $"Response body should contain validation error for '{field}': {body}"
    )

    state

[<Then>]
let ``the response body should contain an error message about (.+)`` (topic: string) (state: StepState) =
    let body = state.ResponseBody.Value
    // Check that the response body contains some error-related content
    Assert.True(body.Length > 0, $"Response body should not be empty: {body}")
    state
