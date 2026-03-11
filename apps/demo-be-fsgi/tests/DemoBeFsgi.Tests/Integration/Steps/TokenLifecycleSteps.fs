module DemoBeFsgi.Tests.Integration.Steps.TokenLifecycleSteps

open System
open System.IdentityModel.Tokens.Jwt
open System.Text
open Microsoft.IdentityModel.Tokens
open TickSpec
open Xunit
open DemoBeFsgi.Tests.State
open DemoBeFsgi.Tests.Integration.Steps.CommonSteps

let private makeExpiredRefreshToken (userId: string) =
    let key =
        SymmetricSecurityKey(Encoding.UTF8.GetBytes("dev-jwt-secret-at-least-32-characters-long-for-hmac"))

    let signingCreds = SigningCredentials(key, SecurityAlgorithms.HmacSha256)
    let now = DateTime.UtcNow

    let claims =
        [| System.Security.Claims.Claim("sub", userId)
           System.Security.Claims.Claim("jti", Guid.NewGuid().ToString())
           System.Security.Claims.Claim("token_type", "refresh") |]

    let token =
        JwtSecurityToken(
            issuer = "demo-be-fsgi",
            audience = "demo-be-fsgi",
            claims = claims,
            notBefore = now.AddDays(-8.0),
            expires = now.AddDays(-1.0),
            signingCredentials = signingCreds
        )

    JwtSecurityTokenHandler().WriteToken(token)

[<When>]
let ``alice sends POST /api/v1/auth/refresh with her refresh token`` (state: StepState) =
    let rt = state.RefreshToken |> Option.defaultValue ""
    let body = $"""{{ "refresh_token": "{rt}" }}"""
    let response, responseBody = sendPost state.Client "/api/v1/auth/refresh" body None

    { state with
        Response = Some response
        ResponseBody = Some responseBody }

[<Given>]
let ``alice's refresh token has expired`` (state: StepState) =
    let userId = state.UserId |> Option.defaultValue (System.Guid.Empty.ToString())
    let expiredToken = makeExpiredRefreshToken userId

    { state with
        RefreshToken = Some expiredToken }

[<Given>]
let ``alice has used her refresh token to get a new token pair`` (state: StepState) =
    let rt = state.RefreshToken |> Option.defaultValue ""
    let body = $"""{{ "refresh_token": "{rt}" }}"""
    let response, responseBody = sendPost state.Client "/api/v1/auth/refresh" body None
    // Store original refresh token for the next step to use
    // The state still has the original RefreshToken
    { state with
        ExtraData = state.ExtraData |> Map.add "originalRefreshToken" rt }

[<When>]
let ``alice sends POST /api/v1/auth/refresh with her original refresh token`` (state: StepState) =
    let rt =
        state.ExtraData |> Map.tryFind "originalRefreshToken" |> Option.defaultValue ""

    let body = $"""{{ "refresh_token": "{rt}" }}"""
    let response, responseBody = sendPost state.Client "/api/v1/auth/refresh" body None

    { state with
        Response = Some response
        ResponseBody = Some responseBody }

[<Given>]
let ``the user "(.+)" has been deactivated`` (username: string) (state: StepState) =
    // Deactivate using the current access token
    let response, body =
        sendPost state.Client "/api/v1/users/me/deactivate" "" state.AccessToken

    state

[<When>]
let ``alice sends POST /api/v1/auth/logout with her access token`` (state: StepState) =
    let response, responseBody =
        sendPost state.Client "/api/v1/auth/logout" "" state.AccessToken

    { state with
        Response = Some response
        ResponseBody = Some responseBody }

[<When>]
let ``alice sends POST /api/v1/auth/logout-all with her access token`` (state: StepState) =
    let response, responseBody =
        sendPost state.Client "/api/v1/auth/logout-all" "" state.AccessToken

    { state with
        Response = Some response
        ResponseBody = Some responseBody }

[<Then>]
let ``alice's access token should be invalidated`` (state: StepState) =
    let response, body = sendGet state.Client "/api/v1/users/me" state.AccessToken
    Assert.Equal(401, int response.StatusCode)
    state

[<Given>]
let ``alice has already logged out once`` (state: StepState) =
    sendPost state.Client "/api/v1/auth/logout" "" state.AccessToken |> ignore
    state
