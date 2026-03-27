module DemoBeFsgi.Tests.Integration.Steps.TokenLifecycleSteps

open System
open System.IdentityModel.Tokens.Jwt
open System.Text
open Microsoft.IdentityModel.Tokens
open TickSpec
open Xunit
open DemoBeFsgi.Tests.State
open DemoBeFsgi.Tests.DirectServices
open DemoBeFsgi.Tests.Integration.Steps.CommonSteps

let private makeExpiredRefreshToken (userId: string) =
    let key =
        SymmetricSecurityKey(Encoding.UTF8.GetBytes("dev-jwt-secret-at-least-32-characters-long-for-hmac"))

    let signingCreds = SigningCredentials(key, SecurityAlgorithms.HmacSha256)
    let now = DateTime.UtcNow

    let claims =
        [| Security.Claims.Claim("sub", userId)
           Security.Claims.Claim("jti", Guid.NewGuid().ToString())
           Security.Claims.Claim("token_type", "refresh") |]

    let token =
        JwtSecurityToken(
            issuer = "demo-be-fsharp-giraffe",
            audience = "demo-be-fsharp-giraffe",
            claims = claims,
            notBefore = now.AddDays(-8.0),
            expires = now.AddDays(-1.0),
            signingCredentials = signingCreds
        )

    JwtSecurityTokenHandler().WriteToken(token)

[<When>]
let ``alice sends POST /api/v1/auth/refresh with her refresh token`` (state: StepState) =
    let rt = state.RefreshToken |> Option.defaultValue ""

    let status, body =
        refresh state.UserRepo state.RefreshTokenRepo rt |> Async.RunSynchronously

    { state with
        Response = Some { Status = status; Body = body }
        ResponseBody = Some body }

[<Given>]
let ``alice's refresh token has expired`` (state: StepState) =
    let userId = state.UserId |> Option.defaultValue (Guid.Empty.ToString())
    let expiredToken = makeExpiredRefreshToken userId

    { state with
        RefreshToken = Some expiredToken }

[<Given>]
let ``alice has used her refresh token to get a new token pair`` (state: StepState) =
    let rt = state.RefreshToken |> Option.defaultValue ""

    refresh state.UserRepo state.RefreshTokenRepo rt
    |> Async.RunSynchronously
    |> ignore
    // Preserve the original refresh token in ExtraData for reuse test
    { state with
        ExtraData = state.ExtraData |> Map.add "originalRefreshToken" rt }

[<When>]
let ``alice sends POST /api/v1/auth/refresh with her original refresh token`` (state: StepState) =
    let rt =
        state.ExtraData |> Map.tryFind "originalRefreshToken" |> Option.defaultValue ""

    let status, body =
        refresh state.UserRepo state.RefreshTokenRepo rt |> Async.RunSynchronously

    { state with
        Response = Some { Status = status; Body = body }
        ResponseBody = Some body }

[<Given>]
let ``the user "(.+)" has been deactivated`` (username: string) (state: StepState) =
    deactivate state.UserRepo state.TokenRepo state.AccessToken
    |> Async.RunSynchronously
    |> ignore

    state

[<When>]
let ``alice sends POST /api/v1/auth/logout with her access token`` (state: StepState) =
    let status, body =
        logout state.TokenRepo state.AccessToken |> Async.RunSynchronously

    { state with
        Response = Some { Status = status; Body = body }
        ResponseBody = Some body }

[<When>]
let ``alice sends POST /api/v1/auth/logout-all with her access token`` (state: StepState) =
    let status, body =
        logoutAll state.UserRepo state.TokenRepo state.RefreshTokenRepo state.AccessToken
        |> Async.RunSynchronously

    { state with
        Response = Some { Status = status; Body = body }
        ResponseBody = Some body }

[<Then>]
let ``alice's access token should be invalidated`` (state: StepState) =
    let status, _body =
        getProfile state.UserRepo state.TokenRepo state.AccessToken
        |> Async.RunSynchronously

    Assert.Equal(401, status)
    state

[<Given>]
let ``alice has already logged out once`` (state: StepState) =
    logout state.TokenRepo state.AccessToken |> Async.RunSynchronously |> ignore
    state
