module DemoBeFsgi.Tests.Integration.Steps.UserAccountSteps

open TickSpec
open Xunit
open DemoBeFsgi.Tests.State
open DemoBeFsgi.Tests.Integration.Steps.CommonSteps

[<When>]
let ``alice sends GET /api/v1/users/me`` (state: StepState) =
    let response, body = sendGet state.Client "/api/v1/users/me" state.AccessToken

    { state with
        Response = Some response
        ResponseBody = Some body }

[<When>]
let ``alice sends PATCH /api/v1/users/me with body (.+)`` (body: string) (state: StepState) =
    let response, responseBody =
        sendPatch state.Client "/api/v1/users/me" body state.AccessToken

    { state with
        Response = Some response
        ResponseBody = Some responseBody }

[<When>]
let ``alice sends POST /api/v1/users/me/password with body (.+)`` (body: string) (state: StepState) =
    let response, responseBody =
        sendPost state.Client "/api/v1/users/me/password" (decode body) state.AccessToken

    { state with
        Response = Some response
        ResponseBody = Some responseBody }

[<When>]
let ``alice sends POST /api/v1/users/me/deactivate`` (state: StepState) =
    let response, responseBody =
        sendPost state.Client "/api/v1/users/me/deactivate" "" state.AccessToken

    { state with
        Response = Some response
        ResponseBody = Some responseBody }

[<Given>]
let ``alice has deactivated her own account via POST /api/v1/users/me/deactivate`` (state: StepState) =
    sendPost state.Client "/api/v1/users/me/deactivate" "" state.AccessToken
    |> ignore

    state
