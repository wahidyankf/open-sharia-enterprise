module DemoBeFsgi.Tests.Integration.Steps.UserAccountSteps

open System.Text.Json
open TickSpec
open DemoBeFsgi.Tests.State
open DemoBeFsgi.Tests.DirectServices
open DemoBeFsgi.Tests.Integration.Steps.CommonSteps

[<When>]
let ``alice sends GET /api/v1/users/me`` (state: StepState) =
    let status, body =
        getProfile state.UserRepo state.TokenRepo state.AccessToken
        |> Async.RunSynchronously

    { state with
        Response = Some { Status = status; Body = body }
        ResponseBody = Some body }

[<When>]
let ``alice sends PATCH /api/v1/users/me with body (.+)`` (bodyStr: string) (state: StepState) =
    let displayName =
        try
            let doc = JsonDocument.Parse(bodyStr)

            match doc.RootElement.TryGetProperty("displayName") with
            | true, el -> el.GetString()
            | _ -> null
        with _ ->
            null

    let status, body =
        updateProfile state.UserRepo state.TokenRepo state.AccessToken displayName
        |> Async.RunSynchronously

    { state with
        Response = Some { Status = status; Body = body }
        ResponseBody = Some body }

[<When>]
let ``alice sends POST /api/v1/users/me/password with body (.+)`` (bodyStr: string) (state: StepState) =
    let decoded = decode bodyStr

    let oldPw, newPw =
        try
            let doc = JsonDocument.Parse(decoded)
            let r = doc.RootElement

            let str (key: string) =
                match r.TryGetProperty(key) with
                | true, el -> el.GetString()
                | _ -> null

            str "oldPassword", str "newPassword"
        with _ ->
            null, null

    let status, body =
        changePassword state.UserRepo state.TokenRepo state.AccessToken oldPw newPw
        |> Async.RunSynchronously

    { state with
        Response = Some { Status = status; Body = body }
        ResponseBody = Some body }

[<When>]
let ``alice sends POST /api/v1/users/me/deactivate`` (state: StepState) =
    let status, body =
        deactivate state.UserRepo state.TokenRepo state.AccessToken
        |> Async.RunSynchronously

    { state with
        Response = Some { Status = status; Body = body }
        ResponseBody = Some body }

[<Given>]
let ``alice has deactivated her own account via POST /api/v1/users/me/deactivate`` (state: StepState) =
    deactivate state.UserRepo state.TokenRepo state.AccessToken
    |> Async.RunSynchronously
    |> ignore

    state
