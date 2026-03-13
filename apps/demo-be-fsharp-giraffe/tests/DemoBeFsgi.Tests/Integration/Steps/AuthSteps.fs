module DemoBeFsgi.Tests.Integration.Steps.AuthSteps

open TickSpec
open DemoBeFsgi.Tests.State
open DemoBeFsgi.Tests.DirectServices
open DemoBeFsgi.Tests.Integration.Steps.CommonSteps

[<Given>]
let ``a user "(.+)" is registered and deactivated`` (username: string) (state: StepState) =
    let email = $"{username}@example.com"
    registerUser state username email "Str0ng#Pass1" |> ignore

    // Login to get token
    let accessToken, _ = loginUser state username "Str0ng#Pass1"

    // Deactivate the account
    deactivate state.Db accessToken |> Async.RunSynchronously |> ignore

    state
