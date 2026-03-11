module DemoBeFsgi.Tests.Integration.Steps.AuthSteps

open TickSpec
open Xunit
open DemoBeFsgi.Tests.State
open DemoBeFsgi.Tests.Integration.Steps.CommonSteps

[<Given>]
let ``a user "(.+)" is registered and deactivated`` (username: string) (state: StepState) =
    let email = $"{username}@example.com"
    registerUser state.Client username email "Str0ng#Pass1" |> ignore

    // Login to get token (using actual # character)
    let accessToken, _ = loginUser state.Client username "Str0ng#Pass1"

    // Deactivate the account
    sendPost state.Client "/api/v1/users/me/deactivate" "" accessToken |> ignore

    state
