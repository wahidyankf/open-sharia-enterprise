module DemoBeFsgi.Tests.Integration.Steps.ReportingSteps

open System.Text.Json
open TickSpec
open Xunit
open DemoBeFsgi.Tests.State
open DemoBeFsgi.Tests.Integration.Steps.CommonSteps

[<When>]
let ``alice sends GET /api/v1/reports/pl\?from=(.+)&to=(.+)&currency=(.+)``
    (fromDate: string)
    (toDate: string)
    (currency: string)
    (state: StepState)
    =
    let url = $"/api/v1/reports/pl?from={fromDate}&to={toDate}&currency={currency}"
    let response, body = sendGet state.Client url state.AccessToken

    { state with
        Response = Some response
        ResponseBody = Some body }

[<Then>]
let ``the income breakdown should contain "(.+)" with amount "(.+)"``
    (category: string)
    (amount: string)
    (state: StepState)
    =
    let body = state.ResponseBody.Value

    try
        let doc = JsonDocument.Parse(body)
        let breakdownEl = doc.RootElement.GetProperty("income_breakdown")
        let found = ref false

        for item in breakdownEl.EnumerateArray() do
            try
                let catEl = item.GetProperty("category")
                let amtEl = item.GetProperty("amount")

                if catEl.GetString() = category && amtEl.GetString() = amount then
                    found.Value <- true
            with _ ->
                ()

        Assert.True(found.Value, $"Expected income_breakdown to contain '{category}' with amount '{amount}' in: {body}")
    with ex ->
        Assert.Fail($"Could not parse response: {body}. Error: {ex.Message}")

    state

[<Then>]
let ``the expense breakdown should contain "(.+)" with amount "(.+)"``
    (category: string)
    (amount: string)
    (state: StepState)
    =
    let body = state.ResponseBody.Value

    try
        let doc = JsonDocument.Parse(body)
        let breakdownEl = doc.RootElement.GetProperty("expense_breakdown")
        let found = ref false

        for item in breakdownEl.EnumerateArray() do
            try
                let catEl = item.GetProperty("category")
                let amtEl = item.GetProperty("amount")

                if catEl.GetString() = category && amtEl.GetString() = amount then
                    found.Value <- true
            with _ ->
                ()

        Assert.True(
            found.Value,
            $"Expected expense_breakdown to contain '{category}' with amount '{amount}' in: {body}"
        )
    with ex ->
        Assert.Fail($"Could not parse response: {body}. Error: {ex.Message}")

    state
