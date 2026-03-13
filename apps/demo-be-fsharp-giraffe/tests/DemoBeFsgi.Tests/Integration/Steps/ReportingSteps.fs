module DemoBeFsgi.Tests.Integration.Steps.ReportingSteps

open System.Text.Json
open TickSpec
open Xunit
open DemoBeFsgi.Tests.State
open DemoBeFsgi.Tests.DirectServices

[<When>]
let ``alice sends GET /api/v1/reports/pl\?from=(.+)&to=(.+)&currency=(.+)``
    (fromDate: string)
    (toDate: string)
    (currency: string)
    (state: StepState)
    =
    let status, body =
        profitAndLoss state.Db state.AccessToken fromDate toDate currency |> Async.RunSynchronously

    { state with
        Response = Some { Status = status; Body = body }
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
        let mutable found = false

        for prop in breakdownEl.EnumerateObject() do
            if prop.Name = category && prop.Value.GetString() = amount then
                found <- true

        Assert.True(found, $"Expected income_breakdown to contain '{category}' with amount '{amount}' in: {body}")
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
        let mutable found = false

        for prop in breakdownEl.EnumerateObject() do
            if prop.Name = category && prop.Value.GetString() = amount then
                found <- true

        Assert.True(found, $"Expected expense_breakdown to contain '{category}' with amount '{amount}' in: {body}")
    with ex ->
        Assert.Fail($"Could not parse response: {body}. Error: {ex.Message}")

    state
