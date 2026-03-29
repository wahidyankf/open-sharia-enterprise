module ADemoBeFsgi.Tests.Integration.Steps.ReportingSteps

open System.Text.Json
open TickSpec
open Xunit
open ADemoBeFsgi.Tests.State
open ADemoBeFsgi.Tests.DirectServices

[<When>]
let ``alice sends GET /api/v1/reports/pl\?from=(.+)&to=(.+)&currency=(.+)``
    (fromDate: string)
    (toDate: string)
    (currency: string)
    (state: StepState)
    =
    let status, body =
        profitAndLoss state.UserRepo state.TokenRepo state.ExpenseRepo state.AccessToken fromDate toDate currency
        |> Async.RunSynchronously

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
        let breakdownEl = doc.RootElement.GetProperty("incomeBreakdown")
        let mutable found = false

        for item in breakdownEl.EnumerateArray() do
            let catOk =
                item.TryGetProperty("category")
                |> (fun (ok, el) -> ok && el.GetString() = category)

            let totalOk =
                item.TryGetProperty("total") |> (fun (ok, el) -> ok && el.GetString() = amount)

            if catOk && totalOk then
                found <- true

        Assert.True(
            found,
            $"Expected incomeBreakdown to contain '{{category: {category}, total: {amount}}}' in: {body}"
        )
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
        let breakdownEl = doc.RootElement.GetProperty("expenseBreakdown")
        let mutable found = false

        for item in breakdownEl.EnumerateArray() do
            let catOk =
                item.TryGetProperty("category")
                |> (fun (ok, el) -> ok && el.GetString() = category)

            let totalOk =
                item.TryGetProperty("total") |> (fun (ok, el) -> ok && el.GetString() = amount)

            if catOk && totalOk then
                found <- true

        Assert.True(
            found,
            $"Expected expenseBreakdown to contain '{{category: {category}, total: {amount}}}' in: {body}"
        )
    with ex ->
        Assert.Fail($"Could not parse response: {body}. Error: {ex.Message}")

    state
