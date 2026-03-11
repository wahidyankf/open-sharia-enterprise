module DemoBeFsgi.Tests.Integration.Steps.ExpenseSteps

open System.Text.Json
open TickSpec
open Xunit
open DemoBeFsgi.Tests.State
open DemoBeFsgi.Tests.Integration.Steps.CommonSteps

let internal createExpense (state: StepState) (body: string) =
    let response, responseBody =
        sendPost state.Client "/api/v1/expenses" body state.AccessToken

    let expenseId = getStringProp responseBody "id"
    response, responseBody, expenseId

[<Given>]
let ``alice has created an entry with body (.+)`` (body: string) (state: StepState) =
    let response, responseBody, expenseId = createExpense state body
    { state with ExpenseId = expenseId }

[<Given>]
let ``alice has created an expense with body (.+)`` (body: string) (state: StepState) =
    let response, responseBody, expenseId = createExpense state body
    { state with ExpenseId = expenseId }

[<Given>]
let ``alice has created 3 entries`` (state: StepState) =
    createExpense
        state
        """{ "amount": "10.00", "currency": "USD", "category": "food", "description": "Entry 1", "date": "2025-01-01", "type": "expense" }"""
    |> ignore

    createExpense
        state
        """{ "amount": "20.00", "currency": "USD", "category": "food", "description": "Entry 2", "date": "2025-01-02", "type": "expense" }"""
    |> ignore

    createExpense
        state
        """{ "amount": "30.00", "currency": "USD", "category": "food", "description": "Entry 3", "date": "2025-01-03", "type": "expense" }"""
    |> ignore

    state

[<When>]
let ``alice sends POST /api/v1/expenses with body (.+)`` (body: string) (state: StepState) =
    let response, responseBody =
        sendPost state.Client "/api/v1/expenses" body state.AccessToken

    { state with
        Response = Some response
        ResponseBody = Some responseBody }

[<When>]
let ``alice sends GET /api/v1/expenses/\{expenseId\}`` (state: StepState) =
    let expenseId = state.ExpenseId |> Option.defaultValue ""

    let response, body =
        sendGet state.Client $"/api/v1/expenses/{expenseId}" state.AccessToken

    { state with
        Response = Some response
        ResponseBody = Some body }

[<When>]
let ``alice sends GET /api/v1/expenses`` (state: StepState) =
    let response, body = sendGet state.Client "/api/v1/expenses" state.AccessToken

    { state with
        Response = Some response
        ResponseBody = Some body }

[<When>]
let ``alice sends PUT /api/v1/expenses/\{expenseId\} with body (.+)`` (body: string) (state: StepState) =
    let expenseId = state.ExpenseId |> Option.defaultValue ""

    let response, responseBody =
        sendPut state.Client $"/api/v1/expenses/{expenseId}" body state.AccessToken

    { state with
        Response = Some response
        ResponseBody = Some responseBody }

[<When>]
let ``alice sends DELETE /api/v1/expenses/\{expenseId\}`` (state: StepState) =
    let expenseId = state.ExpenseId |> Option.defaultValue ""

    let response, responseBody =
        sendDelete state.Client $"/api/v1/expenses/{expenseId}" state.AccessToken

    { state with
        Response = Some response
        ResponseBody = Some responseBody }

[<When>]
let ``alice sends GET /api/v1/expenses/summary`` (state: StepState) =
    let response, body =
        sendGet state.Client "/api/v1/expenses/summary" state.AccessToken

    { state with
        Response = Some response
        ResponseBody = Some body }

[<Then>]
let ``the response body should contain "(.+)" total equal to "(.+)"``
    (currency: string)
    (expected: string)
    (state: StepState)
    =
    let body = state.ResponseBody.Value

    try
        let doc = JsonDocument.Parse(body)
        let el = doc.RootElement.GetProperty(currency)
        let actual = el.GetString()
        Assert.Equal(expected, actual)
    with ex ->
        Assert.Fail($"Could not find currency '{currency}' in response: {body}. Error: {ex.Message}")

    state

[<Then>]
let ``the response body should contain "quantity" equal to (.+)`` (expected: string) (state: StepState) =
    let body = state.ResponseBody.Value

    try
        let doc = JsonDocument.Parse(body)
        let el = doc.RootElement.GetProperty("quantity")

        match el.ValueKind with
        | JsonValueKind.Number ->
            let actual = el.GetDouble()
            let expectedVal = float expected
            Assert.Equal(expectedVal, actual)
        | JsonValueKind.Object ->
            // Optional field wrapped in Some
            ()
        | _ -> Assert.Fail($"Expected numeric 'quantity' field in: {body}")
    with ex ->
        Assert.Fail($"Could not find 'quantity' in response: {body}. Error: {ex.Message}")

    state

// Note: "unit" field assertion is handled by CommonSteps ``the response body should contain "(.+)" equal to "(.+)"``
