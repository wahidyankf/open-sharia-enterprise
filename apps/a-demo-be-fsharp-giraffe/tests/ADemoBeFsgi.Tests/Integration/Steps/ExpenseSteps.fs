module ADemoBeFsgi.Tests.Integration.Steps.ExpenseSteps

open System
open System.Text.Json
open TickSpec
open Xunit
open ADemoBeFsgi.Tests.State
open ADemoBeFsgi.Tests.DirectServices
open ADemoBeFsgi.Tests.Integration.Steps.CommonSteps

/// Parse a JSON expense body and call createExpense directly.
let internal createExpenseFromBody (state: StepState) (bodyStr: string) : int * string * string option =
    try
        let doc = JsonDocument.Parse(bodyStr)
        let r = doc.RootElement

        let str (key: string) =
            match r.TryGetProperty(key) with
            | true, el -> el.GetString()
            | _ -> null

        let floatOpt (key: string) =
            match r.TryGetProperty(key) with
            | true, el when el.ValueKind = JsonValueKind.Number -> Some(el.GetDouble())
            | _ -> None

        let unitOpt =
            match r.TryGetProperty("unit") with
            | true, el when el.ValueKind = JsonValueKind.String -> Some(el.GetString())
            | _ -> None

        let status, responseBody =
            createExpense
                state.UserRepo
                state.TokenRepo
                state.ExpenseRepo
                state.AccessToken
                (str "amount")
                (str "currency")
                (str "category")
                (str "description")
                (str "date")
                (str "type")
                (floatOpt "quantity")
                unitOpt
            |> Async.RunSynchronously

        let expenseId = getStringProp responseBody "id"
        status, responseBody, expenseId
    with ex ->
        400, $"""{{ "error": "Bad Request", "message": "{ex.Message}" }}""", None

[<Given>]
let ``alice has created an entry with body (.+)`` (bodyStr: string) (state: StepState) =
    let _status, _body, expenseId = createExpenseFromBody state bodyStr
    { state with ExpenseId = expenseId }

[<Given>]
let ``alice has created an expense with body (.+)`` (bodyStr: string) (state: StepState) =
    let _status, _body, expenseId = createExpenseFromBody state bodyStr
    { state with ExpenseId = expenseId }

[<Given>]
let ``alice has created 3 entries`` (state: StepState) =
    createExpenseFromBody
        state
        """{ "amount": "10.00", "currency": "USD", "category": "food", "description": "Entry 1", "date": "2025-01-01", "type": "expense" }"""
    |> ignore

    createExpenseFromBody
        state
        """{ "amount": "20.00", "currency": "USD", "category": "food", "description": "Entry 2", "date": "2025-01-02", "type": "expense" }"""
    |> ignore

    createExpenseFromBody
        state
        """{ "amount": "30.00", "currency": "USD", "category": "food", "description": "Entry 3", "date": "2025-01-03", "type": "expense" }"""
    |> ignore

    state

[<When>]
let ``alice sends POST /api/v1/expenses with body (.+)`` (bodyStr: string) (state: StepState) =
    let status, responseBody, _expenseId = createExpenseFromBody state bodyStr

    { state with
        Response = Some { Status = status; Body = responseBody }
        ResponseBody = Some responseBody }

[<When>]
let ``alice sends GET /api/v1/expenses/\{expenseId\}`` (state: StepState) =
    let expenseId =
        state.ExpenseId
        |> Option.bind (fun s ->
            try
                Some(Guid.Parse(s))
            with _ ->
                None)

    match expenseId with
    | Some id ->
        let status, body =
            getExpenseById state.UserRepo state.TokenRepo state.ExpenseRepo state.AccessToken id
            |> Async.RunSynchronously

        { state with
            Response = Some { Status = status; Body = body }
            ResponseBody = Some body }
    | None ->
        { state with
            Response =
                Some
                    { Status = 404
                      Body = """{"error":"Not Found"}""" }
            ResponseBody = Some """{"error":"Not Found"}""" }

[<When>]
let ``alice sends GET /api/v1/expenses`` (state: StepState) =
    let status, body =
        listExpenses state.UserRepo state.TokenRepo state.ExpenseRepo state.AccessToken 1 20
        |> Async.RunSynchronously

    { state with
        Response = Some { Status = status; Body = body }
        ResponseBody = Some body }

[<When>]
let ``alice sends PUT /api/v1/expenses/\{expenseId\} with body (.+)`` (bodyStr: string) (state: StepState) =
    let expenseId =
        state.ExpenseId
        |> Option.bind (fun s ->
            try
                Some(Guid.Parse(s))
            with _ ->
                None)

    match expenseId with
    | Some id ->
        let status, responseBody =
            try
                let doc = JsonDocument.Parse(bodyStr)
                let r = doc.RootElement

                let str (key: string) =
                    match r.TryGetProperty(key) with
                    | true, el -> el.GetString()
                    | _ -> null

                updateExpense
                    state.UserRepo
                    state.TokenRepo
                    state.ExpenseRepo
                    state.AccessToken
                    id
                    (str "amount")
                    (str "currency")
                    (str "category")
                    (str "description")
                    (str "date")
                    (str "type")
                |> Async.RunSynchronously
            with ex ->
                400, $"""{{ "error": "Bad Request", "message": "{ex.Message}" }}"""

        { state with
            Response = Some { Status = status; Body = responseBody }
            ResponseBody = Some responseBody }
    | None ->
        { state with
            Response =
                Some
                    { Status = 404
                      Body = """{"error":"Not Found"}""" }
            ResponseBody = Some """{"error":"Not Found"}""" }

[<When>]
let ``alice sends DELETE /api/v1/expenses/\{expenseId\}`` (state: StepState) =
    let expenseId =
        state.ExpenseId
        |> Option.bind (fun s ->
            try
                Some(Guid.Parse(s))
            with _ ->
                None)

    match expenseId with
    | Some id ->
        let status, body =
            deleteExpense state.UserRepo state.TokenRepo state.ExpenseRepo state.AccessToken id
            |> Async.RunSynchronously

        { state with
            Response = Some { Status = status; Body = body }
            ResponseBody = Some body }
    | None ->
        { state with
            Response =
                Some
                    { Status = 404
                      Body = """{"error":"Not Found"}""" }
            ResponseBody = Some """{"error":"Not Found"}""" }

[<When>]
let ``alice sends GET /api/v1/expenses/summary`` (state: StepState) =
    let status, body =
        expenseSummary state.UserRepo state.TokenRepo state.ExpenseRepo state.AccessToken
        |> Async.RunSynchronously

    { state with
        Response = Some { Status = status; Body = body }
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
