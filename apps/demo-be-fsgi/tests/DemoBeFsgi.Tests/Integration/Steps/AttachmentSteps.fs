module DemoBeFsgi.Tests.Integration.Steps.AttachmentSteps

open System
open System.IO
open System.Net.Http
open System.Net.Http.Headers
open System.Text.Json
open TickSpec
open Xunit
open DemoBeFsgi.Tests.State
open DemoBeFsgi.Tests.Integration.Steps.CommonSteps
open DemoBeFsgi.Tests.Integration.Steps.ExpenseSteps

let private uploadFile
    (client: HttpClient)
    (url: string)
    (filename: string)
    (contentType: string)
    (data: byte[])
    (token: string option)
    =
    use content = new MultipartFormDataContent()
    use fileContent = new ByteArrayContent(data)
    fileContent.Headers.ContentType <- MediaTypeHeaderValue(contentType)
    content.Add(fileContent, "file", filename)

    let req = new HttpRequestMessage(HttpMethod.Post, url)
    req.Content <- content

    match token with
    | Some t -> req.Headers.Authorization <- AuthenticationHeaderValue("Bearer", t)
    | None -> ()

    let response = client.SendAsync(req) |> Async.AwaitTask |> Async.RunSynchronously

    let responseBody =
        response.Content.ReadAsStringAsync()
        |> Async.AwaitTask
        |> Async.RunSynchronously

    response, responseBody

[<When>]
let ``alice uploads file "(.+)" with content type "(.+)" to POST /api/v1/expenses/\{expenseId\}/attachments``
    (filename: string)
    (contentType: string)
    (state: StepState)
    =
    let expenseId = state.ExpenseId |> Option.defaultValue ""
    let url = $"/api/v1/expenses/{expenseId}/attachments"
    let data = Array.create 1024 0uy // 1KB test file

    let response, responseBody =
        uploadFile state.Client url filename contentType data state.AccessToken

    let attachmentId = getStringProp responseBody "id"

    { state with
        Response = Some response
        ResponseBody = Some responseBody
        AttachmentId = attachmentId }

[<When>]
let ``alice uploads file "(.+)" with content type "(.+)" to POST /api/v1/expenses/\{bobExpenseId\}/attachments``
    (filename: string)
    (contentType: string)
    (state: StepState)
    =
    let bobExpenseId =
        state.ExtraData |> Map.tryFind "bobExpenseId" |> Option.defaultValue ""

    let url = $"/api/v1/expenses/{bobExpenseId}/attachments"
    let data = Array.create 1024 0uy

    let response, responseBody =
        uploadFile state.Client url filename contentType data state.AccessToken

    { state with
        Response = Some response
        ResponseBody = Some responseBody }

[<When>]
let ``alice uploads an oversized file to POST /api/v1/expenses/\{expenseId\}/attachments`` (state: StepState) =
    let expenseId = state.ExpenseId |> Option.defaultValue ""
    let url = $"/api/v1/expenses/{expenseId}/attachments"
    let data = Array.create (11 * 1024 * 1024) 0uy // 11MB - over the 10MB limit

    let response, responseBody =
        uploadFile state.Client url "large.jpg" "image/jpeg" data state.AccessToken

    { state with
        Response = Some response
        ResponseBody = Some responseBody }

[<Given>]
let ``alice has uploaded file "(.+)" with content type "(.+)" to the entry``
    (filename: string)
    (contentType: string)
    (state: StepState)
    =
    let expenseId = state.ExpenseId |> Option.defaultValue ""
    let url = $"/api/v1/expenses/{expenseId}/attachments"
    let data = Array.create 1024 0uy

    let response, responseBody =
        uploadFile state.Client url filename contentType data state.AccessToken

    let attachmentId = getStringProp responseBody "id"

    { state with
        AttachmentId = attachmentId }

[<When>]
let ``alice sends GET /api/v1/expenses/\{expenseId\}/attachments`` (state: StepState) =
    let expenseId = state.ExpenseId |> Option.defaultValue ""

    let response, body =
        sendGet state.Client $"/api/v1/expenses/{expenseId}/attachments" state.AccessToken

    { state with
        Response = Some response
        ResponseBody = Some body }

[<When>]
let ``alice sends GET /api/v1/expenses/\{bobExpenseId\}/attachments`` (state: StepState) =
    let bobExpenseId =
        state.ExtraData |> Map.tryFind "bobExpenseId" |> Option.defaultValue ""

    let response, body =
        sendGet state.Client $"/api/v1/expenses/{bobExpenseId}/attachments" state.AccessToken

    { state with
        Response = Some response
        ResponseBody = Some body }

[<When>]
let ``alice sends DELETE /api/v1/expenses/\{expenseId\}/attachments/\{attachmentId\}`` (state: StepState) =
    let expenseId = state.ExpenseId |> Option.defaultValue ""
    let attachmentId = state.AttachmentId |> Option.defaultValue ""

    let response, body =
        sendDelete state.Client $"/api/v1/expenses/{expenseId}/attachments/{attachmentId}" state.AccessToken

    { state with
        Response = Some response
        ResponseBody = Some body }

[<When>]
let ``alice sends DELETE /api/v1/expenses/\{bobExpenseId\}/attachments/\{attachmentId\}`` (state: StepState) =
    let bobExpenseId =
        state.ExtraData |> Map.tryFind "bobExpenseId" |> Option.defaultValue ""

    let attachmentId = state.AttachmentId |> Option.defaultValue ""

    let response, body =
        sendDelete state.Client $"/api/v1/expenses/{bobExpenseId}/attachments/{attachmentId}" state.AccessToken

    { state with
        Response = Some response
        ResponseBody = Some body }

[<When>]
let ``alice sends DELETE /api/v1/expenses/\{expenseId\}/attachments/\{randomAttachmentId\}`` (state: StepState) =
    let expenseId = state.ExpenseId |> Option.defaultValue ""
    let randomId = Guid.NewGuid().ToString()

    let response, body =
        sendDelete state.Client $"/api/v1/expenses/{expenseId}/attachments/{randomId}" state.AccessToken

    { state with
        Response = Some response
        ResponseBody = Some body }

// Note: "a user ... is registered with email ... and password ..." is handled by CommonSteps

[<Given>]
let ``bob has created an entry with body (.+)`` (body: string) (state: StepState) =
    let bobToken, _ = loginUser state.Client "bob" "Str0ng#Pass2"
    let response, responseBody = sendPost state.Client "/api/v1/expenses" body bobToken
    let expenseId = getStringProp responseBody "id" |> Option.defaultValue ""

    { state with
        ExtraData = state.ExtraData |> Map.add "bobExpenseId" expenseId }

[<Then>]
let ``the response body should contain 2 items in the "attachments" array`` (state: StepState) =
    let body = state.ResponseBody.Value

    try
        let doc = JsonDocument.Parse(body)
        let attachmentsEl = doc.RootElement.GetProperty("attachments")
        let count = attachmentsEl.GetArrayLength()
        Assert.Equal(2, count)
    with ex ->
        Assert.Fail($"Could not check attachments count in: {body}. Error: {ex.Message}")

    state

[<Then>]
let ``the response body should contain an attachment with "(.+)" equal to "(.+)"``
    (field: string)
    (expected: string)
    (state: StepState)
    =
    let body = state.ResponseBody.Value

    try
        let doc = JsonDocument.Parse(body)
        let attachmentsEl = doc.RootElement.GetProperty("attachments")
        let found = ref false

        for item in attachmentsEl.EnumerateArray() do
            try
                let el = item.GetProperty(field)

                if el.GetString() = expected then
                    found.Value <- true
            with _ ->
                ()

        Assert.True(found.Value, $"Expected attachment with '{field}' = '{expected}' in: {body}")
    with ex ->
        Assert.Fail($"Could not parse attachments in: {body}. Error: {ex.Message}")

    state
