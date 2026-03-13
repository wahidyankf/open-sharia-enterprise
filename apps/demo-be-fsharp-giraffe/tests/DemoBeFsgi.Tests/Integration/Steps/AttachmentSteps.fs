module DemoBeFsgi.Tests.Integration.Steps.AttachmentSteps

open System
open System.Text.Json
open TickSpec
open Xunit
open DemoBeFsgi.Tests.State
open DemoBeFsgi.Tests.DirectServices
open DemoBeFsgi.Tests.Integration.Steps.CommonSteps
open DemoBeFsgi.Tests.Integration.Steps.ExpenseSteps

[<When>]
let ``alice uploads file "(.+)" with content type "(.+)" to POST /api/v1/expenses/\{expenseId\}/attachments``
    (filename: string)
    (contentType: string)
    (state: StepState)
    =
    let expenseId =
        state.ExpenseId
        |> Option.bind (fun s ->
            try
                Some(Guid.Parse(s))
            with _ ->
                None)

    match expenseId with
    | Some id ->
        let data = Array.create 1024 0uy // 1 KB test file

        let status, responseBody =
            uploadAttachment state.Db state.AccessToken id filename contentType data
            |> Async.RunSynchronously

        let attachmentId = getStringProp responseBody "id"

        { state with
            Response = Some { Status = status; Body = responseBody }
            ResponseBody = Some responseBody
            AttachmentId = attachmentId }
    | None ->
        { state with
            Response =
                Some
                    { Status = 404
                      Body = """{"error":"Not Found"}""" }
            ResponseBody = Some """{"error":"Not Found"}""" }

[<When>]
let ``alice uploads file "(.+)" with content type "(.+)" to POST /api/v1/expenses/\{bobExpenseId\}/attachments``
    (filename: string)
    (contentType: string)
    (state: StepState)
    =
    let bobExpenseId =
        state.ExtraData
        |> Map.tryFind "bobExpenseId"
        |> Option.bind (fun s ->
            try
                Some(Guid.Parse(s))
            with _ ->
                None)

    match bobExpenseId with
    | Some id ->
        let data = Array.create 1024 0uy

        let status, responseBody =
            uploadAttachment state.Db state.AccessToken id filename contentType data
            |> Async.RunSynchronously

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
let ``alice uploads an oversized file to POST /api/v1/expenses/\{expenseId\}/attachments`` (state: StepState) =
    let expenseId =
        state.ExpenseId
        |> Option.bind (fun s ->
            try
                Some(Guid.Parse(s))
            with _ ->
                None)

    match expenseId with
    | Some id ->
        let data = Array.create (11 * 1024 * 1024) 0uy // 11 MB — over the 10 MB limit

        let status, responseBody =
            uploadAttachment state.Db state.AccessToken id "large.jpg" "image/jpeg" data
            |> Async.RunSynchronously

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

[<Given>]
let ``alice has uploaded file "(.+)" with content type "(.+)" to the entry``
    (filename: string)
    (contentType: string)
    (state: StepState)
    =
    let expenseId =
        state.ExpenseId
        |> Option.bind (fun s ->
            try
                Some(Guid.Parse(s))
            with _ ->
                None)

    match expenseId with
    | Some id ->
        let data = Array.create 1024 0uy

        let _status, responseBody =
            uploadAttachment state.Db state.AccessToken id filename contentType data
            |> Async.RunSynchronously

        let attachmentId = getStringProp responseBody "id"

        { state with
            AttachmentId = attachmentId }
    | None -> state

[<When>]
let ``alice sends GET /api/v1/expenses/\{expenseId\}/attachments`` (state: StepState) =
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
            listAttachments state.Db state.AccessToken id |> Async.RunSynchronously

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
let ``alice sends GET /api/v1/expenses/\{bobExpenseId\}/attachments`` (state: StepState) =
    let bobExpenseId =
        state.ExtraData
        |> Map.tryFind "bobExpenseId"
        |> Option.bind (fun s ->
            try
                Some(Guid.Parse(s))
            with _ ->
                None)

    match bobExpenseId with
    | Some id ->
        let status, body =
            listAttachments state.Db state.AccessToken id |> Async.RunSynchronously

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
let ``alice sends DELETE /api/v1/expenses/\{expenseId\}/attachments/\{attachmentId\}`` (state: StepState) =
    let expenseId =
        state.ExpenseId
        |> Option.bind (fun s ->
            try
                Some(Guid.Parse(s))
            with _ ->
                None)

    let attachmentId =
        state.AttachmentId
        |> Option.bind (fun s ->
            try
                Some(Guid.Parse(s))
            with _ ->
                None)

    match expenseId, attachmentId with
    | Some eid, Some aid ->
        let status, body =
            deleteAttachment state.Db state.AccessToken eid aid |> Async.RunSynchronously

        { state with
            Response = Some { Status = status; Body = body }
            ResponseBody = Some body }
    | _ ->
        { state with
            Response =
                Some
                    { Status = 404
                      Body = """{"error":"Not Found"}""" }
            ResponseBody = Some """{"error":"Not Found"}""" }

[<When>]
let ``alice sends DELETE /api/v1/expenses/\{bobExpenseId\}/attachments/\{attachmentId\}`` (state: StepState) =
    let bobExpenseId =
        state.ExtraData
        |> Map.tryFind "bobExpenseId"
        |> Option.bind (fun s ->
            try
                Some(Guid.Parse(s))
            with _ ->
                None)

    let attachmentId =
        state.AttachmentId
        |> Option.bind (fun s ->
            try
                Some(Guid.Parse(s))
            with _ ->
                None)

    match bobExpenseId, attachmentId with
    | Some eid, Some aid ->
        let status, body =
            deleteAttachment state.Db state.AccessToken eid aid |> Async.RunSynchronously

        { state with
            Response = Some { Status = status; Body = body }
            ResponseBody = Some body }
    | _ ->
        { state with
            Response =
                Some
                    { Status = 404
                      Body = """{"error":"Not Found"}""" }
            ResponseBody = Some """{"error":"Not Found"}""" }

[<When>]
let ``alice sends DELETE /api/v1/expenses/\{expenseId\}/attachments/\{randomAttachmentId\}`` (state: StepState) =
    let expenseId =
        state.ExpenseId
        |> Option.bind (fun s ->
            try
                Some(Guid.Parse(s))
            with _ ->
                None)

    let randomId = Guid.NewGuid()

    match expenseId with
    | Some eid ->
        let status, body =
            deleteAttachment state.Db state.AccessToken eid randomId
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

// Note: "a user ... is registered with email ... and password ..." is handled by CommonSteps

[<Given>]
let ``bob has created an entry with body (.+)`` (bodyStr: string) (state: StepState) =
    let bobToken, _ = loginUser state "bob" "Str0ng#Pass2"

    let bobState = { state with AccessToken = bobToken }
    let _status, responseBody, expenseId = createExpenseFromBody bobState bodyStr
    let expenseIdStr = expenseId |> Option.defaultValue ""

    { state with
        ExtraData = state.ExtraData |> Map.add "bobExpenseId" expenseIdStr }

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
