module ADemoBeFsgi.Handlers.AttachmentHandler

open System
open Giraffe
open ADemoBeFsgi.Infrastructure.AppDbContext
open ADemoBeFsgi.Infrastructure.Repositories.RepositoryTypes
open ADemoBeFsgi.Domain.Attachment

let upload (expenseId: Guid) : HttpHandler =
    fun _next ctx ->
        task {
            let userId = ctx.Items["UserId"] :?> Guid
            let expenseRepo = ctx.GetService<ExpenseRepository>()

            let! expenseOpt = expenseRepo.FindById expenseId

            match expenseOpt with
            | None ->
                ctx.Response.StatusCode <- 404

                return!
                    json
                        {| error = "Not Found"
                           message = "Expense not found" |}
                        earlyReturn
                        ctx
            | Some expense when expense.UserId <> userId ->
                ctx.Response.StatusCode <- 403

                return!
                    json
                        {| error = "Forbidden"
                           message = "Access denied" |}
                        earlyReturn
                        ctx
            | Some _ ->
                let form =
                    try
                        ctx.Request.ReadFormAsync() |> Async.AwaitTask |> Async.RunSynchronously |> Some
                    with _ ->
                        None

                match form with
                | None ->
                    ctx.Response.StatusCode <- 400

                    return!
                        json
                            {| error = "Bad Request"
                               message = "Expected multipart form data" |}
                            earlyReturn
                            ctx
                | Some f ->
                    let files = f.Files

                    if files.Count = 0 then
                        ctx.Response.StatusCode <- 400

                        return!
                            json
                                {| error = "Bad Request"
                                   message = "No file uploaded" |}
                                earlyReturn
                                ctx
                    else
                        let file = files[0]
                        let contentType = file.ContentType

                        match validateContentType contentType with
                        | Error(ADemoBeFsgi.Domain.Types.UnsupportedMediaType m) ->
                            ctx.Response.StatusCode <- 415

                            return!
                                json
                                    {| error = "Unsupported Media Type"
                                       field = "file"
                                       message = m |}
                                    earlyReturn
                                    ctx
                        | Error _ ->
                            ctx.Response.StatusCode <- 415

                            return!
                                json
                                    {| error = "Unsupported Media Type"
                                       field = "file"
                                       message = "Unsupported content type" |}
                                    earlyReturn
                                    ctx
                        | Ok _ ->
                            match validateFileSize file.Length with
                            | Error(ADemoBeFsgi.Domain.Types.FileTooLarge limit) ->
                                ctx.Response.StatusCode <- 413

                                return!
                                    json
                                        {| error = "File Too Large"
                                           message = $"File exceeds maximum size of {limit} bytes" |}
                                        earlyReturn
                                        ctx
                            | Error _ ->
                                ctx.Response.StatusCode <- 413

                                return!
                                    json
                                        {| error = "File Too Large"
                                           message = "File is too large" |}
                                        earlyReturn
                                        ctx
                            | Ok _ ->
                                use ms = new IO.MemoryStream()
                                do! file.CopyToAsync(ms)
                                let data = ms.ToArray()
                                let attachmentId = Guid.NewGuid()
                                let now = DateTime.UtcNow
                                let url = $"/api/v1/expenses/{expenseId}/attachments/{attachmentId}/file"

                                let entity: AttachmentEntity =
                                    { Id = attachmentId
                                      ExpenseId = expenseId
                                      Filename = file.FileName
                                      ContentType = contentType
                                      Size = file.Length
                                      Data = data
                                      CreatedAt = now }

                                let attachmentRepo = ctx.GetService<AttachmentRepository>()
                                let! _ = attachmentRepo.Create entity

                                ctx.Response.StatusCode <- 201

                                return!
                                    json
                                        {| id = attachmentId
                                           filename = entity.Filename
                                           contentType = entity.ContentType
                                           size = entity.Size
                                           url = url |}
                                        earlyReturn
                                        ctx
        }

let list (expenseId: Guid) : HttpHandler =
    fun next ctx ->
        task {
            let userId = ctx.Items["UserId"] :?> Guid
            let expenseRepo = ctx.GetService<ExpenseRepository>()

            let! expenseOpt = expenseRepo.FindById expenseId

            match expenseOpt with
            | None ->
                ctx.Response.StatusCode <- 404

                return!
                    json
                        {| error = "Not Found"
                           message = "Expense not found" |}
                        earlyReturn
                        ctx
            | Some expense when expense.UserId <> userId ->
                ctx.Response.StatusCode <- 403

                return!
                    json
                        {| error = "Forbidden"
                           message = "Access denied" |}
                        earlyReturn
                        ctx
            | Some _ ->
                let attachmentRepo = ctx.GetService<AttachmentRepository>()
                let! attachments = attachmentRepo.ListByExpense expenseId

                let data =
                    attachments
                    |> List.map (fun a ->
                        {| id = a.Id
                           filename = a.Filename
                           contentType = a.ContentType
                           size = a.Size
                           url = sprintf "/api/v1/expenses/%O/attachments/%O/download" expenseId a.Id |})
                    |> List.toArray

                return! json {| attachments = data |} next ctx
        }

let delete (expenseId: Guid, attachmentId: Guid) : HttpHandler =
    fun _next ctx ->
        task {
            let userId = ctx.Items["UserId"] :?> Guid
            let expenseRepo = ctx.GetService<ExpenseRepository>()

            let! expenseOpt = expenseRepo.FindById expenseId

            match expenseOpt with
            | None ->
                ctx.Response.StatusCode <- 404

                return!
                    json
                        {| error = "Not Found"
                           message = "Expense not found" |}
                        earlyReturn
                        ctx
            | Some expense when expense.UserId <> userId ->
                ctx.Response.StatusCode <- 403

                return!
                    json
                        {| error = "Forbidden"
                           message = "Access denied" |}
                        earlyReturn
                        ctx
            | Some _ ->
                let attachmentRepo = ctx.GetService<AttachmentRepository>()
                let! attachmentOpt = attachmentRepo.FindById attachmentId expenseId

                match attachmentOpt with
                | None ->
                    ctx.Response.StatusCode <- 404

                    return!
                        json
                            {| error = "Not Found"
                               message = "Attachment not found" |}
                            earlyReturn
                            ctx
                | Some attachment ->
                    do! attachmentRepo.Delete attachment

                    ctx.Response.StatusCode <- 204
                    return! text "" earlyReturn ctx
        }
