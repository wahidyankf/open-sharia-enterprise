module DemoBeFsgi.Handlers.ExpenseHandler

open System
open System.Text.Json
open Giraffe
open DemoBeFsgi.Infrastructure.AppDbContext
open DemoBeFsgi.Infrastructure.Repositories.RepositoryTypes
open DemoBeFsgi.Domain.Types
open DemoBeFsgi.Domain.Expense
open DemoBeFsgi.Contracts.ContractWrappers

let private parseAmount (s: string) =
    if String.IsNullOrEmpty s then
        Error(ValidationError("amount", "Amount is required"))
    else
        match Decimal.TryParse(s, Globalization.NumberStyles.Any, Globalization.CultureInfo.InvariantCulture) with
        | true, v -> Ok v
        | _ -> Error(ValidationError("amount", "Invalid amount format"))

let create: HttpHandler =
    fun _next ctx ->
        task {
            let! body = ctx.ReadBodyFromRequestAsync()

            let req =
                try
                    JsonSerializer.Deserialize<CreateExpenseRequest>(
                        body,
                        JsonSerializerOptions(PropertyNameCaseInsensitive = true)
                    )
                    |> Some
                with _ ->
                    None

            match req with
            | None ->
                ctx.Response.StatusCode <- 400

                return!
                    json
                        {| error = "Bad Request"
                           message = "Invalid request body" |}
                        earlyReturn
                        ctx
            | Some r ->
                let currencyResult = parseCurrency (if r.currency = null then "" else r.currency)

                match currencyResult with
                | Error(ValidationError(f, m)) ->
                    ctx.Response.StatusCode <- 400

                    return!
                        json
                            {| error = "Validation Error"
                               field = f
                               message = m |}
                            earlyReturn
                            ctx
                | Error _ ->
                    ctx.Response.StatusCode <- 400

                    return!
                        json
                            {| error = "Validation Error"
                               field = "currency"
                               message = "Invalid currency" |}
                            earlyReturn
                            ctx
                | Ok _ ->
                    let amountResult = parseAmount r.amount

                    match amountResult with
                    | Error(ValidationError(f, m)) ->
                        ctx.Response.StatusCode <- 400

                        return!
                            json
                                {| error = "Validation Error"
                                   field = f
                                   message = m |}
                                earlyReturn
                                ctx
                    | Error _ ->
                        ctx.Response.StatusCode <- 400

                        return!
                            json
                                {| error = "Validation Error"
                                   field = "amount"
                                   message = "Invalid amount" |}
                                earlyReturn
                                ctx
                    | Ok amount ->
                        let amountValidation = validateAmount amount

                        match amountValidation with
                        | Error(ValidationError(f, m)) ->
                            ctx.Response.StatusCode <- 400

                            return!
                                json
                                    {| error = "Validation Error"
                                       field = f
                                       message = m |}
                                    earlyReturn
                                    ctx
                        | Error _ ->
                            ctx.Response.StatusCode <- 400

                            return!
                                json
                                    {| error = "Validation Error"
                                       field = "amount"
                                       message = "Invalid amount" |}
                                    earlyReturn
                                    ctx
                        | Ok validAmount ->
                            let unitOpt = if String.IsNullOrEmpty r.unit then None else Some r.unit
                            let unitResult = validateUnit unitOpt

                            match unitResult with
                            | Error(ValidationError(f, m)) ->
                                ctx.Response.StatusCode <- 400

                                return!
                                    json
                                        {| error = "Validation Error"
                                           field = f
                                           message = m |}
                                        earlyReturn
                                        ctx
                            | Error _ ->
                                ctx.Response.StatusCode <- 400

                                return!
                                    json
                                        {| error = "Validation Error"
                                           field = "unit"
                                           message = "Invalid unit" |}
                                        earlyReturn
                                        ctx
                            | Ok validUnit ->
                                let expenseRepo = ctx.GetService<ExpenseRepository>()
                                let userId = ctx.Items["UserId"] :?> Guid

                                let dateVal =
                                    match DateTime.TryParse(r.date) with
                                    | true, d -> DateTime.SpecifyKind(d, DateTimeKind.Utc)
                                    | _ -> DateTime.UtcNow

                                let now = DateTime.UtcNow
                                let expenseId = Guid.NewGuid()

                                let entity: ExpenseEntity =
                                    { Id = expenseId
                                      UserId = userId
                                      Amount = validAmount
                                      Currency = r.currency.ToUpperInvariant()
                                      Category = if r.category = null then "" else r.category
                                      Description = if r.description = null then "" else r.description
                                      Date = dateVal
                                      Type =
                                        if r.``type`` = null then
                                            "EXPENSE"
                                        else
                                            r.``type``.ToUpperInvariant()
                                      Quantity =
                                        if r.quantity.HasValue then
                                            Nullable(decimal r.quantity.Value)
                                        else
                                            Nullable()
                                      Unit =
                                        match validUnit with
                                        | Some u -> u
                                        | None -> null
                                      CreatedAt = now
                                      UpdatedAt = now }

                                let! _ = expenseRepo.Create entity

                                ctx.Response.StatusCode <- 201

                                let formattedAmount =
                                    match r.currency.ToUpperInvariant() with
                                    | "IDR" -> validAmount.ToString("0")
                                    | _ -> validAmount.ToString("0.00")

                                return!
                                    json
                                        {| id = expenseId
                                           userId = userId
                                           amount = formattedAmount
                                           currency = entity.Currency
                                           category = entity.Category
                                           description = entity.Description
                                           date = entity.Date.ToString("yyyy-MM-dd")
                                           ``type`` = entity.Type.ToLowerInvariant() |}
                                        earlyReturn
                                        ctx
        }

let list: HttpHandler =
    fun next ctx ->
        task {
            let userId = ctx.Items["UserId"] :?> Guid
            let expenseRepo = ctx.GetService<ExpenseRepository>()
            let pageParam = ctx.TryGetQueryStringValue("page") |> Option.defaultValue "1"
            let sizeParam = ctx.TryGetQueryStringValue("size") |> Option.defaultValue "20"

            let page =
                Math.Max(
                    1,
                    try
                        int pageParam
                    with _ ->
                        1
                )

            let size =
                Math.Max(
                    1,
                    try
                        int sizeParam
                    with _ ->
                        20
                )

            let! total, expenses = expenseRepo.ListByUser userId page size

            let data =
                expenses
                |> List.map (fun e ->
                    let formattedAmount =
                        match e.Currency with
                        | "IDR" -> e.Amount.ToString("0")
                        | _ -> e.Amount.ToString("0.00")

                    let qtyOpt =
                        if e.Quantity.HasValue then
                            Some(float e.Quantity.Value)
                        else
                            None

                    {| id = e.Id
                       userId = e.UserId
                       amount = formattedAmount
                       currency = e.Currency
                       category = e.Category
                       description = e.Description
                       date = e.Date.ToString("yyyy-MM-dd")
                       ``type`` = e.Type.ToLowerInvariant()
                       quantity = qtyOpt
                       unit = if e.Unit = null then None else Some e.Unit |})
                |> List.toArray

            return!
                json
                    {| content = data
                       totalElements = total
                       page = page |}
                    next
                    ctx
        }

let getById (expenseId: Guid) : HttpHandler =
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
            | Some expense ->
                let formattedAmount =
                    match expense.Currency with
                    | "IDR" -> expense.Amount.ToString("0")
                    | _ -> expense.Amount.ToString("0.00")

                let qtyOpt =
                    if expense.Quantity.HasValue then
                        Some(float expense.Quantity.Value)
                    else
                        None

                return!
                    json
                        {| id = expense.Id
                           userId = expense.UserId
                           amount = formattedAmount
                           currency = expense.Currency
                           category = expense.Category
                           description = expense.Description
                           date = expense.Date.ToString("yyyy-MM-dd")
                           ``type`` = expense.Type.ToLowerInvariant()
                           quantity = qtyOpt
                           unit = if expense.Unit = null then None else Some expense.Unit |}
                        next
                        ctx
        }

let update (expenseId: Guid) : HttpHandler =
    fun next ctx ->
        task {
            let! body = ctx.ReadBodyFromRequestAsync()

            let req =
                try
                    JsonSerializer.Deserialize<UpdateExpenseRequest>(
                        body,
                        JsonSerializerOptions(PropertyNameCaseInsensitive = true)
                    )
                    |> Some
                with _ ->
                    None

            match req with
            | None ->
                ctx.Response.StatusCode <- 400

                return!
                    json
                        {| error = "Bad Request"
                           message = "Invalid request body" |}
                        earlyReturn
                        ctx
            | Some r ->
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
                | Some expense ->
                    let amountResult = parseAmount r.amount

                    match amountResult with
                    | Error(ValidationError(f, m)) ->
                        ctx.Response.StatusCode <- 400

                        return!
                            json
                                {| error = "Validation Error"
                                   field = f
                                   message = m |}
                                earlyReturn
                                ctx
                    | Error _ ->
                        ctx.Response.StatusCode <- 400

                        return!
                            json
                                {| error = "Validation Error"
                                   field = "amount"
                                   message = "Invalid amount" |}
                                earlyReturn
                                ctx
                    | Ok amount ->
                        let dateVal =
                            match DateTime.TryParse(r.date) with
                            | true, d -> DateTime.SpecifyKind(d, DateTimeKind.Utc)
                            | _ -> expense.Date

                        let updated =
                            { expense with
                                Amount = amount
                                Currency =
                                    if r.currency <> null then
                                        r.currency.ToUpperInvariant()
                                    else
                                        expense.Currency
                                Category = if r.category <> null then r.category else expense.Category
                                Description =
                                    if r.description <> null then
                                        r.description
                                    else
                                        expense.Description
                                Date = dateVal
                                Type =
                                    if r.``type`` <> null then
                                        r.``type``.ToUpperInvariant()
                                    else
                                        expense.Type
                                UpdatedAt = DateTime.UtcNow }

                        let! saved = expenseRepo.Update updated

                        let formattedAmount =
                            match saved.Currency with
                            | "IDR" -> saved.Amount.ToString("0")
                            | _ -> saved.Amount.ToString("0.00")

                        return!
                            json
                                {| id = saved.Id
                                   userId = saved.UserId
                                   amount = formattedAmount
                                   currency = saved.Currency
                                   category = saved.Category
                                   description = saved.Description
                                   date = saved.Date.ToString("yyyy-MM-dd")
                                   ``type`` = saved.Type.ToLowerInvariant() |}
                                next
                                ctx
        }

let delete (expenseId: Guid) : HttpHandler =
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
            | Some expense ->
                do! expenseRepo.Delete expense

                ctx.Response.StatusCode <- 204
                return! text "" earlyReturn ctx
        }

let summary: HttpHandler =
    fun next ctx ->
        task {
            let userId = ctx.Items["UserId"] :?> Guid
            let expenseRepo = ctx.GetService<ExpenseRepository>()

            let! expenses = expenseRepo.ListSummaryByUser userId

            let grouped =
                expenses
                |> List.groupBy (fun e -> e.Currency)
                |> List.map (fun (currency, items) ->
                    let total = items |> List.sumBy (fun e -> e.Amount)

                    let formattedTotal =
                        match currency with
                        | "IDR" -> total.ToString("0")
                        | _ -> total.ToString("0.00")

                    currency, formattedTotal)
                |> Map.ofList

            return! json grouped next ctx
        }
