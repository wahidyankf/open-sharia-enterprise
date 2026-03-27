module DemoBeFsgi.Handlers.ReportHandler

open System
open Giraffe
open DemoBeFsgi.Infrastructure.Repositories.RepositoryTypes

let profitAndLoss: HttpHandler =
    fun next ctx ->
        task {
            let userId = ctx.Items["UserId"] :?> Guid
            let expenseRepo = ctx.GetService<ExpenseRepository>()

            let fromParam =
                ctx.TryGetQueryStringValue("startDate")
                |> Option.orElseWith (fun () -> ctx.TryGetQueryStringValue("from"))

            let toParam =
                ctx.TryGetQueryStringValue("endDate")
                |> Option.orElseWith (fun () -> ctx.TryGetQueryStringValue("to"))

            let currencyParam =
                ctx.TryGetQueryStringValue("currency") |> Option.defaultValue "USD"

            let fromDate =
                match fromParam with
                | Some s ->
                    match DateTime.TryParse(s) with
                    | true, d -> DateTime.SpecifyKind(d, DateTimeKind.Utc)
                    | _ -> DateTime.SpecifyKind(DateTime.MinValue, DateTimeKind.Utc)
                | None -> DateTime.SpecifyKind(DateTime.MinValue, DateTimeKind.Utc)

            let toDate =
                match toParam with
                | Some s ->
                    match DateTime.TryParse(s) with
                    | true, d -> DateTime.SpecifyKind(d.AddDays(1.0).AddSeconds(-1.0), DateTimeKind.Utc)
                    | _ -> DateTime.SpecifyKind(DateTime.MaxValue, DateTimeKind.Utc)
                | None -> DateTime.SpecifyKind(DateTime.MaxValue, DateTimeKind.Utc)

            let currency = currencyParam.ToUpperInvariant()

            let! entries = expenseRepo.ListByUserAndFilter userId currency fromDate toDate

            let incomeEntries = entries |> List.filter (fun e -> e.Type = "INCOME")
            let expenseEntries = entries |> List.filter (fun e -> e.Type = "EXPENSE")

            let incomeTotal = incomeEntries |> List.sumBy (fun e -> e.Amount)
            let expenseTotal = expenseEntries |> List.sumBy (fun e -> e.Amount)
            let net = incomeTotal - expenseTotal

            let formatAmount (a: decimal) =
                match currency with
                | "IDR" -> a.ToString("0")
                | _ -> a.ToString("0.00")

            let incomeBreakdown =
                incomeEntries
                |> List.groupBy (fun e -> e.Category)
                |> List.map (fun (cat, items) ->
                    {| category = cat
                       ``type`` = "income"
                       total = formatAmount (items |> List.sumBy (fun e -> e.Amount)) |})
                |> List.toArray

            let expenseBreakdown =
                expenseEntries
                |> List.groupBy (fun e -> e.Category)
                |> List.map (fun (cat, items) ->
                    {| category = cat
                       ``type`` = "expense"
                       total = formatAmount (items |> List.sumBy (fun e -> e.Amount)) |})
                |> List.toArray

            return!
                json
                    {| totalIncome = formatAmount incomeTotal
                       totalExpense = formatAmount expenseTotal
                       net = formatAmount net
                       currency = currency
                       incomeBreakdown = incomeBreakdown
                       expenseBreakdown = expenseBreakdown |}
                    next
                    ctx
        }
