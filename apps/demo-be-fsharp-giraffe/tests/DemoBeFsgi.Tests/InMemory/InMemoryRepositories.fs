module DemoBeFsgi.Tests.InMemory.InMemoryRepositories

open System
open System.Collections.Concurrent
open System.Linq
open DemoBeFsgi.Infrastructure.AppDbContext
open DemoBeFsgi.Infrastructure.Repositories.RepositoryTypes

let createUserRepo () : UserRepository =
    let store = ConcurrentDictionary<Guid, UserEntity>()

    { FindById =
        fun id ->
            task {
                return
                    match store.TryGetValue(id) with
                    | true, v -> Some v
                    | _ -> None
            }
      FindByUsername = fun username -> task { return store.Values |> Seq.tryFind (fun u -> u.Username = username) }
      FindByEmail = fun email -> task { return store.Values |> Seq.tryFind (fun u -> u.Email = email) }
      Create =
        fun entity ->
            task {
                store.[entity.Id] <- entity
                return entity
            }
      Update =
        fun entity ->
            task {
                store.[entity.Id] <- entity
                return entity
            }
      CountByFilter =
        fun searchFilter ->
            task {
                let query =
                    match searchFilter with
                    | Some s ->
                        store.Values
                        |> Seq.filter (fun u -> u.Username.Contains(s) || u.Email.Contains(s))
                    | None -> store.Values |> Seq.cast

                return query |> Seq.length
            }
      ListByFilter =
        fun searchFilter page size ->
            task {
                let query =
                    match searchFilter with
                    | Some s ->
                        store.Values
                        |> Seq.filter (fun u -> u.Username.Contains(s) || u.Email.Contains(s))
                    | None -> store.Values |> Seq.cast

                let offset = (page - 1) * size
                return query |> Seq.skip offset |> Seq.truncate size |> Seq.toList
            } }

let createExpenseRepo () : ExpenseRepository =
    let store = ConcurrentDictionary<Guid, ExpenseEntity>()

    { Create =
        fun entity ->
            task {
                store.[entity.Id] <- entity
                return entity
            }
      FindById =
        fun id ->
            task {
                return
                    match store.TryGetValue(id) with
                    | true, v -> Some v
                    | _ -> None
            }
      ListByUser =
        fun userId page size ->
            task {
                let userExpenses =
                    store.Values
                    |> Seq.filter (fun e -> e.UserId = userId)
                    |> Seq.sortByDescending (fun e -> e.Date)
                    |> Seq.toList

                let total = userExpenses.Length
                let offset = (page - 1) * size
                let paged = userExpenses |> List.skip (min offset total) |> List.truncate size
                return total, paged
            }
      ListByUserAndFilter =
        fun userId currency fromDate toDate ->
            task {
                return
                    store.Values
                    |> Seq.filter (fun e ->
                        e.UserId = userId
                        && e.Currency = currency
                        && e.Date >= fromDate
                        && e.Date <= toDate)
                    |> Seq.toList
            }
      ListSummaryByUser =
        fun userId ->
            task {
                return
                    store.Values
                    |> Seq.filter (fun e -> e.UserId = userId && e.Type = "EXPENSE")
                    |> Seq.toList
            }
      Update =
        fun entity ->
            task {
                store.[entity.Id] <- entity
                return entity
            }
      Delete = fun entity -> task { store.TryRemove(entity.Id) |> ignore } }

let createAttachmentRepo () : AttachmentRepository =
    let store = ConcurrentDictionary<Guid, AttachmentEntity>()

    { Create =
        fun entity ->
            task {
                store.[entity.Id] <- entity
                return entity
            }
      FindById =
        fun attachmentId expenseId ->
            task {
                return
                    match store.TryGetValue(attachmentId) with
                    | true, v when v.ExpenseId = expenseId -> Some v
                    | _ -> None
            }
      ListByExpense =
        fun expenseId -> task { return store.Values |> Seq.filter (fun a -> a.ExpenseId = expenseId) |> Seq.toList }
      Delete = fun entity -> task { store.TryRemove(entity.Id) |> ignore } }

let createTokenRepo () : TokenRepository =
    let store = ConcurrentDictionary<Guid, RevokedTokenEntity>()

    { Create =
        fun entity ->
            task {
                store.[entity.Id] <- entity
                return entity
            }
      ExistsJti = fun jti -> task { return store.Values |> Seq.exists (fun rt -> rt.Jti = jti) } }

let createRefreshTokenRepo () : RefreshTokenRepository =
    let store = ConcurrentDictionary<Guid, RefreshTokenEntity>()

    { Create =
        fun entity ->
            task {
                store.[entity.Id] <- entity
                return entity
            }
      FindActiveByHash =
        fun tokenHash ->
            task {
                return
                    store.Values
                    |> Seq.tryFind (fun rt -> rt.TokenHash = tokenHash && not rt.Revoked)
            }
      ListActiveByUser =
        fun userId ->
            task {
                return
                    store.Values
                    |> Seq.filter (fun rt -> rt.UserId = userId && not rt.Revoked)
                    |> Seq.toList
            }
      Update =
        fun entity ->
            task {
                store.[entity.Id] <- entity
                return entity
            } }
