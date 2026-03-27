module DemoBeFsgi.Infrastructure.Repositories.EfRepositories

open System
open System.Linq
open Microsoft.EntityFrameworkCore
open DemoBeFsgi.Infrastructure.AppDbContext
open DemoBeFsgi.Infrastructure.Repositories.RepositoryTypes

let createUserRepo (db: AppDbContext) : UserRepository =
    { FindById =
        fun id ->
            task {
                let! entity = db.Users.AsNoTracking().FirstOrDefaultAsync(fun u -> u.Id = id)

                if obj.ReferenceEquals(entity, null) then
                    return None
                else
                    return Some entity
            }
      FindByUsername =
        fun username ->
            task {
                let! entity = db.Users.AsNoTracking().FirstOrDefaultAsync(fun u -> u.Username = username)

                if obj.ReferenceEquals(entity, null) then
                    return None
                else
                    return Some entity
            }
      FindByEmail =
        fun email ->
            task {
                let! entity = db.Users.AsNoTracking().FirstOrDefaultAsync(fun u -> u.Email = email)

                if obj.ReferenceEquals(entity, null) then
                    return None
                else
                    return Some entity
            }
      Create =
        fun entity ->
            task {
                db.Users.Add(entity) |> ignore
                let! _ = db.SaveChangesAsync()
                return entity
            }
      Update =
        fun entity ->
            task {
                db.ChangeTracker.Clear()
                db.Users.Update(entity) |> ignore
                let! _ = db.SaveChangesAsync()
                return entity
            }
      CountByFilter =
        fun searchFilter ->
            task {
                let query =
                    match searchFilter with
                    | Some s -> db.Users.Where(fun u -> u.Username.Contains(s) || u.Email.Contains(s))
                    | None -> db.Users :> IQueryable<UserEntity>

                return! query.CountAsync()
            }
      ListByFilter =
        fun searchFilter page size ->
            task {
                let query =
                    match searchFilter with
                    | Some s -> db.Users.Where(fun u -> u.Username.Contains(s) || u.Email.Contains(s))
                    | None -> db.Users :> IQueryable<UserEntity>

                let offset = (page - 1) * size
                let! users = query.Skip(offset).Take(size).ToListAsync()
                return users |> Seq.toList
            } }

let createExpenseRepo (db: AppDbContext) : ExpenseRepository =
    { Create =
        fun entity ->
            task {
                db.Expenses.Add(entity) |> ignore
                let! _ = db.SaveChangesAsync()
                return entity
            }
      FindById =
        fun id ->
            task {
                let! entity = db.Expenses.AsNoTracking().FirstOrDefaultAsync(fun e -> e.Id = id)

                if obj.ReferenceEquals(entity, null) then
                    return None
                else
                    return Some entity
            }
      ListByUser =
        fun userId page size ->
            task {
                let query = db.Expenses.Where(fun e -> e.UserId = userId)
                let! total = query.CountAsync()
                let offset = (page - 1) * size

                let! expenses = query.OrderByDescending(fun e -> e.Date).Skip(offset).Take(size).ToListAsync()

                return total, expenses |> Seq.toList
            }
      ListByUserAndFilter =
        fun userId currency fromDate toDate ->
            task {
                let! entries =
                    db.Expenses
                        .Where(fun e ->
                            e.UserId = userId
                            && e.Currency = currency
                            && e.Date >= fromDate
                            && e.Date <= toDate)
                        .ToListAsync()

                return entries |> Seq.toList
            }
      ListSummaryByUser =
        fun userId ->
            task {
                let! expenses = db.Expenses.Where(fun e -> e.UserId = userId && e.Type = "EXPENSE").ToListAsync()

                return expenses |> Seq.toList
            }
      Update =
        fun entity ->
            task {
                db.ChangeTracker.Clear()
                db.Expenses.Update(entity) |> ignore
                let! _ = db.SaveChangesAsync()
                return entity
            }
      Delete =
        fun entity ->
            task {
                db.ChangeTracker.Clear()
                db.Expenses.Remove(entity) |> ignore
                let! _ = db.SaveChangesAsync()
                return ()
            } }

let createAttachmentRepo (db: AppDbContext) : AttachmentRepository =
    { Create =
        fun entity ->
            task {
                db.Attachments.Add(entity) |> ignore
                let! _ = db.SaveChangesAsync()
                return entity
            }
      FindById =
        fun attachmentId expenseId ->
            task {
                let! entity =
                    db.Attachments
                        .AsNoTracking()
                        .FirstOrDefaultAsync(fun a -> a.Id = attachmentId && a.ExpenseId = expenseId)

                if obj.ReferenceEquals(entity, null) then
                    return None
                else
                    return Some entity
            }
      ListByExpense =
        fun expenseId ->
            task {
                let! attachments = db.Attachments.Where(fun a -> a.ExpenseId = expenseId).ToListAsync()
                return attachments |> Seq.toList
            }
      Delete =
        fun entity ->
            task {
                db.ChangeTracker.Clear()
                db.Attachments.Remove(entity) |> ignore
                let! _ = db.SaveChangesAsync()
                return ()
            } }

let createTokenRepo (db: AppDbContext) : TokenRepository =
    { Create =
        fun entity ->
            task {
                db.RevokedTokens.Add(entity) |> ignore
                let! _ = db.SaveChangesAsync()
                return entity
            }
      ExistsJti = fun jti -> task { return! db.RevokedTokens.AnyAsync(fun rt -> rt.Jti = jti) } }

let createRefreshTokenRepo (db: AppDbContext) : RefreshTokenRepository =
    { Create =
        fun entity ->
            task {
                db.RefreshTokens.Add(entity) |> ignore
                let! _ = db.SaveChangesAsync()
                return entity
            }
      FindActiveByHash =
        fun tokenHash ->
            task {
                let! entity =
                    db.RefreshTokens
                        .AsNoTracking()
                        .FirstOrDefaultAsync(fun rt -> rt.TokenHash = tokenHash && not rt.Revoked)

                if obj.ReferenceEquals(entity, null) then
                    return None
                else
                    return Some entity
            }
      ListActiveByUser =
        fun userId ->
            task {
                let! tokens =
                    db.RefreshTokens.AsNoTracking().Where(fun rt -> rt.UserId = userId && not rt.Revoked).ToListAsync()

                return tokens |> Seq.toList
            }
      Update =
        fun entity ->
            task {
                db.ChangeTracker.Clear()
                db.RefreshTokens.Update(entity) |> ignore
                let! _ = db.SaveChangesAsync()
                return entity
            } }
