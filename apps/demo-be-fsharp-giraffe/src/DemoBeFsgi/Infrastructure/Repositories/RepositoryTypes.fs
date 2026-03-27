module DemoBeFsgi.Infrastructure.Repositories.RepositoryTypes

open System
open System.Threading.Tasks
open DemoBeFsgi.Infrastructure.AppDbContext

/// Repository for user operations.
type UserRepository =
    { FindById: Guid -> Task<UserEntity option>
      FindByUsername: string -> Task<UserEntity option>
      FindByEmail: string -> Task<UserEntity option>
      Create: UserEntity -> Task<UserEntity>
      Update: UserEntity -> Task<UserEntity>
      CountByFilter: string option -> Task<int>
      ListByFilter: string option -> int -> int -> Task<UserEntity list> }

/// Repository for expense operations.
type ExpenseRepository =
    { Create: ExpenseEntity -> Task<ExpenseEntity>
      FindById: Guid -> Task<ExpenseEntity option>
      ListByUser: Guid -> int -> int -> Task<int * ExpenseEntity list>
      ListByUserAndFilter: Guid -> string -> DateTime -> DateTime -> Task<ExpenseEntity list>
      ListSummaryByUser: Guid -> Task<ExpenseEntity list>
      Update: ExpenseEntity -> Task<ExpenseEntity>
      Delete: ExpenseEntity -> Task<unit> }

/// Repository for attachment operations.
type AttachmentRepository =
    { Create: AttachmentEntity -> Task<AttachmentEntity>
      FindById: Guid -> Guid -> Task<AttachmentEntity option>
      ListByExpense: Guid -> Task<AttachmentEntity list>
      Delete: AttachmentEntity -> Task<unit> }

/// Repository for revoked token operations.
type TokenRepository =
    { Create: RevokedTokenEntity -> Task<RevokedTokenEntity>
      ExistsJti: string -> Task<bool> }

/// Repository for refresh token operations.
type RefreshTokenRepository =
    { Create: RefreshTokenEntity -> Task<RefreshTokenEntity>
      FindActiveByHash: string -> Task<RefreshTokenEntity option>
      ListActiveByUser: Guid -> Task<RefreshTokenEntity list>
      Update: RefreshTokenEntity -> Task<RefreshTokenEntity> }
