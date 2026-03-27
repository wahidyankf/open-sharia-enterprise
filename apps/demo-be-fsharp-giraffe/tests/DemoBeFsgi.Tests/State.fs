module DemoBeFsgi.Tests.State

open DemoBeFsgi.Infrastructure.Repositories.RepositoryTypes

/// Represents a simulated HTTP-style response from a direct service call.
/// Status maps to HTTP status codes; Body is JSON text.
type ServiceResponse = { Status: int; Body: string }

type StepState =
    { UserRepo: UserRepository
      ExpenseRepo: ExpenseRepository
      AttachmentRepo: AttachmentRepository
      TokenRepo: TokenRepository
      RefreshTokenRepo: RefreshTokenRepository
      Response: ServiceResponse option
      ResponseBody: string option
      AccessToken: string option
      RefreshToken: string option
      UserId: string option
      ExpenseId: string option
      AttachmentId: string option
      ExtraData: Map<string, string> }

let empty
    (userRepo: UserRepository)
    (expenseRepo: ExpenseRepository)
    (attachmentRepo: AttachmentRepository)
    (tokenRepo: TokenRepository)
    (refreshTokenRepo: RefreshTokenRepository)
    =
    { UserRepo = userRepo
      ExpenseRepo = expenseRepo
      AttachmentRepo = attachmentRepo
      TokenRepo = tokenRepo
      RefreshTokenRepo = refreshTokenRepo
      Response = None
      ResponseBody = None
      AccessToken = None
      RefreshToken = None
      UserId = None
      ExpenseId = None
      AttachmentId = None
      ExtraData = Map.empty }
