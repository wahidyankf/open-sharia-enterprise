module DemoBeFsgi.Tests.State

open DemoBeFsgi.Infrastructure.AppDbContext

/// Represents a simulated HTTP-style response from a direct service call.
/// Status maps to HTTP status codes; Body is JSON text.
type ServiceResponse = { Status: int; Body: string }

type StepState =
    { Db: AppDbContext
      Response: ServiceResponse option
      ResponseBody: string option
      AccessToken: string option
      RefreshToken: string option
      UserId: string option
      ExpenseId: string option
      AttachmentId: string option
      ExtraData: Map<string, string> }

let empty (db: AppDbContext) =
    { Db = db
      Response = None
      ResponseBody = None
      AccessToken = None
      RefreshToken = None
      UserId = None
      ExpenseId = None
      AttachmentId = None
      ExtraData = Map.empty }
