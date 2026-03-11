module DemoBeFsgi.Tests.State

open System.Net.Http

type StepState =
    { Client: HttpClient
      Response: HttpResponseMessage option
      ResponseBody: string option
      AccessToken: string option
      RefreshToken: string option
      UserId: string option
      ExpenseId: string option
      AttachmentId: string option
      ExtraData: Map<string, string> }

let empty (client: HttpClient) =
    { Client = client
      Response = None
      ResponseBody = None
      AccessToken = None
      RefreshToken = None
      UserId = None
      ExpenseId = None
      AttachmentId = None
      ExtraData = Map.empty }
