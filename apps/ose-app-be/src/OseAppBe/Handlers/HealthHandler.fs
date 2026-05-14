module OseAppBe.Handlers.HealthHandler

open Giraffe

let handle: HttpHandler = fun next ctx -> json {| status = "healthy" |} next ctx
