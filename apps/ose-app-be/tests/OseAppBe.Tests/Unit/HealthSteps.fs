module OseAppBe.Tests.Unit.HealthSteps

open TickSpec
open OseAppBe.Tests.State
open OseAppBe.Tests.TestFixture

let mutable private state = State.Empty
let mutable private factory: OseAppBeFactory option = None

[<Given>]
let ``the ose-app-be service is running`` () =
    factory <- Some(new OseAppBeFactory())
    state <- State.Empty

[<When>]
let ``I send GET /api/v1/health`` () =
    let f = factory.Value
    let client = f.CreateClient()
    let response = client.GetAsync("/api/v1/health").Result
    let body = response.Content.ReadAsStringAsync().Result

    state <-
        { StatusCode = int response.StatusCode
          ResponseBody = body }

[<Then>]
let ``the response status is 200`` () =
    if state.StatusCode <> 200 then
        failwithf "Expected 200 but got %d" state.StatusCode

[<Then>]
let ``the response body has a "status" field equal to "healthy"`` () =
    if not (state.ResponseBody.Contains("\"healthy\"")) then
        failwithf "Expected body to contain 'healthy' but got: %s" state.ResponseBody
