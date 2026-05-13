module OseGrcBe.Tests.State

type State =
    { StatusCode: int
      ResponseBody: string }

    static member Empty = { StatusCode = 0; ResponseBody = "" }
