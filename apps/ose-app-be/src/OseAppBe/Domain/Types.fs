module OseAppBe.Domain.Types

type AppEnv =
    | Dev
    | Staging
    | Prod

type AppError = UnknownError of string
