module DemoBeFsgi.Domain.Types

type Currency =
    | USD
    | IDR

type Role =
    | User
    | Admin

type UserStatus =
    | Active
    | Inactive
    | Disabled
    | Locked

type EntryType =
    | Income
    | Expense

type DomainError =
    | ValidationError of field: string * message: string
    | NotFound of entity: string
    | Forbidden of message: string
    | Conflict of message: string
    | Unauthorized of message: string
    | FileTooLarge of limit: int64
    | UnsupportedMediaType of message: string
    | AccountLocked of message: string
    | AccountDeactivated of message: string

// Supported units
let supportedUnits =
    set
        [ "liter"
          "ml"
          "kg"
          "g"
          "km"
          "meter"
          "gallon"
          "lb"
          "oz"
          "mile"
          "piece"
          "hour" ]

let parseCurrency (s: string) =
    match s.ToUpperInvariant() with
    | "USD" -> Ok USD
    | "IDR" -> Ok IDR
    | _ -> Error(ValidationError("currency", $"Unsupported currency: {s}"))

let currencyToString =
    function
    | USD -> "USD"
    | IDR -> "IDR"

let roleToString =
    function
    | User -> "USER"
    | Admin -> "ADMIN"

let statusToString =
    function
    | Active -> "ACTIVE"
    | Inactive -> "INACTIVE"
    | Disabled -> "DISABLED"
    | Locked -> "LOCKED"

let parseStatus (s: string) =
    match s.ToUpperInvariant() with
    | "ACTIVE" -> Some Active
    | "INACTIVE" -> Some Inactive
    | "DISABLED" -> Some Disabled
    | "LOCKED" -> Some Locked
    | _ -> None
