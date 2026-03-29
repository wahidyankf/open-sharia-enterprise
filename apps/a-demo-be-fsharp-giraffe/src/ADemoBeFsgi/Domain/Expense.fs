module ADemoBeFsgi.Domain.Expense

open System
open ADemoBeFsgi.Domain.Types

type Expense =
    { Id: Guid
      UserId: Guid
      Amount: decimal
      Currency: string
      Category: string
      Description: string
      Date: DateTime
      Type: string // "income" or "expense"
      Quantity: decimal option
      Unit: string option
      CreatedAt: DateTime
      UpdatedAt: DateTime }

let validateAmount (amount: decimal) =
    if amount < 0m then
        Error(ValidationError("amount", "Amount must not be negative"))
    else
        Ok amount

let validateCurrencyPrecision (currency: string) (amount: decimal) =
    match currency.ToUpperInvariant() with
    | "USD" ->
        if amount <> Math.Round(amount, 2) then
            Error(ValidationError("amount", "USD requires at most 2 decimal places"))
        else
            Ok amount
    | "IDR" ->
        if amount <> Math.Round(amount, 0) then
            Error(ValidationError("amount", "IDR requires 0 decimal places"))
        else
            Ok amount
    | _ -> Error(ValidationError("currency", $"Unsupported currency: %s{currency}"))

let validateUnit (unit: string option) =
    match unit with
    | None
    | Some "" -> Ok unit
    | Some u ->
        if supportedUnits.Contains(u.ToLowerInvariant()) then
            Ok(Some(u.ToLowerInvariant()))
        else
            Error(ValidationError("unit", $"Unsupported unit: %s{u}"))
