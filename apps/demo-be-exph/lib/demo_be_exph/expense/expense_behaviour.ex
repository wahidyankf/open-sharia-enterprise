defmodule DemoBeExph.Expense.ExpenseBehaviour do
  @moduledoc """
  Behaviour contract for the Expense context.
  Allows swapping real Ecto implementation for in-memory implementation in tests.
  """

  alias DemoBeExph.Expense.Expense

  @callback create_expense(integer(), map()) :: {:ok, Expense.t()} | {:error, Ecto.Changeset.t()}
  @callback get_expense(integer(), integer()) :: Expense.t() | nil
  @callback list_expenses(integer(), keyword()) :: map()
  @callback update_expense(integer(), integer(), map()) ::
              {:ok, Expense.t()} | {:error, atom() | Ecto.Changeset.t()}
  @callback delete_expense(integer(), integer()) :: {:ok, any()} | {:error, atom()}
  @callback summary(integer()) :: map()
  @callback pl_report(integer(), Date.t(), Date.t(), String.t()) :: map()
end
