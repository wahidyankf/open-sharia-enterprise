defmodule ADemoBeExph.Expense.ExpenseContext do
  @moduledoc """
  Context for managing financial entries (income and expenses).
  """

  @behaviour ADemoBeExph.Expense.ExpenseBehaviour

  import Ecto.Query

  alias ADemoBeExph.Expense.Expense
  alias ADemoBeExph.Repo

  @doc "Create a new financial entry for user_id."
  def create_expense(user_id, attrs) do
    attrs_with_user = Map.put(attrs, "user_id", user_id)

    %Expense{}
    |> Expense.changeset(attrs_with_user)
    |> Repo.insert()
  end

  @doc "Get a single expense by id, scoped to user_id."
  def get_expense(user_id, expense_id) do
    Repo.get_by(Expense, id: expense_id, user_id: user_id)
  end

  @doc "List expenses for a user, paginated."
  def list_expenses(user_id, opts \\ []) do
    page = Keyword.get(opts, :page, 1)
    page_size = Keyword.get(opts, :page_size, 20)
    offset = (page - 1) * page_size

    base_query = from e in Expense, where: e.user_id == ^user_id

    total = Repo.aggregate(base_query, :count, :id)

    entries =
      Repo.all(
        from e in base_query,
          order_by: [desc: e.date, desc: e.id],
          limit: ^page_size,
          offset: ^offset
      )

    %{data: entries, total: total, page: page, page_size: page_size}
  end

  @doc "Update an expense scoped to user_id."
  def update_expense(user_id, expense_id, attrs) do
    case get_expense(user_id, expense_id) do
      nil -> {:error, :not_found}
      expense -> expense |> Expense.update_changeset(attrs) |> Repo.update()
    end
  end

  @doc "Delete an expense scoped to user_id."
  def delete_expense(user_id, expense_id) do
    case get_expense(user_id, expense_id) do
      nil -> {:error, :not_found}
      expense -> Repo.delete(expense)
    end
  end

  @doc "Summarise totals grouped by currency for the user's expense-type entries."
  def summary(user_id) do
    rows =
      Repo.all(
        from e in Expense,
          where: e.user_id == ^user_id and e.type == "expense",
          group_by: e.currency,
          select: {e.currency, sum(e.amount)}
      )

    Enum.into(rows, %{})
  end

  @doc "Profit-and-loss report for a date range and currency."
  def pl_report(user_id, from_date, to_date, currency) do
    entries =
      Repo.all(
        from e in Expense,
          where:
            e.user_id == ^user_id and
              e.currency == ^currency and
              e.date >= ^from_date and
              e.date <= ^to_date
      )

    income_entries = Enum.filter(entries, &(&1.type == "income"))
    expense_entries = Enum.filter(entries, &(&1.type == "expense"))

    income_total =
      income_entries
      |> Enum.map(& &1.amount)
      |> Enum.reduce(Decimal.new("0.00"), &Decimal.add/2)

    expense_total =
      expense_entries
      |> Enum.map(& &1.amount)
      |> Enum.reduce(Decimal.new("0.00"), &Decimal.add/2)

    net = Decimal.sub(income_total, expense_total)

    income_breakdown =
      income_entries
      |> Enum.group_by(& &1.category)
      |> Enum.map(fn {cat, es} ->
        {cat, es |> Enum.map(& &1.amount) |> Enum.reduce(Decimal.new(0), &Decimal.add/2)}
      end)
      |> Enum.into(%{})

    expense_breakdown =
      expense_entries
      |> Enum.group_by(& &1.category)
      |> Enum.map(fn {cat, es} ->
        {cat, es |> Enum.map(& &1.amount) |> Enum.reduce(Decimal.new(0), &Decimal.add/2)}
      end)
      |> Enum.into(%{})

    %{
      income_total: income_total,
      expense_total: expense_total,
      net: net,
      income_breakdown: income_breakdown,
      expense_breakdown: expense_breakdown
    }
  end
end
