defmodule DemoBeExph.Test.InMemoryExpenseContext do
  @moduledoc """
  In-memory implementation of DemoBeExph.Expense.ExpenseBehaviour backed by InMemoryStore Agent.
  Used in test environment to avoid real PostgreSQL.
  """

  @behaviour DemoBeExph.Expense.ExpenseBehaviour

  alias DemoBeExph.Expense.Expense
  alias DemoBeExph.Test.InMemoryStore

  @impl true
  def create_expense(user_id, attrs) do
    attrs_with_user = Map.put(attrs, "user_id", user_id)
    changeset = Expense.changeset(%Expense{}, attrs_with_user)

    if changeset.valid? do
      store_new_expense(changeset)
    else
      {:error, changeset}
    end
  end

  @impl true
  def get_expense(user_id, expense_id) do
    state = InMemoryStore.get_state()

    case Map.get(state.expenses, expense_id) do
      nil -> nil
      expense -> if expense.user_id == user_id, do: expense, else: nil
    end
  end

  @impl true
  def list_expenses(user_id, opts \\ []) do
    page = Keyword.get(opts, :page, 1)
    page_size = Keyword.get(opts, :page_size, 20)

    all_expenses =
      InMemoryStore.get_state().expenses
      |> Map.values()
      |> Enum.filter(fn e -> e.user_id == user_id end)
      |> Enum.sort_by(fn e -> {e.date, e.id} end, &sort_desc/2)

    total = length(all_expenses)
    offset = (page - 1) * page_size
    data = all_expenses |> Enum.drop(offset) |> Enum.take(page_size)

    %{data: data, total: total, page: page, page_size: page_size}
  end

  @impl true
  def update_expense(user_id, expense_id, attrs) do
    case get_expense(user_id, expense_id) do
      nil -> {:error, :not_found}
      expense -> apply_expense_update(expense, expense_id, attrs)
    end
  end

  @impl true
  def delete_expense(user_id, expense_id) do
    case get_expense(user_id, expense_id) do
      nil -> {:error, :not_found}
      expense -> remove_expense(expense_id, expense)
    end
  end

  @impl true
  def summary(user_id) do
    InMemoryStore.get_state().expenses
    |> Map.values()
    |> Enum.filter(fn e -> e.user_id == user_id and e.type == "expense" end)
    |> Enum.group_by(& &1.currency)
    |> Enum.map(&sum_currency_group/1)
    |> Enum.into(%{})
  end

  @impl true
  def pl_report(user_id, from_date, to_date, currency) do
    entries =
      InMemoryStore.get_state().expenses
      |> Map.values()
      |> Enum.filter(&in_report_range?(&1, user_id, currency, from_date, to_date))

    income_entries = Enum.filter(entries, &(&1.type == "income"))
    expense_entries = Enum.filter(entries, &(&1.type == "expense"))

    income_total = sum_amounts(income_entries)
    expense_total = sum_amounts(expense_entries)
    net = Decimal.sub(income_total, expense_total)

    %{
      income_total: income_total,
      expense_total: expense_total,
      net: net,
      income_breakdown: breakdown_by_category(income_entries),
      expense_breakdown: breakdown_by_category(expense_entries)
    }
  end

  # Private helpers

  defp store_new_expense(changeset) do
    id = InMemoryStore.next_id()
    now = DateTime.utc_now() |> DateTime.truncate(:second)

    expense =
      changeset
      |> Ecto.Changeset.apply_changes()
      |> Map.merge(%{id: id, inserted_at: now, updated_at: now})

    InMemoryStore.update_state(fn s ->
      Map.update!(s, :expenses, &Map.put(&1, id, expense))
    end)

    {:ok, expense}
  end

  defp apply_expense_update(expense, expense_id, attrs) do
    changeset = Expense.update_changeset(expense, attrs)

    if changeset.valid? do
      now = DateTime.utc_now() |> DateTime.truncate(:second)
      updated = changeset |> Ecto.Changeset.apply_changes() |> Map.put(:updated_at, now)

      InMemoryStore.update_state(fn s ->
        Map.update!(s, :expenses, &Map.put(&1, expense_id, updated))
      end)

      {:ok, updated}
    else
      {:error, changeset}
    end
  end

  defp remove_expense(expense_id, expense) do
    InMemoryStore.update_state(fn s ->
      Map.update!(s, :expenses, &Map.delete(&1, expense_id))
    end)

    {:ok, expense}
  end

  defp sort_desc({d1, i1}, {d2, i2}) do
    case Date.compare(d1, d2) do
      :gt -> true
      :lt -> false
      :eq -> i1 > i2
    end
  end

  defp sum_currency_group({currency, entries}) do
    # Use Enum.reduce with the first element as seed to preserve Decimal scale
    amounts = Enum.map(entries, & &1.amount)
    {currency, Enum.reduce(amounts, &Decimal.add/2)}
  end

  defp in_report_range?(entry, user_id, currency, from_date, to_date) do
    entry.user_id == user_id and
      entry.currency == currency and
      Date.compare(entry.date, from_date) != :lt and
      Date.compare(entry.date, to_date) != :gt
  end

  defp sum_amounts([]), do: Decimal.new("0.00")

  defp sum_amounts(entries) do
    entries
    |> Enum.map(& &1.amount)
    |> Enum.reduce(Decimal.new("0.00"), &Decimal.add/2)
  end

  defp breakdown_by_category(entries) do
    entries
    |> Enum.group_by(& &1.category)
    |> Enum.map(fn {cat, es} ->
      {cat, es |> Enum.map(& &1.amount) |> Enum.reduce(Decimal.new(0), &Decimal.add/2)}
    end)
    |> Enum.into(%{})
  end
end
