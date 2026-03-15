defmodule DemoBeExphWeb.ReportController do
  use DemoBeExphWeb, :controller

  alias Guardian.Plug, as: GuardianPlug

  defp expense_ctx,
    do:
      Application.get_env(
        :demo_be_exph,
        :expense_module,
        DemoBeExph.Expense.ExpenseContext
      )

  def pl(conn, params) do
    user = GuardianPlug.current_resource(conn)

    from_str = Map.get(params, "startDate", Map.get(params, "from", ""))
    to_str = Map.get(params, "endDate", Map.get(params, "to", ""))
    currency = Map.get(params, "currency", "")

    with {:ok, from_date} <- parse_date(from_str),
         {:ok, to_date} <- parse_date(to_str) do
      report = expense_ctx().pl_report(user.id, from_date, to_date, currency)

      income_breakdown =
        report.income_breakdown
        |> Enum.map(fn {k, v} -> %{category: k, type: "income", total: Decimal.to_string(v)} end)

      expense_breakdown =
        report.expense_breakdown
        |> Enum.map(fn {k, v} -> %{category: k, type: "expense", total: Decimal.to_string(v)} end)

      json(conn, %{
        totalIncome: Decimal.to_string(report.income_total),
        totalExpense: Decimal.to_string(report.expense_total),
        net: Decimal.to_string(report.net),
        incomeBreakdown: income_breakdown,
        expenseBreakdown: expense_breakdown,
        currency: currency
      })
    else
      {:error, :invalid_date} ->
        conn
        |> put_status(:bad_request)
        |> json(%{message: "Invalid date format. Use YYYY-MM-DD."})
    end
  end

  defp parse_date(""), do: {:error, :invalid_date}

  defp parse_date(str) do
    case Date.from_iso8601(str) do
      {:ok, date} -> {:ok, date}
      {:error, _} -> {:error, :invalid_date}
    end
  end
end
