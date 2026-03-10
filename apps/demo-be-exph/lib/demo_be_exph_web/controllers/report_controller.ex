defmodule DemoBeExphWeb.ReportController do
  use DemoBeExphWeb, :controller

  alias DemoBeExph.Expense.ExpenseContext
  alias Guardian.Plug, as: GuardianPlug

  def pl(conn, params) do
    user = GuardianPlug.current_resource(conn)

    from_str = Map.get(params, "from", "")
    to_str = Map.get(params, "to", "")
    currency = Map.get(params, "currency", "")

    with {:ok, from_date} <- parse_date(from_str),
         {:ok, to_date} <- parse_date(to_str) do
      report = ExpenseContext.pl_report(user.id, from_date, to_date, currency)

      json(conn, %{
        income_total: Decimal.to_string(report.income_total),
        expense_total: Decimal.to_string(report.expense_total),
        net: Decimal.to_string(report.net),
        income_breakdown:
          report.income_breakdown
          |> Enum.into(%{}, fn {k, v} -> {k, Decimal.to_string(v)} end),
        expense_breakdown:
          report.expense_breakdown
          |> Enum.into(%{}, fn {k, v} -> {k, Decimal.to_string(v)} end),
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
