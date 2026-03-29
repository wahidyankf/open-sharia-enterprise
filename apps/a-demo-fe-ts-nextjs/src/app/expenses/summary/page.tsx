"use client";

import { useState } from "react";
import { AppShell } from "@/components/layout/app-shell";
import { usePLReport, useExpenseSummary } from "@/lib/queries/use-expenses";
import type { CategoryBreakdown } from "@/lib/api/types";

const SUPPORTED_CURRENCIES = ["USD", "IDR"];

function getDefaultDates(): { start: string; end: string } {
  const now = new Date();
  const firstDay = new Date(now.getFullYear(), now.getMonth(), 1);
  const fmt = (d: Date) => d.toISOString().split("T")[0] ?? "";
  return { start: fmt(firstDay), end: fmt(now) };
}

const cardClassName = "bg-white p-6 rounded-lg border border-gray-300 shadow-md mb-6";

function CategoryTable({ rows, title }: { rows: CategoryBreakdown[]; title: string }) {
  return (
    <div className={cardClassName}>
      <h2 className="mt-0">{title}</h2>
      {rows.length === 0 ? (
        <p className="text-gray-500">No data for this period.</p>
      ) : (
        <table className="w-full border-collapse">
          <thead>
            <tr className="bg-gray-100">
              <th className="p-[0.6rem] text-left text-[0.85rem] font-bold text-gray-600">Category</th>
              <th className="p-[0.6rem] text-right text-[0.85rem] font-bold text-gray-600">Total</th>
            </tr>
          </thead>
          <tbody>
            {rows.map((row, idx) => (
              <tr
                key={`${row.category}-${idx}`}
                className={`border-b border-gray-200 ${idx % 2 === 0 ? "bg-white" : "bg-gray-50"}`}
              >
                <td className="p-[0.6rem]">{row.category}</td>
                <td className="p-[0.6rem] text-right font-medium">{row.total}</td>
              </tr>
            ))}
          </tbody>
        </table>
      )}
    </div>
  );
}

export default function ExpenseSummaryPage() {
  const defaults = getDefaultDates();
  const [startDate, setStartDate] = useState(defaults.start);
  const [endDate, setEndDate] = useState(defaults.end);
  const [currency, setCurrency] = useState("USD");
  const [submitted, setSubmitted] = useState(false);
  const [queryParams, setQueryParams] = useState({
    startDate: defaults.start,
    endDate: defaults.end,
    currency: "USD",
  });

  const { data, isLoading, isError } = usePLReport(
    submitted ? queryParams.startDate : "",
    submitted ? queryParams.endDate : "",
    submitted ? queryParams.currency : "",
  );

  // Backend returns Record<string, string> e.g. {"USD": "50.00", "IDR": "100000.00"}
  const { data: summaryData } = useExpenseSummary();
  const summaryEntries =
    summaryData && typeof summaryData === "object" && !Array.isArray(summaryData)
      ? Object.entries(summaryData as Record<string, string>)
      : [];

  const handleSubmit = (e: React.FormEvent<HTMLFormElement>) => {
    e.preventDefault();
    setQueryParams({ startDate, endDate, currency });
    setSubmitted(true);
  };

  return (
    <AppShell>
      <h1 className="mb-6">Expense Summary</h1>

      {summaryEntries.length > 0 && (
        <div className={cardClassName}>
          <h2 className="mt-0">Total by Currency</h2>
          <div className="flex flex-wrap gap-4">
            {summaryEntries.map(([cur, total]) => (
              <div key={cur} className="min-w-[140px] rounded-lg border border-gray-200 bg-gray-50 p-4 text-center">
                <div className="mb-1 text-[0.85rem] text-gray-500">{cur}</div>
                <div className="text-[1.2rem] font-bold text-red-700">
                  {cur} {total}
                </div>
              </div>
            ))}
          </div>
        </div>
      )}

      <div className={cardClassName}>
        <h2 className="mt-0">Filter</h2>
        <form onSubmit={handleSubmit} className="flex flex-wrap items-end gap-4">
          <div>
            <label htmlFor="start-date" className="mb-[0.3rem] block text-[0.85rem] font-semibold">
              Start Date
            </label>
            <input
              id="start-date"
              type="date"
              value={startDate}
              onChange={(e) => setStartDate(e.target.value)}
              aria-required="true"
              className="rounded border border-gray-400 px-3 py-2 text-[0.9rem]"
            />
          </div>
          <div>
            <label htmlFor="end-date" className="mb-[0.3rem] block text-[0.85rem] font-semibold">
              End Date
            </label>
            <input
              id="end-date"
              type="date"
              value={endDate}
              onChange={(e) => setEndDate(e.target.value)}
              aria-required="true"
              className="rounded border border-gray-400 px-3 py-2 text-[0.9rem]"
            />
          </div>
          <div>
            <label htmlFor="currency" className="mb-[0.3rem] block text-[0.85rem] font-semibold">
              Currency
            </label>
            <select
              id="currency"
              value={currency}
              onChange={(e) => setCurrency(e.target.value)}
              className="rounded border border-gray-400 px-3 py-2 text-[0.9rem]"
            >
              {SUPPORTED_CURRENCIES.map((c) => (
                <option key={c} value={c}>
                  {c}
                </option>
              ))}
            </select>
          </div>
          <button
            type="submit"
            className="cursor-pointer rounded border-none bg-blue-600 px-5 py-[0.55rem] text-[0.9rem] font-semibold text-white"
          >
            Generate Report
          </button>
        </form>
      </div>

      {isLoading && <p>Generating report...</p>}

      {isError && (
        <p role="alert" className="text-red-700">
          Failed to load report. Please try again.
        </p>
      )}

      {data && (
        <div data-testid="pl-chart">
          <div className={cardClassName}>
            <h2 className="mt-0">
              Summary: {data.currency} &mdash; {data.startDate} to {data.endDate}
            </h2>
            <div className="grid grid-cols-[repeat(auto-fill,minmax(180px,1fr))] gap-4">
              {[
                { label: "Total Income", value: data.totalIncome, color: "text-green-600" },
                { label: "Total Expense", value: data.totalExpense, color: "text-red-700" },
                {
                  label: "Net",
                  value: data.net,
                  color: parseFloat(data.net) >= 0 ? "text-green-600" : "text-red-700",
                },
              ].map(({ label, value, color }) => (
                <div key={label} className="rounded-lg border border-gray-200 bg-gray-50 p-4 text-center">
                  <div className="mb-1 text-[0.85rem] text-gray-500">{label}</div>
                  <div className={`text-[1.4rem] font-bold ${color}`}>
                    {data.currency} {value}
                  </div>
                </div>
              ))}
            </div>
          </div>

          <CategoryTable title="Income Breakdown" rows={data.incomeBreakdown} />
          <CategoryTable title="Expense Breakdown" rows={data.expenseBreakdown} />
        </div>
      )}

      {!submitted && !isLoading && (
        <p className="text-center text-gray-500">Select a date range and currency, then click Generate Report.</p>
      )}
    </AppShell>
  );
}
