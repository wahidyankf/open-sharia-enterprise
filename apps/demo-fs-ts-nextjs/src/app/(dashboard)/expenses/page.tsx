"use client";

import { useState } from "react";
import Link from "next/link";
import { useExpenses, useCreateExpense, useDeleteExpense } from "@/lib/queries/use-expenses";
import type { CreateExpenseRequest, Expense } from "@/lib/api/types";

const SUPPORTED_CURRENCIES = ["USD", "IDR"];
const SUPPORTED_UNITS = [
  "kg",
  "g",
  "mg",
  "lb",
  "oz",
  "l",
  "ml",
  "m",
  "cm",
  "km",
  "ft",
  "in",
  "unit",
  "pcs",
  "dozen",
  "box",
  "pack",
];
const EXPENSE_TYPES = ["income", "expense"];

const inputCn = "w-full px-3 py-2 border border-gray-400 rounded text-sm box-border";
const labelCn = "block mb-1 font-semibold text-sm";

interface FormErrors {
  amount?: string;
  currency?: string;
  category?: string;
  description?: string;
  date?: string;
  type?: string;
  unit?: string;
}

const EMPTY_FORM: CreateExpenseRequest = {
  amount: "",
  currency: "USD",
  category: "",
  description: "",
  date: new Date().toISOString().split("T")[0] ?? "",
  type: "expense",
  quantity: undefined,
  unit: undefined,
};

export default function ExpensesPage() {
  const [page, setPage] = useState(0);
  const [showForm, setShowForm] = useState(false);
  const [form, setForm] = useState<CreateExpenseRequest>(EMPTY_FORM);
  const [formErrors, setFormErrors] = useState<FormErrors>({});
  const [deleteConfirmId, setDeleteConfirmId] = useState<string | null>(null);
  const [createError, setCreateError] = useState<string | null>(null);

  const { data, isLoading, isError } = useExpenses(page, 20);
  const createMutation = useCreateExpense();
  const deleteMutation = useDeleteExpense();

  const validate = (): boolean => {
    const errors: FormErrors = {};
    const amountNum = parseFloat(form.amount);
    if (!form.amount) {
      errors.amount = "Amount is required";
    } else if (isNaN(amountNum) || amountNum < 0) {
      errors.amount = "Amount must be a non-negative number";
    }
    if (!SUPPORTED_CURRENCIES.includes(form.currency.trim().toUpperCase())) {
      errors.currency = "Invalid currency. Supported: USD, IDR";
    }
    if (!form.category.trim()) errors.category = "Category is required";
    if (!form.description.trim()) errors.description = "Description is required";
    if (!form.date) errors.date = "Date is required";
    if (!EXPENSE_TYPES.includes(form.type.trim().toLowerCase())) errors.type = "Type is required";
    if (form.unit && !SUPPORTED_UNITS.includes(form.unit.trim().toLowerCase())) {
      errors.unit = "Invalid unit";
    }
    setFormErrors(errors);
    return Object.keys(errors).length === 0;
  };

  const handleCreate = (e: React.FormEvent<HTMLFormElement>) => {
    e.preventDefault();
    setCreateError(null);
    if (!validate()) return;

    const payload: CreateExpenseRequest = {
      ...form,
      currency: form.currency.trim().toUpperCase(),
      type: form.type.trim().toLowerCase() as CreateExpenseRequest["type"],
      quantity: form.quantity ?? undefined,
      unit: form.unit || undefined,
    };

    createMutation.mutate(payload, {
      onSuccess: () => {
        setShowForm(false);
        setForm(EMPTY_FORM);
        setFormErrors({});
      },
      onError: () => setCreateError("Failed to create expense."),
    });
  };

  const handleDelete = (id: string) => {
    deleteMutation.mutate(id, {
      onSuccess: () => setDeleteConfirmId(null),
    });
  };

  const totalPages = data?.totalPages ?? 1;

  return (
    <>
      <div className="mb-6 flex items-center justify-between">
        <h1 className="m-0">Expenses</h1>
        <button
          onClick={() => setShowForm((s) => !s)}
          className="cursor-pointer rounded border-none bg-blue-600 px-5 py-2.5 font-semibold text-white"
        >
          {showForm ? "Cancel" : "New Expense"}
        </button>
      </div>

      {showForm && (
        <div className="mb-6 rounded-lg border border-gray-300 bg-white p-6 shadow-md">
          <h2 className="mt-0">New Expense</h2>
          {createError && (
            <div id="create-error" role="alert" className="mb-4 rounded bg-red-50 px-4 py-2.5 text-red-700">
              {createError}
            </div>
          )}
          <form onSubmit={handleCreate} noValidate aria-describedby={createError ? "create-error" : undefined}>
            <div className="mb-4 grid grid-cols-[repeat(auto-fill,minmax(200px,1fr))] gap-4">
              <div>
                <label htmlFor="amount" className={labelCn}>
                  Amount
                </label>
                <input
                  id="amount"
                  type="number"
                  min="0"
                  step="0.01"
                  value={form.amount}
                  onChange={(e) => setForm({ ...form, amount: e.target.value })}
                  aria-required="true"
                  aria-describedby={formErrors.amount ? "amount-error" : undefined}
                  aria-invalid={!!formErrors.amount}
                  className={inputCn}
                />
                {formErrors.amount && (
                  <span id="amount-error" role="alert" className="text-xs text-red-700">
                    {formErrors.amount}
                  </span>
                )}
              </div>

              <div>
                <label htmlFor="currency" className={labelCn}>
                  Currency
                </label>
                <input
                  id="currency"
                  type="text"
                  list="create-currency-list"
                  value={form.currency}
                  onChange={(e) => setForm({ ...form, currency: e.target.value })}
                  aria-required="true"
                  className={inputCn}
                />
                <datalist id="create-currency-list">
                  {SUPPORTED_CURRENCIES.map((c) => (
                    <option key={c} value={c} />
                  ))}
                </datalist>
                {formErrors.currency && (
                  <span role="alert" className="text-xs text-red-700">
                    {formErrors.currency}
                  </span>
                )}
              </div>

              <div>
                <label htmlFor="type" className={labelCn}>
                  Type
                </label>
                <input
                  id="type"
                  type="text"
                  list="create-type-list"
                  value={form.type}
                  onChange={(e) => setForm({ ...form, type: e.target.value as CreateExpenseRequest["type"] })}
                  className={inputCn}
                />
                <datalist id="create-type-list">
                  {EXPENSE_TYPES.map((t) => (
                    <option key={t} value={t} />
                  ))}
                </datalist>
                {formErrors.type && (
                  <span role="alert" className="text-xs text-red-700">
                    {formErrors.type}
                  </span>
                )}
              </div>

              <div>
                <label htmlFor="category" className={labelCn}>
                  Category
                </label>
                <input
                  id="category"
                  type="text"
                  value={form.category}
                  onChange={(e) => setForm({ ...form, category: e.target.value })}
                  aria-required="true"
                  aria-describedby={formErrors.category ? "category-error" : undefined}
                  aria-invalid={!!formErrors.category}
                  className={inputCn}
                />
                {formErrors.category && (
                  <span id="category-error" role="alert" className="text-xs text-red-700">
                    {formErrors.category}
                  </span>
                )}
              </div>

              <div>
                <label htmlFor="date" className={labelCn}>
                  Date
                </label>
                <input
                  id="date"
                  type="date"
                  value={form.date}
                  onChange={(e) => setForm({ ...form, date: e.target.value })}
                  aria-required="true"
                  className={inputCn}
                />
              </div>

              <div>
                <label htmlFor="quantity" className={labelCn}>
                  Quantity (optional)
                </label>
                <input
                  id="quantity"
                  type="number"
                  min="0"
                  step="any"
                  value={form.quantity ?? ""}
                  onChange={(e) =>
                    setForm({
                      ...form,
                      quantity: e.target.value ? parseFloat(e.target.value) : undefined,
                    })
                  }
                  className={inputCn}
                />
              </div>

              <div>
                <label htmlFor="unit" className={labelCn}>
                  Unit (optional)
                </label>
                <input
                  id="unit"
                  type="text"
                  list="create-unit-list"
                  value={form.unit ?? ""}
                  onChange={(e) => setForm({ ...form, unit: e.target.value || undefined })}
                  className={inputCn}
                />
                <datalist id="create-unit-list">
                  {SUPPORTED_UNITS.map((u) => (
                    <option key={u} value={u} />
                  ))}
                </datalist>
                {formErrors.unit && (
                  <span role="alert" className="text-xs text-red-700">
                    {formErrors.unit}
                  </span>
                )}
              </div>
            </div>

            <div className="mb-4">
              <label htmlFor="description" className={labelCn}>
                Description
              </label>
              <input
                id="description"
                type="text"
                value={form.description}
                onChange={(e) => setForm({ ...form, description: e.target.value })}
                aria-required="true"
                aria-describedby={formErrors.description ? "desc-error" : undefined}
                aria-invalid={!!formErrors.description}
                className={inputCn}
              />
              {formErrors.description && (
                <span id="desc-error" role="alert" className="text-xs text-red-700">
                  {formErrors.description}
                </span>
              )}
            </div>

            <button
              type="submit"
              disabled={createMutation.isPending}
              className={`rounded border-none bg-blue-600 px-5 py-2.5 font-semibold text-white ${createMutation.isPending ? "cursor-not-allowed" : "cursor-pointer"}`}
            >
              {createMutation.isPending ? "Creating..." : "Create Expense"}
            </button>
          </form>
        </div>
      )}

      {isLoading && <p>Loading expenses...</p>}
      {isError && (
        <p role="alert" className="text-red-700">
          Failed to load expenses.
        </p>
      )}

      {deleteConfirmId && (
        <div
          role="alertdialog"
          aria-modal="true"
          aria-labelledby="delete-dialog-title"
          className="fixed inset-0 z-[300] flex items-center justify-center bg-black/40"
        >
          <div className="w-[22rem] rounded-lg bg-white p-6">
            <h2 id="delete-dialog-title" className="mt-0">
              Delete Expense
            </h2>
            <p>Are you sure you want to delete this expense?</p>
            <div className="flex gap-3">
              <button
                onClick={() => handleDelete(deleteConfirmId)}
                disabled={deleteMutation.isPending}
                className="cursor-pointer rounded border-none bg-red-700 px-4 py-2 font-semibold text-white"
              >
                {deleteMutation.isPending ? "Deleting..." : "Delete"}
              </button>
              <button
                onClick={() => setDeleteConfirmId(null)}
                className="cursor-pointer rounded border border-gray-400 bg-white px-4 py-2 text-gray-800"
              >
                Cancel
              </button>
            </div>
          </div>
        </div>
      )}

      {data && (
        <>
          {data.totalElements !== undefined && (
            <p className="mb-3 text-sm text-gray-600">{data.totalElements} entries</p>
          )}
          <div className="overflow-x-auto">
            <table className="w-full border-collapse overflow-hidden rounded-lg bg-white shadow-md">
              <thead>
                <tr className="bg-gray-200">
                  {["Date", "Description", "Category", "Type", "Amount", "Actions"].map((h) => (
                    <th
                      key={h}
                      className="px-3 py-3 text-left text-sm font-bold tracking-[0.04em] text-gray-600 uppercase"
                    >
                      {h}
                    </th>
                  ))}
                </tr>
              </thead>
              <tbody>
                {data.content.map((expense: Expense, idx: number) => (
                  <tr
                    key={expense.id}
                    data-testid="entry-card"
                    className={`border-b border-gray-200 ${idx % 2 === 0 ? "bg-white" : "bg-gray-50"}`}
                  >
                    <td className="px-3 py-3 text-sm">{expense.date}</td>
                    <td className="px-3 py-3">
                      <Link href={`/expenses/${expense.id}`} className="text-blue-600 no-underline">
                        {expense.description}
                      </Link>
                    </td>
                    <td className="px-3 py-3 text-sm">{expense.category}</td>
                    <td className="px-3 py-3 text-sm">
                      <span
                        className={`font-semibold ${expense.type === "income" ? "text-green-600" : "text-red-700"}`}
                      >
                        {expense.type}
                      </span>
                    </td>
                    <td
                      className={`px-3 py-3 font-semibold ${expense.type === "income" ? "text-green-600" : "text-red-700"}`}
                    >
                      {expense.currency} {expense.amount}
                    </td>
                    <td className="px-3 py-3 whitespace-nowrap">
                      <Link
                        href={`/expenses/${expense.id}`}
                        className="mr-2 inline-block rounded bg-blue-600 px-2.5 py-1 text-xs font-semibold text-white no-underline"
                        aria-label={`Edit expense: ${expense.description}`}
                      >
                        Edit
                      </Link>
                      <button
                        onClick={() => setDeleteConfirmId(expense.id)}
                        className="cursor-pointer rounded border-none bg-red-700 px-2.5 py-1 text-xs font-semibold text-white"
                        aria-label={`Delete expense: ${expense.description}`}
                      >
                        Delete
                      </button>
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>

          {data.content.length === 0 && (
            <p className="mt-8 text-center text-gray-500">No expenses found. Create your first expense!</p>
          )}

          <div data-testid="pagination" className="mt-6 flex items-center justify-center gap-2">
            <button
              onClick={() => setPage((p) => Math.max(0, p - 1))}
              disabled={page === 0}
              aria-label="Previous page"
              className={`rounded border border-gray-400 px-4 py-2 ${page === 0 ? "cursor-not-allowed bg-gray-100" : "cursor-pointer bg-white"}`}
            >
              Previous
            </button>
            <span className="text-gray-600">
              Page {page + 1} of {totalPages}
            </span>
            <button
              onClick={() => setPage((p) => Math.min(totalPages - 1, p + 1))}
              disabled={page >= totalPages - 1}
              aria-label="Next page"
              className={`rounded border border-gray-400 px-4 py-2 ${page >= totalPages - 1 ? "cursor-not-allowed bg-gray-100" : "cursor-pointer bg-white"}`}
            >
              Next
            </button>
          </div>
        </>
      )}
    </>
  );
}
