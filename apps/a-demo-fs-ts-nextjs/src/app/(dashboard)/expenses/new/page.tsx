"use client";

import { useState } from "react";
import { useRouter } from "next/navigation";
import { useCreateExpense } from "@/lib/queries/use-expenses";
import type { CreateExpenseRequest } from "@/lib/api/types";

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

export default function NewExpensePage() {
  const router = useRouter();
  const createMutation = useCreateExpense();
  const [form, setForm] = useState<CreateExpenseRequest>(EMPTY_FORM);
  const [formErrors, setFormErrors] = useState<FormErrors>({});
  const [createError, setCreateError] = useState<string | null>(null);

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
    setFormErrors(errors);
    return Object.keys(errors).length === 0;
  };

  const handleSubmit = (e: React.FormEvent<HTMLFormElement>) => {
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
      onSuccess: (expense) => {
        router.push(`/expenses/${expense.id}`);
      },
      onError: () => setCreateError("Failed to create expense."),
    });
  };

  return (
    <>
      <div className="mb-6">
        <a href="/expenses" className="text-sm text-blue-600">
          &#8592; Back to Expenses
        </a>
        <h1 className="mt-2 mb-0">New Expense</h1>
      </div>

      <div className="rounded-lg border border-gray-300 bg-white p-6 shadow-md">
        {createError && (
          <div id="create-error" role="alert" className="mb-4 rounded bg-red-50 px-4 py-2.5 text-red-700">
            {createError}
          </div>
        )}

        <form onSubmit={handleSubmit} noValidate aria-describedby={createError ? "create-error" : undefined}>
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
                list="currency-list"
                value={form.currency}
                onChange={(e) => setForm({ ...form, currency: e.target.value })}
                aria-required="true"
                className={inputCn}
              />
              <datalist id="currency-list">
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
                list="type-list"
                value={form.type}
                onChange={(e) => setForm({ ...form, type: e.target.value as CreateExpenseRequest["type"] })}
                className={inputCn}
              />
              <datalist id="type-list">
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
                  setForm({ ...form, quantity: e.target.value ? parseFloat(e.target.value) : undefined })
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
                list="unit-list"
                value={form.unit ?? ""}
                onChange={(e) => setForm({ ...form, unit: e.target.value || undefined })}
                className={inputCn}
              />
              <datalist id="unit-list">
                {SUPPORTED_UNITS.map((u) => (
                  <option key={u} value={u} />
                ))}
              </datalist>
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

          <div className="flex gap-3">
            <button
              type="submit"
              disabled={createMutation.isPending}
              className={`rounded border-none bg-blue-600 px-5 py-2.5 font-semibold text-white ${createMutation.isPending ? "cursor-not-allowed" : "cursor-pointer"}`}
            >
              {createMutation.isPending ? "Creating..." : "Create Expense"}
            </button>
            <button
              type="button"
              onClick={() => router.push("/expenses")}
              className="cursor-pointer rounded border border-gray-400 bg-white px-5 py-2.5 text-gray-800"
            >
              Cancel
            </button>
          </div>
        </form>
      </div>
    </>
  );
}
