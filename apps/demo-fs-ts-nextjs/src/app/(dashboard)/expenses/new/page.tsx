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

const inputStyle: React.CSSProperties = {
  width: "100%",
  padding: "0.5rem 0.75rem",
  border: "1px solid #ccc",
  borderRadius: "4px",
  fontSize: "0.9rem",
  boxSizing: "border-box",
};

const labelStyle: React.CSSProperties = {
  display: "block",
  marginBottom: "0.3rem",
  fontWeight: "600",
  fontSize: "0.85rem",
};

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
      <div style={{ marginBottom: "1.5rem" }}>
        <a href="/expenses" style={{ color: "#1a73e8", fontSize: "0.9rem" }}>
          &#8592; Back to Expenses
        </a>
        <h1 style={{ marginTop: "0.5rem", marginBottom: 0 }}>New Expense</h1>
      </div>

      <div
        style={{
          backgroundColor: "#fff",
          padding: "1.5rem",
          borderRadius: "8px",
          border: "1px solid #ddd",
          boxShadow: "0 2px 8px rgba(0,0,0,0.06)",
        }}
      >
        {createError && (
          <div
            id="create-error"
            role="alert"
            style={{
              backgroundColor: "#fdf2f2",
              color: "#c0392b",
              padding: "0.6rem 1rem",
              borderRadius: "4px",
              marginBottom: "1rem",
            }}
          >
            {createError}
          </div>
        )}

        <form onSubmit={handleSubmit} noValidate aria-describedby={createError ? "create-error" : undefined}>
          <div
            style={{
              display: "grid",
              gridTemplateColumns: "repeat(auto-fill, minmax(200px, 1fr))",
              gap: "1rem",
              marginBottom: "1rem",
            }}
          >
            <div>
              <label htmlFor="amount" style={labelStyle}>
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
                style={inputStyle}
              />
              {formErrors.amount && (
                <span id="amount-error" role="alert" style={{ color: "#c0392b", fontSize: "0.8rem" }}>
                  {formErrors.amount}
                </span>
              )}
            </div>

            <div>
              <label htmlFor="currency" style={labelStyle}>
                Currency
              </label>
              <input
                id="currency"
                type="text"
                list="currency-list"
                value={form.currency}
                onChange={(e) => setForm({ ...form, currency: e.target.value })}
                aria-required="true"
                style={inputStyle}
              />
              <datalist id="currency-list">
                {SUPPORTED_CURRENCIES.map((c) => (
                  <option key={c} value={c} />
                ))}
              </datalist>
              {formErrors.currency && (
                <span role="alert" style={{ color: "#c0392b", fontSize: "0.8rem" }}>
                  {formErrors.currency}
                </span>
              )}
            </div>

            <div>
              <label htmlFor="type" style={labelStyle}>
                Type
              </label>
              <input
                id="type"
                type="text"
                list="type-list"
                value={form.type}
                onChange={(e) => setForm({ ...form, type: e.target.value as CreateExpenseRequest["type"] })}
                style={inputStyle}
              />
              <datalist id="type-list">
                {EXPENSE_TYPES.map((t) => (
                  <option key={t} value={t} />
                ))}
              </datalist>
              {formErrors.type && (
                <span role="alert" style={{ color: "#c0392b", fontSize: "0.8rem" }}>
                  {formErrors.type}
                </span>
              )}
            </div>

            <div>
              <label htmlFor="category" style={labelStyle}>
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
                style={inputStyle}
              />
              {formErrors.category && (
                <span id="category-error" role="alert" style={{ color: "#c0392b", fontSize: "0.8rem" }}>
                  {formErrors.category}
                </span>
              )}
            </div>

            <div>
              <label htmlFor="date" style={labelStyle}>
                Date
              </label>
              <input
                id="date"
                type="date"
                value={form.date}
                onChange={(e) => setForm({ ...form, date: e.target.value })}
                aria-required="true"
                style={inputStyle}
              />
            </div>

            <div>
              <label htmlFor="quantity" style={labelStyle}>
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
                style={inputStyle}
              />
            </div>

            <div>
              <label htmlFor="unit" style={labelStyle}>
                Unit (optional)
              </label>
              <input
                id="unit"
                type="text"
                list="unit-list"
                value={form.unit ?? ""}
                onChange={(e) => setForm({ ...form, unit: e.target.value || undefined })}
                style={inputStyle}
              />
              <datalist id="unit-list">
                {SUPPORTED_UNITS.map((u) => (
                  <option key={u} value={u} />
                ))}
              </datalist>
            </div>
          </div>

          <div style={{ marginBottom: "1rem" }}>
            <label htmlFor="description" style={labelStyle}>
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
              style={inputStyle}
            />
            {formErrors.description && (
              <span id="desc-error" role="alert" style={{ color: "#c0392b", fontSize: "0.8rem" }}>
                {formErrors.description}
              </span>
            )}
          </div>

          <div style={{ display: "flex", gap: "0.75rem" }}>
            <button
              type="submit"
              disabled={createMutation.isPending}
              style={{
                padding: "0.6rem 1.25rem",
                backgroundColor: "#1a73e8",
                color: "#fff",
                border: "none",
                borderRadius: "4px",
                cursor: createMutation.isPending ? "not-allowed" : "pointer",
                fontWeight: "600",
              }}
            >
              {createMutation.isPending ? "Creating..." : "Create Expense"}
            </button>
            <button
              type="button"
              onClick={() => router.push("/expenses")}
              style={{
                padding: "0.6rem 1.25rem",
                backgroundColor: "#fff",
                color: "#333",
                border: "1px solid #ccc",
                borderRadius: "4px",
                cursor: "pointer",
              }}
            >
              Cancel
            </button>
          </div>
        </form>
      </div>
    </>
  );
}
