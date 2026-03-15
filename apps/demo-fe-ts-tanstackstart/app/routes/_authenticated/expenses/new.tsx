import { useState } from "react";
import { createFileRoute, useNavigate } from "@tanstack/react-router";
import { AppShell } from "~/components/layout/app-shell";
import { useCreateExpense } from "~/lib/queries/use-expenses";
import type { CreateExpenseRequest } from "~/lib/api/types";

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
const EXPENSE_TYPES = ["INCOME", "EXPENSE"];

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
  unit?: string;
}

const EMPTY_FORM: CreateExpenseRequest = {
  amount: "",
  currency: "USD",
  category: "",
  description: "",
  date: new Date().toISOString().split("T")[0] ?? "",
  type: "EXPENSE",
  quantity: undefined,
  unit: undefined,
};

function NewExpensePage() {
  const navigate = useNavigate();
  const [form, setForm] = useState<CreateExpenseRequest>(EMPTY_FORM);
  const [formErrors, setFormErrors] = useState<FormErrors>({});
  const [createError, setCreateError] = useState<string | null>(null);

  const createMutation = useCreateExpense();

  const validate = (): boolean => {
    const errors: FormErrors = {};
    const amountNum = parseFloat(form.amount);
    if (!form.amount) {
      errors.amount = "Amount is required";
    } else if (isNaN(amountNum) || amountNum < 0) {
      errors.amount = "Amount must be a non-negative number";
    }
    if (!SUPPORTED_CURRENCIES.includes(form.currency)) {
      errors.currency = `Currency must be one of: ${SUPPORTED_CURRENCIES.join(", ")}`;
    }
    if (!form.category.trim()) errors.category = "Category is required";
    if (!form.description.trim()) errors.description = "Description is required";
    if (!form.date) errors.date = "Date is required";
    if (!EXPENSE_TYPES.includes(form.type)) errors.type = "Type is required";
    if (form.unit && !SUPPORTED_UNITS.includes(form.unit)) {
      errors.unit = `Unit must be one of: ${SUPPORTED_UNITS.join(", ")}`;
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
      quantity: form.quantity ?? undefined,
      unit: form.unit || undefined,
    };

    createMutation.mutate(payload, {
      onSuccess: () => {
        void navigate({ to: "/expenses" });
      },
      onError: () => setCreateError("Failed to create expense."),
    });
  };

  return (
    <AppShell>
      <div
        style={{
          display: "flex",
          justifyContent: "space-between",
          alignItems: "center",
          marginBottom: "1.5rem",
        }}
      >
        <h1 style={{ margin: 0 }}>New Expense</h1>
      </div>

      <div
        style={{
          backgroundColor: "#fff",
          padding: "1.5rem",
          borderRadius: "8px",
          border: "1px solid #ddd",
          marginBottom: "1.5rem",
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
        <form onSubmit={handleCreate} noValidate aria-describedby={createError ? "create-error" : undefined}>
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
              <select
                id="currency"
                value={form.currency}
                onChange={(e) => setForm({ ...form, currency: e.target.value })}
                aria-required="true"
                style={inputStyle}
              >
                {SUPPORTED_CURRENCIES.map((c) => (
                  <option key={c} value={c}>
                    {c}
                  </option>
                ))}
              </select>
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
              <select
                id="type"
                value={form.type}
                onChange={(e) => setForm({ ...form, type: e.target.value })}
                style={inputStyle}
              >
                {EXPENSE_TYPES.map((t) => (
                  <option key={t} value={t}>
                    {t}
                  </option>
                ))}
              </select>
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
                  setForm({
                    ...form,
                    quantity: e.target.value ? parseFloat(e.target.value) : undefined,
                  })
                }
                style={inputStyle}
              />
            </div>

            <div>
              <label htmlFor="unit" style={labelStyle}>
                Unit (optional)
              </label>
              <select
                id="unit"
                value={form.unit ?? ""}
                onChange={(e) => setForm({ ...form, unit: e.target.value || undefined })}
                style={inputStyle}
              >
                <option value="">None</option>
                {SUPPORTED_UNITS.map((u) => (
                  <option key={u} value={u}>
                    {u}
                  </option>
                ))}
              </select>
              {formErrors.unit && (
                <span role="alert" style={{ color: "#c0392b", fontSize: "0.8rem" }}>
                  {formErrors.unit}
                </span>
              )}
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
        </form>
      </div>
    </AppShell>
  );
}

export const Route = createFileRoute("/_authenticated/expenses/new")({
  component: NewExpensePage,
});
