import { createBdd } from "playwright-bdd";
import { setResponse } from "../../utils/response-store";
import { getTokenForUser, getLastExpenseId, setLastExpenseId } from "../../utils/token-store";

const { Given, When } = createBdd();

// ---------------------------------------------------------------------------
// Expense management steps
// ---------------------------------------------------------------------------

Given("alice has created 3 entries", async ({ request }) => {
  const token = getTokenForUser("alice");
  const entries = [
    { amount: "10.00", currency: "USD", category: "food", description: "Entry 1", date: "2025-01-01", type: "expense" },
    { amount: "20.00", currency: "USD", category: "food", description: "Entry 2", date: "2025-01-02", type: "expense" },
    { amount: "30.00", currency: "USD", category: "food", description: "Entry 3", date: "2025-01-03", type: "expense" },
  ];
  for (const entry of entries) {
    const res = await request.post("/api/v1/expenses", {
      data: entry,
      headers: {
        "Content-Type": "application/json",
        Authorization: `Bearer ${token}`,
      },
    });
    const body = (await res.json()) as Record<string, unknown>;
    if (body["id"]) {
      setLastExpenseId(body["id"] as string);
    }
  }
});

When(/^alice sends GET \/api\/v1\/expenses$/, async ({ request }) => {
  const token = getTokenForUser("alice");
  setResponse(
    await request.get("/api/v1/expenses", {
      headers: { Authorization: `Bearer ${token}` },
    }),
  );
});

When(
  /^alice sends PUT \/api\/v1\/expenses\/\{expenseId\} with body \{ "amount": "12\.00", "currency": "USD", "category": "food", "description": "Updated breakfast", "date": "2025-01-10", "type": "expense" \}$/,
  async ({ request }) => {
    const token = getTokenForUser("alice");
    const id = getLastExpenseId();
    setResponse(
      await request.put(`/api/v1/expenses/${id}`, {
        data: {
          amount: "12.00",
          currency: "USD",
          category: "food",
          description: "Updated breakfast",
          date: "2025-01-10",
          type: "expense",
        },
        headers: {
          "Content-Type": "application/json",
          Authorization: `Bearer ${token}`,
        },
      }),
    );
  },
);

When(/^alice sends DELETE \/api\/v1\/expenses\/\{expenseId\}$/, async ({ request }) => {
  const token = getTokenForUser("alice");
  const id = getLastExpenseId();
  setResponse(
    await request.delete(`/api/v1/expenses/${id}`, {
      headers: { Authorization: `Bearer ${token}` },
    }),
  );
});

When(
  /^the client sends POST \/api\/v1\/expenses with body \{ "amount": "10\.00", "currency": "USD", "category": "food", "description": "Coffee", "date": "2025-01-01", "type": "expense" \}$/,
  async ({ request }) => {
    // No auth header — intentionally unauthenticated to test 401
    setResponse(
      await request.post("/api/v1/expenses", {
        data: {
          amount: "10.00",
          currency: "USD",
          category: "food",
          description: "Coffee",
          date: "2025-01-01",
          type: "expense",
        },
        headers: { "Content-Type": "application/json" },
      }),
    );
  },
);
