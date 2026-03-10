import { createBdd } from "playwright-bdd";

const { Given, When } = createBdd();

// Stubs — implement alongside production features

Given("alice has created 3 entries", async () => {
  throw new Error("TODO: not implemented");
});

When(/^alice sends GET \/api\/v1\/expenses$/, async () => {
  throw new Error("TODO: not implemented");
});

When(
  /^alice sends PUT \/api\/v1\/expenses\/\{expenseId\} with body \{ "amount": "12\.00", "currency": "USD", "category": "food", "description": "Updated breakfast", "date": "2025-01-10", "type": "expense" \}$/,
  async () => {
    throw new Error("TODO: not implemented");
  },
);

When(/^alice sends DELETE \/api\/v1\/expenses\/\{expenseId\}$/, async () => {
  throw new Error("TODO: not implemented");
});

When(
  /^the client sends POST \/api\/v1\/expenses with body \{ "amount": "10\.00", "currency": "USD", "category": "food", "description": "Coffee", "date": "2025-01-01", "type": "expense" \}$/,
  async () => {
    throw new Error("TODO: not implemented");
  },
);
