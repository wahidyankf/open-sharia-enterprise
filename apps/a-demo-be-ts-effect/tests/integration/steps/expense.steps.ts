import { Given, When, Then } from "@cucumber/cucumber";
import assert from "node:assert/strict";
import type { CustomWorld } from "../world.js";

// Helper: create an expense and store its ID
async function createExpense(world: CustomWorld, token: string, body: Record<string, unknown>): Promise<string> {
  const res = await world.post("/api/v1/expenses", body, token);
  if (res.status !== 201) {
    throw new Error(`Failed to create expense: ${JSON.stringify(res.body)}`);
  }
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  return (res.body as any).id as string;
}

// ---- Given ----

Given(/^alice has created an entry with body (.+)$/, async function (this: CustomWorld, bodyStr: string) {
  const body = JSON.parse(bodyStr) as Record<string, unknown>;
  const token = this.tokens.get("alice_access") ?? "";
  const id = await createExpense(this, token, body);
  this.context["expenseId"] = id;
});

Given(/^alice has created an expense with body (.+)$/, async function (this: CustomWorld, bodyStr: string) {
  const body = JSON.parse(bodyStr) as Record<string, unknown>;
  const token = this.tokens.get("alice_access") ?? "";
  const id = await createExpense(this, token, body);
  this.context["expenseId"] = id;
});

Given("alice has created 3 entries", async function (this: CustomWorld) {
  const token = this.tokens.get("alice_access") ?? "";
  for (let i = 1; i <= 3; i++) {
    await createExpense(this, token, {
      amount: `${i * 10}.00`,
      currency: "USD",
      category: "food",
      description: `Entry ${i}`,
      date: `2025-01-${String(i).padStart(2, "0")}`,
      type: "expense",
    });
  }
});

Given(/^bob has created an entry with body (.+)$/, async function (this: CustomWorld, bodyStr: string) {
  const body = JSON.parse(bodyStr) as Record<string, unknown>;
  // Ensure bob is logged in
  let bobToken = this.tokens.get("bob_access") ?? "";
  if (!bobToken) {
    const password = (this.context["bob_password"] as string | undefined) ?? "Str0ng#Pass2";
    const loginRes = await this.post("/api/v1/auth/login", { username: "bob", password });
    if (loginRes.status !== 200) {
      throw new Error(`Bob not logged in: ${JSON.stringify(loginRes.body)}`);
    }
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    bobToken = (loginRes.body as any).accessToken as string;
    this.tokens.set("bob_access", bobToken);
  }
  const id = await createExpense(this, bobToken, body);
  this.context["bobExpenseId"] = id;
});

// ---- When: alice sends ----

When(/^alice sends POST \/api\/v1\/expenses with body (.+)$/, async function (this: CustomWorld, bodyStr: string) {
  const body = JSON.parse(bodyStr) as Record<string, unknown>;
  const token = this.tokens.get("alice_access") ?? "";
  this.response = await this.post("/api/v1/expenses", body, token);
});

When(/^alice sends GET \/api\/v1\/expenses\/\{expenseId\}$/, async function (this: CustomWorld) {
  const token = this.tokens.get("alice_access") ?? "";
  const expenseId = this.context["expenseId"] as string;
  this.response = await this.get(`/api/v1/expenses/${expenseId}`, token);
});

When(/^alice sends GET \/api\/v1\/expenses$/, async function (this: CustomWorld) {
  const token = this.tokens.get("alice_access") ?? "";
  this.response = await this.get("/api/v1/expenses", token);
});

When(/^alice sends GET \/api\/v1\/expenses\/summary$/, async function (this: CustomWorld) {
  const token = this.tokens.get("alice_access") ?? "";
  this.response = await this.get("/api/v1/expenses/summary", token);
});

When(
  /^alice sends PUT \/api\/v1\/expenses\/\{expenseId\} with body (.+)$/,
  async function (this: CustomWorld, bodyStr: string) {
    const body = JSON.parse(bodyStr) as Record<string, unknown>;
    const token = this.tokens.get("alice_access") ?? "";
    const expenseId = this.context["expenseId"] as string;
    this.response = await this.put(`/api/v1/expenses/${expenseId}`, body, token);
  },
);

When(/^alice sends DELETE \/api\/v1\/expenses\/\{expenseId\}$/, async function (this: CustomWorld) {
  const token = this.tokens.get("alice_access") ?? "";
  const expenseId = this.context["expenseId"] as string;
  this.response = await this.delete(`/api/v1/expenses/${expenseId}`, token);
});

// ---- Then ----

// Numeric equality (for quantity fields): matches unquoted numbers like 50.5 or 10
Then(
  /^the response body should contain "([^"]*)" equal to (\d+(?:\.\d+)?)$/,
  function (this: CustomWorld, field: string, valueStr: string) {
    assert.ok(this.response !== null);
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const body = this.response?.body as Record<string, unknown>;
    const numValue = parseFloat(valueStr);
    // stored as string (e.g. "50.5") or number
    const actual = typeof body[field] === "string" ? parseFloat(body[field] as string) : (body[field] as number);
    assert.strictEqual(actual, numValue);
  },
);

Then(
  /^the response body should contain "([^"]*)" total equal to "([^"]*)"$/,
  function (this: CustomWorld, currency: string, amount: string) {
    assert.ok(this.response !== null);
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const body = this.response?.body as Record<string, string>;
    assert.strictEqual(String(body[currency]), amount);
  },
);
