import { Given, When } from "@cucumber/cucumber";
import type { CustomWorld } from "../world";

async function createExpense(world: CustomWorld, username: string, body: Record<string, unknown>): Promise<string> {
  const resp = await world.dispatch("POST", "/api/v1/expenses", body, world.getAuth(username));
  if (resp.status !== 201) throw new Error(`Create expense failed: ${JSON.stringify(resp.body)}`);
  return (resp.body as { id: string }).id;
}

Given(/^alice has created an entry with body (.+)$/, async function (this: CustomWorld, bodyStr: string) {
  this.context.expenseId = await createExpense(this, "alice", JSON.parse(bodyStr));
});

Given(/^alice has created an expense with body (.+)$/, async function (this: CustomWorld, bodyStr: string) {
  this.context.expenseId = await createExpense(this, "alice", JSON.parse(bodyStr));
});

Given("alice has created 3 entries", async function (this: CustomWorld) {
  for (let i = 0; i < 3; i++) {
    await createExpense(this, "alice", {
      amount: "10.00",
      currency: "USD",
      category: "food",
      description: `Entry ${i}`,
      date: "2025-01-15",
      type: "expense",
    });
  }
});

Given(/^bob has created an entry with body (.+)$/, async function (this: CustomWorld, bodyStr: string) {
  this.context.bobExpenseId = await createExpense(this, "bob", JSON.parse(bodyStr));
});

When(/^alice sends POST \/api\/v1\/expenses with body (.+)$/, async function (this: CustomWorld, bodyStr: string) {
  this.response = await this.dispatch("POST", "/api/v1/expenses", JSON.parse(bodyStr), this.getAuth("alice"));
});

When(/^the client sends POST \/api\/v1\/expenses with body (.+)$/, async function (this: CustomWorld, bodyStr: string) {
  this.response = await this.dispatch("POST", "/api/v1/expenses", JSON.parse(bodyStr), null);
});

When(/^alice sends GET \/api\/v1\/expenses\/\{expenseId\}$/, async function (this: CustomWorld) {
  this.response = await this.dispatch("GET", `/api/v1/expenses/${this.context.expenseId}`, null, this.getAuth("alice"));
});

When(/^alice sends GET \/api\/v1\/expenses$/, async function (this: CustomWorld) {
  this.response = await this.dispatch("GET", "/api/v1/expenses", null, this.getAuth("alice"));
});

When(
  /^alice sends PUT \/api\/v1\/expenses\/\{expenseId\} with body (.+)$/,
  async function (this: CustomWorld, bodyStr: string) {
    this.response = await this.dispatch(
      "PUT",
      `/api/v1/expenses/${this.context.expenseId}`,
      JSON.parse(bodyStr),
      this.getAuth("alice"),
    );
  },
);

When(/^alice sends DELETE \/api\/v1\/expenses\/\{expenseId\}$/, async function (this: CustomWorld) {
  this.response = await this.dispatch(
    "DELETE",
    `/api/v1/expenses/${this.context.expenseId}`,
    null,
    this.getAuth("alice"),
  );
});

When(/^alice sends GET \/api\/v1\/expenses\/summary$/, async function (this: CustomWorld) {
  this.response = await this.dispatch("GET", "/api/v1/expenses/summary", null, this.getAuth("alice"));
});

export { createExpense };
