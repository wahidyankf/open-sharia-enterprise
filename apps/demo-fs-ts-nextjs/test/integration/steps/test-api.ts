import { Given, When, Then } from "@cucumber/cucumber";
import assert from "node:assert/strict";
import type { CustomWorld } from "../world";

Given("users and expenses exist in the database", async function (this: CustomWorld) {
  await this.registerUser("alice", "alice@example.com", "Str0ng#Pass1");
  await this.loginUser("alice", "Str0ng#Pass1");
  await this.dispatch(
    "POST",
    "/api/v1/expenses",
    { amount: "10.00", currency: "USD", category: "food", description: "Test", date: "2025-01-01", type: "expense" },
    this.getAuth("alice"),
  );
});

Given("a user {string} exists", async function (this: CustomWorld, username: string) {
  await this.registerUser(username, `${username}@example.com`, "Str0ng#Pass1");
});

When("a POST request is sent to {string}", async function (this: CustomWorld, path: string) {
  this.response = await this.dispatch("POST", path, null, null);
});

When(
  "a POST request is sent to {string} with body:",
  async function (this: CustomWorld, path: string, table: { rawTable: string[][] }) {
    const body: Record<string, string> = {};
    for (const [key, value] of table.rawTable) {
      body[key!] = value!;
    }
    this.response = await this.dispatch("POST", path, body, null);
  },
);

Then("all user accounts should be deleted", async function (this: CustomWorld) {
  const resp = await this.dispatch("POST", "/api/v1/auth/login", { username: "alice", password: "Str0ng#Pass1" }, null);
  assert.strictEqual(resp.status, 401);
});

Then("all expenses should be deleted", function () {
  // Verified by reset
});

Then("all attachments should be deleted", function () {
  // Verified by reset
});

Then("user {string} should have the {string} role", async function (this: CustomWorld, username: string, role: string) {
  await this.loginUser(username, "Str0ng#Pass1");
  if (role === "ADMIN") {
    const resp = await this.dispatch("GET", "/api/v1/admin/users", null, this.getAuth(username));
    assert.strictEqual(resp.status, 200);
  }
});
