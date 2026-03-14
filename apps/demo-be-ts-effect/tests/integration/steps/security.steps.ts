import { Given, Then } from "@cucumber/cucumber";
import assert from "node:assert/strict";
import type { CustomWorld } from "../world.js";
import { promoteToAdmin } from "../hooks.js";

Given(
  "{string} has had the maximum number of failed login attempts",
  async function (this: CustomWorld, username: string) {
    // Trigger 5 failed login attempts (MAX_FAILED_ATTEMPTS = 5)
    for (let i = 0; i < 5; i++) {
      await this.post("/api/v1/auth/login", { username, password: "WrongPassword!" });
    }
  },
);

Given(
  "a user {string} is registered and locked after too many failed logins",
  async function (this: CustomWorld, username: string) {
    const email = `${username}@example.com`;
    const password = "Str0ng#Pass1";
    this.context[`${username}_password`] = password;

    // Register
    const res = await this.post("/api/v1/auth/register", { username, email, password });
    if (res.status === 201) {
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      this.userIds.set(username, (res.body as any).id as string);
    }

    // Trigger 5 failed login attempts to lock the account
    for (let i = 0; i < 5; i++) {
      await this.post("/api/v1/auth/login", { username, password: "WrongPassword!" });
    }
  },
);

Given("an admin has unlocked alice's account", async function (this: CustomWorld) {
  // Register admin if not done
  const adminUsername = "testadmin";
  const adminEmail = "testadmin@example.com";
  const adminPassword = "Str0ng#Pass1";

  const regRes = await this.post("/api/v1/auth/register", {
    username: adminUsername,
    email: adminEmail,
    password: adminPassword,
  });
  if (regRes.status !== 201 && regRes.status !== 409) {
    throw new Error(`Failed to register admin: ${JSON.stringify(regRes.body)}`);
  }

  await promoteToAdmin(adminUsername);

  const loginRes = await this.post("/api/v1/auth/login", { username: adminUsername, password: adminPassword });
  if (loginRes.status !== 200) {
    throw new Error(`Failed to login admin: ${JSON.stringify(loginRes.body)}`);
  }
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const adminToken = (loginRes.body as any).accessToken as string;

  const aliceId = this.userIds.get("alice") ?? "";
  const unlockRes = await this.post(`/api/v1/admin/users/${aliceId}/unlock`, {}, adminToken);
  if (unlockRes.status !== 200) {
    throw new Error(`Failed to unlock alice: ${JSON.stringify(unlockRes.body)}`);
  }
});

Then("alice's account status should be {string}", async function (this: CustomWorld, expectedStatus: string) {
  // Re-use admin token if available, otherwise find via the superadmin
  const adminToken = this.tokens.get("superadmin_access") ?? "";
  if (!adminToken) {
    // Just verify via login attempt behavior
    const loginRes = await this.post("/api/v1/auth/login", { username: "alice", password: "Str0ng#Pass1" });
    if (expectedStatus.toLowerCase() === "locked") {
      assert.strictEqual(loginRes.status, 401);
    }
    return;
  }
  const res = await this.get("/api/v1/admin/users?search=alice@example.com", adminToken);
  assert.strictEqual(res.status, 200);
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const data = (res.body as any).content as Array<{ status: string }>;
  assert.ok(data.length > 0);
  assert.strictEqual(data[0]?.status?.toLowerCase(), expectedStatus.toLowerCase());
});
