import { Given, When, Then } from "@cucumber/cucumber";
import assert from "node:assert/strict";
import type { CustomWorld } from "../world.js";
import { promoteToAdmin } from "../hooks.js";

// ---- Background ----

Given("an admin user {string} is registered and logged in", async function (this: CustomWorld, username: string) {
  const email = `${username}@example.com`;
  const password = "Str0ng#Pass1";

  // Register
  const res = await this.post("/api/v1/auth/register", { username, email, password });
  if (res.status === 201) {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    this.userIds.set(username, (res.body as any).id as string);
  } else if (res.status !== 409) {
    throw new Error(`Failed to register admin ${username}: ${JSON.stringify(res.body)}`);
  }

  // Promote to ADMIN via direct DB access
  await promoteToAdmin(username);

  // Login
  const loginRes = await this.post("/api/v1/auth/login", { username, password });
  if (loginRes.status !== 200) {
    throw new Error(`Failed to login admin ${username}: ${JSON.stringify(loginRes.body)}`);
  }
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const tokens = loginRes.body as { accessToken: string; refreshToken: string };
  this.tokens.set(`${username}_access`, tokens.accessToken);
  this.tokens.set(`${username}_refresh`, tokens.refreshToken);
  this.context[`${username}_password`] = password;
});

Given(
  "users {string}, {string}, and {string} are registered",
  async function (this: CustomWorld, u1: string, u2: string, u3: string) {
    for (const username of [u1, u2, u3]) {
      const email = `${username}@example.com`;
      const password = "Str0ng#Pass1";
      const res = await this.post("/api/v1/auth/register", { username, email, password });
      if (res.status === 201) {
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        this.userIds.set(username, (res.body as any).id as string);
      } else if (res.status === 409) {
        // Already registered — get ID via admin list
        const adminToken = this.tokens.get("superadmin_access") ?? "";
        const listRes = await this.get(`/api/v1/admin/users?search=${email}`, adminToken);
        if (listRes.status === 200) {
          // eslint-disable-next-line @typescript-eslint/no-explicit-any
          const data = (listRes.body as any).content as Array<{ id: string; username: string }>;
          const user = data.find((u) => u.username === username);
          if (user) {
            this.userIds.set(username, user.id);
          }
        }
      } else {
        throw new Error(`Failed to register ${username}: ${JSON.stringify(res.body)}`);
      }
      this.context[`${username}_password`] = password;
    }
  },
);

// ---- Account disabled ----

Given("alice's account has been disabled by the admin", async function (this: CustomWorld) {
  const adminToken = this.tokens.get("superadmin_access") ?? "";
  const aliceId = this.userIds.get("alice") ?? "";
  const res = await this.post(`/api/v1/admin/users/${aliceId}/disable`, { reason: "Test" }, adminToken);
  if (res.status !== 200) {
    throw new Error(`Failed to disable alice: ${JSON.stringify(res.body)}`);
  }
});

Given("alice's account has been disabled", async function (this: CustomWorld) {
  const adminToken = this.tokens.get("superadmin_access") ?? "";
  const aliceId = this.userIds.get("alice") ?? "";
  const res = await this.post(`/api/v1/admin/users/${aliceId}/disable`, { reason: "Test" }, adminToken);
  if (res.status !== 200) {
    throw new Error(`Failed to disable alice: ${JSON.stringify(res.body)}`);
  }
});

// ---- When: admin requests ----

// Generic: the admin sends GET <path>
When(/^the admin sends GET (.+)$/, async function (this: CustomWorld, path: string) {
  const adminToken = this.tokens.get("superadmin_access") ?? "";
  this.response = await this.get(path, adminToken);
});

// Disable alice with reason
When(
  /^the admin sends POST \/api\/v1\/admin\/users\/\{alice_id\}\/disable with body \{ "reason": "([^"]*)" \}$/,
  async function (this: CustomWorld, reason: string) {
    const adminToken = this.tokens.get("superadmin_access") ?? "";
    const aliceId = this.userIds.get("alice") ?? "";
    this.response = await this.post(`/api/v1/admin/users/${aliceId}/disable`, { reason }, adminToken);
  },
);

// Enable alice
When(/^the admin sends POST \/api\/v1\/admin\/users\/\{alice_id\}\/enable$/, async function (this: CustomWorld) {
  const adminToken = this.tokens.get("superadmin_access") ?? "";
  const aliceId = this.userIds.get("alice") ?? "";
  this.response = await this.post(`/api/v1/admin/users/${aliceId}/enable`, {}, adminToken);
});

// Force password reset for alice
When(
  /^the admin sends POST \/api\/v1\/admin\/users\/\{alice_id\}\/force-password-reset$/,
  async function (this: CustomWorld) {
    const adminToken = this.tokens.get("superadmin_access") ?? "";
    const aliceId = this.userIds.get("alice") ?? "";
    this.response = await this.post(`/api/v1/admin/users/${aliceId}/force-password-reset`, {}, adminToken);
  },
);

// Unlock alice
When(/^the admin sends POST \/api\/v1\/admin\/users\/\{alice_id\}\/unlock$/, async function (this: CustomWorld) {
  const adminToken = this.tokens.get("superadmin_access") ?? "";
  const aliceId = this.userIds.get("alice") ?? "";
  this.response = await this.post(`/api/v1/admin/users/${aliceId}/unlock`, {}, adminToken);
});

// ---- Then: admin assertions ----

Then(
  /^the response body should contain at least one user with "([^"]*)" equal to "([^"]*)"$/,
  function (this: CustomWorld, field: string, value: string) {
    assert.ok(this.response !== null);
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const data = (this.response?.body as any)?.content as Array<Record<string, string>>;
    assert.ok(Array.isArray(data));
    const found = data.some((item) => String(item[field]) === value);
    assert.ok(found);
  },
);

// Note: "alice's account status should be {string}" is defined in security.steps.ts
// Note: "the response body should contain {string} equal to {string}" is defined in auth.steps.ts
