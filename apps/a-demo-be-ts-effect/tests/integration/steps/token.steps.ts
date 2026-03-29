import { Given, When, Then } from "@cucumber/cucumber";
import assert from "node:assert/strict";
import type { CustomWorld } from "../world.js";

// ---- Given ----

Given("alice has logged out and her access token is blacklisted", async function (this: CustomWorld) {
  const token = this.tokens.get("alice_access") ?? "";
  const res = await this.post("/api/v1/auth/logout", {}, token);
  if (res.status !== 200) {
    throw new Error(`Failed to logout: ${JSON.stringify(res.body)}`);
  }
});

// Note: "an admin user {string} is registered and logged in" is defined in admin.steps.ts

Given(
  /^the admin has disabled alice's account via POST \/api\/v1\/admin\/users\/\{alice_id\}\/disable$/,
  async function (this: CustomWorld) {
    const adminToken = this.tokens.get("superadmin_access") ?? "";
    const aliceId = this.userIds.get("alice") ?? "";
    const res = await this.post(`/api/v1/admin/users/${aliceId}/disable`, { reason: "Test" }, adminToken);
    if (res.status !== 200) {
      throw new Error(`Failed to disable alice: ${JSON.stringify(res.body)}`);
    }
  },
);

// ---- When ----

When("alice decodes her access token payload", function (this: CustomWorld) {
  const token = this.tokens.get("alice_access") ?? "";
  const parts = token.split(".");
  if (parts.length !== 3) {
    throw new Error(`Invalid JWT: ${token}`);
  }
  const payload = JSON.parse(Buffer.from(parts[1]!, "base64url").toString()) as Record<string, unknown>;
  this.context["tokenPayload"] = payload;
  this.response = { status: 200, body: payload, headers: {} };
});

// Note: "alice sends POST /api/v1/auth/logout with her access token" is defined in token-lifecycle.steps.ts

// ---- Then ----

Then("the token should contain a non-null {string} claim", function (this: CustomWorld, claim: string) {
  const payload = this.context["tokenPayload"] as Record<string, unknown>;
  assert.ok(payload !== undefined);
  assert.ok(payload[claim] !== null);
  assert.ok(payload[claim] !== undefined);
});

Then(
  "the response body should contain at least one key in the {string} array",
  function (this: CustomWorld, field: string) {
    assert.ok(this.response !== null);
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const body = this.response?.body as Record<string, unknown>;
    const arr = body[field] as unknown[];
    assert.ok(Array.isArray(arr));
    assert.ok(arr.length > 0);
  },
);

Then("alice's access token should be recorded as revoked", async function (this: CustomWorld) {
  const token = this.tokens.get("alice_access") ?? "";
  const res = await this.get("/api/v1/users/me", token);
  assert.strictEqual(res.status, 401);
});
