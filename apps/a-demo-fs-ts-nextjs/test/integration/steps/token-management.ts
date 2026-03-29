import { Given, When, Then } from "@cucumber/cucumber";
import assert from "node:assert/strict";
import type { CustomWorld } from "../world";
import { setupAdmin } from "./security";

When("alice decodes her access token payload", function (this: CustomWorld) {
  const token = this.tokens.get("alice_access")!;
  const parts = token.split(".");
  this.context.payload = JSON.parse(Buffer.from(parts[1]!, "base64url").toString());
});

Then("the token should contain a non-null {string} claim", function (this: CustomWorld, claim: string) {
  assert.ok((this.context.payload as Record<string, unknown>)[claim] !== undefined);
});

When(/^the client sends GET \/\.well-known\/jwks\.json$/, async function (this: CustomWorld) {
  this.response = await this.dispatch("GET", "/.well-known/jwks.json", null, null);
});

Then(
  "the response body should contain at least one key in the {string} array",
  function (this: CustomWorld, _field: string) {
    const body = this.response!.body as { keys: unknown[] };
    assert.ok(body.keys.length >= 1);
  },
);

Then("alice's access token should be recorded as revoked", async function (this: CustomWorld) {
  const resp = await this.dispatch("GET", "/api/v1/users/me", null, this.getAuth("alice"));
  assert.strictEqual(resp.status, 401);
});

Given("alice has logged out and her access token is blacklisted", async function (this: CustomWorld) {
  await this.dispatch("POST", "/api/v1/auth/logout", null, this.getAuth("alice"));
});

When(/^the client sends GET \/api\/v1\/users\/me with alice's access token$/, async function (this: CustomWorld) {
  this.response = await this.dispatch("GET", "/api/v1/users/me", null, this.getAuth("alice"));
});

Given(
  /^the admin has disabled alice's account via POST \/api\/v1\/admin\/users\/\{alice_id\}\/disable$/,
  async function (this: CustomWorld) {
    if (!this.tokens.has("superadmin_access")) await setupAdmin(this);
    const aliceId = this.userIds.get("alice")!;
    await this.dispatch(
      "POST",
      `/api/v1/admin/users/${aliceId}/disable`,
      { reason: "Policy violation" },
      this.getAuth("superadmin"),
    );
  },
);
