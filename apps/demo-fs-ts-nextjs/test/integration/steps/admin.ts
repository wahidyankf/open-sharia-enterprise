import { Given, When, Then } from "@cucumber/cucumber";
import assert from "node:assert/strict";
import type { CustomWorld } from "../world";

Given(
  "users {string}, {string}, and {string} are registered",
  async function (this: CustomWorld, a: string, b: string, c: string) {
    await this.registerUser(a, `${a}@example.com`, "Str0ng#Pass1");
    await this.registerUser(b, `${b}@example.com`, "Str0ng#Pass2");
    await this.registerUser(c, `${c}@example.com`, "Str0ng#Pass3");
  },
);

Given("alice's account has been disabled by the admin", async function (this: CustomWorld) {
  const aliceId = this.userIds.get("alice")!;
  await this.dispatch(
    "POST",
    `/api/v1/admin/users/${aliceId}/disable`,
    { reason: "Policy violation" },
    this.getAuth("superadmin"),
  );
});

Given("alice's account has been disabled", async function (this: CustomWorld) {
  const aliceId = this.userIds.get("alice")!;
  await this.dispatch(
    "POST",
    `/api/v1/admin/users/${aliceId}/disable`,
    { reason: "Policy violation" },
    this.getAuth("superadmin"),
  );
});

When(/^the admin sends GET \/api\/v1\/admin\/users$/, async function (this: CustomWorld) {
  this.response = await this.dispatch("GET", "/api/v1/admin/users", null, this.getAuth("superadmin"));
});

When(/^the admin sends GET \/api\/v1\/admin\/users\?search=(.+)$/, async function (this: CustomWorld, search: string) {
  this.response = await this.dispatch("GET", `/api/v1/admin/users?search=${search}`, null, this.getAuth("superadmin"));
});

When(
  /^the admin sends POST \/api\/v1\/admin\/users\/\{alice_id\}\/disable with body (.+)$/,
  async function (this: CustomWorld, bodyStr: string) {
    const aliceId = this.userIds.get("alice")!;
    this.response = await this.dispatch(
      "POST",
      `/api/v1/admin/users/${aliceId}/disable`,
      JSON.parse(bodyStr),
      this.getAuth("superadmin"),
    );
  },
);

When(/^the admin sends POST \/api\/v1\/admin\/users\/\{alice_id\}\/enable$/, async function (this: CustomWorld) {
  const aliceId = this.userIds.get("alice")!;
  this.response = await this.dispatch(
    "POST",
    `/api/v1/admin/users/${aliceId}/enable`,
    null,
    this.getAuth("superadmin"),
  );
});

When(/^the admin sends POST \/api\/v1\/admin\/users\/\{alice_id\}\/unlock$/, async function (this: CustomWorld) {
  const aliceId = this.userIds.get("alice")!;
  this.response = await this.dispatch(
    "POST",
    `/api/v1/admin/users/${aliceId}/unlock`,
    null,
    this.getAuth("superadmin"),
  );
});

When(
  /^the admin sends POST \/api\/v1\/admin\/users\/\{alice_id\}\/force-password-reset$/,
  async function (this: CustomWorld) {
    const aliceId = this.userIds.get("alice")!;
    this.response = await this.dispatch(
      "POST",
      `/api/v1/admin/users/${aliceId}/force-password-reset`,
      null,
      this.getAuth("superadmin"),
    );
  },
);

Then(
  "the response body should contain at least one user with {string} equal to {string}",
  function (this: CustomWorld, field: string, value: string) {
    const body = this.response!.body as { content: Record<string, unknown>[] };
    assert.strictEqual(
      body.content.some((u) => u[field] === value),
      true,
    );
  },
);
