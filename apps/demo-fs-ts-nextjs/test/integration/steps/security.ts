import { Given, Then } from "@cucumber/cucumber";
import assert from "node:assert/strict";
import type { CustomWorld } from "../world";
import { MAX_FAILED_ATTEMPTS } from "../../../src/lib/types";

async function setupAdmin(world: CustomWorld): Promise<void> {
  await world.registerUser("superadmin", "superadmin@example.com", "Str0ng#Pass1");
  await world.dispatch("POST", "/api/v1/test/promote-admin", { username: "superadmin" }, null);
  await world.loginUser("superadmin", "Str0ng#Pass1");
}

Given(
  "{string} has had the maximum number of failed login attempts",
  async function (this: CustomWorld, username: string) {
    for (let i = 0; i < MAX_FAILED_ATTEMPTS; i++) {
      await this.dispatch("POST", "/api/v1/auth/login", { username, password: "WrongPass!123" }, null);
    }
  },
);

Given(
  "a user {string} is registered and locked after too many failed logins",
  async function (this: CustomWorld, username: string) {
    await this.registerUser(username, `${username}@example.com`, "Str0ng#Pass1");
    for (let i = 0; i < MAX_FAILED_ATTEMPTS; i++) {
      await this.dispatch("POST", "/api/v1/auth/login", { username, password: "WrongPass!123" }, null);
    }
  },
);

Given("an admin user {string} is registered and logged in", async function (this: CustomWorld, _username: string) {
  await setupAdmin(this);
});

Given("an admin has unlocked alice's account", async function (this: CustomWorld) {
  await setupAdmin(this);
  const aliceId = this.userIds.get("alice")!;
  await this.dispatch("POST", `/api/v1/admin/users/${aliceId}/unlock`, null, this.getAuth("superadmin"));
});

Then("alice's account status should be {string}", async function (this: CustomWorld, expectedStatus: string) {
  if (!this.tokens.has("superadmin_access")) await setupAdmin(this);
  const resp = await this.dispatch(
    "GET",
    "/api/v1/admin/users?search=alice@example.com",
    null,
    this.getAuth("superadmin"),
  );
  const body = resp.body as { content: { status: string }[] };
  assert.strictEqual(body.content[0]!.status.toLowerCase(), expectedStatus.toLowerCase());
});

export { setupAdmin };
