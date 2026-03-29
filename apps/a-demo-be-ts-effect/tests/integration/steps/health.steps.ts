import { When, Then } from "@cucumber/cucumber";
import assert from "node:assert/strict";
import type { CustomWorld } from "../world.js";

When("an operations engineer sends GET \\/health", async function (this: CustomWorld) {
  this.response = await this.get("/health");
});

When("an unauthenticated engineer sends GET \\/health", async function (this: CustomWorld) {
  this.response = await this.get("/health");
});

Then("the health status should be {string}", function (this: CustomWorld, status: string) {
  assert.ok(this.response !== null);
  assert.strictEqual(this.response?.body?.status, status);
});

Then("the response should not include detailed component health information", function (this: CustomWorld) {
  assert.ok(this.response !== null);
  const body = this.response?.body;
  // Only status field should be present, no component details
  assert.strictEqual((body as Record<string, unknown>)?.["components"], undefined);
  assert.strictEqual((body as Record<string, unknown>)?.["details"], undefined);
  assert.strictEqual((body as Record<string, unknown>)?.["db"], undefined);
});
