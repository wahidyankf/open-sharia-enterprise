import { When, Then } from "@cucumber/cucumber";
import assert from "node:assert/strict";
import type { CustomWorld } from "../world";

When(/^an operations engineer sends GET \/health$/, async function (this: CustomWorld) {
  this.response = await this.dispatch("GET", "/health", null, null);
});

When(/^an unauthenticated engineer sends GET \/health$/, async function (this: CustomWorld) {
  this.response = await this.dispatch("GET", "/health", null, null);
});

Then("the health status should be {string}", function (this: CustomWorld, status: string) {
  assert.strictEqual((this.response!.body as Record<string, unknown>).status, status);
});

Then("the response should not include detailed component health information", function (this: CustomWorld) {
  assert.strictEqual((this.response!.body as Record<string, unknown>).components, undefined);
});
