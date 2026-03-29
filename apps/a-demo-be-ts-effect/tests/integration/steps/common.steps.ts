import { Given, Then } from "@cucumber/cucumber";
import assert from "node:assert/strict";
import type { CustomWorld } from "../world.js";

Given("the API is running", function (this: CustomWorld) {
  // Integration tests call service functions directly — no HTTP server is needed.
  // This step is a no-op: the service runtime is initialised in the BeforeAll hook.
});

Then("the response status code should be {int}", function (this: CustomWorld, statusCode: number) {
  assert.ok(this.response !== null);
  assert.strictEqual(this.response?.status, statusCode);
});
