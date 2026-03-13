import { Given, Then } from "@cucumber/cucumber";
import { expect } from "@playwright/test";
import type { CustomWorld } from "../world.js";

Given("the API is running", function (this: CustomWorld) {
  // Integration tests call service functions directly — no HTTP server is needed.
  // This step is a no-op: the service runtime is initialised in the BeforeAll hook.
});

Then("the response status code should be {int}", function (this: CustomWorld, statusCode: number) {
  expect(this.response).not.toBeNull();
  expect(this.response?.status).toBe(statusCode);
});
