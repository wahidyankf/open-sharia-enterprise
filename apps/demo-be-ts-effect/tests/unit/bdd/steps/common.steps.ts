import { Given, Then } from "@cucumber/cucumber";
import { expect } from "@playwright/test";
import type { CustomWorld } from "../world.js";
import { TEST_PORT } from "../hooks.js";

Given("the API is running", async function (this: CustomWorld) {
  // Server is started in BeforeAll hook
  this.baseUrl = `http://localhost:${TEST_PORT}`;
});

Then("the response status code should be {int}", function (this: CustomWorld, statusCode: number) {
  expect(this.response).not.toBeNull();
  expect(this.response?.status).toBe(statusCode);
});
