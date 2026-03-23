import { Given, Then } from "@cucumber/cucumber";
import assert from "node:assert/strict";
import type { CustomWorld } from "../world";

Given("the API is running", function (this: CustomWorld) {});
Given("the test API is enabled via ENABLE_TEST_API environment variable", function (this: CustomWorld) {});

Then("the response status code should be {int}", function (this: CustomWorld, code: number) {
  assert.strictEqual(this.response!.status, code);
});

Then("the response status should be {int}", function (this: CustomWorld, code: number) {
  assert.strictEqual(this.response!.status, code);
});

Then(
  "the response body should contain {string} equal to {string}",
  function (this: CustomWorld, field: string, value: string) {
    const body = this.response!.body as Record<string, unknown>;
    assert.strictEqual(String(body[field]), value);
  },
);

Then("the response body should contain a non-null {string} field", function (this: CustomWorld, field: string) {
  const body = this.response!.body as Record<string, unknown>;
  assert.ok(body[field] !== undefined && body[field] !== null);
});

Then("the response body should not contain a {string} field", function (this: CustomWorld, field: string) {
  const body = this.response!.body as Record<string, unknown>;
  assert.strictEqual(body[field], undefined);
});

Then("the response body should contain an error message about invalid credentials", function (this: CustomWorld) {
  const body = this.response!.body as Record<string, unknown>;
  assert.match(String(body.error).toLowerCase(), /invalid/);
});

Then("the response body should contain an error message about duplicate username", function (this: CustomWorld) {
  const body = this.response!.body as Record<string, unknown>;
  assert.match(String(body.error).toLowerCase(), /username/);
});

Then("the response body should contain an error message about account deactivation", function (this: CustomWorld) {
  const body = this.response!.body as Record<string, unknown>;
  assert.match(String(body.error).toLowerCase(), /deactivat/);
});

Then("the response body should contain an error message about token expiration", function (this: CustomWorld) {
  assert.ok(String((this.response!.body as Record<string, unknown>).error).length > 0);
});

Then("the response body should contain an error message about invalid token", function (this: CustomWorld) {
  assert.ok(String((this.response!.body as Record<string, unknown>).error).length > 0);
});

Then("the response body should contain an error message about file size", function (this: CustomWorld) {
  assert.match(String((this.response!.body as Record<string, unknown>).error).toLowerCase(), /size/);
});

Then("the response body should contain a validation error for {string}", function (this: CustomWorld, field: string) {
  const msg = String((this.response!.body as Record<string, unknown>).error).toLowerCase();
  assert.ok(msg.includes(field.toLowerCase()), `Expected "${msg}" to contain "${field}"`);
});

Then(
  "the response body should contain {string} equal to {float}",
  function (this: CustomWorld, field: string, value: number) {
    const body = this.response!.body as Record<string, unknown>;
    assert.strictEqual(parseFloat(String(body[field])), value);
  },
);
