/**
 * Step definitions for the OSE GRC BE health endpoint feature.
 *
 * Covers: specs/apps/ose-grc/behavior/be/gherkin/health.feature
 */
import { expect } from "@playwright/test";
import { createBdd } from "playwright-bdd";
import { setResponse, getResponse, clearResponse } from "../utils/response-store";

const { Given, When, Then, Before } = createBdd();

Before(() => {
  clearResponse();
});

Given("the ose-grc-be service is running", async () => {
  // No-op: the test suite assumes the BE is running at baseURL (http://localhost:8302).
});

When(/^I send GET \/api\/v1\/health$/, async ({ request }) => {
  setResponse(await request.get("/api/v1/health"));
});

// oxlint-disable-next-line no-empty-pattern
Then("the response status is {int}", async ({}, expectedStatus: number) => {
  expect(getResponse().status()).toBe(expectedStatus);
});

Then(
  "the response body has a {string} field equal to {string}",
  // oxlint-disable-next-line no-empty-pattern
  async ({}, field: string, value: string) => {
    const body = (await getResponse().json()) as Record<string, unknown>;
    expect(body[field]).toBe(value);
  },
);
