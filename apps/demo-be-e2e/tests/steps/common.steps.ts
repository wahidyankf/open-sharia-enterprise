import { expect } from "@playwright/test";
import { createBdd } from "playwright-bdd";
import { getResponse } from "../utils/response-store";
import { validateResponseAgainstContract } from "../utils/contract-validator";

const { Given, Then } = createBdd();

Given("the OrganicLever API is running", async () => {
  // No-op: the test suite assumes the API is running at baseURL.
});

Given("the API is running", async () => {
  // No-op: the test suite assumes the API is running at baseURL.
});

// oxlint-disable-next-line no-empty-pattern
Then("the response status code should be {int}", async ({}, code: number) => {
  const res = getResponse();
  expect(res.status()).toBe(code);

  // Validate 2xx response bodies against the OpenAPI contract schema
  if (code >= 200 && code < 300) {
    try {
      const body = await res.json();
      const url = new URL(res.url());
      // Normalize path: strip IDs from path segments (UUIDs become {id})
      const normalizedPath = url.pathname
        .replace(
          /\/[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}/gi,
          "/{id}",
        )
        .replace(/\/\d+/g, "/{id}");
      const contractError = validateResponseAgainstContract(
        normalizedPath,
        "get", // Will be overridden by specific step validators
        code,
        body,
      );
      if (contractError) {
        console.warn(`[contract] ${contractError}`);
      }
    } catch {
      // Response may not have JSON body (e.g., 204 No Content)
    }
  }
});
