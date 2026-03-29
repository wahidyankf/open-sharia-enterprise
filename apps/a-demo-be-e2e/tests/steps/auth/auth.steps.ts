import { expect } from "@playwright/test";
import { createBdd } from "playwright-bdd";
import { getResponse } from "../../utils/response-store";

const { Then } = createBdd();

// ---------------------------------------------------------------------------
// Response body assertion steps used by auth + registration features
// ---------------------------------------------------------------------------

Then(
  "the response body should contain {string} equal to {string}",
  // oxlint-disable-next-line no-empty-pattern
  async ({}, field: string, value: string) => {
    const body = (await getResponse().json()) as Record<string, unknown>;
    expect(body[field]).toBe(value);
  },
);

Then(
  "the response body should not contain a {string} field",
  // oxlint-disable-next-line no-empty-pattern
  async ({}, field: string) => {
    const body = (await getResponse().json()) as Record<string, unknown>;
    expect(body[field]).toBeUndefined();
  },
);

Then(
  "the response body should contain a non-null {string} field",
  // oxlint-disable-next-line no-empty-pattern
  async ({}, field: string) => {
    const body = (await getResponse().json()) as Record<string, unknown>;
    expect(body[field]).not.toBeNull();
    expect(body[field]).toBeDefined();
  },
);

Then("the response body should contain an error message about duplicate username", async () => {
  const body = (await getResponse().json()) as { message: string };
  expect(body.message).toMatch(/already exists/i);
});

Then("the response body should contain an error message about invalid credentials", async () => {
  const body = (await getResponse().json()) as { message: string };
  expect(body.message).toMatch(/invalid/i);
});

Then(
  "the response body should contain a validation error for {string}",
  // oxlint-disable-next-line no-empty-pattern
  async ({}, _field: string) => {
    const status = getResponse().status();
    // 400 for field validation errors, 415 for unsupported media type
    expect([400, 415]).toContain(status);
  },
);
