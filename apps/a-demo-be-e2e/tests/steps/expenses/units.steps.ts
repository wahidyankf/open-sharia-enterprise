import { expect } from "@playwright/test";
import { createBdd } from "playwright-bdd";
import { getResponse } from "../../utils/response-store";

const { Then } = createBdd();

// ---------------------------------------------------------------------------
// Unit handling steps
// ---------------------------------------------------------------------------

Then(
  "the response body should contain {string} equal to {float}",
  // oxlint-disable-next-line no-empty-pattern
  async ({}, field: string, quantity: number) => {
    const body = (await getResponse().json()) as Record<string, unknown>;
    expect(body[field]).toBe(quantity);
  },
);
