import { expect } from "@playwright/test";
import { createBdd } from "playwright-bdd";
import { setResponse, getResponse } from "../../utils/response-store";
import { getTokenForUser } from "../../utils/token-store";

const { When, Then } = createBdd();

// ---------------------------------------------------------------------------
// Currency handling steps
// ---------------------------------------------------------------------------

When(/^alice sends GET \/api\/v1\/expenses\/summary$/, async ({ request }) => {
  const token = getTokenForUser("alice");
  setResponse(
    await request.get("/api/v1/expenses/summary", {
      headers: { Authorization: `Bearer ${token}` },
    }),
  );
});

Then(
  "the response body should contain {string} total equal to {string}",
  // oxlint-disable-next-line no-empty-pattern
  async ({}, currency: string, total: string) => {
    const body = (await getResponse().json()) as Record<string, unknown>;
    // Summary endpoint returns flat object: { "USD": "30.00", "IDR": "150000" }
    expect(body[currency]).toBe(total);
  },
);
