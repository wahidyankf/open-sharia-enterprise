import { expect } from "@playwright/test";
import { createBdd } from "playwright-bdd";
import { setResponse, getResponse } from "../../utils/response-store";
import { getTokenForUser } from "../../utils/token-store";

const { When, Then } = createBdd();

// ---------------------------------------------------------------------------
// Financial reporting (P&L) steps
// ---------------------------------------------------------------------------

When(
  /^alice sends GET \/api\/v1\/reports\/pl\?from=(\d{4}-\d{2}-\d{2})&to=(\d{4}-\d{2}-\d{2})&currency=(\w+)$/,
  async ({ request }, from: string, to: string, currency: string) => {
    const token = getTokenForUser("alice");
    setResponse(
      await request.get(`/api/v1/reports/pl?from=${from}&to=${to}&currency=${currency}`, {
        headers: { Authorization: `Bearer ${token}` },
      }),
    );
  },
);

Then(
  "the income breakdown should contain {string} with amount {string}",
  // oxlint-disable-next-line no-empty-pattern
  async ({}, category: string, amount: string) => {
    const body = (await getResponse().json()) as Record<string, unknown>;
    // income_breakdown is an object: { "salary": "3000.00", "freelance": "500.00" }
    const incomeBreakdown = body["income_breakdown"] as Record<string, string> | undefined;
    expect(incomeBreakdown).toBeDefined();
    expect(incomeBreakdown![category]).toBe(amount);
  },
);

Then(
  "the expense breakdown should contain {string} with amount {string}",
  // oxlint-disable-next-line no-empty-pattern
  async ({}, category: string, amount: string) => {
    const body = (await getResponse().json()) as Record<string, unknown>;
    // expense_breakdown is an object: { "transport": "200.00" }
    const expenseBreakdown = body["expense_breakdown"] as Record<string, string> | undefined;
    expect(expenseBreakdown).toBeDefined();
    expect(expenseBreakdown![category]).toBe(amount);
  },
);
