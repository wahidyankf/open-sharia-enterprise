import { When, Then } from "@cucumber/cucumber";
import { expect } from "@playwright/test";
import type { CustomWorld } from "../world.js";

When(/^alice sends GET (\/api\/v1\/reports\/.+)$/, async function (this: CustomWorld, path: string) {
  const token = this.tokens.get("alice_access") ?? "";
  this.response = await this.get(path, token);
});

Then(
  /^the income breakdown should contain "([^"]*)" with amount "([^"]*)"$/,
  function (this: CustomWorld, category: string, amount: string) {
    expect(this.response).not.toBeNull();
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const body = this.response?.body as any;
    const breakdown = body?.income_breakdown as Record<string, string> | undefined;
    expect(breakdown).toBeDefined();
    expect(String(breakdown?.[category])).toBe(amount);
  },
);

Then(
  /^the expense breakdown should contain "([^"]*)" with amount "([^"]*)"$/,
  function (this: CustomWorld, category: string, amount: string) {
    expect(this.response).not.toBeNull();
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const body = this.response?.body as any;
    const breakdown = body?.expense_breakdown as Record<string, string> | undefined;
    expect(breakdown).toBeDefined();
    expect(String(breakdown?.[category])).toBe(amount);
  },
);
