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
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const breakdown = body?.incomeBreakdown as Array<any> | undefined;
    expect(breakdown).toBeDefined();
    const entry = breakdown?.find((item: { category: string }) => item.category === category);
    expect(entry).toBeDefined();
    expect(String(entry?.total)).toBe(amount);
  },
);

Then(
  /^the expense breakdown should contain "([^"]*)" with amount "([^"]*)"$/,
  function (this: CustomWorld, category: string, amount: string) {
    expect(this.response).not.toBeNull();
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const body = this.response?.body as any;
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const breakdown = body?.expenseBreakdown as Array<any> | undefined;
    expect(breakdown).toBeDefined();
    const entry = breakdown?.find((item: { category: string }) => item.category === category);
    expect(entry).toBeDefined();
    expect(String(entry?.total)).toBe(amount);
  },
);
