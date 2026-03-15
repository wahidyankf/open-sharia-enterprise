import { When, Then } from "@cucumber/cucumber";
import assert from "node:assert/strict";
import type { CustomWorld } from "../world.js";

When(/^alice sends GET (\/api\/v1\/reports\/.+)$/, async function (this: CustomWorld, path: string) {
  const token = this.tokens.get("alice_access") ?? "";
  this.response = await this.get(path, token);
});

Then(
  /^the income breakdown should contain "([^"]*)" with amount "([^"]*)"$/,
  function (this: CustomWorld, category: string, amount: string) {
    assert.ok(this.response !== null);
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const body = this.response?.body as any;
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const breakdown = body?.incomeBreakdown as Array<any> | undefined;
    assert.ok(breakdown !== undefined);
    const entry = breakdown?.find((item: { category: string }) => item.category === category);
    assert.ok(entry !== undefined, `Category '${category}' not found in incomeBreakdown`);
    assert.strictEqual(String(entry.total), amount);
  },
);

Then(
  /^the expense breakdown should contain "([^"]*)" with amount "([^"]*)"$/,
  function (this: CustomWorld, category: string, amount: string) {
    assert.ok(this.response !== null);
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const body = this.response?.body as any;
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const breakdown = body?.expenseBreakdown as Array<any> | undefined;
    assert.ok(breakdown !== undefined);
    const entry = breakdown?.find((item: { category: string }) => item.category === category);
    assert.ok(entry !== undefined, `Category '${category}' not found in expenseBreakdown`);
    assert.strictEqual(String(entry.total), amount);
  },
);
