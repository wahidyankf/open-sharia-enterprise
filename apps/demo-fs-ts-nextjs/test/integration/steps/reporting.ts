import { When, Then } from "@cucumber/cucumber";
import assert from "node:assert/strict";
import type { CustomWorld } from "../world";

When(/^alice sends GET \/api\/v1\/reports\/pl\?(.+)$/, async function (this: CustomWorld, queryString: string) {
  this.response = await this.dispatch("GET", `/api/v1/reports/pl?${queryString}`, null, this.getAuth("alice"));
});

Then(
  "the income breakdown should contain {string} with amount {string}",
  function (this: CustomWorld, category: string, amount: string) {
    const body = this.response!.body as { incomeBreakdown: { category: string; total: string }[] };
    const entry = body.incomeBreakdown.find((b) => b.category === category);
    assert.ok(entry !== undefined);
    assert.strictEqual(entry!.total, amount);
  },
);

Then(
  "the expense breakdown should contain {string} with amount {string}",
  function (this: CustomWorld, category: string, amount: string) {
    const body = this.response!.body as { expenseBreakdown: { category: string; total: string }[] };
    const entry = body.expenseBreakdown.find((b) => b.category === category);
    assert.ok(entry !== undefined);
    assert.strictEqual(entry!.total, amount);
  },
);
