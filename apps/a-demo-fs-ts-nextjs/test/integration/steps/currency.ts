import { Then } from "@cucumber/cucumber";
import assert from "node:assert/strict";
import type { CustomWorld } from "../world";

Then(
  "the response body should contain {string} total equal to {string}",
  function (this: CustomWorld, currency: string, total: string) {
    const body = this.response!.body as { currency: string; totalExpense: string }[];
    const entry = body.find((s) => s.currency === currency);
    assert.ok(entry !== undefined);
    const decimals = currency === "IDR" ? 0 : 2;
    assert.strictEqual(parseFloat(entry!.totalExpense).toFixed(decimals), total);
  },
);
