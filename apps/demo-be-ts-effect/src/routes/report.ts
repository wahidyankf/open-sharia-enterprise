import { HttpRouter, HttpServerResponse, HttpServerRequest } from "@effect/platform";
import { Effect } from "effect";
import { ExpenseRepository } from "../infrastructure/db/expense-repo.js";
import { requireAuth } from "../auth/middleware.js";
import { ValidationError } from "../domain/errors.js";
import { CURRENCY_DECIMALS, isSupportedCurrency } from "../domain/types.js";

function formatAmount(amount: number, currency: string): string {
  const upperCurrency = currency.toUpperCase();
  if (!isSupportedCurrency(upperCurrency)) {
    return amount.toFixed(2);
  }
  const decimals = CURRENCY_DECIMALS[upperCurrency];
  return amount.toFixed(decimals);
}

const getPL = HttpServerRequest.HttpServerRequest.pipe(
  Effect.flatMap((req) =>
    Effect.gen(function* () {
      const claims = yield* requireAuth(req);
      const url = new URL(req.url, "http://localhost");
      const from = url.searchParams.get("from") ?? "";
      const to = url.searchParams.get("to") ?? "";
      const currency = (url.searchParams.get("currency") ?? "").toUpperCase();

      if (!from) {
        return yield* Effect.fail(new ValidationError({ field: "from", message: "from date is required" }));
      }
      if (!to) {
        return yield* Effect.fail(new ValidationError({ field: "to", message: "to date is required" }));
      }
      if (!currency) {
        return yield* Effect.fail(new ValidationError({ field: "currency", message: "currency is required" }));
      }

      const expenseRepo = yield* ExpenseRepository;
      const expenses = yield* expenseRepo.findByDateRange(claims.sub, from, to, currency);

      let incomeTotal = 0;
      let expenseTotal = 0;
      const incomeBreakdown: Record<string, number> = {};
      const expenseBreakdown: Record<string, number> = {};

      for (const entry of expenses) {
        if (entry.type === "INCOME") {
          incomeTotal += entry.amount;
          const cat = entry.category || "uncategorized";
          incomeBreakdown[cat] = (incomeBreakdown[cat] ?? 0) + entry.amount;
        } else {
          expenseTotal += entry.amount;
          const cat = entry.category || "uncategorized";
          expenseBreakdown[cat] = (expenseBreakdown[cat] ?? 0) + entry.amount;
        }
      }

      const net = incomeTotal - expenseTotal;

      // Convert breakdown amounts to formatted strings
      const incomeBreakdownFormatted: Record<string, string> = {};
      for (const [cat, amount] of Object.entries(incomeBreakdown)) {
        incomeBreakdownFormatted[cat] = formatAmount(amount, currency);
      }

      const expenseBreakdownFormatted: Record<string, string> = {};
      for (const [cat, amount] of Object.entries(expenseBreakdown)) {
        expenseBreakdownFormatted[cat] = formatAmount(amount, currency);
      }

      return yield* HttpServerResponse.json({
        totalIncome: formatAmount(incomeTotal, currency),
        totalExpense: formatAmount(expenseTotal, currency),
        net: formatAmount(net, currency),
        income_breakdown: incomeBreakdownFormatted,
        expense_breakdown: expenseBreakdownFormatted,
        currency,
        from,
        to,
      });
    }),
  ),
);

export const reportRouter = HttpRouter.empty.pipe(HttpRouter.get("/api/v1/reports/pl", getPL));
