import type { ExpenseRepository } from "@/repositories/interfaces";
import { ok, type ServiceResult, CURRENCY_DECIMALS, type SupportedCurrency } from "@/lib/types";

interface ReportDeps {
  expenses: ExpenseRepository;
}

export interface PLReport {
  totalIncome: string;
  totalExpense: string;
  net: string;
  incomeBreakdown: { category: string; total: string }[];
  expenseBreakdown: { category: string; total: string }[];
}

export async function generatePLReport(
  deps: ReportDeps,
  userId: string,
  startDate?: string,
  endDate?: string,
  currency?: string,
): Promise<ServiceResult<PLReport>> {
  const expenses = await deps.expenses.findByUserIdFiltered(userId, startDate, endDate, currency);
  const decimals = currency ? (CURRENCY_DECIMALS[currency as SupportedCurrency] ?? 2) : 2;

  let totalIncome = 0;
  let totalExpense = 0;
  const incomeByCat = new Map<string, number>();
  const expenseByCat = new Map<string, number>();

  for (const e of expenses) {
    const amount = parseFloat(e.amount);
    if (e.type === "INCOME") {
      totalIncome += amount;
      incomeByCat.set(e.category, (incomeByCat.get(e.category) ?? 0) + amount);
    } else {
      totalExpense += amount;
      expenseByCat.set(e.category, (expenseByCat.get(e.category) ?? 0) + amount);
    }
  }

  return ok({
    totalIncome: totalIncome.toFixed(decimals),
    totalExpense: totalExpense.toFixed(decimals),
    net: (totalIncome - totalExpense).toFixed(decimals),
    incomeBreakdown: [...incomeByCat.entries()].map(([category, total]) => ({
      category,
      total: total.toFixed(decimals),
    })),
    expenseBreakdown: [...expenseByCat.entries()].map(([category, total]) => ({
      category,
      total: total.toFixed(decimals),
    })),
  });
}
