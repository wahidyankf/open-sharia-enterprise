import type { ExpenseRepository } from "@/repositories/interfaces";
import { ok, type ServiceResult, CURRENCY_DECIMALS, type SupportedCurrency } from "@/lib/types";

interface ReportDeps {
  expenses: ExpenseRepository;
}

export interface PLReport {
  items: PLReportItem[];
}

export interface PLReportItem {
  currency: string;
  totalIncome: string;
  totalExpense: string;
  net: string;
}

export async function generatePLReport(
  deps: ReportDeps,
  userId: string,
  _startDate?: string,
  _endDate?: string,
): Promise<ServiceResult<PLReport>> {
  const summary = await deps.expenses.summaryByUserId(userId);

  const items: PLReportItem[] = summary.map((s) => {
    const income = parseFloat(s.totalIncome);
    const expense = parseFloat(s.totalExpense);
    const net = income - expense;
    const decimals = CURRENCY_DECIMALS[s.currency as SupportedCurrency] ?? 2;
    return {
      currency: s.currency,
      totalIncome: income.toFixed(decimals),
      totalExpense: expense.toFixed(decimals),
      net: net.toFixed(decimals),
    };
  });

  return ok({ items });
}
