import { eq, and, sql, gte, lte } from "drizzle-orm";
import type { Database } from "@/db/client";
import { expenses } from "@/db/schema";
import type { ExpenseRepository } from "./interfaces";
import type { Expense, PagedResult } from "@/lib/types";

function rowToExpense(row: typeof expenses.$inferSelect): Expense {
  return {
    id: row.id,
    userId: row.userId,
    amount: row.amount,
    currency: row.currency,
    category: row.category,
    description: row.description,
    date: row.date,
    type: row.type,
    quantity: row.quantity,
    unit: row.unit,
    createdAt: row.createdAt,
    updatedAt: row.updatedAt,
  };
}

export function createExpenseRepository(db: Database): ExpenseRepository {
  return {
    async create(data) {
      const [row] = await db
        .insert(expenses)
        .values({
          userId: data.userId,
          amount: data.amount,
          currency: data.currency,
          category: data.category,
          description: data.description,
          date: data.date,
          type: data.type,
          quantity: data.quantity ?? null,
          unit: data.unit ?? null,
        })
        .returning();
      return rowToExpense(row!);
    },

    async findById(id) {
      const [row] = await db.select().from(expenses).where(eq(expenses.id, id));
      return row ? rowToExpense(row) : null;
    },

    async findByIdAndUserId(id, userId) {
      const [row] = await db
        .select()
        .from(expenses)
        .where(and(eq(expenses.id, id), eq(expenses.userId, userId)));
      return row ? rowToExpense(row) : null;
    },

    async update(id, data) {
      const [row] = await db
        .update(expenses)
        .set({
          amount: data.amount,
          currency: data.currency,
          category: data.category,
          description: data.description,
          date: data.date,
          type: data.type,
          quantity: data.quantity ?? null,
          unit: data.unit ?? null,
          updatedAt: new Date(),
        })
        .where(eq(expenses.id, id))
        .returning();
      return rowToExpense(row!);
    },

    async delete(id) {
      await db.delete(expenses).where(eq(expenses.id, id));
    },

    async listByUserId(userId, page, size) {
      const offset = (page - 1) * size;
      const [items, [countRow]] = await Promise.all([
        db
          .select()
          .from(expenses)
          .where(eq(expenses.userId, userId))
          .limit(size)
          .offset(offset)
          .orderBy(expenses.createdAt),
        db
          .select({ count: sql<number>`count(*)::int` })
          .from(expenses)
          .where(eq(expenses.userId, userId)),
      ]);
      return {
        items: items.map(rowToExpense),
        total: countRow?.count ?? 0,
        page,
        size,
      } as PagedResult<Expense>;
    },

    async summaryByUserId(userId) {
      const rows = await db
        .select({
          currency: expenses.currency,
          totalIncome: sql<string>`coalesce(sum(case when ${expenses.type} = 'INCOME' then ${expenses.amount}::decimal else 0 end), 0)::text`,
          totalExpense: sql<string>`coalesce(sum(case when ${expenses.type} = 'EXPENSE' then ${expenses.amount}::decimal else 0 end), 0)::text`,
        })
        .from(expenses)
        .where(eq(expenses.userId, userId))
        .groupBy(expenses.currency);
      return rows;
    },

    async findByUserIdFiltered(userId, from, to, currency) {
      const conditions = [eq(expenses.userId, userId)];
      if (from) conditions.push(gte(expenses.date, from));
      if (to) conditions.push(lte(expenses.date, to));
      if (currency) conditions.push(eq(expenses.currency, currency.toUpperCase()));
      const rows = await db
        .select()
        .from(expenses)
        .where(and(...conditions));
      return rows.map(rowToExpense);
    },

    async deleteAll() {
      await db.delete(expenses);
    },
  };
}
