package com.demobektkt.infrastructure

import com.demobektkt.domain.Expense
import com.demobektkt.domain.Page
import com.demobektkt.infrastructure.repositories.CreateExpenseRequest
import com.demobektkt.infrastructure.repositories.CurrencySummary
import com.demobektkt.infrastructure.repositories.ExpenseRepository
import com.demobektkt.infrastructure.repositories.UpdateExpenseRequest
import java.time.Instant
import java.time.LocalDate
import java.util.UUID
import java.util.concurrent.ConcurrentHashMap

class InMemoryExpenseRepository : ExpenseRepository {
  private val store = ConcurrentHashMap<UUID, Expense>()

  override suspend fun create(request: CreateExpenseRequest): Expense {
    val now = Instant.now()
    val expense =
      Expense(
        id = UUID.randomUUID(),
        userId = request.userId,
        type = request.type,
        amount = request.amount,
        currency = request.currency,
        category = request.category,
        description = request.description,
        date = request.date,
        quantity = request.quantity,
        unit = request.unit,
        createdAt = now,
        updatedAt = now,
      )
    store[expense.id] = expense
    return expense
  }

  override suspend fun findById(id: UUID): Expense? = store[id]

  override suspend fun findAllByUser(userId: UUID, page: Int, pageSize: Int): Page<Expense> {
    val items = store.values.filter { it.userId == userId }.sortedByDescending { it.createdAt }
    val total = items.size.toLong()
    val offset = (page - 1) * pageSize
    val pageItems = items.drop(offset).take(pageSize)
    return Page(data = pageItems, total = total, page = page, pageSize = pageSize)
  }

  override suspend fun update(id: UUID, request: UpdateExpenseRequest): Expense? {
    val expense = store[id] ?: return null
    val updated =
      expense.copy(
        type = request.type,
        amount = request.amount,
        currency = request.currency,
        category = request.category,
        description = request.description,
        date = request.date,
        quantity = request.quantity,
        unit = request.unit,
        updatedAt = Instant.now(),
      )
    store[id] = updated
    return updated
  }

  override suspend fun delete(id: UUID): Boolean = store.remove(id) != null

  override suspend fun summaryByUser(userId: UUID): List<CurrencySummary> {
    return store.values
      .filter { it.userId == userId }
      .groupBy { it.currency }
      .map { (currency, expenses) ->
        CurrencySummary(
          currency = currency,
          total = expenses.fold(java.math.BigDecimal.ZERO) { acc, e -> acc + e.amount },
        )
      }
  }

  override suspend fun findByUserAndPeriod(
    userId: UUID,
    from: LocalDate,
    to: LocalDate,
    currency: String,
  ): List<Expense> {
    return store.values.filter { expense ->
      expense.userId == userId &&
        expense.currency == currency &&
        !expense.date.isBefore(from) &&
        !expense.date.isAfter(to)
    }
  }

  fun clear() {
    store.clear()
  }
}
