package com.organiclever.demoktkt.infrastructure

import com.organiclever.demoktkt.domain.Expense
import com.organiclever.demoktkt.domain.Page
import com.organiclever.demoktkt.infrastructure.repositories.CreateExpenseRequest
import com.organiclever.demoktkt.infrastructure.repositories.CurrencySummary
import com.organiclever.demoktkt.infrastructure.repositories.ExpenseRepository
import com.organiclever.demoktkt.infrastructure.repositories.UpdateExpenseRequest
import com.organiclever.demoktkt.infrastructure.tables.ExpensesTable
import java.math.BigDecimal
import java.time.Instant
import java.time.LocalDate
import java.util.UUID
import kotlinx.coroutines.Dispatchers
import org.jetbrains.exposed.sql.ResultRow
import org.jetbrains.exposed.sql.SortOrder
import org.jetbrains.exposed.sql.SqlExpressionBuilder.eq
import org.jetbrains.exposed.sql.andWhere
import org.jetbrains.exposed.sql.deleteWhere
import org.jetbrains.exposed.sql.insert
import org.jetbrains.exposed.sql.selectAll
import org.jetbrains.exposed.sql.sum
import org.jetbrains.exposed.sql.transactions.experimental.newSuspendedTransaction
import org.jetbrains.exposed.sql.update

class ExposedExpenseRepository : ExpenseRepository {
  private fun rowToExpense(row: ResultRow): Expense =
    Expense(
      id = row[ExpensesTable.id],
      userId = row[ExpensesTable.userId],
      type = row[ExpensesTable.type],
      amount = row[ExpensesTable.amount],
      currency = row[ExpensesTable.currency],
      category = row[ExpensesTable.category],
      description = row[ExpensesTable.description],
      date = row[ExpensesTable.date],
      quantity = row[ExpensesTable.quantity],
      unit = row[ExpensesTable.unit],
      createdAt = row[ExpensesTable.createdAt],
      updatedAt = row[ExpensesTable.updatedAt],
    )

  override suspend fun create(request: CreateExpenseRequest): Expense =
    newSuspendedTransaction(Dispatchers.IO) {
      val now = Instant.now()
      val id =
        ExpensesTable.insert {
            it[userId] = request.userId
            it[type] = request.type
            it[amount] = request.amount
            it[currency] = request.currency
            it[category] = request.category
            it[description] = request.description
            it[date] = request.date
            it[quantity] = request.quantity
            it[unit] = request.unit
            it[createdAt] = now
            it[updatedAt] = now
          }[ExpensesTable.id]
      ExpensesTable.selectAll().where { ExpensesTable.id eq id }.map { rowToExpense(it) }.single()
    }

  override suspend fun findById(id: UUID): Expense? =
    newSuspendedTransaction(Dispatchers.IO) {
      ExpensesTable.selectAll()
        .where { ExpensesTable.id eq id }
        .map { rowToExpense(it) }
        .singleOrNull()
    }

  override suspend fun findAllByUser(userId: UUID, page: Int, pageSize: Int): Page<Expense> =
    newSuspendedTransaction(Dispatchers.IO) {
      val query = ExpensesTable.selectAll().where { ExpensesTable.userId eq userId }
      val total = query.count()
      val items =
        query
          .orderBy(ExpensesTable.createdAt to SortOrder.DESC)
          .limit(pageSize)
          .offset(((page - 1) * pageSize).toLong())
          .map { rowToExpense(it) }
      Page(data = items, total = total, page = page, pageSize = pageSize)
    }

  override suspend fun update(id: UUID, request: UpdateExpenseRequest): Expense? =
    newSuspendedTransaction(Dispatchers.IO) {
      ExpensesTable.update({ ExpensesTable.id eq id }) {
        it[type] = request.type
        it[amount] = request.amount
        it[currency] = request.currency
        it[category] = request.category
        it[description] = request.description
        it[date] = request.date
        it[quantity] = request.quantity
        it[unit] = request.unit
        it[updatedAt] = Instant.now()
      }
      ExpensesTable.selectAll()
        .where { ExpensesTable.id eq id }
        .map { rowToExpense(it) }
        .singleOrNull()
    }

  override suspend fun delete(id: UUID): Boolean =
    newSuspendedTransaction(Dispatchers.IO) {
      val deleted = ExpensesTable.deleteWhere { ExpensesTable.id eq id }
      deleted > 0
    }

  override suspend fun summaryByUser(userId: UUID): List<CurrencySummary> =
    newSuspendedTransaction(Dispatchers.IO) {
      ExpensesTable.selectAll()
        .where { ExpensesTable.userId eq userId }
        .groupBy(ExpensesTable.currency)
        .map { row ->
          CurrencySummary(
            currency = row[ExpensesTable.currency],
            total = row[ExpensesTable.amount.sum()] ?: BigDecimal.ZERO,
          )
        }
    }

  override suspend fun findByUserAndPeriod(
    userId: UUID,
    from: LocalDate,
    to: LocalDate,
    currency: String,
  ): List<Expense> =
    newSuspendedTransaction(Dispatchers.IO) {
      ExpensesTable.selectAll()
        .where { ExpensesTable.userId eq userId }
        .andWhere { ExpensesTable.currency eq currency }
        .andWhere { ExpensesTable.date greaterEq from }
        .andWhere { ExpensesTable.date lessEq to }
        .map { rowToExpense(it) }
    }
}
