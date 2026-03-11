package com.organiclever.demoktkt.infrastructure.repositories

import com.organiclever.demoktkt.domain.EntryType
import com.organiclever.demoktkt.domain.Expense
import com.organiclever.demoktkt.domain.Page
import java.math.BigDecimal
import java.time.LocalDate
import java.util.UUID

data class CreateExpenseRequest(
  val userId: UUID,
  val type: EntryType,
  val amount: BigDecimal,
  val currency: String,
  val category: String,
  val description: String,
  val date: LocalDate,
  val quantity: BigDecimal?,
  val unit: String?,
)

data class UpdateExpenseRequest(
  val type: EntryType,
  val amount: BigDecimal,
  val currency: String,
  val category: String,
  val description: String,
  val date: LocalDate,
  val quantity: BigDecimal?,
  val unit: String?,
)

data class CurrencySummary(val currency: String, val total: BigDecimal)

interface ExpenseRepository {
  suspend fun create(request: CreateExpenseRequest): Expense

  suspend fun findById(id: UUID): Expense?

  suspend fun findAllByUser(userId: UUID, page: Int, pageSize: Int): Page<Expense>

  suspend fun update(id: UUID, request: UpdateExpenseRequest): Expense?

  suspend fun delete(id: UUID): Boolean

  suspend fun summaryByUser(userId: UUID): List<CurrencySummary>

  suspend fun findByUserAndPeriod(
    userId: UUID,
    from: LocalDate,
    to: LocalDate,
    currency: String,
  ): List<Expense>
}
