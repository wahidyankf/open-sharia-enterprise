package com.demobektkt.routes

import com.demobektkt.contracts.CreateExpenseRequest as ContractCreateExpenseRequest
import com.demobektkt.domain.DomainError
import com.demobektkt.domain.DomainException
import com.demobektkt.domain.EntryType
import com.demobektkt.domain.validateAmount
import com.demobektkt.domain.validateCurrency
import com.demobektkt.domain.validateUnit
import com.demobektkt.infrastructure.repositories.CreateExpenseRequest
import com.demobektkt.infrastructure.repositories.ExpenseRepository
import com.demobektkt.infrastructure.repositories.UpdateExpenseRequest
import io.ktor.http.HttpStatusCode
import io.ktor.server.auth.jwt.JWTPrincipal
import io.ktor.server.auth.principal
import io.ktor.server.request.receive
import io.ktor.server.response.respond
import io.ktor.server.routing.RoutingCall
import java.math.BigDecimal
import java.time.LocalDate
import java.util.UUID
import org.koin.core.component.KoinComponent
import org.koin.core.component.inject

object ExpenseRoutes : KoinComponent {
  private val expenseRepository: ExpenseRepository by inject()

  private fun requireUserId(call: RoutingCall): UUID {
    val principal =
      call.principal<JWTPrincipal>()
        ?: throw DomainException(DomainError.Unauthorized("Unauthorized"))
    return UUID.fromString(principal.payload.subject)
  }

  /** Convert [kotlinx.datetime.LocalDate] to [java.time.LocalDate]. */
  private fun kotlinx.datetime.LocalDate.toJavaLocalDate(): LocalDate =
    LocalDate.of(year, monthNumber, dayOfMonth)

  /** Map a [ContractCreateExpenseRequest] to validated domain values. */
  private fun ContractCreateExpenseRequest.toDomainRequest(userId: UUID): CreateExpenseRequest {
    val domainCurrency = validateCurrency(currency).getOrThrow()
    val domainAmount = validateAmount(domainCurrency, BigDecimal(amount)).getOrThrow()
    val domainUnit = validateUnit(unit).getOrThrow()
    val domainType =
      when (type) {
        ContractCreateExpenseRequest.Type.income -> EntryType.INCOME
        ContractCreateExpenseRequest.Type.expense -> EntryType.EXPENSE
      }
    return CreateExpenseRequest(
      userId = userId,
      type = domainType,
      amount = domainAmount,
      currency = domainCurrency,
      category = category,
      description = description,
      date = date.toJavaLocalDate(),
      quantity = quantity,
      unit = domainUnit,
    )
  }

  /** Map a [ContractCreateExpenseRequest] to a domain [UpdateExpenseRequest]. */
  private fun ContractCreateExpenseRequest.toUpdateDomainRequest(): UpdateExpenseRequest {
    val domainCurrency = validateCurrency(currency).getOrThrow()
    val domainAmount = validateAmount(domainCurrency, BigDecimal(amount)).getOrThrow()
    val domainUnit = validateUnit(unit).getOrThrow()
    val domainType =
      when (type) {
        ContractCreateExpenseRequest.Type.income -> EntryType.INCOME
        ContractCreateExpenseRequest.Type.expense -> EntryType.EXPENSE
      }
    return UpdateExpenseRequest(
      type = domainType,
      amount = domainAmount,
      currency = domainCurrency,
      category = category,
      description = description,
      date = date.toJavaLocalDate(),
      quantity = quantity,
      unit = domainUnit,
    )
  }

  suspend fun create(call: RoutingCall) {
    val userId = requireUserId(call)
    val dto = call.receive<ContractCreateExpenseRequest>()

    val expense = expenseRepository.create(dto.toDomainRequest(userId))

    call.respond(HttpStatusCode.Created, expense.toContractExpense())
  }

  suspend fun list(call: RoutingCall) {
    val userId = requireUserId(call)
    val rawPage = call.request.queryParameters["page"]?.toIntOrNull() ?: 0
    val page = rawPage + 1
    val pageSize = call.request.queryParameters["pageSize"]?.toIntOrNull() ?: 20

    val result = expenseRepository.findAllByUser(userId, page, pageSize)

    call.respond(result.toContractExpenseListResponse())
  }

  suspend fun summary(call: RoutingCall) {
    val userId = requireUserId(call)
    val summaries = expenseRepository.summaryByUser(userId)

    val summaryMap =
      summaries.associate { s ->
        val scale = if (s.currency.uppercase() == "IDR") 0 else 2
        s.currency to s.total.setScale(scale, java.math.RoundingMode.HALF_UP).toPlainString()
      }

    call.respond(summaryMap)
  }

  suspend fun getById(call: RoutingCall) {
    val userId = requireUserId(call)
    val expenseId =
      call.parameters["id"]?.let { runCatching { UUID.fromString(it) }.getOrNull() }
        ?: throw DomainException(DomainError.NotFound("expense"))

    val expense =
      expenseRepository.findById(expenseId)
        ?: throw DomainException(DomainError.NotFound("expense"))

    if (expense.userId != userId) {
      throw DomainException(DomainError.Forbidden("Access denied"))
    }

    call.respond(expense.toContractExpense())
  }

  suspend fun update(call: RoutingCall) {
    val userId = requireUserId(call)
    val expenseId =
      call.parameters["id"]?.let { runCatching { UUID.fromString(it) }.getOrNull() }
        ?: throw DomainException(DomainError.NotFound("expense"))

    val existing =
      expenseRepository.findById(expenseId)
        ?: throw DomainException(DomainError.NotFound("expense"))

    if (existing.userId != userId) {
      throw DomainException(DomainError.Forbidden("Access denied"))
    }

    val dto = call.receive<ContractCreateExpenseRequest>()

    val expense =
      expenseRepository.update(expenseId, dto.toUpdateDomainRequest())
        ?: throw DomainException(DomainError.NotFound("expense"))

    call.respond(expense.toContractExpense())
  }

  suspend fun delete(call: RoutingCall) {
    val userId = requireUserId(call)
    val expenseId =
      call.parameters["id"]?.let { runCatching { UUID.fromString(it) }.getOrNull() }
        ?: throw DomainException(DomainError.NotFound("expense"))

    val existing =
      expenseRepository.findById(expenseId)
        ?: throw DomainException(DomainError.NotFound("expense"))

    if (existing.userId != userId) {
      throw DomainException(DomainError.Forbidden("Access denied"))
    }

    expenseRepository.delete(expenseId)
    call.respond(HttpStatusCode.NoContent)
  }
}
