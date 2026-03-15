package com.demobektkt.routes

import com.demobektkt.domain.DomainError
import com.demobektkt.domain.DomainException
import com.demobektkt.domain.EntryType
import com.demobektkt.domain.Expense
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
import java.math.RoundingMode
import java.time.LocalDate
import java.util.UUID
import kotlinx.serialization.Serializable
import kotlinx.serialization.json.buildJsonObject
import kotlinx.serialization.json.put
import kotlinx.serialization.json.putJsonArray
import org.koin.core.component.KoinComponent
import org.koin.core.component.inject

@Serializable
data class CreateExpenseDto(
  val amount: String,
  val currency: String,
  val category: String,
  val description: String,
  val date: String,
  val type: String,
  val quantity: Double? = null,
  val unit: String? = null,
)

private fun formatAmount(currency: String, amount: BigDecimal): String {
  val scale = if (currency.uppercase() == "IDR") 0 else 2
  return amount.setScale(scale, RoundingMode.HALF_UP).toPlainString()
}

private fun Expense.toJsonObject() = buildJsonObject {
  put("id", id.toString())
  put("user_id", userId.toString())
  put("type", type.name.lowercase())
  put("amount", formatAmount(currency, amount))
  put("currency", currency)
  put("category", category)
  put("description", description)
  put("date", date.toString())
  quantity?.let { put("quantity", it) }
  unit?.let { put("unit", it) }
  put("created_at", createdAt.toString())
  put("updated_at", updatedAt.toString())
}

object ExpenseRoutes : KoinComponent {
  private val expenseRepository: ExpenseRepository by inject()

  private fun requireUserId(call: RoutingCall): UUID {
    val principal =
      call.principal<JWTPrincipal>()
        ?: throw DomainException(DomainError.Unauthorized("Unauthorized"))
    return UUID.fromString(principal.payload.subject)
  }

  suspend fun create(call: RoutingCall) {
    val userId = requireUserId(call)
    val dto = call.receive<CreateExpenseDto>()

    val currency = validateCurrency(dto.currency).getOrThrow()
    val amount = validateAmount(currency, BigDecimal(dto.amount)).getOrThrow()
    val unit = validateUnit(dto.unit).getOrThrow()

    val type =
      runCatching { EntryType.valueOf(dto.type.uppercase()) }.getOrNull()
        ?: throw DomainException(DomainError.ValidationError("type", "Invalid type: ${dto.type}"))

    val date =
      runCatching { LocalDate.parse(dto.date) }.getOrNull()
        ?: throw DomainException(
          DomainError.ValidationError("date", "Invalid date format: ${dto.date}")
        )

    val expense =
      expenseRepository.create(
        CreateExpenseRequest(
          userId = userId,
          type = type,
          amount = amount,
          currency = currency,
          category = dto.category,
          description = dto.description,
          date = date,
          quantity = dto.quantity?.let { BigDecimal(it.toString()) },
          unit = unit,
        )
      )

    call.respond(HttpStatusCode.Created, expense.toJsonObject())
  }

  suspend fun list(call: RoutingCall) {
    val userId = requireUserId(call)
    val page = call.request.queryParameters["page"]?.toIntOrNull() ?: 1
    val pageSize = call.request.queryParameters["pageSize"]?.toIntOrNull() ?: 20

    val result = expenseRepository.findAllByUser(userId, page, pageSize)

    val response = buildJsonObject {
      putJsonArray("content") { result.data.forEach { add(it.toJsonObject()) } }
      put("totalElements", result.total)
      put("page", result.page)
      put("pageSize", result.pageSize)
    }

    call.respond(response)
  }

  suspend fun summary(call: RoutingCall) {
    val userId = requireUserId(call)
    val summaries = expenseRepository.summaryByUser(userId)

    val summaryMap = summaries.associate { s -> s.currency to formatAmount(s.currency, s.total) }

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

    call.respond(expense.toJsonObject())
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

    val dto = call.receive<CreateExpenseDto>()

    val currency = validateCurrency(dto.currency).getOrThrow()
    val amount = validateAmount(currency, BigDecimal(dto.amount)).getOrThrow()
    val unit = validateUnit(dto.unit).getOrThrow()

    val type =
      runCatching { EntryType.valueOf(dto.type.uppercase()) }.getOrNull()
        ?: throw DomainException(DomainError.ValidationError("type", "Invalid type: ${dto.type}"))

    val date =
      runCatching { LocalDate.parse(dto.date) }.getOrNull()
        ?: throw DomainException(
          DomainError.ValidationError("date", "Invalid date format: ${dto.date}")
        )

    val expense =
      expenseRepository.update(
        expenseId,
        UpdateExpenseRequest(
          type = type,
          amount = amount,
          currency = currency,
          category = dto.category,
          description = dto.description,
          date = date,
          quantity = dto.quantity?.let { BigDecimal(it.toString()) },
          unit = unit,
        ),
      ) ?: throw DomainException(DomainError.NotFound("expense"))

    call.respond(expense.toJsonObject())
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
