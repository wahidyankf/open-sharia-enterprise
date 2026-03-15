package com.demobektkt.routes

import com.demobektkt.domain.DomainError
import com.demobektkt.domain.DomainException
import com.demobektkt.domain.EntryType
import com.demobektkt.infrastructure.repositories.ExpenseRepository
import io.ktor.server.auth.jwt.JWTPrincipal
import io.ktor.server.auth.principal
import io.ktor.server.response.respond
import io.ktor.server.routing.RoutingCall
import java.math.BigDecimal
import java.math.RoundingMode
import java.time.LocalDate
import java.util.UUID
import kotlinx.serialization.json.buildJsonArray
import kotlinx.serialization.json.buildJsonObject
import kotlinx.serialization.json.put
import org.koin.core.component.KoinComponent
import org.koin.core.component.inject

object ReportRoutes : KoinComponent {
  private val expenseRepository: ExpenseRepository by inject()

  suspend fun pl(call: RoutingCall) {
    val principal =
      call.principal<JWTPrincipal>()
        ?: throw DomainException(DomainError.Unauthorized("Unauthorized"))
    val userId = UUID.fromString(principal.payload.subject)

    val fromStr =
      call.request.queryParameters["startDate"]
        ?: throw DomainException(
          DomainError.ValidationError("startDate", "startDate parameter is required")
        )
    val toStr =
      call.request.queryParameters["endDate"]
        ?: throw DomainException(
          DomainError.ValidationError("endDate", "endDate parameter is required")
        )
    val currency =
      call.request.queryParameters["currency"]
        ?: throw DomainException(
          DomainError.ValidationError("currency", "currency parameter is required")
        )

    val from =
      runCatching { LocalDate.parse(fromStr) }.getOrNull()
        ?: throw DomainException(
          DomainError.ValidationError("startDate", "Invalid date format: $fromStr")
        )
    val to =
      runCatching { LocalDate.parse(toStr) }.getOrNull()
        ?: throw DomainException(
          DomainError.ValidationError("endDate", "Invalid date format: $toStr")
        )

    val entries = expenseRepository.findByUserAndPeriod(userId, from, to, currency)

    val incomeEntries = entries.filter { it.type == EntryType.INCOME }
    val expenseEntries = entries.filter { it.type == EntryType.EXPENSE }

    val scale = if (currency == "IDR") 0 else 2

    val incomeTotal =
      incomeEntries
        .fold(BigDecimal.ZERO) { acc, e -> acc + e.amount }
        .setScale(scale, RoundingMode.HALF_UP)
    val expenseTotal =
      expenseEntries
        .fold(BigDecimal.ZERO) { acc, e -> acc + e.amount }
        .setScale(scale, RoundingMode.HALF_UP)
    val net = (incomeTotal - expenseTotal).setScale(scale, RoundingMode.HALF_UP)

    val incomeBreakdownArray =
      buildJsonArray {
        incomeEntries
          .groupBy { it.category }
          .forEach { (cat, list) ->
            val total =
              list
                .fold(BigDecimal.ZERO) { acc, e -> acc + e.amount }
                .setScale(scale, RoundingMode.HALF_UP)
                .toPlainString()
            add(buildJsonObject {
              put("category", cat)
              put("type", "income")
              put("total", total)
            })
          }
      }

    val expenseBreakdownArray =
      buildJsonArray {
        expenseEntries
          .groupBy { it.category }
          .forEach { (cat, list) ->
            val total =
              list
                .fold(BigDecimal.ZERO) { acc, e -> acc + e.amount }
                .setScale(scale, RoundingMode.HALF_UP)
                .toPlainString()
            add(buildJsonObject {
              put("category", cat)
              put("type", "expense")
              put("total", total)
            })
          }
      }

    val response = buildJsonObject {
      put("currency", currency)
      put("totalIncome", incomeTotal.toPlainString())
      put("totalExpense", expenseTotal.toPlainString())
      put("net", net.toPlainString())
      put("incomeBreakdown", incomeBreakdownArray)
      put("expenseBreakdown", expenseBreakdownArray)
    }

    call.respond(response)
  }
}
