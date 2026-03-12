package com.organiclever.demoktkt.routes

import com.organiclever.demoktkt.domain.DomainError
import com.organiclever.demoktkt.domain.DomainException
import com.organiclever.demoktkt.domain.EntryType
import com.organiclever.demoktkt.infrastructure.repositories.ExpenseRepository
import io.ktor.server.auth.jwt.JWTPrincipal
import io.ktor.server.auth.principal
import io.ktor.server.response.respond
import io.ktor.server.routing.RoutingCall
import java.math.BigDecimal
import java.math.RoundingMode
import java.time.LocalDate
import java.util.UUID
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
      call.request.queryParameters["from"]
        ?: throw DomainException(DomainError.ValidationError("from", "from parameter is required"))
    val toStr =
      call.request.queryParameters["to"]
        ?: throw DomainException(DomainError.ValidationError("to", "to parameter is required"))
    val currency =
      call.request.queryParameters["currency"]
        ?: throw DomainException(
          DomainError.ValidationError("currency", "currency parameter is required")
        )

    val from =
      runCatching { LocalDate.parse(fromStr) }.getOrNull()
        ?: throw DomainException(
          DomainError.ValidationError("from", "Invalid date format: $fromStr")
        )
    val to =
      runCatching { LocalDate.parse(toStr) }.getOrNull()
        ?: throw DomainException(DomainError.ValidationError("to", "Invalid date format: $toStr"))

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

    val incomeBreakdown =
      incomeEntries
        .groupBy { it.category }
        .mapValues { (_, list) ->
          list
            .fold(BigDecimal.ZERO) { acc, e -> acc + e.amount }
            .setScale(scale, RoundingMode.HALF_UP)
            .toPlainString()
        }

    val expenseBreakdown =
      expenseEntries
        .groupBy { it.category }
        .mapValues { (_, list) ->
          list
            .fold(BigDecimal.ZERO) { acc, e -> acc + e.amount }
            .setScale(scale, RoundingMode.HALF_UP)
            .toPlainString()
        }

    val response = buildJsonObject {
      put("currency", currency)
      put("from", from.toString())
      put("to", to.toString())
      put("income_total", incomeTotal.toPlainString())
      put("expense_total", expenseTotal.toPlainString())
      put("net", net.toPlainString())
      put(
        "income_breakdown",
        buildJsonObject { incomeBreakdown.forEach { (cat, amt) -> put(cat, amt) } },
      )
      put(
        "expense_breakdown",
        buildJsonObject { expenseBreakdown.forEach { (cat, amt) -> put(cat, amt) } },
      )
    }

    call.respond(response)
  }
}
