package com.demobektkt.routes

import com.demobektkt.domain.DomainError
import com.demobektkt.domain.DomainException
import com.demobektkt.domain.EntryType
import com.demobektkt.infrastructure.repositories.ExpenseRepository
import io.ktor.server.auth.jwt.JWTPrincipal
import io.ktor.server.auth.principal
import io.ktor.server.response.respond
import io.ktor.server.routing.RoutingCall
import java.time.LocalDate
import java.util.UUID
import org.koin.core.component.KoinComponent
import org.koin.core.component.inject

object ReportRoutes : KoinComponent {
  private val expenseRepository: ExpenseRepository by inject()

  private fun requireParam(params: io.ktor.http.Parameters, name: String): String =
    params[name]
      ?: throw DomainException(DomainError.ValidationError(name, "$name parameter is required"))

  private fun parseDate(value: String, param: String): LocalDate =
    runCatching { LocalDate.parse(value) }.getOrNull()
      ?: throw DomainException(DomainError.ValidationError(param, "Invalid date format: $value"))

  suspend fun pl(call: RoutingCall) {
    val principal =
      call.principal<JWTPrincipal>()
        ?: throw DomainException(DomainError.Unauthorized("Unauthorized"))
    val userId = UUID.fromString(principal.payload.subject)
    val params = call.request.queryParameters

    val fromStr = requireParam(params, "startDate")
    val toStr = requireParam(params, "endDate")
    val currency = requireParam(params, "currency")
    val from = parseDate(fromStr, "startDate")
    val to = parseDate(toStr, "endDate")

    val entries = expenseRepository.findByUserAndPeriod(userId, from, to, currency)
    val incomeEntries = entries.filter { it.type == EntryType.INCOME }
    val expenseEntries = entries.filter { it.type == EntryType.EXPENSE }

    call.respond(buildPLReport(currency, from, to, incomeEntries, expenseEntries))
  }
}
