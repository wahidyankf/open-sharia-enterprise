package com.ademobektkt.routes

import com.ademobektkt.domain.DomainError
import com.ademobektkt.domain.DomainException
import com.ademobektkt.domain.Role
import com.ademobektkt.infrastructure.repositories.UpdateUserPatch
import com.ademobektkt.infrastructure.repositories.UserRepository
import com.ademobektkt.infrastructure.tables.AttachmentsTable
import com.ademobektkt.infrastructure.tables.ExpensesTable
import com.ademobektkt.infrastructure.tables.RefreshTokensTable
import com.ademobektkt.infrastructure.tables.RevokedTokensTable
import com.ademobektkt.infrastructure.tables.UsersTable
import io.ktor.http.HttpStatusCode
import io.ktor.server.request.receive
import io.ktor.server.response.respond
import io.ktor.server.routing.RoutingCall
import kotlinx.coroutines.Dispatchers
import kotlinx.serialization.Serializable
import org.jetbrains.exposed.sql.deleteAll
import org.jetbrains.exposed.sql.transactions.experimental.newSuspendedTransaction
import org.koin.core.component.KoinComponent
import org.koin.core.component.inject

@Serializable data class PromoteAdminRequest(val username: String)

object TestRoutes : KoinComponent {
  private val userRepository: UserRepository by inject()

  suspend fun resetDb(call: RoutingCall) {
    newSuspendedTransaction(Dispatchers.IO) {
      AttachmentsTable.deleteAll()
      ExpensesTable.deleteAll()
      RevokedTokensTable.deleteAll()
      RefreshTokensTable.deleteAll()
      UsersTable.deleteAll()
    }
    call.respond(HttpStatusCode.OK, mapOf("message" to "Database reset successful"))
  }

  suspend fun promoteAdmin(call: RoutingCall) {
    val request = call.receive<PromoteAdminRequest>()
    val user =
      userRepository.findByUsername(request.username)
        ?: throw DomainException(DomainError.NotFound("user"))

    userRepository.update(user.id, UpdateUserPatch(role = Role.ADMIN))

    call.respond(
      HttpStatusCode.OK,
      mapOf("message" to "User ${request.username} promoted to ADMIN"),
    )
  }
}
