package com.demobektkt.routes

import com.demobektkt.domain.DomainError
import com.demobektkt.domain.DomainException
import com.demobektkt.domain.Role
import com.demobektkt.infrastructure.repositories.UpdateUserPatch
import com.demobektkt.infrastructure.repositories.UserRepository
import com.demobektkt.infrastructure.tables.AttachmentsTable
import com.demobektkt.infrastructure.tables.ExpensesTable
import com.demobektkt.infrastructure.tables.TokensTable
import com.demobektkt.infrastructure.tables.UsersTable
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
      TokensTable.deleteAll()
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
