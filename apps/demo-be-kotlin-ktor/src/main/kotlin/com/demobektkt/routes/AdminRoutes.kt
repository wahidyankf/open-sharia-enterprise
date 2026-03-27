package com.demobektkt.routes

import com.demobektkt.domain.DomainError
import com.demobektkt.domain.DomainException
import com.demobektkt.domain.Role
import com.demobektkt.domain.UserStatus
import com.demobektkt.infrastructure.repositories.TokenRepository
import com.demobektkt.infrastructure.repositories.UpdateUserPatch
import com.demobektkt.infrastructure.repositories.UserRepository
import io.ktor.server.auth.jwt.JWTPrincipal
import io.ktor.server.auth.principal
import io.ktor.server.response.respond
import io.ktor.server.routing.RoutingCall
import java.util.UUID
import org.koin.core.component.KoinComponent
import org.koin.core.component.inject

object AdminRoutes : KoinComponent {
  private val userRepository: UserRepository by inject()
  private val tokenRepository: TokenRepository by inject()

  private suspend fun requireAdmin(call: RoutingCall): UUID {
    val principal =
      call.principal<JWTPrincipal>()
        ?: throw DomainException(DomainError.Unauthorized("Unauthorized"))
    val role = principal.payload.getClaim("role").asString()
    if (role != Role.ADMIN.name) {
      throw DomainException(DomainError.Forbidden("Admin access required"))
    }
    return UUID.fromString(principal.payload.subject)
  }

  suspend fun listUsers(call: RoutingCall) {
    requireAdmin(call)
    val rawPage = call.request.queryParameters["page"]?.toIntOrNull() ?: 0
    val page = rawPage + 1
    val pageSize = call.request.queryParameters["pageSize"]?.toIntOrNull() ?: 20
    val searchFilter =
      call.request.queryParameters["search"] ?: call.request.queryParameters["email"]

    val result = userRepository.findAll(page, pageSize, searchFilter)

    call.respond(result.toContractUserListResponse())
  }

  suspend fun disable(call: RoutingCall) {
    requireAdmin(call)
    val userId =
      call.parameters["id"]?.let { runCatching { UUID.fromString(it) }.getOrNull() }
        ?: throw DomainException(DomainError.NotFound("user"))

    val user =
      userRepository.update(userId, UpdateUserPatch(status = UserStatus.DISABLED))
        ?: throw DomainException(DomainError.NotFound("user"))

    tokenRepository.revokeAllForUser(userId)

    call.respond(mapOf("id" to user.id.toString(), "status" to user.status.name))
  }

  suspend fun enable(call: RoutingCall) {
    requireAdmin(call)
    val userId =
      call.parameters["id"]?.let { runCatching { UUID.fromString(it) }.getOrNull() }
        ?: throw DomainException(DomainError.NotFound("user"))

    val user =
      userRepository.update(userId, UpdateUserPatch(status = UserStatus.ACTIVE))
        ?: throw DomainException(DomainError.NotFound("user"))

    call.respond(mapOf("id" to user.id.toString(), "status" to user.status.name))
  }

  suspend fun unlock(call: RoutingCall) {
    requireAdmin(call)
    val userId =
      call.parameters["id"]?.let { runCatching { UUID.fromString(it) }.getOrNull() }
        ?: throw DomainException(DomainError.NotFound("user"))

    val user =
      userRepository.update(
        userId,
        UpdateUserPatch(status = UserStatus.ACTIVE, failedLoginAttempts = 0),
      ) ?: throw DomainException(DomainError.NotFound("user"))

    call.respond(mapOf("id" to user.id.toString(), "status" to user.status.name))
  }

  suspend fun forcePasswordReset(call: RoutingCall) {
    requireAdmin(call)
    val userId =
      call.parameters["id"]?.let { runCatching { UUID.fromString(it) }.getOrNull() }
        ?: throw DomainException(DomainError.NotFound("user"))

    userRepository.findById(userId) ?: throw DomainException(DomainError.NotFound("user"))

    val resetToken = UUID.randomUUID().toString()

    call.respond(buildPasswordResetResponse(resetToken))
  }
}
