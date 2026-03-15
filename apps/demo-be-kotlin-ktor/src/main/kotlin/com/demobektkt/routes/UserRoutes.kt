package com.demobektkt.routes

import com.demobektkt.auth.JwtService
import com.demobektkt.auth.PasswordService
import com.demobektkt.domain.DomainError
import com.demobektkt.domain.DomainException
import com.demobektkt.domain.UserStatus
import com.demobektkt.domain.validateDisplayName
import com.demobektkt.infrastructure.repositories.TokenRepository
import com.demobektkt.infrastructure.repositories.TokenType
import com.demobektkt.infrastructure.repositories.UpdateUserPatch
import com.demobektkt.infrastructure.repositories.UserRepository
import io.ktor.http.HttpStatusCode
import io.ktor.server.auth.jwt.JWTPrincipal
import io.ktor.server.auth.principal
import io.ktor.server.request.receive
import io.ktor.server.response.respond
import io.ktor.server.routing.RoutingCall
import java.time.Instant
import java.util.UUID
import kotlinx.serialization.Serializable
import org.koin.core.component.KoinComponent
import org.koin.core.component.inject

@Serializable data class UpdateDisplayNameRequest(val displayName: String)

@Serializable data class ChangePasswordRequest(val oldPassword: String, val newPassword: String)

object UserRoutes : KoinComponent {
  private val userRepository: UserRepository by inject()
  private val tokenRepository: TokenRepository by inject()
  private val passwordService: PasswordService by inject()
  private val jwtService: JwtService by inject()

  suspend fun getProfile(call: RoutingCall) {
    val principal =
      call.principal<JWTPrincipal>()
        ?: throw DomainException(DomainError.Unauthorized("Unauthorized"))
    val userId = UUID.fromString(principal.payload.subject)
    val user =
      userRepository.findById(userId) ?: throw DomainException(DomainError.NotFound("user"))

    call.respond(
      mapOf(
        "id" to user.id.toString(),
        "username" to user.username,
        "email" to user.email,
        "displayName" to user.displayName,
        "role" to user.role.name,
        "status" to user.status.name,
      )
    )
  }

  suspend fun updateDisplayName(call: RoutingCall) {
    val principal =
      call.principal<JWTPrincipal>()
        ?: throw DomainException(DomainError.Unauthorized("Unauthorized"))
    val userId = UUID.fromString(principal.payload.subject)

    val request = call.receive<UpdateDisplayNameRequest>()
    validateDisplayName(request.displayName).getOrThrow()

    val user =
      userRepository.update(userId, UpdateUserPatch(displayName = request.displayName))
        ?: throw DomainException(DomainError.NotFound("user"))

    call.respond(
      mapOf(
        "id" to user.id.toString(),
        "username" to user.username,
        "email" to user.email,
        "displayName" to user.displayName,
      )
    )
  }

  suspend fun changePassword(call: RoutingCall) {
    val principal =
      call.principal<JWTPrincipal>()
        ?: throw DomainException(DomainError.Unauthorized("Unauthorized"))
    val userId = UUID.fromString(principal.payload.subject)

    val request = call.receive<ChangePasswordRequest>()
    val user =
      userRepository.findById(userId) ?: throw DomainException(DomainError.NotFound("user"))

    if (!passwordService.verify(request.oldPassword, user.passwordHash)) {
      throw DomainException(DomainError.Unauthorized("Invalid credentials"))
    }

    val newHash = passwordService.hash(request.newPassword)
    userRepository.update(userId, UpdateUserPatch(passwordHash = newHash))

    call.respond(HttpStatusCode.OK, mapOf("message" to "Password changed"))
  }

  suspend fun deactivate(call: RoutingCall) {
    val principal =
      call.principal<JWTPrincipal>()
        ?: throw DomainException(DomainError.Unauthorized("Unauthorized"))
    val userId = UUID.fromString(principal.payload.subject)
    val jti = principal.payload.getClaim("jti").asString()
    val expiresAt = principal.payload.expiresAt?.toInstant() ?: Instant.now()

    userRepository.update(userId, UpdateUserPatch(status = UserStatus.INACTIVE))
    tokenRepository.revoke(jti, userId, TokenType.ACCESS, expiresAt)
    tokenRepository.revokeAllForUser(userId)

    call.respond(HttpStatusCode.OK, mapOf("message" to "Account deactivated"))
  }
}
