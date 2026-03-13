package com.demobektkt.plugins

import com.demobektkt.auth.JwtService
import com.demobektkt.domain.UserStatus
import com.demobektkt.infrastructure.repositories.TokenRepository
import com.demobektkt.infrastructure.repositories.UserRepository
import io.ktor.http.HttpStatusCode
import io.ktor.server.application.Application
import io.ktor.server.auth.authentication
import io.ktor.server.auth.jwt.JWTPrincipal
import io.ktor.server.auth.jwt.jwt
import io.ktor.server.response.respond
import java.util.UUID
import kotlinx.coroutines.runBlocking
import org.koin.ktor.ext.inject

fun Application.configureAuth() {
  val jwtService: JwtService by inject()
  val tokenRepository: TokenRepository by inject()
  val userRepository: UserRepository by inject()

  authentication {
    jwt("jwt-auth") {
      verifier(jwtService.verifier())
      validate { credential ->
        val jti = credential.payload.getClaim("jti").asString()
        val subjectStr = credential.payload.subject

        if (jti == null || subjectStr == null) return@validate null

        val isRevoked = runBlocking { tokenRepository.isRevoked(jti) }
        if (isRevoked) return@validate null

        val userId = runCatching { UUID.fromString(subjectStr) }.getOrNull() ?: return@validate null
        val user = runBlocking { userRepository.findById(userId) } ?: return@validate null

        if (user.status != UserStatus.ACTIVE) return@validate null

        JWTPrincipal(credential.payload)
      }
      challenge { _, _ ->
        call.respond(HttpStatusCode.Unauthorized, mapOf("message" to "Unauthorized"))
      }
    }
  }
}
