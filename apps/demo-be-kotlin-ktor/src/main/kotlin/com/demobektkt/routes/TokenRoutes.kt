package com.demobektkt.routes

import com.demobektkt.auth.JWT_ISSUER
import com.demobektkt.contracts.TokenClaims
import com.demobektkt.domain.DomainError
import com.demobektkt.domain.DomainException
import io.ktor.server.auth.jwt.JWTPrincipal
import io.ktor.server.auth.principal
import io.ktor.server.response.respond
import io.ktor.server.routing.RoutingCall
import kotlinx.serialization.json.buildJsonObject
import kotlinx.serialization.json.put
import kotlinx.serialization.json.putJsonArray
import org.koin.core.component.KoinComponent

object TokenRoutes : KoinComponent {

  suspend fun claims(call: RoutingCall) {
    val principal =
      call.principal<JWTPrincipal>()
        ?: throw DomainException(DomainError.Unauthorized("Unauthorized"))

    val payload = principal.payload
    val expSeconds = payload.expiresAt?.time?.let { (it / 1000).toInt() } ?: 0
    val iatSeconds = payload.issuedAt?.time?.let { (it / 1000).toInt() } ?: 0
    val role = payload.getClaim("role").asString() ?: ""

    val tokenClaims =
      TokenClaims(
        sub = payload.subject,
        iss = payload.issuer,
        exp = expSeconds,
        iat = iatSeconds,
        roles = if (role.isNotEmpty()) listOf(role) else emptyList(),
      )

    call.respond(tokenClaims)
  }

  suspend fun jwks(call: RoutingCall) {
    // JWKS endpoint uses buildJsonObject because contracts.JwkKey is designed for
    // RSA keys (requires n and e fields) but this service uses HS256 (symmetric key).
    val response = buildJsonObject {
      putJsonArray("keys") {
        add(
          buildJsonObject {
            put("kty", "oct")
            put("use", "sig")
            put("alg", "HS256")
            put("kid", "demo-be-kotlin-ktor-key-1")
            put("iss", JWT_ISSUER)
          }
        )
      }
    }

    call.respond(response)
  }
}
