package com.demobektkt.routes

import com.demobektkt.auth.JWT_ISSUER
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
    val response = buildJsonObject {
      put("sub", payload.subject)
      put("iss", payload.issuer)
      put("jti", payload.getClaim("jti").asString())
      put("username", payload.getClaim("username").asString())
      put("role", payload.getClaim("role").asString())
      payload.expiresAt?.time?.let { put("exp", it) }
      payload.issuedAt?.time?.let { put("iat", it) }
    }

    call.respond(response)
  }

  suspend fun jwks(call: RoutingCall) {
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
