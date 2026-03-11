package com.organiclever.demoktkt.auth

import com.auth0.jwt.JWT
import com.auth0.jwt.JWTVerifier
import com.auth0.jwt.algorithms.Algorithm
import com.auth0.jwt.interfaces.DecodedJWT
import com.organiclever.demoktkt.domain.Role
import java.util.Date
import java.util.UUID

const val JWT_ISSUER = "demo-be-ktkt"
const val ACCESS_TOKEN_EXPIRY_MS = 15 * 60 * 1000L // 15 minutes
const val REFRESH_TOKEN_EXPIRY_MS = 7 * 24 * 60 * 60 * 1000L // 7 days

class JwtService(val secret: String) {
  private val algorithm = Algorithm.HMAC256(secret)

  fun generateAccessToken(userId: UUID, username: String, role: Role): String =
    JWT.create()
      .withIssuer(JWT_ISSUER)
      .withSubject(userId.toString())
      .withClaim("username", username)
      .withClaim("role", role.name)
      .withClaim("jti", UUID.randomUUID().toString())
      .withClaim("type", "access")
      .withIssuedAt(Date())
      .withExpiresAt(Date(System.currentTimeMillis() + ACCESS_TOKEN_EXPIRY_MS))
      .sign(algorithm)

  fun generateRefreshToken(userId: UUID): String =
    JWT.create()
      .withIssuer(JWT_ISSUER)
      .withSubject(userId.toString())
      .withClaim("jti", UUID.randomUUID().toString())
      .withClaim("type", "refresh")
      .withIssuedAt(Date())
      .withExpiresAt(Date(System.currentTimeMillis() + REFRESH_TOKEN_EXPIRY_MS))
      .sign(algorithm)

  fun generateExpiredRefreshToken(userId: UUID): String =
    JWT.create()
      .withIssuer(JWT_ISSUER)
      .withSubject(userId.toString())
      .withClaim("jti", UUID.randomUUID().toString())
      .withClaim("type", "refresh")
      .withIssuedAt(Date(System.currentTimeMillis() - REFRESH_TOKEN_EXPIRY_MS - 1000))
      .withExpiresAt(Date(System.currentTimeMillis() - 1000))
      .sign(algorithm)

  fun verifier(): JWTVerifier = JWT.require(algorithm).withIssuer(JWT_ISSUER).build()

  fun decodeToken(token: String): DecodedJWT? = runCatching { verifier().verify(token) }.getOrNull()

  fun decodeTokenUnchecked(token: String): DecodedJWT? =
    runCatching { JWT.decode(token) }.getOrNull()
}
