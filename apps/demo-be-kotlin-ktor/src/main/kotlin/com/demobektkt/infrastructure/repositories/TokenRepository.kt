package com.demobektkt.infrastructure.repositories

import java.time.Instant
import java.util.UUID

enum class TokenType {
  ACCESS,
  REFRESH,
}

data class TokenRecord(
  val jti: String,
  val userId: UUID,
  val tokenType: TokenType,
  val expiresAt: Instant,
  val revokedAt: Instant?,
)

interface TokenRepository {
  suspend fun revoke(jti: String, userId: UUID, tokenType: TokenType, expiresAt: Instant)

  suspend fun isRevoked(jti: String): Boolean

  suspend fun revokeAllForUser(userId: UUID)

  suspend fun findByJti(jti: String): TokenRecord?
}
