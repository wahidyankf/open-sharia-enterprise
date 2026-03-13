package com.demobektkt.infrastructure

import com.demobektkt.infrastructure.repositories.TokenRecord
import com.demobektkt.infrastructure.repositories.TokenRepository
import com.demobektkt.infrastructure.repositories.TokenType
import java.time.Instant
import java.util.UUID
import java.util.concurrent.ConcurrentHashMap

class InMemoryTokenRepository : TokenRepository {
  private val store = ConcurrentHashMap<String, TokenRecord>()

  override suspend fun revoke(jti: String, userId: UUID, tokenType: TokenType, expiresAt: Instant) {
    store[jti] =
      TokenRecord(
        jti = jti,
        userId = userId,
        tokenType = tokenType,
        expiresAt = expiresAt,
        revokedAt = Instant.now(),
      )
  }

  override suspend fun isRevoked(jti: String): Boolean = store.containsKey(jti)

  override suspend fun revokeAllForUser(userId: UUID) {
    val now = Instant.now()
    store.values
      .filter { it.userId == userId && it.revokedAt == null }
      .forEach { record -> store[record.jti] = record.copy(revokedAt = now) }
  }

  override suspend fun findByJti(jti: String): TokenRecord? = store[jti]

  fun clear() {
    store.clear()
  }
}
