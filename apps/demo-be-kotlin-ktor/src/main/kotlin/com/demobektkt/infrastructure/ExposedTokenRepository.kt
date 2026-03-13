package com.demobektkt.infrastructure

import com.demobektkt.infrastructure.repositories.TokenRecord
import com.demobektkt.infrastructure.repositories.TokenRepository
import com.demobektkt.infrastructure.repositories.TokenType
import com.demobektkt.infrastructure.tables.TokensTable
import java.time.Instant
import java.util.UUID
import kotlinx.coroutines.Dispatchers
import org.jetbrains.exposed.sql.insert
import org.jetbrains.exposed.sql.selectAll
import org.jetbrains.exposed.sql.transactions.experimental.newSuspendedTransaction
import org.jetbrains.exposed.sql.update

class ExposedTokenRepository : TokenRepository {
  private fun rowToRecord(row: org.jetbrains.exposed.sql.ResultRow): TokenRecord =
    TokenRecord(
      jti = row[TokensTable.jti],
      userId = row[TokensTable.userId],
      tokenType = row[TokensTable.tokenType],
      expiresAt = row[TokensTable.expiresAt],
      revokedAt = row[TokensTable.revokedAt],
    )

  override suspend fun revoke(jti: String, userId: UUID, tokenType: TokenType, expiresAt: Instant) {
    newSuspendedTransaction(Dispatchers.IO) {
      val exists = TokensTable.selectAll().where { TokensTable.jti eq jti }.count() > 0
      if (!exists) {
        TokensTable.insert {
          it[TokensTable.jti] = jti
          it[TokensTable.userId] = userId
          it[TokensTable.tokenType] = tokenType
          it[TokensTable.expiresAt] = expiresAt
          it[revokedAt] = Instant.now()
        }
      } else {
        TokensTable.update({ TokensTable.jti eq jti }) { it[revokedAt] = Instant.now() }
      }
    }
  }

  override suspend fun isRevoked(jti: String): Boolean =
    newSuspendedTransaction(Dispatchers.IO) {
      TokensTable.selectAll()
        .where { TokensTable.jti eq jti }
        .map { it[TokensTable.revokedAt] }
        .firstOrNull()
        ?.let { true } ?: false
    }

  override suspend fun revokeAllForUser(userId: UUID) {
    newSuspendedTransaction(Dispatchers.IO) {
      TokensTable.update({ TokensTable.userId eq userId }) { it[revokedAt] = Instant.now() }
    }
  }

  override suspend fun findByJti(jti: String): TokenRecord? =
    newSuspendedTransaction(Dispatchers.IO) {
      TokensTable.selectAll()
        .where { TokensTable.jti eq jti }
        .map { rowToRecord(it) }
        .singleOrNull()
    }
}
