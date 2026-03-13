package com.demobektkt.infrastructure.tables

import com.demobektkt.infrastructure.repositories.TokenType
import org.jetbrains.exposed.sql.Table
import org.jetbrains.exposed.sql.javatime.timestamp

object TokensTable : Table("tokens") {
  val jti = varchar("jti", 255)
  val userId = uuid("user_id")
  val tokenType = enumerationByName("token_type", 10, TokenType::class)
  val expiresAt = timestamp("expires_at")
  val revokedAt = timestamp("revoked_at").nullable()
  override val primaryKey = PrimaryKey(jti)
}
