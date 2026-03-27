package com.demobektkt.infrastructure.tables

import org.jetbrains.exposed.sql.Table
import org.jetbrains.exposed.sql.javatime.timestamp

object RefreshTokensTable : Table("refresh_tokens") {
  val id = uuid("id").autoGenerate()
  val userId = uuid("user_id")
  val tokenHash = varchar("token_hash", 255).uniqueIndex()
  val expiresAt = timestamp("expires_at")
  val revoked = bool("revoked").default(false)
  val createdAt = timestamp("created_at")
  override val primaryKey = PrimaryKey(id)
}
