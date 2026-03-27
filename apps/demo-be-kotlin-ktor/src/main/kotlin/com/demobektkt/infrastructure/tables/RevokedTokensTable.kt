package com.demobektkt.infrastructure.tables

import org.jetbrains.exposed.sql.Table
import org.jetbrains.exposed.sql.javatime.timestamp

object RevokedTokensTable : Table("revoked_tokens") {
  val id = uuid("id").autoGenerate()
  val jti = varchar("jti", 255).uniqueIndex()
  val userId = uuid("user_id")
  val revokedAt = timestamp("revoked_at")
  override val primaryKey = PrimaryKey(id)
}
