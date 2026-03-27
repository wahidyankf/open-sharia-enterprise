package com.demobektkt.infrastructure.tables

import com.demobektkt.domain.Role
import com.demobektkt.domain.UserStatus
import org.jetbrains.exposed.sql.Table
import org.jetbrains.exposed.sql.javatime.timestamp

object UsersTable : Table("users") {
  val id = uuid("id").autoGenerate()
  val username = varchar("username", 50).uniqueIndex()
  val email = varchar("email", 255).uniqueIndex()
  val displayName = varchar("display_name", 100)
  val passwordHash = varchar("password_hash", 255)
  val role = enumerationByName("role", 10, Role::class)
  val status = enumerationByName("status", 10, UserStatus::class)
  val failedLoginAttempts = integer("failed_login_attempts").default(0)
  val createdAt = timestamp("created_at")
  val createdBy = varchar("created_by", 255).default("system")
  val updatedAt = timestamp("updated_at")
  val updatedBy = varchar("updated_by", 255).default("system")
  val deletedAt = timestamp("deleted_at").nullable()
  val deletedBy = varchar("deleted_by", 255).nullable()
  val passwordResetToken = varchar("password_reset_token", 255).nullable()
  override val primaryKey = PrimaryKey(id)
}
