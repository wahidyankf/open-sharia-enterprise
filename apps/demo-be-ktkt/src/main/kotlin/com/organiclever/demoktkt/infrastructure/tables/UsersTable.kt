package com.organiclever.demoktkt.infrastructure.tables

import com.organiclever.demoktkt.domain.Role
import com.organiclever.demoktkt.domain.UserStatus
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
  val failedLoginCount = integer("failed_login_count").default(0)
  val createdAt = timestamp("created_at")
  val updatedAt = timestamp("updated_at")
  override val primaryKey = PrimaryKey(id)
}
