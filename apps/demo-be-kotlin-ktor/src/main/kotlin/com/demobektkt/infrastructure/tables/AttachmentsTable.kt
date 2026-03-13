package com.demobektkt.infrastructure.tables

import org.jetbrains.exposed.sql.Table
import org.jetbrains.exposed.sql.javatime.timestamp

object AttachmentsTable : Table("attachments") {
  val id = uuid("id").autoGenerate()
  val expenseId = uuid("expense_id")
  val userId = uuid("user_id")
  val filename = varchar("filename", 255)
  val contentType = varchar("content_type", 100)
  val sizeBytes = long("size_bytes")
  val storedPath = varchar("stored_path", 500)
  val createdAt = timestamp("created_at")
  override val primaryKey = PrimaryKey(id)
}
