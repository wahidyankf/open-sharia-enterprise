package com.demobektkt.infrastructure.tables

import org.jetbrains.exposed.sql.Table
import org.jetbrains.exposed.sql.javatime.timestamp

object AttachmentsTable : Table("attachments") {
  val id = uuid("id").autoGenerate()
  val expenseId = uuid("expense_id")
  val filename = varchar("filename", 255)
  val contentType = varchar("content_type", 100)
  val size = long("size")
  val data = binary("data")
  val createdAt = timestamp("created_at")
  override val primaryKey = PrimaryKey(id)
}
