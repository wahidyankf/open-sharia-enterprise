package com.demobektkt.infrastructure.tables

import com.demobektkt.domain.EntryType
import org.jetbrains.exposed.sql.Table
import org.jetbrains.exposed.sql.javatime.date
import org.jetbrains.exposed.sql.javatime.timestamp

object ExpensesTable : Table("expenses") {
  val id = uuid("id").autoGenerate()
  val userId = uuid("user_id")
  val type = enumerationByName("type", 20, EntryType::class)
  val amount = decimal("amount", precision = 19, scale = 4)
  val currency = varchar("currency", 10)
  val category = varchar("category", 100)
  val description = varchar("description", 500)
  val date = date("date")
  val quantity = decimal("quantity", precision = 19, scale = 4).nullable()
  val unit = varchar("unit", 50).nullable()
  val createdAt = timestamp("created_at")
  val createdBy = varchar("created_by", 255).default("system")
  val updatedAt = timestamp("updated_at")
  val updatedBy = varchar("updated_by", 255).default("system")
  val deletedAt = timestamp("deleted_at").nullable()
  val deletedBy = varchar("deleted_by", 255).nullable()
  override val primaryKey = PrimaryKey(id)
}
