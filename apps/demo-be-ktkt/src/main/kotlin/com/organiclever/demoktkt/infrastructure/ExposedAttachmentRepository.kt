package com.organiclever.demoktkt.infrastructure

import com.organiclever.demoktkt.domain.Attachment
import com.organiclever.demoktkt.infrastructure.repositories.AttachmentRepository
import com.organiclever.demoktkt.infrastructure.repositories.CreateAttachmentRequest
import com.organiclever.demoktkt.infrastructure.tables.AttachmentsTable
import java.time.Instant
import java.util.UUID
import kotlinx.coroutines.Dispatchers
import org.jetbrains.exposed.sql.ResultRow
import org.jetbrains.exposed.sql.SqlExpressionBuilder.eq
import org.jetbrains.exposed.sql.deleteWhere
import org.jetbrains.exposed.sql.insert
import org.jetbrains.exposed.sql.selectAll
import org.jetbrains.exposed.sql.transactions.experimental.newSuspendedTransaction

class ExposedAttachmentRepository : AttachmentRepository {
  private fun rowToAttachment(row: ResultRow): Attachment =
    Attachment(
      id = row[AttachmentsTable.id],
      expenseId = row[AttachmentsTable.expenseId],
      userId = row[AttachmentsTable.userId],
      filename = row[AttachmentsTable.filename],
      contentType = row[AttachmentsTable.contentType],
      sizeBytes = row[AttachmentsTable.sizeBytes],
      storedPath = row[AttachmentsTable.storedPath],
      createdAt = row[AttachmentsTable.createdAt],
    )

  override suspend fun create(request: CreateAttachmentRequest): Attachment =
    newSuspendedTransaction(Dispatchers.IO) {
      val id =
        AttachmentsTable.insert {
            it[expenseId] = request.expenseId
            it[userId] = request.userId
            it[filename] = request.filename
            it[contentType] = request.contentType
            it[sizeBytes] = request.sizeBytes
            it[storedPath] = request.storedPath
            it[createdAt] = Instant.now()
          }[AttachmentsTable.id]
      AttachmentsTable.selectAll()
        .where { AttachmentsTable.id eq id }
        .map { rowToAttachment(it) }
        .single()
    }

  override suspend fun findById(id: UUID): Attachment? =
    newSuspendedTransaction(Dispatchers.IO) {
      AttachmentsTable.selectAll()
        .where { AttachmentsTable.id eq id }
        .map { rowToAttachment(it) }
        .singleOrNull()
    }

  override suspend fun findAllByExpense(expenseId: UUID): List<Attachment> =
    newSuspendedTransaction(Dispatchers.IO) {
      AttachmentsTable.selectAll()
        .where { AttachmentsTable.expenseId eq expenseId }
        .map { rowToAttachment(it) }
    }

  override suspend fun delete(id: UUID): Boolean =
    newSuspendedTransaction(Dispatchers.IO) {
      val deleted = AttachmentsTable.deleteWhere { AttachmentsTable.id eq id }
      deleted > 0
    }
}
