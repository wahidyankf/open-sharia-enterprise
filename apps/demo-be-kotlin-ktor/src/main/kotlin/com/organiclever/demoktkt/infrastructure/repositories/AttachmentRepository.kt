package com.organiclever.demoktkt.infrastructure.repositories

import com.organiclever.demoktkt.domain.Attachment
import java.util.UUID

data class CreateAttachmentRequest(
  val expenseId: UUID,
  val userId: UUID,
  val filename: String,
  val contentType: String,
  val sizeBytes: Long,
  val storedPath: String,
)

interface AttachmentRepository {
  suspend fun create(request: CreateAttachmentRequest): Attachment

  suspend fun findById(id: UUID): Attachment?

  suspend fun findAllByExpense(expenseId: UUID): List<Attachment>

  suspend fun delete(id: UUID): Boolean
}
