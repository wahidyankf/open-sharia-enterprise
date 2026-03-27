package com.demobektkt.infrastructure.repositories

import com.demobektkt.domain.Attachment
import java.util.UUID

data class CreateAttachmentRequest(
  val expenseId: UUID,
  val filename: String,
  val contentType: String,
  val size: Long,
  val data: ByteArray,
) {
  override fun equals(other: Any?): Boolean {
    if (this === other) return true
    if (other !is CreateAttachmentRequest) return false
    return expenseId == other.expenseId &&
      filename == other.filename &&
      contentType == other.contentType &&
      size == other.size &&
      data.contentEquals(other.data)
  }

  override fun hashCode(): Int {
    var result = expenseId.hashCode()
    result = 31 * result + filename.hashCode()
    result = 31 * result + contentType.hashCode()
    result = 31 * result + size.hashCode()
    result = 31 * result + data.contentHashCode()
    return result
  }
}

interface AttachmentRepository {
  suspend fun create(request: CreateAttachmentRequest): Attachment

  suspend fun findById(id: UUID): Attachment?

  suspend fun findAllByExpense(expenseId: UUID): List<Attachment>

  suspend fun delete(id: UUID): Boolean
}
