package com.organiclever.demoktkt.infrastructure

import com.organiclever.demoktkt.domain.Attachment
import com.organiclever.demoktkt.infrastructure.repositories.AttachmentRepository
import com.organiclever.demoktkt.infrastructure.repositories.CreateAttachmentRequest
import java.time.Instant
import java.util.UUID
import java.util.concurrent.ConcurrentHashMap

class InMemoryAttachmentRepository : AttachmentRepository {
  private val store = ConcurrentHashMap<UUID, Attachment>()

  override suspend fun create(request: CreateAttachmentRequest): Attachment {
    val attachment =
      Attachment(
        id = UUID.randomUUID(),
        expenseId = request.expenseId,
        userId = request.userId,
        filename = request.filename,
        contentType = request.contentType,
        sizeBytes = request.sizeBytes,
        storedPath = request.storedPath,
        createdAt = Instant.now(),
      )
    store[attachment.id] = attachment
    return attachment
  }

  override suspend fun findById(id: UUID): Attachment? = store[id]

  override suspend fun findAllByExpense(expenseId: UUID): List<Attachment> =
    store.values.filter { it.expenseId == expenseId }

  override suspend fun delete(id: UUID): Boolean = store.remove(id) != null

  fun clear() {
    store.clear()
  }
}
