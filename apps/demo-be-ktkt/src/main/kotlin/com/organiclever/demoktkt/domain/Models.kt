package com.organiclever.demoktkt.domain

import java.math.BigDecimal
import java.time.Instant
import java.time.LocalDate
import java.util.UUID

sealed class DomainError {
  data class ValidationError(val field: String, val message: String) : DomainError()

  data class NotFound(val entity: String) : DomainError()

  data class Forbidden(val message: String) : DomainError()

  data class Conflict(val message: String) : DomainError()

  data class Unauthorized(val message: String) : DomainError()

  data class FileTooLarge(val limitBytes: Long) : DomainError()

  data class UnsupportedMediaType(val contentType: String) : DomainError()
}

class DomainException(val domainError: DomainError) : RuntimeException(domainError.toString())

enum class Role {
  USER,
  ADMIN,
}

enum class UserStatus {
  ACTIVE,
  INACTIVE,
  DISABLED,
  LOCKED,
}

enum class EntryType {
  EXPENSE,
  INCOME,
}

data class User(
  val id: UUID,
  val username: String,
  val email: String,
  val displayName: String,
  val passwordHash: String,
  val role: Role,
  val status: UserStatus,
  val failedLoginCount: Int,
  val createdAt: Instant,
  val updatedAt: Instant,
)

data class Expense(
  val id: UUID,
  val userId: UUID,
  val type: EntryType,
  val amount: BigDecimal,
  val currency: String,
  val category: String,
  val description: String,
  val date: LocalDate,
  val quantity: BigDecimal?,
  val unit: String?,
  val createdAt: Instant,
  val updatedAt: Instant,
)

data class Attachment(
  val id: UUID,
  val expenseId: UUID,
  val userId: UUID,
  val filename: String,
  val contentType: String,
  val sizeBytes: Long,
  val storedPath: String,
  val createdAt: Instant,
)

data class Page<T>(val data: List<T>, val total: Long, val page: Int, val pageSize: Int)
