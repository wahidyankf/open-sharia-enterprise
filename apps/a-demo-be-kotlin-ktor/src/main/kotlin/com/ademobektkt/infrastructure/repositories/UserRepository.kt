package com.ademobektkt.infrastructure.repositories

import com.ademobektkt.domain.Page
import com.ademobektkt.domain.User
import com.ademobektkt.domain.UserStatus
import java.util.UUID

data class CreateUserRequest(
  val username: String,
  val email: String,
  val displayName: String,
  val passwordHash: String,
  val role: com.ademobektkt.domain.Role = com.ademobektkt.domain.Role.USER,
)

data class UpdateUserPatch(
  val displayName: String? = null,
  val passwordHash: String? = null,
  val status: UserStatus? = null,
  val failedLoginAttempts: Int? = null,
  val role: com.ademobektkt.domain.Role? = null,
)

interface UserRepository {
  suspend fun findByUsername(username: String): User?

  suspend fun findById(id: UUID): User?

  suspend fun findByEmail(email: String): User?

  suspend fun create(request: CreateUserRequest): User

  suspend fun update(id: UUID, patch: UpdateUserPatch): User?

  suspend fun incrementFailedLogins(id: UUID): Int

  suspend fun resetFailedLogins(id: UUID)

  suspend fun findAll(page: Int, pageSize: Int, searchFilter: String?): Page<User>
}
