package com.demobektkt.infrastructure.repositories

import com.demobektkt.domain.Page
import com.demobektkt.domain.User
import com.demobektkt.domain.UserStatus
import java.util.UUID

data class CreateUserRequest(
  val username: String,
  val email: String,
  val displayName: String,
  val passwordHash: String,
  val role: com.demobektkt.domain.Role = com.demobektkt.domain.Role.USER,
)

data class UpdateUserPatch(
  val displayName: String? = null,
  val passwordHash: String? = null,
  val status: UserStatus? = null,
  val failedLoginCount: Int? = null,
)

interface UserRepository {
  suspend fun findByUsername(username: String): User?

  suspend fun findById(id: UUID): User?

  suspend fun findByEmail(email: String): User?

  suspend fun create(request: CreateUserRequest): User

  suspend fun update(id: UUID, patch: UpdateUserPatch): User?

  suspend fun incrementFailedLogins(id: UUID): Int

  suspend fun resetFailedLogins(id: UUID)

  suspend fun findAll(page: Int, pageSize: Int, emailFilter: String?): Page<User>
}
