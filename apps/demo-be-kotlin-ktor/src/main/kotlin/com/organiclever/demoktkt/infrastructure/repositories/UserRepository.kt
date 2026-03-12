package com.organiclever.demoktkt.infrastructure.repositories

import com.organiclever.demoktkt.domain.Page
import com.organiclever.demoktkt.domain.User
import com.organiclever.demoktkt.domain.UserStatus
import java.util.UUID

data class CreateUserRequest(
  val username: String,
  val email: String,
  val displayName: String,
  val passwordHash: String,
  val role: com.organiclever.demoktkt.domain.Role = com.organiclever.demoktkt.domain.Role.USER,
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
