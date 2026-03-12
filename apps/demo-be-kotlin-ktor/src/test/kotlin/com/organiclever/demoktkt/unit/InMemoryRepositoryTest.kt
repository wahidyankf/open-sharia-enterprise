package com.organiclever.demoktkt.unit

import com.organiclever.demoktkt.auth.PasswordService
import com.organiclever.demoktkt.domain.Role
import com.organiclever.demoktkt.infrastructure.InMemoryUserRepository
import com.organiclever.demoktkt.infrastructure.repositories.CreateUserRequest
import kotlinx.coroutines.runBlocking
import org.junit.jupiter.api.Assertions.assertNotNull
import org.junit.jupiter.api.Assertions.assertNull
import org.junit.jupiter.api.Test

/**
 * Unit tests for InMemoryUserRepository methods not exercised by integration tests. These cover
 * interface methods that have no corresponding route in the application.
 */
class InMemoryRepositoryTest {

  private val repo = InMemoryUserRepository()
  private val passwordService = PasswordService()

  @Test
  fun `findByEmail returns user when email exists`() = runBlocking {
    repo.create(
      CreateUserRequest(
        username = "emailuser",
        email = "emailuser@test.com",
        displayName = "Email User",
        passwordHash = passwordService.hash("Str0ng#Pass1"),
        role = Role.USER,
      )
    )
    val found = repo.findByEmail("emailuser@test.com")
    assertNotNull(found)
  }

  @Test
  fun `findByEmail returns null when email does not exist`() = runBlocking {
    val found = repo.findByEmail("nonexistent@test.com")
    assertNull(found)
  }
}
