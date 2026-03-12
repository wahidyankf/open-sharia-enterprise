package com.organiclever.demoktkt.unit

import com.organiclever.demoktkt.auth.PasswordService
import org.junit.jupiter.api.Assertions.assertFalse
import org.junit.jupiter.api.Assertions.assertNotEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test

class PasswordServiceTest {
  private val service = PasswordService()

  @Test
  fun `hash produces non-empty string`() {
    val hash = service.hash("Str0ng#Pass1")
    assertTrue(hash.isNotEmpty())
  }

  @Test
  fun `hash is different from plaintext`() {
    val password = "Str0ng#Pass1"
    val hash = service.hash(password)
    assertNotEquals(password, hash)
  }

  @Test
  fun `verify returns true for correct password`() {
    val password = "Str0ng#Pass1"
    val hash = service.hash(password)
    assertTrue(service.verify(password, hash))
  }

  @Test
  fun `verify returns false for wrong password`() {
    val hash = service.hash("Str0ng#Pass1")
    assertFalse(service.verify("WrongPass#1234", hash))
  }

  @Test
  fun `two hashes of same password are different (salt)`() {
    val password = "Str0ng#Pass1"
    val hash1 = service.hash(password)
    val hash2 = service.hash(password)
    assertNotEquals(hash1, hash2)
  }

  @Test
  fun `verify returns false for malformed hash`() {
    // BCrypt.checkpw throws an exception for malformed hashes; getOrDefault covers this
    assertFalse(service.verify("anyPassword", "not-a-valid-bcrypt-hash"))
  }
}
