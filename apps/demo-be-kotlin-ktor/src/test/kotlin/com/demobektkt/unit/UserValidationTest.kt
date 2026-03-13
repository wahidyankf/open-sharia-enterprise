package com.demobektkt.unit

import com.demobektkt.domain.DomainException
import com.demobektkt.domain.validateDisplayName
import com.demobektkt.domain.validateEmail
import com.demobektkt.domain.validatePassword
import com.demobektkt.domain.validateUsername
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test

class UserValidationTest {

  @Test
  fun `valid password succeeds`() {
    val result = validatePassword("Str0ng#Pass1")
    assertTrue(result.isSuccess)
  }

  @Test
  fun `empty password fails`() {
    val result = validatePassword("")
    assertTrue(result.isFailure)
    val ex = result.exceptionOrNull() as DomainException
    assertTrue(ex.domainError.toString().contains("password"))
  }

  @Test
  fun `password shorter than 12 chars fails`() {
    val result = validatePassword("Short1!")
    assertTrue(result.isFailure)
  }

  @Test
  fun `password without uppercase fails`() {
    val result = validatePassword("str0ng#pass1!")
    assertTrue(result.isFailure)
  }

  @Test
  fun `password without special char fails`() {
    val result = validatePassword("AllUpperCase1234")
    assertTrue(result.isFailure)
  }

  @Test
  fun `password without lowercase fails`() {
    val result = validatePassword("STR0NG#PASS1!")
    assertTrue(result.isFailure)
  }

  @Test
  fun `password without digit fails`() {
    val result = validatePassword("Str#Password!")
    assertTrue(result.isFailure)
  }

  @Test
  fun `valid email succeeds`() {
    val result = validateEmail("alice@example.com")
    assertTrue(result.isSuccess)
  }

  @Test
  fun `invalid email fails`() {
    val result = validateEmail("not-an-email")
    assertTrue(result.isFailure)
  }

  @Test
  fun `empty email fails`() {
    val result = validateEmail("")
    assertTrue(result.isFailure)
  }

  @Test
  fun `valid username succeeds`() {
    val result = validateUsername("alice")
    assertTrue(result.isSuccess)
  }

  @Test
  fun `empty username fails`() {
    val result = validateUsername("")
    assertTrue(result.isFailure)
  }

  @Test
  fun `username too short fails`() {
    val result = validateUsername("ab")
    assertTrue(result.isFailure)
  }

  @Test
  fun `valid display name succeeds`() {
    val result = validateDisplayName("Alice Smith")
    assertTrue(result.isSuccess)
  }

  @Test
  fun `empty display name fails`() {
    val result = validateDisplayName("")
    assertTrue(result.isFailure)
    val ex = result.exceptionOrNull() as DomainException
    assertTrue(ex.domainError.toString().contains("display_name"))
  }

  @Test
  fun `display name over 100 chars fails`() {
    val longName = "A".repeat(101)
    val result = validateDisplayName(longName)
    assertTrue(result.isFailure)
  }
}
