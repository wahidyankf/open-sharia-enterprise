package com.ademobektkt.unit.steps

import com.ademobektkt.auth.JwtService
import com.ademobektkt.auth.PasswordService
import com.ademobektkt.infrastructure.InMemoryAttachmentRepository
import com.ademobektkt.infrastructure.InMemoryExpenseRepository
import com.ademobektkt.infrastructure.InMemoryTokenRepository
import com.ademobektkt.infrastructure.InMemoryUserRepository
import java.util.concurrent.ConcurrentHashMap

const val UNIT_JWT_SECRET = "test-secret-key-at-least-32-characters-long"

/** Shared mutable test state for unit-level Cucumber step definitions. */
object UnitTestWorld {
  var lastResponseStatus: Int = 0
  var lastResponseBody: String = ""
  val accessTokens: ConcurrentHashMap<String, String> = ConcurrentHashMap()
  val refreshTokens: ConcurrentHashMap<String, String> = ConcurrentHashMap()
  val userIds: ConcurrentHashMap<String, String> = ConcurrentHashMap()
  val expenseIds: ConcurrentHashMap<String, String> = ConcurrentHashMap()
  val attachmentIds: ConcurrentHashMap<String, String> = ConcurrentHashMap()
  var jwtService: JwtService = JwtService(UNIT_JWT_SECRET)
  val passwordService: PasswordService = PasswordService()

  // In-memory repos shared across all step definitions
  val userRepo = InMemoryUserRepository()
  val tokenRepo = InMemoryTokenRepository()
  val expenseRepo = InMemoryExpenseRepository()
  val attachmentRepo = InMemoryAttachmentRepository()

  var testApiEnabled: Boolean = true

  fun reset() {
    lastResponseStatus = 0
    lastResponseBody = ""
    accessTokens.clear()
    refreshTokens.clear()
    userIds.clear()
    expenseIds.clear()
    attachmentIds.clear()
    jwtService = JwtService(UNIT_JWT_SECRET)
    userRepo.clear()
    tokenRepo.clear()
    expenseRepo.clear()
    attachmentRepo.clear()
    testApiEnabled = true
  }
}
