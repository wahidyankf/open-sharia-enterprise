package com.demobektkt.integration.steps

import com.demobektkt.auth.JwtService
import com.demobektkt.auth.PasswordService
import com.demobektkt.domain.Role
import com.demobektkt.infrastructure.DatabaseFactory
import com.demobektkt.infrastructure.ExposedAttachmentRepository
import com.demobektkt.infrastructure.ExposedExpenseRepository
import com.demobektkt.infrastructure.ExposedTokenRepository
import com.demobektkt.infrastructure.ExposedUserRepository
import com.demobektkt.infrastructure.repositories.AttachmentRepository
import com.demobektkt.infrastructure.repositories.CreateUserRequest
import com.demobektkt.infrastructure.repositories.ExpenseRepository
import com.demobektkt.infrastructure.repositories.TokenRepository
import com.demobektkt.infrastructure.repositories.UserRepository
import com.demobektkt.infrastructure.tables.AttachmentsTable
import com.demobektkt.infrastructure.tables.ExpensesTable
import com.demobektkt.infrastructure.tables.RefreshTokensTable
import com.demobektkt.infrastructure.tables.RevokedTokensTable
import com.demobektkt.infrastructure.tables.UsersTable
import java.util.UUID
import java.util.concurrent.ConcurrentHashMap
import org.jetbrains.exposed.sql.deleteAll
import org.jetbrains.exposed.sql.transactions.transaction

/** JWT secret read from environment or a fixed fallback for local runs. */
val WORLD_JWT_SECRET: String =
  System.getenv("JWT_SECRET") ?: "integration-test-secret-key-at-least-32-characters"

/** Shared mutable test state for Cucumber step definitions. */
object TestWorld {
  var lastResponseStatus: Int = 0
  var lastResponseBody: String = ""
  val accessTokens: ConcurrentHashMap<String, String> = ConcurrentHashMap()
  val refreshTokens: ConcurrentHashMap<String, String> = ConcurrentHashMap()
  val userIds: ConcurrentHashMap<String, String> = ConcurrentHashMap()
  val expenseIds: ConcurrentHashMap<String, String> = ConcurrentHashMap()
  val attachmentIds: ConcurrentHashMap<String, String> = ConcurrentHashMap()
  var jwtService: JwtService = JwtService(WORLD_JWT_SECRET)
  val passwordService: PasswordService = PasswordService()

  // Real PostgreSQL repositories wired once per test suite
  val userRepo: UserRepository = ExposedUserRepository()
  val tokenRepo: TokenRepository = ExposedTokenRepository()
  val expenseRepo: ExpenseRepository = ExposedExpenseRepository()
  val attachmentRepo: AttachmentRepository = ExposedAttachmentRepository()

  /** Clear in-scenario state (tokens, IDs) and truncate all DB tables. */
  fun reset() {
    lastResponseStatus = 0
    lastResponseBody = ""
    accessTokens.clear()
    refreshTokens.clear()
    userIds.clear()
    expenseIds.clear()
    attachmentIds.clear()
    jwtService = JwtService(WORLD_JWT_SECRET)
    truncateTables()
  }

  private fun truncateTables() {
    transaction {
      AttachmentsTable.deleteAll()
      ExpensesTable.deleteAll()
      RevokedTokensTable.deleteAll()
      RefreshTokensTable.deleteAll()
      UsersTable.deleteAll()
    }
  }

  /** Create an admin user directly in the DB (bypasses route-level validation). */
  suspend fun createAdminUser(username: String, email: String, passwordHash: String): UUID {
    val user =
      userRepo.create(
        CreateUserRequest(
          username = username,
          email = email,
          displayName = username,
          passwordHash = passwordHash,
          role = Role.ADMIN,
        )
      )
    // Exposed repositories leave users as ACTIVE from create, but we need to ensure
    // Role.ADMIN is stored. The UserRepository.create stores the role from the request.
    return user.id
  }
}

/** Initialises the PostgreSQL connection once per JVM process. */
object TestDatabase {
  private var initialized = false

  fun init() {
    if (initialized) return
    val url =
      System.getenv("DATABASE_URL")
        ?: "jdbc:postgresql://localhost:5432/demo_be_kotlin_ktor_test?user=demo_be_kotlin_ktor&password=demo_be_kotlin_ktor"

    // Parse user/password from URL query string if embedded (JDBC style with params)
    val user = extractParam(url, "user") ?: System.getenv("DATABASE_USER") ?: "demo_be_kotlin_ktor"
    val password =
      extractParam(url, "password") ?: System.getenv("DATABASE_PASSWORD") ?: "demo_be_kotlin_ktor"

    // Strip query parameters from URL for Exposed (it accepts them separately)
    val jdbcUrl = url.substringBefore("?")

    DatabaseFactory.init(jdbcUrl, user, password)
    initialized = true
  }

  private fun extractParam(url: String, key: String): String? {
    val query = url.substringAfter("?", "")
    if (query.isEmpty()) return null
    return query.split("&").firstOrNull { it.startsWith("$key=") }?.substringAfter("=")
  }
}
