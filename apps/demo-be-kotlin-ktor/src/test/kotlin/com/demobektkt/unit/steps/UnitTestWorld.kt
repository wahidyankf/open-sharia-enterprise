package com.demobektkt.unit.steps

import com.demobektkt.auth.JwtService
import com.demobektkt.auth.PasswordService
import com.demobektkt.infrastructure.InMemoryAttachmentRepository
import com.demobektkt.infrastructure.InMemoryExpenseRepository
import com.demobektkt.infrastructure.InMemoryTokenRepository
import com.demobektkt.infrastructure.InMemoryUserRepository
import com.demobektkt.infrastructure.repositories.AttachmentRepository
import com.demobektkt.infrastructure.repositories.ExpenseRepository
import com.demobektkt.infrastructure.repositories.TokenRepository
import com.demobektkt.infrastructure.repositories.UserRepository
import com.demobektkt.plugins.configureAuth
import com.demobektkt.plugins.configureRouting
import com.demobektkt.plugins.configureSerialization
import com.demobektkt.plugins.configureStatusPages
import io.ktor.server.engine.embeddedServer
import io.ktor.server.netty.Netty
import java.net.ServerSocket
import java.util.concurrent.ConcurrentHashMap
import org.koin.core.context.startKoin
import org.koin.core.context.stopKoin
import org.koin.dsl.module
import org.koin.logger.slf4jLogger

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
  var testPort: Int = 0

  // In-memory repos shared across all step definitions
  val userRepo = InMemoryUserRepository()
  val tokenRepo = InMemoryTokenRepository()
  val expenseRepo = InMemoryExpenseRepository()
  val attachmentRepo = InMemoryAttachmentRepository()

  fun baseUrl(): String = "http://localhost:$testPort"

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
  }
}

/** Starts a real Netty server on a random port once per test suite with in-memory repositories. */
object UnitTestServer {
  private var started = false

  fun start() {
    if (started) return

    val port = ServerSocket(0).use { it.localPort }
    UnitTestWorld.testPort = port

    // Stop any existing Koin instance
    runCatching { stopKoin() }

    startKoin {
      slf4jLogger()
      modules(
        module {
          single<UserRepository> { UnitTestWorld.userRepo }
          single<TokenRepository> { UnitTestWorld.tokenRepo }
          single<ExpenseRepository> { UnitTestWorld.expenseRepo }
          single<AttachmentRepository> { UnitTestWorld.attachmentRepo }
          single { UnitTestWorld.jwtService }
          single { PasswordService() }
        }
      )
    }

    val server =
      embeddedServer(Netty, port = port, host = "localhost") {
        configureSerialization()
        configureAuth()
        configureStatusPages()
        configureRouting()
      }
    server.start(wait = false)
    started = true

    // Wait for startup
    Thread.sleep(500)
  }
}
