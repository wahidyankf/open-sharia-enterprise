package com.organiclever.demoktkt.integration.steps

import com.organiclever.demoktkt.auth.JwtService
import com.organiclever.demoktkt.auth.PasswordService
import com.organiclever.demoktkt.infrastructure.InMemoryAttachmentRepository
import com.organiclever.demoktkt.infrastructure.InMemoryExpenseRepository
import com.organiclever.demoktkt.infrastructure.InMemoryTokenRepository
import com.organiclever.demoktkt.infrastructure.InMemoryUserRepository
import com.organiclever.demoktkt.infrastructure.repositories.AttachmentRepository
import com.organiclever.demoktkt.infrastructure.repositories.ExpenseRepository
import com.organiclever.demoktkt.infrastructure.repositories.TokenRepository
import com.organiclever.demoktkt.infrastructure.repositories.UserRepository
import com.organiclever.demoktkt.plugins.configureAuth
import com.organiclever.demoktkt.plugins.configureRouting
import com.organiclever.demoktkt.plugins.configureSerialization
import com.organiclever.demoktkt.plugins.configureStatusPages
import io.ktor.server.engine.embeddedServer
import io.ktor.server.netty.Netty
import java.net.ServerSocket
import java.util.concurrent.ConcurrentHashMap
import org.koin.core.context.startKoin
import org.koin.core.context.stopKoin
import org.koin.dsl.module
import org.koin.logger.slf4jLogger

const val WORLD_JWT_SECRET = "test-secret-key-at-least-32-characters-long"

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
  var testPort: Int = 0
  var serverStarted: Boolean = false

  // In-memory repos that can be cleared between scenarios
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
    jwtService = JwtService(WORLD_JWT_SECRET)
    userRepo.clear()
    tokenRepo.clear()
    expenseRepo.clear()
    attachmentRepo.clear()
  }
}

/** Starts a real Netty server on a random port once per test suite. */
object TestServer {
  private var started = false
  private var port: Int = 0

  fun start() {
    if (started) return

    port = findFreePort()
    TestWorld.testPort = port

    // Stop any existing Koin instance
    runCatching { stopKoin() }

    startKoin {
      slf4jLogger()
      modules(
        module {
          single<UserRepository> { TestWorld.userRepo }
          single<TokenRepository> { TestWorld.tokenRepo }
          single<ExpenseRepository> { TestWorld.expenseRepo }
          single<AttachmentRepository> { TestWorld.attachmentRepo }
          single { TestWorld.jwtService }
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
    TestWorld.serverStarted = true

    // Wait for startup
    Thread.sleep(500)
  }

  private fun findFreePort(): Int {
    ServerSocket(0).use {
      return it.localPort
    }
  }
}
