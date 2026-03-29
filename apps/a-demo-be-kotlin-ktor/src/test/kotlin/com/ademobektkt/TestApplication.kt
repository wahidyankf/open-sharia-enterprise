package com.ademobektkt

import com.ademobektkt.auth.JwtService
import com.ademobektkt.auth.PasswordService
import com.ademobektkt.infrastructure.InMemoryAttachmentRepository
import com.ademobektkt.infrastructure.InMemoryExpenseRepository
import com.ademobektkt.infrastructure.InMemoryTokenRepository
import com.ademobektkt.infrastructure.InMemoryUserRepository
import com.ademobektkt.infrastructure.repositories.AttachmentRepository
import com.ademobektkt.infrastructure.repositories.ExpenseRepository
import com.ademobektkt.infrastructure.repositories.TokenRepository
import com.ademobektkt.infrastructure.repositories.UserRepository
import com.ademobektkt.plugins.configureAuth
import com.ademobektkt.plugins.configureRouting
import com.ademobektkt.plugins.configureSerialization
import com.ademobektkt.plugins.configureStatusPages
import io.ktor.server.application.Application
import io.ktor.server.application.install
import io.ktor.server.testing.ApplicationTestBuilder
import io.ktor.server.testing.testApplication
import org.koin.dsl.module
import org.koin.ktor.plugin.Koin
import org.koin.logger.slf4jLogger

const val TEST_JWT_SECRET = "test-secret-key-at-least-32-characters-long"

val testModule = module {
  single<UserRepository> { InMemoryUserRepository() }
  single<TokenRepository> { InMemoryTokenRepository() }
  single<ExpenseRepository> { InMemoryExpenseRepository() }
  single<AttachmentRepository> { InMemoryAttachmentRepository() }
  single { JwtService(TEST_JWT_SECRET) }
  single { PasswordService() }
}

fun Application.testModuleSetup() {
  install(Koin) {
    slf4jLogger()
    modules(testModule)
  }
  configureSerialization()
  configureAuth()
  configureStatusPages()
  configureRouting()
}

fun withTestApp(block: suspend ApplicationTestBuilder.() -> Unit) {
  testApplication {
    application { testModuleSetup() }
    block()
  }
}
