package com.demobektkt

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
