package com.organiclever.demoktkt

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
