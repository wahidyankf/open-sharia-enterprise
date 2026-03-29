package com.ademobektkt.plugins

import com.ademobektkt.auth.JwtService
import com.ademobektkt.auth.PasswordService
import com.ademobektkt.infrastructure.ExposedAttachmentRepository
import com.ademobektkt.infrastructure.ExposedExpenseRepository
import com.ademobektkt.infrastructure.ExposedTokenRepository
import com.ademobektkt.infrastructure.ExposedUserRepository
import com.ademobektkt.infrastructure.repositories.AttachmentRepository
import com.ademobektkt.infrastructure.repositories.ExpenseRepository
import com.ademobektkt.infrastructure.repositories.TokenRepository
import com.ademobektkt.infrastructure.repositories.UserRepository
import io.ktor.server.application.Application
import io.ktor.server.application.install
import org.koin.dsl.module
import org.koin.ktor.plugin.Koin
import org.koin.logger.slf4jLogger

fun Application.configureDI(jwtSecret: String) {
  install(Koin) {
    slf4jLogger()
    modules(productionModule(jwtSecret))
  }
}

fun productionModule(jwtSecret: String) = module {
  single<UserRepository> { ExposedUserRepository() }
  single<TokenRepository> { ExposedTokenRepository() }
  single<ExpenseRepository> { ExposedExpenseRepository() }
  single<AttachmentRepository> { ExposedAttachmentRepository() }
  single { JwtService(jwtSecret) }
  single { PasswordService() }
}
