package com.demobektkt.plugins

import com.demobektkt.auth.JwtService
import com.demobektkt.auth.PasswordService
import com.demobektkt.infrastructure.ExposedAttachmentRepository
import com.demobektkt.infrastructure.ExposedExpenseRepository
import com.demobektkt.infrastructure.ExposedTokenRepository
import com.demobektkt.infrastructure.ExposedUserRepository
import com.demobektkt.infrastructure.repositories.AttachmentRepository
import com.demobektkt.infrastructure.repositories.ExpenseRepository
import com.demobektkt.infrastructure.repositories.TokenRepository
import com.demobektkt.infrastructure.repositories.UserRepository
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
