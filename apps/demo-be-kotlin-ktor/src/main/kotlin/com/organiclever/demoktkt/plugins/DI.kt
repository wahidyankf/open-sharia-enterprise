package com.organiclever.demoktkt.plugins

import com.organiclever.demoktkt.auth.JwtService
import com.organiclever.demoktkt.auth.PasswordService
import com.organiclever.demoktkt.infrastructure.ExposedAttachmentRepository
import com.organiclever.demoktkt.infrastructure.ExposedExpenseRepository
import com.organiclever.demoktkt.infrastructure.ExposedTokenRepository
import com.organiclever.demoktkt.infrastructure.ExposedUserRepository
import com.organiclever.demoktkt.infrastructure.repositories.AttachmentRepository
import com.organiclever.demoktkt.infrastructure.repositories.ExpenseRepository
import com.organiclever.demoktkt.infrastructure.repositories.TokenRepository
import com.organiclever.demoktkt.infrastructure.repositories.UserRepository
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
