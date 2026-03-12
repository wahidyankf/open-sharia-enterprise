package com.organiclever.demoktkt

import com.organiclever.demoktkt.infrastructure.DatabaseFactory
import com.organiclever.demoktkt.plugins.configureAuth
import com.organiclever.demoktkt.plugins.configureDI
import com.organiclever.demoktkt.plugins.configureRouting
import com.organiclever.demoktkt.plugins.configureSerialization
import com.organiclever.demoktkt.plugins.configureStatusPages
import io.ktor.server.application.Application
import io.ktor.server.engine.embeddedServer
import io.ktor.server.netty.Netty

fun main() {
  val port = System.getenv("PORT")?.toIntOrNull() ?: 8201
  embeddedServer(Netty, port = port, host = "0.0.0.0", module = Application::module)
    .start(wait = true)
}

fun Application.module() {
  val jdbcUrl = System.getenv("DATABASE_URL") ?: "jdbc:postgresql://localhost:5432/demo_be_ktkt"
  val dbUser = System.getenv("DATABASE_USER") ?: "demo_be_ktkt"
  val dbPassword = System.getenv("DATABASE_PASSWORD") ?: "demo_be_ktkt"

  DatabaseFactory.init(jdbcUrl, dbUser, dbPassword)

  val jwtSecret = System.getenv("JWT_SECRET") ?: "dev-jwt-secret-at-least-32-chars-long-here"

  configureDI(jwtSecret)
  configureSerialization()
  configureAuth()
  configureStatusPages()
  configureRouting()
}
