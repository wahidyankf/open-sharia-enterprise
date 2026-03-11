package com.organiclever.demoktkt.plugins

import com.organiclever.demoktkt.domain.DomainError
import com.organiclever.demoktkt.domain.DomainException
import io.ktor.http.HttpStatusCode
import io.ktor.server.application.Application
import io.ktor.server.application.install
import io.ktor.server.plugins.statuspages.StatusPages
import io.ktor.server.response.respond

fun Application.configureStatusPages() {
  install(StatusPages) {
    exception<DomainException> { call, cause ->
      when (val error = cause.domainError) {
        is DomainError.ValidationError ->
          call.respond(
            HttpStatusCode.BadRequest,
            mapOf("message" to error.message, "field" to error.field),
          )
        is DomainError.NotFound ->
          call.respond(HttpStatusCode.NotFound, mapOf("message" to "Not found: ${error.entity}"))
        is DomainError.Forbidden ->
          call.respond(HttpStatusCode.Forbidden, mapOf("message" to error.message))
        is DomainError.Conflict ->
          call.respond(HttpStatusCode.Conflict, mapOf("message" to error.message))
        is DomainError.Unauthorized ->
          call.respond(HttpStatusCode.Unauthorized, mapOf("message" to error.message))
        is DomainError.FileTooLarge ->
          call.respond(
            HttpStatusCode.PayloadTooLarge,
            mapOf("message" to "File size exceeds the maximum allowed limit"),
          )
        is DomainError.UnsupportedMediaType ->
          call.respond(
            HttpStatusCode.UnsupportedMediaType,
            mapOf("message" to "Unsupported file type: ${error.contentType}"),
          )
      }
    }
    exception<kotlinx.serialization.SerializationException> { call, cause ->
      call.respond(
        HttpStatusCode.BadRequest,
        mapOf("message" to "Invalid request body: ${cause.message}"),
      )
    }
    exception<Exception> { call, cause ->
      call.application.environment.log.error(
        "Unhandled exception: ${cause.javaClass.name}: ${cause.message}",
        cause,
      )
      call.respond(HttpStatusCode.InternalServerError, mapOf("message" to "Internal server error"))
    }
  }
}
