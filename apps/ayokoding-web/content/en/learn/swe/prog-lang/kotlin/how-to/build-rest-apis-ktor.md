---
title: "How to Build REST APIs with Ktor"
date: 2025-12-18T00:00:00+07:00
draft: false
weight: 614
description: "Build production-ready REST APIs using Ktor framework"
tags: ["kotlin", "ktor", "rest-api", "web-development"]
categories: ["learn"]
---

## Problem

Building REST APIs requires routing, request handling, JSON serialization, validation, and error handling. Traditional frameworks can be heavyweight. Ktor is a lightweight, coroutine-based framework designed for Kotlin.

This guide shows how to build REST APIs with Ktor.

## Basic Setup

### Project Configuration

Set up Ktor with Gradle Kotlin DSL.

```kotlin
// ✅ build.gradle.kts
plugins {
  kotlin("jvm") version "2.3.0"
  kotlin("plugin.serialization") version "2.3.0"
  id("io.ktor.plugin") version "2.3.7"
}

dependencies {
  implementation("io.ktor:ktor-server-core:2.3.7")
  implementation("io.ktor:ktor-server-netty:2.3.7")
  implementation("io.ktor:ktor-server-content-negotiation:2.3.7")
  implementation("io.ktor:ktor-serialization-kotlinx-json:2.3.7")

  testImplementation("io.ktor:ktor-server-test-host:2.3.7")
  testImplementation(kotlin("test"))
}
```

### Simple Server

Create a basic Ktor server.

```kotlin
import io.ktor.server.engine.*
import io.ktor.server.netty.*
import io.ktor.server.application.*
import io.ktor.server.response.*
import io.ktor.server.routing.*

// ✅ Basic Ktor server
fun main() {
  embeddedServer(Netty, port = 8080) {
    routing {
      get("/") {
        call.respondText("Hello, Ktor!")
      }
    }
  }.start(wait = true)
}
```

**How it works**: `embeddedServer` creates a server, `routing` defines endpoints.

## Routing

### Defining Routes

Create RESTful endpoints.

```kotlin
import io.ktor.http.*
import io.ktor.server.request.*

// ✅ CRUD routes
fun Application.configureRouting() {
  routing {
    // GET /users - list all users
    get("/users") {
      val users = userService.getAllUsers()
      call.respond(users)
    }

    // GET /users/{id} - get user by ID
    get("/users/{id}") {
      val id = call.parameters["id"] ?: return@get call.respond(
        HttpStatusCode.BadRequest,
        "Missing id"
      )

      val user = userService.getUser(id)
        ?: return@get call.respond(HttpStatusCode.NotFound)

      call.respond(user)
    }

    // POST /users - create user
    post("/users") {
      val user = call.receive<CreateUserRequest>()
      val created = userService.createUser(user)
      call.respond(HttpStatusCode.Created, created)
    }

    // PUT /users/{id} - update user
    put("/users/{id}") {
      val id = call.parameters["id"] ?: return@put call.respond(
        HttpStatusCode.BadRequest
      )

      val update = call.receive<UpdateUserRequest>()
      val updated = userService.updateUser(id, update)
        ?: return@put call.respond(HttpStatusCode.NotFound)

      call.respond(updated)
    }

    // DELETE /users/{id} - delete user
    delete("/users/{id}") {
      val id = call.parameters["id"] ?: return@delete call.respond(
        HttpStatusCode.BadRequest
      )

      val deleted = userService.deleteUser(id)
      if (deleted) {
        call.respond(HttpStatusCode.NoContent)
      } else {
        call.respond(HttpStatusCode.NotFound)
      }
    }
  }
}
```

### Route Organization

Group related routes.

```kotlin
// ✅ Nested routing
fun Application.configureRouting() {
  routing {
    route("/api") {
      route("/users") {
        get { /* list users */ }
        post { /* create user */ }

        route("/{id}") {
          get { /* get user */ }
          put { /* update user */ }
          delete { /* delete user */ }
        }
      }

      route("/posts") {
        get { /* list posts */ }
        post { /* create post */ }
      }
    }
  }
}
```

## JSON Serialization

### Configure Content Negotiation

Enable JSON serialization with kotlinx.serialization.

```kotlin
import io.ktor.serialization.kotlinx.json.*
import io.ktor.server.plugins.contentnegotiation.*
import kotlinx.serialization.json.Json

// ✅ Configure JSON serialization
fun Application.configureSerialization() {
  install(ContentNegotiation) {
    json(Json {
      prettyPrint = true
      isLenient = true
      ignoreUnknownKeys = true
    })
  }
}
```

### Serializable Data Classes

Define request/response models.

```kotlin
import kotlinx.serialization.Serializable

// ✅ Response models
@Serializable
data class User(
  val id: String,
  val name: String,
  val email: String,
  val createdAt: Long
)

@Serializable
data class CreateUserRequest(
  val name: String,
  val email: String
)

@Serializable
data class UpdateUserRequest(
  val name: String?,
  val email: String?
)

// ✅ Usage in routes
post("/users") {
  val request = call.receive<CreateUserRequest>()
  val user = User(
    id = generateId(),
    name = request.name,
    email = request.email,
    createdAt = System.currentTimeMillis()
  )
  call.respond(HttpStatusCode.Created, user)
}
```

## Request Validation

### Manual Validation

Validate request data.

```kotlin
// ✅ Validation function
fun validateCreateUserRequest(request: CreateUserRequest): List<String> {
  val errors = mutableListOf<String>()

  if (request.name.isBlank()) {
    errors.add("Name cannot be blank")
  }

  if (request.name.length < 2) {
    errors.add("Name must be at least 2 characters")
  }

  if (!request.email.contains("@")) {
    errors.add("Invalid email format")
  }

  return errors
}

// ✅ Usage in route
post("/users") {
  val request = call.receive<CreateUserRequest>()

  val errors = validateCreateUserRequest(request)
  if (errors.isNotEmpty()) {
    return@post call.respond(
      HttpStatusCode.BadRequest,
      mapOf("errors" to errors)
    )
  }

  val user = userService.createUser(request)
  call.respond(HttpStatusCode.Created, user)
}
```

### Validation DSL

Create reusable validation.

```kotlin
// ✅ Validation DSL
class ValidationResult {
  private val errors = mutableListOf<String>()

  fun error(message: String) {
    errors.add(message)
  }

  fun isValid() = errors.isEmpty()
  fun getErrors() = errors.toList()
}

fun validate(block: ValidationResult.() -> Unit): ValidationResult {
  return ValidationResult().apply(block)
}

// ✅ Usage
val validation = validate {
  if (request.name.isBlank()) error("Name required")
  if (request.email.isBlank()) error("Email required")
  if (!request.email.contains("@")) error("Invalid email")
}

if (!validation.isValid()) {
  call.respond(HttpStatusCode.BadRequest, validation.getErrors())
}
```

## Error Handling

### Status Pages Plugin

Handle exceptions globally.

```kotlin
import io.ktor.server.plugins.statuspages.*

// ✅ Global error handling
fun Application.configureErrorHandling() {
  install(StatusPages) {
    exception<IllegalArgumentException> { call, cause ->
      call.respond(
        HttpStatusCode.BadRequest,
        mapOf("error" to cause.message)
      )
    }

    exception<NotFoundException> { call, cause ->
      call.respond(
        HttpStatusCode.NotFound,
        mapOf("error" to cause.message)
      )
    }

    exception<Throwable> { call, cause ->
      call.respond(
        HttpStatusCode.InternalServerError,
        mapOf("error" to "Internal server error")
      )
      cause.printStackTrace()
    }
  }
}

// ✅ Custom exception
class NotFoundException(message: String) : Exception(message)

// ✅ Usage in routes
get("/users/{id}") {
  val id = call.parameters["id"]!!
  val user = userService.getUser(id)
    ?: throw NotFoundException("User not found: $id")

  call.respond(user)
}
```

## Testing

### Testing Routes

Test API endpoints with ktor-server-test-host.

```kotlin
import io.ktor.client.request.*
import io.ktor.client.statement.*
import io.ktor.http.*
import io.ktor.server.testing.*
import kotlin.test.Test
import kotlin.test.assertEquals

class UserRoutesTest {
  @Test
  fun testGetUsers() = testApplication {
    application {
      configureRouting()
    }

    // ✅ Test GET request
    val response = client.get("/users")
    assertEquals(HttpStatusCode.OK, response.status)
  }

  @Test
  fun testCreateUser() = testApplication {
    application {
      configureSerialization()
      configureRouting()
    }

    // ✅ Test POST request with JSON
    val response = client.post("/users") {
      contentType(ContentType.Application.Json)
      setBody("""{"name":"Alice","email":"alice@example.com"}""")
    }

    assertEquals(HttpStatusCode.Created, response.status)
  }

  @Test
  fun testGetUserNotFound() = testApplication {
    application {
      configureRouting()
      configureErrorHandling()
    }

    // ✅ Test 404 response
    val response = client.get("/users/999")
    assertEquals(HttpStatusCode.NotFound, response.status)
  }
}
```

## Common Pitfalls

### Forgetting Content Negotiation

```kotlin
// ❌ No ContentNegotiation installed
get("/users") {
  val users = listOf(User(...))
  call.respond(users)  // Won't serialize to JSON!
}

// ✅ Install ContentNegotiation first
fun Application.module() {
  install(ContentNegotiation) {
    json()
  }
  configureRouting()
}
```

### Blocking in Routes

```kotlin
// ❌ Blocking call in route
get("/users") {
  val users = database.query("SELECT * FROM users")  // Blocks!
  call.respond(users)
}

// ✅ Use suspend functions
get("/users") {
  val users = withContext(Dispatchers.IO) {
    database.query("SELECT * FROM users")
  }
  call.respond(users)
}
```

### Not Handling Exceptions

```kotlin
// ❌ Unhandled exception crashes server
get("/users/{id}") {
  val id = call.parameters["id"]!!  // Crashes if missing
  call.respond(userService.getUser(id))
}

// ✅ Proper error handling
get("/users/{id}") {
  val id = call.parameters["id"] ?: return@get call.respond(
    HttpStatusCode.BadRequest,
    "Missing id"
  )

  val user = userService.getUser(id)
    ?: return@get call.respond(HttpStatusCode.NotFound)

  call.respond(user)
}
```

## Variations

### Authentication

Add authentication with JWT.

```kotlin
import io.ktor.server.auth.*
import io.ktor.server.auth.jwt.*

// ✅ Configure JWT authentication
fun Application.configureSecurity() {
  install(Authentication) {
    jwt("auth-jwt") {
      verifier(makeJwtVerifier())
      validate { credential ->
        if (credential.payload.getClaim("userId").asString() != "") {
          JWTPrincipal(credential.payload)
        } else null
      }
    }
  }
}

// ✅ Protected routes
routing {
  authenticate("auth-jwt") {
    get("/protected") {
      val principal = call.principal<JWTPrincipal>()
      val userId = principal!!.payload.getClaim("userId").asString()
      call.respond("Hello, $userId")
    }
  }
}
```

### CORS Configuration

Enable cross-origin requests.

```kotlin
import io.ktor.server.plugins.cors.routing.*

// ✅ Configure CORS
fun Application.configureCORS() {
  install(CORS) {
    allowHost("localhost:3000")
    allowHeader(HttpHeaders.ContentType)
    allowMethod(HttpMethod.Get)
    allowMethod(HttpMethod.Post)
    allowMethod(HttpMethod.Put)
    allowMethod(HttpMethod.Delete)
  }
}
```

## Related Patterns

**Learn more**:

- [Gradle with Kotlin DSL](/en/learn/swe/prog-lang/kotlin/how-to/gradle-kotlin-dsl) - Project setup
- [Coroutines Basics](/en/learn/swe/prog-lang/kotlin/how-to/handle-coroutines-and-async) - Async handling
- [Intermediate Tutorial - REST API](/en/learn/swe/prog-lang/kotlin/tutorials/intermediate#rest-api-with-ktor) - Complete REST API example

**Cookbook recipes**:

- [REST API with Ktor](/en/learn/swe/prog-lang/kotlin/how-to/cookbook#rest-api-ktor) - Quick reference patterns
- [JSON Serialization](/en/learn/swe/prog-lang/kotlin/how-to/cookbook#json-serialization) - Serialization patterns
