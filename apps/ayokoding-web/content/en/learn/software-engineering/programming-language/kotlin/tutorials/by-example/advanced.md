---
title: "Advanced"
weight: 10000003
date: 2025-12-30T02:00:00+07:00
draft: false
description: "Examples 55-80: Advanced coroutines, reflection, metaprogramming, multiplatform (75-95% coverage)"
---

This section covers advanced Kotlin techniques from examples 55-80, achieving 75-95% topic coverage.

## Example 55: Coroutine Channels

Channels enable communication between coroutines with send/receive operations.

```kotlin
import kotlinx.coroutines.*
import kotlinx.coroutines.channels.*

fun main() = runBlocking {
    val channel = Channel<Int>()             // => Unbuffered channel

    // Producer coroutine
    launch {
        for (x in 1..5) {
            channel.send(x)                  // => Suspends until received
            println("Sent $x")
        }
        channel.close()                      // => Signal completion
    }

    // Consumer coroutine
    launch {
        for (value in channel) {             // => Iterate until closed
            println("Received $value")
            delay(100)
        }
    }

    delay(1000)
}
```

**Key Takeaway**: Channels provide CSP-style communication between coroutines with backpressure control.

## Example 56: Buffered Channels and Capacity

Channel capacity controls buffering behavior and backpressure.

```kotlin
import kotlinx.coroutines.*
import kotlinx.coroutines.channels.*

fun main() = runBlocking {
    // Buffered channel (capacity = 2)
    val channel = Channel<Int>(2)            // => Can hold 2 elements

    launch {
        for (x in 1..5) {
            println("Sending $x")
            channel.send(x)                  // => Suspends when buffer full
            println("Sent $x")
        }
        channel.close()
    }

    delay(500)  // => Let producer fill buffer

    for (value in channel) {
        println("Received $value")
        delay(200)                           // => Slow consumer
    }
}
// Output shows sends block when buffer full
```

**Key Takeaway**: Buffered channels decouple producer/consumer speeds with configurable capacity.

## Example 57: Select Expression

Select awaits multiple suspending operations returning first completed result.

```kotlin
import kotlinx.coroutines.*
import kotlinx.coroutines.channels.*
import kotlinx.coroutines.selects.*

fun main() = runBlocking {
    val channel1 = Channel<String>()
    val channel2 = Channel<String>()

    launch {
        delay(100)
        channel1.send("from channel 1")
    }

    launch {
        delay(50)
        channel2.send("from channel 2")      // => Completes first
    }

    val result = select<String> {            // => Wait for first completion
        channel1.onReceive { "Got: $it" }
        channel2.onReceive { "Got: $it" }    // => This completes first
    }

    println(result)                          // => Got: from channel 2
}
```

**Key Takeaway**: Select enables racing multiple async operations returning first result.

## Example 58: Hot and Cold Flows

Cold flows create new producer per collector; hot flows share single producer.

```kotlin
import kotlinx.coroutines.*
import kotlinx.coroutines.flow.*

fun coldFlow() = flow {                      // => Cold flow (new producer per collector)
    println("Flow started")
    for (i in 1..3) {
        delay(100)
        emit(i)
    }
}

fun main() = runBlocking {
    val cold = coldFlow()

    // Each collector gets own producer
    launch {
        cold.collect { println("Collector 1: $it") }  // => Flow started
    }

    delay(50)

    launch {
        cold.collect { println("Collector 2: $it") }  // => Flow started (again)
    }

    delay(500)

    // SharedFlow (hot flow)
    val hot = MutableSharedFlow<Int>()

    launch {
        hot.collect { println("Hot collector 1: $it") }
    }

    launch {
        hot.collect { println("Hot collector 2: $it") }
    }

    delay(100)
    hot.emit(1)                              // => Both collectors receive
    hot.emit(2)
}
```

**Key Takeaway**: Cold flows restart per collector; shared flows broadcast to all collectors.

## Example 59: Flow Exception Handling

Handle flow exceptions with catch operator preserving flow continuity.

```kotlin
import kotlinx.coroutines.*
import kotlinx.coroutines.flow.*

fun dataFlow() = flow {
    emit(1)
    emit(2)
    throw RuntimeException("Flow error")     // => Exception during flow
    emit(3)  // => Never reached
}

fun main() = runBlocking {
    dataFlow()
        .catch { e ->                        // => Catch upstream exceptions
            println("Caught: ${e.message}")
            emit(-1)                         // => Emit fallback value
        }
        .collect { value ->
            println("Collected: $value")
        }
    // => Collected: 1, Collected: 2, Caught: Flow error, Collected: -1
}
```

**Key Takeaway**: catch operator handles upstream exceptions enabling graceful error recovery.

## Example 60: Flow Backpressure with buffer

Control flow backpressure using buffer, conflate, and collectLatest operators.

```kotlin
import kotlinx.coroutines.*
import kotlinx.coroutines.flow.*
import kotlin.system.measureTimeMillis

fun dataFlow() = flow {
    for (i in 1..5) {
        delay(100)                           // => Producer delay
        emit(i)
    }
}

fun main() = runBlocking {
    // Without buffer (slow consumer blocks producer)
    val time1 = measureTimeMillis {
        dataFlow().collect {
            delay(200)                       // => Consumer delay
            println("No buffer: $it")
        }
    }
    println("No buffer time: $time1 ms")     // => ~1500ms (100+200 per item)

    // With buffer (producer doesn't wait for consumer)
    val time2 = measureTimeMillis {
        dataFlow()
            .buffer()                        // => Buffer emissions
            .collect {
                delay(200)
                println("Buffered: $it")
            }
    }
    println("Buffered time: $time2 ms")      // => ~1200ms (faster)

    // Conflate (skip intermediate values)
    dataFlow()
        .conflate()                          // => Keep only latest
        .collect {
            delay(200)
            println("Conflated: $it")        // => May skip values
        }
}
```

**Key Takeaway**: buffer/conflate/collectLatest control backpressure strategies for flow processing.

## Example 61: Reflection Basics

Access class metadata at runtime using Kotlin reflection API.

```kotlin
import kotlin.reflect.full.*

class User(val name: String, var age: Int) {
    fun greet() = "Hello, $name!"
}

fun main() {
    val userClass = User::class                // => KClass reference

    // Class metadata
    println(userClass.simpleName)              // => User
    println(userClass.isData)                  // => false

    // Member properties
    userClass.memberProperties.forEach { prop ->
        println("Property: ${prop.name}, type: ${prop.returnType}")
    }
    // => Property: age, type: kotlin.Int
    // => Property: name, type: kotlin.String

    // Member functions
    userClass.memberFunctions.forEach { func ->
        println("Function: ${func.name}")
    }

    // Create instance reflectively
    val constructor = userClass.primaryConstructor!!
    val instance = constructor.call("Alice", 30)  // => Create User instance
    println(instance.greet())                  // => Hello, Alice!

    // Access property reflectively
    val nameProp = userClass.memberProperties.find { it.name == "name" }!!
    println(nameProp.get(instance))            // => Alice
}
```

**Key Takeaway**: Kotlin reflection enables runtime class introspection and manipulation.

## Example 62: Reflection Property Access

Read and modify properties reflectively with proper mutability handling.

```kotlin
import kotlin.reflect.full.*
import kotlin.reflect.jvm.isAccessible

class Config {
    var host: String = "localhost"
    val port: Int = 8080
    private var secret: String = "secret123"
}

fun main() {
    val config = Config()
    val configClass = config::class

    // Get mutable property
    val hostProp = configClass.memberProperties
        .find { it.name == "host" } as? kotlin.reflect.KMutableProperty<*>

    hostProp?.let {
        println("Current host: ${it.get(config)}")  // => localhost
        it.setter.call(config, "example.com")       // => Set new value
        println("New host: ${it.get(config)}")      // => example.com
    }

    // Access private property (requires isAccessible = true)
    val secretProp = configClass.memberProperties.find { it.name == "secret" }!!
    secretProp.isAccessible = true                  // => Allow private access
    println("Secret: ${secretProp.get(config)}")    // => secret123
}
```

**Key Takeaway**: Reflection enables property read/write with mutability and visibility control.

## Example 63: Annotations and Reflection

Define custom annotations and process them reflectively.

```kotlin
import kotlin.reflect.full.*

@Target(AnnotationTarget.CLASS)
annotation class Entity(val tableName: String)

@Target(AnnotationTarget.PROPERTY)
annotation class Column(val name: String = "")

@Entity(tableName = "users")
data class User(
    @Column(name = "user_name") val name: String,
    @Column val age: Int
)

fun main() {
    val userClass = User::class

    // Read class annotation
    val entityAnnotation = userClass.findAnnotation<Entity>()
    println("Table: ${entityAnnotation?.tableName}")  // => Table: users

    // Read property annotations
    userClass.memberProperties.forEach { prop ->
        val columnAnnotation = prop.findAnnotation<Column>()
        val columnName = columnAnnotation?.name?.takeIf { it.isNotEmpty() } ?: prop.name
        println("Property ${prop.name} -> Column $columnName")
    }
    // => Property name -> Column user_name
    // => Property age -> Column age
}
```

**Key Takeaway**: Annotations with reflection enable metadata-driven frameworks and code generation.

## Example 64: KProperty Delegates with Reflection

Access delegated properties metadata using reflection.

```kotlin
import kotlin.properties.Delegates
import kotlin.reflect.KProperty
import kotlin.reflect.full.*
import kotlin.reflect.jvm.*

class TrackedDelegate<T>(private var value: T) {
    operator fun getValue(thisRef: Any?, property: KProperty<*>): T {
        return value
    }

    operator fun setValue(thisRef: Any?, property: KProperty<*>, newValue: T) {
        value = newValue
    }
}

class Settings {
    var theme: String by TrackedDelegate("light")
    var language: String by Delegates.observable("en") { _, old, new ->
        println("Language: $old -> $new")
    }
}

fun main() {
    val settings = Settings()
    val settingsClass = settings::class

    // Find delegated properties
    settingsClass.memberProperties.forEach { prop ->
        val javaField = prop.javaField
        if (javaField != null) {
            javaField.isAccessible = true
            val delegate = javaField.get(settings)
            println("${prop.name} delegate: ${delegate::class.simpleName}")
        }
    }
    // => theme delegate: TrackedDelegate
    // => language delegate: ObservableProperty
}
```

**Key Takeaway**: Reflection reveals property delegation implementation details at runtime.

## Example 65: DSL with Lambda Extensions

Create expressive DSLs using lambda with receiver and operator overloading.

```kotlin
@DslMarker
annotation class QueryDsl

@QueryDsl
class Query {
    private val conditions = mutableListOf<String>()

    infix fun String.eq(value: Any) {
        conditions.add("$this = '$value'")
    }

    infix fun String.gt(value: Any) {
        conditions.add("$this > $value")
    }

    fun and(block: Query.() -> Unit) {
        block()
    }

    fun build(): String = "SELECT * WHERE ${conditions.joinToString(" AND ")}"
}

fun query(init: Query.() -> Unit): String {
    return Query().apply(init).build()
}

fun main() {
    val sql = query {
        "name" eq "Alice"
        and {
            "age" gt 25
        }
    }
    println(sql)  // => SELECT * WHERE name = 'Alice' AND age > 25
}
```

**Key Takeaway**: DSL marker prevents scope pollution in nested DSL builders.

## Example 66: Inline Reified for JSON Parsing

Combine inline and reified for type-safe generic operations.

```kotlin
import kotlin.reflect.KClass

// Simulated JSON parser
object JsonParser {
    fun <T : Any> parse(json: String, clazz: KClass<T>): T? {
        // Parse based on class type
        return when (clazz.simpleName) {
            "User" -> User("Alice", 30) as T
            "Product" -> Product("Laptop", 999.99) as T
            else -> null
        }
    }
}

data class User(val name: String, val age: Int)
data class Product(val name: String, val price: Double)

// Generic function without reified (verbose)
fun <T : Any> parseJsonOld(json: String, clazz: KClass<T>): T? {
    return JsonParser.parse(json, clazz)
}

// Generic function with reified (clean API)
inline fun <reified T : Any> parseJson(json: String): T? {
    return JsonParser.parse(json, T::class)  // => T::class available due to reified
}

fun main() {
    // Old way (need to pass class)
    val user1 = parseJsonOld("{...}", User::class)

    // New way (type inferred)
    val user2: User? = parseJson("{...}")    // => Type parameter inferred
    val product: Product? = parseJson("{...}")

    println(user2)                           // => User(name=Alice, age=30)
    println(product)                         // => Product(name=Laptop, price=999.99)
}
```

**Key Takeaway**: Reified type parameters eliminate explicit class parameter passing for cleaner generic APIs.

## Example 67: Multiplatform Common Module

Define shared business logic in common module with expect declarations.

```kotlin
// commonMain/Platform.kt
expect class PlatformLogger() {
    fun log(message: String)
}

// commonMain/UserService.kt
class UserService(private val logger: PlatformLogger) {
    fun createUser(name: String): User {
        logger.log("Creating user: $name")   // => Platform-specific logging
        return User(name)
    }
}

data class User(val name: String)

// jvmMain/Platform.kt
actual class PlatformLogger {
    actual fun log(message: String) {
        println("[JVM] $message")            // => JVM implementation
    }
}

// jsMain/Platform.kt
// actual class PlatformLogger {
//     actual fun log(message: String) {
//         console.log("[JS] $message")      // => JS implementation
//     }
// }

// Usage in common code
fun main() {
    val logger = PlatformLogger()            // => Resolved to platform impl
    val service = UserService(logger)
    val user = service.createUser("Alice")   // => [JVM] Creating user: Alice
}
```

**Key Takeaway**: Common modules define shared logic; platform modules provide implementations.

## Example 68: Gradle Multiplatform Configuration

Configure Kotlin multiplatform project targeting JVM, JS, and Native.

```kotlin
// build.gradle.kts
plugins {
    kotlin("multiplatform") version "1.9.20"
}

kotlin {
    // JVM target
    jvm {
        withJava()                           // => Include Java support
    }

    // JavaScript target
    js {
        browser {
            commonWebpackConfig {
                cssSupport.enabled = true
            }
        }
    }

    // Native targets
    linuxX64()
    macosArm64()

    sourceSets {
        // Common source set (shared code)
        val commonMain by getting {
            dependencies {
                implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.7.3")
            }
        }

        // JVM source set
        val jvmMain by getting {
            dependencies {
                implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core-jvm:1.7.3")
            }
        }

        // JS source set
        val jsMain by getting {
            dependencies {
                implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core-js:1.7.3")
            }
        }
    }
}
```

**Key Takeaway**: Multiplatform configuration defines targets and source sets for cross-platform development.

## Example 69: Serialization with kotlinx.serialization

Serialize data classes to JSON with compiler plugin support.

```kotlin
import kotlinx.serialization.*
import kotlinx.serialization.json.*

@Serializable
data class User(
    val id: Int,
    val name: String,
    @SerialName("email_address")            // => Custom JSON field name
    val email: String,
    val roles: List<String> = emptyList()
)

fun main() {
    val user = User(1, "Alice", "alice@example.com", listOf("admin", "user"))

    // Serialize to JSON
    val json = Json.encodeToString(user)
    println(json)
    // => {"id":1,"name":"Alice","email_address":"alice@example.com","roles":["admin","user"]}

    // Deserialize from JSON
    val parsed = Json.decodeFromString<User>(json)
    println(parsed)                          // => User(id=1, name=Alice, ...)

    // Pretty printing
    val prettyJson = Json { prettyPrint = true }
    println(prettyJson.encodeToString(user))
}
```

**Key Takeaway**: kotlinx.serialization provides compile-time safe JSON serialization with annotation configuration.

## Example 70: Custom Serializers

Implement custom serializers for complex types.

```kotlin
import kotlinx.serialization.*
import kotlinx.serialization.descriptors.*
import kotlinx.serialization.encoding.*
import kotlinx.serialization.json.Json
import java.time.LocalDate
import java.time.format.DateTimeFormatter

object LocalDateSerializer : KSerializer<LocalDate> {
    private val formatter = DateTimeFormatter.ISO_LOCAL_DATE

    override val descriptor: SerialDescriptor =
        PrimitiveSerialDescriptor("LocalDate", PrimitiveKind.STRING)

    override fun serialize(encoder: Encoder, value: LocalDate) {
        encoder.encodeString(value.format(formatter))  // => Serialize as string
    }

    override fun deserialize(decoder: Decoder): LocalDate {
        return LocalDate.parse(decoder.decodeString(), formatter)  // => Parse string
    }
}

@Serializable
data class Event(
    val name: String,
    @Serializable(with = LocalDateSerializer::class)
    val date: LocalDate
)

fun main() {
    val event = Event("Conference", LocalDate.of(2025, 12, 30))

    val json = Json.encodeToString(event)
    println(json)  // => {"name":"Conference","date":"2025-12-30"}

    val parsed = Json.decodeFromString<Event>(json)
    println(parsed.date)                     // => 2025-12-30
}
```

**Key Takeaway**: Custom serializers handle types without built-in serialization support.

## Example 71: Ktor Server Basics

Build HTTP servers with Ktor framework using DSL-based routing.

```kotlin
import io.ktor.server.application.*
import io.ktor.server.engine.*
import io.ktor.server.netty.*
import io.ktor.server.response.*
import io.ktor.server.routing.*
import io.ktor.http.*

fun main() {
    embeddedServer(Netty, port = 8080) {
        routing {
            get("/") {                       // => Handle GET /
                call.respondText("Hello, Ktor!", ContentType.Text.Plain)
            }

            get("/user/{id}") {              // => Path parameter
                val id = call.parameters["id"]
                call.respondText("User ID: $id")
            }

            post("/users") {                 // => Handle POST /users
                call.respond(HttpStatusCode.Created, "User created")
            }
        }
    }.start(wait = true)                     // => Start server (blocks)
}
```

**Key Takeaway**: Ktor provides type-safe DSL for building HTTP servers with coroutine support.

## Example 72: Ktor Content Negotiation

Automatic JSON serialization/deserialization with content negotiation.

```kotlin
import io.ktor.server.application.*
import io.ktor.server.plugins.contentnegotiation.*
import io.ktor.serialization.kotlinx.json.*
import io.ktor.server.request.*
import io.ktor.server.response.*
import io.ktor.server.routing.*
import kotlinx.serialization.Serializable

@Serializable
data class User(val id: Int, val name: String)

fun Application.module() {
    install(ContentNegotiation) {
        json()                               // => Enable JSON support
    }

    routing {
        get("/users/{id}") {
            val id = call.parameters["id"]?.toInt() ?: 0
            call.respond(User(id, "User $id"))  // => Automatic JSON serialization
        }

        post("/users") {
            val user = call.receive<User>()  // => Automatic JSON deserialization
            call.respond(user)
        }
    }
}
```

**Key Takeaway**: Content negotiation plugin handles automatic JSON conversion for requests/responses.

## Example 73: Arrow Either for Error Handling

Use Arrow's Either type for functional error handling.

```kotlin
import arrow.core.Either
import arrow.core.left
import arrow.core.right

sealed class UserError {
    object NotFound : UserError()
    data class ValidationError(val message: String) : UserError()
}

data class User(val id: Int, val name: String)

fun findUser(id: Int): Either<UserError, User> {
    return when {
        id < 0 -> UserError.ValidationError("Invalid ID").left()
        id == 0 -> UserError.NotFound.left()
        else -> User(id, "User $id").right()  // => Success case
    }
}

fun main() {
    // Success case
    findUser(1).fold(
        { error -> println("Error: $error") },
        { user -> println("Found: $user") }   // => Found: User(id=1, name=User 1)
    )

    // Error cases
    findUser(0).fold(
        { error -> println("Error: $error") },  // => Error: NotFound
        { user -> println("Found: $user") }
    )

    findUser(-1).fold(
        { error -> println("Error: $error") },  // => Error: ValidationError(...)
        { user -> println("Found: $user") }
    )
}
```

**Key Takeaway**: Either provides type-safe error handling as an alternative to exceptions.

## Example 74: Arrow Validated for Accumulating Errors

Validated accumulates all validation errors instead of failing fast.

```kotlin
import arrow.core.*
import arrow.core.raise.*

data class ValidationError(val field: String, val message: String)
data class User(val name: String, val email: String, val age: Int)

fun validateName(name: String): Validated<ValidationError, String> =
    if (name.isNotBlank()) name.valid()
    else ValidationError("name", "Name cannot be blank").invalid()

fun validateEmail(email: String): Validated<ValidationError, String> =
    if (email.contains("@")) email.valid()
    else ValidationError("email", "Invalid email").invalid()

fun validateAge(age: Int): Validated<ValidationError, Int> =
    if (age >= 0) age.valid()
    else ValidationError("age", "Age must be non-negative").invalid()

fun createUser(name: String, email: String, age: Int): ValidatedNel<ValidationError, User> {
    return zipOrAccumulate(
        validateName(name).toValidatedNel(),
        validateEmail(email).toValidatedNel(),
        validateAge(age).toValidatedNel()
    ) { n, e, a -> User(n, e, a) }           // => Accumulate all errors
}

fun main() {
    // Valid user
    createUser("Alice", "alice@example.com", 30).fold(
        { errors -> println("Errors: $errors") },
        { user -> println("Created: $user") }  // => Created: User(...)
    )

    // Invalid user (multiple errors)
    createUser("", "invalid", -5).fold(
        { errors ->                          // => All 3 errors accumulated
            errors.forEach { println("- ${it.field}: ${it.message}") }
        },
        { user -> println("Created: $user") }
    )
}
```

**Key Takeaway**: Validated accumulates errors enabling comprehensive validation feedback.

## Example 75: Performance Optimization - Inline Classes

Use inline classes to eliminate allocation overhead for wrapper types.

```kotlin
@JvmInline
value class Meters(val value: Double) {
    operator fun plus(other: Meters) = Meters(value + other.value)
}

@JvmInline
value class Seconds(val value: Double)

fun calculateSpeed(distance: Meters, time: Seconds): Double {
    return distance.value / time.value       // => No boxing overhead
}

fun main() {
    val d1 = Meters(100.0)                   // => No allocation (inlined)
    val d2 = Meters(50.0)
    val total = d1 + d2                      // => Inline arithmetic

    val speed = calculateSpeed(total, Seconds(10.0))
    println("Speed: $speed m/s")             // => Speed: 15.0 m/s
}
```

**Key Takeaway**: Inline classes provide zero-cost type safety for performance-critical code.

## Example 76: Performance - Lazy Sequences

Use sequences for large collections to avoid intermediate allocations.

```kotlin
fun main() {
    // List operations (eager - creates intermediate lists)
    val listResult = (1..1_000_000)
        .map { it * 2 }                      // => Allocates 1M element list
        .filter { it > 1000 }                // => Allocates another list
        .take(10)

    // Sequence operations (lazy - computes on demand)
    val seqResult = (1..1_000_000).asSequence()
        .map { it * 2 }                      // => No allocation yet
        .filter { it > 1000 }                // => No allocation yet
        .take(10)                            // => Computes only 10 elements
        .toList()

    println(seqResult)                       // => [1002, 1004, ..., 1020]
}
```

**Key Takeaway**: Sequences eliminate intermediate collections for multi-step transformations.

## Example 77: Testing with kotest

Write expressive tests using kotest's specification styles.

```kotlin
import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe
import io.kotest.matchers.string.shouldStartWith

class UserServiceTest : StringSpec({
    "user creation should set default role" {
        val user = createUser("Alice")
        user.role shouldBe "user"            // => Assertion
    }

    "email should contain @" {
        val email = "alice@example.com"
        email shouldStartWith "alice"
        email should include("@")            // => Matcher
    }
})

fun createUser(name: String) = User(name, "user")
data class User(val name: String, val role: String)
```

**Key Takeaway**: kotest provides expressive DSL-based testing with rich matcher library.

## Example 78: Testing Coroutines with runTest

Test coroutines with virtual time using runTest from kotlinx-coroutines-test.

```kotlin
import kotlinx.coroutines.*
import kotlinx.coroutines.test.*
import kotlin.test.Test
import kotlin.test.assertEquals

class CoroutineTest {
    @Test
    fun testDelayedOperation() = runTest {   // => Virtual time environment
        var result = 0

        launch {
            delay(1000)                      // => Virtual delay (instant)
            result = 42
        }

        advanceUntilIdle()                   // => Fast-forward virtual time
        assertEquals(42, result)             // => Verify result
    }

    @Test
    fun testTimeout() = runTest {
        val deferred = async {
            delay(2000)
            "result"
        }

        advanceTimeBy(1000)                  // => Advance 1 second
        assert(!deferred.isCompleted)        // => Not completed yet

        advanceTimeBy(1000)                  // => Advance another second
        assert(deferred.isCompleted)         // => Now completed
        assertEquals("result", deferred.await())
    }
}
```

**Key Takeaway**: runTest enables fast deterministic testing of time-dependent coroutines.

## Example 79: Mocking with MockK

Create test doubles using MockK library for Kotlin-friendly mocking.

```kotlin
import io.mockk.*
import kotlin.test.Test
import kotlin.test.assertEquals

interface UserRepository {
    fun findUser(id: Int): User?
    fun saveUser(user: User)
}

data class User(val id: Int, val name: String)

class UserServiceTest {
    @Test
    fun testFindUser() {
        val repo = mockk<UserRepository>()   // => Create mock

        every { repo.findUser(1) } returns User(1, "Alice")  // => Stub behavior
        every { repo.findUser(2) } returns null

        val result = repo.findUser(1)
        assertEquals("Alice", result?.name)  // => Verify stubbed response

        verify { repo.findUser(1) }          // => Verify method called
    }

    @Test
    fun testSaveUser() {
        val repo = mockk<UserRepository>(relaxed = true)  // => Relaxed mock
        val user = User(1, "Bob")

        repo.saveUser(user)

        verify { repo.saveUser(user) }       // => Verify save called
    }
}
```

**Key Takeaway**: MockK provides Kotlin-idiomatic mocking with DSL-based stubbing and verification.

## Example 80: Best Practices - Immutability and Data Classes

Embrace immutability with data classes and copy for modifications.

```kotlin
data class User(
    val id: Int,
    val name: String,
    val email: String,
    val roles: List<String> = emptyList()
) {
    // Immutable modification methods
    fun withName(newName: String) = copy(name = newName)
    fun addRole(role: String) = copy(roles = roles + role)
}

fun main() {
    val user = User(1, "Alice", "alice@example.com")

    // Immutable updates (original unchanged)
    val renamed = user.withName("Alicia")    // => New instance
    val withRole = user.addRole("admin")     // => New instance

    println(user)                            // => Original unchanged
    println(renamed)                         // => Name updated
    println(withRole)                        // => Role added

    // Chaining copies
    val updated = user
        .copy(name = "Bob")
        .addRole("admin")
        .addRole("user")

    println(updated)                         // => User(id=1, name=Bob, roles=[admin, user])
}
```

**Key Takeaway**: Immutability with data classes and copy enables thread-safe value-based programming.

## Summary

Advanced Kotlin (examples 55-80) covers expert-level techniques: advanced coroutines with channels and flows, reflection for runtime introspection, multiplatform development, functional programming with Arrow, Ktor for server development, performance optimization with inline classes and sequences, comprehensive testing strategies, and immutability best practices. Master these techniques to write production-grade Kotlin systems operating at 95% language coverage.
