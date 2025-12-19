---
title: "How to Implement Error Handling Patterns"
date: 2025-12-18T00:00:00+07:00
draft: false
weight: 606
description: "Effective error handling with exceptions, Result type, and sealed classes"
tags: ["kotlin", "error-handling", "result-type", "exceptions"]
---

## Problem

Error handling requires balancing between throwing exceptions, returning null, and functional error handling. Exceptions can be expensive and implicit. Null lacks context. Kotlin provides multiple approaches: exceptions, Result type, and sealed classes for errors.

This guide shows effective error handling patterns in Kotlin.

## Exception Basics

### Try-Catch Blocks

Handle exceptions with try-catch.

```kotlin
// ✅ Basic try-catch
fun readFile(path: String): String {
  return try {
    File(path).readText()
  } catch (e: IOException) {
    println("Error reading file: ${e.message}")
    ""
  }
}

// ✅ Try as expression
val content: String = try {
  File(path).readText()
} catch (e: IOException) {
  "Default content"
}

// ✅ Multiple catch blocks
fun parseData(input: String): Int {
  return try {
    input.toInt()
  } catch (e: NumberFormatException) {
    println("Invalid number format")
    0
  } catch (e: Exception) {
    println("Unexpected error")
    -1
  }
}
```

**Pattern**: Use try as expression to return values from error handling.

### Finally Block

Execute cleanup code.

```kotlin
// ✅ Finally always executes
fun processFile(path: String) {
  val file = File(path)
  try {
    val content = file.readText()
    process(content)
  } catch (e: IOException) {
    println("Error: ${e.message}")
  } finally {
    println("Cleanup complete")  // Always runs
  }
}

// ✅ Use for resource cleanup
fun writeFile(path: String, content: String) {
  val writer = FileWriter(path)
  try {
    writer.write(content)
  } finally {
    writer.close()  // Ensure resource closed
  }
}
```

### Use Function

Automatic resource management with `use`.

```kotlin
// ✅ Automatic resource cleanup
fun readFile(path: String): String {
  return File(path).inputStream().use { stream ->
    stream.readBytes().toString(Charsets.UTF_8)
  }
  // Stream automatically closed
}

// ✅ Use with multiple resources
fun copyFile(source: String, dest: String) {
  FileInputStream(source).use { input ->
    FileOutputStream(dest).use { output ->
      input.copyTo(output)
    }
  }
  // Both streams automatically closed
}
```

**Pattern**: `use` calls `close()` automatically, even on exception.

## Result Type

### Functional Error Handling

Use Result for operations that can fail.

```kotlin
// ✅ Return Result instead of throwing
fun parseInt(value: String): Result<Int> {
  return try {
    Result.success(value.toInt())
  } catch (e: NumberFormatException) {
    Result.failure(e)
  }
}

// ✅ Handle Result
fun example() {
  val result = parseInt("42")

  result
    .onSuccess { value -> println("Parsed: $value") }
    .onFailure { error -> println("Error: ${error.message}") }
}

// ✅ Get value or default
val value: Int = parseInt("abc").getOrDefault(0)

// ✅ Get value or throw
val value: Int = parseInt("123").getOrThrow()

// ✅ Transform Result
val doubled: Result<Int> = parseInt("10")
  .map { it * 2 }  // Result.success(20)
```

**Use case**: When failure is expected and not exceptional.

### Chaining Result Operations

Compose operations that can fail.

```kotlin
// ✅ Chain Result operations
fun fetchUser(id: String): Result<User> {
  return try {
    val user = database.findUser(id)
      ?: return Result.failure(NotFoundException("User not found"))
    Result.success(user)
  } catch (e: Exception) {
    Result.failure(e)
  }
}

fun fetchProfile(user: User): Result<Profile> {
  return try {
    Result.success(user.profile)
  } catch (e: Exception) {
    Result.failure(e)
  }
}

// ✅ Compose with mapCatching
fun getUserProfile(id: String): Result<Profile> {
  return fetchUser(id)
    .mapCatching { user -> fetchProfile(user).getOrThrow() }
}

// ✅ Usage
getUserProfile("123")
  .onSuccess { profile -> println("Profile: $profile") }
  .onFailure { error -> println("Error: ${error.message}") }
```

## Sealed Classes for Errors

### Type-Safe Error Modeling

Model errors as sealed classes.

```kotlin
// ✅ Sealed class for domain errors
sealed class UserError {
  data class NotFound(val id: String) : UserError()
  data class InvalidEmail(val email: String) : UserError()
  data class DatabaseError(val cause: Throwable) : UserError()
  data object Unauthorized : UserError()
}

// ✅ Return type with error
fun createUser(name: String, email: String): Result<User, UserError> {
  if (!email.contains("@")) {
    return Err(UserError.InvalidEmail(email))
  }

  return try {
    val user = User(name, email)
    database.save(user)
    Ok(user)
  } catch (e: SQLException) {
    Err(UserError.DatabaseError(e))
  }
}

// ✅ Custom Result type
sealed class Result<out T, out E> {
  data class Ok<T>(val value: T) : Result<T, Nothing>()
  data class Err<E>(val error: E) : Result<Nothing, E>()
}

// ✅ Handle errors exhaustively
fun handleUserCreation(result: Result<User, UserError>) {
  when (result) {
    is Result.Ok -> println("User created: ${result.value}")
    is Result.Err -> when (result.error) {
      is UserError.NotFound -> println("User not found")
      is UserError.InvalidEmail -> println("Invalid email: ${result.error.email}")
      is UserError.DatabaseError -> println("DB error: ${result.error.cause}")
      UserError.Unauthorized -> println("Unauthorized")
    }
  }
}
```

**Pattern**: Exhaustive when expressions ensure all errors handled.

### Either Type Pattern

Implement Either for success/failure.

```kotlin
// ✅ Either type
sealed class Either<out L, out R> {
  data class Left<L>(val value: L) : Either<L, Nothing>()
  data class Right<R>(val value: R) : Either<Nothing, R>()

  fun <T> map(f: (R) -> T): Either<L, T> = when (this) {
    is Left -> this
    is Right -> Right(f(value))
  }

  fun <T> flatMap(f: (R) -> Either<L, T>): Either<L, T> = when (this) {
    is Left -> this
    is Right -> f(value)
  }
}

// ✅ Usage
fun divide(a: Int, b: Int): Either<String, Int> {
  return if (b == 0) {
    Either.Left("Division by zero")
  } else {
    Either.Right(a / b)
  }
}

// ✅ Chain operations
val result = divide(10, 2)
  .map { it * 2 }          // Right(10)
  .flatMap { divide(it, 0) }  // Left("Division by zero")
```

## Validation Patterns

### Accumulating Errors

Collect multiple validation errors.

```kotlin
// ✅ Validation result
data class ValidationResult(
  val errors: List<String> = emptyList()
) {
  val isValid: Boolean get() = errors.isEmpty()

  fun addError(message: String): ValidationResult {
    return copy(errors = errors + message)
  }
}

// ✅ Validate with accumulation
fun validateUser(name: String, email: String, age: Int): ValidationResult {
  var result = ValidationResult()

  if (name.isBlank()) {
    result = result.addError("Name cannot be blank")
  }

  if (name.length < 2) {
    result = result.addError("Name must be at least 2 characters")
  }

  if (!email.contains("@")) {
    result = result.addError("Invalid email format")
  }

  if (age < 0 || age > 150) {
    result = result.addError("Age must be between 0 and 150")
  }

  return result
}

// ✅ Handle validation
val validation = validateUser("A", "invalid-email", -5)
if (!validation.isValid) {
  println("Validation errors:")
  validation.errors.forEach { println("- $it") }
}
```

### Validation DSL

Create fluent validation API.

```kotlin
// ✅ Validation DSL
class Validator<T> {
  private val errors = mutableListOf<String>()

  fun check(condition: Boolean, message: String) {
    if (!condition) errors.add(message)
  }

  fun validate(value: T, block: Validator<T>.(T) -> Unit): ValidationResult {
    this.block(value)
    return ValidationResult(errors.toList())
  }
}

fun validateUser(user: CreateUserRequest): ValidationResult {
  return Validator<CreateUserRequest>().validate(user) {
    check(it.name.isNotBlank(), "Name required")
    check(it.name.length >= 2, "Name too short")
    check(it.email.contains("@"), "Invalid email")
  }
}
```

## Custom Exceptions

### Creating Domain Exceptions

Define application-specific exceptions.

```kotlin
// ✅ Base exception
open class AppException(message: String, cause: Throwable? = null) : Exception(message, cause)

// ✅ Domain exceptions
class NotFoundException(message: String) : AppException(message)
class ValidationException(message: String) : AppException(message)
class AuthorizationException(message: String) : AppException(message)

// ✅ Exception with data
class InsufficientFundsException(
  val requested: Double,
  val available: Double
) : AppException("Insufficient funds: requested $requested, available $available")

// ✅ Usage
fun transfer(from: Account, to: Account, amount: Double) {
  if (from.balance < amount) {
    throw InsufficientFundsException(amount, from.balance)
  }

  from.balance -= amount
  to.balance += amount
}

// ✅ Catch specific exceptions
try {
  transfer(account1, account2, 1000.0)
} catch (e: InsufficientFundsException) {
  println("Cannot transfer ${e.requested}, only ${e.available} available")
}
```

## Common Pitfalls

### Swallowing Exceptions

```kotlin
// ❌ Silent failure
try {
  riskyOperation()
} catch (e: Exception) {
  // Nothing - error lost!
}

// ✅ At least log
try {
  riskyOperation()
} catch (e: Exception) {
  logger.error("Operation failed", e)
  throw e  // Re-throw if can't handle
}
```

### Catching Too Broadly

```kotlin
// ❌ Catches everything
try {
  processData()
} catch (e: Exception) {  // Too broad!
  // Can't distinguish different errors
}

// ✅ Catch specific exceptions
try {
  processData()
} catch (e: IOException) {
  // Handle IO errors
} catch (e: ParseException) {
  // Handle parse errors
}
```

### Using Exceptions for Control Flow

```kotlin
// ❌ Exception for normal control flow
fun findUser(id: String): User {
  val user = database.find(id)
  if (user == null) {
    throw NotFoundException()  // Not exceptional!
  }
  return user
}

// ✅ Return null or Result
fun findUser(id: String): User? {
  return database.find(id)
}

fun findUser(id: String): Result<User> {
  val user = database.find(id)
  return if (user != null) {
    Result.success(user)
  } else {
    Result.failure(NotFoundException())
  }
}
```

## Variations

### requireNotNull/checkNotNull

Validate preconditions and state.

```kotlin
// ✅ Validate arguments
fun processUser(user: User?) {
  requireNotNull(user) { "User cannot be null" }
  // user is non-null here
}

// ✅ Validate state
class Service {
  private var connection: Connection? = null

  fun execute(query: String) {
    checkNotNull(connection) { "Service not initialized" }
    connection!!.execute(query)
  }
}
```

### runCatching

Wrap code in Result automatically.

```kotlin
// ✅ runCatching converts exceptions to Result
val result: Result<Int> = runCatching {
  "123".toInt()
}

// ✅ Chain with other Result operations
val doubled: Result<Int> = runCatching {
  "123".toInt()
}.map { it * 2 }

// ✅ Catch specific exceptions
val result = runCatching {
  fetchData()
}.recoverCatching { error ->
  if (error is IOException) {
    loadFromCache()
  } else {
    throw error
  }
}
```

## Related Patterns

**Learn more**:

- [Nullable Types](/en/learn/swe/prog-lang/kotlin/how-to/working-with-nullable-types) - Null handling patterns
- [Sealed Classes](/en/learn/swe/prog-lang/kotlin/how-to/sealed-classes-interfaces) - Type-safe error modeling
- [Beginner Tutorial - Error Handling](/en/learn/swe/prog-lang/kotlin/tutorials/beginner#error-handling) - Exception basics

**Cookbook recipes**:

- [Result Type](/en/learn/swe/prog-lang/kotlin/how-to/cookbook#result-type) - Functional error handling
- [Sealed Errors](/en/learn/swe/prog-lang/kotlin/how-to/cookbook#sealed-errors) - Type-safe errors
- [Try-Catch Idioms](/en/learn/swe/prog-lang/kotlin/how-to/cookbook#try-catch-idioms) - Exception patterns
