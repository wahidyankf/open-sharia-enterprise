---
title: "Best Practices"
date: 2025-12-18T00:00:00+07:00
draft: false
weight: 1000030
description: "Idiomatic Kotlin patterns and best practices for production code"
tags: ["kotlin", "best-practices", "idioms", "design-principles"]
categories: ["learn"]
---

## What Makes Kotlin Special

Kotlin combines object-oriented and functional programming paradigms with pragmatic design. Key philosophy:

**Conciseness Without Obscurity**: Reduce boilerplate while maintaining readability. `data class User(val name: String)` generates equals/hashCode/toString automatically.

**Null Safety by Design**: Eliminate NullPointerException at compile-time. The type system distinguishes nullable (`String?`) from non-nullable (`String`) types.

**Interoperability**: Seamless Java integration. Call Java from Kotlin and vice versa without friction.

**Tooling First**: IntelliJ IDEA provides excellent support. IDE features guide you toward idiomatic patterns.

**Practical Over Pure**: Pragmatic choices (mutable collections exist) over dogmatic purity.

## Code Organization

### Principle: One Public Class Per File

**Rationale**: Improves navigability and follows IDE expectations.

```kotlin
// ✅ Good - one public class
// File: User.kt
data class User(val id: String, val name: String)

// Private helpers in same file OK
private fun validateEmail(email: String): Boolean = email.contains("@")

// ❌ Bad - multiple public classes
// File: Models.kt
data class User(val id: String)
data class Order(val id: String)  // Should be Order.kt
data class Product(val id: String)  // Should be Product.kt
```

**Exceptions**: Sealed classes with subclasses, tightly coupled types.

```kotlin
// ✅ Exception - sealed class hierarchy
// File: Result.kt
sealed class Result<out T> {
  data class Success<T>(val data: T) : Result<T>()
  data class Error(val message: String) : Result<Nothing>()
  data object Loading : Result<Nothing>()
}
```

### Principle: Package by Feature, Not Layer

**Rationale**: Related code stays together, easier to understand and change.

```kotlin
// ✅ Good - package by feature
com.example.user/
  User.kt
  UserRepository.kt
  UserService.kt
  UserController.kt

com.example.order/
  Order.kt
  OrderRepository.kt
  OrderService.kt

// ❌ Bad - package by layer
com.example.models/
  User.kt
  Order.kt

com.example.repositories/
  UserRepository.kt
  OrderRepository.kt

com.example.services/
  UserService.kt
  OrderService.kt
```

**Exceptions**: Shared infrastructure (logging, database, HTTP clients).

## Naming Conventions

### Principle: Follow Kotlin Standard Library Conventions

**Rationale**: Consistency with ecosystem improves readability.

```kotlin
// ✅ Good - standard conventions
class UserService  // PascalCase for classes
val userName: String  // camelCase for properties
fun getUserById(id: String): User  // camelCase for functions
const val MAX_RETRY = 3  // SCREAMING_SNAKE_CASE for constants

// ❌ Bad - non-standard
class user_service  // Wrong
val UserName: String  // Wrong
fun GetUserById(): User  // Wrong
const val maxRetry = 3  // Wrong
```

### Principle: Use Meaningful Names

**Rationale**: Code is read more than written.

```kotlin
// ✅ Good - clear intent
fun calculateTotalPrice(items: List<Item>): Double

val validatedUser: User = validate(user)

class EmailValidator

// ❌ Bad - unclear
fun calc(list: List<Item>): Double  // What calculation?

val u2: User = validate(user)  // What's u2?

class EV  // What does EV mean?
```

### Principle: Boolean Properties Start with is/has/can

**Rationale**: Reads naturally in conditionals.

```kotlin
// ✅ Good - natural reading
val isValid: Boolean
val hasPermission: Boolean
val canEdit: Boolean

if (user.isActive && user.hasRole("admin")) { ... }

// ❌ Bad - awkward reading
val valid: Boolean  // if (user.valid) - sounds wrong
val permission: Boolean  // if (user.permission) - unclear
```

## Null Safety Idioms

### Principle: Prefer Non-Nullable Types

**Rationale**: Null safety works best when null is the exception.

```kotlin
// ✅ Good - non-nullable by default
data class User(
  val id: String,
  val name: String,
  val email: String
)

// ❌ Bad - unnecessary nullability
data class User(
  val id: String?,  // ID should never be null
  val name: String?,
  val email: String?
)
```

**Exceptions**: Truly optional data (middle name, optional address).

### Principle: Use Elvis for Fallbacks

**Rationale**: Concise default values.

```kotlin
// ✅ Good - Elvis operator
val displayName = user?.name ?: "Guest"
val timeout = config.timeout ?: 30

// ❌ Bad - verbose
val displayName = if (user?.name != null) user.name else "Guest"
```

### Principle: Use let for Null-Safe Blocks

**Rationale**: Execute code only when non-null.

```kotlin
// ✅ Good - let with safe call
user?.let {
  println("Hello, ${it.name}")
  updateLastSeen(it)
}

// ❌ Bad - explicit null check
if (user != null) {
  println("Hello, ${user.name}")
  updateLastSeen(user)
}
```

**Exception**: When you need else branch, if-else is clearer.

## Function Design

### Principle: Single Responsibility

**Rationale**: Functions should do one thing well.

```kotlin
// ✅ Good - focused functions
fun validateEmail(email: String): Boolean {
  return email.contains("@") && email.contains(".")
}

fun sendEmail(to: String, subject: String, body: String) {
  // Send email logic
}

// ❌ Bad - mixed concerns
fun validateAndSendEmail(email: String, subject: String, body: String): Boolean {
  // Validation AND sending in one function
  if (!email.contains("@")) return false
  // Send email...
  return true
}
```

### Principle: Use Expression Bodies for Simple Functions

**Rationale**: More concise when function is single expression.

```kotlin
// ✅ Good - expression body
fun double(x: Int): Int = x * 2

fun isAdult(age: Int): Boolean = age >= 18

// ❌ Bad - unnecessary block
fun double(x: Int): Int {
  return x * 2
}
```

**Exception**: Multiple statements or complex logic need block bodies.

### Principle: Use Named Arguments for Clarity

**Rationale**: Improves readability with multiple parameters.

```kotlin
// ✅ Good - named arguments
createUser(
  name = "Alice",
  email = "alice@example.com",
  age = 30,
  isActive = true
)

// ❌ Bad - positional arguments
createUser("Alice", "alice@example.com", 30, true)  // What's true?
```

**Exception**: Functions with 1-2 obvious parameters.

## Coroutine Best Practices

### Principle: Use Structured Concurrency

**Rationale**: Avoid leaks, ensure proper cancellation.

```kotlin
// ✅ Good - structured scope
class UserViewModel {
  private val scope = CoroutineScope(SupervisorJob() + Dispatchers.Main)

  fun loadUser() {
    scope.launch {
      val user = fetchUser()
      updateUI(user)
    }
  }

  fun onCleared() {
    scope.cancel()  // Clean cancellation
  }
}

// ❌ Bad - GlobalScope
fun loadUser() {
  GlobalScope.launch {  // Leaks, can't cancel
    val user = fetchUser()
  }
}
```

### Principle: Use Appropriate Dispatchers

**Rationale**: Avoid blocking main thread, use right context.

```kotlin
// ✅ Good - IO dispatcher for blocking calls
suspend fun fetchUser(id: String): User {
  return withContext(Dispatchers.IO) {
    database.query("SELECT * FROM users WHERE id = ?", id)
  }
}

// ❌ Bad - blocking on default dispatcher
suspend fun fetchUser(id: String): User {
  return database.query("...")  // Blocks coroutine thread
}
```

### Principle: Prefer Flow Over Callbacks

**Rationale**: Flow provides structured reactive streams.

```kotlin
// ✅ Good - Flow
fun observeUsers(): Flow<List<User>> = flow {
  while (true) {
    val users = fetchUsers()
    emit(users)
    delay(5000)
  }
}

// ❌ Bad - callbacks
fun observeUsers(callback: (List<User>) -> Unit) {
  // Callback hell
}
```

## Collection Operations

### Principle: Use Functional Operations

**Rationale**: More expressive than imperative loops.

```kotlin
// ✅ Good - functional
val adults = users.filter { it.age >= 18 }
val names = users.map { it.name }
val total = prices.sum()

// ❌ Bad - imperative
val adults = mutableListOf<User>()
for (user in users) {
  if (user.age >= 18) {
    adults.add(user)
  }
}
```

**Exception**: Complex logic where loops are clearer.

### Principle: Use Sequences for Large Collections

**Rationale**: Lazy evaluation improves performance.

```kotlin
// ✅ Good - sequence for large data
val result = largeList.asSequence()
  .filter { it.isActive }
  .map { it.process() }
  .take(10)
  .toList()

// ❌ Bad - eager evaluation creates intermediate lists
val result = largeList
  .filter { it.isActive }  // Creates full filtered list
  .map { it.process() }    // Creates full mapped list
  .take(10)
```

## Testing Practices

### Principle: Test Behavior, Not Implementation

**Rationale**: Tests should survive refactoring.

```kotlin
// ✅ Good - tests behavior
@Test
fun `creating user sends welcome email`() {
  val service = UserService(repository, emailService)

  service.createUser("Alice", "alice@example.com")

  verify { emailService.send(match { it.subject == "Welcome" }) }
}

// ❌ Bad - tests implementation
@Test
fun `creating user calls repository save`() {
  service.createUser("Alice", "alice@example.com")

  verify { repository.save(any()) }  // Implementation detail
}
```

### Principle: Use Descriptive Test Names

**Rationale**: Test names document behavior.

```kotlin
// ✅ Good - describes what and why
@Test
fun `user login fails when password is incorrect`() { }

@Test
fun `order total includes tax and shipping`() { }

// ❌ Bad - unclear
@Test
fun testLogin() { }

@Test
fun test1() { }
```

## Documentation Standards

### Principle: Document Why, Not What

**Rationale**: Code shows what, comments explain why.

```kotlin
// ✅ Good - explains rationale
// Use exponential backoff to avoid overwhelming the API
// when it's recovering from an outage
val delay = baseDelay * (2.0.pow(attempt))

// ❌ Bad - obvious from code
// Multiply baseDelay by 2 raised to attempt power
val delay = baseDelay * (2.0.pow(attempt))
```

### Principle: Use KDoc for Public API

**Rationale**: Documentation for library users.

```kotlin
// ✅ Good - KDoc for public API
/**
 * Fetches user by ID from the database.
 *
 * @param id The user's unique identifier
 * @return The user if found, null otherwise
 * @throws DatabaseException if the database is unavailable
 */
fun getUser(id: String): User?
```

## Code Review Checklist

**Use this checklist during code reviews**:

- [ ] All variables use `val` unless mutation is essential
- [ ] Nullable types have explicit handling (`?.`, `?:`, `!!` with justification)
- [ ] Public APIs return sealed types for errors (not exceptions)
- [ ] Coroutines use structured concurrency (no GlobalScope)
- [ ] Extension functions are in appropriate scope (not global)
- [ ] Data classes used for data containers (immutable preferred)
- [ ] Smart casts leveraged (avoid unnecessary casting)

## When to Break Rules

**Every rule has exceptions.** Break rules when:

1. **Clarity improves**: Sometimes verbose is clearer
2. **Performance matters**: Optimize hot paths
3. **Interop requires it**: Java compatibility needs
4. **Domain demands it**: Business rules take precedence

**Example**: Using var instead of val when mutation is clearer:

```kotlin
// Breaking immutability for clarity
fun calculate Statistics(data: List<Int>): Stats {
  var sum = 0
  var count = 0
  var max = Int.MIN_VALUE

  for (value in data) {
    sum += value
    count++
    if (value > max) max = value
  }

  return Stats(sum, count, max)
}
```

## Related Resources

**Learn more**:

- [Anti-Patterns](/en/learn/software-engineering/programming-languages/kotlin/explanation/anti-patterns) - Common mistakes to avoid
- [Beginner Tutorial](/en/learn/software-engineering/programming-languages/kotlin/tutorials/beginner) - Fundamentals
- [Intermediate Tutorial](/en/learn/software-engineering/programming-languages/kotlin/tutorials/intermediate) - Production patterns

**How-To Guides**:

- [Null Safety](/en/learn/software-engineering/programming-languages/kotlin/how-to/working-with-nullable-types)
- [Coroutines](/en/learn/software-engineering/programming-languages/kotlin/how-to/handle-coroutines-and-async)
- [Testing](/en/learn/software-engineering/programming-languages/kotlin/how-to/testing-junit-mockk)
