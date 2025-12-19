---
title: "Common Kotlin Anti-Patterns"
date: 2025-12-18T00:00:00+07:00
draft: false
weight: 704
description: "Identify and avoid common Kotlin mistakes and anti-patterns"
tags: ["kotlin", "anti-patterns", "common-mistakes", "code-smells"]
---

## Java Developer Migration Pitfalls

### Using !! (Not-Null Assertion) Everywhere

**Severity**: Critical

**Why Problematic**: Defeats Kotlin's null safety, converts compile-time safety to runtime crashes.

```kotlin
// ❌ Bad - !! everywhere
fun getUser(id: String): User {
  return repository.findById(id)!!  // NullPointerException at runtime
}

fun processUser(user: User?) {
  println(user!!.name)  // Crashes if null
  sendEmail(user!!.email)
}

// ✅ Better - use safe operators
fun getUser(id: String): User? {
  return repository.findById(id)
}

fun processUser(user: User?) {
  user?.let {
    println(it.name)
    sendEmail(it.email)
  }
}
```

**Context**: Only use !! when you're absolutely certain the value cannot be null and can document why.

### Writing Java-Style Getters/Setters

**Severity**: Minor

**Why Problematic**: Kotlin properties provide this automatically.

```kotlin
// ❌ Bad - Java style
class User {
  private var name: String = ""

  fun getName(): String {
    return name
  }

  fun setName(name: String) {
    this.name = name
  }
}

// ✅ Better - Kotlin properties
class User {
  var name: String = ""
}

// ✅ With validation
class User {
  var name: String = ""
    set(value) {
      require(value.isNotBlank()) { "Name cannot be blank" }
      field = value
    }
}
```

**Context**: Use properties unless you need complex getter/setter logic.

### Not Using Data Classes

**Severity**: Minor

**Why Problematic**: Manually implementing equals/hashCode/toString is error-prone.

```kotlin
// ❌ Bad - manual implementation
class User(val id: String, val name: String) {
  override fun equals(other: Any?): Boolean {
    if (this === other) return true
    if (other !is User) return false
    return id == other.id && name == other.name
  }

  override fun hashCode(): Int {
    return 31 * id.hashCode() + name.hashCode()
  }

  override fun toString(): String {
    return "User(id=$id, name=$name)"
  }
}

// ✅ Better - data class
data class User(val id: String, val name: String)
```

**Context**: Use data classes for data-holding objects unless you need custom behavior.

## Null Safety Violations

### Returning Null Instead of Empty Collections

**Severity**: Major

**Why Problematic**: Forces callers to null-check, increases complexity.

```kotlin
// ❌ Bad - null collection
fun getUsers(): List<User>? {
  val users = database.query()
  return users  // Can be null
}

// Caller must check
val users = getUsers()
if (users != null) {
  users.forEach { ... }
}

// ✅ Better - empty collection
fun getUsers(): List<User> {
  return database.query() ?: emptyList()
}

// Caller can use directly
getUsers().forEach { ... }
```

**Context**: Return empty collections instead of null unless null has specific meaning (e.g., "not fetched yet" vs "empty result").

### Platform Type Unchecked

**Severity**: Major

**Why Problematic**: Java interop can introduce null unexpectedly.

```kotlin
// ❌ Bad - assuming Java method never returns null
val name: String = javaApi.getUserName()  // Might be null!

// ✅ Better - explicit nullability
val name: String? = javaApi.getUserName()
val safeName: String = name ?: "Unknown"

// ✅ Or defensive
val name: String = requireNotNull(javaApi.getUserName()) {
  "API returned null username"
}
```

**Context**: Always treat Java method returns as potentially null unless documented otherwise.

### Late Init Overuse

**Severity**: Major

**Why Problematic**: lateinit bypasses null safety, can crash if accessed before initialization.

```kotlin
// ❌ Bad - lateinit for everything
class UserService {
  lateinit var repository: UserRepository
  lateinit var cache: Cache
  lateinit var logger: Logger
}

// ✅ Better - constructor injection
class UserService(
  private val repository: UserRepository,
  private val cache: Cache,
  private val logger: Logger
)

// ✅ Or nullable if truly optional
class Service {
  private var optionalDependency: Dependency? = null
}
```

**Context**: Only use lateinit for dependency injection frameworks where constructor injection isn't possible.

## Coroutine Misuse

### Using runBlocking in Production

**Severity**: Critical

**Why Problematic**: Blocks threads, defeats purpose of coroutines.

```kotlin
// ❌ Bad - runBlocking in prod
fun getUser(id: String): User {
  return runBlocking {  // Blocks thread!
    fetchUserSuspend(id)
  }
}

// ✅ Better - make function suspend
suspend fun getUser(id: String): User {
  return fetchUserSuspend(id)
}

// ✅ Or use proper scope
class UserService {
  private val scope = CoroutineScope(Dispatchers.IO)

  fun getUser(id: String, callback: (User) -> Unit) {
    scope.launch {
      val user = fetchUserSuspend(id)
      withContext(Dispatchers.Main) {
        callback(user)
      }
    }
  }
}
```

**Context**: runBlocking only for tests and main() functions.

### GlobalScope for Everything

**Severity**: Critical

**Why Problematic**: Memory leaks, can't cancel, violates structured concurrency.

```kotlin
// ❌ Bad - GlobalScope
fun loadData() {
  GlobalScope.launch {  // Leaks if view destroyed
    val data = fetchData()
    updateUI(data)
  }
}

// ✅ Better - lifecycle-aware scope
class ViewModel {
  private val scope = CoroutineScope(SupervisorJob() + Dispatchers.Main)

  fun loadData() {
    scope.launch {
      val data = fetchData()
      updateUI(data)
    }
  }

  fun onCleared() {
    scope.cancel()
  }
}
```

**Context**: Never use GlobalScope except for truly application-lifetime operations.

### Not Using Dispatchers

**Severity**: Major

**Why Problematic**: Blocking operations on wrong dispatcher hurt performance.

```kotlin
// ❌ Bad - blocking on Default dispatcher
suspend fun fetchUser(id: String): User {
  return httpClient.get("/users/$id")  // Blocks coroutine
}

// ✅ Better - use IO dispatcher
suspend fun fetchUser(id: String): User {
  return withContext(Dispatchers.IO) {
    httpClient.get("/users/$id")
  }
}

// ✅ CPU-bound work on Default
suspend fun processData(data: List<Int>): List<Int> {
  return withContext(Dispatchers.Default) {
    data.map { complexCalculation(it) }
  }
}
```

**Context**: IO for blocking I/O, Default for CPU-intensive work, Main for UI updates.

## Performance Anti-Patterns

### Sequence Misuse

**Severity**: Minor

**Why Problematic**: Sequences have overhead, not always faster.

```kotlin
// ❌ Bad - sequence for small collection
val result = listOf(1, 2, 3)
  .asSequence()  // Overhead not worth it
  .map { it * 2 }
  .toList()

// ✅ Better - direct operations
val result = listOf(1, 2, 3)
  .map { it * 2 }

// ✅ Good - sequence for large collection
val result = (1..1_000_000).asSequence()
  .filter { it % 2 == 0 }
  .map { it * it }
  .take(100)
  .toList()
```

**Context**: Use sequences for large collections or when chaining many operations.

### Unnecessary Object Creation in Loops

**Severity**: Major

**Why Problematic**: Excessive allocations cause garbage collection pressure.

```kotlin
// ❌ Bad - creates objects in loop
fun process(data: List<String>): List<Result> {
  val results = mutableListOf<Result>()
  for (item in data) {
    results.add(Result(item.uppercase()))  // New object each iteration
  }
  return results
}

// ✅ Better - functional approach
fun process(data: List<String>): List<Result> {
  return data.map { Result(it.uppercase()) }  // Still creates objects but clearer
}

// ✅ Best - inline class for zero overhead
@JvmInline
value class Result(val value: String)

fun process(data: List<String>): List<Result> {
  return data.map { Result(it.uppercase()) }
}
```

**Context**: Profile before optimizing, but be aware of allocation patterns.

### Excessive Use of Reflection

**Severity**: Minor

**Why Problematic**: Reflection is slow and bypasses compile-time checks.

```kotlin
// ❌ Bad - reflection in hot path
fun process(obj: Any) {
  val clazz = obj::class
  val properties = clazz.memberProperties  // Slow!
  properties.forEach { prop ->
    println("${prop.name}: ${prop.get(obj)}")
  }
}

// ✅ Better - visitor pattern or sealed classes
sealed class Entity {
  abstract fun process()
}

data class User(val name: String) : Entity() {
  override fun process() {
    println("User: $name")
  }
}
```

**Context**: Reflection for frameworks/serialization OK, but avoid in hot code paths.

## Type System Misuse

### Any Type Overuse

**Severity**: Major

**Why Problematic**: Loses type safety benefits.

```kotlin
// ❌ Bad - Any everywhere
fun process(data: Any): Any {
  return when (data) {
    is String -> data.uppercase()
    is Int -> data * 2
    else -> data
  }
}

// ✅ Better - sealed class
sealed class Data {
  data class Text(val value: String) : Data()
  data class Number(val value: Int) : Data()
}

fun process(data: Data): Data {
  return when (data) {
    is Data.Text -> Data.Text(data.value.uppercase())
    is Data.Number -> Data.Number(data.value * 2)
  }
}

// ✅ Or generics
fun <T> process(data: T, transform: (T) -> T): T {
  return transform(data)
}
```

**Context**: Use Any only when truly heterogeneous types needed (JSON parsing, serialization).

### Star Projection Everywhere

**Severity**: Minor

**Why Problematic**: Loses generic type information.

```kotlin
// ❌ Bad - star projection loses information
fun processList(list: List<*>) {
  list.forEach {
    // Can't do much with it: Any?
  }
}

// ✅ Better - preserve type
fun <T> processList(list: List<T>, process: (T) -> Unit) {
  list.forEach { process(it) }
}

// ✅ Or bounded type
fun <T : Number> sumList(list: List<T>): Double {
  return list.sumOf { it.toDouble() }
}
```

**Context**: Star projection when type doesn't matter, but preserve types when possible.

### Mutable vs Immutable Confusion

**Severity**: Major

**Why Problematic**: Unexpected mutations cause bugs.

```kotlin
// ❌ Bad - mutable when immutable intended
fun getUsers(): MutableList<User> {
  return database.query().toMutableList()
}

// Caller can modify!
val users = getUsers()
users.clear()  // Oops!

// ✅ Better - immutable interface
fun getUsers(): List<User> {
  return database.query()
}

// ✅ Internal mutability, external immutability
class UserCache {
  private val _users = mutableListOf<User>()
  val users: List<User> get() = _users.toList()

  fun add(user: User) {
    _users.add(user)
  }
}
```

**Context**: Return immutable interfaces, use mutable collections internally.

## Code Organization

### God Classes

**Severity**: Major

**Why Problematic**: Violates Single Responsibility Principle, hard to test and maintain.

```kotlin
// ❌ Bad - does everything
class UserManager {
  fun createUser() { }
  fun deleteUser() { }
  fun sendEmail() { }
  fun processPayment() { }
  fun generateReport() { }
  fun validateInput() { }
  fun logActivity() { }
  // ... 50 more methods
}

// ✅ Better - single responsibilities
class UserService(
  private val repository: UserRepository,
  private val emailService: EmailService
) {
  fun createUser(data: CreateUserRequest): User {
    val user = data.toUser()
    repository.save(user)
    emailService.sendWelcome(user)
    return user
  }
}

class PaymentService { }
class ReportGenerator { }
```

**Context**: Keep classes focused on one responsibility.

### Public Everything

**Severity**: Minor

**Why Problematic**: Exposes implementation details, hard to refactor.

```kotlin
// ❌ Bad - everything public
class UserService {
  val database: Database  // Should be private!
  val cache: Cache        // Should be private!

  fun validateEmail(email: String): Boolean { }  // Should be private!
  fun hashPassword(password: String): String { } // Should be private!
}

// ✅ Better - minimal public surface
class UserService(
  private val database: Database,
  private val cache: Cache
) {
  fun createUser(name: String, email: String): User {
    validateEmail(email)  // Private helper
    // ...
  }

  private fun validateEmail(email: String): Boolean {
    return email.contains("@")
  }

  private fun hashPassword(password: String): String {
    return BCrypt.hash(password)
  }
}
```

**Context**: Make everything private by default, expose only what's necessary.

## Testing Anti-Patterns

### Testing Implementation Details

**Severity**: Major

**Why Problematic**: Tests break on refactoring, not on behavior changes.

```kotlin
// ❌ Bad - tests internal calls
@Test
fun `create user test`() {
  service.createUser("Alice", "alice@example.com")

  verify { repository.save(any()) }  // Implementation detail
  verify { cache.put(any(), any()) } // Implementation detail
  verify { logger.info(any()) }      // Implementation detail
}

// ✅ Better - tests behavior
@Test
fun `creating user makes it retrievable`() {
  val userId = service.createUser("Alice", "alice@example.com")

  val user = service.getUser(userId)
  assertEquals("Alice", user.name)
  assertEquals("alice@example.com", user.email)
}
```

**Context**: Test public contracts, not private implementation.

### No Assertions

**Severity**: Critical

**Why Problematic**: Test passes even if code is broken.

```kotlin
// ❌ Bad - no assertions
@Test
fun `test user creation`() {
  service.createUser("Alice", "alice@example.com")
  // Test passes but proves nothing!
}

// ✅ Better - clear assertions
@Test
fun `creating user returns user with generated ID`() {
  val user = service.createUser("Alice", "alice@example.com")

  assertNotNull(user.id)
  assertEquals("Alice", user.name)
  assertEquals("alice@example.com", user.email)
}
```

**Context**: Every test needs at least one meaningful assertion.

---

## Java-to-Kotlin Migration Anti-Patterns

### Anti-Pattern: Overusing Force Unwrap (!!)

**Severity**: CRITICAL

**Why Problematic**: Defeats Kotlin's null safety system and causes NullPointerException.

```kotlin
// ❌ Bad - force unwrap everywhere
fun processUser(userId: String) {
    val user = userRepository.findById(userId)!!  // NPE risk
    val address = user.address!!  // NPE risk
    val city = address.city!!  // NPE risk
    println(city.name)
}

// ✅ Better - safe calls and elvis operator
fun processUser(userId: String) {
    val cityName = userRepository.findById(userId)
        ?.address
        ?.city
        ?.name
        ?: "Unknown"
    println(cityName)
}

// ✅ Better - early return
fun processUser(userId: String) {
    val user = userRepository.findById(userId) ?: return
    val address = user.address ?: return
    val city = address.city ?: return
    println(city.name)
}
```

**Context**: Use `!!` only when you have external guarantees and document why.

### Anti-Pattern: Using Mutable Collections Unnecessarily

**Severity**: MAJOR

**Why Problematic**: Mutable state increases complexity and makes code harder to reason about.

```kotlin
// ❌ Bad - mutable collections by default (Java habit)
class UserService {
    private val users = mutableListOf<User>()

    fun getUsers(): MutableList<User> = users  // Exposes internal state!
}

// ✅ Better - immutable by default
class UserService {
    private val _users = mutableListOf<User>()
    val users: List<User> get() = _users.toList()  // Returns immutable copy

    fun addUser(user: User) {
        _users.add(user)
    }
}

// ✅ Best - truly immutable
class UserService {
    private val users = listOf<User>()

    fun withUser(user: User): UserService {
        return UserService(users + user)  // New instance
    }
}
```

**Context**: Default to immutable collections. Use mutable only when necessary and keep them private.

### Anti-Pattern: Not Using Data Classes

**Severity**: MAJOR

**Why Problematic**: Misses free implementations of equals/hashCode/toString/copy.

```kotlin
// ❌ Bad - manual implementation (Java habit)
class User(val id: String, val name: String, val email: String) {
    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is User) return false
        return id == other.id && name == other.name && email == other.email
    }

    override fun hashCode(): Int {
        var result = id.hashCode()
        result = 31 * result + name.hashCode()
        result = 31 * result + email.hashCode()
        return result
    }

    override fun toString(): String = "User(id=$id, name=$name, email=$email)"
}

// ✅ Better - data class generates all of this
data class User(val id: String, val name: String, val email: String)
```

**Context**: Use data classes for simple value holders. Regular classes for complex domain objects with behavior.

---

## Coroutine Anti-Patterns

### Anti-Pattern: Blocking in Suspend Functions

**Severity**: CRITICAL

**Why Problematic**: Defeats the purpose of coroutines and can deadlock.

```kotlin
// ❌ Bad - blocking call in suspend function
suspend fun fetchUser(userId: String): User {
    Thread.sleep(1000)  // Blocks thread!
    return apiClient.getUser(userId).execute()  // Blocking call!
}

// ✅ Better - use delay and suspending APIs
suspend fun fetchUser(userId: String): User {
    delay(1000)  // Suspends without blocking
    return apiClient.getUser(userId)  // Suspending call
}

// ❌ Bad - blocking I/O
suspend fun readFile(path: String): String {
    return File(path).readText()  // Blocks thread!
}

// ✅ Better - use IO dispatcher for blocking operations
suspend fun readFile(path: String): String = withContext(Dispatchers.IO) {
    File(path).readText()  // Blocking OK on IO dispatcher
}
```

**Context**: Use `delay()` instead of `Thread.sleep()`, suspending functions instead of blocking calls.

### Anti-Pattern: GlobalScope for Coroutine Launch

**Severity**: CRITICAL

**Why Problematic**: Coroutines are never cancelled, causing memory leaks.

```kotlin
// ❌ Bad - GlobalScope leaks
class UserViewModel {
    fun loadUser(userId: String) {
        GlobalScope.launch {  // Never cancelled!
            val user = fetchUser(userId)
            updateUI(user)  // Crashes if view destroyed
        }
    }
}

// ✅ Better - lifecycle-aware scope
class UserViewModel : ViewModel() {
    private val viewModelScope = CoroutineScope(
        SupervisorJob() + Dispatchers.Main
    )

    fun loadUser(userId: String) {
        viewModelScope.launch {
            val user = fetchUser(userId)
            updateUI(user)
        }
    }

    override fun onCleared() {
        viewModelScope.cancel()  // Cancels all coroutines
    }
}

// ✅ Best - use structured concurrency
class UserViewModel {
    suspend fun loadUser(userId: String): User = coroutineScope {
        async { fetchUser(userId) }.await()
    }
}
```

**Context**: Never use GlobalScope except for truly application-wide operations. Use lifecycle-aware scopes.

### Anti-Pattern: Swallowing Exceptions in Coroutines

**Severity**: MAJOR

**Why Problematic**: Silent failures make debugging impossible.

```kotlin
// ❌ Bad - silent failure
suspend fun fetchData() {
    try {
        apiClient.fetch()
    } catch (e: Exception) {
        // Exception swallowed - no logging, no handling
    }
}

// ✅ Better - log and propagate
suspend fun fetchData(): Result<Data> {
    return try {
        Result.Success(apiClient.fetch())
    } catch (e: IOException) {
        logger.error("Network error", e)
        Result.Error("Network error: ${e.message}")
    } catch (e: Exception) {
        logger.error("Unexpected error", e)
        Result.Error("Unexpected error: ${e.message}")
    }
}

// ✅ Better - exception handler
val exceptionHandler = CoroutineExceptionHandler { _, exception ->
    logger.error("Coroutine failed", exception)
    showErrorToUser(exception.message)
}

viewModelScope.launch(exceptionHandler) {
    fetchData()
}
```

**Context**: Always log exceptions. Return Result types or use CoroutineExceptionHandler.

---

## Performance Anti-Patterns

### Anti-Pattern: Unnecessary Object Allocation

**Severity**: MAJOR

**Why Problematic**: Causes garbage collection pressure and performance degradation.

```kotlin
// ❌ Bad - allocates intermediate objects
fun processData(items: List<Item>): List<Result> {
    return items
        .map { transform(it) }     // Allocates list
        .filter { it.isValid }     // Allocates another list
        .map { convert(it) }       // Allocates another list
}

// ✅ Better - use sequence for lazy evaluation
fun processData(items: List<Item>): List<Result> {
    return items.asSequence()
        .map { transform(it) }     // No intermediate allocation
        .filter { it.isValid }     // No intermediate allocation
        .map { convert(it) }       // No intermediate allocation
        .toList()                  // Single final allocation
}

// ❌ Bad - string concatenation in loop
fun buildMessage(items: List<String>): String {
    var message = ""
    for (item in items) {
        message += item + "\n"  // Creates new string each iteration!
    }
    return message
}

// ✅ Better - use StringBuilder
fun buildMessage(items: List<String>): String {
    return buildString {
        items.forEach { append(it).append("\n") }
    }
}
```

**Context**: Use sequences for large collections, buildString for string concatenation.

### Anti-Pattern: Inefficient Collection Operations

**Severity**: MAJOR

**Why Problematic**: Quadratic time complexity instead of linear.

```kotlin
// ❌ Bad - O(n²) complexity
fun removeDuplicates(items: List<String>): List<String> {
    val result = mutableListOf<String>()
    for (item in items) {
        if (!result.contains(item)) {  // O(n) lookup each iteration!
            result.add(item)
        }
    }
    return result
}

// ✅ Better - O(n) with set
fun removeDuplicates(items: List<String>): List<String> {
    return items.toSet().toList()  // O(n) complexity
}

// ❌ Bad - repeated list traversal
fun processItems(items: List<Item>) {
    val valid = items.filter { it.isValid }
    val invalid = items.filter { !it.isValid }
    // Traverses list twice!
}

// ✅ Better - single traversal with partition
fun processItems(items: List<Item>) {
    val (valid, invalid) = items.partition { it.isValid }
    // Traverses list once!
}
```

**Context**: Choose appropriate data structures. Use partition instead of multiple filters.

## Related Resources

**Learn more**:

- [Best Practices](/en/learn/swe/prog-lang/kotlin/explanation/best-practices) - Idiomatic patterns
- [Beginner Tutorial](/en/learn/swe/prog-lang/kotlin/tutorials/beginner) - Fundamentals
- [Intermediate Tutorial](/en/learn/swe/prog-lang/kotlin/tutorials/intermediate) - Production patterns

**How-To Guides**:

- [Null Safety](/en/learn/swe/prog-lang/kotlin/how-to/working-with-nullable-types)
- [Coroutines](/en/learn/swe/prog-lang/kotlin/how-to/handle-coroutines-and-async)
- [Testing](/en/learn/swe/prog-lang/kotlin/how-to/testing-junit-mockk)
