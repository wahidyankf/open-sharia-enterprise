---
title: "Use Delegates"
date: 2025-12-18T14:50:04+07:00
draft: false
weight: 1000011
description: "Master Kotlin property delegation with lazy, observable, vetoable, and custom delegates"
tags: ["kotlin", "delegates", "lazy-initialization", "observable-properties"]
categories: ["learn"]
---

## Problem

Implementing common property patterns like lazy initialization, observable properties, or validation requires boilerplate code. Kotlin's property delegation mechanism allows you to extract these patterns into reusable delegates, reducing code duplication and improving maintainability.

This guide shows how to use built-in delegates and create custom ones.

## Lazy Delegate

### Basic Lazy Initialization

Initialize properties on first access using `lazy`.

```kotlin
// ✅ Lazy initialization
class DatabaseService {
  val connection: Connection by lazy {
    println("Establishing connection...")
    Database.connect()  // Expensive operation
  }
}

val service = DatabaseService()
println("Service created")  // Connection NOT initialized yet
println(service.connection)  // NOW connection is initialized
println(service.connection)  // Uses cached value
```

**Output**:

```
Service created
Establishing connection...
Connection@12345
Connection@12345
```

**How it works**: Lambda executes on first access, result cached for subsequent accesses.

### Lazy Thread Safety Modes

Control thread safety behavior of lazy initialization.

```kotlin
// ✅ SYNCHRONIZED (default) - thread-safe with lock
val heavyResource: Resource by lazy {
  Resource.load()
}

// ✅ PUBLICATION - multiple threads may initialize, first wins
val cachedData: Data by lazy(LazyThreadSafetyMode.PUBLICATION) {
  Data.fetch()
}

// ✅ NONE - no thread safety, fastest
val singleThreadedCache: Cache by lazy(LazyThreadSafetyMode.NONE) {
  Cache.create()  // Only use in single-threaded contexts
}
```

**Thread safety modes**:

- `SYNCHRONIZED` (default): Thread-safe with double-check locking
- `PUBLICATION`: Multiple initializations possible, first result wins
- `NONE`: No synchronization, fastest but not thread-safe

### Lazy with Dependencies

Lazy properties can depend on other properties.

```kotlin
// ✅ Chained lazy initialization
class Configuration {
  val environment: String by lazy {
    System.getenv("ENV") ?: "development"
  }

  val databaseUrl: String by lazy {
    when (environment) {
      "production" -> "jdbc:postgresql://prod-db/app"
      "staging" -> "jdbc:postgresql://stage-db/app"
      else -> "jdbc:h2:mem:test"
    }
  }

  val connection: Connection by lazy {
    DriverManager.getConnection(databaseUrl)
  }
}

val config = Configuration()
// All properties initialized on first access in correct order
println(config.connection)
```

**Pattern**: Dependencies resolved automatically during initialization.

## Observable Delegate

### Tracking Property Changes

React to property changes with `observable`.

```kotlin
// ✅ Observable property
import kotlin.properties.Delegates

class User {
  var name: String by Delegates.observable("") { property, oldValue, newValue ->
    println("${property.name} changed from '$oldValue' to '$newValue'")
  }
}

val user = User()
user.name = "Alice"  // Prints: name changed from '' to 'Alice'
user.name = "Bob"    // Prints: name changed from 'Alice' to 'Bob'
```

**Use case**: Logging, UI updates, validation triggers.

### Multiple Observers

Implement multi-cast observers for complex scenarios.

```kotlin
// ✅ Multiple observers
class ViewModel {
  private val listeners = mutableListOf<(String) -> Unit>()

  var status: String by Delegates.observable("idle") { _, _, newValue ->
    listeners.forEach { it(newValue) }
  }

  fun addListener(listener: (String) -> Unit) {
    listeners.add(listener)
  }
}

val viewModel = ViewModel()
viewModel.addListener { status -> println("UI: Status is $status") }
viewModel.addListener { status -> println("Log: Status changed to $status") }

viewModel.status = "loading"
// UI: Status is loading
// Log: Status changed to loading
```

### Observable with State Management

Use observable for reactive state management.

```kotlin
// ✅ State management with observable
data class AppState(
  val isLoading: Boolean = false,
  val error: String? = null,
  val data: List<String> = emptyList()
)

class Store {
  var state: AppState by Delegates.observable(AppState()) { _, oldState, newState ->
    if (oldState != newState) {
      notifySubscribers(newState)
    }
  }

  private val subscribers = mutableListOf<(AppState) -> Unit>()

  fun subscribe(subscriber: (AppState) -> Unit) {
    subscribers.add(subscriber)
  }

  private fun notifySubscribers(state: AppState) {
    subscribers.forEach { it(state) }
  }
}

val store = Store()
store.subscribe { state -> println("State updated: $state") }
store.state = store.state.copy(isLoading = true)
```

## Vetoable Delegate

### Validation Before Assignment

Use `vetoable` to validate or reject property changes.

```kotlin
// ✅ Vetoable property
import kotlin.properties.Delegates

class Account {
  var balance: Double by Delegates.vetoable(0.0) { _, _, newValue ->
    newValue >= 0.0  // Only allow non-negative balance
  }
}

val account = Account()
account.balance = 100.0  // ✅ Accepted
println(account.balance)  // 100.0

account.balance = -50.0   // ❌ Rejected
println(account.balance)  // 100.0 (unchanged)
```

**How it works**: Lambda returns `true` to accept, `false` to reject change.

### Complex Validation

Implement multi-condition validation.

```kotlin
// ✅ Complex validation with vetoable
class User {
  var email: String by Delegates.vetoable("") { property, oldValue, newValue ->
    when {
      newValue.isBlank() -> {
        println("Email cannot be blank")
        false
      }
      !newValue.contains("@") -> {
        println("Invalid email format")
        false
      }
      newValue.length > 100 -> {
        println("Email too long")
        false
      }
      else -> true
    }
  }
}

val user = User()
user.email = "alice@example.com"  // ✅ Accepted
user.email = "invalid"            // ❌ Rejected: Invalid email format
user.email = ""                   // ❌ Rejected: Email cannot be blank
```

### Vetoable with Side Effects

Combine validation with logging or notifications.

```kotlin
// ✅ Vetoable with side effects
class Temperature {
  var celsius: Double by Delegates.vetoable(20.0) { _, oldValue, newValue ->
    val isValid = newValue in -273.15..1000.0  // Physical limits

    if (!isValid) {
      logger.warn("Invalid temperature: $newValue°C")
    } else if (newValue > 30.0) {
      logger.info("High temperature alert: $newValue°C")
    }

    isValid
  }
}
```

## Map Delegate

### Storing Properties in Map

Use map as property delegate for dynamic property storage.

```kotlin
// ✅ Properties backed by map
class User(val map: Map<String, Any?>) {
  val name: String by map
  val age: Int by map
  val email: String? by map
}

val user = User(mapOf(
  "name" to "Alice",
  "age" to 30,
  "email" to "alice@example.com"
))

println(user.name)   // Alice
println(user.age)    // 30
println(user.email)  // alice@example.com
```

**Use case**: JSON parsing, configuration files, dynamic data.

### Mutable Map Delegate

Use mutable map for read-write properties.

```kotlin
// ✅ Mutable properties backed by mutable map
class MutableUser(val map: MutableMap<String, Any?>) {
  var name: String by map
  var age: Int by map
  var email: String? by map
}

val user = MutableUser(mutableMapOf(
  "name" to "Bob",
  "age" to 25
))

println(user.email)  // null
user.email = "bob@example.com"  // Modifies backing map
println(user.email)  // bob@example.com
println(user.map)    // {name=Bob, age=25, email=bob@example.com}
```

### Default Values with Map

Provide defaults for missing map keys.

```kotlin
// ✅ Map with defaults
class Config(private val map: Map<String, Any?>) {
  val timeout: Int by map.withDefault { 30 }
  val retries: Int by map.withDefault { 3 }
  val debug: Boolean by map.withDefault { false }
}

val config = Config(mapOf("timeout" to 60))
println(config.timeout)  // 60 (from map)
println(config.retries)  // 3 (default)
println(config.debug)    // false (default)
```

## Custom Delegates

### Implementing ReadOnlyProperty

Create read-only delegate implementing `ReadOnlyProperty` interface.

```kotlin
// ✅ Custom read-only delegate
import kotlin.reflect.KProperty

class Uppercase {
  operator fun getValue(thisRef: Any?, property: KProperty<*>): String {
    return property.name.uppercase()
  }
}

class Example {
  val greeting: String by Uppercase()
  val message: String by Uppercase()
}

val example = Example()
println(example.greeting)  // GREETING
println(example.message)   // MESSAGE
```

**Interface**: `getValue(thisRef: Any?, property: KProperty<*>): T`

### Implementing ReadWriteProperty

Create read-write delegate implementing `ReadWriteProperty` interface.

```kotlin
// ✅ Custom read-write delegate
import kotlin.properties.ReadWriteProperty
import kotlin.reflect.KProperty

class Trimmed : ReadWriteProperty<Any?, String> {
  private var value: String = ""

  override fun getValue(thisRef: Any?, property: KProperty<*>): String {
    return value
  }

  override fun setValue(thisRef: Any?, property: KProperty<*>, value: String) {
    this.value = value.trim()
  }
}

class Form {
  var username: String by Trimmed()
  var email: String by Trimmed()
}

val form = Form()
form.username = "  alice  "
form.email = "  alice@example.com  "
println("'${form.username}'")  // 'alice'
println("'${form.email}'")     // 'alice@example.com'
```

### Parameterized Delegates

Create delegates that accept parameters.

```kotlin
// ✅ Delegate with parameters
class RangeValidator(
  private val min: Int,
  private val max: Int
) : ReadWriteProperty<Any?, Int> {
  private var value: Int = min

  override fun getValue(thisRef: Any?, property: KProperty<*>): Int {
    return value
  }

  override fun setValue(thisRef: Any?, property: KProperty<*>, value: Int) {
    require(value in min..max) {
      "${property.name} must be between $min and $max, got $value"
    }
    this.value = value
  }
}

class Player {
  var health: Int by RangeValidator(0, 100)
  var level: Int by RangeValidator(1, 50)
}

val player = Player()
player.health = 75   // ✅ Valid
player.level = 10    // ✅ Valid
// player.health = 150  // ❌ Throws exception
```

### Delegate Factory Functions

Create factory functions for common delegate patterns.

```kotlin
// ✅ Delegate factory function
fun <T> cached(initializer: () -> T) = lazy(initializer)

fun <T> validated(
  initial: T,
  validator: (T) -> Boolean
): ReadWriteProperty<Any?, T> {
  return object : ReadWriteProperty<Any?, T> {
    private var value: T = initial

    override fun getValue(thisRef: Any?, property: KProperty<*>): T = value

    override fun setValue(thisRef: Any?, property: KProperty<*>, value: T) {
      require(validator(value)) { "Validation failed for ${property.name}" }
      this.value = value
    }
  }
}

class Config {
  val apiUrl: String by cached { "https://api.example.com" }
  var timeout: Int by validated(30) { it in 1..300 }
}
```

## Advanced Patterns

### Contextual Delegates

Delegates that use containing class context.

```kotlin
// ✅ Delegate with access to containing class
class LoggedProperty<T>(private val initialValue: T) : ReadWriteProperty<Any, T> {
  private var value: T = initialValue

  override fun getValue(thisRef: Any, property: KProperty<*>): T {
    println("[${thisRef::class.simpleName}] Reading ${property.name}: $value")
    return value
  }

  override fun setValue(thisRef: Any, property: KProperty<*>, value: T) {
    println("[${thisRef::class.simpleName}] Setting ${property.name}: $value")
    this.value = value
  }
}

class User {
  var name: String by LoggedProperty("Unknown")
}

val user = User()
user.name = "Alice"
// [User] Setting name: Alice
println(user.name)
// [User] Reading name: Alice
```

### Composed Delegates

Combine multiple delegate behaviors.

```kotlin
// ✅ Composed delegate
class ValidatedObservable<T>(
  initialValue: T,
  private val validator: (T) -> Boolean,
  private val observer: (T, T) -> Unit
) : ReadWriteProperty<Any?, T> {
  private var value: T = initialValue

  override fun getValue(thisRef: Any?, property: KProperty<*>): T = value

  override fun setValue(thisRef: Any?, property: KProperty<*>, newValue: T) {
    require(validator(newValue)) { "Validation failed" }
    val oldValue = value
    value = newValue
    observer(oldValue, newValue)
  }
}

fun <T> validatedObservable(
  initialValue: T,
  validator: (T) -> Boolean,
  observer: (T, T) -> Unit
) = ValidatedObservable(initialValue, validator, observer)

class Account {
  var balance: Double by validatedObservable(
    initialValue = 0.0,
    validator = { it >= 0.0 },
    observer = { old, new -> println("Balance changed: $old → $new") }
  )
}
```

### Delegate Providers

Create delegates dynamically using `provideDelegate`.

```kotlin
// ✅ Delegate provider
import kotlin.properties.PropertyDelegateProvider
import kotlin.properties.ReadWriteProperty
import kotlin.reflect.KProperty

class LogLevel(private val level: String) {
  operator fun provideDelegate(
    thisRef: Any?,
    property: KProperty<*>
  ): ReadWriteProperty<Any?, String> {
    println("Creating delegate for ${property.name} with level $level")

    return object : ReadWriteProperty<Any?, String> {
      private var value: String = ""

      override fun getValue(thisRef: Any?, property: KProperty<*>): String {
        return value
      }

      override fun setValue(thisRef: Any?, property: KProperty<*>, value: String) {
        println("[$level] ${property.name} = $value")
        this.value = value
      }
    }
  }
}

class Logger {
  var info: String by LogLevel("INFO")
  var error: String by LogLevel("ERROR")
}

// Output when class is initialized:
// Creating delegate for info with level INFO
// Creating delegate for error with level ERROR
```

## Common Pitfalls

### Thread Safety with Lazy

```kotlin
// ❌ NONE mode in multi-threaded context
class Cache {
  val data: Data by lazy(LazyThreadSafetyMode.NONE) {
    Data.load()  // ❌ Not thread-safe if accessed concurrently
  }
}

// ✅ Use SYNCHRONIZED (default) for multi-threaded
class Cache {
  val data: Data by lazy {  // SYNCHRONIZED mode by default
    Data.load()
  }
}
```

**Why problematic**: NONE mode can cause race conditions in concurrent scenarios.

### Observable Not Called on Same Value

```kotlin
// ❌ Observable only fires when value changes
var count: Int by Delegates.observable(0) { _, old, new ->
  println("Changed: $old → $new")
}

count = 5  // Prints: Changed: 0 → 5
count = 5  // Does NOT print (same value)
```

**Workaround**: Use custom delegate if you need to observe all assignments.

### Map Delegate Type Mismatch

```kotlin
// ❌ Runtime exception on type mismatch
class Config(map: Map<String, Any?>) {
  val timeout: Int by map
}

val config = Config(mapOf("timeout" to "30"))  // String, not Int
// ❌ Throws ClassCastException at runtime
```

**Solution**: Validate map contents or use type-safe parsing.

### Forgetting Delegation Syntax

```kotlin
// ❌ Missing 'by' keyword
class User {
  val name: String = Delegates.observable("")  // Wrong!
}

// ✅ Correct delegation syntax
class User {
  var name: String by Delegates.observable("")
}
```

## Variations

### Delegate for Dependency Injection

Use delegates for service location.

```kotlin
// ✅ Delegate-based dependency injection
object ServiceLocator {
  private val services = mutableMapOf<String, Any>()

  fun <T> register(name: String, service: T) {
    services[name] = service as Any
  }

  operator fun <T> getValue(thisRef: Any?, property: KProperty<*>): T {
    @Suppress("UNCHECKED_CAST")
    return services[property.name] as T
  }
}

class MyApp {
  val database: Database by ServiceLocator
  val apiClient: ApiClient by ServiceLocator
}

// Setup
ServiceLocator.register("database", DatabaseImpl())
ServiceLocator.register("apiClient", ApiClientImpl())

val app = MyApp()
```

### Reset-able Lazy

Create lazy delegate that can be reset.

```kotlin
// ✅ Reset-able lazy delegate
class ResettableLazy<T>(private val initializer: () -> T) : ReadWriteProperty<Any?, T> {
  private var value: T? = null

  override fun getValue(thisRef: Any?, property: KProperty<*>): T {
    return value ?: initializer().also { value = it }
  }

  override fun setValue(thisRef: Any?, property: KProperty<*>, value: T) {
    this.value = null  // Reset on any assignment
  }

  fun reset() {
    value = null
  }
}

fun <T> resettableLazy(initializer: () -> T) = ResettableLazy(initializer)

class Cache {
  var data: List<String> by resettableLazy {
    println("Loading data...")
    listOf("Item1", "Item2", "Item3")
  }
}

val cache = Cache()
println(cache.data)  // Loading data... [Item1, Item2, Item3]
cache.data = listOf()  // Reset
println(cache.data)  // Loading data... [Item1, Item2, Item3]
```

## Related Patterns

**Learn more**:

- [Beginner Tutorial - Delegates](/en/learn/software-engineering/programming-language/kotlin/tutorials/beginner#delegation) - Delegation basics
- [Intermediate Tutorial - Advanced Delegates](/en/learn/software-engineering/programming-language/kotlin/tutorials/intermediate#property-delegation) - Complex patterns
- [Custom Delegates](/en/learn/software-engineering/programming-language/kotlin/tutorials/advanced#metaprogramming) - Advanced delegate creation

**Cookbook recipes**:

- [Property Delegation](/en/learn/software-engineering/programming-language/kotlin/how-to/cookbook#property-delegation) - Quick reference
- [Observable Patterns](/en/learn/software-engineering/programming-language/kotlin/how-to/cookbook#observable-properties) - Reactive programming
- [Validation Patterns](/en/learn/software-engineering/programming-language/kotlin/how-to/cookbook#validation) - Input validation
