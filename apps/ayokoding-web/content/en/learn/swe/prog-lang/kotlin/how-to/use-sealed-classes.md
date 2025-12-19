---
title: "How to Use Sealed Classes and Interfaces"
date: 2025-12-18T00:00:00+07:00
draft: false
weight: 612
description: "Create type-safe restricted hierarchies with sealed classes and interfaces"
tags: ["kotlin", "sealed-classes", "type-safety", "kotlin-features"]
---

## Problem

Traditional class hierarchies allow any class to extend your base class, making exhaustive `when` checks impossible. You need restricted hierarchies where all subclasses are known at compile-time. Kotlin's sealed classes and interfaces provide this guarantee.

This guide shows how to use sealed types for type-safe state modeling.

## Sealed Class Basics

### Creating Sealed Classes

Define sealed classes for restricted hierarchies.

```kotlin
// ✅ Sealed class with known subclasses
sealed class Result {
  data class Success(val data: String) : Result()
  data class Error(val message: String) : Result()
  data object Loading : Result()
}

// ✅ Exhaustive when expression
fun handleResult(result: Result): String {
  return when (result) {
    is Result.Success -> "Data: ${result.data}"
    is Result.Error -> "Error: ${result.message}"
    Result.Loading -> "Loading..."
  // No else needed - compiler knows all cases covered
  }
}

// ✅ Usage
val success = Result.Success("User data")
val error = Result.Error("Network failure")
println(handleResult(success))  // Data: User data
println(handleResult(error))    // Error: Network failure
```

**How it works**: `sealed` keyword restricts subclasses to same package/module.

### Sealed Class Benefits

Compiler enforces exhaustiveness in when expressions.

```kotlin
sealed class Payment {
  data class Cash(val amount: Double) : Payment()
  data class CreditCard(val number: String, val amount: Double) : Payment()
  data class BankTransfer(val account: String, val amount: Double) : Payment()
}

// ✅ Compiler error if case missing
fun processPayment(payment: Payment) {
  when (payment) {
    is Payment.Cash -> println("Cash: ${payment.amount}")
    is Payment.CreditCard -> println("Card: ${payment.number}")
    // ❌ Missing BankTransfer case - compilation error!
  }
}

// ✅ Complete coverage
fun processPaymentComplete(payment: Payment) {
  when (payment) {
    is Payment.Cash -> println("Cash: ${payment.amount}")
    is Payment.CreditCard -> println("Card: ${payment.number}")
    is Payment.BankTransfer -> println("Transfer: ${payment.account}")
  // ✅ All cases handled
  }
}
```

**Key benefit**: Refactoring safety - adding new subclass causes compilation errors at all when sites.

## Sealed Interfaces

### Creating Sealed Interfaces

Sealed interfaces provide multiple inheritance.

```kotlin
// ✅ Sealed interface
sealed interface Action

// ✅ Multiple implementations
data class Click(val x: Int, val y: Int) : Action
data class Swipe(val direction: String) : Action
data object Refresh : Action

// ✅ Class implementing sealed interface
class CustomAction(val name: String) : Action

// ✅ Exhaustive when
fun handleAction(action: Action) {
  when (action) {
    is Click -> println("Clicked at (${action.x}, ${action.y})")
    is Swipe -> println("Swiped ${action.direction}")
    Refresh -> println("Refreshing...")
    is CustomAction -> println("Custom: ${action.name}")
  }
}
```

**Use case**: When you need multiple inheritance or want to seal an interface.

### Sealed Interface vs Sealed Class

```kotlin
// ✅ Sealed class - single inheritance
sealed class Animal {
  data class Dog(val breed: String) : Animal()
  data class Cat(val color: String) : Animal()
}

// ✅ Sealed interface - multiple inheritance
sealed interface Drawable
sealed interface Clickable

data class Button(val label: String) : Drawable, Clickable
data class Icon(val image: String) : Drawable

fun render(drawable: Drawable) {
  when (drawable) {
    is Button -> println("Button: ${drawable.label}")
    is Icon -> println("Icon: ${drawable.image}")
  }
}
```

**Guideline**: Use sealed class for single inheritance, sealed interface for multiple.

## State Modeling

### UI State Pattern

Model UI states with sealed classes.

```kotlin
// ✅ UI state sealed class
sealed class UiState<out T> {
  data object Idle : UiState<Nothing>()
  data object Loading : UiState<Nothing>()
  data class Success<T>(val data: T) : UiState<T>()
  data class Error(val message: String, val cause: Throwable? = null) : UiState<Nothing>()
}

// ✅ Handle UI states
fun <T> renderUi(state: UiState<T>) {
  when (state) {
    UiState.Idle -> showPlaceholder()
    UiState.Loading -> showSpinner()
    is UiState.Success -> showData(state.data)
    is UiState.Error -> showError(state.message)
  }
}

fun showPlaceholder() = println("Idle state")
fun showSpinner() = println("Loading...")
fun <T> showData(data: T) = println("Data: $data")
fun showError(message: String) = println("Error: $message")

// ✅ Usage
val state: UiState<List<String>> = UiState.Success(listOf("Item1", "Item2"))
renderUi(state)  // Data: [Item1, Item2]
```

**Pattern**: Generic sealed class for reusable state modeling.

### Navigation State

Model navigation with sealed classes.

```kotlin
// ✅ Navigation sealed class
sealed class Screen {
  data object Home : Screen()
  data class Profile(val userId: String) : Screen()
  data class Details(val itemId: Int) : Screen()
  data class Settings(val section: String? = null) : Screen()
}

// ✅ Navigation handling
fun navigate(screen: Screen) {
  when (screen) {
    Screen.Home -> showHome()
    is Screen.Profile -> showProfile(screen.userId)
    is Screen.Details -> showDetails(screen.itemId)
    is Screen.Settings -> showSettings(screen.section)
  }
}

fun showHome() = println("Home Screen")
fun showProfile(userId: String) = println("Profile: $userId")
fun showDetails(itemId: Int) = println("Details: $itemId")
fun showSettings(section: String?) = println("Settings: ${section ?: "main"}")

// ✅ Type-safe navigation
navigate(Screen.Home)
navigate(Screen.Profile("user123"))
navigate(Screen.Settings("notifications"))
```

## Sealed Classes with Generics

### Generic Sealed Hierarchies

Create reusable sealed class patterns.

```kotlin
// ✅ Generic Result type
sealed class ApiResult<out T> {
  data class Success<T>(val data: T) : ApiResult<T>()
  data class Error(val code: Int, val message: String) : ApiResult<Nothing>()
  data object NetworkError : ApiResult<Nothing>()
}

// ✅ Type-safe handling
fun <T> handleApiResult(result: ApiResult<T>): T? {
  return when (result) {
    is ApiResult.Success -> result.data
    is ApiResult.Error -> {
      println("Error ${result.code}: ${result.message}")
      null
    }
    ApiResult.NetworkError -> {
      println("Network error occurred")
      null
    }
  }
}

// ✅ Usage with different types
val userResult: ApiResult<User> = ApiResult.Success(User("1", "Alice"))
val user: User? = handleApiResult(userResult)

val listResult: ApiResult<List<String>> = ApiResult.Error(404, "Not found")
val items: List<String>? = handleApiResult(listResult)
```

**Use case**: Generic error handling across different data types.

### Variance in Sealed Classes

Use variance for flexible sealed hierarchies.

```kotlin
// ✅ Covariant sealed class (out T)
sealed class Response<out T> {
  data class Data<T>(val value: T) : Response<T>()
  data object Empty : Response<Nothing>()
}

// ✅ Can assign Response<String> to Response<Any>
val stringResponse: Response<String> = Response.Data("Hello")
val anyResponse: Response<Any> = stringResponse  // ✅ Covariance allows this
```

## Nested Sealed Hierarchies

### Hierarchical State Modeling

Create nested sealed class structures.

```kotlin
// ✅ Nested sealed classes
sealed class AuthState {
  data object Unauthenticated : AuthState()

  sealed class Authenticated : AuthState() {
    data class User(val id: String, val name: String) : Authenticated()
    data class Admin(val id: String, val name: String, val permissions: List<String>) : Authenticated()
    data class Guest(val sessionId: String) : Authenticated()
  }

  sealed class Error : AuthState() {
    data object InvalidCredentials : Error()
    data object NetworkError : Error()
    data class ServerError(val code: Int) : Error()
  }
}

// ✅ Handle nested hierarchies
fun handleAuth(state: AuthState) {
  when (state) {
    AuthState.Unauthenticated -> println("Please log in")
    is AuthState.Authenticated.User -> println("User: ${state.name}")
    is AuthState.Authenticated.Admin -> println("Admin: ${state.name}")
    is AuthState.Authenticated.Guest -> println("Guest: ${state.sessionId}")
    AuthState.Error.InvalidCredentials -> println("Invalid credentials")
    AuthState.Error.NetworkError -> println("Network error")
    is AuthState.Error.ServerError -> println("Server error: ${state.code}")
  }
}
```

**Pattern**: Group related states with nested sealed classes.

## Sealed Classes vs Enums

### When to Use Each

```kotlin
// ✅ Enum - fixed set of constants, no data
enum class Color {
  RED, GREEN, BLUE
}

// ✅ Sealed class - fixed set with associated data
sealed class HttpResponse {
  data class Ok(val body: String) : HttpResponse()
  data class NotFound(val path: String) : HttpResponse()
  data class ServerError(val code: Int, val message: String) : HttpResponse()
}

// ❌ Can't add data to enum
enum class Status {
  SUCCESS,  // Can't attach result data
  ERROR     // Can't attach error details
}

// ✅ Sealed class for data-carrying states
sealed class Status {
  data class Success(val data: String) : Status()
  data class Error(val message: String) : Status()
}
```

**Guideline**:

- Use enums for simple constants
- Use sealed classes when you need associated data

## Common Pitfalls

### Forgetting data object for Singleton Cases

```kotlin
// ❌ Using class for singleton case
sealed class State {
  class Loading : State()  // New instance each time!
}

val s1 = State.Loading()
val s2 = State.Loading()
println(s1 === s2)  // false - different instances

// ✅ Use data object for singleton
sealed class State {
  data object Loading : State()
}

println(State.Loading === State.Loading)  // true - same instance
```

**Why problematic**: Creates unnecessary object instances.

### Not Using Exhaustive when

```kotlin
// ❌ Using else instead of exhaustive when
sealed class Result {
  data class Success(val data: String) : Result()
  data class Error(val message: String) : Result()
}

fun handle(result: Result) {
  when (result) {
    is Result.Success -> println(result.data)
    else -> println("Error")  // ❌ Loses type safety
  }
}

// ✅ Exhaustive when
fun handleBetter(result: Result) {
  when (result) {
    is Result.Success -> println(result.data)
    is Result.Error -> println(result.message)  // ✅ Explicit handling
  }
}
```

**Why problematic**: `else` defeats exhaustiveness checking.

### Sealed Class Visibility Issues

```kotlin
// ❌ Subclasses must be in same package/module
// File: api/Result.kt
sealed class Result

// File: internal/Success.kt (different package)
// class Success : Result()  // ❌ Compilation error
```

**Rule**: All subclasses must be declared in same file or same package.

## Variations

### Sealed Classes with Interfaces

Combine sealed classes with interfaces.

```kotlin
interface Loggable {
  fun log(): String
}

// ✅ Sealed class implementing interface
sealed class Operation : Loggable {
  data class Add(val a: Int, val b: Int) : Operation() {
    override fun log() = "Adding $a + $b"
  }

  data class Multiply(val a: Int, val b: Int) : Operation() {
    override fun log() = "Multiplying $a * $b"
  }
}

fun execute(op: Operation) {
  println(op.log())
  when (op) {
    is Operation.Add -> println("Result: ${op.a + op.b}")
    is Operation.Multiply -> println("Result: ${op.a * op.b}")
  }
}
```

### Sealed Classes for DSL

Use sealed classes in domain-specific languages.

```kotlin
// ✅ DSL with sealed classes
sealed class SqlQuery {
  data class Select(val columns: List<String>, val from: String) : SqlQuery()
  data class Insert(val into: String, val values: Map<String, Any>) : SqlQuery()
  data class Update(val table: String, val set: Map<String, Any>, val where: String) : SqlQuery()
}

fun buildSql(query: SqlQuery): String {
  return when (query) {
    is SqlQuery.Select ->
      "SELECT ${query.columns.joinToString()} FROM ${query.from}"
    is SqlQuery.Insert ->
      "INSERT INTO ${query.into} ..."
    is SqlQuery.Update ->
      "UPDATE ${query.table} SET ..."
  }
}
```

## Related Patterns

**Learn more**:

- [Data Classes](/en/learn/swe/prog-lang/kotlin/how-to/use-data-classes-effectively) - Often used as sealed class members
- [Smart Casts](/en/learn/swe/prog-lang/kotlin/how-to/using-smart-casts) - Automatic casting in when expressions
- [Beginner Tutorial - Sealed Classes](/en/learn/swe/prog-lang/kotlin/tutorials/beginner#sealed-classes) - Sealed class fundamentals
- [Intermediate Tutorial - Design Patterns](/en/learn/swe/prog-lang/kotlin/tutorials/intermediate#design-patterns) - State pattern with sealed classes

**Cookbook recipes**:

- [Sealed Data Structures](/en/learn/swe/prog-lang/kotlin/how-to/cookbook#sealed-data-structures) - Custom data structures with sealed classes
- [Observer Pattern with Flow](/en/learn/swe/prog-lang/kotlin/how-to/cookbook#observer-flow) - Sealed classes for events
