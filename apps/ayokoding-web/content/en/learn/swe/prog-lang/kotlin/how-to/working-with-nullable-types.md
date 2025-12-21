---
title: "How to Work with Nullable Types"
date: 2025-12-18T00:00:00+07:00
draft: false
weight: 1000002
description: "Master Kotlin's nullable types and avoid null pointer exceptions"
tags: ["kotlin", "null-safety", "nullable-types", "type-system"]
categories: ["learn"]
---

## Problem

Null pointer exceptions are one of the most common runtime errors. Kotlin eliminates this problem at compile-time with nullable types, but you need to understand how to use them effectively.

This guide shows how to work with nullable types and leverage Kotlin's null safety features.

## Nullable Type Basics

### Declaring Nullable Types

In Kotlin, types are non-nullable by default. Add `?` to make them nullable.

```kotlin
// ✅ Non-nullable (default)
var name: String = "Alice"
// name = null  // ❌ Compilation error

// ✅ Nullable type
var optionalName: String? = "Alice"
optionalName = null  // ✅ Allowed

// ✅ Nullable types work with all types
val age: Int? = null
val user: User? = null
val list: List<String>? = null
```

**Key rule**: Use `?` after the type to make it nullable.

### Null Checks

Kotlin forces you to check for null before using nullable values.

```kotlin
fun printLength(text: String?) {
  // ❌ Compilation error - might be null
  // println(text.length)

  // ✅ Explicit null check
  if (text != null) {
    println(text.length)  // Smart cast to non-nullable
  }
}

// ✅ Early return pattern
fun processUser(user: User?) {
  if (user == null) {
    println("No user provided")
    return
  }
  // user is automatically non-nullable here
  println("User: ${user.name}")
}
```

**How it works**: After a null check, Kotlin smart casts the variable to non-nullable in the safe scope.

## Safe Call Operator (?.)

### Basic Usage

Use `?.` to safely access properties or call methods on nullable types.

```kotlin
val name: String? = getUsername()

// ❌ Compilation error
// val length = name.length

// ✅ Safe call - returns null if name is null
val length: Int? = name?.length

// ✅ Chain safe calls
val firstChar: Char? = name?.uppercase()?.firstOrNull()

// ✅ Safe calls with properties
data class User(val name: String, val address: Address?)
data class Address(val city: String)

val user: User? = getUser()
val city: String? = user?.address?.city
```

**When to use**: Accessing properties or calling methods on nullable types without explicit null checks.

### Safe Calls in Chains

Safe calls short-circuit - if any part is null, the entire expression becomes null.

```kotlin
// ✅ Graceful handling of null chains
fun getUserCity(userId: String): String? {
  return database
    .findUser(userId)
    ?.profile
    ?.address
    ?.city
}

// ✅ Safe calls with function calls
val uppercaseEmail: String? = user?.email?.trim()?.uppercase()
```

**Key benefit**: No NullPointerException, returns null instead.

## Elvis Operator (?:)

### Providing Default Values

Use `?:` to provide a default value when the left side is null.

```kotlin
// ✅ Simple default value
val name: String? = getUsername()
val displayName: String = name ?: "Guest"

// ✅ With safe calls
val city: String = user?.address?.city ?: "Unknown"

// ✅ Elvis with expressions
val value: Int = config.get("timeout")?.toIntOrNull() ?: 30

// ✅ Multiple fallbacks
val email = user?.email
  ?: user?.profile?.contactEmail
  ?: "no-email@example.com"
```

**Pattern**: `nullable ?: defaultValue` - returns defaultValue if nullable is null.

### Early Return with Elvis

Use Elvis with `return` or `throw` for validation.

```kotlin
// ✅ Early return
fun processUser(user: User?) {
  val validUser = user ?: return
  println("Processing: ${validUser.name}")
}

// ✅ Throw on null
fun requireUser(user: User?): User {
  return user ?: throw IllegalArgumentException("User cannot be null")
}

// ✅ Validation pattern
fun validateInput(input: String?): String {
  val trimmed = input?.trim() ?: throw ValidationException("Input required")
  if (trimmed.isEmpty()) throw ValidationException("Input cannot be empty")
  return trimmed
}
```

**Use case**: Guard clauses and validation logic.

## Not-Null Assertion (!!)

### When to Use

Use `!!` only when you're absolutely certain a value is not null.

```kotlin
// ❌ Bad practice - don't use without good reason
val name: String = getName()!!

// ✅ Reasonable use - after explicit check
val config: Config? = loadConfig()
if (config == null) {
  throw IllegalStateException("Config must be loaded")
}
val timeout: Int = config!!.timeout  // Safe because we checked

// ✅ Platform type conversion
val javaString: String? = javaLibrary.getString()
val kotlinString: String = javaString!!  // Known to not return null
```

**Warning**: `!!` throws NullPointerException if the value is null. Use sparingly.

### Better Alternatives

Prefer safe operators over not-null assertions.

```kotlin
// ❌ Risky
fun getLength(text: String?): Int {
  return text!!.length
}

// ✅ Better - safe call with default
fun getLength(text: String?): Int {
  return text?.length ?: 0
}

// ✅ Better - early return
fun getLength(text: String?): Int? {
  return text?.length
}

// ✅ Better - require check
fun getLength(text: String?): Int {
  requireNotNull(text) { "Text cannot be null" }
  return text.length
}
```

## Let Function with Nullable Types

### Safe Execution Blocks

Use `let` to execute code only when a value is not null.

```kotlin
// ✅ Execute block if not null
val name: String? = getUsername()
name?.let { validName ->
  println("Hello, $validName")
  database.updateLastSeen(validName)
}

// ✅ Avoid temporary variables
user?.let {
  println("User: ${it.name}")
  println("Email: ${it.email}")
}

// ✅ With Elvis for fallback
val result = fetchData()?.let { data ->
  process(data)
} ?: "No data available"
```

**Pattern**: `nullable?.let { ... }` - block executes only if nullable is not null.

### Avoiding Nested Null Checks

Replace nested if statements with let chains.

```kotlin
// ❌ Nested null checks
if (user != null) {
  val profile = user.profile
  if (profile != null) {
    val address = profile.address
    if (address != null) {
      println(address.city)
    }
  }
}

// ✅ Cleaner with safe calls and let
user?.profile?.address?.let { address ->
  println(address.city)
}
```

## Common Pitfalls

### Overusing Not-Null Assertions

```kotlin
// ❌ Defeats null safety
val user = findUser()!!
val name = user.name!!
val email = user.email!!

// ✅ Use safe operators
val user = findUser()
user?.let {
  val name = it.name ?: "Unknown"
  val email = it.email ?: "no-email@example.com"
}
```

**Why problematic**: Converts null safety to runtime exceptions, defeating Kotlin's design.

### Unnecessary Null Checks

```kotlin
// ❌ Redundant checks
var name: String = "Alice"
if (name != null) {  // Unnecessary - String is non-nullable
  println(name.length)
}

// ✅ Use non-nullable types directly
val name: String = "Alice"
println(name.length)
```

**Why problematic**: Non-nullable types never need null checks.

### Ignoring Nullability in Collections

```kotlin
// ❌ Ignoring nulls in list
val names: List<String?> = listOf("Alice", null, "Bob")
names.forEach { name ->
  println(name.length)  // ❌ Compilation error
}

// ✅ Filter out nulls
names.filterNotNull().forEach { name ->
  println(name.length)  // ✅ name is non-nullable
}

// ✅ Handle nulls explicitly
names.forEach { name ->
  name?.let { println(it.length) }
}
```

## Variations

### requireNotNull Function

Use `requireNotNull` for precondition checks.

```kotlin
// ✅ With error message
fun processOrder(order: Order?) {
  val validOrder = requireNotNull(order) { "Order cannot be null" }
  println("Processing order: ${validOrder.id}")
}

// ✅ With property
class OrderProcessor(order: Order?) {
  private val order: Order = requireNotNull(order)
}
```

### checkNotNull Function

Similar to requireNotNull, but throws IllegalStateException.

```kotlin
// ✅ State validation
class Service {
  private var connection: Connection? = null

  fun execute() {
    val conn = checkNotNull(connection) { "Service not initialized" }
    conn.query("SELECT * FROM users")
  }
}
```

### Platform Types (Java Interop)

When calling Java code, Kotlin infers platform types (Type!).

```kotlin
// Java code returns String (possibly null)
val javaString = javaApi.getString()  // Type: String!

// ✅ Treat as nullable
val safeString: String? = javaString
safeString?.let { println(it) }

// ✅ Or assert non-null if guaranteed
val definitelyNotNull: String = javaString  // Runtime check
```

## Related Patterns

**Learn more**:

- [Smart Casts](/en/learn/swe/prog-lang/kotlin/how-to/using-smart-casts) - Automatic type casting after checks
- [Elvis and Safe Call](/en/learn/swe/prog-lang/kotlin/how-to/avoid-null-pointer-errors) - Advanced null handling patterns
- [Beginner Tutorial - Type System](/en/learn/swe/prog-lang/kotlin/tutorials/beginner#type-system-and-variables) - Fundamentals of Kotlin's type system
- [Quick Start - Null Safety](/en/learn/swe/prog-lang/kotlin/tutorials/quick-start#null-safety) - Introduction to null safety

**Cookbook recipes**:

- [Nullable and Optional Values](/en/learn/swe/prog-lang/kotlin/how-to/cookbook#nullable-optional) - Quick reference patterns
- [Result Type for Errors](/en/learn/swe/prog-lang/kotlin/how-to/cookbook#result-type) - Functional error handling
