---
title: "How to Handle Null with Elvis and Safe Call"
date: 2025-12-18T00:00:00+07:00
draft: false
weight: 1000003
description: "Advanced null handling patterns with Elvis (?:) and safe call (?.) operators"
tags: ["kotlin", "null-safety", "elvis-operator", "safe-call"]
categories: ["learn"]
---

## Problem

While Kotlin's null safety prevents NullPointerException, you still need elegant ways to handle null values in real applications. Explicit null checks can become verbose. Elvis and safe call operators provide concise, expressive patterns for null handling.

This guide shows advanced techniques for handling nulls with Kotlin's operators.

## Safe Call Operator (?.) Patterns

### Chaining Safe Calls

Chain multiple safe calls to navigate nullable object graphs.

```kotlin
data class Address(val street: String, val city: String, val country: String)
data class Profile(val name: String, val address: Address?)
data class User(val id: String, val profile: Profile?)

// ✅ Graceful null handling in chains
fun getUserCountry(user: User?): String? {
  return user?.profile?.address?.country
}

// ✅ If any part is null, returns null
val user1 = User("1", Profile("Alice", Address("Main St", "NYC", "USA")))
println(getUserCountry(user1))  // "USA"

val user2 = User("2", Profile("Bob", null))
println(getUserCountry(user2))  // null

val user3 = User("3", null)
println(getUserCountry(user3))  // null
```

**How it works**: Safe call chain short-circuits at first null, returning null immediately.

### Safe Calls with Functions

Apply safe calls to method invocations.

```kotlin
// ✅ Safe method calls
val text: String? = getUserInput()
val uppercase: String? = text?.trim()?.uppercase()

// ✅ With multiple operations
fun processEmail(email: String?): String? {
  return email
    ?.trim()
    ?.lowercase()
    ?.takeIf { it.contains("@") }
}

// ✅ Safe calls prevent errors
processEmail("  USER@EXAMPLE.COM  ")  // "user@example.com"
processEmail("invalid-email")         // null
processEmail(null)                    // null
```

**Pattern**: Chain safe calls with transformations for null-safe pipelines.

### Safe Calls with Collections

Use safe calls on nullable collections.

```kotlin
// ✅ Nullable collection access
val items: List<String>? = getItems()
val firstItem: String? = items?.firstOrNull()
val itemCount: Int? = items?.size

// ✅ Safe iteration
items?.forEach { item ->
  println(item)
}

// ✅ Map operations
val uppercased: List<String>? = items?.map { it.uppercase() }

// ✅ Filter operations
val filtered: List<String>? = items?.filter { it.startsWith("A") }
```

**Key benefit**: Avoid null checks before collection operations.

## Elvis Operator (?:) Patterns

### Simple Default Values

Provide fallback values with Elvis operator.

```kotlin
// ✅ Basic default
val name: String? = getUsername()
val displayName: String = name ?: "Guest"

// ✅ With safe calls
val city: String = user?.profile?.address?.city ?: "Unknown"

// ✅ Numeric defaults
val timeout: Int = config.getTimeout() ?: 30
val retries: Int = config.getRetries() ?: 3

// ✅ Collection defaults
val items: List<String> = getItems() ?: emptyList()
val users: Set<User> = fetchUsers() ?: emptySet()
```

**Pattern**: `nullable ?: default` - concise alternative to if-else.

### Elvis with Expressions

Right side of Elvis can be any expression.

```kotlin
// ✅ Elvis with function call
fun getConfig(key: String): String {
  return loadFromCache(key) ?: loadFromDatabase(key)
}

// ✅ Multiple fallbacks
fun getEmail(user: User?): String {
  return user?.email
    ?: user?.profile?.contactEmail
    ?: user?.profile?.backupEmail
    ?: "no-email@example.com"
}

// ✅ Elvis with calculations
fun calculatePrice(discount: Double?): Double {
  val basePrice = 100.0
  return basePrice - (discount ?: (basePrice * 0.1))  // 10% default discount
}
```

**Use case**: Cascading fallback logic.

### Elvis with Control Flow

Use Elvis with `return`, `throw`, or `continue` for validation.

```kotlin
// ✅ Early return
fun processUser(user: User?) {
  val validUser = user ?: return
  println("Processing: ${validUser.name}")
}

// ✅ Throw exception
fun requireConfig(config: Config?): Config {
  return config ?: throw IllegalStateException("Config not initialized")
}

// ✅ Custom exception
class ValidationException(message: String) : Exception(message)

fun validateInput(input: String?): String {
  return input?.trim() ?: throw ValidationException("Input required")
}

// ✅ In loops
fun processItems(items: List<Item?>?) {
  items ?: return  // Early exit if list is null

  for (item in items) {
    val validItem = item ?: continue  // Skip null items
    process(validItem)
  }
}
```

**Pattern**: Guard clauses with Elvis for concise validation.

## Combining Safe Call and Elvis

### Complete Null Handling Pattern

Combine both operators for robust null handling.

```kotlin
// ✅ Safe call + Elvis = null-safe with default
val city: String = user?.profile?.address?.city ?: "Unknown"

// ✅ With transformation
val displayEmail: String = user?.email?.lowercase() ?: "no-email@example.com"

// ✅ Multiple operations
fun getUserDisplay(user: User?): String {
  return user?.profile?.name?.trim()?.takeIf { it.isNotEmpty() } ?: "Anonymous"
}
```

**Pattern**: `nullable?.property?.method() ?: default` - most common null handling pattern.

### Safe Call with let and Elvis

Combine `?.let` with Elvis for advanced patterns.

```kotlin
// ✅ Execute block or provide default
val result: String = user?.let { u ->
  "User: ${u.name}, Email: ${u.email}"
} ?: "No user available"

// ✅ Transform or default
fun formatPrice(price: Double?): String {
  return price?.let { "$${String.format("%.2f", it)}" } ?: "Price unavailable"
}

// ✅ Nested safe operations
fun getFullAddress(user: User?): String {
  return user?.profile?.address?.let { addr ->
    "${addr.street}, ${addr.city}, ${addr.country}"
  } ?: "Address not provided"
}
```

**Use case**: Complex transformations with fallbacks.

## Advanced Patterns

### Null-Safe Assignment Chains

Build objects safely with nullable sources.

```kotlin
data class UserProfile(
  val name: String,
  val email: String,
  val city: String,
  val country: String
)

// ✅ Safe construction
fun buildProfile(user: User?): UserProfile? {
  return user?.let {
    UserProfile(
      name = it.profile?.name ?: return null,
      email = it.email ?: return null,
      city = it.profile?.address?.city ?: "Unknown",
      country = it.profile?.address?.country ?: "Unknown"
    )
  }
}
```

**Pattern**: Early return in let blocks for required fields.

### Elvis with run or apply

Use scope functions for complex fallback logic.

```kotlin
// ✅ Elvis with run for multi-line default
val config = loadConfig() ?: run {
  logger.warn("Config not found, using defaults")
  createDefaultConfig()
}

// ✅ Elvis with apply for setup
val connection = establishConnection() ?: run {
  logger.error("Connection failed, creating fallback")
  createFallbackConnection()
}.apply {
  setTimeout(5000)
  setRetries(3)
}
```

**Use case**: Complex initialization with fallbacks.

### Null-Safe Comparison Chains

Use safe calls in comparison logic.

```kotlin
// ✅ Safe comparisons
fun isUserActive(user: User?): Boolean {
  return user?.profile?.status?.equals("active") ?: false
}

// ✅ Safe numeric comparisons
fun hasLowBalance(account: Account?): Boolean {
  return (account?.balance ?: 0.0) < 100.0
}

// ✅ Safe date comparisons
fun isExpired(subscription: Subscription?): Boolean {
  val expiryDate = subscription?.expiryDate ?: return true
  return expiryDate.isBefore(LocalDate.now())
}
```

## Common Pitfalls

### Overusing Elvis with Complex Logic

```kotlin
// ❌ Hard to read
val result = user?.profile?.address?.city
  ?: user?.profile?.homeAddress?.city
  ?: user?.businessAddress?.city
  ?: config.defaultCity
  ?: "Unknown"

// ✅ Extract to function
fun getUserCity(user: User?): String {
  user?.profile?.address?.city?.let { return it }
  user?.profile?.homeAddress?.city?.let { return it }
  user?.businessAddress?.city?.let { return it }
  return config.defaultCity ?: "Unknown"
}
```

**Why problematic**: Long Elvis chains reduce readability.

### Returning Null Instead of Meaningful Default

```kotlin
// ❌ Returns null, forcing callers to handle it
fun getDisplayName(user: User?): String? {
  return user?.profile?.name
}

// ✅ Provide meaningful default
fun getDisplayName(user: User?): String {
  return user?.profile?.name ?: "Guest"
}
```

**Why problematic**: Propagating nullability unnecessarily.

### Elvis with Side Effects

```kotlin
// ❌ Side effect in Elvis expression
val user = findUser(id) ?: {
  logger.error("User not found")  // ❌ This is a lambda, not called!
  null
}()

// ✅ Use run for side effects
val user = findUser(id) ?: run {
  logger.error("User not found")
  null
}

// ✅ Or separate statements
val user = findUser(id)
if (user == null) {
  logger.error("User not found")
}
```

**Why problematic**: Elvis evaluates expressions, not blocks.

### Excessive Safe Call Chaining

```kotlin
// ❌ Too deep, hard to debug
val value = obj?.a?.b?.c?.d?.e?.f?.g

// ✅ Break into steps
val value = obj?.a?.b?.c?.let { intermediate ->
  intermediate.d?.e?.f?.g
}

// ✅ Or validate structure
fun getValue(obj: ComplexObject?): String? {
  val a = obj?.a ?: return null
  val b = a.b ?: return null
  val c = b.c ?: return null
  return c.d?.e?.f?.g
}
```

**Why problematic**: Deep chains hide which part is null.

## Variations

### takeIf and takeUnless with Elvis

Conditional null handling with validation.

```kotlin
// ✅ takeIf returns null if predicate fails
val validEmail = email?.takeIf { it.contains("@") } ?: "invalid@example.com"

// ✅ takeUnless returns null if predicate succeeds
val nonEmptyText = text?.takeUnless { it.isEmpty() } ?: "No text"

// ✅ Chain with safe calls
fun validateAge(input: String?): Int {
  return input
    ?.trim()
    ?.toIntOrNull()
    ?.takeIf { it in 0..150 }
    ?: throw IllegalArgumentException("Invalid age")
}
```

### Safe Cast with Elvis

Combine safe casting with fallbacks.

```kotlin
// ✅ Safe cast with default
val number: Int = value as? Int ?: 0

// ✅ With transformation
val text: String = (value as? String)?.uppercase() ?: "NOT A STRING"

// ✅ Multiple type attempts
fun parseValue(value: Any): Number {
  return (value as? Int)
    ?: (value as? Double)
    ?: (value as? Long)
    ?: 0
}
```

### Null-Coalescing with Sequences

Find first non-null value in sequence.

```kotlin
// ✅ First non-null from multiple sources
fun getConfiguration(): Config {
  return loadFromEnvironment()
    ?: loadFromFile()
    ?: loadFromDefaults()
    ?: throw IllegalStateException("No configuration available")
}

// ✅ With sequenceOf
fun findValue(key: String): String? {
  return sequenceOf(
    cache.get(key),
    database.query(key),
    api.fetch(key)
  ).firstOrNull { it != null }
}
```

## Related Patterns

**Learn more**:

- [Working with Nullable Types](/en/learn/software-engineering/programming-language/kotlin/how-to/working-with-nullable-types) - Foundation for null handling
- [Scope Functions](/en/learn/software-engineering/programming-language/kotlin/how-to/scope-functions) - let, run, also, apply with nulls
- [Beginner Tutorial - Null Safety](/en/learn/software-engineering/programming-language/kotlin/tutorials/beginner#type-system-and-variables) - Null safety fundamentals
- [Quick Start - Null Safety](/en/learn/software-engineering/programming-language/kotlin/tutorials/quick-start#null-safety) - Introduction to null operators

**Cookbook recipes**:

- [Nullable and Optional Values](/en/learn/software-engineering/programming-language/kotlin/how-to/cookbook#nullable-optional) - Quick reference patterns
- [Result Type](/en/learn/software-engineering/programming-language/kotlin/how-to/cookbook#result-type) - Functional null handling alternative
- [Try-Catch Idioms](/en/learn/software-engineering/programming-language/kotlin/how-to/cookbook#try-catch-idioms) - Error handling patterns
