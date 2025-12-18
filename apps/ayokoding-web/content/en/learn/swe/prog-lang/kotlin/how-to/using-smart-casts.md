---
title: "How to Use Smart Casts"
date: 2025-12-18T00:00:00+07:00
draft: false
weight: 604
description: "Leverage Kotlin's smart cast feature for cleaner type checking and casting"
tags: ["kotlin", "smart-cast", "type-checking", "type-safety"]
categories: ["learn"]
---

## Problem

Traditional type checking and casting in languages like Java requires explicit casts after checking types, leading to verbose code. Kotlin's smart cast feature automatically casts variables after type checks, eliminating redundancy.

This guide shows how to use smart casts effectively for cleaner, safer code.

## Smart Cast Basics

### Automatic Casting After is Check

Kotlin automatically casts a variable after you check its type with `is`.

```kotlin
fun processValue(value: Any) {
  // ❌ Java-style (explicit cast)
  // if (value instanceof String) {
  //   String str = (String) value;
  //   println(str.length);
  // }

  // ✅ Kotlin smart cast
  if (value is String) {
    // value is automatically cast to String
    println(value.length)
    println(value.uppercase())
  }
}
```

**How it works**: After `is` check succeeds, Kotlin treats the variable as the checked type within that scope.

### Smart Cast with Multiple Types

```kotlin
fun describe(obj: Any): String {
  return when (obj) {
    is String -> "String of length ${obj.length}"  // obj is String
    is Int -> "Int with value $obj"                // obj is Int
    is List<*> -> "List with ${obj.size} elements" // obj is List
    else -> "Unknown type"
  }
}

// ✅ Usage
println(describe("Hello"))      // String of length 5
println(describe(42))           // Int with value 42
println(describe(listOf(1, 2))) // List with 2 elements
```

**Pattern**: `when` expression with `is` checks enables smart casting in each branch.

## Smart Cast with Nullable Types

### Null Checks Enable Smart Cast

After checking for null, Kotlin smart casts to non-nullable type.

```kotlin
fun printLength(text: String?) {
  // ✅ After null check, text is non-nullable
  if (text != null) {
    println(text.length)  // Smart cast to String
    println(text.uppercase())
  }
}

// ✅ Early return pattern
fun processUser(user: User?): String {
  if (user == null) return "No user"

  // user is automatically non-nullable here
  return "User: ${user.name}, ${user.email}"
}
```

**Key benefit**: No explicit casting needed after null checks.

### require and check Functions

Use `require` and `check` for smart casting with validation.

```kotlin
fun processOrder(order: Order?) {
  // ✅ require throws IllegalArgumentException if false
  require(order != null) { "Order cannot be null" }

  // order is smart cast to non-nullable Order
  println("Processing order: ${order.id}")
}

fun executeQuery(connection: Connection?) {
  // ✅ check throws IllegalStateException if false
  check(connection != null) { "Connection not established" }

  // connection is smart cast to non-nullable Connection
  connection.execute("SELECT * FROM users")
}
```

## Smart Cast with when Expression

### Exhaustive Type Checking

Use `when` for exhaustive type checking with smart casts.

```kotlin
sealed interface Result
data class Success(val data: String) : Result
data class Error(val message: String) : Result
data object Loading : Result

fun handleResult(result: Result) {
  when (result) {
    is Success -> {
      // result is smart cast to Success
      println("Data: ${result.data}")
    }
    is Error -> {
      // result is smart cast to Error
      println("Error: ${result.message}")
    }
    Loading -> {
      println("Loading...")
    }
  }
  // Compiler ensures all cases handled
}
```

**Use case**: Sealed classes with exhaustive when expressions.

### Type Checking with Conditions

Combine type checks with other conditions.

```kotlin
fun processValue(value: Any) {
  when {
    value is String && value.isNotEmpty() -> {
      println("Non-empty string: ${value.uppercase()}")
    }
    value is Int && value > 0 -> {
      println("Positive integer: $value")
    }
    value is List<*> && value.isNotEmpty() -> {
      println("Non-empty list with ${value.size} elements")
    }
    else -> println("Other value")
  }
}
```

## Smart Cast Limitations

### When Smart Cast Doesn't Work

Smart cast requires compiler certainty that the value hasn't changed.

```kotlin
// ❌ Smart cast fails - var can be modified
var value: Any = "Hello"
if (value is String) {
  // ❌ Compilation error - value might change
  // println(value.length)
}

// ✅ Use local val for smart cast
var value: Any = "Hello"
val localValue = value
if (localValue is String) {
  println(localValue.length)  // ✅ Works
}
```

**Why**: `var` properties can be modified by another thread or custom getter.

### val Properties with Custom Getters

```kotlin
class Container(private var _value: Any) {
  // ❌ Smart cast fails - custom getter might return different value
  val value: Any
    get() = _value

  fun process() {
    if (value is String) {
      // ❌ Compilation error
      // println(value.length)
    }
  }
}

// ✅ Solution - store in local val
fun process() {
  val currentValue = value
  if (currentValue is String) {
    println(currentValue.length)  // ✅ Works
  }
}
```

### Mutable var Properties in Classes

```kotlin
class Example {
  var data: Any = "Hello"

  fun printLength() {
    // ❌ Smart cast fails for var property
    if (data is String) {
      // ❌ Compilation error
      // println(data.length)
    }
  }
}

// ✅ Solutions
class Example {
  // Option 1: Use val
  val data: Any = "Hello"

  fun printLength() {
    if (data is String) {
      println(data.length)  // ✅ Works
    }
  }

  // Option 2: Local variable
  var data: Any = "Hello"

  fun printLength() {
    val localData = data
    if (localData is String) {
      println(localData.length)  // ✅ Works
    }
  }
}
```

## Negated Type Checks

### Using !is for Negative Checks

Smart cast works with `!is` for early returns.

```kotlin
fun process(value: Any) {
  // ✅ Early return with !is
  if (value !is String) {
    println("Not a string")
    return
  }

  // value is smart cast to String
  println("String: ${value.uppercase()}")
}

// ✅ With when
fun describe(obj: Any): String {
  if (obj !is String) return "Not a string"

  // obj is smart cast to String
  return "String of length ${obj.length}"
}
```

**Pattern**: Use `!is` for guard clauses, then benefit from smart cast in remaining code.

### Combining Conditions

```kotlin
fun validateInput(value: Any): String {
  // ✅ Early returns with !is
  if (value !is String) {
    throw IllegalArgumentException("Value must be String")
  }

  // value is String here
  if (value.isEmpty()) {
    throw IllegalArgumentException("Value cannot be empty")
  }

  return value.trim()
}
```

## Smart Cast with Collections

### Type-Safe Collection Access

Smart cast works with collection element types.

```kotlin
fun processList(items: List<Any>) {
  for (item in items) {
    when (item) {
      is String -> println("String: ${item.uppercase()}")
      is Int -> println("Int: ${item * 2}")
      is List<*> -> println("Nested list with ${item.size} elements")
    }
  }
}

// ✅ Filter by type
val mixed: List<Any> = listOf("Hello", 42, "World", 3.14)
val strings = mixed.filterIsInstance<String>()
// strings is List<String>, each element is smart cast
strings.forEach { println(it.uppercase()) }
```

**Use case**: Processing heterogeneous collections.

### Safe Cast with as?

Use `as?` for safe casting that returns null on failure.

```kotlin
// ✅ Safe cast
fun tryParse(value: Any): Int? {
  return value as? Int  // Returns null if value is not Int
}

// ✅ With Elvis operator
fun parseInt(value: Any): Int {
  return (value as? Int) ?: throw IllegalArgumentException("Not an Int")
}

// ✅ Chain with safe call
fun processValue(value: Any) {
  val str = value as? String
  str?.let {
    println("String: ${it.uppercase()}")
  }
}
```

**Pattern**: `as?` returns null instead of throwing ClassCastException.

## Common Pitfalls

### Assuming Smart Cast on var

```kotlin
// ❌ Expects smart cast but fails
class Processor {
  var data: Any = "Hello"

  fun process() {
    if (data is String) {
      // ❌ Won't compile
      // println(data.length)
    }
  }
}

// ✅ Use val or local variable
class Processor {
  val data: Any = "Hello"

  fun process() {
    if (data is String) {
      println(data.length)  // ✅ Works
    }
  }
}
```

**Why problematic**: Compiler can't guarantee `var` hasn't changed.

### Forgetting Nullability After Type Check

```kotlin
// ❌ Type check doesn't check null
fun process(value: Any) {
  if (value is String) {
    // value is String, but could still be nullable String?
    // This example is actually fine because Any excludes null
  }
}

// ✅ Proper nullable handling
fun process(value: Any?) {
  // Need both null and type check
  if (value != null && value is String) {
    println(value.uppercase())  // Smart cast to String
  }
}
```

### Using Unsafe Cast (as)

```kotlin
// ❌ Unsafe cast throws exception
fun process(value: Any) {
  val str = value as String  // Throws ClassCastException if not String
  println(str.length)
}

// ✅ Use is check with smart cast
fun process(value: Any) {
  if (value is String) {
    println(value.length)  // Safe
  }
}

// ✅ Or safe cast
fun process(value: Any) {
  val str = value as? String
  str?.let { println(it.length) }
}
```

**Why problematic**: Unsafe cast can crash at runtime.

## Variations

### Smart Cast in Expressions

Smart cast works in expression bodies.

```kotlin
// ✅ Expression function with smart cast
fun getLength(value: Any): Int? = if (value is String) value.length else null

// ✅ In when expression
fun describe(value: Any): String = when {
  value is String -> "String: ${value.uppercase()}"
  value is Int -> "Int: ${value * 2}"
  else -> "Unknown"
}
```

### Smart Cast with Contracts

Use contracts to enable smart cast in custom functions.

```kotlin
import kotlin.contracts.ExperimentalContracts
import kotlin.contracts.contract

@OptIn(ExperimentalContracts::class)
fun isNotNullOrEmpty(value: String?): Boolean {
  contract {
    returns(true) implies (value != null)
  }
  return !value.isNullOrEmpty()
}

// ✅ Smart cast enabled by contract
fun process(value: String?) {
  if (isNotNullOrEmpty(value)) {
    println(value.uppercase())  // value is smart cast to String
  }
}
```

**Note**: Contracts are experimental but powerful for library authors.

## Related Patterns

**Learn more**:

- [Working with Nullable Types](/en/learn/swe/prog-lang/kotlin/how-to/working-with-nullable-types) - Foundation for null-safe smart casts
- [Sealed Classes](/en/learn/swe/prog-lang/kotlin/how-to/sealed-classes-interfaces) - Exhaustive when with smart casts
- [Beginner Tutorial - Smart Casts](/en/learn/swe/prog-lang/kotlin/tutorials/beginner#smart-casts) - Smart cast fundamentals
- [Advanced Tutorial - Compiler Internals](/en/learn/swe/prog-lang/kotlin/tutorials/advanced#compiler-internals) - How smart casts work under the hood

**Cookbook recipes**:

- [Type-Safe Builders](/en/learn/swe/prog-lang/kotlin/how-to/cookbook#type-safe-builders) - Smart casts with DSLs
- [Sealed Data Structures](/en/learn/swe/prog-lang/kotlin/how-to/cookbook#sealed-data-structures) - Pattern matching with smart casts
