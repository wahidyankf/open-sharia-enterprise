---
title: "How to Use Extension Functions"
date: 2025-12-18T00:00:00+07:00
draft: false
weight: 611
description: "Add functionality to existing classes without inheritance using extension functions"
tags: ["kotlin", "extension-functions", "kotlin-features", "clean-code"]
---

## Problem

You need to add functionality to existing classes but can't modify their source code. Traditional solutions like utility classes or inheritance have drawbacks. Kotlin's extension functions let you add methods to classes without changing them.

This guide shows how to create and use extension functions effectively.

## Extension Function Basics

### Creating Extension Functions

Define functions that extend existing types.

```kotlin
// ✅ Extend String class
fun String.isPalindrome(): Boolean {
  val cleaned = this.lowercase().filter { it.isLetterOrDigit() }
  return cleaned == cleaned.reversed()
}

// ✅ Usage
println("racecar".isPalindrome())  // true
println("hello".isPalindrome())    // false

// ✅ Extend Int class
fun Int.isEven(): Boolean = this % 2 == 0

fun Int.square(): Int = this * this

println(4.isEven())   // true
println(5.square())   // 25
```

**How it works**: Extension functions are resolved statically - they don't actually modify the class.

### Extension Functions with Parameters

```kotlin
// ✅ Extension with parameters
fun String.repeat(times: Int): String {
  return buildString {
    repeat(times) {
      append(this@repeat)  // this@repeat is the String receiver
    }
  }
}

println("Hi ".repeat(3))  // "Hi Hi Hi "

// ✅ Multiple parameters
fun Int.isBetween(min: Int, max: Int): Boolean {
  return this in min..max
}

println(5.isBetween(1, 10))   // true
println(15.isBetween(1, 10))  // false
```

**Pattern**: `fun ReceiverType.functionName(params): ReturnType`

## Extending Standard Library Classes

### String Extensions

Common string extensions for cleaner code.

```kotlin
// ✅ Validation extensions
fun String.isValidEmail(): Boolean {
  return this.contains("@") && this.contains(".")
}

fun String.isNumeric(): Boolean {
  return this.toIntOrNull() != null
}

// ✅ Transformation extensions
fun String.toTitleCase(): String {
  return this.split(" ").joinToString(" ") { word ->
    word.lowercase().replaceFirstChar { it.uppercase() }
  }
}

fun String.truncate(maxLength: Int, suffix: String = "..."): String {
  return if (this.length <= maxLength) this
  else this.take(maxLength - suffix.length) + suffix
}

// ✅ Usage
println("user@example.com".isValidEmail())  // true
println("hello world".toTitleCase())        // "Hello World"
println("Long text here".truncate(10))      // "Long te..."
```

### Collection Extensions

Extend collections for domain-specific operations.

```kotlin
// ✅ List extensions
fun <T> List<T>.secondOrNull(): T? {
  return if (this.size >= 2) this[1] else null
}

fun List<Int>.sum(): Int {
  return this.fold(0) { acc, num -> acc + num }
}

// ✅ Usage
val numbers = listOf(1, 2, 3, 4)
println(numbers.secondOrNull())  // 2
println(numbers.sum())           // 10

// ✅ Generic extensions
fun <T> List<T>.random(): T {
  return this[(0 until size).random()]
}

val colors = listOf("red", "blue", "green")
println(colors.random())  // Random color
```

**Use case**: Domain-specific collection operations.

## Extensions for Null Safety

### Nullable Receiver Extensions

Create extensions that work on nullable types.

```kotlin
// ✅ Extension on nullable type
fun String?.isNullOrEmpty(): Boolean {
  return this == null || this.isEmpty()
}

// ✅ Safe defaults for nulls
fun String?.orDefault(default: String = "N/A"): String {
  return this ?: default
}

// ✅ Usage
val text: String? = null
println(text.isNullOrEmpty())       // true
println(text.orDefault("Unknown"))  // "Unknown"

// ✅ Null-safe transformation
fun Int?.orZero(): Int = this ?: 0

val value: Int? = null
println(value.orZero())  // 0
```

**Pattern**: `fun ReceiverType?.functionName()` - extension on nullable receiver.

### Conditional Extensions

Extensions that check conditions before operating.

```kotlin
// ✅ Extension with validation
fun String.capitalizeIfNotEmpty(): String {
  return if (this.isNotEmpty()) {
    this.replaceFirstChar { it.uppercase() }
  } else {
    this
  }
}

// ✅ Extension with takeIf
fun Int.positiveOrNull(): Int? {
  return this.takeIf { it > 0 }
}

println(-5.positiveOrNull())  // null
println(5.positiveOrNull())   // 5
```

## Extension Properties

### Read-Only Extension Properties

Add computed properties to types.

```kotlin
// ✅ Extension property
val String.wordCount: Int
  get() = this.split("\\s+".toRegex()).size

val String.firstWord: String
  get() = this.split("\\s+".toRegex()).firstOrNull() ?: ""

// ✅ Usage
val text = "Hello world from Kotlin"
println(text.wordCount)   // 4
println(text.firstWord)   // "Hello"

// ✅ Numeric properties
val Int.isPositive: Boolean
  get() = this > 0

val Double.isWhole: Boolean
  get() = this % 1.0 == 0.0

println(5.isPositive)     // true
println(3.5.isWhole)      // false
```

**Note**: Extension properties can't have backing fields - only getters.

### Extension Properties for Collections

```kotlin
// ✅ Collection properties
val <T> List<T>.middle: T?
  get() = if (this.isNotEmpty()) this[this.size / 2] else null

val List<Int>.average: Double
  get() = if (this.isNotEmpty()) this.sum().toDouble() / this.size else 0.0

// ✅ Usage
val numbers = listOf(1, 2, 3, 4, 5)
println(numbers.middle)    // 3
println(numbers.average)   // 3.0
```

## Generic Extension Functions

### Type Parameter Extensions

Create generic extensions that work with any type.

```kotlin
// ✅ Generic extension
fun <T> T.alsoPrint(): T {
  println(this)
  return this
}

// ✅ Usage - works with any type
5.alsoPrint().square()      // Prints 5, returns 25
"Hello".alsoPrint().length  // Prints "Hello", returns 5

// ✅ Generic with constraint
fun <T : Number> T.isGreaterThan(other: T): Boolean {
  return this.toDouble() > other.toDouble()
}

println(10.isGreaterThan(5))      // true
println(3.5.isGreaterThan(7.2))   // false
```

**Pattern**: `fun <T> T.extensionName()` - generic extension on any type.

### Collection Generic Extensions

```kotlin
// ✅ Generic collection extensions
fun <T> List<T>.containsAny(vararg elements: T): Boolean {
  return elements.any { this.contains(it) }
}

fun <T> List<T>.swap(index1: Int, index2: Int): List<T> {
  val mutable = this.toMutableList()
  val temp = mutable[index1]
  mutable[index1] = mutable[index2]
  mutable[index2] = temp
  return mutable
}

// ✅ Usage
val numbers = listOf(1, 2, 3, 4, 5)
println(numbers.containsAny(3, 6))      // true
println(numbers.swap(0, 4))             // [5, 2, 3, 4, 1]
```

## Extension Functions vs Member Functions

### Dispatch Rules

Member functions take precedence over extension functions.

```kotlin
class Person {
  fun greet() = "Hello from member"
}

// ❌ This extension is shadowed by member
fun Person.greet() = "Hello from extension"

// Member function is called
val person = Person()
println(person.greet())  // "Hello from member"
```

**Rule**: If a class has a member function with the same signature, the member wins.

### When to Use Extensions

```kotlin
// ✅ Use extensions for:
// 1. Adding utility to classes you don't own
fun String.toSlug(): String {
  return this.lowercase()
    .replace("\\s+".toRegex(), "-")
    .replace("[^a-z0-9-]".toRegex(), "")
}

// 2. Domain-specific operations
fun BigDecimal.formatCurrency(): String {
  return "$${String.format("%.2f", this)}"
}

// 3. Null-safe utilities
fun List<String>?.joinSafe(separator: String = ", "): String {
  return this?.joinToString(separator) ?: ""
}

// ❌ Don't use extensions for:
// - Core class behavior (use member functions)
// - Operations that need access to private members
// - Polymorphic behavior (use member functions)
```

## Common Pitfalls

### Expecting Access to Private Members

```kotlin
class User(private val password: String) {
  val username: String = "alice"
}

// ❌ Can't access private members
fun User.validatePassword(input: String): Boolean {
  // return this.password == input  // ❌ Compilation error
  return false
}

// ✅ Extensions only access public members
fun User.greet(): String {
  return "Hello, ${this.username}"  // ✅ Public property accessible
}
```

**Why problematic**: Extensions can't break encapsulation.

### Overusing Extensions for Everything

```kotlin
// ❌ Don't create extension for every function
fun calculateTotal(items: List<Item>): Double { ... }

// Should be:
// ✅ Top-level function if not specific to a type
fun calculateTotal(items: List<Item>): Double { ... }

// ✅ Extension if it's clearly a List operation
fun List<Item>.calculateTotal(): Double { ... }
```

**Why problematic**: Not every function needs to be an extension.

### Extension Name Collisions

```kotlin
// ❌ Name collision with standard library
fun String.length(): Int = this.length * 2  // Confusing - length is a property

// ✅ Use distinct names
fun String.doubleLength(): Int = this.length * 2
```

**Why problematic**: Confuses readers familiar with standard library.

## Variations

### Infix Extension Functions

Create extension functions with infix notation.

```kotlin
// ✅ Infix extension
infix fun Int.multiplyBy(other: Int): Int {
  return this * other
}

// ✅ Usage - cleaner syntax
println(5 multiplyBy 3)   // 15
println(5.multiplyBy(3))  // Also valid

// ✅ Infix for comparisons
infix fun String.matches(regex: String): Boolean {
  return this.matches(Regex(regex))
}

println("hello123" matches "[a-z]+\\d+")  // true
```

### Extension Functions in DSLs

Extensions enable type-safe DSL builders.

```kotlin
// ✅ DSL with extensions
class HTML {
  private val elements = mutableListOf<String>()

  fun body(init: BODY.() -> Unit) {
    val body = BODY()
    body.init()
    elements.add("<body>${body.render()}</body>")
  }

  fun render() = elements.joinToString("\n")
}

class BODY {
  private val elements = mutableListOf<String>()

  fun p(text: String) {
    elements.add("<p>$text</p>")
  }

  fun render() = elements.joinToString("\n")
}

// ✅ Usage
fun html(init: HTML.() -> Unit): HTML {
  val html = HTML()
  html.init()
  return html
}

val page = html {
  body {
    p("Hello World")
    p("Welcome to Kotlin")
  }
}
```

### Extension Functions in Files

Organize extensions by domain.

```kotlin
// StringExtensions.kt
fun String.isValidEmail(): Boolean = this.contains("@")
fun String.toSlug(): String = this.lowercase().replace(" ", "-")

// DateExtensions.kt
fun LocalDate.isWeekend(): Boolean {
  return this.dayOfWeek in listOf(DayOfWeek.SATURDAY, DayOfWeek.SUNDAY)
}
```

**Best practice**: Group related extensions in dedicated files.

## Related Patterns

**Learn more**:

- [Scope Functions](/en/learn/swe/prog-lang/kotlin/how-to/scope-functions) - Built-in extensions (let, run, apply, also, with)
- [Data Classes](/en/learn/swe/prog-lang/kotlin/how-to/use-data-classes-effectively) - Classes often extended with utilities
- [Beginner Tutorial - Extensions](/en/learn/swe/prog-lang/kotlin/tutorials/beginner#extension-functions) - Extension fundamentals
- [Advanced Tutorial - DSLs](/en/learn/swe/prog-lang/kotlin/tutorials/advanced#dsl-building-and-metaprogramming) - Extensions in domain-specific languages

**Cookbook recipes**:

- [Type-Safe Builders](/en/learn/swe/prog-lang/kotlin/how-to/cookbook#type-safe-builders) - DSL with extensions
- [String Manipulation](/en/learn/swe/prog-lang/kotlin/how-to/cookbook#string-manipulation) - Common string extensions
