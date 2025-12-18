---
title: "How to Use Data Classes Effectively"
date: 2025-12-18T00:00:00+07:00
draft: false
weight: 607
description: "Master Kotlin's data classes for clean, concise domain models"
tags: ["kotlin", "data-classes", "domain-modeling", "kotlin-features"]
categories: ["learn"]
---

## Problem

Creating classes to hold data requires boilerplate code: equals(), hashCode(), toString(), and copy methods. Manually writing these is error-prone and verbose. Kotlin's data classes automatically generate these methods.

This guide shows how to use data classes effectively for domain modeling.

## Data Class Basics

### Creating Data Classes

Define data classes for data-holding objects.

```kotlin
// ✅ Simple data class
data class User(
  val id: String,
  val name: String,
  val email: String
)

// ✅ Auto-generated methods available
val user1 = User("1", "Alice", "alice@example.com")
val user2 = User("1", "Alice", "alice@example.com")

// toString() - readable representation
println(user1)  // User(id=1, name=Alice, email=alice@example.com)

// equals() and hashCode() - structural equality
println(user1 == user2)  // true
println(user1 === user2) // false (different objects)

// copy() - create modified copies
val user3 = user1.copy(email = "newemail@example.com")
println(user3)  // User(id=1, name=Alice, email=newemail@example.com)
```

**How it works**: `data` keyword generates equals(), hashCode(), toString(), copy(), and componentN() methods.

### Requirements for Data Classes

Data classes must meet specific requirements.

```kotlin
// ✅ Valid data class
data class Product(
  val id: Long,           // Primary constructor with at least one parameter
  val name: String,       // val or var parameters
  val price: Double,
  var quantity: Int       // var is allowed
)

// ❌ Invalid - no primary constructor parameters
// data class Empty()

// ❌ Invalid - abstract, open, sealed, or inner
// abstract data class AbstractData(val x: Int)
// open data class OpenData(val x: Int)
// sealed data class SealedData(val x: Int)
```

**Rules**:

- Primary constructor needs at least one parameter
- Parameters must be `val` or `var`
- Can't be abstract, open, sealed, or inner
- May inherit from other classes and implement interfaces

## Copy Function

### Creating Modified Copies

Use copy() to create instances with modified properties.

```kotlin
data class Address(
  val street: String,
  val city: String,
  val zipCode: String,
  val country: String
)

val address = Address(
  street = "123 Main St",
  city = "New York",
  zipCode = "10001",
  country = "USA"
)

// ✅ Copy with changes
val newAddress = address.copy(street = "456 Oak Ave")
println(newAddress)
// Address(street=456 Oak Ave, city=New York, zipCode=10001, country=USA)

// ✅ Copy multiple properties
val foreignAddress = address.copy(
  city = "London",
  zipCode = "SW1A 1AA",
  country = "UK"
)

// ✅ Copy without changes (creates clone)
val clone = address.copy()
println(clone == address)  // true
println(clone === address) // false
```

**Use case**: Immutable data transformations (common in functional programming).

### Copy in Immutable Patterns

Data classes enable immutable programming.

```kotlin
// ✅ Immutable state updates
data class GameState(
  val score: Int,
  val level: Int,
  val lives: Int
)

fun increaseScore(state: GameState, points: Int): GameState {
  return state.copy(score = state.score + points)
}

fun nextLevel(state: GameState): GameState {
  return state.copy(level = state.level + 1, lives = state.lives + 1)
}

// ✅ Usage
var state = GameState(score = 0, level = 1, lives = 3)
state = increaseScore(state, 100)
state = nextLevel(state)
println(state)  // GameState(score=100, level=2, lives=4)
```

**Pattern**: Functional updates instead of mutation.

## Destructuring Declarations

### Component Functions

Data classes generate componentN() functions for destructuring.

```kotlin
data class Point(val x: Int, val y: Int)

// ✅ Destructuring
val point = Point(10, 20)
val (x, y) = point
println("x=$x, y=$y")  // x=10, y=20

// ✅ In loops
val points = listOf(Point(1, 2), Point(3, 4), Point(5, 6))
for ((x, y) in points) {
  println("($x, $y)")
}
// (1, 2)
// (3, 4)
// (5, 6)

// ✅ In lambda parameters
val distances = points.map { (x, y) -> Math.sqrt((x * x + y * y).toDouble()) }
```

**How it works**: `component1()` returns first property, `component2()` returns second, etc.

### Partial Destructuring

Use underscore to skip properties you don't need.

```kotlin
data class User(val id: String, val name: String, val email: String, val age: Int)

val user = User("1", "Alice", "alice@example.com", 30)

// ✅ Skip properties with _
val (id, name, _) = user  // Skip email
println("$id: $name")

val (_, _, email) = user  // Skip id and name
println(email)

// ✅ Take only what you need
val (id2, _) = user  // Just id
```

## Data Classes in Collections

### Using as Map Keys

Data classes work well as map keys due to proper equals()/hashCode().

```kotlin
data class Coordinate(val x: Int, val y: Int)

// ✅ Data class as map key
val grid = mutableMapOf<Coordinate, String>()
grid[Coordinate(0, 0)] = "Origin"
grid[Coordinate(1, 0)] = "East"
grid[Coordinate(0, 1)] = "North"

// ✅ Lookup by value (not identity)
val key = Coordinate(0, 0)
println(grid[key])  // "Origin" - works because of proper equals()
```

### In Sets

Data classes provide structural equality for sets.

```kotlin
data class Product(val id: Long, val name: String)

// ✅ Set deduplication with data classes
val products = setOf(
  Product(1, "Widget"),
  Product(2, "Gadget"),
  Product(1, "Widget")  // Duplicate - removed by set
)

println(products.size)  // 2 - duplicate eliminated
```

## Nested and Complex Data Classes

### Nested Data Classes

Data classes can contain other data classes.

```kotlin
data class Address(val street: String, val city: String)
data class Person(val name: String, val address: Address)

val person = Person("Alice", Address("Main St", "NYC"))

// ✅ Copy nested data classes
val movedPerson = person.copy(
  address = person.address.copy(city = "LA")
)
println(movedPerson)
// Person(name=Alice, address=Address(street=Main St, city=LA))
```

### Data Classes with Collections

```kotlin
data class Order(
  val id: String,
  val items: List<String>,
  val total: Double
)

val order = Order("ORD-001", listOf("Item1", "Item2"), 99.99)

// ✅ Copy with collection modification
val updatedOrder = order.copy(
  items = order.items + "Item3",
  total = order.total + 19.99
)
```

**Note**: Collections in data classes are references - copy() doesn't deep copy them.

### Deep Copy Pattern

Implement deep copying when needed.

```kotlin
data class Address(val street: String, val city: String)
data class Person(val name: String, val address: Address) {
  // ✅ Custom deep copy
  fun deepCopy(): Person {
    return this.copy(address = this.address.copy())
  }
}

val person1 = Person("Alice", Address("Main St", "NYC"))
val person2 = person1.deepCopy()
```

## Common Pitfalls

### Mutable Properties in Data Classes

```kotlin
// ❌ Mutable properties break equality
data class Counter(var count: Int)

val c1 = Counter(0)
val c2 = Counter(0)
println(c1 == c2)  // true

c1.count = 5
println(c1 == c2)  // false - but both started equal!

// ✅ Use immutable properties
data class ImmutableCounter(val count: Int) {
  fun increment(): ImmutableCounter = copy(count = count + 1)
}
```

**Why problematic**: Mutable data classes violate immutability contracts, especially in collections.

### Forgetting Data Class Limitations

```kotlin
// ❌ Only primary constructor properties participate in equals/hashCode
data class User(val id: String) {
  var lastLogin: Long = 0  // Not in primary constructor
}

val u1 = User("1")
u1.lastLogin = 1000

val u2 = User("1")
u2.lastLogin = 2000

println(u1 == u2)  // true - lastLogin not compared!
```

**Why problematic**: Properties outside primary constructor ignored by generated methods.

### Data Classes with Default Values

```kotlin
// ❌ Default values can cause confusion
data class Config(
  val timeout: Int = 30,
  val retries: Int = 3
)

val c1 = Config()
val c2 = Config()
println(c1 == c2)  // true

// But semantically, are these the same configuration?
val c3 = Config(timeout = 30, retries = 3)
println(c1 == c3)  // true - but was it intentional?
```

**Why problematic**: Implicit defaults may not represent business equivalence.

## Variations

### Data Classes Implementing Interfaces

Data classes can implement interfaces.

```kotlin
interface Identifiable {
  val id: String
}

// ✅ Data class implementing interface
data class User(
  override val id: String,
  val name: String,
  val email: String
) : Identifiable

fun printId(obj: Identifiable) {
  println("ID: ${obj.id}")
}

val user = User("1", "Alice", "alice@example.com")
printId(user)  // ID: 1
```

### Data Classes with Validation

Add validation to data class construction.

```kotlin
// ✅ Validation in init block
data class Email(val value: String) {
  init {
    require(value.contains("@")) { "Invalid email format" }
  }
}

// ✅ Factory function with validation
data class Age(val value: Int) {
  companion object {
    fun of(value: Int): Age {
      require(value in 0..150) { "Age must be between 0 and 150" }
      return Age(value)
    }
  }

  private constructor(value: Int) : this(value)
}

val age = Age.of(25)  // ✅ Valid
// val invalid = Age.of(200)  // ❌ Throws exception
```

### Data Classes with Computed Properties

Add derived properties outside primary constructor.

```kotlin
data class Rectangle(val width: Double, val height: Double) {
  // ✅ Computed properties
  val area: Double
    get() = width * height

  val perimeter: Double
    get() = 2 * (width + height)

  val isSquare: Boolean
    get() = width == height
}

val rect = Rectangle(5.0, 10.0)
println(rect.area)       // 50.0
println(rect.perimeter)  // 30.0
println(rect.isSquare)   // false
```

**Note**: Computed properties not included in equals(), hashCode(), or copy().

## Related Patterns

**Learn more**:

- [Sealed Classes](/en/learn/swe/prog-lang/kotlin/how-to/sealed-classes-interfaces) - Type-safe hierarchies with data classes
- [Copy Function in Immutable Patterns](/en/learn/swe/prog-lang/kotlin/tutorials/beginner#data-classes) - Functional programming patterns
- [Beginner Tutorial - Data Classes](/en/learn/swe/prog-lang/kotlin/tutorials/beginner#data-classes) - Data class fundamentals
- [Best Practices - Immutability](/en/learn/swe/prog-lang/kotlin/explanation/best-practices#immutability) - Data class design guidelines

**Cookbook recipes**:

- [Sealed Data Structures](/en/learn/swe/prog-lang/kotlin/how-to/cookbook#sealed-data-structures) - Data classes with sealed hierarchies
- [Immutable Collections](/en/learn/swe/prog-lang/kotlin/how-to/cookbook#immutable-collections) - Immutable patterns with data classes
