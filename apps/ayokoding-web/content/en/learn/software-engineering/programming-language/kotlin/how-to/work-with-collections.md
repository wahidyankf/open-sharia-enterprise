---
title: "How to Work with Collections Idiomatically"
date: 2025-12-18T14:50:04+07:00
draft: false
weight: 1000007
description: "Master Kotlin's powerful collection operations including transformations, filtering, and sequences"
tags: ["kotlin", "collections", "functional-programming", "sequences"]
categories: ["learn"]
---

## Problem

Working with collections requires common operations like filtering, mapping, grouping, and aggregating. Writing imperative loops is verbose and error-prone. Kotlin provides expressive collection operations that make code more readable and maintainable.

This guide shows idiomatic collection patterns in Kotlin.

## Collection Basics

### Immutable vs Mutable Collections

Kotlin distinguishes read-only and mutable collections.

```kotlin
// ✅ Read-only collections (default)
val numbers = listOf(1, 2, 3, 4, 5)
val names = setOf("Alice", "Bob", "Charlie")
val ages = mapOf("Alice" to 30, "Bob" to 25)

// ❌ Can't modify
// numbers.add(6)  // Compilation error

// ✅ Mutable collections (explicit)
val mutableNumbers = mutableListOf(1, 2, 3)
val mutableNames = mutableSetOf("Alice", "Bob")
val mutableAges = mutableMapOf("Alice" to 30)

// ✅ Can modify
mutableNumbers.add(4)
mutableNames.remove("Bob")
mutableAges["Charlie"] = 35
```

**Best practice**: Default to immutable collections, use mutable only when necessary.

### Collection Types

Choose appropriate collection type for your use case.

```kotlin
// ✅ List - ordered, allows duplicates
val fruits = listOf("apple", "banana", "apple", "cherry")
println(fruits[0])  // apple
println(fruits.size)  // 4

// ✅ Set - unordered, no duplicates
val uniqueFruits = setOf("apple", "banana", "apple", "cherry")
println(uniqueFruits.size)  // 3 (duplicate removed)

// ✅ Map - key-value pairs
val prices = mapOf(
  "apple" to 1.20,
  "banana" to 0.80,
  "cherry" to 2.50
)
println(prices["apple"])  // 1.20
```

## Transformation Operations

### map - Transform Elements

Apply function to each element, producing new collection.

```kotlin
// ✅ Basic map
val numbers = listOf(1, 2, 3, 4, 5)
val doubled = numbers.map { it * 2 }
println(doubled)  // [2, 4, 6, 8, 10]

// ✅ Transform to different type
val lengths = listOf("apple", "banana", "cherry").map { it.length }
println(lengths)  // [5, 6, 6]

// ✅ Map with index
val indexed = listOf("a", "b", "c").mapIndexed { index, value ->
  "$index: $value"
}
println(indexed)  // [0: a, 1: b, 2: c]
```

### flatMap - Flatten Nested Collections

Transform and flatten nested structures.

```kotlin
// ✅ flatMap transforms and flattens
val departments = listOf(
  Department("Engineering", listOf("Alice", "Bob")),
  Department("Marketing", listOf("Charlie", "Diana")),
  Department("Sales", listOf("Eve"))
)

val allEmployees = departments.flatMap { it.employees }
println(allEmployees)  // [Alice, Bob, Charlie, Diana, Eve]

// ✅ flatMap vs map + flatten
val words = listOf("Hello world", "Kotlin rocks")

val allWords1 = words.flatMap { it.split(" ") }
val allWords2 = words.map { it.split(" ") }.flatten()
// Both produce: [Hello, world, Kotlin, rocks]
```

### mapNotNull - Filter Nulls During Transform

Transform and remove null results in one operation.

```kotlin
// ✅ mapNotNull combines map and filterNotNull
val inputs = listOf("1", "2", "abc", "3", "def")

val numbers = inputs.mapNotNull { it.toIntOrNull() }
println(numbers)  // [1, 2, 3]

// ✅ Equivalent verbose version
val numbersVerbose = inputs
  .map { it.toIntOrNull() }
  .filterNotNull()
```

## Filtering Operations

### filter - Select Elements

Keep elements matching predicate.

```kotlin
// ✅ Basic filter
val numbers = listOf(1, 2, 3, 4, 5, 6)
val evenNumbers = numbers.filter { it % 2 == 0 }
println(evenNumbers)  // [2, 4, 6]

// ✅ Filter with complex condition
data class User(val name: String, val age: Int, val active: Boolean)

val users = listOf(
  User("Alice", 30, true),
  User("Bob", 25, false),
  User("Charlie", 35, true)
)

val activeAdults = users.filter { it.active && it.age >= 30 }
println(activeAdults)  // [User(Alice, 30, true), User(Charlie, 35, true)]
```

### partition - Split by Predicate

Divide collection into two groups.

```kotlin
// ✅ partition returns Pair of lists
val numbers = listOf(1, 2, 3, 4, 5, 6)
val (even, odd) = numbers.partition { it % 2 == 0 }

println(even)  // [2, 4, 6]
println(odd)   // [1, 3, 5]

// ✅ Partition by multiple criteria
val scores = listOf(95, 87, 72, 65, 90, 55)
val (passing, failing) = scores.partition { it >= 70 }
```

### take, drop, and Variants

Select elements by position.

```kotlin
val numbers = listOf(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

// ✅ take - first N elements
println(numbers.take(3))  // [1, 2, 3]

// ✅ takeLast - last N elements
println(numbers.takeLast(3))  // [8, 9, 10]

// ✅ takeWhile - take until condition fails
println(numbers.takeWhile { it < 5 })  // [1, 2, 3, 4]

// ✅ drop - skip first N elements
println(numbers.drop(3))  // [4, 5, 6, 7, 8, 9, 10]

// ✅ dropLast - skip last N elements
println(numbers.dropLast(3))  // [1, 2, 3, 4, 5, 6, 7]

// ✅ dropWhile - skip until condition fails
println(numbers.dropWhile { it < 5 })  // [5, 6, 7, 8, 9, 10]
```

## Aggregation Operations

### reduce and fold

Accumulate values into single result.

```kotlin
// ✅ reduce - accumulate using first element as initial
val numbers = listOf(1, 2, 3, 4, 5)
val sum = numbers.reduce { acc, num -> acc + num }
println(sum)  // 15

// ✅ fold - accumulate with explicit initial value
val sum2 = numbers.fold(0) { acc, num -> acc + num }
println(sum2)  // 15

val product = numbers.fold(1) { acc, num -> acc * num }
println(product)  // 120

// ✅ fold with different result type
val concatenated = numbers.fold("Numbers:") { acc, num -> "$acc $num" }
println(concatenated)  // Numbers: 1 2 3 4 5
```

**Difference**: `reduce` uses first element as initial, `fold` requires explicit initial value.

### Built-in Aggregations

Common aggregation operations have dedicated functions.

```kotlin
val numbers = listOf(3, 1, 4, 1, 5, 9, 2, 6)

// ✅ Built-in aggregations
println(numbers.sum())      // 31
println(numbers.average())  // 3.875
println(numbers.min())      // 1
println(numbers.max())      // 9
println(numbers.count())    // 8

// ✅ sumOf - transform then sum
data class Product(val name: String, val price: Double)
val products = listOf(
  Product("Widget", 19.99),
  Product("Gadget", 29.99),
  Product("Doohickey", 9.99)
)

val totalPrice = products.sumOf { it.price }
println(totalPrice)  // 59.97
```

### count with Predicate

Count elements matching condition.

```kotlin
// ✅ count with predicate
val numbers = listOf(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
val evenCount = numbers.count { it % 2 == 0 }
println(evenCount)  // 5

// ✅ More efficient than filter + size
val efficientCount = numbers.count { it > 5 }  // ✅ Efficient
val inefficientCount = numbers.filter { it > 5 }.size  // ❌ Creates intermediate list
```

## Grouping and Association

### groupBy - Group Elements

Group elements by key.

```kotlin
// ✅ Basic groupBy
val words = listOf("apple", "apricot", "banana", "cherry", "avocado")
val byFirstLetter = words.groupBy { it.first() }
println(byFirstLetter)
// {a=[apple, apricot, avocado], b=[banana], c=[cherry]}

// ✅ Group with transformation
data class Person(val name: String, val age: Int)
val people = listOf(
  Person("Alice", 30),
  Person("Bob", 25),
  Person("Charlie", 30),
  Person("Diana", 25)
)

val byAge = people.groupBy { it.age }
println(byAge)
// {30=[Person(Alice, 30), Person(Charlie, 30)], 25=[Person(Bob, 25), Person(Diana, 25)]}
```

### associateBy - Create Map by Key

Convert list to map using element property as key.

```kotlin
// ✅ associateBy creates map from collection
data class User(val id: String, val name: String)
val users = listOf(
  User("1", "Alice"),
  User("2", "Bob"),
  User("3", "Charlie")
)

val usersById = users.associateBy { it.id }
println(usersById["2"])  // User(2, Bob)

// ✅ associateBy with value transform
val nameById = users.associateBy(
  keySelector = { it.id },
  valueTransform = { it.name }
)
println(nameById)  // {1=Alice, 2=Bob, 3=Charlie}
```

### associate - Custom Key-Value Pairs

Create map with custom key and value.

```kotlin
// ✅ associate - full control over keys and values
val words = listOf("apple", "banana", "cherry")
val lengthMap = words.associate { it to it.length }
println(lengthMap)  // {apple=5, banana=6, cherry=6}

// ✅ Transform both key and value
val uppercaseMap = words.associate { it.uppercase() to it.length }
println(uppercaseMap)  // {APPLE=5, BANANA=6, CHERRY=6}
```

## Finding Elements

### find and firstOrNull

Find first element matching predicate.

```kotlin
val numbers = listOf(1, 2, 3, 4, 5, 6)

// ✅ find returns first match or null
val firstEven = numbers.find { it % 2 == 0 }
println(firstEven)  // 2

// ✅ firstOrNull (same as find)
val firstEven2 = numbers.firstOrNull { it % 2 == 0 }

// ✅ first throws exception if not found
val firstEven3 = numbers.first { it % 2 == 0 }  // 2
// val notFound = numbers.first { it > 10 }  // ❌ NoSuchElementException
```

### single - Expect Exactly One

Get single element, throw if zero or multiple.

```kotlin
// ✅ single - expects exactly one element
val list1 = listOf(42)
println(list1.single())  // 42

val list2 = listOf(1, 2, 3, 4, 5)
val singleEven = list2.single { it == 2 }  // 2

// ❌ Throws if multiple matches
// val multiple = list2.single { it % 2 == 0 }  // IllegalArgumentException

// ✅ singleOrNull returns null instead of throwing
val result = list2.singleOrNull { it % 2 == 0 }  // null (multiple matches)
```

### any, all, none

Check predicates on collections.

```kotlin
val numbers = listOf(1, 2, 3, 4, 5)

// ✅ any - at least one matches
println(numbers.any { it > 3 })  // true
println(numbers.any { it > 10 })  // false

// ✅ all - all elements match
println(numbers.all { it > 0 })  // true
println(numbers.all { it % 2 == 0 })  // false

// ✅ none - no elements match
println(numbers.none { it < 0 })  // true
println(numbers.none { it % 2 == 0 })  // false
```

## Sorting

### sorted and sortedBy

Sort collections.

```kotlin
// ✅ sorted - natural order
val numbers = listOf(3, 1, 4, 1, 5, 9, 2, 6)
println(numbers.sorted())  // [1, 1, 2, 3, 4, 5, 6, 9]

// ✅ sortedDescending
println(numbers.sortedDescending())  // [9, 6, 5, 4, 3, 2, 1, 1]

// ✅ sortedBy - sort by selector
data class Person(val name: String, val age: Int)
val people = listOf(
  Person("Charlie", 30),
  Person("Alice", 25),
  Person("Bob", 30)
)

val sortedByName = people.sortedBy { it.name }
println(sortedByName)  // [Alice, Bob, Charlie]

val sortedByAge = people.sortedBy { it.age }
println(sortedByAge)  // [Alice(25), Charlie(30), Bob(30)]
```

### sortedWith - Custom Comparator

Complex sorting with custom comparator.

```kotlin
// ✅ sortedWith with compareBy
data class Product(val name: String, val price: Double, val rating: Int)
val products = listOf(
  Product("Widget", 19.99, 4),
  Product("Gadget", 29.99, 5),
  Product("Doohickey", 19.99, 3)
)

// Sort by price, then by rating
val sorted = products.sortedWith(
  compareBy({ it.price }, { -it.rating })  // Negative for descending
)
println(sorted)
// [Doohickey(19.99, 3), Widget(19.99, 4), Gadget(29.99, 5)]

// ✅ sortedWith with custom comparison
val customSorted = products.sortedWith { p1, p2 ->
  when {
    p1.price != p2.price -> p1.price.compareTo(p2.price)
    else -> p2.rating.compareTo(p1.rating)  // Descending rating
  }
}
```

## Distinct and Unique

### distinct and distinctBy

Remove duplicates from collections.

```kotlin
// ✅ distinct - remove duplicates
val numbers = listOf(1, 2, 2, 3, 3, 3, 4)
println(numbers.distinct())  // [1, 2, 3, 4]

// ✅ distinctBy - unique by selector
data class Person(val name: String, val age: Int)
val people = listOf(
  Person("Alice", 30),
  Person("Bob", 25),
  Person("Alice", 35),  // Same name, different age
  Person("Charlie", 30)
)

val uniqueByName = people.distinctBy { it.name }
println(uniqueByName)  // [Alice(30), Bob(25), Charlie(30)]

val uniqueByAge = people.distinctBy { it.age }
println(uniqueByAge)  // [Alice(30), Bob(25)]
```

## Sequences vs Collections

### When to Use Sequences

Sequences provide lazy evaluation for large datasets.

```kotlin
// ❌ Collection operations are eager (intermediate lists created)
val result1 = (1..1_000_000)
  .map { it * 2 }        // Creates intermediate list of 1M elements
  .filter { it > 1000 }  // Creates another intermediate list
  .take(10)              // Creates final list of 10 elements

// ✅ Sequences are lazy (no intermediate collections)
val result2 = (1..1_000_000).asSequence()
  .map { it * 2 }        // No list created
  .filter { it > 1000 }  // No list created
  .take(10)              // Only 10 elements processed
  .toList()              // Materialize final result
```

**Use sequences when**:

- Working with large datasets
- Multiple operations chained
- Only need subset of results (take, first)

### Sequence Operations

Sequences support same operations as collections.

```kotlin
// ✅ Sequence pipeline
val result = sequenceOf(1, 2, 3, 4, 5)
  .map { it * 2 }
  .filter { it > 5 }
  .toList()
println(result)  // [6, 8, 10]

// ✅ generateSequence for infinite sequences
val fibonacci = generateSequence(1 to 1) { (a, b) -> b to (a + b) }
  .map { it.first }
  .take(10)
  .toList()
println(fibonacci)  // [1, 1, 2, 3, 5, 8, 13, 21, 34, 55]

// ✅ sequence builder
val customSequence = sequence {
  yield(1)
  yieldAll(listOf(2, 3, 4))
  yield(5)
}
println(customSequence.toList())  // [1, 2, 3, 4, 5]
```

## Windowing and Chunking

### windowed - Sliding Window

Process elements in sliding windows.

```kotlin
// ✅ windowed - sliding window
val numbers = listOf(1, 2, 3, 4, 5)
val windows = numbers.windowed(size = 3)
println(windows)  // [[1, 2, 3], [2, 3, 4], [3, 4, 5]]

// ✅ windowed with step
val stepped = numbers.windowed(size = 3, step = 2)
println(stepped)  // [[1, 2, 3], [3, 4, 5]]

// ✅ windowed with transformation
val sums = numbers.windowed(3) { it.sum() }
println(sums)  // [6, 9, 12]
```

### chunked - Fixed-Size Chunks

Divide collection into fixed-size chunks.

```kotlin
// ✅ chunked - non-overlapping chunks
val numbers = listOf(1, 2, 3, 4, 5, 6, 7, 8, 9)
val chunks = numbers.chunked(3)
println(chunks)  // [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

// ✅ Last chunk may be smaller
val numbers2 = listOf(1, 2, 3, 4, 5, 6, 7)
val chunks2 = numbers2.chunked(3)
println(chunks2)  // [[1, 2, 3], [4, 5, 6], [7]]

// ✅ chunked with transformation
val chunkSums = numbers.chunked(3) { it.sum() }
println(chunkSums)  // [6, 15, 24]
```

## Common Pitfalls

### Creating Intermediate Collections

```kotlin
// ❌ Multiple intermediate collections
val result = list
  .filter { it > 0 }     // Creates list 1
  .map { it * 2 }        // Creates list 2
  .sortedBy { it }       // Creates list 3
  .take(10)              // Creates list 4

// ✅ Use sequence for large datasets
val result = list.asSequence()
  .filter { it > 0 }
  .map { it * 2 }
  .sortedBy { it }
  .take(10)
  .toList()  // Only final list created
```

### Using filter + map Instead of mapNotNull

```kotlin
// ❌ Less efficient
val numbers = inputs
  .map { it.toIntOrNull() }
  .filterNotNull()

// ✅ More efficient
val numbers = inputs.mapNotNull { it.toIntOrNull() }
```

### Forgetting Immutability

```kotlin
// ❌ List operations return new lists
val original = listOf(1, 2, 3)
original.plus(4)  // Returns new list, original unchanged
println(original)  // [1, 2, 3]

// ✅ Assign result
val updated = original.plus(4)
println(updated)  // [1, 2, 3, 4]
```

## Variations

### Custom Collection Operations

Extend collections with domain-specific operations.

```kotlin
// ✅ Extension functions on collections
fun List<Int>.median(): Double {
  val sorted = this.sorted()
  val middle = sorted.size / 2
  return if (sorted.size % 2 == 0) {
    (sorted[middle - 1] + sorted[middle]) / 2.0
  } else {
    sorted[middle].toDouble()
  }
}

val numbers = listOf(3, 1, 4, 1, 5, 9, 2, 6)
println(numbers.median())  // 3.5
```

### Zip and Unzip

Combine or separate paired collections.

```kotlin
// ✅ zip - combine two lists
val names = listOf("Alice", "Bob", "Charlie")
val ages = listOf(30, 25, 35)
val people = names.zip(ages)
println(people)  // [(Alice, 30), (Bob, 25), (Charlie, 35)]

// ✅ zip with transformation
val formatted = names.zip(ages) { name, age -> "$name is $age" }
println(formatted)  // [Alice is 30, Bob is 25, Charlie is 35]

// ✅ unzip - separate pairs
val (names2, ages2) = people.unzip()
println(names2)  // [Alice, Bob, Charlie]
println(ages2)   // [30, 25, 35]
```

## Related Patterns

**Learn more**:

- [Beginner Tutorial - Collections](/en/learn/software-engineering/programming-language/kotlin/tutorials/beginner#collections) - Collection fundamentals
- [Intermediate Tutorial - Sequences](/en/learn/software-engineering/programming-language/kotlin/tutorials/intermediate#sequences) - Lazy evaluation
- [Functional Programming](/en/learn/software-engineering/programming-language/kotlin/tutorials/beginner#functional-programming) - Functional collection patterns
- [Handle Collections](/en/learn/software-engineering/programming-language/kotlin/how-to/handle-collections-idiomatically) - Idiomatic patterns

**Cookbook recipes**:

- [Collection Operations](/en/learn/software-engineering/programming-language/kotlin/how-to/cookbook#collections) - Quick reference
- [Sequences](/en/learn/software-engineering/programming-language/kotlin/how-to/cookbook#sequences) - Lazy evaluation patterns
- [Grouping and Aggregation](/en/learn/software-engineering/programming-language/kotlin/how-to/cookbook#grouping) - Advanced aggregations
