---
title: "How to Use Inline Functions"
date: 2025-12-18T00:00:00+07:00
draft: false
weight: 1000012
description: "Optimize performance with inline functions and reified type parameters"
tags: ["kotlin", "inline-functions", "performance", "kotlin-features"]
categories: ["learn"]
---

## Problem

Higher-order functions (functions taking lambdas) create object allocations at runtime, impacting performance in hot code paths. Inline functions eliminate this overhead by copying function code to call sites. Additionally, regular functions can't access type parameters at runtime. Reified types solve this.

This guide shows when and how to use inline functions effectively.

## Inline Function Basics

### Creating Inline Functions

Mark functions with `inline` to copy code to call site.

```kotlin
// ✅ Inline function
inline fun measureTime(block: () -> Unit): Long {
  val start = System.currentTimeMillis()
  block()
  return System.currentTimeMillis() - start
}

// ✅ Usage - no lambda object created
val time = measureTime {
  // This code is inlined at call site
  processData()
}
```

**How it works**: Compiler copies function body and lambda to call site, eliminating object allocation.

### Performance Benefits

Inline functions avoid lambda allocation overhead.

```kotlin
// ❌ Regular function - creates lambda object
fun repeat(times: Int, action: () -> Unit) {
  for (i in 1..times) {
    action()  // Lambda object allocated
  }
}

// ✅ Inline - no allocation
inline fun repeatInline(times: Int, action: () -> Unit) {
  for (i in 1..times) {
    action()  // Inlined, no allocation
  }
}

// Compiled result for: repeatInline(3) { println("Hi") }
// Becomes:
// for (i in 1..3) {
//   println("Hi")
// }
```

**Use case**: Performance-critical code with lambdas.

## Reified Type Parameters

### Runtime Type Access

Use `reified` with inline functions to access type parameters at runtime.

```kotlin
// ❌ Regular function - type erased
fun <T> getClassName(): String {
  // return T::class.simpleName  // ❌ Cannot access T at runtime
  return "Unknown"
}

// ✅ Inline with reified
inline fun <reified T> getClassName(): String {
  return T::class.simpleName ?: "Unknown"
}

// ✅ Usage
println(getClassName<String>())  // "String"
println(getClassName<Int>())     // "Int"
println(getClassName<List<String>>())  // "List"
```

**How it works**: `inline` + `reified` preserves type information at call site.

### Type-Safe Casting

Use reified types for safe casting.

```kotlin
// ✅ Reified type casting
inline fun <reified T> Any.asInstance(): T? {
  return this as? T
}

// ✅ Usage
val obj: Any = "Hello"
val str: String? = obj.asInstance<String>()  // ✅ "Hello"
val num: Int? = obj.asInstance<Int>()        // null
```

### Filtering by Type

```kotlin
// ✅ Reified filter
inline fun <reified T> List<*>.filterByType(): List<T> {
  return this.filterIsInstance<T>()
}

// ✅ Usage
val mixed: List<Any> = listOf("Hello", 42, "World", 3.14, "Kotlin")
val strings = mixed.filterByType<String>()
println(strings)  // [Hello, World, Kotlin]
```

## Non-Local Returns

### Control Flow with Inline

Inline functions allow non-local returns from lambdas.

```kotlin
// ✅ Inline function enables return from lambda
inline fun processItems(items: List<String>, action: (String) -> Unit) {
  for (item in items) {
    action(item)
  }
}

fun findAndProcess(items: List<String>) {
  processItems(items) { item ->
    if (item == "stop") {
      return  // Returns from findAndProcess, not lambda
    }
    println(item)
  }
  println("Done")  // May not execute if "stop" found
}

// ❌ Regular function - compilation error
fun regularProcess(items: List<String>, action: (String) -> Unit) {
  for (item in items) {
    action(item)
  }
}

fun cannotReturn(items: List<String>) {
  regularProcess(items) { item ->
    // return  // ❌ Compilation error
    println(item)
  }
}
```

**Use case**: Early returns in functional code.

### Labeled Returns

Use labeled returns to return from lambda only.

```kotlin
inline fun processWithLabel(items: List<String>, action: (String) -> Unit) {
  for (item in items) {
    action(item)
  }
}

fun process(items: List<String>) {
  processWithLabel(items) { item ->
    if (item.isEmpty()) {
      return@processWithLabel  // Returns from lambda only
    }
    println(item)
  }
  println("Always executed")
}
```

## crossinline Modifier

### Disallowing Non-Local Returns

Use `crossinline` when lambda is stored or passed to non-inline context.

```kotlin
// ✅ crossinline prevents non-local returns
inline fun executeAsync(crossinline action: () -> Unit) {
  thread {  // Lambda stored in thread
    action()
  }
}

fun usage() {
  executeAsync {
    // return  // ❌ Compilation error - crossinline disallows this
    println("Executed asynchronously")
  }
}

// ✅ Without crossinline would cause runtime issues
inline fun broken(action: () -> Unit) {
  thread {
    action()  // If action has return, undefined behavior
  }
}
```

**Pattern**: Use `crossinline` when lambda escapes inline context.

## noinline Modifier

### Preventing Inlining

Use `noinline` when you need to pass lambda to non-inline function.

```kotlin
// ✅ noinline for specific parameters
inline fun process(
  inline action1: () -> Unit,
  noinline action2: () -> Unit  // Not inlined
) {
  action1()  // Inlined
  storeAction(action2)  // Can store because not inlined
}

fun storeAction(action: () -> Unit) {
  // Store for later use
}
```

**Use case**: Some lambdas inlined, others stored or passed.

## When to Use Inline

### Good Use Cases

```kotlin
// ✅ Higher-order functions with lambdas
inline fun <T> measure(block: () -> T): Pair<T, Long> {
  val start = System.nanoTime()
  val result = block()
  val time = System.nanoTime() - start
  return result to time
}

// ✅ Reified type parameters needed
inline fun <reified T> Gson.fromJson(json: String): T {
  return this.fromJson(json, T::class.java)
}

// ✅ Control flow (forEach, repeat)
inline fun repeat(times: Int, action: (Int) -> Unit) {
  for (i in 0 until times) {
    action(i)
  }
}
```

### Bad Use Cases

```kotlin
// ❌ Large function bodies - increases code size
inline fun hugeFunction() {
  // 100+ lines of code
  // Inlining duplicates all this code
}

// ❌ Recursive functions - can't inline
inline fun factorial(n: Int): Int {
  return if (n <= 1) 1 else n * factorial(n - 1)  // ❌ Won't inline
}

// ❌ No lambdas - unnecessary
inline fun add(a: Int, b: Int): Int {
  return a + b  // No benefit from inlining
}
```

**Guidelines**:

- Inline functions with lambda parameters
- Inline small utility functions - Inline when reified types needed
- Don't inline large functions
- Don't inline recursive functions

## Common Pitfalls

### Overusing Inline

```kotlin
// ❌ Inlining everything
inline fun add(a: Int, b: Int) = a + b
inline fun multiply(a: Int, b: Int) = a * b
// No lambdas - inlining provides no benefit

// ✅ Only inline when beneficial
fun add(a: Int, b: Int) = a + b
fun multiply(a: Int, b: Int) = a * b
```

**Why problematic**: Increases bytecode size without performance gain.

### Ignoring Code Size

```kotlin
// ❌ Large inline function called many times
inline fun processData(data: List<Int>): List<Int> {
  // 50+ lines of processing logic
  return data.filter { it > 0 }
    .map { it * 2 }
    .sorted()
    // ... many more operations
}

// Called 100 times = 5000+ lines of bytecode

// ✅ Regular function
fun processData(data: List<Int>): List<Int> {
  // Large logic stays in one place
  return data.filter { it > 0 }
    .map { it * 2 }
    .sorted()
}
```

**Why problematic**: Excessive code duplication.

## Variations

### Inline Properties

Inline property accessors for zero overhead.

```kotlin
class Point(val x: Double, val y: Double) {
  // ✅ Inline property
  inline val magnitude: Double
    get() = Math.sqrt(x * x + y * y)
}

// Call site inlines getter
val point = Point(3.0, 4.0)
println(point.magnitude)  // Math.sqrt(3.0 * 3.0 + 4.0 * 4.0) inlined
```

### Inline Extension Functions

```kotlin
// ✅ Inline extension
inline fun <T> List<T>.second(): T {
  return this[1]
}

// ✅ With reified types
inline fun <reified T> List<*>.countOfType(): Int {
  return this.count { it is T }
}
```

## Related Patterns

**Learn more**:

- [Extension Functions](/en/learn/swe/prog-lang/kotlin/how-to/extension-functions) - Often combined with inline
- [Advanced Tutorial - Compiler Internals](/en/learn/swe/prog-lang/kotlin/tutorials/advanced#compiler-internals) - How inline works
- [Intermediate Tutorial - Performance](/en/learn/swe/prog-lang/kotlin/tutorials/intermediate#performance-optimization) - When to optimize

**Cookbook recipes**:

- [Inline Performance](/en/learn/swe/prog-lang/kotlin/how-to/cookbook#inline-performance) - Performance patterns
- [Avoiding Allocations](/en/learn/swe/prog-lang/kotlin/how-to/cookbook#avoiding-allocations) - Memory optimization
