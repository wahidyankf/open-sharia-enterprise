---
title: "How to Work with Coroutines Basics"
date: 2025-12-18T00:00:00+07:00
draft: false
weight: 615
description: "Master Kotlin coroutines for asynchronous programming"
tags: ["kotlin", "coroutines", "async", "concurrency"]
categories: ["learn"]
---

## Problem

Traditional threading is complex, resource-intensive, and error-prone. Callback-based async code leads to callback hell. Kotlin coroutines provide lightweight, structured concurrency that's easier to write and understand.

This guide shows how to use coroutines for asynchronous operations.

## Coroutine Basics

### Launching Coroutines

Use `launch` for fire-and-forget coroutines.

```kotlin
import kotlinx.coroutines.*

fun main() = runBlocking {  // Creates coroutine scope
  // ✅ Launch coroutine
  launch {
    delay(1000)  // Suspend for 1 second
    println("World!")
  }

  println("Hello")
  // Output: Hello (immediately)
  //         World! (after 1 second)
}
```

**How it works**: `launch` starts a coroutine that runs concurrently.

### Async and Await

Use `async` when you need a result.

```kotlin
fun main() = runBlocking {
  // ✅ Async with result
  val deferred = async {
    delay(1000)
    "Result"
  }

  println("Started")
  val result = deferred.await()  // Wait for result
  println(result)
}

// ✅ Multiple async operations
fun main() = runBlocking {
  val result1 = async { fetchUser() }
  val result2 = async { fetchProfile() }

  // Both run concurrently
  val user = result1.await()
  val profile = result2.await()
}
```

**Pattern**: `async` for concurrent computation, `await` for result.

## Suspend Functions

### Creating Suspend Functions

Suspend functions can call other suspend functions.

```kotlin
// ✅ Suspend function
suspend fun fetchData(): String {
  delay(1000)  // Suspend, don't block
  return "Data"
}

suspend fun processData(): String {
  val data = fetchData()  // Can call suspend function
  return data.uppercase()
}

fun main() = runBlocking {
  val result = processData()
  println(result)
}
```

**Rule**: Suspend functions can only be called from coroutines or other suspend functions.

### Real-World Example

```kotlin
suspend fun getUser(id: String): User {
  return withContext(Dispatchers.IO) {
    database.findUser(id)  // IO operation
  }
}

suspend fun getUserProfile(id: String): Profile {
  val user = getUser(id)
  return user.profile
}

// ✅ Usage
fun main() = runBlocking {
  val profile = getUserProfile("123")
  println(profile)
}
```

## Coroutine Scope

### Structured Concurrency

Coroutines must run in a scope.

```kotlin
// ✅ runBlocking - blocks current thread
fun main() = runBlocking {
  launch { delay(1000); println("Task") }
  println("Main")
}

// ✅ coroutineScope - suspends, doesn't block
suspend fun doWork() = coroutineScope {
  launch { delay(1000); println("Work 1") }
  launch { delay(1000); println("Work 2") }
  // Waits for all children
}

fun main() = runBlocking {
  doWork()
  println("Done")
}
```

**Pattern**: `coroutineScope` suspends until all children complete.

### GlobalScope (Avoid)

```kotlin
// ❌ GlobalScope ties to application lifetime
GlobalScope.launch {
  // Runs beyond current scope
  delay(1000)
  println("Task")
}
// May not complete before app exits

// ✅ Use structured scope
runBlocking {
  launch {
    delay(1000)
    println("Task")
  }
}
```

## Cancellation

### Canceling Coroutines

Coroutines are cooperative - they must check cancellation.

```kotlin
fun main() = runBlocking {
  val job = launch {
    repeat(10) { i ->
      println("Task $i")
      delay(500)  // Suspension point checks cancellation
    }
  }

  delay(1500)
  job.cancel()  // Cancel after 1.5 seconds
  job.join()    // Wait for cancellation
  println("Canceled")
}
```

**How it works**: `delay` and other suspend functions check `isActive`.

### Cancellation-Aware Code

```kotlin
// ✅ Check isActive in loops
fun main() = runBlocking {
  val job = launch(Dispatchers.Default) {
    var i = 0
    while (isActive) {  // Check cancellation
      i++
    }
    println("Computed $i iterations")
  }

  delay(100)
  job.cancelAndJoin()
}
```

## Exception Handling

### Try-Catch in Coroutines

Handle exceptions like regular code.

```kotlin
fun main() = runBlocking {
  val job = launch {
    try {
      val data = fetchData()
      process(data)
    } catch (e: Exception) {
      println("Error: ${e.message}")
    }
  }
}
```

### Async Exception Handling

```kotlin
fun main() = runBlocking {
  val deferred = async {
    throw Exception("Error!")
  }

  try {
    deferred.await()  // Exception thrown here
  } catch (e: Exception) {
    println("Caught: ${e.message}")
  }
}
```

## Common Pitfalls

### Blocking the Main Thread

```kotlin
// ❌ Blocks main thread
fun main() {
  runBlocking {
    delay(1000)
  }
}

// ✅ Don't block in production
class MyService {
  private val scope = CoroutineScope(Dispatchers.Default)

  fun doAsync() {
    scope.launch {
      // Non-blocking
    }
  }
}
```

### Forgetting to Wait

```kotlin
// ❌ Coroutine may not complete
fun main() = runBlocking {
  launch {
    delay(1000)
    println("Task")
  }
  // Exits immediately
}

// ✅ Wait with join
fun main() = runBlocking {
  val job = launch {
    delay(1000)
    println("Task")
  }
  job.join()  // Wait for completion
}
```

## Related Patterns

**Learn more**:

- [Flow and State Management](/en/learn/swe/prog-lang/kotlin/how-to/flow-state-management) - Reactive streams with coroutines
- [Intermediate Tutorial - Coroutines](/en/learn/swe/prog-lang/kotlin/tutorials/intermediate#advanced-coroutines) - Advanced patterns
- [Quick Start - Coroutines](/en/learn/swe/prog-lang/kotlin/tutorials/quick-start#coroutines-basics) - Introduction

**Cookbook recipes**:

- [Coroutine Basics](/en/learn/swe/prog-lang/kotlin/how-to/cookbook#coroutine-basics) - Common patterns
- [Timeout and Retry](/en/learn/swe/prog-lang/kotlin/how-to/cookbook#timeout-retry) - Resilient operations
