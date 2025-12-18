---
title: "How to Use Flow and State Management"
date: 2025-12-18T00:00:00+07:00
draft: false
weight: 612
description: "Manage reactive state with Kotlin Flow, StateFlow, and SharedFlow"
tags: ["kotlin", "flow", "state-management", "reactive", "coroutines"]
categories: ["learn"]
---

## Problem

Managing asynchronous data streams and application state requires handling multiple values over time. Callbacks lead to complexity. RxJava is powerful but has a steep learning curve. Kotlin Flow provides a simpler, coroutine-based solution for reactive streams.

This guide shows how to use Flow for reactive programming and state management.

## Flow Basics

### Creating Flows

Flows emit multiple values sequentially.

```kotlin
import kotlinx.coroutines.flow.*
import kotlinx.coroutines.*

// ✅ Simple flow
fun simpleFlow(): Flow<Int> = flow {
  for (i in 1..3) {
    delay(100)
    emit(i)  // Emit value
  }
}

// ✅ Collect flow
fun main() = runBlocking {
  simpleFlow().collect { value ->
    println(value)
  }
  // Output: 1, 2, 3 (with delays)
}
```

**How it works**: `flow { }` builder creates cold streams that execute on collection.

### Flow Builders

Multiple ways to create flows.

```kotlin
// ✅ flowOf - fixed values
val numbersFlow = flowOf(1, 2, 3, 4, 5)

// ✅ asFlow - from collections
val listFlow = listOf("a", "b", "c").asFlow()

// ✅ flow builder - custom logic
val customFlow = flow {
  repeat(3) { i ->
    emit(i * 2)
    delay(100)
  }
}

// ✅ channelFlow - concurrent emissions
val channelFlow = channelFlow {
  launch { send(1) }
  launch { send(2) }
}
```

## Flow Operators

### Transformation Operators

Transform flow values.

```kotlin
// ✅ map - transform each value
val squared = (1..5).asFlow()
  .map { it * it }
// 1, 4, 9, 16, 25

// ✅ filter - select values
val evens = (1..10).asFlow()
  .filter { it % 2 == 0 }
// 2, 4, 6, 8, 10

// ✅ transform - flexible transformation
val transformed = (1..3).asFlow()
  .transform { value ->
    emit(value)      // Emit original
    emit(value * 2)  // Emit doubled
  }
// 1, 2, 2, 4, 3, 6
```

### Terminal Operators

Collect flow results.

```kotlin
// ✅ collect - process each value
flow.collect { value ->
  println(value)
}

// ✅ toList - collect to list
val list: List<Int> = flow.toList()

// ✅ first - get first value
val first: Int = flow.first()

// ✅ reduce - accumulate
val sum = (1..5).asFlow().reduce { acc, value -> acc + value }
println(sum)  // 15
```

### Combining Flows

Combine multiple flows.

```kotlin
// ✅ zip - combine pairs
val flow1 = flowOf(1, 2, 3)
val flow2 = flowOf("a", "b", "c")
flow1.zip(flow2) { num, letter ->
  "$num$letter"
}.collect { println(it) }
// Output: 1a, 2b, 3c

// ✅ combine - latest from both
val combined = flow1.combine(flow2) { num, letter ->
  "$num-$letter"
}

// ✅ flatMapConcat - sequential flattening
val flattened = (1..3).asFlow()
  .flatMapConcat { value ->
    flow {
      emit("$value-a")
      emit("$value-b")
    }
  }
```

## StateFlow

### Hot State Stream

StateFlow maintains current state and notifies observers.

```kotlin
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow

// ✅ StateFlow for state management
class CounterViewModel {
  private val _count = MutableStateFlow(0)
  val count: StateFlow<Int> = _count

  fun increment() {
    _count.value++
  }

  fun decrement() {
    _count.value--
  }
}

// ✅ Usage
fun main() = runBlocking {
  val viewModel = CounterViewModel()

  // Collect state changes
  launch {
    viewModel.count.collect { count ->
      println("Count: $count")
    }
  }

  delay(100)
  viewModel.increment()  // Count: 1
  viewModel.increment()  // Count: 2
  viewModel.decrement()  // Count: 1
}
```

**Key feature**: Always has a current value, new collectors get latest value immediately.

### UI State Pattern

Model UI state with StateFlow.

```kotlin
// ✅ Sealed class for UI states
sealed class UiState<out T> {
  object Loading : UiState<Nothing>()
  data class Success<T>(val data: T) : UiState<T>()
  data class Error(val message: String) : UiState<Nothing>()
}

class UserViewModel {
  private val _uiState = MutableStateFlow<UiState<User>>(UiState.Loading)
  val uiState: StateFlow<UiState<User>> = _uiState

  suspend fun loadUser(id: String) {
    _uiState.value = UiState.Loading
    try {
      val user = fetchUser(id)
      _uiState.value = UiState.Success(user)
    } catch (e: Exception) {
      _uiState.value = UiState.Error(e.message ?: "Unknown error")
    }
  }
}

// ✅ Observe state
fun main() = runBlocking {
  val viewModel = UserViewModel()

  launch {
    viewModel.uiState.collect { state ->
      when (state) {
        UiState.Loading -> println("Loading...")
        is UiState.Success -> println("User: ${state.data}")
        is UiState.Error -> println("Error: ${state.message}")
      }
    }
  }

  viewModel.loadUser("123")
}
```

## SharedFlow

### Broadcasting Events

SharedFlow broadcasts values to multiple collectors.

```kotlin
import kotlinx.coroutines.flow.MutableSharedFlow

// ✅ SharedFlow for events
class EventBus {
  private val _events = MutableSharedFlow<Event>()
  val events = _events

  suspend fun emit(event: Event) {
    _events.emit(event)
  }
}

sealed class Event {
  data class UserLoggedIn(val userId: String) : Event()
  data class MessageReceived(val message: String) : Event()
}

// ✅ Multiple subscribers
fun main() = runBlocking {
  val eventBus = EventBus()

  // Subscriber 1
  launch {
    eventBus.events.collect { event ->
      println("Subscriber 1: $event")
    }
  }

  // Subscriber 2
  launch {
    eventBus.events.collect { event ->
      println("Subscriber 2: $event")
    }
  }

  delay(100)
  eventBus.emit(Event.UserLoggedIn("user123"))
  eventBus.emit(Event.MessageReceived("Hello"))
}
```

**Difference from StateFlow**: SharedFlow doesn't require initial value, can configure replay and buffer.

### SharedFlow Configuration

```kotlin
// ✅ Configure replay and buffer
val sharedFlow = MutableSharedFlow<Int>(
  replay = 1,        // New collectors receive last 1 value
  extraBufferCapacity = 10  // Buffer up to 10 values
)

// ✅ Usage
fun main() = runBlocking {
  sharedFlow.emit(1)
  sharedFlow.emit(2)
  sharedFlow.emit(3)

  // New collector receives last value (replay = 1)
  sharedFlow.collect { value ->
    println(value)  // Starts with 3
  }
}
```

## Flow Exception Handling

### Catch Operator

Handle exceptions in flows.

```kotlin
// ✅ catch operator
val flow = flow {
  emit(1)
  emit(2)
  throw RuntimeException("Error!")
}.catch { e ->
  println("Caught: ${e.message}")
  emit(-1)  // Emit fallback value
}

flow.collect { value ->
  println(value)
}
// Output: 1, 2, Caught: Error!, -1
```

### Try-Catch in Collect

```kotlin
// ✅ Try-catch around collect
flow {
  emit(1)
  throw RuntimeException("Error!")
}.collect { value ->
  try {
    println(value)
    processValue(value)
  } catch (e: Exception) {
    println("Error processing: ${e.message}")
  }
}
```

## Flow Context

### Flow on Different Dispatchers

Control execution context.

```kotlin
// ✅ flowOn - change upstream context
val flow = flow {
  println("Flow: ${Thread.currentThread().name}")
  emit(1)
}.flowOn(Dispatchers.IO)  // Upstream runs on IO

// ✅ Collect on different dispatcher
fun main() = runBlocking {
  flow.collect { value ->
    println("Collect: ${Thread.currentThread().name}")
  }
}
```

## Common Pitfalls

### Collecting Flow Multiple Times

```kotlin
// ❌ Cold flow re-executes for each collector
val flow = flow {
  println("Executing flow")
  emit(1)
}

flow.collect { println(it) }  // "Executing flow", 1
flow.collect { println(it) }  // "Executing flow", 1 (executes again!)

// ✅ Use StateFlow or SharedFlow for hot streams
val stateFlow = MutableStateFlow(1)
stateFlow.collect { println(it) }  // 1
stateFlow.collect { println(it) }  // 1 (no re-execution)
```

### Not Handling Cancellation

```kotlin
// ❌ Flow doesn't complete
fun main() = runBlocking {
  val job = launch {
    flow {
      while (true) {  // Infinite loop
        emit(1)
        delay(100)
      }
    }.collect { println(it) }
  }

  delay(500)
  job.cancel()  // Must cancel explicitly
}

// ✅ Use finite flows or check cancellation
flow {
  repeat(10) {
    emit(it)
    delay(100)
  }
}
```

### Forgetting flowOn

```kotlin
// ❌ Blocking main thread
fun main() = runBlocking {
  flow {
    // Heavy computation on main thread
    emit(expensiveComputation())
  }.collect { println(it) }
}

// ✅ Use flowOn for background work
fun main() = runBlocking {
  flow {
    emit(expensiveComputation())
  }
    .flowOn(Dispatchers.Default)  // Run on background thread
    .collect { println(it) }
}
```

## Variations

### StateFlow with Derived State

Derive state from other StateFlows.

```kotlin
class CartViewModel {
  private val _items = MutableStateFlow<List<Item>>(emptyList())
  val items: StateFlow<List<Item>> = _items

  // ✅ Derived state
  val totalPrice: StateFlow<Double> = _items
    .map { items -> items.sumOf { it.price } }
    .stateIn(
      scope = viewModelScope,
      started = SharingStarted.Lazily,
      initialValue = 0.0
    )

  fun addItem(item: Item) {
    _items.value = _items.value + item
  }
}
```

### Flow with Retry

Retry failed operations.

```kotlin
// ✅ Retry on failure
fun fetchDataFlow(): Flow<Data> = flow {
  emit(fetchFromNetwork())
}.retry(retries = 3) { cause ->
  delay(1000)  // Wait before retry
  cause is IOException
}
```

## Related Patterns

**Learn more**:

- [Coroutines Basics](/en/learn/swe/prog-lang/kotlin/how-to/handle-coroutines-and-async) - Foundation for Flow
- [Intermediate Tutorial - Flow](/en/learn/swe/prog-lang/kotlin/tutorials/intermediate#advanced-coroutines) - Advanced Flow patterns
- [Advanced Tutorial - Flow Operators](/en/learn/swe/prog-lang/kotlin/tutorials/advanced#advanced-coroutines) - Custom operators

**Cookbook recipes**:

- [Flow for Reactive Streams](/en/learn/swe/prog-lang/kotlin/how-to/cookbook#flow-reactive) - Practical Flow patterns
- [Observer Pattern with Flow](/en/learn/swe/prog-lang/kotlin/how-to/cookbook#observer-flow) - StateFlow/SharedFlow patterns
