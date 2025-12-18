---
title: "How to Optimize Kotlin Performance"
date: 2025-12-18T14:50:04+07:00
draft: false
weight: 624
description: "Performance optimization techniques including inline functions, collections, memory management, and profiling"
tags: ["kotlin", "performance", "optimization", "profiling"]
categories: ["learn"]
---

## Problem

Applications may suffer from performance issues due to inefficient code patterns, excessive allocations, or inappropriate data structures. Performance optimization requires measuring before optimizing, understanding JVM behavior, and applying targeted improvements. Premature optimization wastes time, but strategic optimization yields significant gains.

This guide shows evidence-based performance optimization techniques for Kotlin.

## Profiling and Measurement

### Measure Before Optimizing

Always profile before optimizing to identify actual bottlenecks.

```kotlin
// ✅ Use measureTimeMillis for quick measurements
import kotlin.system.measureTimeMillis

val time = measureTimeMillis {
  // Code to measure
  val result = expensiveOperation()
}
println("Operation took $time ms")

// ✅ More detailed measurement with runBlocking
import kotlinx.coroutines.runBlocking
import kotlin.system.measureNanoTime

val nanos = measureNanoTime {
  processData()
}
println("Operation took ${nanos / 1_000_000.0} ms")
```

### Using JMH for Benchmarking

JMH (Java Microbenchmark Harness) provides accurate performance measurements.

**Setup (build.gradle.kts)**:

```kotlin
plugins {
  id("me.champeau.jmh") version "0.7.1"
}

dependencies {
  jmhImplementation("org.openjdk.jmh:jmh-core:1.36")
}
```

**Benchmark**:

```kotlin
import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

@State(Scope.Benchmark)
@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.MICROSECONDS)
open class CollectionBenchmark {
  private val data = (1..1000).toList()

  @Benchmark
  fun filterMap() {
    data.filter { it % 2 == 0 }.map { it * 2 }
  }

  @Benchmark
  fun filterMapSequence() {
    data.asSequence().filter { it % 2 == 0 }.map { it * 2 }.toList()
  }
}
```

**Run**: `./gradlew jmh`

### Profiling Tools

Use professional profilers for production analysis.

```kotlin
// ✅ Available profilers:
// - IntelliJ IDEA Profiler (built-in)
// - YourKit Java Profiler
// - JProfiler
// - VisualVM (free)
// - async-profiler (low overhead)

// ✅ Enable profiling in code
// YourKit example
import com.yourkit.api.Controller

val controller = Controller()
controller.startCPUProfiling(ProfilingModes.CPU_SAMPLING)
// ... code to profile
controller.stopCPUProfiling()
```

## Inline Functions

### When to Use Inline

Inline functions eliminate lambda allocation overhead.

```kotlin
// ❌ Non-inline - creates lambda object
fun measureTime(block: () -> Unit) {
  val start = System.nanoTime()
  block()  // Lambda allocated
  println("Time: ${System.nanoTime() - start}")
}

// ✅ Inline - no lambda allocation
inline fun measureTimeInline(block: () -> Unit) {
  val start = System.nanoTime()
  block()  // Code inlined, no allocation
  println("Time: ${System.nanoTime() - start}")
}

// ✅ Usage - inline version is faster
measureTimeInline {
  expensiveOperation()
}
```

**Performance gain**: Eliminates lambda object creation, reduces method call overhead.

### Inline with Reified Types

Use reified for generic type access without reflection overhead.

```kotlin
// ❌ Runtime type check requires Class parameter
fun <T> parseJson(json: String, clazz: Class<T>): T {
  return objectMapper.readValue(json, clazz)
}

val user = parseJson(json, User::class.java)  // Verbose

// ✅ Inline + reified - cleaner API
inline fun <reified T> parseJson(json: String): T {
  return objectMapper.readValue(json, T::class.java)
}

val user = parseJson<User>(json)  // Cleaner
```

### Inline for Small Utility Functions

Inline small frequently-called functions.

```kotlin
// ✅ Inline small utilities
inline fun <T> T.applyIf(condition: Boolean, block: T.() -> Unit): T {
  if (condition) block()
  return this
}

// ✅ Usage - no overhead
val result = StringBuilder()
  .applyIf(debug) { append("[DEBUG] ") }
  .append("Message")
  .toString()
```

**Guidelines**: Inline functions < 10-15 lines, used frequently.

## Collection Optimization

### Sequences for Large Datasets

Use sequences to avoid intermediate collection allocations.

```kotlin
// ❌ Eager evaluation - creates intermediate lists
val result = (1..1_000_000)
  .filter { it % 2 == 0 }      // List of 500k elements
  .map { it * 2 }              // Another list of 500k elements
  .take(10)                    // Final list of 10 elements

// ✅ Lazy evaluation - no intermediate collections
val result = (1..1_000_000).asSequence()
  .filter { it % 2 == 0 }      // No allocation
  .map { it * 2 }              // No allocation
  .take(10)                    // Only 10 elements processed
  .toList()
```

**Performance gain**: Reduces memory allocations, faster for large datasets with early termination.

### Choosing Right Collection Type

Select optimal collection for your use case.

```kotlin
// ✅ List - ordered, indexed access, duplicates allowed
val list = listOf(1, 2, 3, 2, 1)  // O(1) get by index

// ✅ Set - no duplicates, fast contains check
val set = setOf(1, 2, 3)  // O(1) contains check

// ✅ Map - key-value pairs, fast lookup
val map = mapOf("key1" to "value1")  // O(1) get by key

// ✅ ArrayDeque - efficient add/remove at both ends
val deque = ArrayDeque<Int>()  // O(1) addFirst/addLast

// ✅ LinkedHashMap - insertion-order map
val orderedMap = linkedMapOf("a" to 1, "b" to 2)
```

### Pre-sizing Collections

Avoid reallocations by pre-sizing mutable collections.

```kotlin
// ❌ Multiple reallocations as list grows
val list = mutableListOf<Int>()
for (i in 1..10000) {
  list.add(i)  // Reallocates when capacity exceeded
}

// ✅ Pre-size to avoid reallocations
val list = ArrayList<Int>(10000)  // Initial capacity
for (i in 1..10000) {
  list.add(i)  // No reallocations
}
```

### Array vs List

Use arrays for primitive types to avoid boxing.

```kotlin
// ❌ List<Int> uses Integer objects (boxing)
val list = List(1000) { it }  // 1000 Integer objects

// ✅ IntArray uses primitive ints
val array = IntArray(1000) { it }  // No boxing

// ✅ Specialized array types
val doubles = DoubleArray(100)  // Primitive doubles
val longs = LongArray(100)      // Primitive longs
val booleans = BooleanArray(100) // Primitive booleans
```

**Performance gain**: Eliminates boxing overhead, reduces memory usage.

## String Optimization

### String Concatenation

Use StringBuilder for multiple concatenations.

```kotlin
// ❌ Inefficient - creates multiple String objects
var result = ""
for (i in 1..1000) {
  result += "Item $i, "  // New String object each iteration
}

// ✅ Efficient - single StringBuilder
val result = buildString {
  for (i in 1..1000) {
    append("Item $i, ")
  }
}

// ✅ Alternative with joinToString
val result = (1..1000).joinToString(", ") { "Item $it" }
```

### String Templates vs Concatenation

String templates compile to efficient bytecode.

```kotlin
// ✅ String template (efficient)
val message = "User $name has $age years"

// ✅ Equivalent to:
val message = StringBuilder()
  .append("User ")
  .append(name)
  .append(" has ")
  .append(age)
  .append(" years")
  .toString()
```

**Myth**: String templates are slower - they compile to optimized code.

## Memory Management

### Avoid Unnecessary Object Creation

Reuse objects when possible.

```kotlin
// ❌ Creates new object each call
fun formatDate(timestamp: Long): String {
  val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
  return Instant.ofEpochMilli(timestamp)
    .atZone(ZoneId.systemDefault())
    .format(formatter)
}

// ✅ Reuse formatter (thread-local or synchronized)
class DateFormatter {
  companion object {
    private val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
  }

  fun formatDate(timestamp: Long): String {
    return Instant.ofEpochMilli(timestamp)
      .atZone(ZoneId.systemDefault())
      .format(formatter)
  }
}
```

### Object Pooling

Pool expensive objects for reuse.

```kotlin
// ✅ Simple object pool
class ObjectPool<T>(
  private val factory: () -> T,
  private val reset: (T) -> Unit,
  private val maxSize: Int = 10
) {
  private val available = ArrayDeque<T>()

  fun acquire(): T {
    return available.removeFirstOrNull() ?: factory()
  }

  fun release(obj: T) {
    if (available.size < maxSize) {
      reset(obj)
      available.addLast(obj)
    }
  }
}

// Usage
val bufferPool = ObjectPool(
  factory = { ByteBuffer.allocate(1024) },
  reset = { it.clear() }
)

val buffer = bufferPool.acquire()
try {
  // Use buffer
} finally {
  bufferPool.release(buffer)
}
```

### Value Classes for Wrapper-Free Types

Use value classes to avoid allocation overhead.

```kotlin
// ❌ Regular class - heap allocation
data class UserId(val value: String)

// ✅ Value class - no wrapper allocation at runtime
@JvmInline
value class UserId(val value: String)

// ✅ No object created - direct String usage
val id = UserId("user-123")  // No UserId object allocated
```

**Performance gain**: Zero-cost abstraction, no runtime wrapper.

## Coroutines Performance

### Dispatcher Selection

Choose appropriate dispatcher for workload.

```kotlin
// ✅ Dispatchers.Default - CPU-intensive work
withContext(Dispatchers.Default) {
  val result = complexCalculation()  // Uses CPU cores efficiently
}

// ✅ Dispatchers.IO - blocking I/O operations
withContext(Dispatchers.IO) {
  val data = database.query()  // Thread pool for I/O
}

// ✅ Dispatchers.Main - UI updates (Android/Desktop)
withContext(Dispatchers.Main) {
  updateUI(data)
}

// ✅ Custom dispatcher for fine-tuning
val customDispatcher = Executors.newFixedThreadPool(4).asCoroutineDispatcher()
withContext(customDispatcher) {
  // Custom thread pool
}
```

### Parallel Decomposition

Use async for parallel execution.

```kotlin
// ❌ Sequential - slow
suspend fun fetchUserData(userId: String): UserData {
  val user = fetchUser(userId)        // 100ms
  val profile = fetchProfile(userId)  // 100ms
  val orders = fetchOrders(userId)    // 100ms
  return UserData(user, profile, orders)  // Total: 300ms
}

// ✅ Parallel - fast
suspend fun fetchUserData(userId: String): UserData = coroutineScope {
  val user = async { fetchUser(userId) }
  val profile = async { fetchProfile(userId) }
  val orders = async { fetchOrders(userId) }
  UserData(user.await(), profile.await(), orders.await())  // Total: ~100ms
}
```

### Avoiding Channel Contention

Use appropriate channel capacity.

```kotlin
// ❌ Unbuffered channel - context switches on each send
val channel = Channel<Int>()

// ✅ Buffered channel - reduces context switches
val channel = Channel<Int>(capacity = 100)

// ✅ Conflated channel - only latest value matters
val channel = Channel<Int>(capacity = Channel.CONFLATED)

// ✅ Unlimited channel - no blocking (use carefully)
val channel = Channel<Int>(capacity = Channel.UNLIMITED)
```

## Data Class Optimization

### Minimize Data Class Properties

Keep data classes focused and small.

```kotlin
// ❌ Large data class - expensive copy/equals
data class User(
  val id: String,
  val firstName: String,
  val lastName: String,
  val email: String,
  val phone: String,
  val address: Address,
  val createdAt: Instant,
  val updatedAt: Instant,
  val preferences: Preferences,
  val metadata: Map<String, String>
  // ... 20 more properties
)

// ✅ Separate concerns
data class User(val id: String, val profile: UserProfile)
data class UserProfile(val firstName: String, val lastName: String, val email: String)
```

### Avoid Mutable Data Classes

Immutable data classes enable optimizations.

```kotlin
// ❌ Mutable data class - can't cache hashCode
data class Counter(var count: Int)

val c1 = Counter(0)
val set = mutableSetOf(c1)
c1.count = 5  // ❌ Breaks set invariants

// ✅ Immutable data class - safe caching
data class Counter(val count: Int) {
  fun increment() = copy(count = count + 1)
}
```

## Common Pitfalls

### Premature Optimization

```kotlin
// ❌ Over-optimizing readable code
fun process(items: List<Int>): Int {
  var sum = 0
  var i = 0
  val size = items.size
  while (i < size) {
    if (items[i] % 2 == 0) sum += items[i]
    i++
  }
  return sum
}

// ✅ Clear code (unless profiling shows bottleneck)
fun process(items: List<Int>): Int {
  return items.filter { it % 2 == 0 }.sum()
}
```

**Rule**: Optimize only proven bottlenecks identified by profiling.

### Over-Using Sequences

```kotlin
// ❌ Sequence overhead for small collections
val result = listOf(1, 2, 3).asSequence()
  .map { it * 2 }
  .toList()

// ✅ Direct operations faster for small collections
val result = listOf(1, 2, 3).map { it * 2 }
```

**Guideline**: Use sequences for collections > 100 elements or with early termination.

### Unnecessary Boxing

```kotlin
// ❌ List<Int> boxes integers
val numbers: List<Int> = (1..1000).toList()

// ✅ IntArray avoids boxing
val numbers = IntArray(1000) { it + 1 }
```

### Reflection Overuse

```kotlin
// ❌ Reflection in hot path
fun process(obj: Any) {
  val method = obj::class.members.find { it.name == "getValue" }
  method?.call(obj)  // Slow reflection call
}

// ✅ Direct call or interface
interface HasValue {
  fun getValue(): String
}

fun process(obj: HasValue) {
  obj.getValue()  // Fast direct call
}
```

## Variations

### Lazy Initialization for Expensive Objects

Defer initialization until needed.

```kotlin
// ✅ Lazy property
class Service {
  private val heavyResource by lazy {
    println("Initializing expensive resource")
    ExpensiveResource()
  }

  fun process() {
    heavyResource.doWork()  // Initialized on first call
  }
}
```

### Caching Expensive Computations

Cache results of expensive operations.

```kotlin
// ✅ Simple cache
class FibonacciCalculator {
  private val cache = mutableMapOf<Int, Long>()

  fun calculate(n: Int): Long {
    return cache.getOrPut(n) {
      when (n) {
        0 -> 0L
        1 -> 1L
        else -> calculate(n - 1) + calculate(n - 2)
      }
    }
  }
}

// ✅ LRU cache for memory control
class LRUCache<K, V>(private val maxSize: Int) : LinkedHashMap<K, V>(maxSize, 0.75f, true) {
  override fun removeEldestEntry(eldest: Map.Entry<K, V>?): Boolean {
    return size > maxSize
  }
}
```

### Batch Processing

Process items in batches to reduce overhead.

```kotlin
// ❌ Individual processing
suspend fun processItems(items: List<Item>) {
  items.forEach { item ->
    database.save(item)  // N database calls
  }
}

// ✅ Batch processing
suspend fun processItems(items: List<Item>) {
  items.chunked(100).forEach { batch ->
    database.saveBatch(batch)  // N/100 database calls
  }
}
```

### Memory-Mapped Files for Large Data

Use memory-mapped files for large file operations.

```kotlin
// ✅ Memory-mapped file
val channel = FileChannel.open(path, StandardOpenOption.READ)
val mappedBuffer = channel.map(FileChannel.MapMode.READ_ONLY, 0, channel.size())

// Fast sequential reads
while (mappedBuffer.hasRemaining()) {
  val byte = mappedBuffer.get()
  // Process byte
}
```

## Related Patterns

**Learn more**:

- [Intermediate Tutorial - Performance](/en/learn/swe/prog-lang/kotlin/tutorials/intermediate#performance) - Performance fundamentals
- [Advanced Tutorial - Optimization](/en/learn/swe/prog-lang/kotlin/tutorials/advanced#performance-optimization) - Advanced techniques
- [Inline Functions](/en/learn/swe/prog-lang/kotlin/how-to/use-inline-functions-reified) - Inline and reified patterns
- [Collections](/en/learn/swe/prog-lang/kotlin/how-to/work-with-collections) - Collection performance

**Cookbook recipes**:

- [Performance Patterns](/en/learn/swe/prog-lang/kotlin/how-to/cookbook#performance) - Quick reference
- [Memory Optimization](/en/learn/swe/prog-lang/kotlin/how-to/cookbook#memory) - Memory management
- [Coroutine Performance](/en/learn/swe/prog-lang/kotlin/how-to/cookbook#coroutine-performance) - Async optimization
