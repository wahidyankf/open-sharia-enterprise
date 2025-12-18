---
title: Kotlin Cheat Sheet
date: 2025-12-18T00:00:00+07:00
draft: false
weight: 803
description: Quick reference for Kotlin syntax, operators, and common patterns
---

**Quick reference guide** for essential Kotlin syntax and patterns. Copy-paste ready snippets for daily development.

## Variables and Types

### Variable Declaration

```kotlin
val immutable = "Cannot change"     // Immutable (read-only)
var mutable = "Can change"          // Mutable
const val CONSTANT = "Compile-time" // Compile-time constant (top-level or object)

// Type inference
val age = 25                        // Int
val price = 19.99                   // Double
val name = "Alice"                  // String

// Explicit types
val count: Int = 10
val rate: Double = 4.5
val message: String = "Hello"
```

### Basic Types

```kotlin
// Numbers
val byte: Byte = 127
val short: Short = 32767
val int: Int = 2147483647
val long: Long = 9223372036854775807L
val float: Float = 3.14f
val double: Double = 3.14159265359

// Text
val char: Char = 'A'
val string: String = "Text"

// Boolean
val flag: Boolean = true

// Type conversion (explicit only)
val x: Int = 100
val y: Long = x.toLong()
val z: Double = x.toDouble()
```

### Nullable Types

```kotlin
val cannotBeNull: String = "Never null"
val canBeNull: String? = null

// Safe call
val length = canBeNull?.length      // Returns null if canBeNull is null

// Elvis operator (default value)
val len = canBeNull?.length ?: 0    // Returns 0 if null

// Let function (execute block if not null)
canBeNull?.let { println(it) }

// Not-null assertion (throws NPE if null)
val definitelyLength = canBeNull!!.length  // Use sparingly!
```

## Functions

### Function Declaration

```kotlin
// Basic function
fun greet(name: String): String {
    return "Hello, $name"
}

// Single-expression function
fun add(a: Int, b: Int): Int = a + b

// No return value (Unit)
fun logMessage(msg: String) {
    println(msg)
}

// Default parameters
fun greet(name: String, greeting: String = "Hello"): String {
    return "$greeting, $name"
}

// Named arguments
greet(name = "Alice", greeting = "Hi")

// Varargs
fun sum(vararg numbers: Int): Int = numbers.sum()
sum(1, 2, 3, 4, 5)
```

### Lambdas and Higher-Order Functions

```kotlin
// Lambda syntax
val multiply: (Int, Int) -> Int = { a, b -> a * b }
multiply(3, 4)  // 12

// Single parameter (implicit 'it')
val double: (Int) -> Int = { it * 2 }
double(5)  // 10

// Higher-order function
fun operate(x: Int, y: Int, op: (Int, Int) -> Int): Int {
    return op(x, y)
}
operate(10, 5, { a, b -> a + b })  // 15

// Trailing lambda syntax
operate(10, 5) { a, b -> a - b }  // 5
```

## Classes and Objects

### Class Declaration

```kotlin
// Basic class
class Person(val name: String, var age: Int)

// With custom constructor
class Person(val name: String, var age: Int) {
    init {
        require(age >= 0) { "Age must be positive" }
    }

    fun celebrateBirthday() {
        age++
    }
}

// Properties with custom accessors
class Rectangle(val width: Int, val height: Int) {
    val area: Int
        get() = width * height

    var isSquare: Boolean
        get() = width == height
        set(value) {
            if (value) println("Making square")
        }
}
```

### Data Classes

```kotlin
data class User(val id: Int, val name: String, val email: String)

val user = User(1, "Alice", "alice@example.com")

// Auto-generated methods
val copy = user.copy(name = "Bob")           // Copy with modifications
val (id, name, email) = user                 // Destructuring
println(user)                                // toString()
user == User(1, "Alice", "alice@example.com") // equals()
```

### Sealed Classes

```kotlin
sealed class Result {
    data class Success(val data: String) : Result()
    data class Error(val message: String) : Result()
    object Loading : Result()
}

// Exhaustive when expression
fun handleResult(result: Result): String = when (result) {
    is Result.Success -> result.data
    is Result.Error -> "Error: ${result.message}"
    Result.Loading -> "Loading..."
}
```

### Objects and Companions

```kotlin
// Singleton object
object DatabaseConfig {
    val url = "jdbc:postgresql://localhost:5432/db"
    fun connect() = println("Connecting to $url")
}
DatabaseConfig.connect()

// Companion object (static-like members)
class User(val name: String) {
    companion object {
        fun create(name: String): User {
            return User(name)
        }
    }
}
val user = User.create("Alice")
```

## Control Flow

### If Expression

```kotlin
// As expression (returns value)
val max = if (a > b) a else b

// Multi-line
val result = if (x > 0) {
    println("Positive")
    "positive"
} else {
    println("Non-positive")
    "non-positive"
}
```

### When Expression

```kotlin
// Like switch but powerful
val description = when (x) {
    1 -> "One"
    2 -> "Two"
    in 3..10 -> "Between 3 and 10"
    !in 11..20 -> "Not between 11 and 20"
    is String -> "A string"
    else -> "Unknown"
}

// Without argument (replaces if-else chain)
when {
    x < 0 -> println("Negative")
    x == 0 -> println("Zero")
    x > 0 -> println("Positive")
}
```

### Loops

```kotlin
// For loop
for (i in 1..5) {
    println(i)  // 1, 2, 3, 4, 5
}

// Ranges
for (i in 1 until 5) {     // 1, 2, 3, 4 (excludes 5)
    println(i)
}

for (i in 5 downTo 1) {    // 5, 4, 3, 2, 1
    println(i)
}

for (i in 1..10 step 2) {  // 1, 3, 5, 7, 9
    println(i)
}

// Iterate with index
for ((index, value) in list.withIndex()) {
    println("$index: $value")
}

// While
while (condition) {
    // code
}

// Do-while
do {
    // code
} while (condition)
```

## Collections

### Creating Collections

```kotlin
// Immutable (read-only)
val list = listOf(1, 2, 3)
val set = setOf("a", "b", "c")
val map = mapOf("key1" to "value1", "key2" to "value2")

// Mutable
val mutableList = mutableListOf(1, 2, 3)
val mutableSet = mutableSetOf("a", "b", "c")
val mutableMap = mutableMapOf("key1" to "value1")

// Array
val array = arrayOf(1, 2, 3)
val intArray = intArrayOf(1, 2, 3)  // Primitive array
```

### Collection Operations

```kotlin
val numbers = listOf(1, 2, 3, 4, 5)

// Transformation
numbers.map { it * 2 }              // [2, 4, 6, 8, 10]
numbers.mapNotNull { if (it > 2) it else null }  // [3, 4, 5]
numbers.flatMap { listOf(it, it * 10) }  // [1, 10, 2, 20, 3, 30, 4, 40, 5, 50]

// Filtering
numbers.filter { it > 2 }           // [3, 4, 5]
numbers.filterNot { it > 2 }        // [1, 2]
numbers.take(3)                     // [1, 2, 3]
numbers.drop(2)                     // [3, 4, 5]

// Aggregation
numbers.sum()                       // 15
numbers.average()                   // 3.0
numbers.reduce { acc, n -> acc + n }  // 15
numbers.fold(10) { acc, n -> acc + n }  // 25 (initial value 10)

// Predicates
numbers.any { it > 3 }              // true
numbers.all { it > 0 }              // true
numbers.none { it < 0 }             // true
numbers.find { it > 3 }             // 4 (first match)

// Map operations
val map = mapOf("a" to 1, "b" to 2)
map.keys                            // ["a", "b"]
map.values                          // [1, 2]
map["a"]                            // 1
map.getOrDefault("c", 0)            // 0
```

## Extension Functions

```kotlin
// Add method to existing class
fun String.isPalindrome(): Boolean {
    return this == this.reversed()
}

"racecar".isPalindrome()  // true

// Generic extension
fun <T> List<T>.secondOrNull(): T? {
    return if (size >= 2) this[1] else null
}

listOf(1, 2, 3).secondOrNull()  // 2

// Nullable receiver
fun String?.orDefault(default: String): String {
    return this ?: default
}

null.orDefault("N/A")  // "N/A"
```

## Scope Functions

```kotlin
val person = Person("Alice", 25)

// let: Use result, 'it' context
val nameLength = person.let {
    println(it.name)
    it.name.length
}

// run: Use result, 'this' context
val age = person.run {
    celebrateBirthday()
    age  // Return age
}

// with: Use result, 'this' context (not extension)
val info = with(person) {
    "$name is $age years old"
}

// apply: Use object, 'this' context (builder pattern)
val newPerson = Person("Bob", 30).apply {
    celebrateBirthday()
}

// also: Use object, 'it' context (side effects)
val logged = person.also {
    println("Person: ${it.name}")
}
```

## Coroutines

### Basic Coroutines

```kotlin
import kotlinx.coroutines.*

// Launch coroutine (fire and forget)
GlobalScope.launch {
    delay(1000)
    println("World")
}
println("Hello")

// Async/await (returns value)
suspend fun fetchData(): String {
    delay(1000)
    return "Data"
}

runBlocking {
    val result = async { fetchData() }
    println(result.await())
}

// Structured concurrency
suspend fun doWork() = coroutineScope {
    launch {
        delay(1000)
        println("Task 1")
    }
    launch {
        delay(500)
        println("Task 2")
    }
}
```

### Flow

```kotlin
import kotlinx.coroutines.flow.*

// Create Flow
val flow = flow {
    for (i in 1..5) {
        delay(100)
        emit(i)
    }
}

// Collect Flow
runBlocking {
    flow.collect { value ->
        println(value)
    }
}

// Flow operators
flow
    .map { it * 2 }
    .filter { it > 5 }
    .collect { println(it) }

// StateFlow (hot flow, has state)
val stateFlow = MutableStateFlow(0)
stateFlow.value = 10
stateFlow.collect { println(it) }
```

## Error Handling

### Try-Catch

```kotlin
// Basic try-catch
try {
    val result = riskyOperation()
} catch (e: Exception) {
    println("Error: ${e.message}")
} finally {
    cleanup()
}

// As expression
val result = try {
    parseValue(input)
} catch (e: NumberFormatException) {
    0
}
```

### Result Type

```kotlin
fun divide(a: Int, b: Int): Result<Int> {
    return if (b == 0) {
        Result.failure(IllegalArgumentException("Division by zero"))
    } else {
        Result.success(a / b)
    }
}

// Using Result
divide(10, 2)
    .onSuccess { println("Result: $it") }
    .onFailure { println("Error: ${it.message}") }

// Get or default
val value = divide(10, 0).getOrDefault(0)
```

## String Operations

```kotlin
// String templates
val name = "Alice"
val greeting = "Hello, $name"
val info = "Name length: ${name.length}"

// Multi-line strings
val text = """
    Line 1
    Line 2
    Line 3
""".trimIndent()

// String operations
"hello".uppercase()                 // "HELLO"
"HELLO".lowercase()                 // "hello"
"  text  ".trim()                   // "text"
"hello".startsWith("he")            // true
"hello".endsWith("lo")              // true
"hello".contains("ll")              // true
"a,b,c".split(",")                  // ["a", "b", "c"]
listOf("a", "b").joinToString(",")  // "a,b"
```

## Common Patterns

### Smart Casts

```kotlin
fun describe(obj: Any): String = when (obj) {
    is String -> "String of length ${obj.length}"
    is Int -> "Int with value $obj"
    is List<*> -> "List of size ${obj.size}"
    else -> "Unknown type"
}
```

### Delegation

```kotlin
// Property delegation
class LazyValue {
    val value: String by lazy {
        println("Computing value")
        "Hello"
    }
}

// Observable property
import kotlin.properties.Delegates

class User {
    var name: String by Delegates.observable("Initial") { prop, old, new ->
        println("$old -> $new")
    }
}

// Class delegation
interface Base {
    fun print()
}

class BaseImpl : Base {
    override fun print() = println("BaseImpl")
}

class Derived(b: Base) : Base by b

val base = BaseImpl()
val derived = Derived(base)
derived.print()  // Calls BaseImpl.print()
```

### Type Aliases

```kotlin
typealias UserMap = Map<String, User>
typealias Handler = (String) -> Unit

val users: UserMap = mapOf()
val handler: Handler = { println(it) }
```

## Testing Quick Reference

```kotlin
import kotlin.test.*

class UserTest {
    @Test
    fun `should create user with valid data`() {
        val user = User(1, "Alice", "alice@example.com")
        assertEquals("Alice", user.name)
        assertTrue(user.email.contains("@"))
    }

    @Test
    fun `should throw on invalid email`() {
        assertFailsWith<IllegalArgumentException> {
            User(1, "Bob", "invalid-email")
        }
    }
}
```

## Learn More

**Detailed Documentation**:

- [Quick Start Tutorial](/en/learn/swe/prog-lang/kotlin/tutorials/quick-start) - 12 Kotlin touchpoints
- [Beginner Tutorial](/en/learn/swe/prog-lang/kotlin/tutorials/beginner) - Comprehensive fundamentals
- [Cookbook](/en/learn/swe/prog-lang/kotlin/how-to/cookbook) - Practical recipes
- [How-To Guides](/en/learn/swe/prog-lang/kotlin/how-to) - Problem-solving guides

**Official Resources**:

- [Kotlin Language Reference](https://kotlinlang.org/docs/reference/) - Complete language documentation
- [Kotlin Standard Library API](https://kotlinlang.org/api/latest/jvm/stdlib/) - API documentation
