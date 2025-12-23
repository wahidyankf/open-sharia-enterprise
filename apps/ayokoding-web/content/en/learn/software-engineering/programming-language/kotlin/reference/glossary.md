---
title: "Glossary"
date: 2025-12-18T00:00:00+07:00
draft: false
weight: 1000040
description: Kotlin-specific terminology and definitions for learners and developers
---

**Comprehensive glossary** of Kotlin-specific terms, concepts, and language features. Organized alphabetically for quick reference.

## A

### Actual Declaration

**Definition**: Platform-specific implementation of an `expect` declaration in multiplatform projects.

**Example**:

```kotlin
// Common code
expect fun platformName(): String

// JVM actual
actual fun platformName() = "JVM"

// JS actual
actual fun platformName() = "JavaScript"
```

**See Also**: Expect Declaration, Multiplatform

### Apply

**Definition**: Scope function that executes a block on an object and returns the object. Uses `this` context.

**Example**:

```kotlin
val person = Person("Alice", 25).apply {
    celebrateBirthday()
    println("Age is now $age")
}
```

**See Also**: Scope Functions, Let, Also, Run, With

## B

### Backing Field

**Definition**: Automatically generated field for a property that stores its value.

**Example**:

```kotlin
var name: String = ""
    get() = field.uppercase()
    set(value) {
        field = value.trim()
    }
```

**See Also**: Property, Getter, Setter

### Builder Pattern

**Definition**: Pattern for constructing complex objects, commonly implemented with `apply` or DSL syntax in Kotlin.

**Example**:

```kotlin
val person = Person("Alice", 25).apply {
    email = "alice@example.com"
    address = "123 Main St"
}
```

**See Also**: Apply, DSL, Scope Functions

## C

### Companion Object

**Definition**: Object declared inside a class with `companion` keyword. Provides class-level (static-like) members.

**Example**:

```kotlin
class User(val name: String) {
    companion object {
        const val MAX_NAME_LENGTH = 50

        fun create(name: String): User {
            require(name.length <= MAX_NAME_LENGTH)
            return User(name)
        }
    }
}

val user = User.create("Alice")
```

**See Also**: Object Declaration, Static Methods

### Coroutine

**Definition**: Lightweight thread that can be suspended and resumed without blocking the underlying thread.

**Example**:

```kotlin
launch {
    delay(1000)
    println("World")
}
println("Hello")
```

**See Also**: Suspend Function, Launch, Async, Structured Concurrency

### Coroutine Context

**Definition**: Set of elements that define the behavior of a coroutine (dispatcher, job, exception handler).

**Example**:

```kotlin
launch(Dispatchers.IO + CoroutineName("MyCoroutine")) {
    // Runs on IO dispatcher with name "MyCoroutine"
}
```

**See Also**: Dispatcher, Job, CoroutineScope

### Crossinline

**Definition**: Modifier for lambda parameters that prevents non-local returns but allows inlining.

**Example**:

```kotlin
inline fun runTwice(crossinline action: () -> Unit) {
    action()
    action()
}
```

**See Also**: Inline Function, Noinline, Non-Local Return

## D

### Data Class

**Definition**: Class with `data` modifier that automatically generates `equals()`, `hashCode()`, `toString()`, `copy()`, and component functions.

**Example**:

```kotlin
data class User(val id: Int, val name: String, val email: String)

val user = User(1, "Alice", "alice@example.com")
val copy = user.copy(name = "Bob")
val (id, name, email) = user  // Destructuring
```

**See Also**: Destructuring, Copy Function, Component Functions

### Delegation

**Definition**: Pattern where an object delegates operations to another object. Kotlin provides language-level support with `by` keyword.

**Example**:

```kotlin
interface Base {
    fun print()
}

class BaseImpl : Base {
    override fun print() = println("BaseImpl")
}

class Derived(b: Base) : Base by b
```

**See Also**: Property Delegation, Lazy, Observable

### Destructuring Declaration

**Definition**: Syntax that unpacks an object into multiple variables using component functions.

**Example**:

```kotlin
val (name, age) = person
val (key, value) = mapEntry
for ((index, item) in list.withIndex()) {
    println("$index: $item")
}
```

**See Also**: Data Class, Component Functions

### Dispatcher

**Definition**: Determines which thread or threads a coroutine runs on.

**Example**:

```kotlin
launch(Dispatchers.Default) { }  // CPU-intensive work
launch(Dispatchers.IO) { }       // I/O operations
launch(Dispatchers.Main) { }     // UI updates (Android/Desktop)
launch(Dispatchers.Unconfined) { }  // Not confined to specific thread
```

**See Also**: Coroutine, Coroutine Context

### DSL (Domain-Specific Language)

**Definition**: Specialized syntax for a specific problem domain, built using Kotlin features like lambdas with receivers and type-safe builders.

**Example**:

```kotlin
html {
    head {
        title("My Page")
    }
    body {
        h1("Welcome")
        p("Hello, world!")
    }
}
```

**See Also**: Lambda with Receiver, @DslMarker, Type-Safe Builder

## E

### Elvis Operator

**Definition**: Binary operator `?:` that returns the left side if not null, otherwise the right side.

**Example**:

```kotlin
val length = name?.length ?: 0
val result = nullableValue ?: return
val user = findUser(id) ?: throw UserNotFoundException()
```

**See Also**: Nullable Type, Safe Call Operator

### Expect Declaration

**Definition**: Declaration in common code that expects platform-specific implementations in multiplatform projects.

**Example**:

```kotlin
expect fun currentTimeMillis(): Long
```

**See Also**: Actual Declaration, Multiplatform

### Expression

**Definition**: Code that produces a value. In Kotlin, `if`, `when`, and `try` are expressions (unlike Java where they're statements).

**Example**:

```kotlin
val max = if (a > b) a else b
val result = when (x) {
    1 -> "One"
    2 -> "Two"
    else -> "Other"
}
```

**See Also**: Statement, If Expression, When Expression

### Extension Function

**Definition**: Function that adds new functionality to an existing class without inheriting from it.

**Example**:

```kotlin
fun String.isPalindrome(): Boolean {
    return this == this.reversed()
}

"racecar".isPalindrome()  // true
```

**See Also**: Extension Property, Receiver, This

## F

### Flow

**Definition**: Cold asynchronous stream that emits values sequentially and can be transformed with operators.

**Example**:

```kotlin
flow {
    for (i in 1..5) {
        delay(100)
        emit(i)
    }
}.collect { value ->
    println(value)
}
```

**See Also**: StateFlow, SharedFlow, Collect, Emit

## G

### Getter

**Definition**: Function that returns the value of a property. Can be custom-defined in Kotlin.

**Example**:

```kotlin
val isEmpty: Boolean
    get() = size == 0
```

**See Also**: Setter, Property, Backing Field

## I

### Immutable

**Definition**: Data that cannot be changed after creation. In Kotlin, `val` properties and read-only collections are immutable.

**Example**:

```kotlin
val name = "Alice"  // Cannot reassign
val list = listOf(1, 2, 3)  // Read-only collection
```

**See Also**: Val, Mutable, Read-Only Collection

### Infix Function

**Definition**: Function that can be called without dot and parentheses, using infix notation.

**Example**:

```kotlin
infix fun Int.times(str: String) = str.repeat(this)

val repeated = 3 times "Ha"  // "HaHaHa"

// Built-in infix functions
val pair = "key" to "value"
val range = 1 until 10
```

**See Also**: Extension Function, Operator Overloading

### Inline Class (Value Class)

**Definition**: Wrapper class with single property that the compiler optimizes to avoid object allocation.

**Example**:

```kotlin
@JvmInline
value class UserId(val value: Int)

val id = UserId(123)  // No boxing at runtime
```

**See Also**: Inline Function, Performance Optimization

### Inline Function

**Definition**: Function whose code is inserted directly at call site by the compiler, avoiding function call overhead.

**Example**:

```kotlin
inline fun <T> measureTime(block: () -> T): Pair<T, Long> {
    val start = System.currentTimeMillis()
    val result = block()
    val time = System.currentTimeMillis() - start
    return result to time
}
```

**See Also**: Inline Class, Reified Type Parameter, Crossinline, Noinline

### Init Block

**Definition**: Initialization block in a class that runs when an instance is created.

**Example**:

```kotlin
class Person(val name: String, var age: Int) {
    init {
        require(age >= 0) { "Age must be positive" }
        println("Person created: $name")
    }
}
```

**See Also**: Constructor, Primary Constructor, Secondary Constructor

## J

### Job

**Definition**: Cancellable coroutine lifecycle handle that can be used to wait for completion or cancel execution.

**Example**:

```kotlin
val job = launch {
    delay(1000)
    println("Done")
}

job.join()    // Wait for completion
job.cancel()  // Cancel execution
```

**See Also**: Coroutine, Launch, Structured Concurrency

## K

### K2 Compiler

**Definition**: Modern Kotlin compiler with improved performance, better error messages, and enhanced type inference.

**See Also**: Compiler, KSP

### KSP (Kotlin Symbol Processing)

**Definition**: API for building lightweight compiler plugins that process Kotlin code annotations.

**See Also**: Annotation Processing, Compiler Plugin

## L

### Lambda

**Definition**: Anonymous function that can be passed as a parameter or stored in a variable.

**Example**:

```kotlin
val sum: (Int, Int) -> Int = { a, b -> a + b }
val double: (Int) -> Int = { it * 2 }

list.filter { it > 5 }
```

**See Also**: Higher-Order Function, Function Type, It

### Lambda with Receiver

**Definition**: Lambda that has an implicit receiver object, allowing direct access to its members.

**Example**:

```kotlin
fun buildString(builder: StringBuilder.() -> Unit): String {
    val sb = StringBuilder()
    sb.builder()
    return sb.toString()
}

val result = buildString {
    append("Hello")  // 'this' is StringBuilder
    append(", ")
    append("World")
}
```

**See Also**: DSL, Extension Function, This

### Late-Initialized Property (lateinit)

**Definition**: Non-null property that is initialized after construction, not in constructor.

**Example**:

```kotlin
class MyTest {
    lateinit var database: Database

    @BeforeEach
    fun setup() {
        database = Database.connect()
    }
}
```

**See Also**: Lazy, By Delegates, Null Safety

### Lazy Initialization

**Definition**: Property delegation pattern where the value is computed only on first access.

**Example**:

```kotlin
val expensiveValue: String by lazy {
    println("Computing value")
    "Result"
}
```

**See Also**: Property Delegation, Lateinit, By Delegates

### Let

**Definition**: Scope function that executes a block on a non-null object and returns the result. Uses `it` context.

**Example**:

```kotlin
val length = name?.let {
    println("Name: $it")
    it.length
}
```

**See Also**: Scope Functions, Apply, Also, Run, With

## M

### Multiplatform

**Definition**: Kotlin feature for sharing code across multiple platforms (JVM, JS, Native, Android, iOS).

**See Also**: Expect Declaration, Actual Declaration

### Mutable

**Definition**: Data that can be changed after creation. In Kotlin, `var` properties and mutable collections.

**Example**:

```kotlin
var count = 0  // Can reassign
val mutableList = mutableListOf(1, 2, 3)  // Can modify
```

**See Also**: Immutable, Var, Val

## N

### Noinline

**Definition**: Modifier for lambda parameters in inline functions to prevent that specific parameter from being inlined.

**Example**:

```kotlin
inline fun processData(
    noinline logger: () -> Unit,
    action: () -> Unit
) {
    // logger can be stored or passed to non-inline functions
    action()
}
```

**See Also**: Inline Function, Crossinline

### Non-Local Return

**Definition**: Return statement in a lambda that exits the enclosing function (only allowed in inline lambdas).

**Example**:

```kotlin
fun findValue(list: List<Int>): Int {
    list.forEach {
        if (it > 5) return it  // Returns from findValue
    }
    return -1
}
```

**See Also**: Inline Function, Return, Crossinline

### Nothing Type

**Definition**: Type with no instances, used for functions that never return normally (always throw or loop forever).

**Example**:

```kotlin
fun fail(message: String): Nothing {
    throw IllegalStateException(message)
}

val value = nullableValue ?: fail("Value is null")
```

**See Also**: Unit, Type System

### Nullable Type

**Definition**: Type that can hold `null` value, denoted with `?` suffix.

**Example**:

```kotlin
val canBeNull: String? = null
val cannotBeNull: String = "Never null"
```

**See Also**: Null Safety, Safe Call, Elvis Operator, Not-Null Assertion

### Null Safety

**Definition**: Kotlin's type system feature that distinguishes nullable and non-nullable types at compile time.

**See Also**: Nullable Type, Safe Call, Elvis Operator, Platform Type

## O

### Object Declaration

**Definition**: Singleton object declaration that creates a single instance.

**Example**:

```kotlin
object DatabaseConfig {
    val url = "jdbc:postgresql://localhost:5432/db"
    fun connect() = println("Connecting")
}

DatabaseConfig.connect()
```

**See Also**: Companion Object, Singleton

### Object Expression

**Definition**: Anonymous object created with `object` keyword, similar to Java anonymous classes.

**Example**:

```kotlin
val listener = object : ClickListener {
    override fun onClick() {
        println("Clicked")
    }
}
```

**See Also**: Object Declaration, Anonymous Class

### Observable Property

**Definition**: Property delegation pattern that triggers a callback when the value changes.

**Example**:

```kotlin
var name: String by Delegates.observable("Initial") { prop, old, new ->
    println("$old -> $new")
}
```

**See Also**: Property Delegation, Lazy, Delegates

### Operator Overloading

**Definition**: Defining custom behavior for operators like `+`, `-`, `*`, `[]`, etc.

**Example**:

```kotlin
data class Point(val x: Int, val y: Int) {
    operator fun plus(other: Point) = Point(x + other.x, y + other.y)
}

val p1 = Point(1, 2)
val p2 = Point(3, 4)
val p3 = p1 + p2  // Point(4, 6)
```

**See Also**: Infix Function, Conventions

## P

### Platform Type

**Definition**: Type from Java interop whose nullability is unknown. Denoted with `!` in error messages.

**Example**:

```kotlin
// Java method: public String getName() { ... }
val name = javaObject.getName()  // Platform type String!
```

**See Also**: Null Safety, Java Interop

### Primary Constructor

**Definition**: Main constructor declared in the class header.

**Example**:

```kotlin
class Person(val name: String, var age: Int)
```

**See Also**: Secondary Constructor, Init Block, Constructor

### Property

**Definition**: Class member that represents a value with optional getter and setter.

**Example**:

```kotlin
class Rectangle(val width: Int, val height: Int) {
    val area: Int
        get() = width * height
}
```

**See Also**: Val, Var, Getter, Setter, Backing Field

### Property Delegation

**Definition**: Pattern where property getter/setter operations are delegated to another object.

**Example**:

```kotlin
val lazyValue: String by lazy { "Computed" }
var observableValue: Int by Delegates.observable(0) { _, old, new ->
    println("$old -> $new")
}
```

**See Also**: Lazy, Observable, By Keyword

## R

### Range

**Definition**: Sequence of values with defined start and end, created with `..` or `until`.

**Example**:

```kotlin
for (i in 1..10) { }        // 1 to 10 (inclusive)
for (i in 1 until 10) { }   // 1 to 9 (exclusive)
for (i in 10 downTo 1) { }  // 10 to 1 (descending)
for (i in 1..10 step 2) { } // 1, 3, 5, 7, 9
```

**See Also**: For Loop, Progression

### Receiver

**Definition**: Object that an extension function or lambda with receiver operates on.

**Example**:

```kotlin
fun String.addExclamation(): String {
    return "$this!"  // 'this' is the receiver (String)
}
```

**See Also**: Extension Function, Lambda with Receiver, This

### Reified Type Parameter

**Definition**: Type parameter in inline functions that is available at runtime (not erased).

**Example**:

```kotlin
inline fun <reified T> isInstance(value: Any): Boolean {
    return value is T  // Can check type at runtime
}
```

**See Also**: Inline Function, Type Erasure, Generics

### Result Type

**Definition**: Built-in type for functional error handling that encapsulates success or failure.

**Example**:

```kotlin
fun divide(a: Int, b: Int): Result<Int> {
    return if (b == 0) {
        Result.failure(IllegalArgumentException("Division by zero"))
    } else {
        Result.success(a / b)
    }
}

divide(10, 2).onSuccess { println(it) }
```

**See Also**: Error Handling, Exception

### Run

**Definition**: Scope function that executes a block and returns the result. Uses `this` context.

**Example**:

```kotlin
val result = person.run {
    celebrateBirthday()
    "$name is now $age years old"
}
```

**See Also**: Scope Functions, Let, Apply, Also, With

## S

### Safe Call Operator

**Definition**: Operator `?.` that calls a method or accesses a property only if the receiver is not null.

**Example**:

```kotlin
val length = name?.length
val upper = name?.uppercase()
```

**See Also**: Nullable Type, Elvis Operator, Let

### Scope Function

**Definition**: Function that executes a block of code in the context of an object (let, run, with, apply, also).

**See Also**: Let, Run, With, Apply, Also

### Sealed Class

**Definition**: Abstract class with restricted subclass hierarchy (all subclasses must be in same file/module).

**Example**:

```kotlin
sealed class Result {
    data class Success(val data: String) : Result()
    data class Error(val message: String) : Result()
    object Loading : Result()
}

// Exhaustive when
when (result) {
    is Result.Success -> println(result.data)
    is Result.Error -> println(result.message)
    Result.Loading -> println("Loading...")
}
```

**See Also**: Data Class, Object, When Expression

### Setter

**Definition**: Function that sets the value of a property. Can be custom-defined in Kotlin.

**Example**:

```kotlin
var name: String = ""
    set(value) {
        field = value.trim()
    }
```

**See Also**: Getter, Property, Backing Field

### Smart Cast

**Definition**: Automatic type casting after a type check.

**Example**:

```kotlin
fun describe(obj: Any): String {
    if (obj is String) {
        return "Length: ${obj.length}"  // Auto-cast to String
    }
    return "Not a string"
}
```

**See Also**: Is Operator, Type Check, Casting

### Structured Concurrency

**Definition**: Coroutine principle where children coroutines are tied to parent scope and cancellation propagates.

**Example**:

```kotlin
suspend fun doWork() = coroutineScope {
    launch { task1() }
    launch { task2() }
    // Both tasks complete before function returns
}
```

**See Also**: Coroutine, CoroutineScope, Job

### Suspend Function

**Definition**: Function that can be paused and resumed, used with coroutines.

**Example**:

```kotlin
suspend fun fetchData(): String {
    delay(1000)
    return "Data"
}
```

**See Also**: Coroutine, Async, Launch

## T

### Trailing Lambda

**Definition**: Syntax where the last lambda parameter can be moved outside parentheses.

**Example**:

```kotlin
list.filter { it > 5 }
repeat(3) { println("Hello") }

// Instead of:
list.filter({ it > 5 })
```

**See Also**: Lambda, Higher-Order Function

### Type Alias

**Definition**: Alternative name for an existing type.

**Example**:

```kotlin
typealias UserMap = Map<String, User>
typealias Handler = (String) -> Unit

val users: UserMap = mapOf()
```

**See Also**: Type System

## U

### Unit

**Definition**: Type with single value (Unit), representing "no meaningful value". Equivalent to Java's `void`.

**Example**:

```kotlin
fun logMessage(msg: String): Unit {
    println(msg)
}

// Unit can be omitted
fun logMessage(msg: String) {
    println(msg)
}
```

**See Also**: Nothing, Return Type

## V

### Val

**Definition**: Keyword for read-only (immutable) property or variable.

**Example**:

```kotlin
val name = "Alice"
name = "Bob"  // Error: cannot reassign
```

**See Also**: Var, Immutable, Property

### Var

**Definition**: Keyword for mutable property or variable.

**Example**:

```kotlin
var count = 0
count = 1  // OK
```

**See Also**: Val, Mutable, Property

## W

### When Expression

**Definition**: Powerful replacement for switch statement that returns a value and supports pattern matching.

**Example**:

```kotlin
val result = when (x) {
    1 -> "One"
    2 -> "Two"
    in 3..10 -> "Between 3 and 10"
    is String -> "A string"
    else -> "Unknown"
}
```

**See Also**: If Expression, Pattern Matching, Sealed Class

### With

**Definition**: Scope function (not extension) that executes a block on an object and returns the result. Uses `this` context.

**Example**:

```kotlin
val result = with(person) {
    celebrateBirthday()
    "$name is $age years old"
}
```

**See Also**: Scope Functions, Let, Run, Apply, Also

## Learn More

**Comprehensive Documentation**:

- [Beginner Tutorial](/en/learn/software-engineering/programming-language/kotlin/tutorials/beginner) - Detailed explanations of concepts
- [Quick Start](/en/learn/software-engineering/programming-language/kotlin/tutorials/quick-start) - Overview of key features
- [How-To Guides](/en/learn/software-engineering/programming-language/kotlin/how-to) - Practical usage examples
- [Cheat Sheet](/en/learn/software-engineering/programming-language/kotlin/reference/cheat-sheet) - Quick syntax reference

**Official Resources**:

- [Kotlin Language Reference](https://kotlinlang.org/docs/reference/) - Official documentation
- [Kotlin Glossary](https://kotlinlang.org/docs/keyword-reference.html) - Official keyword reference
