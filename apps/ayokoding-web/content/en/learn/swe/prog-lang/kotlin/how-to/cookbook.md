---
title: Cookbook
date: 2025-12-18T15:48:37+07:00
draft: false
description: Day-to-day recipes and solutions for common Kotlin programming problems - ready to copy and use
weight: 603
tags:
  - kotlin
  - cookbook
  - recipes
  - practical
  - solutions
  - reference
  - learn
---

---

# Kotlin Cookbook - Practical Recipes

**Solve common problems with ready-to-use recipes.** This cookbook contains practical, copy-paste-ready solutions for everyday Kotlin development. Perfect for finding solutions to problems you encounter in real projects.

## How to Use This Cookbook

- **Search** for your problem (CTRL+F)
- **Copy** the code example
- **Adapt** to your specific needs
- **Reference** the links for deeper learning

Each recipe includes:

- Problem statement
- Copy-paste-ready code
- Brief explanation
- Common variations

---

## Data Structures and Algorithms

### Recipe 1: Working with Immutable Collections

**Problem**: You need to create collections that cannot be modified after creation, ensuring thread-safety and preventing accidental mutations in concurrent environments.

**Solution**:

```kotlin
// Creating immutable collections
val numbers = listOf(1, 2, 3, 4, 5)
val names = setOf("Alice", "Bob", "Charlie")
val ages = mapOf("Alice" to 30, "Bob" to 25, "Charlie" to 35)

// Transforming immutable collections (returns new collections)
val doubled = numbers.map { it * 2 }  // [2, 4, 6, 8, 10]
val filtered = numbers.filter { it > 3 }  // [4, 5]
val sorted = names.sorted()  // [Alice, Bob, Charlie]

// Combining collections
val combined = numbers + listOf(6, 7)  // [1, 2, 3, 4, 5, 6, 7]
val union = names + setOf("David")  // [Alice, Bob, Charlie, David]

// Converting mutable to immutable
val mutableList = mutableListOf(1, 2, 3)
mutableList.add(4)
val immutableCopy = mutableList.toList()  // Creates immutable copy

// Working with indices
val indexed = numbers.withIndex()  // Sequence of IndexedValue
indexed.forEach { (index, value) ->
    println("Index $index: $value")
}

// Safe parallel access example
import kotlin.concurrent.thread

fun processDataSafely(data: List<Int>) {
    val threads = (1..5).map { threadId ->
        thread {
            // Safe: read-only access, no race conditions
            data.forEach { value ->
                println("Thread $threadId processing $value")
            }
        }
    }
    threads.forEach { it.join() }
}

processDataSafely(numbers)
```

**How It Works**: `listOf()`, `setOf()`, `mapOf()` return read-only interfaces (`List`, `Set`, `Map`). These interfaces don't provide mutation methods like `add()` or `remove()`. All transformation methods (`map`, `filter`, `sorted`, etc.) return new collections, preserving original data.

**Use Cases**:

- Public APIs and return values (prevent external modification)
- Shared state in concurrent code (thread-safe without locks)
- Functional transformations (chain operations without modifying original)
- Default method parameters (caller can't modify defaults)
- Data class collections (immutable prevents accidental changes)

**Learn More**: See [Beginner Tutorial - Collections](/en/learn/swe/prog-lang/kotlin/tutorials/beginner/#part-4-collections-and-sequences) for fundamentals.

---

### Recipe 2: Using Sequences for Lazy Evaluation

**Problem**: You need to chain multiple collection operations (map, filter, etc.) on large datasets without creating intermediate collections, or you want to work with infinite or computed sequences efficiently.

**Solution**:

```kotlin
// Problem: Eager evaluation creates intermediate lists
val numbers = (1..1_000_000).toList()
val result1 = numbers
    .map { it * 2 }      // Creates intermediate list of 1M items
    .filter { it > 100 } // Creates another intermediate list
    .take(10)            // Finally takes 10 items
// Total: Processed 1M items twice, created 2M intermediate items

// Solution: Lazy evaluation with sequences
val result2 = (1..1_000_000).asSequence()
    .map { it * 2 }      // No intermediate collection
    .filter { it > 100 } // No intermediate collection
    .take(10)            // Stops after finding 10 items
    .toList()            // Materialize final result
// Total: Processed ~50 items total (stops early with take)

// Creating sequences
val sequenceFromRange = (1..10).asSequence()
val sequenceFromList = listOf(1, 2, 3, 4, 5).asSequence()
val generatedSequence = generateSequence(1) { it + 1 }  // Infinite sequence

// Infinite sequences (compute on demand)
val fibonacci = generateSequence(Pair(0, 1)) { (a, b) ->
    Pair(b, a + b)
}.map { it.first }

// First 10 Fibonacci numbers
val first10Fib = fibonacci.take(10).toList()
println(first10Fib)  // [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]

// Reading large files lazily
import java.io.File

fun processLargeFile(path: String) {
    File(path).useLines { lines ->
        lines  // Sequence of lines
            .filter { it.startsWith("ERROR") }
            .map { it.uppercase() }
            .take(100)  // Process only first 100 matches
            .forEach { println(it) }
    }
    // File closed automatically after useLines block
}

// Custom sequence with yield
fun numbersWithMessages(): Sequence<String> = sequence {
    yield("Starting")

    for (i in 1..5) {
        yield("Number: $i")
    }

    yield("Finished")
}

numbersWithMessages().forEach { println(it) }

// Performance comparison
import kotlin.system.measureTimeMillis

fun comparePerformance() {
    val range = 1..10_000_000

    // Eager evaluation (collections)
    val timeEager = measureTimeMillis {
        range.toList()
            .map { it * 2 }
            .filter { it > 100 }
            .take(10)
    }

    // Lazy evaluation (sequences)
    val timeLazy = measureTimeMillis {
        range.asSequence()
            .map { it * 2 }
            .filter { it > 100 }
            .take(10)
            .toList()
    }

    println("Eager: ${timeEager}ms")
    println("Lazy: ${timeLazy}ms")  // Significantly faster
}
```

**How It Works**: Sequence operations are not executed until a terminal operation is called (`toList()`, `toSet()`, `first()`, `count()`, `forEach()`, etc.). Intermediate operations (`map`, `filter`) are just stored as transformations. Sequences process one element at a time through the entire pipeline, allowing early termination.

**Use Cases**:

- Large data processing (millions of records)
- Multiple transformations (5+ operations chained)
- Early termination (using `first()`, `take()`, `any()`)
- Infinite data (pagination, random number generation)
- File processing (without loading into memory)

**Learn More**: See [Beginner Tutorial - Collections](/en/learn/swe/prog-lang/kotlin/tutorials/beginner/#part-4-collections-and-sequences) for sequence basics.

---

### Recipe 3: Implementing Custom Data Structures with Sealed Classes

**Problem**: You need to create custom data structures (like trees, linked lists, or result types) with exhaustive pattern matching and compile-time type safety.

**Solution**:

```kotlin
// Binary Tree with sealed class
sealed class BinaryTree<out T> {
    object Empty : BinaryTree<Nothing>()

    data class Node<T>(
        val value: T,
        val left: BinaryTree<T> = Empty,
        val right: BinaryTree<T> = Empty
    ) : BinaryTree<T>()

    // Type-safe operations with exhaustive when
    fun <R> fold(
        onEmpty: () -> R,
        onNode: (T, R, R) -> R
    ): R = when (this) {
        is Empty -> onEmpty()
        is Node -> onNode(
            value,
            left.fold(onEmpty, onNode),
            right.fold(onEmpty, onNode)
        )
    }

    fun contains(element: @UnsafeVariance T): Boolean = when (this) {
        is Empty -> false
        is Node -> value == element ||
                   left.contains(element) ||
                   right.contains(element)
    }

    fun size(): Int = fold(
        onEmpty = { 0 },
        onNode = { _, leftSize, rightSize -> 1 + leftSize + rightSize }
    )
}

// Using the tree
val tree = BinaryTree.Node(
    value = 5,
    left = BinaryTree.Node(
        value = 3,
        left = BinaryTree.Node(1),
        right = BinaryTree.Node(4)
    ),
    right = BinaryTree.Node(
        value = 7,
        left = BinaryTree.Node(6),
        right = BinaryTree.Node(9)
    )
)

println(tree.contains(4))  // true
println(tree.contains(10)) // false
println(tree.size())       // 7

// Linked List with sealed class
sealed class LinkedList<out T> {
    object Nil : LinkedList<Nothing>()

    data class Cons<T>(
        val head: T,
        val tail: LinkedList<T>
    ) : LinkedList<T>()

    fun <R> fold(
        onNil: () -> R,
        onCons: (T, LinkedList<T>) -> R
    ): R = when (this) {
        is Nil -> onNil()
        is Cons -> onCons(head, tail)
    }

    fun length(): Int = fold(
        onNil = { 0 },
        onCons = { _, tail -> 1 + tail.length() }
    )

    fun reverse(): LinkedList<T> {
        tailrec fun go(current: LinkedList<T>, acc: LinkedList<T>): LinkedList<T> =
            when (current) {
                is Nil -> acc
                is Cons -> go(current.tail, Cons(current.head, acc))
            }
        return go(this, Nil)
    }
}

// Helper function to create lists
fun <T> linkedListOf(vararg elements: T): LinkedList<T> =
    elements.foldRight(LinkedList.Nil as LinkedList<T>) { element, acc ->
        LinkedList.Cons(element, acc)
    }

val list = linkedListOf(1, 2, 3, 4, 5)
println(list.length())  // 5

val reversed = list.reverse()
// Cons(5, Cons(4, Cons(3, Cons(2, Cons(1, Nil)))))

// Result Type (functional error handling)
sealed class Result<out T, out E> {
    data class Success<T>(val value: T) : Result<T, Nothing>()
    data class Failure<E>(val error: E) : Result<Nothing, E>()

    fun <R> map(transform: (T) -> R): Result<R, E> = when (this) {
        is Success -> Success(transform(value))
        is Failure -> this
    }

    fun <R> flatMap(transform: (T) -> Result<R, E>): Result<R, E> = when (this) {
        is Success -> transform(value)
        is Failure -> this
    }

    fun <F> mapError(transform: (E) -> F): Result<T, F> = when (this) {
        is Success -> this
        is Failure -> Failure(transform(error))
    }

    fun getOrElse(default: @UnsafeVariance T): T = when (this) {
        is Success -> value
        is Failure -> default
    }
}

// Using Result type
data class User(val id: Int, val name: String)
sealed class DatabaseError {
    object NotFound : DatabaseError()
    data class ConnectionError(val message: String) : DatabaseError()
}

fun findUser(id: Int): Result<User, DatabaseError> {
    return if (id > 0) {
        Result.Success(User(id, "User $id"))
    } else {
        Result.Failure(DatabaseError.NotFound)
    }
}

fun getUserEmail(user: User): Result<String, DatabaseError> {
    return Result.Success("${user.name.lowercase().replace(" ", ".")}@example.com")
}

// Chain operations with flatMap
val result: Result<String, DatabaseError> = findUser(42)
    .flatMap { user -> getUserEmail(user) }
    .map { email -> email.uppercase() }

when (result) {
    is Result.Success -> println("Email: ${result.value}")
    is Result.Failure -> when (result.error) {
        is DatabaseError.NotFound -> println("User not found")
        is DatabaseError.ConnectionError -> println("Connection error: ${result.error.message}")
    }
}

// JSON AST (Abstract Syntax Tree)
sealed class Json {
    data class JObject(val fields: Map<String, Json>) : Json()
    data class JArray(val elements: List<Json>) : Json()
    data class JString(val value: String) : Json()
    data class JNumber(val value: Double) : Json()
    data class JBoolean(val value: Boolean) : Json()
    object JNull : Json()

    fun stringify(): String = when (this) {
        is JObject -> fields.entries.joinToString(
            prefix = "{",
            postfix = "}",
            separator = ","
        ) { (key, value) ->
            "\"$key\":${value.stringify()}"
        }
        is JArray -> elements.joinToString(
            prefix = "[",
            postfix = "]",
            separator = ","
        ) { it.stringify() }
        is JString -> "\"$value\""
        is JNumber -> value.toString()
        is JBoolean -> value.toString()
        is JNull -> "null"
    }
}

// Using JSON AST
val json = Json.JObject(
    mapOf(
        "name" to Json.JString("Alice"),
        "age" to Json.JNumber(30.0),
        "active" to Json.JBoolean(true),
        "address" to Json.JObject(
            mapOf(
                "city" to Json.JString("Jakarta"),
                "country" to Json.JString("Indonesia")
            )
        ),
        "hobbies" to Json.JArray(
            listOf(
                Json.JString("reading"),
                Json.JString("coding")
            )
        )
    )
)

println(json.stringify())
```

**How It Works**: Sealed classes represent a closed hierarchy of subtypes. Compiler knows all possible subtypes at compile-time, enabling exhaustive `when` expressions (no `else` needed). Perfect for recursive data structures and algebraic data types. Use `object` for terminal cases (Empty, Nil, JNull) and `data class` with references to same sealed type for recursive cases.

**Use Cases**:

- Binary trees and search trees (parsing, AST, decision trees)
- Linked lists (functional programming, stack implementations)
- Result types (functional error handling without exceptions)
- State machines (well-defined states with exhaustive handling)
- Expression evaluators (compilers, interpreters)

**Learn More**: See [Beginner Tutorial - Sealed Classes](/en/learn/swe/prog-lang/kotlin/tutorials/beginner/#sealed-classes) for fundamentals.

---

### Recipe 4: Type-Safe Builders with DSL Syntax

**Problem**: You need to build complex objects or configurations with a clean, readable syntax that provides compile-time type safety and IDE support.

**Solution**:

```kotlin
// HTML DSL example
@DslMarker
annotation class HtmlTagMarker

@HtmlTagMarker
abstract class Tag(val name: String) {
    val children = mutableListOf<Tag>()
    val attributes = mutableMapOf<String, String>()

    protected fun <T : Tag> initTag(tag: T, init: T.() -> Unit): T {
        tag.init()
        children.add(tag)
        return tag
    }

    fun render(): String = buildString {
        append("<$name")
        attributes.forEach { (key, value) ->
            append(" $key=\"$value\"")
        }
        append(">")
        children.forEach { append(it.render()) }
        append("</$name>")
    }
}

class HTML : Tag("html") {
    fun head(init: Head.() -> Unit) = initTag(Head(), init)
    fun body(init: Body.() -> Unit) = initTag(Body(), init)
}

class Head : Tag("head") {
    fun title(init: Title.() -> Unit) = initTag(Title(), init)
}

class Title : Tag("title") {
    operator fun String.unaryPlus() {
        children.add(object : Tag("") {
            override fun render() = this@unaryPlus
        })
    }
}

class Body : Tag("body") {
    fun h1(init: H1.() -> Unit) = initTag(H1(), init)
    fun p(init: P.() -> Unit) = initTag(P(), init)
    fun div(init: Div.() -> Unit) = initTag(Div(), init)
}

class H1 : Tag("h1") {
    operator fun String.unaryPlus() {
        children.add(object : Tag("") {
            override fun render() = this@unaryPlus
        })
    }
}

class P : Tag("p") {
    operator fun String.unaryPlus() {
        children.add(object : Tag("") {
            override fun render() = this@unaryPlus
        })
    }
}

class Div : Tag("div") {
    fun p(init: P.() -> Unit) = initTag(P(), init)

    var id: String
        get() = attributes["id"] ?: ""
        set(value) { attributes["id"] = value }

    var cssClass: String
        get() = attributes["class"] ?: ""
        set(value) { attributes["class"] = value }
}

fun html(init: HTML.() -> Unit): HTML {
    val html = HTML()
    html.init()
    return html
}

// Using the HTML DSL
val page = html {
    head {
        title {
            +"My Page Title"
        }
    }
    body {
        h1 {
            +"Welcome to Kotlin DSL"
        }
        div {
            id = "content"
            cssClass = "container"
            p {
                +"This is a paragraph"
            }
            p {
                +"This is another paragraph"
            }
        }
    }
}

println(page.render())

// SQL DSL example
@DslMarker
annotation class SqlMarker

@SqlMarker
class SelectQuery {
    private val columns = mutableListOf<String>()
    private var tableName: String = ""
    private val whereClauses = mutableListOf<String>()
    private val orderByClauses = mutableListOf<String>()
    private var limitValue: Int? = null

    fun select(vararg cols: String) {
        columns.addAll(cols)
    }

    fun from(table: String) {
        tableName = table
    }

    fun where(clause: String) {
        whereClauses.add(clause)
    }

    fun orderBy(vararg cols: String) {
        orderByClauses.addAll(cols)
    }

    fun limit(value: Int) {
        limitValue = value
    }

    fun build(): String = buildString {
        append("SELECT ")
        append(if (columns.isEmpty()) "*" else columns.joinToString(", "))
        append(" FROM $tableName")

        if (whereClauses.isNotEmpty()) {
            append(" WHERE ")
            append(whereClauses.joinToString(" AND "))
        }

        if (orderByClauses.isNotEmpty()) {
            append(" ORDER BY ")
            append(orderByClauses.joinToString(", "))
        }

        limitValue?.let { append(" LIMIT $it") }
    }
}

fun query(init: SelectQuery.() -> Unit): String {
    val query = SelectQuery()
    query.init()
    return query.build()
}

// Using SQL DSL
val sql = query {
    select("id", "name", "email")
    from("users")
    where("age > 18")
    where("status = 'ACTIVE'")
    orderBy("name", "id")
    limit(10)
}

println(sql)
// SELECT id, name, email FROM users WHERE age > 18 AND status = 'ACTIVE' ORDER BY name, id LIMIT 10

// Configuration DSL
@DslMarker
annotation class ConfigMarker

@ConfigMarker
class ServerConfig {
    var host: String = "localhost"
    var port: Int = 8080
    var ssl: Boolean = false

    private val routes = mutableListOf<Route>()

    fun route(path: String, init: Route.() -> Unit) {
        val route = Route(path)
        route.init()
        routes.add(route)
    }

    fun getRoutes() = routes.toList()
}

@ConfigMarker
class Route(val path: String) {
    var method: String = "GET"
    var handler: (String) -> String = { "Default response" }
}

fun server(init: ServerConfig.() -> Unit): ServerConfig {
    val config = ServerConfig()
    config.init()
    return config
}

// Using configuration DSL
val appServer = server {
    host = "0.0.0.0"
    port = 9000
    ssl = true

    route("/users") {
        method = "GET"
        handler = { "Fetching users" }
    }

    route("/users/{id}") {
        method = "POST"
        handler = { id -> "Creating user $id" }
    }
}

println("Server: ${appServer.host}:${appServer.port}, SSL: ${appServer.ssl}")
appServer.getRoutes().forEach { route ->
    println("Route: ${route.method} ${route.path}")
}
```

**How It Works**: DSLs use @DslMarker annotation to prevent implicit receivers from outer scopes, lambda with receiver (`fun html(init: HTML.() -> Unit)`), and operator overloading (`operator fun String.unaryPlus()`). The type-safe builders pattern uses generics to initialize, configure, and add tags to hierarchy.

**Use Cases**:

- HTML/XML generation (email templates, web pages)
- Test data builders (clean test setup)
- API request builders (fluent HTTP requests)
- Database migration DSLs (schema definitions)
- Configuration files (Gradle build scripts)

**Learn More**: See [Advanced Tutorial - DSL Building](/en/learn/swe/prog-lang/kotlin/tutorials/advanced/#part-3-dsl-building-and-metaprogramming) for advanced techniques.

---

### Recipe 5: Efficient String Manipulation

**Problem**: You need to perform common string operations efficiently while avoiding performance pitfalls and leveraging Kotlin's string manipulation features.

**Solution**:

```kotlin
// String templates (avoid concatenation)
val name = "Alice"
val age = 30

// ❌ Inefficient (creates multiple String objects)
val message1 = "Name: " + name + ", Age: " + age

// ✅ Efficient (string template)
val message2 = "Name: $name, Age: $age"

// ✅ Expression in template
val message3 = "Name: ${name.uppercase()}, Age: ${age + 5}"

// Multi-line strings with trimIndent
val sql = """
    SELECT id, name, email
    FROM users
    WHERE age > 18
      AND status = 'ACTIVE'
    ORDER BY name
""".trimIndent()

// Multi-line with custom margin
val html = """
    |<html>
    |  <body>
    |    <h1>Hello</h1>
    |  </body>
    |</html>
""".trimMargin()

// StringBuilder for loops (avoid concatenation in loops)
fun buildLargeString(items: List<String>): String {
    // ❌ Inefficient - creates new String each iteration
    var result = ""
    for (item in items) {
        result += item + ", "
    }
    return result

    // ✅ Efficient - use buildString
}

fun buildLargeStringEfficiently(items: List<String>): String = buildString {
    items.forEachIndexed { index, item ->
        append(item)
        if (index < items.lastIndex) append(", ")
    }
}

// String splitting and joining
val csv = "Alice,30,alice@example.com"
val parts = csv.split(",")  // List<String>
val (name2, age2, email) = parts  // Destructuring

// Join with separator
val joined = listOf("apple", "banana", "cherry").joinToString(
    separator = ", ",
    prefix = "[",
    postfix = "]"
)
println(joined)  // [apple, banana, cherry]

// Custom transform
val uppercased = listOf("alice", "bob").joinToString { it.uppercase() }
println(uppercased)  // ALICE, BOB

// Regular expressions
val emailPattern = Regex("""[\w.]+@[\w.]+\.\w+""")
val text = "Contact us at support@example.com or sales@example.com"

// Find all matches
val emails = emailPattern.findAll(text).map { it.value }.toList()
println(emails)  // [support@example.com, sales@example.com]

// Replace with regex
val sanitized = text.replace(Regex("""@[\w.]+"""), "@***.**")
println(sanitized)  // Contact us at support@***.** or sales@***.**

// Validation
val isValidEmail = emailPattern.matches("user@example.com")

// String padding and trimming
val code = "42"
val padded = code.padStart(5, '0')  // "00042"
val rightPad = code.padEnd(5, '0')  // "42000"

val dirty = "  hello world  "
val clean = dirty.trim()          // "hello world"
val leftClean = dirty.trimStart() // "hello world  "
val rightClean = dirty.trimEnd()  // "  hello world"

// Character operations
val sentence = "Hello World"
val firstChar = sentence.first()      // 'H'
val lastChar = sentence.last()        // 'd'
val chars = sentence.toCharArray()

val filtered = sentence.filter { it.isUpperCase() }  // "HW"
val mapped = sentence.map { it.uppercase() }  // List<String>
val mappedStr = sentence.map { it.uppercase() }.joinToString("")  // "HELLO WORLD"

// Substring operations
val url = "https://example.com/api/users/123"
val hasPrefix = url.startsWith("https://")  // true
val hasSuffix = url.endsWith("/123")        // true

val domain = url.substringAfter("://").substringBefore("/")  // "example.com"
val userId = url.substringAfterLast("/")  // "123"

// Remove prefix/suffix
val apiPath = url.removePrefix("https://example.com")  // "/api/users/123"
val cleanPath = apiPath.removeSuffix("/123")  // "/api/users"

// Case conversion
val original = "Hello World"
println(original.lowercase())     // "hello world"
println(original.uppercase())     // "HELLO WORLD"
println(original.capitalize())    // "Hello world"
println(original.decapitalize())  // "hello World"

// Title case (custom)
fun String.toTitleCase(): String = split(" ").joinToString(" ") {
    it.lowercase().replaceFirstChar { char -> char.uppercase() }
}
println("hello world".toTitleCase())  // "Hello World"

// String replacement
val text2 = "Hello World Hello Kotlin"
val replaced = text2.replace("Hello", "Hi")  // "Hi World Hi Kotlin"
val firstOnly = text2.replaceFirst("Hello", "Hi")  // "Hi World Hello Kotlin"

// Replace with transform
val numbers = "I have 2 apples and 3 oranges"
val doubled = numbers.replace(Regex("""\d+""")) { match ->
    (match.value.toInt() * 2).toString()
}
println(doubled)  // "I have 4 apples and 6 oranges"

// Checking string content
val number = "12345"
println(number.all { it.isDigit() })      // true
println(number.any { it.isLetter() })     // false
println(number.none { it.isLetter() })    // true

val mixed = "abc123"
println(mixed.all { it.isLetterOrDigit() })  // true

// Line operations
val multiLine = """
    Line 1
    Line 2
    Line 3
""".trimIndent()

val lines = multiLine.lines()  // List<String>
val nonEmpty = multiLine.lineSequence().filter { it.isNotBlank() }.toList()

// Chunking strings
val long = "ABCDEFGHIJK"
val chunked = long.chunked(3)  // ["ABC", "DEF", "GHI", "JK"]

// With transform
val chunkedUpper = long.chunked(3) { it.toString().lowercase() }
// ["abc", "def", "ghi", "jk"]

// Windowed (sliding window)
val windowed = "ABCDE".windowed(3)  // ["ABC", "BCD", "CDE"]
val step2 = "ABCDEF".windowed(3, step = 2)  // ["ABC", "CDE"]

// Common validation patterns
fun String.isValidEmail(): Boolean =
    Regex("""^[\w._%+-]+@[\w.-]+\.\w{2,}$""").matches(this)

fun String.isValidPhone(): Boolean =
    Regex("""^\+?[0-9]{10,15}$""").matches(this)

fun String.isValidUrl(): Boolean =
    Regex("""^https?://[\w.-]+\.\w{2,}(/.*)?$""").matches(this)

println("user@example.com".isValidEmail())  // true
println("+628123456789".isValidPhone())     // true
println("https://example.com".isValidUrl()) // true
```

**How It Works**: String templates are evaluated at compile-time when possible, more efficient than concatenation. Strings are immutable, every modification creates a new String. Use `buildString {}` (internally uses StringBuilder) for multiple modifications. Regular expressions are compiled once, reused multiple times.

**Use Cases**:

- SQL query building (dynamic queries with parameters)
- CSV processing (parsing and generating CSV data)
- Template rendering (email templates, HTML generation)
- Log message formatting (structured logging)
- URL manipulation (building URLs with parameters)

**Learn More**: See [Beginner Tutorial - Strings](/en/learn/swe/prog-lang/kotlin/tutorials/beginner/#string-types) for string basics.

---

## Coroutines and Concurrency

### Recipe 6: Basic Coroutine Launch and Async Patterns

**Problem**: You need to execute multiple tasks concurrently or asynchronously, returning results efficiently without blocking threads.

**Solution**:

```kotlin
import kotlinx.coroutines.*

// Basic launch (fire and forget)
fun basicLaunch() = runBlocking {
    launch {
        delay(1000)
        println("Task 1 completed")
    }

    launch {
        delay(500)
        println("Task 2 completed")
    }

    println("Main thread continues...")
}
// Output:
// Main thread continues...
// Task 2 completed (after 500ms)
// Task 1 completed (after 1000ms)

// Launch with explicit scope
suspend fun launchWithScope() = coroutineScope {
    val job1 = launch {
        repeat(5) { i ->
            delay(100)
            println("Job 1: $i")
        }
    }

    val job2 = launch {
        repeat(3) { i ->
            delay(200)
            println("Job 2: $i")
        }
    }

    // Wait for specific job
    job1.join()
    println("Job 1 finished")
}

// Async for results
suspend fun asyncWithResult(): Int = coroutineScope {
    val deferred1 = async {
        delay(1000)
        40
    }

    val deferred2 = async {
        delay(500)
        2
    }

    // Wait for both results
    val result = deferred1.await() + deferred2.await()
    result  // Returns 42
}

// Parallel API calls
data class User(val id: Int, val name: String)
data class Profile(val bio: String)
data class Posts(val count: Int)

suspend fun fetchUser(id: Int): User {
    delay(100)  // Simulate API call
    return User(id, "User $id")
}

suspend fun fetchProfile(userId: Int): Profile {
    delay(150)  // Simulate API call
    return Profile("Bio for user $userId")
}

suspend fun fetchPosts(userId: Int): Posts {
    delay(200)  // Simulate API call
    return Posts(10)
}

suspend fun getUserData(userId: Int): Triple<User, Profile, Posts> = coroutineScope {
    // All three calls run in parallel
    val userDeferred = async { fetchUser(userId) }
    val profileDeferred = async { fetchProfile(userId) }
    val postsDeferred = async { fetchPosts(userId) }

    // Wait for all results
    Triple(
        userDeferred.await(),
        profileDeferred.await(),
        postsDeferred.await()
    )
}
// Sequential: 100 + 150 + 200 = 450ms
// Parallel: max(100, 150, 200) = 200ms

// Launch vs Async comparison
fun launchVsAsync() = runBlocking {
    // launch: fire and forget, no result
    val job = launch {
        delay(1000)
        println("Launch completed")
    }
    // job.join()  // Can only wait, no result

    // async: returns Deferred<T>, has result
    val deferred = async {
        delay(1000)
        "Async result"
    }
    val result = deferred.await()  // Can get result
    println(result)
}

// Multiple async tasks with awaitAll
suspend fun processMultiple(ids: List<Int>): List<User> = coroutineScope {
    ids.map { id ->
        async { fetchUser(id) }
    }.awaitAll()  // Wait for all in parallel
}

// Structured concurrency example
suspend fun structuredConcurrency() = coroutineScope {
    launch {
        delay(500)
        println("Child 1")
    }

    launch {
        delay(1000)
        println("Child 2")
    }

    println("Parent started")
    // Parent waits for ALL children before completing
}

// Cancellation example
fun cancellationExample() = runBlocking {
    val job = launch {
        repeat(1000) { i ->
            if (!isActive) {
                println("Cancelled at $i")
                return@launch
            }
            delay(100)
            println("Working: $i")
        }
    }

    delay(350)
    job.cancel()  // Cancel after 3 iterations
    job.join()
    println("Job cancelled")
}

// Exception handling with launch
fun exceptionInLaunch() = runBlocking {
    val exceptionHandler = CoroutineExceptionHandler { _, exception ->
        println("Caught exception: ${exception.message}")
    }

    val job = launch(exceptionHandler) {
        throw RuntimeException("Something went wrong")
    }

    job.join()
}

// Exception handling with async
suspend fun exceptionInAsync() = coroutineScope {
    val deferred = async {
        delay(100)
        throw RuntimeException("Async failed")
    }

    try {
        deferred.await()  // Exception thrown here
    } catch (e: Exception) {
        println("Caught: ${e.message}")
    }
}
```

**How It Works**: `launch` returns `Job` (fire-and-forget), `async` returns `Deferred<T>` (has result). Structured concurrency means parent coroutine waits for all children to complete, cancelling parent cancels all children. Coroutines are lightweight (thousands on single thread), use `delay()` instead of `Thread.sleep()`.

**Use Cases**:

- Fire-and-forget background tasks (logging, analytics)
- Parallel API calls (loading dashboard data)
- Batch processing (processing list of users)
- Timeout wrappers (cancelling long-running operations)
- Sequential with error handling (user registration workflow with rollback)

**Learn More**: See [Quick Start - Coroutines](/en/learn/swe/prog-lang/kotlin/tutorials/quick-start/#8-coroutines-basics) for introduction.

---

### Recipe 7: Timeout and Retry with Coroutines

**Problem**: You need to execute operations with time limits and automatic retry logic for failed operations.

**Solution**:

```kotlin
import kotlinx.coroutines.*

// Basic timeout
suspend fun <T> executeWithTimeout(timeoutMs: Long, block: suspend () -> T): T? =
    try {
        withTimeout(timeoutMs) {
            block()
        }
    } catch (e: TimeoutCancellationException) {
        println("Operation timed out after ${timeoutMs}ms")
        null
    }

// Usage
suspend fun fetchData(): String {
    delay(2000)
    return "Data"
}

val result = executeWithTimeout(1000) {
    fetchData()  // Will timeout
}

// Retry logic with exponential backoff
suspend fun <T> retryWithBackoff(
    times: Int = 3,
    initialDelay: Long = 100,
    maxDelay: Long = 1000,
    factor: Double = 2.0,
    block: suspend () -> T
): T {
    var currentDelay = initialDelay
    repeat(times - 1) { attempt ->
        try {
            return block()
        } catch (e: Exception) {
            println("Attempt ${attempt + 1} failed: ${e.message}")
        }
        delay(currentDelay)
        currentDelay = (currentDelay * factor).toLong().coerceAtMost(maxDelay)
    }
    return block()  // Last attempt throws exception if fails
}

// Retry with specific exceptions
suspend fun <T> retryOn(
    times: Int = 3,
    exceptionClass: Class<out Exception>,
    block: suspend () -> T
): T {
    repeat(times - 1) {
        try {
            return block()
        } catch (e: Exception) {
            if (!exceptionClass.isInstance(e)) throw e
            println("Retrying after ${e::class.simpleName}")
        }
    }
    return block()
}

// Combined timeout and retry
suspend fun <T> executeWithTimeoutAndRetry(
    timeoutMs: Long,
    retries: Int = 3,
    block: suspend () -> T
): T {
    return retryWithBackoff(times = retries) {
        withTimeout(timeoutMs) {
            block()
        }
    }
}

// Real-world example: HTTP request with retry
class NetworkException(message: String) : Exception(message)

suspend fun makeHttpRequest(url: String): String {
    // Simulate network call
    delay(500)
    if (Math.random() < 0.7) throw NetworkException("Network error")
    return "Response from $url"
}

suspend fun reliableHttpRequest(url: String): String {
    return retryWithBackoff(times = 5, initialDelay = 200) {
        withTimeout(3000) {
            makeHttpRequest(url)
        }
    }
}

// Conditional retry
suspend fun <T> retryIf(
    times: Int = 3,
    predicate: (Exception) -> Boolean,
    block: suspend () -> T
): T {
    repeat(times - 1) {
        try {
            return block()
        } catch (e: Exception) {
            if (!predicate(e)) throw e
            println("Retrying due to: ${e.message}")
            delay(1000)
        }
    }
    return block()
}

// Usage with custom condition
val result2 = retryIf(
    times = 3,
    predicate = { it is NetworkException }
) {
    makeHttpRequest("https://api.example.com")
}
```

**How It Works**: `withTimeout()` cancels coroutine if exceeds time limit, throws `TimeoutCancellationException`. Exponential backoff increases delay between retries (100ms, 200ms, 400ms, 800ms, max 1000ms). Conditional retry only retries specific exception types.

**Learn More**: See [Intermediate Tutorial - Advanced Coroutines](/en/learn/swe/prog-lang/kotlin/tutorials/intermediate/#part-1-advanced-coroutines).

---

### Recipe 8: Parallel Execution with Async/Await

**Problem**: You need to execute multiple independent tasks concurrently and combine their results efficiently.

**Solution**:

```kotlin
import kotlinx.coroutines.*

// Parallel execution with async
suspend fun parallelTasks() = coroutineScope {
    val task1 = async { computeTask1() }
    val task2 = async { computeTask2() }
    val task3 = async { computeTask3() }

    val results = listOf(task1.await(), task2.await(), task3.await())
}

// awaitAll for multiple tasks
suspend fun batchProcess(ids: List<Int>) = coroutineScope {
    ids.map { id ->
        async { processItem(id) }
    }.awaitAll()
}

// Parallel map operation
suspend fun <T, R> List<T>.parallelMap(transform: suspend (T) -> R): List<R> =
    coroutineScope {
        map { async { transform(it) } }.awaitAll()
    }

// Usage
val numbers = (1..100).toList()
val doubled = numbers.parallelMap { it * 2 }
```

**How It Works**: `async` creates `Deferred<T>` that computes value concurrently. `await()` waits for result. `awaitAll()` waits for all Deferred results in parallel.

**Learn More**: See [Intermediate Tutorial](/en/learn/swe/prog-lang/kotlin/tutorials/intermediate/#part-1-advanced-coroutines).

---

### Recipe 9: Flow for Reactive Streams

**Problem**: You need to work with asynchronous streams of data that emit multiple values over time.

**Solution**:

```kotlin
import kotlinx.coroutines.*
import kotlinx.coroutines.flow.*

// Basic Flow creation
fun simpleFlow(): Flow<Int> = flow {
    for (i in 1..5) {
        delay(100)
        emit(i)
    }
}

// Collecting flow
suspend fun collectFlow() {
    simpleFlow().collect { value ->
        println(value)
    }
}

// Flow operators
suspend fun flowOperators() {
    simpleFlow()
        .map { it * 2 }
        .filter { it > 4 }
        .collect { println(it) }
}

// StateFlow for state management
class Counter {
    private val _count = MutableStateFlow(0)
    val count: StateFlow<Int> = _count.asStateFlow()

    fun increment() {
        _count.value++
    }
}

// SharedFlow for events
class EventBus {
    private val _events = MutableSharedFlow<String>()
    val events: SharedFlow<String> = _events.asSharedFlow()

    suspend fun emit(event: String) {
        _events.emit(event)
    }
}
```

**How It Works**: Flow is a cold stream (starts emitting when collected). StateFlow is a hot stream (always has value, emits latest). SharedFlow is a hot stream (broadcasts to multiple collectors).

**Learn More**: See [Intermediate Tutorial - Flow](/en/learn/swe/prog-lang/kotlin/tutorials/intermediate/#flow-for-reactive-streams).

---

### Recipe 10: Channels for Producer-Consumer Pattern

**Problem**: You need to communicate between coroutines using a queue-like structure for producer-consumer patterns.

**Solution**:

```kotlin
import kotlinx.coroutines.*
import kotlinx.coroutines.channels.*

// Basic channel
suspend fun basicChannel() = coroutineScope {
    val channel = Channel<Int>()

    launch {
        for (x in 1..5) {
            channel.send(x)
        }
        channel.close()
    }

    for (y in channel) {
        println(y)
    }
}

// Producer-consumer with multiple consumers
suspend fun producerConsumer() = coroutineScope {
    val channel = Channel<Int>()

    // Producer
    launch {
        repeat(10) {
            channel.send(it)
        }
        channel.close()
    }

    // Multiple consumers
    repeat(3) { id ->
        launch {
            for (value in channel) {
                println("Consumer $id: $value")
            }
        }
    }
}

// Buffered channel
suspend fun bufferedChannel() = coroutineScope {
    val channel = Channel<Int>(capacity = 5)

    launch {
        repeat(10) {
            channel.send(it)
            println("Sent $it")
        }
        channel.close()
    }

    delay(1000)
    for (value in channel) {
        println("Received $value")
    }
}
```

**How It Works**: Channel is a communication primitive between coroutines. Buffered channels store values until capacity reached. Unbuffered channels make sender wait until receiver ready.

**Learn More**: See [Intermediate Tutorial - Channels](/en/learn/swe/prog-lang/kotlin/tutorials/intermediate/#channels-for-producer-consumer).

---

### Recipe 11: Structured Concurrency with supervisorScope

**Problem**: You need to manage parent-child coroutine relationships where child failures shouldn't cancel siblings.

**Solution**:

```kotlin
import kotlinx.coroutines.*

// Regular coroutineScope - one failure cancels all
suspend fun regularScope() = coroutineScope {
    launch {
        delay(100)
        throw RuntimeException("Child 1 failed")
    }

    launch {
        delay(200)
        println("Child 2")  // Won't execute
    }
}

// supervisorScope - failures isolated
suspend fun supervisedScope() = supervisorScope {
    launch {
        delay(100)
        throw RuntimeException("Child 1 failed")
    }

    launch {
        delay(200)
        println("Child 2")  // Still executes
    }
}

// SupervisorJob for independent tasks
fun independentTasks() = runBlocking {
    val supervisor = SupervisorJob()
    val scope = CoroutineScope(Dispatchers.Default + supervisor)

    val job1 = scope.launch {
        throw RuntimeException("Task 1 failed")
    }

    val job2 = scope.launch {
        delay(500)
        println("Task 2 completed")
    }

    job1.join()
    job2.join()
}
```

**How It Works**: `coroutineScope` means one child fails, all cancel. `supervisorScope` means children fail independently. `SupervisorJob` provides parent for independent child jobs.

**Learn More**: See [Intermediate Tutorial - Structured Concurrency](/en/learn/swe/prog-lang/kotlin/tutorials/intermediate/#structured-concurrency-with-supervisorscope).

---

## Error Handling

### Recipe 12: Result Type for Functional Error Handling

**Problem**: You need type-safe error handling that avoids exceptions and enables functional composition.

**Solution**:

```kotlin
// Using stdlib Result
fun parseInt(str: String): Result<Int> = runCatching {
    str.toInt()
}

// Chaining operations
fun processNumber(str: String): Result<String> =
    parseInt(str)
        .map { it * 2 }
        .map { "Result: $it" }

// With recovery
val result = parseInt("invalid")
    .recover { 0 }
    .getOrThrow()

// Custom Result type
sealed class AppResult<out T> {
    data class Success<T>(val value: T) : AppResult<T>()
    data class Failure(val error: AppError) : AppResult<Nothing>()
}

sealed class AppError {
    data class ValidationError(val message: String) : AppError()
    data class NetworkError(val code: Int) : AppError()
}
```

**How It Works**: `Result<T>` encapsulates success (T) or failure (Throwable). `map/flatMap` transform successful results. `recover` provides fallback value. `getOrElse/getOrThrow` extract value.

**Learn More**: See [Intermediate Tutorial - Error Handling](/en/learn/swe/prog-lang/kotlin/tutorials/intermediate/#result-type-for-functional-error-handling).

---

### Recipe 13: Sealed Class for Type-Safe Errors

**Problem**: You need compile-time guaranteed handling of all possible error cases.

**Solution**:

```kotlin
sealed class DatabaseResult<out T> {
    data class Success<T>(val data: T) : DatabaseResult<T>()
    sealed class Error : DatabaseResult<Nothing>() {
        object NotFound : Error()
        object PermissionDenied : Error()
        data class QueryError(val sql: String, val message: String) : Error()
    }
}

fun getUser(id: Int): DatabaseResult<User> {
    return if (id > 0) {
        DatabaseResult.Success(User(id, "User $id"))
    } else {
        DatabaseResult.Error.NotFound
    }
}

// Exhaustive when
fun handleResult(result: DatabaseResult<User>) = when (result) {
    is DatabaseResult.Success -> println("User: ${result.data}")
    is DatabaseResult.Error.NotFound -> println("Not found")
    is DatabaseResult.Error.PermissionDenied -> println("Access denied")
    is DatabaseResult.Error.QueryError -> println("Query failed: ${result.message}")
}
```

**How It Works**: Sealed class hierarchy means compiler knows all subtypes. Exhaustive `when` must handle all cases. Type-safe means each error type has specific data.

**Learn More**: See [Beginner Tutorial - Sealed Classes](/en/learn/swe/prog-lang/kotlin/tutorials/beginner/#sealed-classes).

---

### Recipe 14: Nullable Types for Optional Values

**Problem**: You need to handle values that may or may not be present without null pointer exceptions.

**Solution**:

```kotlin
// Safe calls
val user: User? = findUser(42)
val email: String? = user?.email

// Elvis operator
val displayName = user?.name ?: "Guest"

// let for null-safe scoping
user?.let {
    println("User: ${it.name}")
    sendEmail(it.email)
}

// Chaining safe calls
val city: String? = user?.address?.city

// Safe casting
val userAsAdmin: Admin? = user as? Admin

// requireNotNull for assertions
fun processUser(user: User?) {
    val nonNullUser = requireNotNull(user) { "User must not be null" }
    // Now nonNullUser is User (not User?)
}
```

**How It Works**: `?` marks nullable type. `?.` is safe call (returns null if receiver is null). `?:` is Elvis operator (provides default when null). `let` executes block only if not null.

**Learn More**: See [Quick Start - Null Safety](/en/learn/swe/prog-lang/kotlin/tutorials/quick-start/#2-null-safety).

---

### Recipe 15: Try-Catch with Kotlin Idioms

**Problem**: You need to handle exceptions in Kotlin's functional style while leveraging expression syntax.

**Solution**:

```kotlin
// try as expression
val result: Int = try {
    "42".toInt()
} catch (e: NumberFormatException) {
    0
}

// runCatching for functional style
val parseResult = runCatching {
    "invalid".toInt()
}.getOrElse { 0 }

// Multiple catch blocks
fun processData(data: String): String = try {
    parseAndValidate(data)
} catch (e: NumberFormatException) {
    "Invalid number"
} catch (e: IllegalArgumentException) {
    "Invalid argument: ${e.message}"
} catch (e: Exception) {
    "Unknown error"
}

// finally block
fun readFile(path: String): String {
    val reader = openFile(path)
    try {
        return reader.readText()
    } finally {
        reader.close()
    }
}

// use for auto-closing resources
fun readFileIdiomatic(path: String): String =
    File(path).bufferedReader().use { it.readText() }
```

**How It Works**: `try` as expression returns value from try or catch. `runCatching` wraps in Result type. `finally` always executes. `use` auto-closes Closeable resources.

**Learn More**: See [Beginner Tutorial - Error Handling](/en/learn/swe/prog-lang/kotlin/tutorials/beginner/#part-5-error-handling-and-exceptions).

---

## Design Patterns

### Recipe 16: Singleton with Object Keyword

**Problem**: You need a single instance of a class that's lazily initialized and thread-safe.

**Solution**:

```kotlin
// Simple singleton
object DatabaseConnection {
    fun connect() = println("Connected")
}

// Usage
DatabaseConnection.connect()

// Singleton with state
object AppConfig {
    var apiUrl: String = "https://api.example.com"
    var timeout: Int = 30

    fun loadConfig(path: String) {
        // Load configuration
    }
}

// Singleton implementing interface
interface Logger {
    fun log(message: String)
}

object ConsoleLogger : Logger {
    override fun log(message: String) {
        println("[LOG] $message")
    }
}
```

**How It Works**: `object` creates singleton automatically. Thread-safe, lazily initialized on first access.

**Learn More**: See [Beginner Tutorial - Objects](/en/learn/swe/prog-lang/kotlin/tutorials/beginner/#object-declarations-singletons).

---

### Recipe 17: Factory Pattern with Companion Objects

**Problem**: You need to create instances with factory methods that encapsulate creation logic.

**Solution**:

```kotlin
// Factory with companion object
class User private constructor(
    val id: Int,
    val name: String,
    val email: String
) {
    companion object {
        fun create(name: String, email: String): User {
            val id = generateId()
            return User(id, name, email)
        }

        fun fromJson(json: String): User {
            // Parse JSON
            return User(1, "Alice", "alice@example.com")
        }

        private fun generateId(): Int = (1..1000).random()
    }
}

// Usage
val user = User.create("Bob", "bob@example.com")
val userFromJson = User.fromJson("""{"name":"Alice"}""")

// Factory with sealed class
sealed class Result {
    companion object {
        fun success(value: String): Result = Success(value)
        fun failure(error: String): Result = Failure(error)
    }

    data class Success(val value: String) : Result()
    data class Failure(val error: String) : Result()
}
```

**How It Works**: Companion object can access private constructor. Acts as static factory methods.

**Learn More**: See [Intermediate Tutorial - Factory Pattern](/en/learn/swe/prog-lang/kotlin/tutorials/intermediate/#factory-pattern-with-companion-objects).

---

### Recipe 18: Builder Pattern with apply/also

**Problem**: You need to build complex objects with optional parameters fluently.

**Solution**:

```kotlin
// Data class with defaults
data class HttpRequest(
    var url: String = "",
    var method: String = "GET",
    var headers: MutableMap<String, String> = mutableMapOf(),
    var body: String? = null
)

// Builder with apply
fun httpRequest(init: HttpRequest.() -> Unit): HttpRequest {
    return HttpRequest().apply(init)
}

// Usage
val request = httpRequest {
    url = "https://api.example.com/users"
    method = "POST"
    headers["Content-Type"] = "application/json"
    body = """{"name":"Alice"}"""
}

// Traditional builder class
class EmailBuilder {
    private var to: String = ""
    private var subject: String = ""
    private var body: String = ""

    fun to(address: String) = apply { to = address }
    fun subject(text: String) = apply { subject = text }
    fun body(content: String) = apply { body = content }

    fun build() = Email(to, subject, body)
}

val email = EmailBuilder()
    .to("user@example.com")
    .subject("Hello")
    .body("Message")
    .build()
```

**How It Works**: `apply` executes block with `this`, returns receiver. `also` executes block with `it`, returns receiver.

**Learn More**: See [Intermediate Tutorial - Builder Pattern](/en/learn/swe/prog-lang/kotlin/tutorials/intermediate/#builder-pattern-with-applyalso).

---

### Recipe 19: Observer Pattern with Flow

**Problem**: You need to notify multiple observers when state changes or events occur.

**Solution**:

```kotlin
import kotlinx.coroutines.flow.*

// State observer with StateFlow
class CounterViewModel {
    private val _count = MutableStateFlow(0)
    val count: StateFlow<Int> = _count.asStateFlow()

    fun increment() {
        _count.value++
    }
}

// Event observer with SharedFlow
class EventBus {
    private val _events = MutableSharedFlow<Event>()
    val events: SharedFlow<Event> = _events.asSharedFlow()

    suspend fun emit(event: Event) {
        _events.emit(event)
    }
}

sealed class Event {
    data class UserLoggedIn(val userId: String) : Event()
    object UserLoggedOut : Event()
}
```

**How It Works**: StateFlow is hot stream with latest value. New collectors immediately receive current state. SharedFlow is hot stream for events, broadcasts to all collectors.

**Learn More**: See [Intermediate Tutorial - Observer Pattern](/en/learn/swe/prog-lang/kotlin/tutorials/intermediate/#observer-pattern-with-flow).

---

### Recipe 20: Delegation Pattern with by Keyword

**Problem**: You need to delegate interface implementation to another object without inheritance.

**Solution**:

```kotlin
// Interface delegation
interface Logger {
    fun log(message: String)
}

class ConsoleLogger : Logger {
    override fun log(message: String) = println("[LOG] $message")
}

class Service(logger: Logger) : Logger by logger {
    fun doWork() {
        log("Starting work")
        // Do actual work
        log("Work completed")
    }
}

// Usage
val service = Service(ConsoleLogger())
service.doWork()

// Property delegation
class User {
    var name: String by Delegates.observable("") { _, old, new ->
        println("Name changed from $old to $new")
    }

    val lazyValue: String by lazy {
        println("Computed")
        "Heavy computation result"
    }
}
```

**How It Works**: `by` delegates interface/property to another object. Compiler generates forwarding methods automatically.

**Learn More**: See [Intermediate Tutorial - Delegation](/en/learn/swe/prog-lang/kotlin/tutorials/intermediate/#delegation-pattern-with-by-keyword).

---

### Recipe 21: Strategy Pattern with Lambdas

**Problem**: You need to select algorithm/behavior at runtime without complex inheritance hierarchies.

**Solution**:

```kotlin
// Strategy as function type
typealias SortStrategy<T> = (List<T>) -> List<T>

class Sorter<T>(private val strategy: SortStrategy<T>) {
    fun sort(list: List<T>): List<T> = strategy(list)
}

// Different strategies
val quickSort: SortStrategy<Int> = { list -> list.sorted() }
val reverseSort: SortStrategy<Int> = { list -> list.sortedDescending() }

// Usage
val sorter = Sorter(quickSort)
val sorted = sorter.sort(listOf(3, 1, 4, 1, 5))

// Validation strategy
typealias Validator<T> = (T) -> Boolean

class Form<T>(private val validator: Validator<T>) {
    fun validate(value: T): Boolean = validator(value)
}

val emailValidator: Validator<String> = { it.contains("@") }
val emailForm = Form(emailValidator)
println(emailForm.validate("user@example.com"))  // true
```

**How It Works**: Function types mean functions are first-class objects. Can be passed as parameters and stored.

**Learn More**: See [Intermediate Tutorial - Strategy Pattern](/en/learn/swe/prog-lang/kotlin/tutorials/intermediate/#strategy-pattern-with-lambdas).

---

## Web Development

### Recipe 22: REST API with Ktor

**Problem**: You need to build a RESTful API server with routing, JSON serialization, and error handling.

**Solution**:

```kotlin
import io.ktor.server.application.*
import io.ktor.server.engine.*
import io.ktor.server.netty.*
import io.ktor.server.routing.*
import io.ktor.server.response.*
import io.ktor.http.*

fun main() {
    embeddedServer(Netty, port = 8080) {
        routing {
            get("/users") {
                call.respond(listOf(
                    mapOf("id" to 1, "name" to "Alice"),
                    mapOf("id" to 2, "name" to "Bob")
                ))
            }

            get("/users/{id}") {
                val id = call.parameters["id"]?.toIntOrNull()
                if (id != null) {
                    call.respond(mapOf("id" to id, "name" to "User $id"))
                } else {
                    call.respond(HttpStatusCode.BadRequest, "Invalid ID")
                }
            }

            post("/users") {
                call.respond(HttpStatusCode.Created, mapOf("message" to "User created"))
            }
        }
    }.start(wait = true)
}
```

**How It Works**: Ktor is asynchronous web framework built on coroutines. Lightweight and flexible.

**Learn More**: See [Intermediate Tutorial - REST API with Ktor](/en/learn/swe/prog-lang/kotlin/tutorials/intermediate/#part-4-rest-api-development-with-ktor).

---

### Recipe 23: JSON Serialization with kotlinx.serialization

**Problem**: You need type-safe JSON serialization and deserialization for data classes.

**Solution**:

```kotlin
import kotlinx.serialization.*
import kotlinx.serialization.json.*

@Serializable
data class User(
    val id: Int,
    val name: String,
    val email: String
)

// Serialize to JSON
val user = User(1, "Alice", "alice@example.com")
val json = Json.encodeToString(user)
println(json)  // {"id":1,"name":"Alice","email":"alice@example.com"}

// Deserialize from JSON
val jsonString = """{"id":2,"name":"Bob","email":"bob@example.com"}"""
val parsedUser = Json.decodeFromString<User>(jsonString)

// Custom names
@Serializable
data class Product(
    val id: Int,
    @SerialName("product_name")
    val name: String,
    val price: Double
)

// Nullable and default values
@Serializable
data class Config(
    val host: String,
    val port: Int = 8080,
    val ssl: Boolean? = null
)
```

**How It Works**: `@Serializable` generates serializers at compile-time. Type-safe and efficient.

**Learn More**: See [Intermediate Tutorial - JSON Serialization](/en/learn/swe/prog-lang/kotlin/tutorials/intermediate/#json-serialization-with-kotlinxserialization).

---

### Recipe 24: Validation with Custom DSL

**Problem**: You need expressive, reusable validation logic for user input.

**Solution**:

```kotlin
// Validation DSL
class Validator<T> {
    private val rules = mutableListOf<(T) -> String?>()

    fun rule(message: String, check: (T) -> Boolean) {
        rules.add { value ->
            if (check(value)) null else message
        }
    }

    fun validate(value: T): List<String> {
        return rules.mapNotNull { it(value) }
    }
}

fun <T> validate(init: Validator<T>.() -> Unit): Validator<T> {
    return Validator<T>().apply(init)
}

// Usage
val emailValidator = validate<String> {
    rule("Email must not be empty") { it.isNotBlank() }
    rule("Email must contain @") { it.contains("@") }
    rule("Email must have domain") { it.substringAfter("@").contains(".") }
}

val errors = emailValidator.validate("invalid")
errors.forEach { println(it) }
```

**How It Works**: DSL provides domain-specific language for readable validation rules. Type-safe with compile-time checking.

**Learn More**: See [Advanced Tutorial - DSL Building](/en/learn/swe/prog-lang/kotlin/tutorials/advanced/#part-3-dsl-building-and-metaprogramming).

---

## Database Patterns

### Recipe 25: Database Access with Exposed

**Problem**: You need type-safe SQL queries with an ORM that leverages Kotlin's type system.

**Solution**:

```kotlin
import org.jetbrains.exposed.sql.*
import org.jetbrains.exposed.sql.transactions.transaction

// Define table
object Users : Table() {
    val id = integer("id").autoIncrement()
    val name = varchar("name", 50)
    val email = varchar("email", 100)

    override val primaryKey = PrimaryKey(id)
}

// Connect and create table
Database.connect("jdbc:h2:mem:test", driver = "org.h2.Driver")

transaction {
    SchemaUtils.create(Users)

    // Insert
    Users.insert {
        it[name] = "Alice"
        it[email] = "alice@example.com"
    }

    // Query
    Users.selectAll().forEach {
        println("${it[Users.name]}: ${it[Users.email]}")
    }
}
```

**How It Works**: Exposed provides type-safe SQL DSL. Compile-time validation of queries.

**Learn More**: See [Intermediate Tutorial - Database Integration](/en/learn/swe/prog-lang/kotlin/tutorials/intermediate/#part-3-database-integration).

---

### Recipe 26: JDBC with Kotlin Extensions

**Problem**: You need to work with JDBC using Kotlin idioms for cleaner, safer code.

**Solution**:

```kotlin
import java.sql.*

// Extension function for safe result set iteration
inline fun <T> ResultSet.map(transform: (ResultSet) -> T): List<T> {
    val result = mutableListOf<T>()
    while (next()) {
        result.add(transform(this))
    }
    return result
}

// Usage
fun getUsers(connection: Connection): List<User> {
    val sql = "SELECT id, name, email FROM users"
    return connection.prepareStatement(sql).use { stmt ->
        stmt.executeQuery().use { rs ->
            rs.map { User(it.getInt("id"), it.getString("name"), it.getString("email")) }
        }
    }
}

// Extension for prepared statement parameters
fun PreparedStatement.setParameters(vararg params: Any?) {
    params.forEachIndexed { index, param ->
        when (param) {
            is String -> setString(index + 1, param)
            is Int -> setInt(index + 1, param)
            is Long -> setLong(index + 1, param)
            null -> setNull(index + 1, Types.NULL)
        }
    }
}
```

**How It Works**: `use` auto-closes JDBC resources. Extensions make JDBC more Kotlin-friendly.

**Learn More**: See [Intermediate Tutorial - Database Patterns](/en/learn/swe/prog-lang/kotlin/tutorials/intermediate/#jdbc-basics).

---

### Recipe 27: Transaction Management

**Problem**: You need to execute multiple database operations atomically with rollback on failure.

**Solution**:

```kotlin
import java.sql.Connection

fun <T> Connection.transaction(block: Connection.() -> T): T {
    val originalAutoCommit = autoCommit
    autoCommit = false
    return try {
        val result = block()
        commit()
        result
    } catch (e: Exception) {
        rollback()
        throw e
    } finally {
        autoCommit = originalAutoCommit
    }
}

// Usage
fun transferMoney(from: Int, to: Int, amount: Double) {
    connection.transaction {
        val deductSql = "UPDATE accounts SET balance = balance - ? WHERE id = ?"
        prepareStatement(deductSql).use {
            it.setDouble(1, amount)
            it.setInt(2, from)
            it.executeUpdate()
        }

        val addSql = "UPDATE accounts SET balance = balance + ? WHERE id = ?"
        prepareStatement(addSql).use {
            it.setDouble(1, amount)
            it.setInt(2, to)
            it.executeUpdate()
        }
    }
}
```

**How It Works**: Transaction block commits on success, rolls back on exception. Restores original autoCommit state.

**Learn More**: See [Intermediate Tutorial - Transaction Management](/en/learn/swe/prog-lang/kotlin/tutorials/intermediate/#transaction-management).

---

### Recipe 28: Connection Pooling

**Problem**: You need efficient database connection management with connection pooling.

**Solution**:

```kotlin
import com.zaxxer.hikari.HikariConfig
import com.zaxxer.hikari.HikariDataSource

// Configure HikariCP
fun createDataSource(): HikariDataSource {
    val config = HikariConfig().apply {
        jdbcUrl = "jdbc:postgresql://localhost:5432/mydb"
        username = "user"
        password = "password"
        maximumPoolSize = 10
        minimumIdle = 2
        connectionTimeout = 30000
        idleTimeout = 600000
        maxLifetime = 1800000
    }
    return HikariDataSource(config)
}

// Usage
val dataSource = createDataSource()

fun getUser(id: Int): User? {
    dataSource.connection.use { conn ->
        val sql = "SELECT * FROM users WHERE id = ?"
        return conn.prepareStatement(sql).use { stmt ->
            stmt.setInt(1, id)
            stmt.executeQuery().use { rs ->
                if (rs.next()) {
                    User(rs.getInt("id"), rs.getString("name"))
                } else null
            }
        }
    }
}
```

**How It Works**: HikariCP is fast, lightweight connection pool. Manages connection lifecycle automatically.

**Learn More**: See [Intermediate Tutorial - Connection Pooling](/en/learn/swe/prog-lang/kotlin/tutorials/intermediate/#connection-pooling).

---

## Testing Patterns

### Recipe 29: Unit Testing with JUnit 5

**Problem**: You need to write comprehensive unit tests for Kotlin code with assertions and lifecycle management.

**Solution**:

```kotlin
import org.junit.jupiter.api.*
import kotlin.test.*

class CalculatorTest {
    private lateinit var calculator: Calculator

    @BeforeEach
    fun setup() {
        calculator = Calculator()
    }

    @Test
    fun `addition should return sum of two numbers`() {
        val result = calculator.add(2, 3)
        assertEquals(5, result)
    }

    @Test
    fun `division should throw exception for zero divisor`() {
        assertThrows<IllegalArgumentException> {
            calculator.divide(10, 0)
        }
    }

    @Test
    fun `should handle nullable results`() {
        val result = calculator.safeDivide(10, 0)
        assertNull(result)
    }

    @ParameterizedTest
    @ValueSource(ints = [1, 2, 3, 4, 5])
    fun `should multiply by two correctly`(number: Int) {
        val result = calculator.multiply(number, 2)
        assertEquals(number * 2, result)
    }
}
```

**How It Works**: JUnit 5 is modern testing framework. kotlin.test provides Kotlin-specific assertions. @BeforeEach runs before each test.

**Learn More**: See [Beginner Tutorial - Testing](/en/learn/swe/prog-lang/kotlin/tutorials/beginner/#part-9-testing-with-junit-5).

---

### Recipe 30: Mocking with MockK

**Problem**: You need to mock dependencies and verify interactions in unit tests.

**Solution**:

```kotlin
import io.mockk.*
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

interface UserRepository {
    fun findById(id: Int): User?
    fun save(user: User): User
}

class UserService(private val repository: UserRepository) {
    fun getUserName(id: Int): String {
        val user = repository.findById(id)
        return user?.name ?: "Unknown"
    }
}

class UserServiceTest {
    @Test
    fun `should return user name when user exists`() {
        // Create mock
        val mockRepo = mockk<UserRepository>()

        // Define behavior
        every { mockRepo.findById(1) } returns User(1, "Alice")

        val service = UserService(mockRepo)
        val name = service.getUserName(1)

        assertEquals("Alice", name)

        // Verify interaction
        verify(exactly = 1) { mockRepo.findById(1) }
    }

    @Test
    fun `should return Unknown when user not found`() {
        val mockRepo = mockk<UserRepository>()
        every { mockRepo.findById(any()) } returns null

        val service = UserService(mockRepo)
        val name = service.getUserName(99)

        assertEquals("Unknown", name)
    }
}
```

**How It Works**: `mockk()` creates mock object. `every` defines behavior. `verify` checks interactions.

**Learn More**: See [Intermediate Tutorial - Mocking](/en/learn/swe/prog-lang/kotlin/tutorials/intermediate/#unit-testing-with-mockk).

---

### Recipe 31: Coroutine Testing

**Problem**: You need to test suspend functions and coroutines with controlled time and dispatchers.

**Solution**:

```kotlin
import kotlinx.coroutines.*
import kotlinx.coroutines.test.*
import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class DataService {
    suspend fun fetchData(): String {
        delay(1000)
        return "Data"
    }
}

class DataServiceTest {
    @Test
    fun `should fetch data after delay`() = runTest {
        val service = DataService()
        val result = service.fetchData()
        assertEquals("Data", result)
    }

    @Test
    fun `should handle concurrent operations`() = runTest {
        val results = coroutineScope {
            val deferred1 = async { delay(100); "A" }
            val deferred2 = async { delay(200); "B" }
            listOf(deferred1.await(), deferred2.await())
        }
        assertEquals(listOf("A", "B"), results)
    }

    @Test
    fun `should test with custom dispatcher`() = runTest {
        val dispatcher = StandardTestDispatcher(testScheduler)
        val scope = CoroutineScope(dispatcher)

        var result = ""
        scope.launch {
            delay(1000)
            result = "Done"
        }

        advanceTimeBy(1000)
        assertEquals("Done", result)
    }
}
```

**How It Works**: `runTest` provides test coroutine scope. `advanceTimeBy` skips virtual time. Tests execute instantly.

**Learn More**: See [Intermediate Tutorial - Coroutine Testing](/en/learn/swe/prog-lang/kotlin/tutorials/intermediate/#coroutine-testing).

---

### Recipe 32: Property-Based Testing

**Problem**: You need to test properties that should hold for wide range of inputs.

**Solution**:

```kotlin
import io.kotest.core.spec.style.StringSpec
import io.kotest.matchers.shouldBe
import io.kotest.property.Arb
import io.kotest.property.arbitrary.int
import io.kotest.property.forAll

class PropertyTest : StringSpec({
    "reversing a list twice should return original" {
        forAll(Arb.list(Arb.int())) { list ->
            list.reversed().reversed() == list
        }
    }

    "addition is commutative" {
        forAll(Arb.int(), Arb.int()) { a, b ->
            a + b == b + a
        }
    }

    "string length is non-negative" {
        forAll(Arb.string()) { str ->
            str.length >= 0
        }
    }
})
```

**How It Works**: `forAll` tests property with generated inputs. `Arb` is arbitrary value generator. Automatically finds edge cases.

**Learn More**: See [Intermediate Tutorial - Testing Strategies](/en/learn/swe/prog-lang/kotlin/tutorials/intermediate/#part-6-testing-strategies).

---

## Performance Optimization

### Recipe 33: Inline Functions for Performance

**Problem**: You need to optimize higher-order functions by eliminating lambda overhead.

**Solution**:

```kotlin
// Without inline - creates lambda object
fun measureTime(block: () -> Unit): Long {
    val start = System.currentTimeMillis()
    block()
    return System.currentTimeMillis() - start
}

// With inline - no lambda object created
inline fun measureTimeInline(block: () -> Unit): Long {
    val start = System.currentTimeMillis()
    block()
    return System.currentTimeMillis() - start
}

// Reified type parameters (only with inline)
inline fun <reified T> isInstance(value: Any): Boolean {
    return value is T
}

// Usage
val time = measureTimeInline {
    // Heavy computation
    (1..1_000_000).sum()
}

// Non-local returns (only with inline)
inline fun forEach(list: List<Int>, action: (Int) -> Unit) {
    for (item in list) {
        action(item)
    }
}

fun findFirst(): Int? {
    forEach(listOf(1, 2, 3, 4)) {
        if (it == 3) return it  // Returns from findFirst, not lambda
    }
    return null
}
```

**How It Works**: `inline` copies function body to call site. Eliminates lambda allocation. Enables reified types and non-local returns.

**Learn More**: See [Intermediate Tutorial - Inline Functions](/en/learn/swe/prog-lang/kotlin/tutorials/intermediate/#inline-functions-for-performance).

---

### Recipe 34: Avoiding Unnecessary Allocations

**Problem**: You need to reduce garbage collection pressure by avoiding unnecessary object creation.

**Solution**:

```kotlin
// ❌ Bad: Creates string on every call
fun formatBad(count: Int): String {
    return "Count: $count"
}

// ✅ Good: Reuse StringBuilder
fun formatGood(count: Int): String = buildString {
    append("Count: ")
    append(count)
}

// Use value classes (inline classes) for zero overhead
@JvmInline
value class UserId(val id: Int)

fun processUser(userId: UserId) {
    // UserId is Int at runtime - no object allocation
}

// Avoid boxing with primitives
// ❌ Bad: Boxing
val list: List<Int> = listOf(1, 2, 3)  // Uses Integer objects

// ✅ Good: IntArray (no boxing)
val array: IntArray = intArrayOf(1, 2, 3)  // Primitive ints

// Object pooling for expensive objects
class BufferPool(size: Int) {
    private val pool = ArrayDeque<ByteArray>(size)

    fun acquire(): ByteArray {
        return pool.removeFirstOrNull() ?: ByteArray(1024)
    }

    fun release(buffer: ByteArray) {
        pool.addFirst(buffer)
    }
}
```

**How It Works**: Value classes compile to underlying type at runtime. `buildString` reuses StringBuilder. Primitive arrays avoid boxing overhead.

**Learn More**: See [Advanced Tutorial - Performance](/en/learn/swe/prog-lang/kotlin/tutorials/advanced/#avoiding-boxing).

---

### Recipe 35: Using Sequences vs Collections

**Problem**: You need to choose between sequences and collections for best performance based on use case.

**Solution**:

```kotlin
import kotlin.system.measureTimeMillis

// ❌ Collections: Eager evaluation, intermediate lists
fun usingCollections(data: List<Int>): List<Int> {
    return data
        .filter { it % 2 == 0 }  // Creates intermediate list
        .map { it * 2 }          // Creates another intermediate list
        .take(10)
}

// ✅ Sequences: Lazy evaluation, no intermediate collections
fun usingSequences(data: List<Int>): List<Int> {
    return data.asSequence()
        .filter { it % 2 == 0 }  // No intermediate list
        .map { it * 2 }          // No intermediate list
        .take(10)
        .toList()                // Only final list created
}

// Performance comparison
fun comparePerformance() {
    val data = (1..10_000_000).toList()

    val collectionTime = measureTimeMillis {
        usingCollections(data)
    }

    val sequenceTime = measureTimeMillis {
        usingSequences(data)
    }

    println("Collections: ${collectionTime}ms")
    println("Sequences: ${sequenceTime}ms")
}

// When to use collections
fun smallDataCollections() {
    val numbers = listOf(1, 2, 3, 4, 5)
    // Collection is fine for small data
    numbers.filter { it > 2 }.map { it * 2 }
}

// When to use sequences
fun largeDataSequences() {
    val large = (1..1_000_000)
    // Sequence for large data with multiple operations
    large.asSequence()
        .filter { it % 2 == 0 }
        .map { it * 2 }
        .filter { it > 100 }
        .take(100)
        .toList()
}
```

**How It Works**: Collections process all elements through each operation (good for small data). Sequences process one element through all operations (good for large data or early termination).

**Learn More**: See [Using Sequences for Lazy Evaluation](/en/learn/swe/prog-lang/kotlin/tutorials/cookbook/sequences/).

---

### Recipe 36: Reified Type Parameters for Generic Functions

**Problem**: You need runtime access to generic type information, which is normally erased due to type erasure.

**Solution**:

```kotlin
import kotlin.reflect.KClass

// ❌ Without reified: Type erasure prevents runtime type check
fun <T> isInstanceOf(value: Any): Boolean {
    // return value is T  // Error: Cannot check for instance of erased type T
    return false
}

// ✅ With reified: Runtime type information available
inline fun <reified T> isInstanceOf(value: Any): Boolean {
    return value is T
}

// Practical example: Type-safe JSON parsing
inline fun <reified T> parseJson(json: String): T {
    return when (T::class) {
        String::class -> json as T
        Int::class -> json.toInt() as T
        List::class -> json.split(",").map { it.trim() } as T
        else -> throw IllegalArgumentException("Unsupported type")
    }
}

// Usage
val number: Int = parseJson("42")
val text: String = parseJson("hello")
val list: List<String> = parseJson("a, b, c")

// Real-world: Dependency injection
class Container {
    private val instances = mutableMapOf<KClass<*>, Any>()

    inline fun <reified T : Any> register(instance: T) {
        instances[T::class] = instance
    }

    inline fun <reified T : Any> get(): T {
        return instances[T::class] as? T
            ?: throw IllegalStateException("No instance of ${T::class.simpleName}")
    }
}

// Usage
interface Logger {
    fun log(message: String)
}

class ConsoleLogger : Logger {
    override fun log(message: String) = println(message)
}

val container = Container()
container.register(ConsoleLogger())
val logger = container.get<Logger>()  // Type-safe retrieval
logger.log("Hello from DI container")
```

**How It Works**: `reified` keyword (only available with `inline` functions) preserves type information at runtime by inlining the function code at call sites. The compiler replaces `T` with the actual type, making runtime type checks and reflection possible.

**Use Cases**:

- Dependency injection containers (type-safe registration/resolution)
- Generic JSON/XML parsers (avoid unsafe casts)
- Type-safe builders and DSLs (validate types at runtime)
- Reflection-based frameworks (access type information)

---

### Recipe 37: Advanced Flow Operators

**Problem**: You need to transform, combine, and manage multiple reactive data streams with complex business logic.

**Solution**:

```kotlin
import kotlinx.coroutines.*
import kotlinx.coroutines.flow.*
import kotlin.time.Duration.Companion.seconds

// Combining multiple flows
fun combinedUserData(): Flow<UserProfile> = flow {
    val userFlow = getUserUpdates()
    val preferencesFlow = getPreferenceUpdates()
    val activityFlow = getActivityUpdates()

    combine(userFlow, preferencesFlow, activityFlow) { user, prefs, activity ->
        UserProfile(user, prefs, activity)
    }.collect { emit(it) }
}

// Debouncing search queries
fun searchWithDebounce(queries: Flow<String>): Flow<List<SearchResult>> {
    return queries
        .debounce(300) // Wait 300ms after last emission
        .filter { it.length >= 3 }  // Min 3 characters
        .distinctUntilChanged()  // Ignore duplicates
        .flatMapLatest { query ->  // Cancel previous search
            flow {
                emit(performSearch(query))
            }
        }
}

// Retry with exponential backoff
fun fetchDataWithRetry(): Flow<Data> = flow {
    emit(fetchFromApi())
}.retry(3) { cause ->
    (cause is IOException).also { shouldRetry ->
        if (shouldRetry) delay(1000)  // Wait before retry
    }
}.retryWhen { cause, attempt ->
    if (cause is IOException && attempt < 3) {
        delay(1000 * (attempt + 1))  // Exponential backoff
        true
    } else false
}

// Sharing flow among multiple collectors
@OptIn(ExperimentalCoroutinesApi::class)
fun sharedSensorData(): SharedFlow<SensorData> {
    return flow {
        while (true) {
            emit(readSensor())
            delay(100)
        }
    }.shareIn(
        scope = CoroutineScope(Dispatchers.Default),
        started = SharingStarted.WhileSubscribed(5000),  // Keep active 5s after last subscriber
        replay = 1  // Replay last value to new subscribers
    )
}

// Transform and buffer
fun processLargeDataset(data: Flow<DataChunk>): Flow<ProcessedData> {
    return data
        .map { chunk -> preprocess(chunk) }
        .buffer(100)  // Buffer up to 100 items
        .flatMapMerge(concurrency = 4) { preprocessed ->  // Process 4 items concurrently
            flow { emit(heavyProcessing(preprocessed)) }
        }
        .onEach { println("Processed: $it") }
        .catch { e -> println("Error: ${e.message}") }
}

// Example data classes
data class UserProfile(val user: User, val prefs: Preferences, val activity: Activity)
data class User(val id: String, val name: String)
data class Preferences(val theme: String)
data class Activity(val lastSeen: Long)
data class SearchResult(val title: String, val url: String)
data class Data(val value: String)
data class SensorData(val temperature: Double, val humidity: Double)
data class DataChunk(val bytes: ByteArray)
data class ProcessedData(val result: String)

// Helper functions
suspend fun getUserUpdates(): Flow<User> = flow { emit(User("1", "Alice")) }
suspend fun getPreferenceUpdates(): Flow<Preferences> = flow { emit(Preferences("dark")) }
suspend fun getActivityUpdates(): Flow<Activity> = flow { emit(Activity(System.currentTimeMillis())) }
suspend fun performSearch(query: String): List<SearchResult> = listOf(SearchResult(query, "url"))
suspend fun fetchFromApi(): Data = Data("value")
suspend fun readSensor(): SensorData = SensorData(25.0, 60.0)
suspend fun preprocess(chunk: DataChunk): DataChunk = chunk
suspend fun heavyProcessing(chunk: DataChunk): ProcessedData = ProcessedData("result")
```

**How It Works**: Flow operators enable declarative stream processing:

- `combine`: Merges multiple flows into one
- `debounce`: Waits for quiet period before emitting
- `flatMapLatest`: Cancels previous operation when new value arrives
- `retry`/`retryWhen`: Automatic retry with custom logic
- `shareIn`: Converts cold flow to hot shared flow
- `buffer`: Buffers emissions for better throughput
- `flatMapMerge`: Processes multiple items concurrently

**Use Cases**:

- Search UIs (debounce user input)
- Network requests (retry with backoff)
- Sensor data (share among multiple subscribers)
- Real-time dashboards (combine multiple data sources)

---

### Recipe 38: Context Receivers for Implicit Context

**Problem**: You need to access multiple contextual values in a function without explicitly passing them as parameters.

**Solution**:

```kotlin
// Enable context receivers (Kotlin 1.6.20+)
// Add to build.gradle.kts: kotlinOptions { freeCompilerArgs += "-Xcontext-receivers" }

// Define contexts
interface LogContext {
    fun log(message: String)
}

interface DatabaseContext {
    suspend fun query(sql: String): List<String>
}

interface AuthContext {
    val currentUser: User?
}

// Function using multiple contexts
context(LogContext, DatabaseContext, AuthContext)
suspend fun processUserRequest(requestId: String) {
    log("Processing request $requestId for user ${currentUser?.name}")

    val results = query("SELECT * FROM requests WHERE id = '$requestId'")

    if (currentUser == null) {
        log("Unauthorized access attempt")
        return
    }

    log("Found ${results.size} results")
}

// Implementation with contexts
class ConsoleLog : LogContext {
    override fun log(message: String) {
        println("[LOG] $message")
    }
}

class PostgresDatabase : DatabaseContext {
    override suspend fun query(sql: String): List<String> {
        delay(100)  // Simulate DB call
        return listOf("result1", "result2")
    }
}

class SimpleAuth(override val currentUser: User?) : AuthContext

// Usage: Provide contexts
suspend fun handleRequest() {
    val log = ConsoleLog()
    val db = PostgresDatabase()
    val auth = SimpleAuth(User("123", "Alice"))

    with(log) {
        with(db) {
            with(auth) {
                processUserRequest("REQ-001")
            }
        }
    }
}

// Example: DSL with context receivers
interface BuildContext {
    fun add(item: String)
}

class ListBuilder : BuildContext {
    private val items = mutableListOf<String>()

    override fun add(item: String) {
        items.add(item)
    }

    fun build(): List<String> = items.toList()
}

context(BuildContext)
fun addGreetings() {
    add("Hello")
    add("World")
}

fun buildList(): List<String> {
    val builder = ListBuilder()
    with(builder) {
        addGreetings()
    }
    return builder.build()
}

data class User(val id: String, val name: String)
```

**How It Works**: Context receivers allow functions to require specific contexts to be available in their calling scope. The compiler ensures all required contexts are provided at call sites. This pattern eliminates parameter passing for cross-cutting concerns like logging, transactions, and authentication.

**Use Cases**:

- Logging and monitoring (implicit logger context)
- Database transactions (implicit transaction context)
- Authentication and authorization (implicit user context)
- Configuration management (implicit config context)
- DSL builders (implicit builder context)

**Learn More**: See [Intermediate Tutorial](/en/learn/swe/prog-lang/kotlin/tutorials/intermediate/) for context patterns.

---

### Recipe 39: Type-Safe Configuration DSL

**Problem**: You need a readable, type-safe way to configure complex objects without verbose builder code.

**Solution**:

```kotlin
// Define configuration classes
data class ServerConfig(
    var host: String = "localhost",
    var port: Int = 8080,
    var ssl: SslConfig? = null,
    var database: DatabaseConfig? = null,
    var logging: LoggingConfig = LoggingConfig()
)

data class SslConfig(
    var keyStore: String = "",
    var keyStorePassword: String = "",
    var enabled: Boolean = false
)

data class DatabaseConfig(
    var url: String = "",
    var username: String = "",
    var password: String = "",
    var maxConnections: Int = 10
)

data class LoggingConfig(
    var level: LogLevel = LogLevel.INFO,
    var format: String = "json",
    var outputs: MutableList<String> = mutableListOf("console")
)

enum class LogLevel { DEBUG, INFO, WARN, ERROR }

// DSL builder functions
fun server(init: ServerConfig.() -> Unit): ServerConfig {
    return ServerConfig().apply(init)
}

fun ServerConfig.ssl(init: SslConfig.() -> Unit) {
    ssl = SslConfig().apply(init)
}

fun ServerConfig.database(init: DatabaseConfig.() -> Unit) {
    database = DatabaseConfig().apply(init)
}

fun ServerConfig.logging(init: LoggingConfig.() -> Unit) {
    logging = logging.apply(init)
}

// Usage: Type-safe, readable configuration
val config = server {
    host = "api.example.com"
    port = 443

    ssl {
        enabled = true
        keyStore = "/path/to/keystore.jks"
        keyStorePassword = "secret"
    }

    database {
        url = "jdbc:postgresql://localhost:5432/mydb"
        username = "admin"
        password = "admin123"
        maxConnections = 50
    }

    logging {
        level = LogLevel.DEBUG
        format = "text"
        outputs.add("file:logs/app.log")
    }
}

// Advanced: Validation in DSL
class ValidatedServerConfig {
    var host: String = "localhost"
        set(value) {
            require(value.isNotBlank()) { "Host cannot be blank" }
            field = value
        }

    var port: Int = 8080
        set(value) {
            require(value in 1..65535) { "Port must be between 1 and 65535" }
            field = value
        }
}

fun validatedServer(init: ValidatedServerConfig.() -> Unit): ValidatedServerConfig {
    return ValidatedServerConfig().apply(init)
}

// Usage with validation
val validatedConfig = validatedServer {
    host = "api.example.com"
    port = 8443  // Validated at assignment
}

// Real-world: Gradle-style dependencies DSL
class Dependencies {
    private val deps = mutableListOf<Dependency>()

    fun implementation(notation: String) {
        deps.add(Dependency("implementation", notation))
    }

    fun testImplementation(notation: String) {
        deps.add(Dependency("testImplementation", notation))
    }

    fun getDependencies() = deps.toList()
}

data class Dependency(val scope: String, val notation: String)

fun dependencies(init: Dependencies.() -> Unit): List<Dependency> {
    return Dependencies().apply(init).getDependencies()
}

// Usage
val deps = dependencies {
    implementation("org.jetbrains.kotlin:kotlin-stdlib:1.9.0")
    implementation("io.ktor:ktor-server-core:2.3.0")
    testImplementation("io.kotest:kotest-runner-junit5:5.5.0")
}
```

**How It Works**: Kotlin's `apply` scope function and extension functions enable DSL creation. The `apply` function receives a lambda with receiver, allowing direct property access. Extension functions on configuration classes provide nested DSL blocks. Type safety ensures compile-time validation of configuration values.

**Use Cases**:

- Application configuration (server settings, database connections)
- Build scripts (Gradle-style DSLs)
- HTML/UI builders (type-safe markup)
- Test fixtures (readable test data setup)
- API clients (fluent configuration)

---

### Recipe 40: Ktor Server with Routing and Middleware

**Problem**: You need to build a production-ready REST API with authentication, validation, and error handling.

**Solution**:

```kotlin
import io.ktor.server.application.*
import io.ktor.server.engine.*
import io.ktor.server.netty.*
import io.ktor.server.routing.*
import io.ktor.server.response.*
import io.ktor.server.request.*
import io.ktor.http.*
import io.ktor.serialization.kotlinx.json.*
import io.ktor.server.plugins.contentnegotiation.*
import kotlinx.serialization.Serializable

// Data models
@Serializable
data class User(val id: String, val name: String, val email: String)

@Serializable
data class CreateUserRequest(val name: String, val email: String)

@Serializable
data class ErrorResponse(val error: String, val message: String)

// In-memory database
class UserRepository {
    private val users = mutableMapOf<String, User>()
    private var counter = 0

    fun create(name: String, email: String): User {
        val id = (++counter).toString()
        val user = User(id, name, email)
        users[id] = user
        return user
    }

    fun findById(id: String): User? = users[id]

    fun findAll(): List<User> = users.values.toList()

    fun update(id: String, name: String, email: String): User? {
        val user = users[id] ?: return null
        val updated = user.copy(name = name, email = email)
        users[id] = updated
        return updated
    }

    fun delete(id: String): Boolean = users.remove(id) != null
}

// Main application
fun Application.module() {
    val repository = UserRepository()

    // Install plugins
    install(ContentNegotiation) {
        json()
    }

    // Routing
    routing {
        route("/api/users") {
            // GET /api/users - List all users
            get {
                call.respond(repository.findAll())
            }

            // GET /api/users/{id} - Get user by ID
            get("/{id}") {
                val id = call.parameters["id"] ?: return@get call.respond(
                    HttpStatusCode.BadRequest,
                    ErrorResponse("INVALID_ID", "User ID is required")
                )

                val user = repository.findById(id)
                if (user != null) {
                    call.respond(user)
                } else {
                    call.respond(
                        HttpStatusCode.NotFound,
                        ErrorResponse("USER_NOT_FOUND", "User with ID $id not found")
                    )
                }
            }

            // POST /api/users - Create user
            post {
                val request = call.receive<CreateUserRequest>()

                // Validation
                if (request.name.isBlank()) {
                    return@post call.respond(
                        HttpStatusCode.BadRequest,
                        ErrorResponse("INVALID_NAME", "Name cannot be blank")
                    )
                }

                if (!request.email.contains("@")) {
                    return@post call.respond(
                        HttpStatusCode.BadRequest,
                        ErrorResponse("INVALID_EMAIL", "Invalid email format")
                    )
                }

                val user = repository.create(request.name, request.email)
                call.respond(HttpStatusCode.Created, user)
            }

            // PUT /api/users/{id} - Update user
            put("/{id}") {
                val id = call.parameters["id"] ?: return@put call.respond(
                    HttpStatusCode.BadRequest,
                    ErrorResponse("INVALID_ID", "User ID is required")
                )

                val request = call.receive<CreateUserRequest>()
                val updated = repository.update(id, request.name, request.email)

                if (updated != null) {
                    call.respond(updated)
                } else {
                    call.respond(
                        HttpStatusCode.NotFound,
                        ErrorResponse("USER_NOT_FOUND", "User with ID $id not found")
                    )
                }
            }

            // DELETE /api/users/{id} - Delete user
            delete("/{id}") {
                val id = call.parameters["id"] ?: return@delete call.respond(
                    HttpStatusCode.BadRequest,
                    ErrorResponse("INVALID_ID", "User ID is required")
                )

                if (repository.delete(id)) {
                    call.respond(HttpStatusCode.NoContent)
                } else {
                    call.respond(
                        HttpStatusCode.NotFound,
                        ErrorResponse("USER_NOT_FOUND", "User with ID $id not found")
                    )
                }
            }
        }
    }
}

// Start server
fun main() {
    embeddedServer(Netty, port = 8080, module = Application::module)
        .start(wait = true)
}
```

**How It Works**: Ktor provides a lightweight, asynchronous web framework for Kotlin. Key components:

- `routing {}`: Defines HTTP routes
- `get/post/put/delete`: HTTP method handlers
- `call.receive<T>()`: Deserializes request body
- `call.respond()`: Serializes response
- `ContentNegotiation`: Automatic JSON serialization
- Route parameters: `call.parameters["id"]`

**Use Cases**:

- RESTful APIs (CRUD operations)
- Microservices (lightweight, fast startup)
- WebSocket servers (real-time communication)
- GraphQL APIs (with ktor-graphql)
- Server-side rendering (with ktor-html-builder)

**Learn More**: See [Ktor Documentation](https://ktor.io) and [Web Development Tutorial](/en/learn/swe/prog-lang/kotlin/tutorials/intermediate/).

---

### Recipe 41: Extension Functions for Third-Party Libraries

**Problem**: You need to add utility methods to classes you don't own (e.g., standard library or third-party libraries).

**Solution**:

```kotlin
// String extensions
fun String.isValidEmail(): Boolean {
    val emailRegex = "^[A-Za-z0-9+_.-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$".toRegex()
    return emailRegex.matches(this)
}

fun String.toTitleCase(): String {
    return split(" ").joinToString(" ") { word ->
        word.lowercase().replaceFirstChar { it.uppercase() }
    }
}

fun String.truncate(maxLength: Int, suffix: String = "..."): String {
    return if (length <= maxLength) this
    else take(maxLength - suffix.length) + suffix
}

// Usage
val email = "user@example.com"
println(email.isValidEmail())  // true

val text = "hello world from kotlin"
println(text.toTitleCase())  // "Hello World From Kotlin"

val long = "This is a very long string that needs truncation"
println(long.truncate(20))  // "This is a very lo..."

// List extensions
fun <T> List<T>.second(): T? = getOrNull(1)
fun <T> List<T>.secondOrNull(): T? = getOrNull(1)

fun <T> List<T>.partitionBy(predicate: (T) -> Boolean): Pair<List<T>, List<T>> {
    val (matching, notMatching) = partition(predicate)
    return matching to notMatching
}

// Usage
val numbers = listOf(1, 2, 3, 4, 5)
println(numbers.second())  // 2

val (evens, odds) = numbers.partitionBy { it % 2 == 0 }
println("Evens: $evens, Odds: $odds")

// Nullable extensions
fun String?.orDefault(default: String): String = this ?: default

// Usage
val nullableString: String? = null
println(nullableString.orDefault("N/A"))  // "N/A"

// Generic extensions with reified types
inline fun <reified T> List<*>.filterIsInstance(): List<T> {
    return filter { it is T }.map { it as T }
}

// Usage
val mixed: List<Any> = listOf(1, "two", 3, "four", 5)
val strings: List<String> = mixed.filterIsInstance()
println(strings)  // [two, four]

// Extension properties
val String.firstWord: String
    get() = split(" ").firstOrNull() ?: ""

val List<Int>.average: Double
    get() = if (isEmpty()) 0.0 else sum().toDouble() / size

// Usage
println("Hello World".firstWord)  // "Hello"
println(listOf(1, 2, 3, 4, 5).average)  // 3.0

// Context-specific extensions
class User(val name: String, val age: Int)

fun List<User>.averageAge(): Double = map { it.age }.average()
fun List<User>.olderThan(age: Int): List<User> = filter { it.age > age }

// Usage
val users = listOf(
    User("Alice", 30),
    User("Bob", 25),
    User("Charlie", 35)
)
println(users.averageAge())  // 30.0
println(users.olderThan(28))  // [User(Alice, 30), User(Charlie, 35)]
```

**How It Works**: Extension functions add methods to existing classes without modifying source code. They're resolved statically (not via inheritance), so they can't override existing methods. Extensions are especially powerful when combined with nullable types and reified generics.

**Use Cases**:

- String utilities (validation, formatting, manipulation)
- Collection helpers (domain-specific queries)
- Nullable type conveniences (safe defaults)
- Third-party library adapters (wrap APIs with cleaner syntax)
- DSL builders (extend types for fluent APIs)

---

### Recipe 42: Scope Functions for Cleaner Code

**Problem**: You need to execute code in the context of an object, initialize objects, or perform operations on nullable values.

**Solution**:

```kotlin
// let: Transform object and handle nullables
data class User(val name: String, val email: String)

fun processUser(user: User?) {
    user?.let {
        println("Processing ${it.name}")
        sendEmail(it.email)
        saveToDatabase(it)
    } ?: println("User is null")
}

// also: Additional operations (debugging, logging)
fun createUser(name: String, email: String): User {
    return User(name, email).also {
        println("Created user: $it")
        logToFile("User created: ${it.name}")
    }
}

// apply: Configure object properties
fun configureServer(): ServerConfig {
    return ServerConfig().apply {
        host = "localhost"
        port = 8080
        ssl = SslConfig().apply {
            enabled = true
            keyStore = "/path/to/keystore"
        }
    }
}

// run: Execute block and return result
fun loadConfiguration(): Config {
    return Config().run {
        load("config.yml")
        validate()
        this  // Return the config
    }
}

// with: Call multiple methods on object
fun printUserDetails(user: User) {
    with(user) {
        println("Name: $name")
        println("Email: $email")
        println("Valid: ${email.contains("@")}")
    }
}

// Combining scope functions
class Builder {
    private val items = mutableListOf<String>()

    fun add(item: String) = apply { items.add(item) }

    fun build(): List<String> = items.toList()
}

val result = Builder()
    .apply {
        println("Building list...")
    }
    .add("Item 1")
    .add("Item 2")
    .also {
        println("Added ${it.build().size} items")
    }
    .build()

// Real-world: Database transaction
class DatabaseTransaction {
    private var committed = false

    fun execute(sql: String) {
        println("Executing: $sql")
    }

    fun commit() {
        committed = true
        println("Committed")
    }

    fun rollback() {
        println("Rolled back")
    }
}

fun <T> transaction(block: DatabaseTransaction.() -> T): T {
    return DatabaseTransaction().run {
        try {
            block().also { commit() }
        } catch (e: Exception) {
            rollback()
            throw e
        }
    }
}

// Usage
val user = transaction {
    execute("INSERT INTO users VALUES (...)")
    execute("INSERT INTO profiles VALUES (...)")
    User("Alice", "alice@example.com")
}

// Scope function decision guide
fun scopeFunctionExamples() {
    val user = User("Alice", "alice@example.com")

    // let: Transform object, handle nullables
    val emailDomain = user.let { it.email.substringAfter("@") }

    // also: Side effects (logging, validation)
    val validatedUser = user.also {
        require(it.email.isNotEmpty()) { "Email required" }
    }

    // apply: Configure object
    val config = ServerConfig().apply {
        host = "localhost"
        port = 8080
    }

    // run: Execute block in object context
    val isValid = user.run {
        name.isNotEmpty() && email.contains("@")
    }

    // with: Call multiple methods
    with(user) {
        println(name)
        println(email)
    }
}

// Helper classes and functions
data class ServerConfig(
    var host: String = "",
    var port: Int = 0,
    var ssl: SslConfig? = null
)

data class SslConfig(
    var enabled: Boolean = false,
    var keyStore: String = ""
)

class Config {
    fun load(file: String) {}
    fun validate() {}
}

fun sendEmail(email: String) {}
fun saveToDatabase(user: User) {}
fun logToFile(message: String) {}
```

**How It Works**: Kotlin's five scope functions provide different contexts for executing code blocks:

- **let**: Lambda receives object as `it`, returns lambda result (nullable handling)
- **also**: Lambda receives object as `it`, returns object (side effects)
- **apply**: Lambda receives object as `this`, returns object (object configuration)
- **run**: Lambda receives object as `this`, returns lambda result (execute and transform)
- **with**: Function receives object as parameter, lambda uses `this`, returns lambda result

**Use Cases**:

- `let`: Transform values, handle nullables
- `also`: Logging, validation, debugging
- `apply`: Object initialization and configuration
- `run`: Execute operations and return result
- `with`: Call multiple methods on same object

**Learn More**: See [Beginner Tutorial](/en/learn/swe/prog-lang/kotlin/tutorials/beginner/) for scope functions fundamentals.

---

### Recipe 43: Inline Value Classes for Type Safety

**Problem**: You need to distinguish between different types of the same primitive value without runtime overhead.

**Solution**:

```kotlin
// Define value classes
@JvmInline
value class UserId(val value: String)

@JvmInline
value class OrderId(val value: String)

@JvmInline
value class Email(val value: String) {
    init {
        require(value.contains("@")) { "Invalid email format" }
    }
}

@JvmInline
value class Age(val value: Int) {
    init {
        require(value in 0..150) { "Age must be between 0 and 150" }
    }
}

// Type-safe functions
fun getUserById(id: UserId): User {
    // Compiler ensures only UserId can be passed
    return User(id.value, "Alice", Email("alice@example.com"), Age(30))
}

fun getOrderById(id: OrderId): Order {
    // Different type from UserId - prevents mix-ups
    return Order(id.value, 100.0)
}

// ❌ This won't compile - type mismatch
// val user = getUserById(OrderId("123"))

// ✅ This is type-safe
val userId = UserId("user-123")
val user = getUserById(userId)

// Real-world: Domain modeling
@JvmInline
value class Currency(val code: String) {
    init {
        require(code.length == 3) { "Currency code must be 3 letters" }
    }
}

@JvmInline
value class Amount(val value: Double) {
    operator fun plus(other: Amount) = Amount(value + other.value)
    operator fun minus(other: Amount) = Amount(value - other.value)
    operator fun times(multiplier: Double) = Amount(value * multiplier)
}

data class Money(val amount: Amount, val currency: Currency) {
    operator fun plus(other: Money): Money {
        require(currency == other.currency) { "Cannot add different currencies" }
        return Money(amount + other.amount, currency)
    }
}

// Usage: Type-safe money operations
val price = Money(Amount(100.0), Currency("USD"))
val tax = Money(Amount(10.0), Currency("USD"))
val total = price + tax  // Type-safe addition

// Value class with multiple operations
@JvmInline
value class Temperature(val celsius: Double) {
    val fahrenheit: Double get() = celsius * 9/5 + 32
    val kelvin: Double get() = celsius + 273.15

    operator fun plus(other: Temperature) = Temperature(celsius + other.celsius)
    operator fun minus(other: Temperature) = Temperature(celsius - other.celsius)
}

// Usage
val temp1 = Temperature(25.0)
val temp2 = Temperature(10.0)
val diff = temp1 - temp2
println("Temperature: ${diff.celsius}°C = ${diff.fahrenheit}°F")

// Value class for units
@JvmInline
value class Meters(val value: Double) {
    operator fun plus(other: Meters) = Meters(value + other.value)
    operator fun times(scalar: Double) = Meters(value * scalar)
}

@JvmInline
value class Seconds(val value: Double) {
    operator fun plus(other: Seconds) = Seconds(value + other.value)
}

data class Velocity(val meters: Meters, val seconds: Seconds) {
    val metersPerSecond: Double get() = meters.value / seconds.value
}

// Usage: Physics calculations
val distance = Meters(100.0)
val time = Seconds(10.0)
val velocity = Velocity(distance, time)
println("Velocity: ${velocity.metersPerSecond} m/s")

// Example data classes
data class User(val id: String, val name: String, val email: Email, val age: Age)
data class Order(val id: String, val total: Double)
```

**How It Works**: Inline value classes (marked with `@JvmInline`) provide compile-time type safety without runtime overhead. The wrapper is eliminated at compile time, so `UserId("123")` becomes just `"123"` in bytecode. Initialization blocks allow validation, and operator overloading enables natural mathematical operations.

**Use Cases**:

- Domain IDs (prevent passing wrong ID type)
- Units of measurement (meters, seconds, temperature)
- Currencies and amounts (type-safe money operations)
- Email, URL, phone numbers (validated at construction)
- API keys, tokens (prevent accidental exposure)

**Learn More**: See [Intermediate Tutorial](/en/learn/swe/prog-lang/kotlin/tutorials/intermediate/) for value classes and inline types.

---

### Recipe 44: Structured Concurrency with Coroutine Scope

**Problem**: You need to manage coroutine lifecycles, prevent leaks, and handle cancellation properly in complex concurrent operations.

**Solution**:

```kotlin
import kotlinx.coroutines.*
import kotlin.time.Duration.Companion.seconds

// ❌ Bad: Unstructured concurrency (leaks, no cancellation)
class BadUserService {
    suspend fun loadUser(userId: String): User {
        GlobalScope.launch {  // Leak: never cancelled
            // Long-running operation
        }
        return User(userId, "Alice")
    }
}

// ✅ Good: Structured concurrency with coroutineScope
class GoodUserService {
    suspend fun loadUser(userId: String): User = coroutineScope {
        val userDeferred = async { fetchUserFromDb(userId) }
        val preferencesDeferred = async { fetchUserPreferences(userId) }
        val activityDeferred = async { fetchUserActivity(userId) }

        // All children complete or all are cancelled
        val user = userDeferred.await()
        val preferences = preferencesDeferred.await()
        val activity = activityDeferred.await()

        User(user.id, user.name, preferences, activity)
    }
}

// Supervisor scope for independent failures
class RobustService {
    suspend fun fetchDashboardData(): Dashboard = supervisorScope {
        val userDataDeferred = async {
            try {
                fetchUserData()
            } catch (e: Exception) {
                null  // Handle failure independently
            }
        }

        val analyticsDeferred = async {
            try {
                fetchAnalytics()
            } catch (e: Exception) {
                null  // Analytics failure doesn't affect user data
            }
        }

        val notificationsDeferred = async {
            try {
                fetchNotifications()
            } catch (e: Exception) {
                emptyList()  // Default to empty
            }
        }

        Dashboard(
            userData = userDataDeferred.await(),
            analytics = analyticsDeferred.await(),
            notifications = notificationsDeferred.await()
        )
    }
}

// Custom CoroutineScope for lifecycle management
class ViewModel {
    private val viewModelScope = CoroutineScope(
        SupervisorJob() + Dispatchers.Main
    )

    fun loadData() {
        viewModelScope.launch {
            try {
                val data = fetchData()
                updateUI(data)
            } catch (e: CancellationException) {
                // Cancellation is normal, don't log as error
                throw e
            } catch (e: Exception) {
                handleError(e)
            }
        }
    }

    fun onDestroy() {
        viewModelScope.cancel()  // Cancels all child coroutines
    }

    private suspend fun fetchData(): String {
        delay(1000)
        return "Data"
    }

    private fun updateUI(data: String) {
        println("UI updated with: $data")
    }

    private fun handleError(e: Exception) {
        println("Error: ${e.message}")
    }
}

// Timeout with structured concurrency
suspend fun fetchWithTimeout(userId: String): User? {
    return try {
        withTimeout(5.seconds) {
            fetchUserFromDb(userId)
        }
    } catch (e: TimeoutCancellationException) {
        println("Request timed out")
        null
    }
}

// Parallel execution with error handling
suspend fun processMultipleUsers(userIds: List<String>): List<User> = coroutineScope {
    userIds.map { userId ->
        async {
            try {
                fetchUserFromDb(userId)
            } catch (e: Exception) {
                println("Failed to fetch user $userId: ${e.message}")
                null
            }
        }
    }.awaitAll().filterNotNull()
}

// Cancellation cooperation
suspend fun longRunningTask() = coroutineScope {
    repeat(1000) { i ->
        ensureActive()  // Check cancellation
        println("Processing item $i")
        delay(10)  // Cancellable suspension
    }
}

// Example: Background task with cleanup
class BackgroundProcessor {
    private val scope = CoroutineScope(SupervisorJob() + Dispatchers.Default)

    fun startProcessing() {
        scope.launch {
            try {
                while (isActive) {
                    val item = fetchNextItem()
                    processItem(item)
                    delay(1000)
                }
            } finally {
                // Cleanup always runs, even if cancelled
                withContext(NonCancellable) {
                    cleanup()
                }
            }
        }
    }

    fun stop() {
        scope.cancel()
    }

    private suspend fun fetchNextItem(): String {
        delay(100)
        return "item"
    }

    private suspend fun processItem(item: String) {
        delay(100)
    }

    private suspend fun cleanup() {
        delay(100)
        println("Cleanup completed")
    }
}

// Example data classes
data class User(
    val id: String,
    val name: String,
    val preferences: UserPreferences? = null,
    val activity: UserActivity? = null
)
data class UserPreferences(val theme: String)
data class UserActivity(val lastLogin: Long)
data class Dashboard(
    val userData: String?,
    val analytics: String?,
    val notifications: List<String>
)

// Helper functions
suspend fun fetchUserFromDb(userId: String): User {
    delay(100)
    return User(userId, "Alice")
}

suspend fun fetchUserPreferences(userId: String): UserPreferences {
    delay(50)
    return UserPreferences("dark")
}

suspend fun fetchUserActivity(userId: String): UserActivity {
    delay(50)
    return UserActivity(System.currentTimeMillis())
}

suspend fun fetchUserData(): String {
    delay(100)
    return "User data"
}

suspend fun fetchAnalytics(): String {
    delay(100)
    return "Analytics"
}

suspend fun fetchNotifications(): List<String> {
    delay(100)
    return listOf("Notification 1", "Notification 2")
}
```

**How It Works**: Structured concurrency ensures coroutines follow a hierarchy:

- `coroutineScope`: Creates scope that waits for all children, propagates cancellation
- `supervisorScope`: Creates scope where child failures don't affect siblings
- `CoroutineScope(job)`: Custom scope for lifecycle management
- `ensureActive()`: Checks if scope is still active
- `withContext(NonCancellable)`: Executes cleanup even if cancelled

**Use Cases**:

- Parallel data fetching (wait for all or cancel all)
- Independent operations (analytics, logging don't block main flow)
- Lifecycle-aware operations (Android ViewModels, background services)
- Timeout handling (bounded execution time)
- Graceful shutdown (cleanup resources on cancellation)

**Learn More**: See [Intermediate Tutorial](/en/learn/swe/prog-lang/kotlin/tutorials/intermediate/) for coroutines and [Recipe 11](/en/learn/swe/prog-lang/kotlin/how-to/cookbook/#recipe-11-structured-concurrency-with-supervisorscope) for supervisorScope.

---

### Recipe 45: Property Delegation for Reusable Logic

**Problem**: You need to reuse property getter/setter logic across multiple classes without inheritance.

**Solution**:

```kotlin
import kotlin.properties.Delegates
import kotlin.properties.ReadOnlyProperty
import kotlin.properties.ReadWriteProperty
import kotlin.reflect.KProperty

// Built-in: Lazy initialization
class ExpensiveResource {
    val data: String by lazy {
        println("Computing expensive data...")
        Thread.sleep(1000)  // Simulate expensive operation
        "Expensive Data"
    }
}

// Usage
val resource = ExpensiveResource()
println("Resource created")
println(resource.data)  // First access: computes value
println(resource.data)  // Second access: returns cached value

// Built-in: Observable properties
class User {
    var name: String by Delegates.observable("Unknown") { property, oldValue, newValue ->
        println("${property.name} changed from '$oldValue' to '$newValue'")
    }

    var age: Int by Delegates.vetoable(0) { property, oldValue, newValue ->
        newValue >= 0  // Veto negative ages
    }
}

// Usage
val user = User()
user.name = "Alice"  // Prints: name changed from 'Unknown' to 'Alice'
user.age = 30  // Accepted
user.age = -5  // Rejected (age stays 30)

// Custom delegation: Validation
class ValidatedProperty<T>(
    private val initialValue: T,
    private val validator: (T) -> Boolean
) : ReadWriteProperty<Any?, T> {
    private var value = initialValue

    override fun getValue(thisRef: Any?, property: KProperty<*>): T = value

    override fun setValue(thisRef: Any?, property: KProperty<*>, value: T) {
        if (validator(value)) {
            this.value = value
        } else {
            throw IllegalArgumentException("Invalid value for ${property.name}: $value")
        }
    }
}

fun <T> validated(initial: T, validator: (T) -> Boolean) =
    ValidatedProperty(initial, validator)

// Usage
class Account {
    var balance: Double by validated(0.0) { it >= 0 }
    var email: String by validated("") { it.contains("@") }
}

val account = Account()
account.balance = 100.0  // OK
account.email = "user@example.com"  // OK
// account.balance = -50.0  // Throws IllegalArgumentException
// account.email = "invalid"  // Throws IllegalArgumentException

// Custom delegation: Logging
class LoggedProperty<T>(private var value: T) : ReadWriteProperty<Any?, T> {
    override fun getValue(thisRef: Any?, property: KProperty<*>): T {
        println("[GET] ${property.name} = $value")
        return value
    }

    override fun setValue(thisRef: Any?, property: KProperty<*>, value: T) {
        println("[SET] ${property.name} from ${this.value} to $value")
        this.value = value
    }
}

fun <T> logged(initial: T) = LoggedProperty(initial)

// Usage
class Config {
    var timeout: Int by logged(30)
    var retries: Int by logged(3)
}

val config = Config()
config.timeout = 60  // Logs: [SET] timeout from 30 to 60
println(config.timeout)  // Logs: [GET] timeout = 60

// Map delegation for dynamic properties
class DynamicConfig(private val map: Map<String, Any?>) {
    val host: String by map
    val port: Int by map
    val ssl: Boolean by map
}

// Usage
val configMap = mapOf(
    "host" to "localhost",
    "port" to 8080,
    "ssl" to true
)
val dynamicConfig = DynamicConfig(configMap)
println("${dynamicConfig.host}:${dynamicConfig.port} (SSL: ${dynamicConfig.ssl})")

// Mutable map delegation
class MutableConfig(private val map: MutableMap<String, Any?>) {
    var host: String by map
    var port: Int by map
}

val mutableMap = mutableMapOf<String, Any?>(
    "host" to "localhost",
    "port" to 8080
)
val mutableConfig = MutableConfig(mutableMap)
mutableConfig.host = "api.example.com"
println(mutableMap)  // {host=api.example.com, port=8080}

// Custom: Thread-safe property
class SynchronizedProperty<T>(private var value: T) : ReadWriteProperty<Any?, T> {
    private val lock = Any()

    override fun getValue(thisRef: Any?, property: KProperty<*>): T {
        synchronized(lock) {
            return value
        }
    }

    override fun setValue(thisRef: Any?, property: KProperty<*>, value: T) {
        synchronized(lock) {
            this.value = value
        }
    }
}

fun <T> synchronized(initial: T) = SynchronizedProperty(initial)

// Usage
class Counter {
    var count: Int by synchronized(0)

    fun increment() {
        count++  // Thread-safe
    }
}

// Custom: Cached computation
class CachedProperty<T>(
    private val ttlMillis: Long,
    private val compute: () -> T
) : ReadOnlyProperty<Any?, T> {
    private var value: T? = null
    private var lastComputed: Long = 0

    override fun getValue(thisRef: Any?, property: KProperty<*>): T {
        val now = System.currentTimeMillis()
        if (value == null || now - lastComputed > ttlMillis) {
            value = compute()
            lastComputed = now
        }
        return value!!
    }
}

fun <T> cached(ttlMillis: Long, compute: () -> T) =
    CachedProperty(ttlMillis, compute)

// Usage
class ApiClient {
    val authToken: String by cached(60_000) {  // Cache for 60 seconds
        println("Fetching new auth token...")
        Thread.sleep(500)
        "token-${System.currentTimeMillis()}"
    }
}

val client = ApiClient()
println(client.authToken)  // Fetches token
println(client.authToken)  // Returns cached token (within TTL)
```

**How It Works**: Property delegation uses the `by` keyword to delegate property access to another object. The delegate must implement `ReadOnlyProperty` (for `val`) or `ReadWriteProperty` (for `var`). Kotlin automatically calls `getValue()` and `setValue()` methods when the property is accessed or modified.

**Use Cases**:

- Lazy initialization (defer expensive computations)
- Validation (enforce constraints on property values)
- Logging and debugging (track property changes)
- Caching (TTL-based caching for computed properties)
- Thread safety (synchronized access to shared state)
- Dynamic properties (map-backed configurations)

**Learn More**: See [Intermediate Tutorial](/en/learn/swe/prog-lang/kotlin/tutorials/intermediate/) for delegation patterns.

---

### Recipe 46: Kotest for Modern Testing

**Problem**: You need expressive, readable tests with powerful matchers and property-based testing.

**Solution**:

```kotlin
import io.kotest.core.spec.style.StringSpec
import io.kotest.core.spec.style.FunSpec
import io.kotest.core.spec.style.DescribeSpec
import io.kotest.matchers.shouldBe
import io.kotest.matchers.shouldNotBe
import io.kotest.matchers.collections.*
import io.kotest.matchers.string.*
import io.kotest.property.Arb
import io.kotest.property.arbitrary.*
import io.kotest.property.checkAll
import io.kotest.assertions.throwables.shouldThrow

// StringSpec: Simplest style
class CalculatorTest : StringSpec({
    "addition should work correctly" {
        val calculator = Calculator()
        calculator.add(2, 3) shouldBe 5
    }

    "division by zero should throw exception" {
        val calculator = Calculator()
        shouldThrow<ArithmeticException> {
            calculator.divide(10, 0)
        }
    }
})

// FunSpec: Structured with test/context
class UserServiceTest : FunSpec({
    val service = UserService()

    test("should create user with valid data") {
        val user = service.createUser("Alice", "alice@example.com")
        user.name shouldBe "Alice"
        user.email shouldBe "alice@example.com"
    }

    context("email validation") {
        test("should accept valid emails") {
            val emails = listOf("user@example.com", "test.user@example.co.uk")
            emails.forEach { email ->
                service.validateEmail(email) shouldBe true
            }
        }

        test("should reject invalid emails") {
            val emails = listOf("invalid", "@example.com", "user@", "user")
            emails.forEach { email ->
                service.validateEmail(email) shouldBe false
            }
        }
    }
})

// DescribeSpec: BDD-style
class OrderProcessorTest : DescribeSpec({
    describe("OrderProcessor") {
        val processor = OrderProcessor()

        context("with valid order") {
            val order = Order("ORD-001", 100.0, listOf("item1", "item2"))

            it("should process successfully") {
                val result = processor.process(order)
                result.success shouldBe true
                result.orderId shouldBe "ORD-001"
            }

            it("should calculate total correctly") {
                val total = processor.calculateTotal(order)
                total shouldBe 100.0
            }
        }

        context("with empty order") {
            val emptyOrder = Order("ORD-002", 0.0, emptyList())

            it("should fail validation") {
                shouldThrow<IllegalArgumentException> {
                    processor.process(emptyOrder)
                }
            }
        }
    }
})

// Collection matchers
class CollectionMatchersTest : StringSpec({
    "list matchers" {
        val numbers = listOf(1, 2, 3, 4, 5)

        numbers shouldHaveSize 5
        numbers shouldContain 3
        numbers shouldContainAll listOf(1, 2, 3)
        numbers shouldNotContain 10
        numbers.first() shouldBe 1
        numbers.last() shouldBe 5
    }

    "string matchers" {
        val text = "Hello, Kotlin!"

        text shouldStartWith "Hello"
        text shouldEndWith "Kotlin!"
        text shouldContain "Kotlin"
        text shouldHaveLength 14
        text shouldMatch "Hello, .*!".toRegex()
    }
})

// Property-based testing
class PropertyBasedTest : StringSpec({
    "reverse twice should equal original" {
        checkAll<String> { str ->
            str.reversed().reversed() shouldBe str
        }
    }

    "adding two numbers should be commutative" {
        checkAll<Int, Int> { a, b ->
            (a + b) shouldBe (b + a)
        }
    }

    "list size after adding element" {
        checkAll(Arb.list(Arb.int()), Arb.int()) { list, element ->
            val newList = list + element
            newList.size shouldBe list.size + 1
            newList shouldContain element
        }
    }
})

// Custom generators
class CustomGeneratorTest : StringSpec({
    "email validator with custom generator" {
        val emailArb = Arb.string(5..20, Codepoint.az())
            .map { name -> "$name@example.com" }

        checkAll(emailArb) { email ->
            email shouldContain "@"
            email shouldEndWith "@example.com"
        }
    }

    "user age validation" {
        val validAgeArb = Arb.int(0..150)

        checkAll(validAgeArb) { age ->
            val user = User("Test", "test@example.com", age)
            user.age shouldBe age
            user.age shouldBeGreaterThanOrEqual 0
            user.age shouldBeLessThanOrEqual 150
        }
    }
})

// Example classes
class Calculator {
    fun add(a: Int, b: Int) = a + b
    fun divide(a: Int, b: Int) = if (b == 0) throw ArithmeticException("Division by zero") else a / b
}

class UserService {
    fun createUser(name: String, email: String) = User(name, email, 0)
    fun validateEmail(email: String) = email.contains("@") && email.contains(".")
}

data class User(val name: String, val email: String, val age: Int)

class OrderProcessor {
    fun process(order: Order): ProcessResult {
        if (order.items.isEmpty()) throw IllegalArgumentException("Order cannot be empty")
        return ProcessResult(true, order.id)
    }

    fun calculateTotal(order: Order) = order.total
}

data class Order(val id: String, val total: Double, val items: List<String>)
data class ProcessResult(val success: Boolean, val orderId: String)
```

**How It Works**: Kotest provides multiple test styles (StringSpec, FunSpec, DescribeSpec), expressive matchers (`shouldBe`, `shouldContain`), and property-based testing (`checkAll`). Property-based testing generates hundreds of random inputs to test invariants, catching edge cases traditional example-based tests miss.

**Use Cases**:

- Unit testing (business logic, algorithms)
- Integration testing (databases, APIs)
- Property-based testing (invariants, mathematical properties)
- BDD-style tests (describe/context/it structure)
- Data-driven testing (checkAll with custom generators)

**Learn More**: See [Kotest Documentation](https://kotest.io) and [Recipe 29](/en/learn/swe/prog-lang/kotlin/how-to/cookbook/#recipe-29-unit-testing-with-junit-5) for JUnit 5 testing.

---

### Recipe 47: Arrow Kt for Functional Programming

**Problem**: You need functional programming patterns like Option, Either, and immutable data transformations without reinventing the wheel.

**Solution**:

```kotlin
import arrow.core.*
import arrow.core.raise.either
import arrow.core.raise.Raise

// Option type for null safety
sealed class UserRepository {
    data object InMemory : UserRepository() {
        private val users = mapOf(
            "1" to User("1", "Alice", "alice@example.com"),
            "2" to User("2", "Bob", "bob@example.com")
        )

        fun findById(id: String): Option<User> = users[id].toOption()
    }
}

// Usage: Option eliminates null checks
fun displayUser(id: String) {
    UserRepository.InMemory.findById(id)
        .map { user -> "User: ${user.name} (${user.email})" }
        .getOrElse { "User not found" }
        .let(::println)
}

// Either type for error handling
sealed interface DomainError {
    data class ValidationError(val message: String) : DomainError
    data class NotFound(val id: String) : DomainError
    data class DatabaseError(val cause: Throwable) : DomainError
}

fun createUser(name: String, email: String): Either<DomainError, User> = either {
    if (name.isBlank()) {
        raise(DomainError.ValidationError("Name cannot be blank"))
    }
    if (!email.contains("@")) {
        raise(DomainError.ValidationError("Invalid email format"))
    }
    User(generateId(), name, email)
}

// Usage: Type-safe error handling
fun handleUserCreation(name: String, email: String) {
    createUser(name, email).fold(
        ifLeft = { error ->
            when (error) {
                is DomainError.ValidationError -> println("Validation failed: ${error.message}")
                is DomainError.NotFound -> println("Not found: ${error.id}")
                is DomainError.DatabaseError -> println("Database error: ${error.cause}")
            }
        },
        ifRight = { user -> println("Created user: ${user.name}") }
    )
}

// Chaining Either operations
fun getUserEmail(userId: String): Either<DomainError, String> = either {
    val user = UserRepository.InMemory.findById(userId)
        .toEither { DomainError.NotFound(userId) }
        .bind()

    if (user.email.isBlank()) {
        raise(DomainError.ValidationError("User has no email"))
    }

    user.email
}

// Validated type for accumulating errors
import arrow.core.raise.zipOrAccumulate

data class UserInput(val name: String, val email: String, val age: String)

fun validateUserInput(input: UserInput): Either<Nel<String>, ValidatedUser> = either {
    zipOrAccumulate(
        { validateName(input.name).bind() },
        { validateEmail(input.email).bind() },
        { validateAge(input.age).bind() }
    ) { name, email, age ->
        ValidatedUser(name, email, age)
    }
}

fun validateName(name: String): Either<String, String> =
    if (name.isBlank()) "Name cannot be blank".left()
    else name.right()

fun validateEmail(email: String): Either<String, String> =
    if (!email.contains("@")) "Invalid email format".left()
    else email.right()

fun validateAge(age: String): Either<String, Int> =
    age.toIntOrNull()?.let {
        if (it in 0..150) it.right()
        else "Age must be between 0 and 150".left()
    } ?: "Age must be a number".left()

// Usage: Get all validation errors at once
fun processInput(input: UserInput) {
    validateUserInput(input).fold(
        ifLeft = { errors ->
            println("Validation errors:")
            errors.forEach { println("  - $it") }
        },
        ifRight = { user ->
            println("Valid user: $user")
        }
    )
}

// Nel (NonEmptyList) for guaranteed non-empty collections
fun calculateAverage(numbers: Nel<Int>): Double =
    numbers.sum().toDouble() / numbers.size

// ❌ This won't compile - must have at least one element
// val empty = nonEmptyListOf<Int>()

// ✅ This is valid
val scores = nonEmptyListOf(85, 90, 78, 92)
println("Average: ${calculateAverage(scores)}")

// Example data classes
data class User(val id: String, val name: String, val email: String)
data class ValidatedUser(val name: String, val email: String, val age: Int)

var idCounter = 0
fun generateId(): String = (++idCounter).toString()

// Example usage
fun main() {
    displayUser("1")  // User: Alice (alice@example.com)
    displayUser("999")  // User not found

    handleUserCreation("Alice", "alice@example.com")  // Created user: Alice
    handleUserCreation("", "invalid")  // Validation failed: Name cannot be blank

    val input1 = UserInput("Alice", "alice@example.com", "30")
    processInput(input1)  // Valid user: ValidatedUser(Alice, alice@example.com, 30)

    val input2 = UserInput("", "invalid", "999")
    processInput(input2)  // Validation errors: Name cannot be blank, Invalid email format, Age must be between 0 and 150
}
```

**How It Works**: Arrow provides functional data types:

- **Option**: Represents optional values (replaces nullable types)
- **Either**: Represents success or failure (typed errors)
- **Nel**: Non-empty list (compile-time guarantee of at least one element)
- **Validated**: Accumulates multiple errors instead of failing fast
- **raise/bind**: Railway-oriented programming for error handling

**Use Cases**:

- Error handling (typed errors instead of exceptions)
- Null safety (Option instead of nullable types)
- Validation (accumulate all errors)
- Data pipelines (chain transformations with error handling)
- Domain modeling (make illegal states unrepresentable)

**Learn More**: See [Arrow Documentation](https://arrow-kt.io) and [Intermediate Tutorial](/en/learn/swe/prog-lang/kotlin/tutorials/intermediate/) for functional patterns.

---

## Summary

This cookbook provides 47 practical recipes organized into 9 categories covering the most common Kotlin programming tasks. Each recipe is designed to be self-contained and ready to adapt to your specific needs.

**Key Takeaways**:

- **Data Structures**: Use immutable collections for safety, sequences for performance, sealed classes for type safety, reified types for generic functions
- **Coroutines**: Master launch/async patterns, timeout/retry logic, Flow for reactive streams, advanced Flow operators (debounce, flatMapLatest, retry), structured concurrency patterns
- **Error Handling**: Leverage Result types, sealed classes, nullable types, functional error handling with Arrow (Option, Either, Validated)
- **Design Patterns**: Apply Kotlin's built-in support for singleton, factory, builder, observer, delegation, strategy patterns, plus property delegation for reusable logic
- **Web Development**: Build REST APIs with Ktor, handle JSON with kotlinx.serialization, type-safe configuration DSLs
- **Database**: Use Exposed for type-safe queries, JDBC extensions for cleaner code
- **Testing**: Write comprehensive tests with JUnit 5, MockK, Kotest (property-based testing, multiple test styles)
- **Performance**: Optimize with inline functions, avoid allocations, use sequences wisely, inline value classes for zero-overhead type safety
- **Modern Kotlin**: Context receivers, extension functions, scope functions, DSL construction patterns

**Next Steps**:

- Explore the [Beginner Tutorial](/en/learn/swe/prog-lang/kotlin/tutorials/beginner/) for fundamentals
- Dive into [Intermediate Tutorial](/en/learn/swe/prog-lang/kotlin/tutorials/intermediate/) for advanced patterns
- Master [Advanced Tutorial](/en/learn/swe/prog-lang/kotlin/tutorials/advanced/) for expert-level techniques
- Check [How-To Guides](/en/learn/swe/prog-lang/kotlin/how-to/) for specific tasks
