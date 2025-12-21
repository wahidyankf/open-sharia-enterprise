---
title: Cookbook
date: 2025-12-18T15:48:37+07:00
draft: false
description: Day-to-day recipes and solutions for common Kotlin programming problems - ready to copy and use
weight: 1000001
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

## Advanced Coroutines Patterns

Master sophisticated coroutine patterns for production applications.

### Recipe: StateFlow and SharedFlow for Reactive State Management

**Problem**: You need reactive state management with backpressure handling and state replay.

**Solution**:

```kotlin
import kotlinx.coroutines.flow.*
import kotlinx.coroutines.*

class UserViewModel {
    // StateFlow - Always has value, replays latest to new collectors
    private val _userState = MutableStateFlow<UserState>(UserState.Idle)
    val userState: StateFlow<UserState> = _userState.asStateFlow()

    // SharedFlow - Configurable replay, for one-time events
    private val _events = MutableSharedFlow<Event>(
        replay = 0,
        extraBufferCapacity = 64,
        onBufferOverflow = BufferOverflow.DROP_OLDEST
    )
    val events: SharedFlow<Event> = _events.asSharedFlow()

    sealed class UserState {
        object Idle : UserState()
        data class Loading(val message: String) : UserState()
        data class Success(val data: String) : UserState()
        data class Error(val error: String) : UserState()
    }

    sealed class Event {
        data class ShowMessage(val message: String) : Event()
        object NavigateBack : Event()
    }

    suspend fun loadData() {
        _userState.value = UserState.Loading("Fetching data...")
        delay(1000)

        try {
            val data = fetchData()
            _userState.value = UserState.Success(data)
            _events.emit(Event.ShowMessage("Data loaded"))
        } catch (e: Exception) {
            _userState.value = UserState.Error(e.message ?: "Unknown error")
            _events.emit(Event.ShowMessage("Failed to load data"))
        }
    }
}

// Usage in UI
lifecycleScope.launch {
    viewModel.userState.collect { state ->
        when (state) {
            is UserState.Loading -> showLoading(state.message)
            is UserState.Success -> showData(state.data)
            is UserState.Error -> showError(state.error)
            UserState.Idle -> hideLoading()
        }
    }
}
```

**When to use**: Building reactive UIs with state management and one-time events.

**Learn More**: See [Advanced Coroutines Guide](./coroutines-advanced.md).

---

### Recipe: Flow Transformation and Combination

**Problem**: You need to transform, combine, and debounce multiple data streams.

**Solution**:

```kotlin
class SearchViewModel {
    private val _searchQuery = MutableStateFlow("")

    // Debounced search with transformation
    val searchResults: Flow<List<Result>> = _searchQuery
        .debounce(300)  // Wait 300ms after typing stops
        .filter { it.length >= 3 }  // Minimum 3 characters
        .distinctUntilChanged()  // Ignore duplicate queries
        .flatMapLatest { query ->  // Cancel previous search
            flow {
                emit(performSearch(query))
            }
        }
        .catch { emit(emptyList()) }  // Handle errors gracefully

    // Combine multiple flows
    fun getUserDashboard(userId: String): Flow<Dashboard> {
        val userFlow = fetchUser(userId)
        val statsFlow = fetchStats(userId)
        val notificationsFlow = fetchNotifications(userId)

        return combine(userFlow, statsFlow, notificationsFlow) { user, stats, notifications ->
            Dashboard(user, stats, notifications)
        }
    }

    // Merge flows
    fun getAllUpdates(): Flow<Update> {
        val userUpdates = userUpdateFlow()
        val systemUpdates = systemUpdateFlow()

        return merge(userUpdates, systemUpdates)
    }

    fun updateSearchQuery(query: String) {
        _searchQuery.value = query
    }
}
```

**When to use**: Search functionality, combining multiple data sources, real-time updates.

**Learn More**: See [Flow State Management](./flow-state-management.md).

---

### Recipe: Structured Concurrency with supervisorScope

**Problem**: You need independent task execution where one failure doesn't cancel others.

**Solution**:

```kotlin
class DataSyncService {
    // Regular coroutineScope - one failure cancels all
    suspend fun syncAllOrNothing(): Result<Unit> = coroutineScope {
        try {
            val user = async { syncUserData() }
            val settings = async { syncSettings() }
            val files = async { syncFiles() }

            awaitAll(user, settings, files)
            Result.success(Unit)
        } catch (e: Exception) {
            Result.failure(e)
        }
    }

    // supervisorScope - failures are independent
    suspend fun syncIndependently(): SyncResult = supervisorScope {
        val userResult = async {
            runCatching { syncUserData() }
        }
        val settingsResult = async {
            runCatching { syncSettings() }
        }
        val filesResult = async {
            runCatching { syncFiles() }
        }

        SyncResult(
            user = userResult.await(),
            settings = settingsResult.await(),
            files = filesResult.await()
        )
    }

    data class SyncResult(
        val user: Result<Unit>,
        val settings: Result<Unit>,
        val files: Result<Unit>
    ) {
        val allSucceeded = user.isSuccess && settings.isSuccess && files.isSuccess
        val anySucceeded = user.isSuccess || settings.isSuccess || files.isSuccess
    }
}
```

**When to use**: Independent tasks, partial failure tolerance, batch operations.

**Learn More**: See [Handle Coroutines and Async](./handle-coroutines-and-async.md).

---

### Recipe: Channel-Based Producer-Consumer

**Problem**: You need communication between multiple coroutines with buffering.

**Solution**:

```kotlin
class TaskQueue {
    private val taskChannel = Channel<Task>(capacity = Channel.BUFFERED)

    data class Task(val id: Int, val data: String)

    // Producer
    suspend fun submitTask(task: Task) {
        taskChannel.send(task)
        println("Submitted task ${task.id}")
    }

    // Consumer
    fun startWorkers(workerCount: Int, scope: CoroutineScope) {
        repeat(workerCount) { workerId ->
            scope.launch {
                for (task in taskChannel) {
                    processTask(workerId, task)
                }
            }
        }
    }

    private suspend fun processTask(workerId: Int, task: Task) {
        println("Worker $workerId processing task ${task.id}")
        delay(1000)  // Simulate work
        println("Worker $workerId completed task ${task.id}")
    }

    fun close() {
        taskChannel.close()
    }
}

// Usage
val queue = TaskQueue()
val scope = CoroutineScope(Dispatchers.Default)

// Start 3 workers
queue.startWorkers(3, scope)

// Submit tasks
scope.launch {
    repeat(10) { i ->
        queue.submitTask(TaskQueue.Task(i, "Data $i"))
        delay(100)
    }
    queue.close()
}
```

**When to use**: Work queues, producer-consumer patterns, rate limiting.

**Learn More**: See [Advanced Coroutines Guide](./coroutines-advanced.md#channels).

---

### Recipe: Coroutine Cancellation and Cleanup

**Problem**: You need proper cancellation handling and resource cleanup.

**Solution**:

```kotlin
class ResourceManager {
    suspend fun processWithCleanup() {
        val resource = acquireResource()

        try {
            // Main work
            repeat(100) { i ->
                ensureActive()  // Check cancellation
                processItem(i)
                delay(100)
            }
        } catch (e: CancellationException) {
            println("Processing cancelled")
            throw e  // Must re-throw CancellationException
        } finally {
            // Cleanup - runs even on cancellation
            withContext(NonCancellable) {
                resource.close()
                println("Resource cleaned up")
            }
        }
    }

    // Timeout with fallback
    suspend fun fetchWithTimeout(timeout: Long): Data? {
        return try {
            withTimeout(timeout) {
                fetchData()
            }
        } catch (e: TimeoutCancellationException) {
            println("Timed out, using cached data")
            getCachedData()
        }
    }

    // Exception handler
    private val exceptionHandler = CoroutineExceptionHandler { _, exception ->
        println("Coroutine failed: ${exception.message}")
    }

    fun startBackgroundTask(scope: CoroutineScope) {
        scope.launch(exceptionHandler) {
            // Uncaught exceptions handled by exceptionHandler
            riskyOperation()
        }
    }
}
```

**When to use**: Resource management, timeouts, background tasks, cleanup operations.

**Learn More**: See [Error Handling Patterns](./error-handling-patterns.md).

---

## Kotlin-Specific Features

Leverage Kotlin's unique language features for cleaner code.

### Recipe: Extension Functions for Domain-Specific APIs

**Problem**: You want to add functionality to existing types without inheritance.

**Solution**:

```kotlin
// String extensions
fun String.isValidEmail(): Boolean =
    matches(Regex("^[A-Za-z0-9+_.-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$"))

fun String.truncate(maxLength: Int, ellipsis: String = "..."): String =
    if (length <= maxLength) this else take(maxLength - ellipsis.length) + ellipsis

fun String.toTitleCase(): String =
    split(" ").joinToString(" ") { it.capitalize() }

// Collection extensions
fun <T> List<T>.second(): T = this[1]

fun <T> List<T>.secondOrNull(): T? = getOrNull(1)

fun <K, V> Map<K, V>.getOrThrow(key: K, message: String = "Key not found: $key"): V =
    this[key] ?: throw IllegalArgumentException(message)

// Nullable extensions
fun <T : Any> T?.orThrow(message: String): T =
    this ?: throw IllegalStateException(message)

// Domain-specific extensions
data class User(val id: String, val name: String, val age: Int)

fun User.isAdult(): Boolean = age >= 18

fun User.displayName(): String = name.toTitleCase()

fun List<User>.adults(): List<User> = filter { it.isAdult() }

fun List<User>.sortedByName(): List<User> = sortedBy { it.name }

// Usage
val email = "user@example.com"
if (email.isValidEmail()) {
    println("Valid email")
}

val text = "This is a very long text that needs truncation"
println(text.truncate(20))  // "This is a very lo..."

val users = listOf(
    User("1", "john doe", 25),
    User("2", "jane smith", 17)
)
val adults = users.adults().sortedByName()
```

**When to use**: Adding utility functions, domain-specific operations, improving readability.

**Learn More**: See [Extension Functions Guide](./extension-functions.md).

---

### Recipe: Sealed Classes for Type-Safe States

**Problem**: You need exhaustive when expressions and type-safe state modeling.

**Solution**:

```kotlin
// Network result modeling
sealed class NetworkResult<out T> {
    data class Success<T>(val data: T) : NetworkResult<T>()
    data class Error(val exception: Exception, val code: Int? = null) : NetworkResult<Nothing>()
    object Loading : NetworkResult<Nothing>()
    object Empty : NetworkResult<Nothing>()

    // Utility methods
    fun <R> map(transform: (T) -> R): NetworkResult<R> = when (this) {
        is Success -> Success(transform(data))
        is Error -> this
        is Loading -> this
        is Empty -> this
    }

    fun getOrNull(): T? = when (this) {
        is Success -> data
        else -> null
    }

    fun getOrThrow(): T = when (this) {
        is Success -> data
        is Error -> throw exception
        is Loading -> throw IllegalStateException("Still loading")
        is Empty -> throw NoSuchElementException("No data")
    }
}

// UI state modeling
sealed class UiState<out T> {
    object Idle : UiState<Nothing>()
    object Loading : UiState<Nothing>()
    data class Content<T>(val data: T) : UiState<T>()
    data class Error(val message: String) : UiState<Nothing>()
    object Empty : UiState<Nothing>()
}

// Usage with exhaustive when
fun handleNetworkResult(result: NetworkResult<User>) {
    when (result) {
        is NetworkResult.Success -> showUser(result.data)
        is NetworkResult.Error -> showError(result.exception.message)
        is NetworkResult.Loading -> showLoading()
        is NetworkResult.Empty -> showEmptyState()
    }  // Compiler ensures all cases handled
}

// Transform results
val userNames: NetworkResult<List<String>> = userResult.map { users ->
    users.map { it.name }
}
```

**When to use**: State machines, result types, exhaustive pattern matching, type-safe APIs.

**Learn More**: See [Use Sealed Classes](./use-sealed-classes.md).

---

### Recipe: Inline Functions and Reified Types

**Problem**: You need type information at runtime without reflection overhead.

**Solution**:

```kotlin
// Generic JSON parsing with reified types
inline fun <reified T> String.parseJson(): T {
    val gson = Gson()
    return gson.fromJson(this, T::class.java)
}

// Type-safe filtering
inline fun <reified T> List<*>.filterIsInstance(): List<T> =
    filter { it is T }.map { it as T }

// Intent creation (Android)
inline fun <reified T : Activity> Context.startActivity() {
    val intent = Intent(this, T::class.java)
    startActivity(intent)
}

// Repository pattern with reified types
class Repository {
    inline fun <reified T : Any> findById(id: String): T? {
        val tableName = T::class.simpleName
        return database.query(tableName, id) as? T
    }

    inline fun <reified T : Any> findAll(): List<T> {
        val tableName = T::class.simpleName
        return database.queryAll(tableName).filterIsInstance<T>()
    }
}

// Usage
val json = """{"name":"John","age":30}"""
val user = json.parseJson<User>()  // Type inferred!

val mixed: List<Any> = listOf(1, "text", 2, User("1", "John", 25))
val users: List<User> = mixed.filterIsInstance<User>()

// Android
context.startActivity<MainActivity>()  // Type-safe!

// Database
val user = repository.findById<User>("123")
val allUsers = repository.findAll<User>()
```

**When to use**: Generic APIs, type-safe operations, avoiding reflection, library design.

**Learn More**: See [Use Inline Functions and Reified](./use-inline-functions-reified.md).

---

### Recipe: Delegation Patterns

**Problem**: You want to reuse behavior without inheritance.

**Solution**:

```kotlin
// Property delegation
class User {
    // Lazy initialization
    val expensiveProperty: String by lazy {
        println("Computing expensive value")
        computeExpensiveValue()
    }

    // Observable property
    var name: String by Delegates.observable("initial") { property, oldValue, newValue ->
        println("${property.name} changed from $oldValue to $newValue")
    }

    // Vetoable property
    var age: Int by Delegates.vetoable(0) { property, oldValue, newValue ->
        newValue >= 0  // Only allow non-negative ages
    }

    // Map-backed properties
    class MappedUser(map: Map<String, Any>) {
        val name: String by map
        val age: Int by map
    }
}

// Interface delegation
interface Logger {
    fun log(message: String)
}

class ConsoleLogger : Logger {
    override fun log(message: String) {
        println("[LOG] $message")
    }
}

// UserService delegates logging to ConsoleLogger
class UserService(logger: Logger) : Logger by logger {
    fun createUser(name: String) {
        log("Creating user: $name")  // Delegated to logger
        // Create user logic
    }
}

// Custom delegate
class Preference<T>(
    private val key: String,
    private val defaultValue: T,
    private val preferences: SharedPreferences
) {
    operator fun getValue(thisRef: Any?, property: KProperty<*>): T {
        return when (defaultValue) {
            is String -> preferences.getString(key, defaultValue as String) as T
            is Int -> preferences.getInt(key, defaultValue as Int) as T
            is Boolean -> preferences.getBoolean(key, defaultValue as Boolean) as T
            else -> throw IllegalArgumentException("Unsupported type")
        }
    }

    operator fun setValue(thisRef: Any?, property: KProperty<*>, value: T) {
        preferences.edit().apply {
            when (value) {
                is String -> putString(key, value)
                is Int -> putInt(key, value)
                is Boolean -> putBoolean(key, value)
            }
            apply()
        }
    }
}

// Usage
class Settings(prefs: SharedPreferences) {
    var username: String by Preference("username", "", prefs)
    var fontSize: Int by Preference("fontSize", 14, prefs)
    var darkMode: Boolean by Preference("darkMode", false, prefs)
}
```

**When to use**: Property initialization, change observation, interface composition, shared preferences.

**Learn More**: See [Use Delegates](./use-delegates.md).

---

## Functional Programming Patterns

Apply functional programming techniques in Kotlin.

### Recipe: Higher-Order Functions and Function Composition

**Problem**: You need to compose and chain operations functionally.

**Solution**:

```kotlin
// Function composition
infix fun <A, B, C> ((A) -> B).then(other: (B) -> C): (A) -> C = { a ->
    other(this(a))
}

// Pipe operator
infix fun <T, R> T.pipe(transform: (T) -> R): R = transform(this)

// Practical example
val processUser = ::validateUser then ::enrichUser then ::saveUser

fun validateUser(user: User): User {
    require(user.email.isNotBlank()) { "Email required" }
    return user
}

fun enrichUser(user: User): User =
    user.copy(name = user.name.toTitleCase())

fun saveUser(user: User): User {
    database.save(user)
    return user
}

// Usage
val result = User("1", "john", "john@example.com")
    .pipe(processUser)

// Currying
fun add(a: Int): (Int) -> Int = { b -> a + b }

val add5 = add(5)
println(add5(3))  // 8

// Partial application
fun <A, B, C> ((A, B) -> C).partial(a: A): (B) -> C = { b -> this(a, b) }

fun multiply(a: Int, b: Int): Int = a * b
val multiplyBy10 = ::multiply.partial(10)
println(multiplyBy10(5))  // 50

// Memoization
fun <A, R> ((A) -> R).memoize(): (A) -> R {
    val cache = mutableMapOf<A, R>()
    return { a ->
        cache.getOrPut(a) { this(a) }
    }
}

fun fibonacci(n: Int): Int = if (n <= 1) n else fibonacci(n - 1) + fibonacci(n - 2)
val memoizedFib = ::fibonacci.memoize()
```

**When to use**: Function composition, pipeline processing, currying, memoization.

**Learn More**: See [Work with Scope Functions](./work-with-scope-functions.md).

---

### Recipe: Monadic Operations with Result

**Problem**: You need functional error handling without exceptions.

**Solution**:

```kotlin
// Result chaining
fun processUser(id: String): Result<User> =
    findUser(id)
        .mapCatching { user -> validateUser(user) }
        .mapCatching { user -> enrichUser(user) }
        .mapCatching { user -> saveUser(user) }

fun findUser(id: String): Result<User> = runCatching {
    database.findById(id) ?: throw NoSuchElementException("User not found")
}

// Railway-oriented programming
sealed class Either<out L, out R> {
    data class Left<L>(val value: L) : Either<L, Nothing>()
    data class Right<R>(val value: R) : Either<Nothing, R>()

    fun <T> map(transform: (R) -> T): Either<L, T> = when (this) {
        is Left -> this
        is Right -> Right(transform(value))
    }

    fun <T> flatMap(transform: (R) -> Either<L, T>): Either<L, T> = when (this) {
        is Left -> this
        is Right -> transform(value)
    }
}

typealias ValidationResult<T> = Either<List<String>, T>

fun validateUserData(user: User): ValidationResult<User> {
    val errors = mutableListOf<String>()

    if (user.email.isBlank()) errors.add("Email required")
    if (user.name.length < 3) errors.add("Name too short")
    if (user.age < 0) errors.add("Invalid age")

    return if (errors.isEmpty()) {
        Either.Right(user)
    } else {
        Either.Left(errors)
    }
}
```

**When to use**: Functional error handling, validation, railway-oriented programming.

**Learn More**: See [Error Handling Patterns](./error-handling-patterns.md).

---

### Recipe: Sequence Operations for Lazy Evaluation

**Problem**: You need efficient processing of large collections without intermediate allocations.

**Solution**:

```kotlin
// Lazy sequence processing
val largeDataset = (1..1_000_000).asSequence()
    .filter { it % 2 == 0 }  // Not executed yet
    .map { it * it }  // Not executed yet
    .take(10)  // Only first 10 elements computed
    .toList()  // Execution triggered here

// Infinite sequences
fun fibonacci(): Sequence<Long> = sequence {
    var a = 0L
    var b = 1L
    while (true) {
        yield(a)
        val next = a + b
        a = b
        b = next
    }
}

val first10Fib = fibonacci().take(10).toList()

// File processing (memory efficient)
fun processLargeFile(file: File): Map<String, Int> =
    file.bufferedReader()
        .lineSequence()
        .filter { it.isNotBlank() }
        .flatMap { it.split(" ").asSequence() }
        .map { it.lowercase() }
        .groupingBy { it }
        .eachCount()

// Custom sequence generator
fun <T> repeatSequence(value: T): Sequence<T> = generateSequence { value }

val tenOnes = repeatSequence(1).take(10).toList()

// Windowed operations
val movingAverage = listOf(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    .asSequence()
    .windowed(size = 3, step = 1)
    .map { window -> window.average() }
    .toList()

// Chunked processing
fun processInBatches(items: List<Item>) {
    items.asSequence()
        .chunked(100)
        .forEach { batch ->
            processBatch(batch)
        }
}
```

**When to use**: Large datasets, infinite sequences, file processing, memory optimization.

**Learn More**: See [Work with Collections](./work-with-collections.md).

---

## DSL Creation

Build type-safe domain-specific languages.

### Recipe: Type-Safe Builder DSL

**Problem**: You want to build structured data programmatically with compile-time safety.

**Solution**:

```kotlin
@DslMarker
annotation class HtmlDsl

@HtmlDsl
abstract class Tag(val name: String) {
    val children = mutableListOf<Tag>()
    val attributes = mutableMapOf<String, String>()

    protected fun <T : Tag> initTag(tag: T, init: T.() -> Unit): T {
        tag.init()
        children.add(tag)
        return tag
    }

    override fun toString(): String {
        val attrs = attributes.map { (k, v) -> """$k="$v"""" }.joinToString(" ")
        val attrsStr = if (attrs.isNotEmpty()) " $attrs" else ""
        val childrenStr = children.joinToString("")
        return "<$name$attrsStr>$childrenStr</$name>"
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
            override fun toString() = this@unaryPlus
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
            override fun toString() = this@unaryPlus
        })
    }
}

class P : Tag("p") {
    operator fun String.unaryPlus() {
        children.add(object : Tag("") {
            override fun toString() = this@unaryPlus
        })
    }
}

class Div : Tag("div") {
    var id: String
        get() = attributes["id"] ?: ""
        set(value) { attributes["id"] = value }

    var cssClass: String
        get() = attributes["class"] ?: ""
        set(value) { attributes["class"] = value }

    fun p(init: P.() -> Unit) = initTag(P(), init)
}

fun html(init: HTML.() -> Unit): HTML {
    val html = HTML()
    html.init()
    return html
}

// Usage
val page = html {
    head {
        title {
            +"My Page"
        }
    }
    body {
        h1 {
            +"Welcome"
        }
        div {
            id = "content"
            cssClass = "container"
            p {
                +"This is a paragraph"
            }
        }
    }
}

println(page)
```

**When to use**: HTML generation, configuration DSLs, builder patterns.

**Learn More**: See [Build REST APIs with Ktor](./build-rest-apis-ktor.md).

---

### Recipe: Type-Safe SQL Query DSL

**Problem**: You want compile-time safe SQL queries without string concatenation.

**Solution**:

```kotlin
@DslMarker
annotation class SqlDsl

@SqlDsl
class SelectBuilder {
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

    fun orderBy(column: String, direction: String = "ASC") {
        orderByClauses.add("$column $direction")
    }

    fun limit(value: Int) {
        limitValue = value
    }

    fun build(): String {
        val columnsStr = if (columns.isEmpty()) "*" else columns.joinToString(", ")
        var sql = "SELECT $columnsStr FROM $tableName"

        if (whereClauses.isNotEmpty()) {
            sql += " WHERE " + whereClauses.joinToString(" AND ")
        }

        if (orderByClauses.isNotEmpty()) {
            sql += " ORDER BY " + orderByClauses.joinToString(", ")
        }

        limitValue?.let {
            sql += " LIMIT $it"
        }

        return sql
    }
}

fun select(init: SelectBuilder.() -> Unit): String {
    val builder = SelectBuilder()
    builder.init()
    return builder.build()
}

// Usage
val query = select {
    select("id", "name", "email")
    from("users")
    where("age > 18")
    where("active = true")
    orderBy("name", "ASC")
    limit(10)
}

println(query)
// SELECT id, name, email FROM users WHERE age > 18 AND active = true ORDER BY name ASC LIMIT 10
```

**When to use**: SQL builders, configuration, fluent APIs, type-safe builders.

**Learn More**: See [Database Access with Exposed](./database-access-exposed.md).

---

## Multiplatform Development

Build cross-platform applications with shared code.

### Recipe: Expect/Actual for Platform-Specific APIs

**Problem**: You need platform-specific implementations with shared interface.

**Solution**:

```kotlin
// commonMain - Expect declarations
expect class Platform() {
    val name: String
}

expect fun getPlatformName(): String

expect object Logger {
    fun log(message: String)
    fun error(message: String, throwable: Throwable?)
}

expect class KeyValueStorage {
    fun getString(key: String): String?
    fun putString(key: String, value: String)
    fun remove(key: String)
}

// androidMain - Android implementations
actual class Platform actual constructor() {
    actual val name: String = "Android ${android.os.Build.VERSION.SDK_INT}"
}

actual fun getPlatformName(): String = Platform().name

actual object Logger {
    actual fun log(message: String) {
        android.util.Log.d("KMP", message)
    }

    actual fun error(message: String, throwable: Throwable?) {
        android.util.Log.e("KMP", message, throwable)
    }
}

actual class KeyValueStorage(private val context: Context) {
    private val prefs = context.getSharedPreferences("kmp_storage", Context.MODE_PRIVATE)

    actual fun getString(key: String): String? = prefs.getString(key, null)

    actual fun putString(key: String, value: String) {
        prefs.edit().putString(key, value).apply()
    }

    actual fun remove(key: String) {
        prefs.edit().remove(key).apply()
    }
}

// iosMain - iOS implementations
actual class Platform actual constructor() {
    actual val name: String =
        UIDevice.currentDevice.systemName() + " " + UIDevice.currentDevice.systemVersion
}

actual fun getPlatformName(): String = Platform().name

actual object Logger {
    actual fun log(message: String) {
        NSLog("KMP: %@", message)
    }

    actual fun error(message: String, throwable: Throwable?) {
        NSLog("KMP ERROR: %@ - %@", message, throwable?.message ?: "")
    }
}

actual class KeyValueStorage {
    private val userDefaults = NSUserDefaults.standardUserDefaults

    actual fun getString(key: String): String? = userDefaults.stringForKey(key)

    actual fun putString(key: String, value: String) {
        userDefaults.setObject(value, forKey = key)
    }

    actual fun remove(key: String) {
        userDefaults.removeObjectForKey(key)
    }
}

// Shared business logic in commonMain
class UserRepository(private val storage: KeyValueStorage) {
    fun saveUser(user: User) {
        val json = Json.encodeToString(User.serializer(), user)
        storage.putString("current_user", json)
        Logger.log("User saved: ${user.name}")
    }

    fun getCurrentUser(): User? {
        val json = storage.getString("current_user") ?: return null
        return try {
            Json.decodeFromString(User.serializer(), json)
        } catch (e: Exception) {
            Logger.error("Failed to parse user", e)
            null
        }
    }
}
```

**When to use**: Cross-platform apps, shared business logic, platform abstraction.

**Learn More**: See [Multiplatform Development](./multiplatform-development.md).

---

### Recipe: Shared Network Layer with Ktor

**Problem**: You need HTTP client that works on all platforms.

**Solution**:

```kotlin
// commonMain - build.gradle.kts
val commonMain by getting {
    dependencies {
        implementation("io.ktor:ktor-client-core:3.0.3")
        implementation("io.ktor:ktor-client-content-negotiation:3.0.3")
        implementation("io.ktor:ktor-serialization-kotlinx-json:3.0.3")
    }
}

val androidMain by getting {
    dependencies {
        implementation("io.ktor:ktor-client-okhttp:3.0.3")
    }
}

val iosMain by getting {
    dependencies {
        implementation("io.ktor:ktor-client-darwin:3.0.3")
    }
}

// commonMain - Shared API client
class ApiClient {
    private val client = HttpClient {
        install(ContentNegotiation) {
            json(Json {
                ignoreUnknownKeys = true
                isLenient = true
            })
        }
    }

    suspend fun fetchUsers(): Result<List<User>> = runCatching {
        client.get("https://api.example.com/users").body<List<User>>()
    }

    suspend fun createUser(user: User): Result<User> = runCatching {
        client.post("https://api.example.com/users") {
            setBody(user)
        }.body<User>()
    }

    suspend fun updateUser(id: String, user: User): Result<User> = runCatching {
        client.put("https://api.example.com/users/$id") {
            setBody(user)
        }.body<User>()
    }

    suspend fun deleteUser(id: String): Result<Unit> = runCatching {
        client.delete("https://api.example.com/users/$id")
    }

    fun close() {
        client.close()
    }
}
```

**When to use**: Shared networking, API clients, cross-platform data access.

**Learn More**: See [Multiplatform Development](./multiplatform-development.md#networking-with-ktor-client).

---

## Testing Strategies

Write comprehensive tests for Kotlin code.

### Recipe: MockK for Mocking and Verification

**Problem**: You need powerful mocking with minimal boilerplate.

**Solution**:

```kotlin
class UserServiceTest {
    private val repository = mockk<UserRepository>()
    private val service = UserService(repository)

    @Test
    fun `should return user when found`() {
        // Given
        val userId = "123"
        val user = User(userId, "John", "john@example.com")
        every { repository.findById(userId) } returns user

        // When
        val result = service.getUser(userId)

        // Then
        assertEquals(user, result)
        verify(exactly = 1) { repository.findById(userId) }
    }

    @Test
    fun `should throw when user not found`() {
        // Given
        every { repository.findById(any()) } returns null

        // When/Then
        assertFailsWith<NoSuchElementException> {
            service.getUser("999")
        }
    }

    @Test
    fun `should save user with generated id`() {
        // Given
        val user = User("", "Jane", "jane@example.com")
        val slot = slot<User>()
        every { repository.save(capture(slot)) } returns Unit

        // When
        service.createUser(user.name, user.email)

        // Then
        verify { repository.save(any()) }
        assertTrue(slot.captured.id.isNotEmpty())
        assertEquals(user.name, slot.captured.name)
    }

    @Test
    fun `should call repository methods in order`() {
        // Given
        val userId = "123"
        every { repository.findById(userId) } returns User(userId, "John", "john@example.com")
        every { repository.update(any()) } returns Unit
        every { repository.save(any()) } returns Unit

        // When
        service.updateAndBackup(userId)

        // Then
        verifyOrder {
            repository.findById(userId)
            repository.update(any())
            repository.save(any())
        }
    }

    @Test
    fun `should handle suspend functions`() = runTest {
        // Given
        val api = mockk<ApiClient>()
        coEvery { api.fetchUsers() } returns Result.success(listOf(
            User("1", "John", "john@example.com")
        ))

        // When
        val result = api.fetchUsers()

        // Then
        assertTrue(result.isSuccess)
        coVerify { api.fetchUsers() }
    }
}
```

**When to use**: Unit testing, mocking dependencies, verifying interactions, testing coroutines.

**Learn More**: See [Write Effective Tests](./write-effective-tests.md).

---

### Recipe: Property-Based Testing with Kotest

**Problem**: You need to test properties across many random inputs.

**Solution**:

```kotlin
class StringUtilsTest : FunSpec({
    test("reverse of reverse should equal original") {
        checkAll<String> { str ->
            str.reversed().reversed() shouldBe str
        }
    }

    test("length should be preserved after reversing") {
        checkAll<String> { str ->
            str.reversed().length shouldBe str.length
        }
    }

    test("sum of two positives is always positive") {
        checkAll(Arb.positiveInt(), Arb.positiveInt()) { a, b ->
            (a + b) shouldBeGreaterThan 0
        }
    }

    test("list concatenation is associative") {
        checkAll<List<Int>, List<Int>, List<Int>> { a, b, c ->
            ((a + b) + c) shouldBe (a + (b + c))
        }
    }

    test("custom generator for email") {
        val emailArb = arbitrary {
            val username = Arb.string(5..10, Codepoint.alphanumeric()).bind()
            val domain = Arb.string(5..10, Codepoint.alphanumeric()).bind()
            "$username@$domain.com"
        }

        checkAll(emailArb) { email ->
            email should contain("@")
            email should endWith(".com")
        }
    }

    test("user validation properties") {
        val userArb = arbitrary {
            User(
                id = Arb.string(10).bind(),
                name = Arb.string(3..50).bind(),
                age = Arb.int(0..150).bind()
            )
        }

        checkAll(userArb) { user ->
            validateUser(user).let { result ->
                if (user.age >= 18) {
                    result shouldBe Valid(user)
                } else {
                    result shouldBe Invalid("Underage")
                }
            }
        }
    }
})
```

**When to use**: Testing properties, invariants, edge cases, generating test data.

**Learn More**: See [Write Effective Tests](./write-effective-tests.md#property-based-testing).

---

## Advanced Coroutines Recipes

### Recipe: Channel-Based Communication Patterns

**Problem**: You need thread-safe communication between coroutines.

**Solution**:

```kotlin
import kotlinx.coroutines.*
import kotlinx.coroutines.channels.*

// Producer-Consumer Pattern
fun CoroutineScope.produceNumbers(count: Int) = produce {
    repeat(count) {
        delay(100)
        send(it)
        println("Produced: $it")
    }
}

fun CoroutineScope.squareNumbers(numbers: ReceiveChannel<Int>) = produce {
    for (num in numbers) {
        val squared = num * num
        send(squared)
        println("Squared: $num -> $squared")
    }
}

// Fan-out: Multiple workers processing from single channel
fun CoroutineScope.launchWorkers(channel: ReceiveChannel<Int>, count: Int) {
    repeat(count) { id ->
        launch {
            for (item in channel) {
                delay(Random.nextLong(100, 300))
                println("Worker $id processed $item")
            }
        }
    }
}

// Buffered channel for backpressure
suspend fun bufferedChannelExample() = coroutineScope {
    val channel = Channel<Int>(capacity = 5) // Buffer size = 5

    launch {
        repeat(10) {
            channel.send(it)
            println("Sent: $it")
        }
        channel.close()
    }

    delay(500) // Simulate slow consumer

    launch {
        for (item in channel) {
            delay(100)
            println("Received: $it")
        }
    }
}

// Usage
runBlocking {
    val numbers = produceNumbers(5)
    val squares = squareNumbers(numbers)

    for (square in squares) {
        println("Result: $square")
    }
}
```

**When to use**: Producer-consumer patterns, work distribution, pipeline processing, backpressure handling.

**Learn More**: See [Work with Coroutines](./work-with-coroutines.md).

---

### Recipe: Structured Concurrency and Supervision

**Problem**: You need to manage coroutine hierarchies and handle child failures.

**Solution**:

```kotlin
import kotlinx.coroutines.*

// Structured concurrency with proper cleanup
suspend fun structuredConcurrencyExample() = coroutineScope {
    println("Parent scope started")

    launch {
        repeat(5) { i ->
            delay(100)
            println("Child 1: $i")
        }
    }

    launch {
        repeat(3) { i ->
            delay(150)
            println("Child 2: $i")
        }
    }

    println("Parent scope waiting for children")
    // Automatically waits for all children
}

// SupervisorJob for independent child failures
suspend fun supervisedConcurrency() = coroutineScope {
    val supervisor = SupervisorJob()

    with(CoroutineScope(coroutineContext + supervisor)) {
        val child1 = launch {
            delay(100)
            throw Exception("Child 1 failed!")
        }

        val child2 = launch {
            repeat(5) { i ->
                delay(200)
                println("Child 2: $i (still running)")
            }
        }

        child1.join()
        child2.join()
    }

    println("Supervisor completed")
}

// Cancellation and cleanup
suspend fun cancellationExample() = coroutineScope {
    val job = launch {
        try {
            repeat(10) { i ->
                delay(100)
                println("Working: $i")
            }
        } finally {
            withContext(NonCancellable) {
                println("Cleanup resources")
                delay(100)
                println("Cleanup done")
            }
        }
    }

    delay(350)
    println("Cancelling job")
    job.cancelAndJoin()
    println("Job cancelled")
}
```

**When to use**: Managing coroutine lifecycles, handling failures, cancellation with cleanup, independent task execution.

**Learn More**: See [Work with Coroutines](./work-with-coroutines.md#structured-concurrency).

---

### Recipe: Advanced Flow Operators

**Problem**: You need to transform, combine, and process asynchronous data streams.

**Solution**:

```kotlin
import kotlinx.coroutines.*
import kotlinx.coroutines.flow.*

// Combining flows
suspend fun combineFlowsExample() {
    val numbers = flowOf(1, 2, 3).onEach { delay(100) }
    val strings = flowOf("A", "B", "C").onEach { delay(150) }

    // Combine latest values
    numbers.combine(strings) { num, str ->
        "$str-$num"
    }.collect { println("Combined: $it") }

    // Zip: pair corresponding elements
    numbers.zip(strings) { num, str ->
        "$str-$num"
    }.collect { println("Zipped: $it") }
}

// StateFlow and SharedFlow
class DataRepository {
    private val _state = MutableStateFlow<DataState>(DataState.Loading)
    val state: StateFlow<DataState> = _state.asStateFlow()

    private val _events = MutableSharedFlow<DataEvent>()
    val events: SharedFlow<DataEvent> = _events.asSharedFlow()

    suspend fun loadData() {
        _state.value = DataState.Loading
        delay(500)

        try {
            val data = fetchData()
            _state.value = DataState.Success(data)
            _events.emit(DataEvent.DataLoaded(data))
        } catch (e: Exception) {
            _state.value = DataState.Error(e.message ?: "Unknown error")
            _events.emit(DataEvent.ErrorOccurred(e))
        }
    }

    private suspend fun fetchData(): String {
        delay(100)
        return "Sample data"
    }
}

sealed class DataState {
    object Loading : DataState()
    data class Success(val data: String) : DataState()
    data class Error(val message: String) : DataState()
}

sealed class DataEvent {
    data class DataLoaded(val data: String) : DataEvent()
    data class ErrorOccurred(val error: Exception) : DataEvent()
}

// Usage
runBlocking {
    val repo = DataRepository()

    launch {
        repo.state.collect { state ->
            println("State: $state")
        }
    }

    launch {
        repo.events.collect { event ->
            println("Event: $event")
        }
    }

    repo.loadData()
    delay(1000)
}
```

**When to use**: Asynchronous data streams, reactive state management, combining multiple data sources.

**Learn More**: See [Work with Coroutines](./work-with-coroutines.md#flows).

---

## Functional Programming Recipes

### Recipe: Function Composition and Currying

**Problem**: You need to compose functions and create reusable function builders.

**Solution**:

```kotlin
// Function composition
infix fun <A, B, C> ((B) -> C).compose(other: (A) -> B): (A) -> C = { a ->
    this(other(a))
}

infix fun <A, B, C> ((A) -> B).andThen(other: (B) -> C): (A) -> C = { a ->
    other(this(a))
}

// Currying
fun <A, B, C> curry(fn: (A, B) -> C): (A) -> (B) -> C = { a ->
    { b -> fn(a, b) }
}

// Partial application
fun <A, B, C> partial(fn: (A, B) -> C, a: A): (B) -> C = { b ->
    fn(a, b)
}

// Examples
fun double(x: Int): Int = x * 2
fun increment(x: Int): Int = x + 1
fun square(x: Int): Int = x * x

val doubleAndIncrement = ::increment compose ::double
val incrementAndSquare = ::increment andThen ::square

println(doubleAndIncrement(5))  // 11 = 5 * 2 + 1
println(incrementAndSquare(5))  // 36 = (5 + 1)^2

// Currying example
fun add(a: Int, b: Int): Int = a + b

val curriedAdd = curry(::add)
val add5 = curriedAdd(5)

println(add5(3))  // 8
println(add5(10)) // 15

// Pipeline operator
infix fun <T, R> T.pipe(fn: (T) -> R): R = fn(this)

val result = 5
    .pipe(::double)
    .pipe(::increment)
    .pipe(::square)

println(result)  // 121 = ((5 * 2) + 1)^2
```

**When to use**: Building reusable function chains, data transformation pipelines, functional composition patterns.

**Learn More**: See [Use Functional Programming](./use-functional-programming.md).

---

### Recipe: Monads and Functional Error Handling

**Problem**: You need functional error handling without exceptions.

**Solution**:

```kotlin
// Either monad
sealed class Either<out L, out R> {
    data class Left<out L>(val value: L) : Either<L, Nothing>()
    data class Right<out R>(val value: R) : Either<Nothing, R>()

    fun <T> map(fn: (R) -> T): Either<L, T> = when (this) {
        is Left -> this
        is Right -> Right(fn(value))
    }

    fun <T> flatMap(fn: (R) -> Either<L, T>): Either<L, T> = when (this) {
        is Left -> this
        is Right -> fn(value)
    }

    fun getOrElse(default: R): R = when (this) {
        is Left -> default
        is Right -> value
    }
}

// Usage
fun divide(a: Int, b: Int): Either<String, Int> =
    if (b == 0) Either.Left("Division by zero")
    else Either.Right(a / b)

val result1 = divide(10, 2).map { it * 2 }
println(result1)  // Right(10)

val result2 = divide(10, 0).map { it * 2 }
println(result2)  // Left("Division by zero")

// Chaining operations
fun parseNumber(s: String): Either<String, Int> =
    s.toIntOrNull()?.let { Either.Right(it) }
        ?: Either.Left("Not a valid number")

fun validatePositive(n: Int): Either<String, Int> =
    if (n > 0) Either.Right(n)
    else Either.Left("Number must be positive")

val chainResult = parseNumber("42")
    .flatMap { validatePositive(it) }
    .map { it * 2 }

println(chainResult)  // Right(84)
```

**When to use**: Functional error handling, composable operations, avoiding exceptions.

**Learn More**: See [Handle Errors Gracefully](./handle-errors-gracefully.md).

---

## DSL Creation Recipes

### Recipe: Type-Safe HTML DSL

**Problem**: You need to build HTML dynamically with compile-time safety.

**Solution**:

```kotlin
// HTML DSL Implementation
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

    override fun toString(): String {
        val attrs = attributes.entries.joinToString(" ") { "${it.key}=\"${it.value}\"" }
        val attrsStr = if (attrs.isNotEmpty()) " $attrs" else ""
        return if (children.isEmpty()) {
            "<$name$attrsStr/>"
        } else {
            val content = children.joinToString("")
            "<$name$attrsStr>$content</$name>"
        }
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
            override fun toString() = this@unaryPlus
        })
    }
}

class Body : Tag("body") {
    fun h1(init: H1.() -> Unit) = initTag(H1(), init)
    fun p(init: P.() -> Unit) = initTag(P(), init)
    fun div(cssClass: String? = null, init: Div.() -> Unit) = initTag(Div(), init).apply {
        cssClass?.let { attributes["class"] = it }
    }
}

class H1 : Tag("h1") {
    operator fun String.unaryPlus() {
        children.add(object : Tag("") {
            override fun toString() = this@unaryPlus
        })
    }
}

class P : Tag("p") {
    operator fun String.unaryPlus() {
        children.add(object : Tag("") {
            override fun toString() = this@unaryPlus
        })
    }
}

class Div : Tag("div") {
    fun p(init: P.() -> Unit) = initTag(P(), init)
    fun span(init: Span.() -> Unit) = initTag(Span(), init)
}

class Span : Tag("span") {
    operator fun String.unaryPlus() {
        children.add(object : Tag("") {
            override fun toString() = this@unaryPlus
        })
    }
}

// DSL builder function
fun html(init: HTML.() -> Unit): HTML {
    val html = HTML()
    html.init()
    return html
}

// Usage
val page = html {
    head {
        title {
            +"My Page"
        }
    }
    body {
        h1 {
            +"Welcome"
        }
        div(cssClass = "content") {
            p {
                +"This is a paragraph."
            }
            span {
                +"Some text in a span."
            }
        }
    }
}

println(page)
```

**When to use**: Building type-safe DSLs, HTML generation, configuration builders, API design.

**Learn More**: See [Create Domain-Specific Languages](./create-dsls.md).

---

### Recipe: Configuration DSL

**Problem**: You need a type-safe configuration builder.

**Solution**:

```kotlin
// Database Configuration DSL
@DslMarker
annotation class ConfigDsl

@ConfigDsl
class DatabaseConfig {
    var host: String = "localhost"
    var port: Int = 5432
    var database: String = "mydb"
    var username: String = ""
    var password: String = ""
    var maxConnections: Int = 10

    fun validate() {
        require(host.isNotBlank()) { "Host cannot be blank" }
        require(port in 1..65535) { "Port must be between 1 and 65535" }
        require(database.isNotBlank()) { "Database name cannot be blank" }
        require(username.isNotBlank()) { "Username cannot be blank" }
    }
}

@ConfigDsl
class PoolConfig {
    var minIdle: Int = 5
    var maxIdle: Int = 10
    var maxLifetime: Long = 1800000 // 30 minutes
    var connectionTimeout: Long = 30000 // 30 seconds
}

@ConfigDsl
class AppConfig {
    private var _database: DatabaseConfig? = null
    private var _pool: PoolConfig? = null

    val database: DatabaseConfig get() = _database!!
    val pool: PoolConfig get() = _pool!!

    fun database(init: DatabaseConfig.() -> Unit) {
        val config = DatabaseConfig()
        config.init()
        config.validate()
        _database = config
    }

    fun pool(init: PoolConfig.() -> Unit) {
        val config = PoolConfig()
        config.init()
        _pool = config
    }
}

// Builder function
fun appConfig(init: AppConfig.() -> Unit): AppConfig {
    val config = AppConfig()
    config.init()
    return config
}

// Usage
val config = appConfig {
    database {
        host = "db.example.com"
        port = 5432
        database = "production"
        username = "admin"
        password = "secret"
        maxConnections = 20
    }

    pool {
        minIdle = 10
        maxIdle = 20
        maxLifetime = 3600000
        connectionTimeout = 60000
    }
}

println("Database: ${config.database.host}:${config.database.port}/${config.database.database}")
println("Pool: min=${config.pool.minIdle}, max=${config.pool.maxIdle}")
```

**When to use**: Application configuration, build scripts, testing fixtures, fluent APIs.

**Learn More**: See [Create Domain-Specific Languages](./create-dsls.md#configuration-dsl).

---

## Multiplatform Development Recipes

### Recipe: Expect/Actual Platform-Specific APIs

**Problem**: You need platform-specific implementations in Kotlin Multiplatform.

**Solution**:

```kotlin
// commonMain/Platform.kt
expect class Platform() {
    val name: String
    val version: String
}

expect fun currentTimeMillis(): Long

expect fun randomUUID(): String

expect suspend fun httpGet(url: String): String

// androidMain/Platform.kt
actual class Platform actual constructor() {
    actual val name: String = "Android"
    actual val version: String = android.os.Build.VERSION.RELEASE
}

actual fun currentTimeMillis(): Long = System.currentTimeMillis()

actual fun randomUUID(): String = java.util.UUID.randomUUID().toString()

actual suspend fun httpGet(url: String): String {
    return withContext(Dispatchers.IO) {
        java.net.URL(url).readText()
    }
}

// iosMain/Platform.kt
import platform.Foundation.*
import platform.UIKit.UIDevice

actual class Platform actual constructor() {
    actual val name: String = UIDevice.currentDevice.systemName()
    actual val version: String = UIDevice.currentDevice.systemVersion
}

actual fun currentTimeMillis(): Long {
    return (NSDate().timeIntervalSince1970 * 1000).toLong()
}

actual fun randomUUID(): String {
    return NSUUID().UUIDString
}

actual suspend fun httpGet(url: String): String = withContext(Dispatchers.IO) {
    val nsUrl = NSURL.URLWithString(url)!!
    val data = NSData.dataWithContentsOfURL(nsUrl)!!
    NSString.create(data, NSUTF8StringEncoding) as String
}

// jsMain/Platform.kt
actual class Platform actual constructor() {
    actual val name: String = "JavaScript"
    actual val version: String = js("process.version") as String
}

actual fun currentTimeMillis(): Long = js("Date.now()") as Long

actual fun randomUUID(): String {
    return js("crypto.randomUUID()") as String
}

actual suspend fun httpGet(url: String): String = suspendCoroutine { cont ->
    js("""
        fetch(url)
            .then(response => response.text())
            .then(text => cont.resume(text))
            .catch(error => cont.resumeWithException(error))
    """)
}

// commonMain usage
class PlatformService {
    private val platform = Platform()

    fun getPlatformInfo(): String {
        return "${platform.name} ${platform.version}"
    }

    suspend fun fetchData(url: String): String {
        val start = currentTimeMillis()
        val data = httpGet(url)
        val elapsed = currentTimeMillis() - start
        println("Fetched in ${elapsed}ms")
        return data
    }

    fun generateId(): String {
        return randomUUID()
    }
}
```

**When to use**: Kotlin Multiplatform projects, platform-specific functionality, shared business logic.

**Learn More**: See [Build Multiplatform Apps](./multiplatform-development.md).

---

### Recipe: Shared ViewModel Pattern

**Problem**: You need to share view models across Android/iOS in Kotlin Multiplatform.

**Solution**:

```kotlin
// commonMain
import kotlinx.coroutines.*
import kotlinx.coroutines.flow.*

sealed class UiState<out T> {
    object Loading : UiState<Nothing>()
    data class Success<T>(val data: T) : UiState<T>()
    data class Error(val message: String) : UiState<Nothing>()
}

abstract class BaseViewModel {
    protected val viewModelScope = CoroutineScope(Dispatchers.Main + SupervisorJob())

    open fun onCleared() {
        viewModelScope.cancel()
    }
}

class UserListViewModel(
    private val repository: UserRepository
) : BaseViewModel() {
    private val _uiState = MutableStateFlow<UiState<List<User>>>(UiState.Loading)
    val uiState: StateFlow<UiState<List<User>>> = _uiState.asStateFlow()

    init {
        loadUsers()
    }

    fun loadUsers() {
        viewModelScope.launch {
            _uiState.value = UiState.Loading

            try {
                val users = repository.getUsers()
                _uiState.value = UiState.Success(users)
            } catch (e: Exception) {
                _uiState.value = UiState.Error(e.message ?: "Unknown error")
            }
        }
    }

    fun refresh() {
        loadUsers()
    }
}

data class User(
    val id: String,
    val name: String,
    val email: String
)

interface UserRepository {
    suspend fun getUsers(): List<User>
}

// androidMain - Android Integration
class AndroidUserListViewModel(repository: UserRepository) : ViewModel() {
    private val viewModel = UserListViewModel(repository)

    val uiState = viewModel.uiState

    fun refresh() = viewModel.refresh()

    override fun onCleared() {
        super.onCleared()
        viewModel.onCleared()
    }
}

// Android Compose UI
@Composable
fun UserListScreen(viewModel: AndroidUserListViewModel) {
    val uiState by viewModel.uiState.collectAsState()

    when (val state = uiState) {
        is UiState.Loading -> LoadingView()
        is UiState.Success -> UserList(state.data)
        is UiState.Error -> ErrorView(state.message) {
            viewModel.refresh()
        }
    }
}

// iosMain - iOS Integration
class IOSUserListViewModel(repository: UserRepository) {
    private val viewModel = UserListViewModel(repository)

    fun observe(onChange: (UiState<List<User>>) -> Unit): Closeable {
        val job = viewModel.viewModelScope.launch {
            viewModel.uiState.collect { onChange(it) }
        }

        return object : Closeable {
            override fun close() {
                job.cancel()
            }
        }
    }

    fun refresh() = viewModel.refresh()

    fun onCleared() = viewModel.onCleared()
}

// iOS SwiftUI Integration
class UserListView: ObservableObject {
    @Published var uiState: UiState<[User]> = .loading

    private let viewModel: IOSUserListViewModel
    private var observation: Closeable?

    init(viewModel: IOSUserListViewModel) {
        self.viewModel = viewModel
        observation = viewModel.observe { [weak self] state in
            DispatchQueue.main.async {
                self?.uiState = state
            }
        }
    }

    func refresh() {
        viewModel.refresh()
    }

    deinit {
        observation?.close()
        viewModel.onCleared()
    }
}
```

**When to use**: Kotlin Multiplatform Mobile (KMM), shared business logic, cross-platform state management.

**Learn More**: See [Build Multiplatform Apps](./multiplatform-development.md#shared-viewmodels).

---

## Advanced Testing Recipes

### Recipe: Integration Testing with Testcontainers

**Problem**: You need to test against real databases and services in integration tests.

**Solution**:

```kotlin
import org.junit.jupiter.api.*
import org.testcontainers.containers.PostgreSQLContainer
import org.testcontainers.junit.jupiter.Container
import org.testcontainers.junit.jupiter.Testcontainers

@Testcontainers
class UserRepositoryIntegrationTest {
    companion object {
        @Container
        val postgres = PostgreSQLContainer<Nothing>("postgres:15").apply {
            withDatabaseName("testdb")
            withUsername("test")
            withPassword("test")
        }
    }

    private lateinit var repository: UserRepository
    private lateinit var database: Database

    @BeforeEach
    fun setup() {
        database = Database.connect(
            url = postgres.jdbcUrl,
            driver = "org.postgresql.Driver",
            user = postgres.username,
            password = postgres.password
        )

        transaction {
            SchemaUtils.create(Users)
        }

        repository = UserRepositoryImpl(database)
    }

    @AfterEach
    fun teardown() {
        transaction {
            SchemaUtils.drop(Users)
        }
    }

    @Test
    fun `should save and retrieve user`() = runBlocking {
        // Given
        val user = User("1", "John Doe", "john@example.com")

        // When
        repository.save(user)
        val retrieved = repository.findById("1")

        // Then
        assertEquals(user, retrieved)
    }

    @Test
    fun `should find users by email`() = runBlocking {
        // Given
        val users = listOf(
            User("1", "John", "john@example.com"),
            User("2", "Jane", "jane@example.com"),
            User("3", "John Jr", "john.jr@example.com")
        )

        users.forEach { repository.save(it) }

        // When
        val johnUsers = repository.findByEmailContaining("john")

        // Then
        assertEquals(2, johnUsers.size)
        assertTrue(johnUsers.all { it.email.contains("john", ignoreCase = true) })
    }

    @Test
    fun `should handle concurrent updates correctly`() = runBlocking {
        // Given
        val user = User("1", "John", "john@example.com")
        repository.save(user)

        // When - Concurrent updates
        val jobs = (1..10).map { index ->
            async(Dispatchers.IO) {
                repository.update(user.copy(name = "John $index"))
            }
        }

        jobs.awaitAll()

        // Then - Last update wins
        val updated = repository.findById("1")
        assertNotNull(updated)
        assertTrue(updated.name.startsWith("John "))
    }
}

// Redis integration test
@Testcontainers
class CacheServiceIntegrationTest {
    companion object {
        @Container
        val redis = GenericContainer<Nothing>("redis:7-alpine").apply {
            withExposedPorts(6379)
        }
    }

    private lateinit var cacheService: CacheService

    @BeforeEach
    fun setup() {
        val jedis = Jedis(redis.host, redis.getMappedPort(6379))
        cacheService = RedisCacheService(jedis)
    }

    @Test
    fun `should cache and retrieve values`() {
        // When
        cacheService.set("key1", "value1")
        val retrieved = cacheService.get("key1")

        // Then
        assertEquals("value1", retrieved)
    }

    @Test
    fun `should expire keys after TTL`() = runBlocking {
        // Given
        cacheService.set("key1", "value1", ttlSeconds = 1)

        // When - Wait for expiration
        delay(1500)

        // Then
        assertNull(cacheService.get("key1"))
    }
}
```

**When to use**: Integration testing, testing with real databases/services, container-based testing, E2E tests.

**Learn More**: See [Write Effective Tests](./write-effective-tests.md#integration-testing).

---

## Summary

This cookbook provides 58 practical recipes organized into 16 categories covering the most common Kotlin programming tasks. Each recipe is designed to be self-contained and ready to adapt to your specific needs.

**Key Takeaways**:

- **Data Structures**: Use immutable collections for safety, sequences for performance, sealed classes for type safety
- **Coroutines**: Master launch/async patterns, timeout/retry logic, Flow for reactive streams
- **Error Handling**: Leverage Result types, sealed classes, nullable types, and functional error handling
- **Design Patterns**: Apply Kotlin's built-in support for singleton, factory, builder, observer, delegation, and strategy patterns
- **Web Development**: Build REST APIs with Ktor, handle JSON with kotlinx.serialization
- **Database**: Use Exposed for type-safe queries, JDBC extensions for cleaner code
- **Testing**: Write comprehensive tests with JUnit 5, MockK, and property-based testing
- **Performance**: Optimize with inline functions, avoid allocations, use sequences wisely

**Next Steps**:

- Explore the [Beginner Tutorial](/en/learn/swe/prog-lang/kotlin/tutorials/beginner/) for fundamentals
- Dive into [Intermediate Tutorial](/en/learn/swe/prog-lang/kotlin/tutorials/intermediate/) for advanced patterns
- Master [Advanced Tutorial](/en/learn/swe/prog-lang/kotlin/tutorials/advanced/) for expert-level techniques
- Check [How-To Guides](/en/learn/swe/prog-lang/kotlin/how-to/) for specific tasks
