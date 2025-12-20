---
title: "How to Work with Scope Functions"
date: 2025-12-18T00:00:00+07:00
draft: false
weight: 1000100
description: "Master let, run, with, apply, and also for cleaner Kotlin code"
tags: ["kotlin", "scope-functions", "idiomatic-kotlin", "kotlin-features"]
categories: ["learn"]
---

## Problem

Object configuration and null-safe operations often lead to verbose code with repeated variable names and temporary variables. Kotlin's scope functions (let, run, with, apply, also) provide concise patterns for operating on objects within a specific scope.

This guide shows how to choose and use the right scope function for each situation.

## Scope Function Overview

### The Five Scope Functions

Each scope function has specific use cases based on context object access and return value.

```kotlin
// Context = this, Returns = context
val person = Person().apply {
  name = "Alice"
  age = 30
}

// Context = this, Returns = lambda result
val greeting = person.run {
  "Hello, $name (age $age)"
}

// Context = it, Returns = lambda result
val name = person.let {
  it.name.uppercase()
}

// Context = it, Returns = context
val samePerson = person.also {
  println("Created: ${it.name}")
}

// Non-extension, Context = this, Returns = lambda result
val result = with(person) {
  "$name is $age years old"
}
```

**Quick Reference**:

| Function | Context | Returns        | Common Use                         |
| -------- | ------- | -------------- | ---------------------------------- |
| `let`    | `it`    | Lambda result  | Null-safe calls, transformations   |
| `run`    | `this`  | Lambda result  | Object configuration + computation |
| `with`   | `this`  | Lambda result  | Grouping calls on object           |
| `apply`  | `this`  | Context object | Object configuration               |
| `also`   | `it`    | Context object | Additional operations (logging)    |

## let Function

### Null-Safe Operations

Use `let` with safe call for null-safe execution.

```kotlin
// ✅ Execute block only if not null
val name: String? = getUsername()
name?.let {
  println("Hello, $it")
  database.updateLastSeen(it)
}

// ✅ Transform nullable value
val length: Int? = text?.let { it.trim().length }

// ✅ Avoid nested null checks
user?.profile?.address?.let { address ->
  println("${address.city}, ${address.country}")
}
```

**Pattern**: `nullable?.let { ... }` - safe execution with clear scope.

### Introducing Local Variables

Use `let` to limit variable scope.

```kotlin
// ✅ Transform and use
val result = fetchData().let { data ->
  process(data)
  validate(data)
  data.transform()
}

// ✅ Avoid polluting outer scope
fun processConfig() {
  loadConfig().let { config ->
    // config only exists in this scope
    applySettings(config)
    saveToCache(config)
  }
  // config not accessible here
}
```

**Use case**: Temporary transformations without polluting namespace.

### Chaining Operations

Chain `let` for functional pipelines.

```kotlin
// ✅ Functional transformation pipeline
val result = getInput()
  ?.let { it.trim() }
  ?.let { it.uppercase() }
  ?.let { it.split(" ") }
  ?.let { it.filter { word -> word.length > 3 } }

// ✅ With explicit parameters
val formatted = data
  .let { rawData -> validate(rawData) }
  .let { validData -> transform(validData) }
  .let { transformed -> format(transformed) }
```

## run Function

### Object Configuration with Result

Use `run` when you need to configure and return a computed value.

```kotlin
// ✅ Configure object and compute result
val message = Person().run {
  name = "Alice"
  age = 30
  email = "alice@example.com"
  "Created user: $name"  // Return value
}
println(message)  // "Created user: Alice"

// ✅ Complex initialization
val config = Configuration().run {
  host = "localhost"
  port = 8080
  timeout = 30
  validate()  // Returns Boolean
}
```

**Pattern**: Configure with `this`, return transformation result.

### Null-Safe run

Use `run` with safe call for null-safe operations.

```kotlin
// ✅ Null-safe run
val result: String? = user?.run {
  "$name (${email})"
}

// ✅ Execute multiple operations
connection?.run {
  open()
  execute("SELECT * FROM users")
  close()
}
```

### run for Expression Blocks

Use `run` to create scoped expressions.

```kotlin
// ✅ Multi-line expression
val greeting = run {
  val hour = LocalDateTime.now().hour
  when (hour) {
    in 0..11 -> "Good morning"
    in 12..17 -> "Good afternoon"
    else -> "Good evening"
  }
}

// ✅ Complex condition
val isValid = run {
  val emailValid = email.contains("@")
  val nameValid = name.length > 2
  emailValid && nameValid
}
```

## with Function

### Operating on Non-Null Objects

Use `with` for multiple operations on an object.

```kotlin
// ✅ Group operations on object
val person = Person("Alice", 30)
val description = with(person) {
  println("Name: $name")
  println("Age: $age")
  "Person: $name"  // Return value
}

// ✅ Configure with builder pattern
val request = with(HttpRequest()) {
  setUrl("https://api.example.com")
  setMethod("POST")
  addHeader("Content-Type", "application/json")
  build()
}
```

**Pattern**: Non-extension function - pass object as parameter.

### with for String Building

```kotlin
// ✅ Build complex strings
val html = with(StringBuilder()) {
  append("<html>")
  append("<body>")
  append("<h1>Hello</h1>")
  append("</body>")
  append("</html>")
  toString()
}
```

**Use case**: When you have an object and need multiple operations returning a result.

## apply Function

### Object Configuration

Use `apply` for configuring objects (returns the object itself).

```kotlin
// ✅ Configure object (returns object)
val person = Person().apply {
  name = "Alice"
  age = 30
  email = "alice@example.com"
}
// person is the configured Person instance

// ✅ Builder pattern
val dialog = AlertDialog(context).apply {
  setTitle("Confirm")
  setMessage("Are you sure?")
  setPositiveButton("Yes") { _, _ -> }
  setNegativeButton("No") { _, _ -> }
}

// ✅ List configuration
val list = mutableListOf<String>().apply {
  add("Item 1")
  add("Item 2")
  add("Item 3")
}
```

**Pattern**: Configure with `this`, return configured object.

### apply for Side Effects

Use `apply` when you want the object back after operations.

```kotlin
// ✅ Configure and pass forward
fun createUser(name: String): User {
  return User().apply {
    this.name = name
    this.createdAt = System.currentTimeMillis()
    this.status = "active"
  }
}

// ✅ Chaining with apply
val result = fetchData()
  .apply { println("Fetched: $this") }
  .apply { validate(this) }
  .apply { cache(this) }
```

**Use case**: Configuration with method chaining.

## also Function

### Additional Operations

Use `also` for side effects while keeping the original object.

```kotlin
// ✅ Logging and debugging
val user = createUser("Alice")
  .also { println("Created user: ${it.name}") }
  .also { logUserCreation(it) }
  .also { sendWelcomeEmail(it) }

// ✅ Validation
val data = fetchData()
  .also { require(it.isNotEmpty()) { "Data cannot be empty" } }
  .also { println("Fetched ${it.size} items") }

// ✅ Caching
val config = loadConfig()
  .also { cache.put("config", it) }
  .also { logger.info("Config loaded: $it") }
```

**Pattern**: Side effects with `it`, return original object.

### also vs apply

```kotlin
// ✅ apply - configure object members
val person1 = Person().apply {
  name = "Alice"  // this.name
  age = 30        // this.age
}

// ✅ also - external operations on object
val person2 = Person().also {
  database.save(it)
  logger.info("Saved: ${it}")
}

// ✅ Combining both
val person3 = Person().apply {
  name = "Bob"
  age = 25
}.also {
  database.save(it)
  println("Created and saved: ${it.name}")
}
```

## Choosing the Right Scope Function

### Decision Tree

```kotlin
// Need to return the object itself?
// ├─ Yes
// │  ├─ Need `this` context? → apply
// │  └─ Need `it` context? → also
// └─ No (return lambda result)
//    ├─ Extension function?
//    │  ├─ Need `this` context? → run
//    │  └─ Need `it` context? → let
//    └─ Not extension? → with
```

### Practical Examples

```kotlin
// ✅ let - null safety, transformation
val length = text?.let { it.trim().length }

// ✅ run - configure and compute
val valid = form.run {
  val emailOk = email.isValidEmail()
  val nameOk = name.length > 2
  emailOk && nameOk
}

// ✅ with - operate on non-null object
val description = with(person) {
  "$name is $age years old"
}

// ✅ apply - configure object
val user = User().apply {
  name = "Alice"
  email = "alice@example.com"
}

// ✅ also - logging, side effects
val data = fetchData()
  .also { println("Fetched ${it.size} items") }
```

## Common Pitfalls

### Overusing Scope Functions

```kotlin
// ❌ Unnecessary scope function
val result = value.let { it }

// ✅ Direct usage
val result = value

// ❌ Too many nested scope functions
val output = data.let {
  it.apply {
    field1 = 1
  }.run {
    process(this)
  }
}

// ✅ Simpler alternative
val output = data.apply {
  field1 = 1
}
process(output)
```

**Why problematic**: Reduces readability without adding value.

### Mixing this and it Contexts

```kotlin
// ❌ Confusing context mixing
Person().apply {
  name = "Alice"  // this.name
}.also {
  it.age = 30     // it.age (different context!)
}

// ✅ Consistent context
Person().apply {
  name = "Alice"
  age = 30
}

// ✅ Or explicit parameters
Person().apply {
  name = "Alice"
}.also { person ->
  person.age = 30
  println(person.name)
}
```

**Why problematic**: Context switching reduces clarity.

### Ignoring Return Values

```kotlin
// ❌ Expecting apply to return result
val description = person.apply {
  "$name is $age"  // Lost! apply returns person, not string
}

// ✅ Use run for result
val description = person.run {
  "$name is $age"  // Returns string
}
```

**Why problematic**: Wrong scope function for the task.

## Variations

### Combining Scope Functions

Chain scope functions for complex operations.

```kotlin
// ✅ Practical combination
val result = fetchUser(userId)
  ?.let { user ->                      // Null safety
    user.profile?.address
  }
  ?.run {                              // Transform
    "$city, $country"
  }
  .also { address ->                   // Log
    println("User address: $address")
  }
  ?.let { it.uppercase() }             // Final transformation
  ?: "Unknown location"                // Default
```

### Scope Functions with DSLs

```kotlin
// ✅ DSL with apply
fun buildHtml() = StringBuilder().apply {
  append("<html>")
  apply {
    append("<head>")
    append("<title>My Page</title>")
    append("</head>")
  }
  apply {
    append("<body>")
    append("<h1>Hello</h1>")
    append("</body>")
  }
  append("</html>")
}.toString()
```

## Related Patterns

**Learn more**:

- [Nullable Types](/en/learn/swe/prog-lang/kotlin/how-to/working-with-nullable-types) - let with null safety
- [Extension Functions](/en/learn/swe/prog-lang/kotlin/how-to/extension-functions) - Custom scope-like functions
- [Beginner Tutorial - Scope Functions](/en/learn/swe/prog-lang/kotlin/tutorials/beginner#scope-functions) - Scope function fundamentals
- [Best Practices - Scope Functions](/en/learn/swe/prog-lang/kotlin/explanation/best-practices#scope-functions) - Idiomatic usage guidelines

**Cookbook recipes**:

- [Builder Pattern with apply](/en/learn/swe/prog-lang/kotlin/how-to/cookbook#builder-apply) - Configuration patterns
- [Type-Safe Builders](/en/learn/swe/prog-lang/kotlin/how-to/cookbook#type-safe-builders) - DSL with scope functions
