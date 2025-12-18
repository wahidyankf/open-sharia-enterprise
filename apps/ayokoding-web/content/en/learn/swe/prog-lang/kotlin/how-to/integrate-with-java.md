---
title: "How to Handle Java Interoperability"
date: 2025-12-18T14:50:04+07:00
draft: false
weight: 618
description: "Master calling Java from Kotlin and Kotlin from Java with interoperability best practices"
tags: ["kotlin", "java", "interoperability", "jvm"]
categories: ["learn"]
---

## Problem

Most Kotlin projects interact with existing Java code - whether using Java libraries, maintaining legacy code, or providing APIs for Java clients. Kotlin's seamless Java interoperability enables gradual migration and polyglot codebases, but requires understanding platform types, nullability annotations, and JVM-specific features.

This guide shows how to achieve smooth Java-Kotlin interoperability.

## Calling Java from Kotlin

### Basic Java Calls

Call Java code naturally from Kotlin.

**Java class**:

```java
// User.java
public class User {
    private String name;
    private int age;

    public User(String name, int age) {
        this.name = name;
        this.age = age;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public int getAge() {
        return age;
    }
}
```

**Kotlin usage**:

```kotlin
// ✅ Call Java from Kotlin
val user = User("Alice", 30)
println(user.name)  // Property syntax for getters
user.name = "Bob"   // Property syntax for setters
println(user.age)
```

**How it works**: Kotlin treats Java getters/setters as properties.

### Platform Types

Java types without null annotations become platform types.

```kotlin
// Java method without annotations
// public String getName() { return name; }

// ✅ Kotlin sees platform type (String!)
val name: String! = javaUser.name  // Platform type

// ✅ Can treat as nullable
val safeName: String? = javaUser.name
val length = safeName?.length

// ✅ Can treat as non-null (risky!)
val unsafeName: String = javaUser.name
// Runtime NPE if getName() returns null

// ✅ Best practice: defensive approach
val defensiveName = javaUser.name ?: "Unknown"
```

**Warning**: Platform types (`T!`) bypass null safety - handle carefully.

### Java Nullability Annotations

Kotlin respects standard nullability annotations.

**Java with annotations**:

```java
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class UserService {
    @NotNull
    public User getUser(@NotNull String id) {
        // ...
    }

    @Nullable
    public User findUser(@NotNull String id) {
        // ...
    }
}
```

**Kotlin sees correct types**:

```kotlin
// ✅ Annotations translated to Kotlin types
val service = UserService()

val user: User = service.getUser("123")  // ✅ Non-null
val found: User? = service.findUser("123")  // ✅ Nullable

// ❌ Compilation error if types don't match
// val wrong: User? = service.getUser("123")  // Error
```

**Supported annotations**:

- JetBrains: `@NotNull`, `@Nullable`
- JSR-305: `@Nonnull`, `@Nullable`, `@CheckForNull`
- Android: `@NonNull`, `@Nullable`
- Eclipse: `@NonNull`, `@Nullable`

### Calling Java Static Methods

Static methods become package-level functions or companion object members.

**Java statics**:

```java
public class StringUtils {
    public static boolean isEmpty(String str) {
        return str == null || str.isEmpty();
    }

    public static String capitalize(String str) {
        return str.substring(0, 1).toUpperCase() + str.substring(1);
    }
}
```

**Kotlin usage**:

```kotlin
// ✅ Call Java static methods
import StringUtils

val empty = StringUtils.isEmpty("")  // true
val capitalized = StringUtils.capitalize("hello")  // "Hello"
```

### SAM Conversions

Convert Kotlin lambdas to Java Single Abstract Method interfaces.

**Java SAM interface**:

```java
public interface Callback {
    void onComplete(String result);
}

public class AsyncTask {
    public void execute(Callback callback) {
        // ... async work
        callback.onComplete("Done");
    }
}
```

**Kotlin usage**:

```kotlin
// ✅ SAM conversion - lambda as interface
val task = AsyncTask()
task.execute { result ->
  println("Result: $result")
}

// ✅ Equivalent verbose version
task.execute(object : Callback {
  override fun onComplete(result: String) {
    println("Result: $result")
  }
})
```

**Requirement**: Java interface must have exactly one abstract method.

## Calling Kotlin from Java

### Properties as Getters/Setters

Kotlin properties generate Java getters and setters.

**Kotlin class**:

```kotlin
class User(
  val id: String,         // Read-only property
  var name: String,       // Read-write property
  var age: Int
)
```

**Java usage**:

```java
// ✅ Kotlin properties → Java methods
User user = new User("1", "Alice", 30);

String id = user.getId();          // val → getter only
String name = user.getName();      // var → getter
user.setName("Bob");               // var → setter
int age = user.getAge();           // var → getter
user.setAge(31);                   // var → setter
```

### @JvmField Annotation

Expose Kotlin property as public Java field.

**Kotlin with @JvmField**:

```kotlin
class Config {
  @JvmField
  var timeout: Int = 30

  var retries: Int = 3  // Normal property
}
```

**Java usage**:

```java
Config config = new Config();

// ✅ @JvmField - direct field access
config.timeout = 60;
int timeout = config.timeout;

// ✅ Normal property - getter/setter
config.setRetries(5);
int retries = config.getRetries();
```

**Use case**: Matching Java field semantics, avoiding method overhead.

### @JvmStatic for Static Members

Make companion object members static in Java.

**Kotlin companion object**:

```kotlin
class Factory {
  companion object {
    @JvmStatic
    fun create(): Factory {
      return Factory()
    }

    fun createPrivate(): Factory {
      return Factory()
    }
  }
}
```

**Java usage**:

```java
// ✅ @JvmStatic - true static method
Factory factory1 = Factory.create();

// ✅ Without @JvmStatic - companion access
Factory factory2 = Factory.Companion.createPrivate();
```

### @JvmOverloads for Default Parameters

Generate Java overloads for default parameters.

**Kotlin with defaults**:

```kotlin
class HttpClient {
  @JvmOverloads
  fun request(
    url: String,
    method: String = "GET",
    timeout: Int = 30,
    retries: Int = 3
  ): Response {
    // ...
  }
}
```

**Java sees multiple overloads**:

```java
HttpClient client = new HttpClient();

// ✅ All parameter combinations available
client.request("https://api.example.com");
client.request("https://api.example.com", "POST");
client.request("https://api.example.com", "POST", 60);
client.request("https://api.example.com", "POST", 60, 5);
```

**Generated methods**: Kotlin generates overloads for each combination.

### @JvmName for Method Names

Customize JVM method names.

**Kotlin with @JvmName**:

```kotlin
class UserRepository {
  @JvmName("findActiveUsers")
  fun findActive(): List<User> {
    // ...
  }

  @JvmName("getAllUsers")
  fun getAll(): List<User> {
    // ...
  }
}
```

**Java usage**:

```java
UserRepository repo = new UserRepository();

List<User> active = repo.findActiveUsers();
List<User> all = repo.getAllUsers();
```

**Use case**: Resolve JVM signature conflicts, provide Java-friendly names.

## Handling Null Safety

### Adding Nullability Annotations

Make Kotlin nullability visible to Java.

**Kotlin with annotations**:

```kotlin
import org.jetbrains.annotations.NotNull
import org.jetbrains.annotations.Nullable

class UserService {
  @NotNull
  fun getUser(@NotNull id: String): User {
    return repository.findById(id) ?: throw NotFoundException()
  }

  @Nullable
  fun findUser(@NotNull id: String): User? {
    return repository.findById(id)
  }
}
```

**Java sees annotated types**:

```java
UserService service = new UserService();

// ✅ IDE warnings if passing null
User user = service.getUser("123");  // @NotNull
User found = service.findUser("123");  // @Nullable - may be null
```

### Safe Calls from Java

Protect against nulls when calling Java from Kotlin.

```kotlin
// ✅ Defensive null handling
val userName = javaUser.getName() ?: "Unknown"

// ✅ Safe call chain
val email = javaUser.getProfile()?.getEmail()

// ✅ Let with null check
javaUser.getProfile()?.let { profile ->
  println("Email: ${profile.getEmail()}")
}

// ❌ Risky - assumes non-null
val unsafeName = javaUser.getName()  // May throw NPE
```

## Collections Interoperability

### Java Collections in Kotlin

Java collections become platform types with read-only view.

**Java method**:

```java
public List<String> getNames() {
    return Arrays.asList("Alice", "Bob", "Charlie");
}
```

**Kotlin usage**:

```kotlin
// ✅ Java List → Kotlin read-only List
val names: List<String> = javaObject.names

// ❌ Can't call add (compile error)
// names.add("Diana")

// ✅ Convert to mutable if needed
val mutableNames = names.toMutableList()
mutableNames.add("Diana")
```

### Kotlin Collections in Java

Kotlin collections translate to Java standard interfaces.

**Kotlin collections**:

```kotlin
class DataService {
  fun getUsers(): List<User> {
    return listOf(User("1", "Alice"), User("2", "Bob"))
  }

  fun getActiveUsers(): Set<User> {
    return setOf(User("1", "Alice"))
  }

  fun getUserAges(): Map<String, Int> {
    return mapOf("Alice" to 30, "Bob" to 25)
  }
}
```

**Java usage**:

```java
DataService service = new DataService();

// ✅ Kotlin List → java.util.List
List<User> users = service.getUsers();
for (User user : users) {
    System.out.println(user.getName());
}

// ✅ Kotlin Set → java.util.Set
Set<User> activeUsers = service.getActiveUsers();

// ✅ Kotlin Map → java.util.Map
Map<String, Integer> ages = service.getUserAges();
```

### Mutable vs Immutable

Kotlin distinguishes mutable and immutable, Java sees only standard interfaces.

**Kotlin**:

```kotlin
class CollectionProvider {
  fun getImmutable(): List<String> {
    return listOf("a", "b", "c")
  }

  fun getMutable(): MutableList<String> {
    return mutableListOf("a", "b", "c")
  }
}
```

**Java**:

```java
CollectionProvider provider = new CollectionProvider();

// ✅ Both return java.util.List (no compile-time distinction)
List<String> immutable = provider.getImmutable();
List<String> mutable = provider.getMutable();

// ⚠️ Java can mutate both (runtime exception for immutable)
mutable.add("d");     // ✅ Works
immutable.add("d");   // ❌ UnsupportedOperationException at runtime
```

## Extension Functions

### Calling Extensions from Java

Extensions compile to static methods.

**Kotlin extensions**:

```kotlin
// StringExtensions.kt
package com.example.extensions

fun String.isValidEmail(): Boolean {
  return contains("@")
}

fun String.truncate(maxLength: Int): String {
  return if (length <= maxLength) this else substring(0, maxLength) + "..."
}
```

**Java usage**:

```java
import static com.example.extensions.StringExtensionsKt.*;

// ✅ Call as static methods
boolean valid = isValidEmail("alice@example.com");
String truncated = truncate("Long text here", 10);
```

**Pattern**: Extensions become static methods in `[FileName]Kt` class.

### @JvmName for Extension Files

Customize generated class name for extensions.

**Kotlin with @JvmName**:

```kotlin
@file:JvmName("StringUtils")
package com.example.utils

fun String.isValidEmail(): Boolean {
  return contains("@")
}
```

**Java usage**:

```java
import static com.example.utils.StringUtils.*;

// ✅ Custom class name
boolean valid = isValidEmail("alice@example.com");
```

## Data Classes and Destructuring

### Data Classes in Java

Data classes generate standard Java methods.

**Kotlin data class**:

```kotlin
data class User(
  val id: String,
  val name: String,
  val email: String
)
```

**Java sees**:

```java
User user = new User("1", "Alice", "alice@example.com");

// ✅ Auto-generated methods
String id = user.getId();
String name = user.getName();
String email = user.getEmail();

String str = user.toString();  // User(id=1, name=Alice, email=alice@example.com)
boolean equals = user.equals(other);
int hash = user.hashCode();

// ✅ Copy method
User updated = user.copy("1", "Alice Updated", user.getEmail());

// ✅ Component methods (destructuring)
String id = user.component1();
String name = user.component2();
String email = user.component3();
```

### @JvmRecord for Java Records

Kotlin 1.5+ can generate Java records.

**Kotlin**:

```kotlin
@JvmRecord
data class Point(val x: Int, val y: Int)
```

**Compiles to Java record** (Java 16+):

```java
public record Point(int x, int y) {}
```

## Sealed Classes and When

### Sealed Classes in Java

Sealed classes compile to inheritance hierarchies.

**Kotlin sealed class**:

```kotlin
sealed class Result {
  data class Success(val data: String) : Result()
  data class Error(val message: String) : Result()
  object Loading : Result()
}
```

**Java usage**:

```java
Result result = fetchData();

// ✅ instanceof checks
if (result instanceof Result.Success) {
    Result.Success success = (Result.Success) result;
    System.out.println(success.getData());
} else if (result instanceof Result.Error) {
    Result.Error error = (Result.Error) result;
    System.err.println(error.getMessage());
} else if (result instanceof Result.Loading) {
    System.out.println("Loading...");
}
```

**Note**: Java doesn't have exhaustiveness checking like Kotlin's `when`.

## Object and Companion Object

### Object Declarations

Kotlin objects become Java singletons.

**Kotlin object**:

```kotlin
object Database {
  fun connect(): Connection {
    return DriverManager.getConnection("jdbc:...")
  }
}
```

**Java usage**:

```java
// ✅ Access via INSTANCE
Connection conn = Database.INSTANCE.connect();
```

### Companion Object

Companion object members accessed via `Companion`.

**Kotlin**:

```kotlin
class Factory {
  companion object {
    fun create(): Factory = Factory()
  }
}
```

**Java**:

```java
// ✅ Without @JvmStatic
Factory factory = Factory.Companion.create();

// ✅ With @JvmStatic (see earlier section)
// Factory factory = Factory.create();
```

## Common Pitfalls

### Platform Type Surprises

```kotlin
// ❌ Assuming non-null from Java
val name: String = javaUser.getName()  // May throw NPE
name.length  // NPE propagates

// ✅ Defensive approach
val name: String? = javaUser.getName()
val length = name?.length ?: 0
```

### Collection Mutability Assumptions

```kotlin
// ❌ Java may mutate "immutable" Kotlin collections
fun getNames(): List<String> {
  return listOf("Alice", "Bob")
}

// Java can cast and mutate
// List<String> names = (MutableList<String>) getNames();
// names.add("Charlie");  // UnsupportedOperationException

// ✅ Return truly immutable collections
fun getNames(): List<String> {
  return listOf("Alice", "Bob").toList()  // Defensive copy
}
```

### Forgetting @JvmOverloads

```kotlin
// ❌ Java can't use default parameters
fun request(url: String, method: String = "GET") { }

// Java: request("url")  // ❌ No overload

// ✅ Add @JvmOverloads
@JvmOverloads
fun request(url: String, method: String = "GET") { }

// Java: request("url")  // ✅ Works
```

### Extension Function Confusion

```kotlin
// Kotlin: user.isActive()
// Java: UserExtensionsKt.isActive(user)  // ❌ Easy to forget
```

## Variations

### @Throws for Checked Exceptions

Kotlin doesn't have checked exceptions, but Java does.

**Kotlin**:

```kotlin
@Throws(IOException::class, SQLException::class)
fun saveToDatabase(data: String) {
  // May throw IOException or SQLException
}
```

**Java**:

```java
// ✅ Java sees checked exceptions
try {
    service.saveToDatabase("data");
} catch (IOException | SQLException e) {
    e.printStackTrace();
}
```

### @JvmSuppressWildcards

Control generic variance in Java signatures.

**Kotlin**:

```kotlin
// ✅ Suppress wildcard generation
fun process(items: List<@JvmSuppressWildcards String>) {
  // Java sees: List<String> instead of List<? extends String>
}
```

### Java-Friendly Builders

Create builder patterns for Java consumers.

**Kotlin**:

```kotlin
class HttpRequest private constructor(
  val url: String,
  val method: String,
  val headers: Map<String, String>
) {
  class Builder {
    private var url: String = ""
    private var method: String = "GET"
    private val headers = mutableMapOf<String, String>()

    fun url(url: String) = apply { this.url = url }
    fun method(method: String) = apply { this.method = method }
    fun header(key: String, value: String) = apply { headers[key] = value }

    fun build() = HttpRequest(url, method, headers)
  }

  companion object {
    @JvmStatic
    fun builder() = Builder()
  }
}
```

**Java usage**:

```java
HttpRequest request = HttpRequest.builder()
    .url("https://api.example.com")
    .method("POST")
    .header("Authorization", "Bearer token")
    .build();
```

## Related Patterns

**Learn more**:

- [Beginner Tutorial - Java Interop](/en/learn/swe/prog-lang/kotlin/tutorials/beginner#java-interoperability) - Interoperability basics
- [Migrate from Java](/en/learn/swe/prog-lang/kotlin/how-to/migrate-from-java) - Migration guide
- [Platform Types](/en/learn/swe/prog-lang/kotlin/tutorials/intermediate#platform-types) - Understanding platform types
- [Annotations](/en/learn/swe/prog-lang/kotlin/tutorials/intermediate#annotations) - JVM annotations

**Cookbook recipes**:

- [Java Interop](/en/learn/swe/prog-lang/kotlin/how-to/cookbook#java-interop) - Quick reference
- [Nullability Annotations](/en/learn/swe/prog-lang/kotlin/how-to/cookbook#nullability) - Null safety patterns
- [Extension Functions](/en/learn/swe/prog-lang/kotlin/how-to/cookbook#extensions) - Extension patterns
