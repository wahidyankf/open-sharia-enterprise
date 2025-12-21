---
title: "How to Migrate Java Code to Kotlin"
date: 2025-12-18T14:50:04+07:00
draft: false
weight: 1000017
description: "Complete guide for Java developers migrating to Kotlin with syntax comparisons and migration strategies"
tags: ["kotlin", "java", "migration", "interoperability"]
categories: ["learn"]
---

## Problem

Migrating a Java codebase to Kotlin requires understanding syntax differences, null safety implications, and idiomatic Kotlin patterns. Automated conversion tools help but produce unidiomatic code. A gradual, strategic migration yields better results.

This guide provides a complete migration strategy with syntax comparisons and best practices.

## Migration Strategy

### Incremental Migration Approach

Migrate gradually, file by file or module by module.

```kotlin
// ✅ Phase 1: New code in Kotlin
// Write all new features in Kotlin
// Java and Kotlin coexist seamlessly

// ✅ Phase 2: Convert utility classes
// Start with utility/helper classes (no dependencies)
// These are easiest to migrate

// ✅ Phase 3: Convert data classes
// Migrate POJOs to Kotlin data classes
// Massive boilerplate reduction

// ✅ Phase 4: Convert business logic
// Migrate service/business logic classes
// Take advantage of Kotlin features

// ✅ Phase 5: Convert infrastructure
// Migrate framework integration code last
// Ensure stability throughout
```

**Strategy**: Bottom-up approach minimizes breaking changes.

### Using IntelliJ IDEA Converter

IntelliJ provides automated Java to Kotlin conversion.

```kotlin
// ✅ Steps to convert:
// 1. Open Java file in IntelliJ IDEA
// 2. Code → Convert Java File to Kotlin File (Ctrl+Alt+Shift+K)
// 3. Review generated code
// 4. Refactor to idiomatic Kotlin
// 5. Fix null safety warnings
// 6. Run tests

// ❌ Don't commit converted code as-is
// ✅ Always refactor after automatic conversion
```

**Important**: Automatic conversion produces working but unidiomatic code.

## Syntax Comparison

### Class Declarations

Java verbose class declarations vs Kotlin concise syntax.

**Java**:

```java
public class User {
    private final String id;
    private final String name;
    private String email;

    public User(String id, String name, String email) {
        this.id = id;
        this.name = name;
        this.email = email;
    }

    public String getId() {
        return id;
    }

    public String getName() {
        return name;
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    @Override
    public boolean equals(Object o) {
        // ... 15 lines of equals implementation
    }

    @Override
    public int hashCode() {
        // ... 5 lines of hashCode implementation
    }

    @Override
    public String toString() {
        return "User{" +
            "id='" + id + '\'' +
            ", name='" + name + '\'' +
            ", email='" + email + '\'' +
            '}';
    }
}
```

**Kotlin**:

```kotlin
// ✅ Same functionality, 3 lines
data class User(
  val id: String,
  val name: String,
  var email: String
)
```

**Reduction**: 50+ lines → 5 lines with same functionality.

### Null Safety

Java's nullable references vs Kotlin's null safety system.

**Java**:

```java
// ❌ Null safety not enforced
public String getUserEmail(User user) {
    if (user != null && user.getProfile() != null) {
        return user.getProfile().getEmail();
    }
    return null;
}

// ❌ NullPointerException waiting to happen
public int getEmailLength(User user) {
    return user.getProfile().getEmail().length();  // 💥
}
```

**Kotlin**:

```kotlin
// ✅ Null safety enforced by compiler
fun getUserEmail(user: User?): String? {
  return user?.profile?.email  // Safe call chain
}

// ✅ Compiler error prevents NPE
fun getEmailLength(user: User): Int {
  return user.profile.email.length  // ✅ Guaranteed non-null
}

// ✅ Explicit null handling
fun getEmailLengthSafe(user: User?): Int {
  return user?.profile?.email?.length ?: 0  // Elvis operator
}
```

**Key difference**: Kotlin distinguishes `String` (never null) from `String?` (may be null).

### Function Syntax

Java methods vs Kotlin functions.

**Java**:

```java
public class Calculator {
    public int add(int a, int b) {
        return a + b;
    }

    public String formatResult(int result) {
        return "Result: " + result;
    }

    public void printResult(int result) {
        System.out.println(formatResult(result));
    }
}
```

**Kotlin**:

```kotlin
// ✅ Top-level functions (no class required)
fun add(a: Int, b: Int): Int {
  return a + b
}

// ✅ Expression body
fun add(a: Int, b: Int) = a + b

// ✅ String templates
fun formatResult(result: Int) = "Result: $result"

// ✅ Unit return type (void equivalent)
fun printResult(result: Int) {
  println(formatResult(result))
}
```

**Benefits**: Functions don't need classes, expression bodies reduce boilerplate.

### Collections

Java's verbose collection APIs vs Kotlin's expressive operations.

**Java**:

```java
// ❌ Verbose filtering and mapping
List<String> names = new ArrayList<>();
for (User user : users) {
    if (user.isActive()) {
        names.add(user.getName().toUpperCase());
    }
}

// ❌ Stream API is better but still verbose
List<String> names = users.stream()
    .filter(User::isActive)
    .map(user -> user.getName().toUpperCase())
    .collect(Collectors.toList());
```

**Kotlin**:

```kotlin
// ✅ Concise, readable collection operations
val names = users
  .filter { it.isActive }
  .map { it.name.uppercase() }

// ✅ Even more concise
val names = users.filter { it.isActive }.map { it.name.uppercase() }
```

**Improvement**: Kotlin collections are more expressive and concise.

## Null Safety Migration

### Converting Nullable Types

Map Java's potential nulls to Kotlin nullable types.

**Java**:

```java
// ❌ Everything is potentially null
public User findUser(String id) {
    // May return null
    return database.query(id);
}

public String getDefaultEmail() {
    return config.get("email");  // May return null
}
```

**Kotlin**:

```kotlin
// ✅ Explicit nullability
fun findUser(id: String): User? {
  return database.query(id)  // Explicitly nullable
}

fun getDefaultEmail(): String {
  return config.get("email") ?: "default@example.com"  // Never null
}

// ✅ Or keep nullable if appropriate
fun getConfigEmail(): String? {
  return config.get("email")
}
```

**Rule**: Make return types non-nullable when possible using Elvis operator.

### Handling @Nullable and @NotNull

Kotlin respects Java nullability annotations.

**Java with annotations**:

```java
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class UserService {
    @NotNull
    public User getUser(@NotNull String id) {
        // Implementation
    }

    @Nullable
    public User findUser(@NotNull String id) {
        // Implementation
    }
}
```

**Kotlin conversion**:

```kotlin
// ✅ Annotations translate to Kotlin types
class UserService {
  fun getUser(id: String): User {  // @NotNull → String and User
    // Implementation
  }

  fun findUser(id: String): User? {  // @Nullable → User?
    // Implementation
  }
}
```

**Tip**: Add nullability annotations to Java code before migration for smoother conversion.

### Platform Types

Kotlin uses platform types for Java code without annotations.

```kotlin
// Java method without annotations
// public String getName() { ... }

// ✅ Kotlin sees platform type String!
val name: String! = javaObject.name  // Platform type

// ✅ Treat as non-null
val name: String = javaObject.name  // Compiler allows, runtime may throw

// ✅ Treat as nullable (safer)
val name: String? = javaObject.name

// ✅ Best practice: explicit null check
val name = javaObject.name ?: "Unknown"
```

**Warning**: Platform types bypass null safety - handle carefully.

## Extension Functions

### Converting Utility Methods

Java static utility methods become Kotlin extension functions.

**Java**:

```java
// ❌ Utility class pattern
public class StringUtils {
    public static boolean isValidEmail(String email) {
        return email != null && email.contains("@");
    }

    public static String truncate(String text, int maxLength) {
        if (text.length() <= maxLength) {
            return text;
        }
        return text.substring(0, maxLength) + "...";
    }
}

// Usage
if (StringUtils.isValidEmail(email)) {
    String truncated = StringUtils.truncate(email, 20);
}
```

**Kotlin**:

```kotlin
// ✅ Extension functions
fun String.isValidEmail(): Boolean {
  return contains("@")
}

fun String.truncate(maxLength: Int): String {
  if (length <= maxLength) return this
  return substring(0, maxLength) + "..."
}

// ✅ Usage - more natural
if (email.isValidEmail()) {
  val truncated = email.truncate(20)
}
```

**Benefit**: Extension functions feel like native methods.

### Converting Builder Patterns

Replace Java builders with Kotlin's apply/also.

**Java**:

```java
// ❌ Builder pattern
public class HttpRequest {
    private String url;
    private String method;
    private Map<String, String> headers;

    private HttpRequest(Builder builder) {
        this.url = builder.url;
        this.method = builder.method;
        this.headers = builder.headers;
    }

    public static class Builder {
        private String url;
        private String method = "GET";
        private Map<String, String> headers = new HashMap<>();

        public Builder url(String url) {
            this.url = url;
            return this;
        }

        public Builder method(String method) {
            this.method = method;
            return this;
        }

        public Builder header(String key, String value) {
            this.headers.put(key, value);
            return this;
        }

        public HttpRequest build() {
            return new HttpRequest(this);
        }
    }
}

// Usage
HttpRequest request = new HttpRequest.Builder()
    .url("https://api.example.com")
    .method("POST")
    .header("Authorization", "Bearer token")
    .build();
```

**Kotlin**:

```kotlin
// ✅ Simple data class with defaults
data class HttpRequest(
  val url: String,
  val method: String = "GET",
  val headers: Map<String, String> = emptyMap()
)

// ✅ Usage with named parameters
val request = HttpRequest(
  url = "https://api.example.com",
  method = "POST",
  headers = mapOf("Authorization" to "Bearer token")
)

// ✅ Or with apply for complex setup
val request = HttpRequest(url = "https://api.example.com").apply {
  // Additional setup if needed
}
```

**Simplification**: Named parameters + default values eliminate builder pattern.

## Smart Casts

### Eliminating instanceof Checks

Kotlin's smart casts eliminate redundant type checks.

**Java**:

```java
// ❌ Explicit casting required
public void processShape(Shape shape) {
    if (shape instanceof Circle) {
        Circle circle = (Circle) shape;  // Redundant cast
        System.out.println("Radius: " + circle.getRadius());
    } else if (shape instanceof Rectangle) {
        Rectangle rectangle = (Rectangle) shape;  // Redundant cast
        System.out.println("Width: " + rectangle.getWidth());
    }
}
```

**Kotlin**:

```kotlin
// ✅ Smart casts - no explicit casting
fun processShape(shape: Shape) {
  when (shape) {
    is Circle -> println("Radius: ${shape.radius}")  // Automatically cast
    is Rectangle -> println("Width: ${shape.width}")  // Automatically cast
  }
}

// ✅ In if expressions
fun processShape(shape: Shape) {
  if (shape is Circle) {
    println("Radius: ${shape.radius}")  // Smart cast to Circle
  }
}
```

**Benefit**: Compiler automatically casts after type check.

## When Expression

### Replacing Switch Statements

Java's switch vs Kotlin's when expression.

**Java**:

```java
// ❌ Statement, not expression
public String getStatusMessage(Status status) {
    String message;
    switch (status) {
        case PENDING:
            message = "Waiting for approval";
            break;
        case APPROVED:
            message = "Processing";
            break;
        case COMPLETED:
            message = "Done";
            break;
        case REJECTED:
            message = "Failed";
            break;
        default:
            message = "Unknown status";
    }
    return message;
}
```

**Kotlin**:

```kotlin
// ✅ Expression with return value
fun getStatusMessage(status: Status): String = when (status) {
  Status.PENDING -> "Waiting for approval"
  Status.APPROVED -> "Processing"
  Status.COMPLETED -> "Done"
  Status.REJECTED -> "Failed"
}  // No default needed if all cases covered (sealed class)

// ✅ With multiple conditions
fun getCategory(value: Int) = when {
  value < 0 -> "Negative"
  value == 0 -> "Zero"
  value in 1..10 -> "Small"
  value in 11..100 -> "Medium"
  else -> "Large"
}
```

**Advantage**: `when` is an expression (returns value) and more powerful than `switch`.

## Sealed Classes for Type Safety

### Converting Inheritance Hierarchies

Java's open hierarchies vs Kotlin's sealed classes.

**Java**:

```java
// ❌ Open hierarchy - any class can extend
public abstract class Result {
    public static class Success extends Result {
        private final String data;

        public Success(String data) {
            this.data = data;
        }

        public String getData() {
            return data;
        }
    }

    public static class Error extends Result {
        private final String message;

        public Error(String message) {
            this.message = message;
        }

        public String getMessage() {
            return message;
        }
    }
}

// ❌ Need default case in switch
public void handleResult(Result result) {
    if (result instanceof Result.Success) {
        Result.Success success = (Result.Success) result;
        System.out.println(success.getData());
    } else if (result instanceof Result.Error) {
        Result.Error error = (Result.Error) result;
        System.err.println(error.getMessage());
    } else {
        // Default case required
    }
}
```

**Kotlin**:

```kotlin
// ✅ Sealed class - all subclasses known at compile time
sealed class Result {
  data class Success(val data: String) : Result()
  data class Error(val message: String) : Result()
  data class Loading(val progress: Int) : Result()
}

// ✅ Exhaustive when - no else needed
fun handleResult(result: Result) = when (result) {
  is Result.Success -> println(result.data)
  is Result.Error -> System.err.println(result.message)
  is Result.Loading -> println("Loading: ${result.progress}%")
}  // Compiler ensures all cases handled
```

**Benefit**: Sealed classes provide exhaustive when expressions (compiler guarantees all cases handled).

## Property Delegation

### Replacing Lazy Initialization

Java's lazy initialization patterns vs Kotlin's delegates.

**Java**:

```java
// ❌ Verbose lazy initialization
public class DatabaseService {
    private Connection connection;

    public Connection getConnection() {
        if (connection == null) {
            synchronized (this) {
                if (connection == null) {
                    connection = Database.connect();
                }
            }
        }
        return connection;
    }
}
```

**Kotlin**:

```kotlin
// ✅ Lazy delegate - thread-safe by default
class DatabaseService {
  val connection: Connection by lazy {
    Database.connect()
  }
}

// ✅ Usage is transparent
val service = DatabaseService()
val conn = service.connection  // Initialized on first access
```

**Simplification**: Thread-safe lazy initialization in one line.

### Observable Properties

Replace Java listeners with Kotlin delegates.

**Java**:

```java
// ❌ Manual property change notification
public class User {
    private String name;
    private List<PropertyChangeListener> listeners = new ArrayList<>();

    public String getName() {
        return name;
    }

    public void setName(String name) {
        String oldValue = this.name;
        this.name = name;
        notifyListeners("name", oldValue, name);
    }

    public void addPropertyChangeListener(PropertyChangeListener listener) {
        listeners.add(listener);
    }

    private void notifyListeners(String property, Object oldValue, Object newValue) {
        for (PropertyChangeListener listener : listeners) {
            listener.propertyChange(new PropertyChangeEvent(this, property, oldValue, newValue));
        }
    }
}
```

**Kotlin**:

```kotlin
// ✅ Observable delegate
class User {
  var name: String by Delegates.observable("") { property, oldValue, newValue ->
    println("${property.name} changed from $oldValue to $newValue")
  }
}

// ✅ Usage
val user = User()
user.name = "Alice"  // Automatically triggers observer
```

## Coroutines vs Threads

### Asynchronous Code Migration

Java's threads and futures vs Kotlin coroutines.

**Java**:

```java
// ❌ Callback hell
public void fetchUserData(String userId, Callback callback) {
    new Thread(() -> {
        try {
            User user = userApi.getUser(userId);
            Profile profile = profileApi.getProfile(user.getId());
            Posts posts = postsApi.getPosts(user.getId());
            callback.onSuccess(new UserData(user, profile, posts));
        } catch (Exception e) {
            callback.onError(e);
        }
    }).start();
}

// ❌ CompletableFuture better but still complex
public CompletableFuture<UserData> fetchUserData(String userId) {
    return CompletableFuture.supplyAsync(() -> userApi.getUser(userId))
        .thenCompose(user ->
            CompletableFuture.supplyAsync(() -> profileApi.getProfile(user.getId()))
                .thenCombine(
                    CompletableFuture.supplyAsync(() -> postsApi.getPosts(user.getId())),
                    (profile, posts) -> new UserData(user, profile, posts)
                )
        );
}
```

**Kotlin**:

```kotlin
// ✅ Sequential-looking async code
suspend fun fetchUserData(userId: String): UserData {
  val user = userApi.getUser(userId)  // Suspend, don't block
  val profile = profileApi.getProfile(user.id)
  val posts = postsApi.getPosts(user.id)
  return UserData(user, profile, posts)
}

// ✅ Parallel execution
suspend fun fetchUserDataParallel(userId: String): UserData = coroutineScope {
  val user = async { userApi.getUser(userId) }
  val profile = async { profileApi.getProfile(userId) }
  val posts = async { postsApi.getPosts(userId) }
  UserData(user.await(), profile.await(), posts.await())
}
```

**Advantage**: Coroutines provide sequential syntax for asynchronous code.

## Common Pitfalls

### Over-Using !! Operator

```kotlin
// ❌ Don't force unwrap nulls
fun getEmail(user: User?): String {
  return user!!.email!!  // Defeats null safety purpose
}

// ✅ Handle nulls properly
fun getEmail(user: User?): String? {
  return user?.email
}

// ✅ Or provide default
fun getEmail(user: User?): String {
  return user?.email ?: "no-email@example.com"
}
```

**Why problematic**: `!!` throws NullPointerException, negating Kotlin's null safety.

### Not Using Data Classes

```kotlin
// ❌ Converted POJO, not idiomatic
class User(val id: String, val name: String) {
  override fun equals(other: Any?): Boolean {
    // Manually implemented equals
  }

  override fun hashCode(): Int {
    // Manually implemented hashCode
  }
}

// ✅ Use data class
data class User(val id: String, val name: String)
```

**Why problematic**: Missing out on Kotlin's data class benefits.

### Mutable Collections by Default

```kotlin
// ❌ Converted Java code often uses mutable collections
val users = mutableListOf<User>()  // Mutable

// ✅ Prefer immutable by default
val users = listOf<User>()  // Immutable

// ✅ Use mutable only when needed
val users = mutableListOf<User>()
users.add(newUser)  // Now mutation is intentional
```

## Variations

### Gradual Migration with @JvmName

Maintain Java compatibility during migration.

```kotlin
// ✅ Keep Java-friendly method names
@JvmName("getUserDisplayName")
fun User.displayName(): String = "$name ($email)"

// Java can call: UserExtensionsKt.getUserDisplayName(user)
```

### Using @JvmStatic for Static Methods

```kotlin
// ✅ Make companion object methods static in Java
class Config {
  companion object {
    @JvmStatic
    fun load(): Config {
      return Config()
    }
  }
}

// Java: Config.load() (static method)
```

### Migration Testing Strategy

```kotlin
// ✅ Keep existing tests, add Kotlin tests
// 1. Convert Java test to Kotlin
// 2. Run both versions side-by-side
// 3. Remove Java test after verification

// Example: JUnit test migration
class UserServiceTest {
  @Test
  fun `should create user successfully`() {
    val service = UserService()
    val user = service.create("Alice", "alice@example.com")

    assertNotNull(user)
    assertEquals("Alice", user.name)
  }
}
```

## Related Patterns

**Learn more**:

- [Beginner Tutorial - Java Interop](/en/learn/swe/prog-lang/kotlin/tutorials/beginner#java-interoperability) - Interoperability basics
- [Handle Java Interoperability](/en/learn/swe/prog-lang/kotlin/how-to/integrate-with-java) - Calling Java from Kotlin
- [Extension Functions](/en/learn/swe/prog-lang/kotlin/tutorials/intermediate#extension-functions) - Extending Java classes
- [Coroutines Guide](/en/learn/swe/prog-lang/kotlin/tutorials/intermediate#coroutines) - Async migration patterns

**Cookbook recipes**:

- [Java Interop Patterns](/en/learn/swe/prog-lang/kotlin/how-to/cookbook#java-interop) - Quick reference
- [Null Safety Patterns](/en/learn/swe/prog-lang/kotlin/how-to/cookbook#nullable-optional) - Handling nulls
- [Collection Operations](/en/learn/swe/prog-lang/kotlin/how-to/cookbook#collections) - Modern collection APIs
