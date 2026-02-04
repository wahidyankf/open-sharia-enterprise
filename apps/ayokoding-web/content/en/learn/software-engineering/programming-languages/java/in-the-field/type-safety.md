---
title: "Type Safety"
date: 2026-02-03T00:00:00+07:00
draft: false
description: Leverage Java's type system with records, Optional, sealed classes, and nullability annotations for compile-time safety
weight: 10000008
tags: ["java", "type-safety", "records", "optional", "sealed-classes", "null-safety", "jspecify"]
---

## Understanding Type Safety in Java

Type safety means using the compiler to catch errors before runtime. Modern Java provides powerful type system features: records for immutable data, Optional for explicit nullability, sealed classes for exhaustive pattern matching, and JSpecify annotations for null safety.

**Why type safety matters:**

- **Compile-time error detection**: Catch bugs during compilation, not production
- **Self-documenting code**: Types communicate intent and constraints
- **Refactoring confidence**: Compiler verifies all usages
- **IDE support**: Better autocomplete and refactoring tools

This guide covers modern type safety patterns that eliminate entire categories of runtime errors.

## Records - Immutable Data Carriers

**Problem**: Traditional JavaBeans require boilerplate (getters, setters, equals, hashCode, toString) and are mutable by default, leading to verbose code and potential bugs.

**Recognition signals:**

- Classes with only private fields and getters
- Manual equals/hashCode implementations
- Lengthy toString methods
- Defensive copying to maintain immutability
- Constructor parameter validation scattered across code

**Solution**: Records provide concise, immutable data classes with automatic implementations.

| Characteristic  | Traditional Class          | Record              |
| --------------- | -------------------------- | ------------------- |
| Syntax          | 30+ lines for simple class | 1 line              |
| Immutability    | Manual final fields        | Automatic           |
| equals/hashCode | Manual implementation      | Generated           |
| toString        | Manual string building     | Generated           |
| Validation      | Constructor logic          | Compact constructor |

**Example transformation:**

```java
// PROBLEMATIC: Verbose JavaBean
public class PersonBean {
    private final String name;
    private final int age;

    public PersonBean(String name, int age) {
        this.name = name;
        this.age = age;
    }

    public String getName() { return name; }
    public int getAge() { return age; }

    @Override
    public boolean equals(Object o) {
        // 10+ lines of equals logic
    }

    @Override
    public int hashCode() {
        // hashCode logic
    }

    @Override
    public String toString() {
        // toString logic
    }
}

// SOLUTION: Concise record
public record Person(String name, int age) {
    // COMPACT CONSTRUCTOR: validation
    public Person {
        if (age < 0) throw new IllegalArgumentException("Age must be non-negative");
    }
}
```

**Impact**: Records reduce boilerplate by 90%, enforce immutability automatically, and make data modeling clear and concise.

### Record Patterns and Features

**Functional updates** (since records are immutable):

```java
public record Person(String name, int age) {
    public Person withName(String newName) {
        return new Person(newName, this.age);
    }

    public Person withAge(int newAge) {
        return new Person(this.name, newAge);
    }
}

// USAGE
Person original = new Person("Alice", 30);
Person updated = original.withAge(31);  // NEW INSTANCE
```

**Pattern matching** (Java 16+):

```java
if (obj instanceof Person(String name, int age)) {
    System.out.println(name + " is " + age + " years old");  // EXTRACTED
}
```

## Optional - Explicit Nullability

**Problem**: Null references cause NullPointerException - the "billion-dollar mistake". Null is implicit, forcing defensive null checks everywhere.

**Recognition signals:**

- Pervasive `if (x != null)` checks
- NullPointerException in production
- Unclear which variables can be null
- Defensive programming bloat
- @Nullable annotations ignored at runtime

**Solution**: Optional<T> makes nullability explicit in the type system.

| Characteristic | Null-based              | Optional-based              |
| -------------- | ----------------------- | --------------------------- |
| Null safety    | Runtime checks          | Compile-time enforcement    |
| Intent         | Unclear if null allowed | Explicit in type            |
| Chaining       | Nested if-null checks   | Monadic chaining            |
| Default values | Manual ternary          | `.orElse()`, `.orElseGet()` |

**Comparison:**

```java
// PROBLEMATIC: Implicit null handling
public String getUserEmail(String userId) {
    User user = findUser(userId);  // MIGHT BE NULL
    if (user == null) return null;  // PROPAGATES NULL
    Address address = user.getAddress();  // MIGHT BE NULL
    if (address == null) return null;
    return address.getEmail();  // MIGHT BE NULL
}

// SOLUTION: Explicit Optional chaining
public Optional<String> getUserEmail(String userId) {
    return findUser(userId)  // Optional<User>
        .flatMap(User::getAddress)  // Optional<Address>
        .flatMap(Address::getEmail);  // Optional<String>
}

// USAGE
String email = getUserEmail("123")
    .orElse("no-email@example.com");
```

### Optional Best Practices

| Use Case    | Recommendation            | Rationale                                |
| ----------- | ------------------------- | ---------------------------------------- |
| Return type | ✓ Use Optional            | Signals "may be absent"                  |
| Field       | ✗ Avoid Optional fields   | Extra indirection, not serializable      |
| Parameter   | ✗ Avoid Optional params   | Caller forced to wrap                    |
| Collection  | ✗ Return empty collection | Collections already handle "no elements" |

**Key operations:**

```java
// CREATION
Optional<String> present = Optional.of("value");  // NULL THROWS
Optional<String> nullable = Optional.ofNullable(maybeNull);  // NULL -> EMPTY
Optional<String> empty = Optional.empty();

// TRANSFORMATION
optional.map(String::toUpperCase)  // Transform if present
    .filter(s -> s.length() > 5)  // Keep if matches
    .flatMap(this::findRelated)  // Chain Optional operations

// EXTRACTION
optional.orElse("default");  // EAGER: Always evaluates default
optional.orElseGet(() -> computeDefault());  // LAZY: Only if empty
optional.orElseThrow();  // Throw NoSuchElementException if empty
optional.orElseThrow(() -> new CustomException());  // Custom exception
```

## Sealed Classes - Exhaustive Type Hierarchies

**Problem**: Open inheritance allows any class to extend, making exhaustive handling impossible. Cannot guarantee all subtypes are known.

**Recognition signals:**

- Switch statements with default case for "should never happen"
- instanceof chains without completeness guarantee
- Documentation stating "only these subclasses exist"
- Runtime errors from unexpected subtypes

**Solution**: Sealed classes explicitly control which classes can extend/implement, enabling exhaustive pattern matching.

| Characteristic   | Open Hierarchy               | Sealed Hierarchy        |
| ---------------- | ---------------------------- | ----------------------- |
| Extensibility    | Any class can extend         | Only permitted subtypes |
| Exhaustiveness   | Cannot verify                | Compiler enforces       |
| Pattern matching | Default case required        | No default needed       |
| Documentation    | Comments describe subclasses | Type system enforces    |

**Example:**

```java
// SEALED HIERARCHY: Compiler knows all subtypes
public sealed interface Result<T>
    permits Success, Failure {
}

public record Success<T>(T value) implements Result<T> {}
public record Failure<T>(String error) implements Result<T> {}

// EXHAUSTIVE PATTERN MATCHING: No default needed
public <T> String format(Result<T> result) {
    return switch (result) {
        case Success<T>(var value) -> "Success: " + value;
        case Failure<T>(var error) -> "Failure: " + error;
        // NO DEFAULT: Compiler verifies exhaustiveness
    };
}
```

**Adding new subtype:**

```java
// COMPILATION ERROR: Must update all switch statements
public record Pending<T>() implements Result<T> {}
// Compiler: "The switch expression does not cover all possible input values"
```

### Sealed Class Patterns

**Domain modeling:**

```java
sealed interface PaymentMethod permits CreditCard, BankTransfer, Cash {
}

record CreditCard(String number, String cvv) implements PaymentMethod {}
record BankTransfer(String accountNumber) implements PaymentMethod {}
record Cash(double amount) implements PaymentMethod {}

// EXHAUSTIVE: All cases handled
public double processingFee(PaymentMethod payment) {
    return switch (payment) {
        case CreditCard c -> c.number().length() == 16 ? 2.5 : 0;
        case BankTransfer b -> 0.0;
        case Cash c -> 0.0;
    };
}
```

**Benefits:**

- Compiler-verified exhaustiveness
- Refactoring safety (adding subtype forces updates everywhere)
- Clear domain modeling (all variants explicit)
- No default cases hiding bugs

## JSpecify Annotations - Null Safety

**Problem**: Java's type system doesn't distinguish nullable from non-nullable references. Tools like IntelliJ and NullAway need explicit annotations for static analysis.

**Solution**: JSpecify provides standard nullability annotations that tools understand.

| Annotation    | Purpose                        | Example                      |
| ------------- | ------------------------------ | ---------------------------- |
| `@Nullable`   | Value can be null              | `@Nullable String getName()` |
| `@NonNull`    | Value cannot be null           | `@NonNull String getId()`    |
| `@NullMarked` | Package/class default non-null | Applied to package-info.java |

**Comparison:**

```java
// WITHOUT ANNOTATIONS: Unclear nullability
public String getUserName(String userId) {
    User user = findUser(userId);  // NULL?
    return user.getName();  // MIGHT NPE
}

// WITH ANNOTATIONS: Explicit contract
@NullMarked  // Package-level: all non-null by default
public class UserService {
    public @Nullable String getUserName(String userId) {
        User user = findUser(userId);  // NON-NULL (default)
        if (user == null) {  // COMPILATION WARNING: Impossible
            return null;
        }
        return user.getName();  // NON-NULL
    }

    private @Nullable User findUser(String userId) {
        return database.get(userId);  // CAN RETURN NULL
    }
}
```

**Static analysis benefits:**

- IntelliJ IDEA: Warns on potential NPE
- NullAway: Enforces null safety at build time
- Error Prone: Catches null-related bugs
- Prevents NullPointerException before runtime

### Nullability Patterns

**Defensive programming replacement:**

```java
// BEFORE: Runtime checks
public void processUser(User user) {
    Objects.requireNonNull(user, "User cannot be null");
    String name = user.getName();
    Objects.requireNonNull(name, "Name cannot be null");
    // Process...
}

// AFTER: Compile-time safety
@NullMarked
public void processUser(User user) {  // NON-NULL PARAMETER
    String name = user.getName();  // NON-NULL RETURN
    // Static analysis verifies no nulls possible
}
```

## Combining Type Safety Features

**Problem**: Using features in isolation provides partial safety. Combined, they create comprehensive type safety.

**Solution**: Layer type safety features for maximum compile-time guarantees.

### Pattern: Sealed Records with Optional

```java
// DOMAIN MODEL: All cases explicit, no null confusion
public sealed interface ApiResponse<T>
    permits Success, Error, Loading {
}

public record Success<T>(T data) implements ApiResponse<T> {}
public record Error<T>(String message) implements ApiResponse<T> {}
public record Loading<T>() implements ApiResponse<T> {}

// USAGE: Exhaustive handling, no nulls
public <T> Optional<T> extractData(ApiResponse<T> response) {
    return switch (response) {
        case Success<T>(var data) -> Optional.of(data);  // NON-NULL
        case Error<T> e -> Optional.empty();  // EXPLICIT ABSENCE
        case Loading<T> l -> Optional.empty();
    };
}
```

### Pattern: Records with Validation

```java
public record Email(String value) {
    private static final Pattern EMAIL_PATTERN =
        Pattern.compile("^[A-Za-z0-9+_.-]+@[A-Za-z0-9.-]+$");

    public Email {  // COMPACT CONSTRUCTOR
        if (value == null || !EMAIL_PATTERN.matcher(value).matches()) {
            throw new IllegalArgumentException("Invalid email: " + value);
        }
    }
}

// TYPE SAFETY: Email type guarantees valid email
public void sendNotification(Email recipient, String message) {
    // NO VALIDATION NEEDED: Type system guarantees validity
}
```

### Pattern: Nullability with Sealed Types

```java
@NullMarked
public sealed interface DatabaseResult<T>
    permits Found, NotFound, DatabaseError {
}

public record Found<T>(T value) implements DatabaseResult<T> {}
public record NotFound<T>() implements DatabaseResult<T> {}
public record DatabaseError<T>(Exception cause) implements DatabaseResult<T> {}

// USAGE: No @Nullable, no Optional, exhaustive handling
public <T> String format(DatabaseResult<T> result) {
    return switch (result) {
        case Found<T>(var value) -> "Found: " + value;
        case NotFound<T> n -> "Not found";
        case DatabaseError<T>(var cause) -> "Error: " + cause.getMessage();
    };
}
```

## Migration Strategy

Transform null-based code to type-safe style incrementally:

**Phase 1**: Introduce Optional for return types

```java
// BEFORE
public User findUser(String id) {
    return database.get(id);  // MIGHT BE NULL
}

// AFTER
public Optional<User> findUser(String id) {
    return Optional.ofNullable(database.get(id));
}
```

**Phase 2**: Replace data classes with records

```java
// BEFORE
public class Point {
    private final int x, y;
    // Constructor, getters, equals, hashCode, toString
}

// AFTER
public record Point(int x, int y) {}
```

**Phase 3**: Add JSpecify annotations

```java
// BEFORE
public String getName(User user) {
    return user.getName();
}

// AFTER
@NullMarked
public String getName(User user) {  // NON-NULL
    return user.getName();  // NON-NULL
}
```

**Phase 4**: Model variants with sealed classes

```java
// BEFORE
public enum Status { SUCCESS, ERROR, LOADING }
public class Response {
    private Status status;
    private Object data;  // UNSAFE: might be wrong type
}

// AFTER
public sealed interface Response<T>
    permits Success, Error, Loading {
}
```

## Guidelines

**When to use type safety features:**

- ✓ Records: Immutable data carriers, DTOs, value objects
- ✓ Optional: Methods that may not return a value
- ✓ Sealed classes: Finite set of variants, exhaustive handling
- ✓ JSpecify: New code, critical paths, library APIs

**When to avoid:**

- ✗ Optional fields/parameters (use @Nullable instead)
- ✗ Records for mutable entities (JPA entities, builders)
- ✗ Sealed classes for open extension points
- ✗ Annotations in legacy code without tool support

**Best practices:**

1. **Never return null**: Use Optional or sealed types for "absence"
2. **Validate in constructors**: Records guarantee valid state
3. **Prefer sealed over enums**: When variants carry different data
4. **Enable static analysis**: NullAway, Error Prone, IntelliJ inspections
5. **Document nullability**: JSpecify annotations communicate contracts

## Conclusion

Type safety in modern Java provides:

- **Compile-time error detection**: Catch bugs before runtime
- **Self-documenting code**: Types express constraints clearly
- **Refactoring confidence**: Compiler verifies all changes
- **Reduced defensive programming**: Type system enforces contracts

Start with records for data classes, Optional for nullability, and gradually adopt sealed classes and nullability annotations. Modern type safety eliminates entire categories of runtime errors through compile-time verification.
