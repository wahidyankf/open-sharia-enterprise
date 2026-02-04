---
title: "Java 23 (September 2024)"
date: 2026-02-04T00:00:00+07:00
draft: false
description: "Java 23 release highlights - ZGC Generational by default, Markdown Documentation, Primitive Patterns, Module Imports, Class-File API, Stream Gatherers, Flexible Constructor Bodies"
weight: 1000007
tags: ["java", "java-23", "zgc-generational", "markdown-javadoc", "primitive-patterns", "module-imports"]
---

## Release Information

- **Release Date**: September 17, 2024
- **Support Type**: Non-LTS (6-month support)
- **End of Support**: March 2025 (superseded by Java 24)
- **JEPs**: 12 enhancements

**Highlights**: 2 final features, 8 preview features, 1 incubator, 1 deprecation

## Final Features

### ZGC: Generational Mode by Default (JEP 474) - **FINAL**

ZGC now uses **generational garbage collection by default**.

**What changed:**

```bash
# Before (Java 22): Non-generational by default
java -XX:+UseZGC MyApp

# After (Java 23): Generational by default
java -XX:+UseZGC MyApp  # Uses generational ZGC

# Opt-out to non-generational
java -XX:+UseZGC -XX:-ZGenerational MyApp
```

**Why generational?**

Generational hypothesis: Most objects die young.

**Benefits:**

- **Lower CPU overhead** (10-20% reduction)
- **Better throughput** (similar latency)
- **Smaller heap footprint**

**Performance comparison:**

| Mode             | CPU Overhead | Pause Time | Throughput |
| ---------------- | ------------ | ---------- | ---------- |
| Non-generational | Higher       | <1ms       | Lower      |
| Generational     | Lower        | <1ms       | Higher     |

**When to use ZGC generational:**

- Applications with high allocation rates
- Large heaps (multiple GB)
- Need both low latency AND good throughput

### Markdown Documentation Comments (JEP 467) - **FINAL**

Write JavaDoc using **Markdown** instead of HTML.

**Old (HTML in JavaDoc):**

```java
/**
 * Computes the sum of two numbers.
 * <p>
 * <b>Example:</b>
 * <pre>{@code
 * int result = add(2, 3); // returns 5
 * }</pre>
 *
 * @param a the first number
 * @param b the second number
 * @return the sum of a and b
 */
public int add(int a, int b) {
    return a + b;
}
```

**New (Markdown):**

````java
/// Computes the sum of two numbers.
///
/// **Example:**
/// ```java
/// int result = add(2, 3); // returns 5
/// ```
///
/// @param a the first number
/// @param b the second number
/// @return the sum of a and b
public int add(int a, int b) {
    return a + b;
}
````

**Markdown features:**

- **Headers**: `## Header`, `### Subheader`
- **Bold/Italic**: `**bold**`, `*italic*`
- **Code blocks**: ` ```java ... ``` `
- **Lists**: `- item 1`, `1. numbered`
- **Links**: `[text](https://example.com)`
- **Tables**: Markdown table syntax

**Mixed mode:**

```java
/**
 * Traditional JavaDoc comment (HTML)
 */

///
/// Markdown comment (Markdown syntax)
///
```

## Preview Features

### Primitive Types in Patterns (JEP 455) - **Preview**

Pattern matching now works with **primitive types**.

**Example:**

```java
static void printType(Object obj) {
    switch (obj) {
        case Integer i -> System.out.println("Integer: " + i);
        case Long l -> System.out.println("Long: " + l);
        case int i -> System.out.println("int: " + i);  // ✨ NEW: primitive
        case long l -> System.out.println("long: " + l); // ✨ NEW: primitive
        case String s -> System.out.println("String: " + s);
        default -> System.out.println("Unknown");
    }
}
```

**Widening conversions:**

```java
static void process(Number num) {
    switch (num) {
        case byte b -> System.out.println("byte: " + b);
        case short s -> System.out.println("short: " + s);
        case int i -> System.out.println("int: " + i);
        case long l -> System.out.println("long: " + l);
        case float f -> System.out.println("float: " + f);
        case double d -> System.out.println("double: " + d);
        default -> System.out.println("Other number");
    }
}
```

### Module Import Declarations (JEP 476) - **Preview**

Import all packages from a module in one statement.

**Old (verbose):**

```java
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.stream.Stream;
import java.util.stream.Collectors;
// ... many more imports
```

**New (concise):**

```java
import module java.base; // Imports all java.util.*, java.io.*, etc.
```

**Real-world example:**

```java
import module java.base;
import module java.sql;

public class DatabaseExample {
    public void process() {
        // All java.base and java.sql classes available
        List<String> names = new ArrayList<>();
        try (Connection conn = DriverManager.getConnection(url)) {
            // ...
        }
    }
}
```

**Benefits:**

- Fewer import statements
- Clearer module dependencies
- Easier refactoring

### Class-File API (JEP 466) - **Second Preview**

Standard API for parsing, generating, and transforming Java class files.

**Use cases:**

- Bytecode manipulation frameworks (ASM, ByteBuddy alternatives)
- Code coverage tools
- Profilers and instrumentation
- Build tools

**Example (read class file):**

```java
import java.lang.classfile.*;

Path classFile = Path.of("MyClass.class");
ClassModel model = ClassFile.of().parse(classFile);

System.out.println("Class: " + model.thisClass().asInternalName());
for (MethodModel method : model.methods()) {
    System.out.println("Method: " + method.methodName());
}
```

**Transform class file:**

```java
ClassFile cf = ClassFile.of();
byte[] transformedBytes = cf.transform(originalBytes, (cb, ce) -> {
    if (ce instanceof MethodModel mm) {
        cb.transformMethod(mm, (mb, me) -> {
            // Add logging to each method
            mb.with(me);
        });
    } else {
        cb.with(ce);
    }
});
```

### Stream Gatherers (JEP 473) - **Second Preview**

Enhanced custom stream operations.

**New gatherers:**

```java
// distinctBy: Remove duplicates by key
List<Person> people = ...;
List<Person> uniqueByName = people.stream()
    .gather(Gatherers.distinctBy(Person::name))
    .toList();

// mapConcurrent: Parallel transformation
List<String> urls = ...;
List<Data> results = urls.stream()
    .gather(Gatherers.mapConcurrent(url -> fetchData(url)))
    .toList();
```

## Continued Preview Features

### Structured Concurrency (JEP 480) - **Third Preview**

Refinements to task scoping.

**New: Timeout support:**

```java
try (var scope = new StructuredTaskScope.ShutdownOnFailure()) {
    scope.fork(() -> fetchUserData());
    scope.fork(() -> fetchOrderData());

    scope.joinUntil(Instant.now().plusSeconds(5)); // Timeout after 5s
    scope.throwIfFailed();
}
```

### Scoped Values (JEP 481) - **Third Preview**

Improved thread-local alternative.

**Enhanced API:**

```java
private static final ScopedValue<User> CURRENT_USER = ScopedValue.newInstance();

void handleRequest(User user) {
    ScopedValue.runWhere(CURRENT_USER, user, () -> {
        processRequest();
        // CURRENT_USER available in all nested calls
    });
}
```

### Flexible Constructor Bodies (JEP 482) - **Second Preview**

More flexibility in constructor initialization.

**Example:**

```java
class ValidatedPoint {
    final int x, y;

    public ValidatedPoint(int x, int y) {
        // Validation before field initialization
        if (x < 0 || y < 0) {
            throw new IllegalArgumentException("Coordinates must be non-negative");
        }

        // Transform before super()
        int normalizedX = normalize(x);
        int normalizedY = normalize(y);

        this.x = normalizedX;
        this.y = normalizedY;
    }
}
```

### Implicitly Declared Classes (JEP 477) - **Third Preview**

Simplified Java for beginners.

**Example:**

```java
// Entire program
void main() {
    String name = readLine("Enter name: ");
    println("Hello, " + name + "!");
}
```

## Incubator Features

### Vector API (JEP 469) - **Eighth Incubator**

Continued SIMD API refinement.

## Deprecations

### Deprecate sun.misc.Unsafe Memory-Access Methods (JEP 471)

Memory access methods in `sun.misc.Unsafe` deprecated for removal.

**Reason**: FFM API is now standard (safer and more efficient)

**Migration:**

```java
// ❌ OLD: sun.misc.Unsafe
Unsafe unsafe = Unsafe.getUnsafe();
long address = unsafe.allocateMemory(1024);
unsafe.putInt(address, 42);
unsafe.freeMemory(address);

// ✅ NEW: Foreign Function & Memory API
try (Arena arena = Arena.ofConfined()) {
    MemorySegment segment = arena.allocate(1024);
    segment.set(ValueLayout.JAVA_INT, 0, 42);
} // Automatic cleanup
```

### Prepare to Restrict JNI Use (JEP 472)

Warnings when using JNI (preparation for future restrictions).

**Goal**: Encourage migration to FFM API

## Migration Considerations

**Upgrading from Java 21 LTS:**

1. **ZGC**: Generational mode now default (better performance)
2. **Markdown JavaDoc**: Can simplify documentation writing
3. **Module imports**: Reduce import boilerplate
4. **Unsafe deprecation**: Plan migration to FFM API
5. **Preview features**: Most features require `--enable-preview`

**Enable preview features:**

```bash
java --enable-preview --source 23 MyApp.java
```

**Compatibility**: Binary compatible with Java 21

## Related Topics

- [Java 22](/en/learn/software-engineering/programming-languages/java/release-highlights/java-22) - Previous release
- [Java 21 LTS](/en/learn/software-engineering/programming-languages/java/release-highlights/java-21) - Latest LTS
- [Performance Optimization](/en/learn/software-engineering/programming-languages/java/in-practice/performance) - In-practice guide

## References

**Sources:**

- [The Arrival of Java 23 | Oracle Java Blog](https://blogs.oracle.com/java/the-arrival-of-java-23)
- [Oracle Releases Java 23](https://www.oracle.com/news/announcement/oracle-releases-java-23-2024-09-17/)
- [JDK 23: What is new in Java 23?](https://symflower.com/en/company/blog/2024/what-is-new-in-java-23/)
- [What's New With Java 23 | JRebel](https://www.jrebel.com/blog/whats-new-java-23)

**Official OpenJDK JEPs:**

- [JEP 474: ZGC: Generational Mode by Default](https://openjdk.org/jeps/474)
- [JEP 467: Markdown Documentation Comments](https://openjdk.org/jeps/467)
- [JEP 455: Primitive Types in Patterns, instanceof, and switch (Preview)](https://openjdk.org/jeps/455)
- [JEP 476: Module Import Declarations (Preview)](https://openjdk.org/jeps/476)
- [JEP 466: Class-File API (Second Preview)](https://openjdk.org/jeps/466)
- [JEP 473: Stream Gatherers (Second Preview)](https://openjdk.org/jeps/473)
