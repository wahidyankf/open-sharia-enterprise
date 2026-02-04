---
title: "Java 22 (March 2024)"
date: 2026-02-04T00:00:00+07:00
draft: false
description: "Java 22 release highlights - Foreign Function & Memory API final, Unnamed Variables & Patterns, Launch Multi-File Programs, Stream Gatherers, String Templates second preview"
weight: 1000009
tags: ["java", "java-22", "ffm-api", "unnamed-patterns", "stream-gatherers", "string-templates"]
---

## Release Information

- **Release Date**: March 19, 2024
- **Support Type**: Non-LTS (6-month support)
- **End of Support**: September 2024 (superseded by Java 23)
- **JEPs**: 12 enhancements

**Highlights**: 4 final features, 7 preview features, 1 incubator

## Final Features

### Foreign Function & Memory API (JEP 454) - **FINAL** 🎉

After **8 rounds** of incubation/preview (Java 14-21), FFM API is now **standard**.

**Why this matters**: Safe, efficient alternative to JNI for native code integration.

**Call C library functions:**

```java
import java.lang.foreign.*;

// Load C standard library
Linker linker = Linker.nativeLinker();
SymbolLookup stdlib = linker.defaultLookup();

// Get strlen function
MethodHandle strlen = linker.downcallHandle(
    stdlib.find("strlen").orElseThrow(),
    FunctionDescriptor.of(ValueLayout.JAVA_LONG, ValueLayout.ADDRESS)
);

// Call strlen
try (Arena arena = Arena.ofConfined()) {
    MemorySegment str = arena.allocateFrom("Hello, Java 22!");
    long length = (long) strlen.invoke(str);
    System.out.println("Length: " + length); // 16
}
```

**Access off-heap memory:**

```java
// Allocate 1 MB of native memory
try (Arena arena = Arena.ofConfined()) {
    MemorySegment segment = arena.allocate(1024 * 1024);

    // Write data
    segment.setAtIndex(ValueLayout.JAVA_INT, 0, 42);
    segment.setAtIndex(ValueLayout.JAVA_INT, 1, 100);

    // Read data
    int first = segment.getAtIndex(ValueLayout.JAVA_INT, 0);
    int second = segment.getAtIndex(ValueLayout.JAVA_INT, 1);

    System.out.printf("Values: %d, %d%n", first, second);
} // Memory automatically freed
```

**Benefits:**

- **90% less code** than JNI
- **4-5x better performance** than JNI
- **Type-safe** (compile-time checks)
- **Automatic memory management** (try-with-resources)
- **No security warnings** (unlike sun.misc.Unsafe)

### Unnamed Variables & Patterns (JEP 456) - **FINAL**

Use `_` for unused variables - improves code readability.

**Unnamed variables:**

```java
// Old: Unused variable warning
try {
    processFile();
} catch (IOException e) { // Warning: e is never used
    System.out.println("Failed to process file");
}

// New: Explicitly mark as unused
try {
    processFile();
} catch (IOException _) { // No warning
    System.out.println("Failed to process file");
}
```

**Multiple unused variables:**

```java
// Connecting with credentials (ignoring password)
var connection = connect(username, _, hostname);

// Reading tuple (ignoring second value)
var (first, _, third) = readTuple();
```

**Unnamed patterns:**

```java
record Point(int x, int y) {}

// Old: Must name unused components
if (obj instanceof Point(int x, int y)) {
    System.out.println("X coordinate: " + x); // y unused
}

// New: Use _ for unused components
if (obj instanceof Point(int x, _)) {
    System.out.println("X coordinate: " + x);
}
```

**Switch patterns:**

```java
switch (shape) {
    case Circle(_, double radius) -> // Ignore center
        System.out.println("Radius: " + radius);
    case Rectangle(Point(int x, _), _) -> // Ignore y and second point
        System.out.println("Top-left X: " + x);
}
```

### Launch Multi-File Source-Code Programs (JEP 458) - **FINAL**

Run Java programs with **multiple files** without compilation.

**Before (Java 21):**

```bash
# Single file: Works
java HelloWorld.java

# Multiple files: Must compile first
javac Main.java Helper.java
java Main
```

**After (Java 22):**

```bash
# Multiple files: Works directly!
java Main.java
# Automatically compiles and runs Main.java and dependencies
```

**Example:**

**Main.java:**

```java
public class Main {
    public static void main(String[] args) {
        Helper.greet("Java 22");
    }
}
```

**Helper.java:**

```java
public class Helper {
    public static void greet(String name) {
        System.out.println("Hello, " + name + "!");
    }
}
```

**Run:**

```bash
java Main.java
# Output: Hello, Java 22!
```

**Use cases:**

- Scripts and utilities
- Learning and prototyping
- Quick experiments
- CI/CD scripts

### Region Pinning for G1 (JEP 423) - **FINAL**

G1 GC can now perform garbage collection **even when JNI code holds references**.

**Problem (Java 21):**

- JNI code locks entire heap regions
- GC must wait for JNI to complete
- Can cause long pause times

**Solution (Java 22):**

- Pin individual objects instead of regions
- GC continues on other regions
- Reduced pause times

**Impact**: Lower latency for applications using JNI

## Preview Features

### String Templates (JEP 459) - **Second Preview**

Embed expressions in strings safely.

**Example:**

```java
int x = 10, y = 20;

// Old: Concatenation
String message = "Sum of " + x + " and " + y + " is " + (x + y);

// Old: String.format
String message = String.format("Sum of %d and %d is %d", x, y, x + y);

// New: String templates
String message = STR."Sum of \{x} and \{y} is \{x + y}";
```

**Multi-line templates:**

```java
String json = STR."""
    {
        "name": "\{user.name()}",
        "age": \{user.age()},
        "active": \{user.isActive()}
    }
    """;
```

**Custom processors:**

```java
// JSON template
var json = JSON."""
    {
        "id": \{id},
        "value": "\{value}"
    }
    """;

// SQL template (with escaping)
ResultSet rs = DB."""
    SELECT * FROM users
    WHERE name = \{userName}
    """;
```

**Benefits:**

- Type-safe interpolation
- Protection against injection attacks
- Better readability than concatenation

### Stream Gatherers (JEP 461) - **Preview**

Custom intermediate stream operations.

**Built-in gatherers:**

```java
// Window: Group elements into fixed-size windows
List<Integer> numbers = List.of(1, 2, 3, 4, 5, 6);
List<List<Integer>> windows = numbers.stream()
    .gather(Gatherers.windowFixed(3))
    .toList();
// [[1, 2, 3], [4, 5, 6]]

// Scan: Running accumulation
List<Integer> runningSum = numbers.stream()
    .gather(Gatherers.scan(() -> 0, (sum, num) -> sum + num))
    .toList();
// [1, 3, 6, 10, 15, 21]
```

**Custom gatherer:**

```java
// Group consecutive duplicates
Gatherer<String, ?, List<String>> deduplicateConsecutive = /* ... */;

List<String> items = List.of("a", "a", "b", "b", "b", "c");
List<String> deduplicated = items.stream()
    .gather(deduplicateConsecutive)
    .toList();
// ["a", "b", "c"]
```

### Implicitly Declared Classes (JEP 463) - **Second Preview**

Simplified entry point for beginners.

**Old (verbose for beginners):**

```java
public class HelloWorld {
    public static void main(String[] args) {
        System.out.println("Hello, World!");
    }
}
```

**New (simplified):**

```java
void main() {
    System.out.println("Hello, World!");
}
```

**Benefits:**

- No class declaration
- No String[] args
- No public/static modifiers
- Perfect for learning

### Statements before super() (JEP 447) - **Preview**

Execute code before calling super constructor.

**Old:**

```java
class SubClass extends BaseClass {
    public SubClass(int value) {
        // ❌ Can't do anything before super()
        super(value * 2);
    }
}
```

**New:**

```java
class SubClass extends BaseClass {
    public SubClass(int value) {
        // ✅ Can validate/transform before super()
        if (value < 0) {
            throw new IllegalArgumentException("Value must be positive");
        }
        int transformed = value * 2;
        super(transformed);
    }
}
```

### Structured Concurrency (JEP 462) & Scoped Values (JEP 464) - **Second Preview**

Continued refinement from Java 21.

## Incubator Features

### Vector API (JEP 460) - **Seventh Incubator**

Continued SIMD API refinement with minor improvements.

## Migration Considerations

**Upgrading from Java 21 LTS:**

1. **FFM API now standard**: Remove `--enable-preview` for FFM code
2. **Unnamed patterns**: Refactor unused variable suppression
3. **Multi-file launch**: Simplify development scripts
4. **String Templates**: Consider for safer string composition
5. **G1 region pinning**: May see reduced pause times with JNI

**Compatibility**: Binary compatible with Java 21

## Related Topics

- [Java 21 LTS](/en/learn/software-engineering/programming-languages/java/release-highlights/java-21) - Previous LTS
- [Java 23](/en/learn/software-engineering/programming-languages/java/release-highlights/java-23) - Next release
- [Foreign Function & Memory API](/en/learn/software-engineering/programming-languages/java/in-the-field/json-and-api-integration) - In-the-field guide

## References

**Sources:**

- [The Arrival of Java 22! – Inside.java](https://inside.java/2024/03/19/the-arrival-of-java-22/)
- [Oracle Releases Java 22](https://www.oracle.com/news/announcement/oracle-releases-java-22-2024-03-19/)
- [Java 22 Features (with Examples)](https://www.happycoders.eu/java/java-22-features/)
- [Java 22 Delivers Foreign Memory & Memory API, Unnamed Variables & Patterns - InfoQ](https://www.infoq.com/news/2024/03/java22-released/)

**Official OpenJDK JEPs:**

- [JEP 454: Foreign Function & Memory API](https://openjdk.org/jeps/454)
- [JEP 456: Unnamed Variables & Patterns](https://openjdk.org/jeps/456)
- [JEP 458: Launch Multi-File Source-Code Programs](https://openjdk.org/jeps/458)
- [JEP 423: Region Pinning for G1](https://openjdk.org/jeps/423)
- [JEP 459: String Templates (Second Preview)](https://openjdk.org/jeps/459)
- [JEP 461: Stream Gatherers (Preview)](https://openjdk.org/jeps/461)
