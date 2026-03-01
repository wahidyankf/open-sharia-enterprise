---
title: "Java 19"
date: 2026-02-04T00:00:00+07:00
draft: false
description: "Java 19 release highlights - Virtual Threads preview (Project Loom), Structured Concurrency, Record Patterns, Pattern Matching for switch third preview, and Foreign Function & Memory API"
weight: 1000006
tags: ["java", "java-19", "virtual-threads", "project-loom", "structured-concurrency", "record-patterns"]
---

## Release Information

- **Release Date**: September 20, 2022
- **Support Type**: Non-LTS (6-month support)
- **End of Support**: March 2023 (superseded by Java 20)
- **JEPs**: 7 enhancements

**Headline Feature**: Virtual Threads (Project Loom)

## Preview Features

### Virtual Threads (JEP 425) - **Preview** 🌟

**Game-changing concurrency feature** from Project Loom - lightweight threads managed by the JVM.

**Traditional threads (platform threads):**

```java
// Creates OS thread - expensive (1-2 MB stack)
Thread thread = new Thread(() -> {
    // Task
});
thread.start();
```

**Virtual threads:**

```java
// Creates lightweight JVM thread - cheap (~1 KB)
Thread virtualThread = Thread.startVirtualThread(() -> {
    // Task
});
```

**Key characteristics:**

- **Lightweight**: Millions of virtual threads possible (vs thousands of platform threads)
- **Cheap to create**: No OS thread overhead
- **Cheap to block**: Blocking doesn't waste OS threads
- **Same API**: Uses standard Thread API

**High-concurrency web server example:**

```java
import java.util.concurrent.Executors;

// Platform threads - limited to ~few thousand concurrent requests
var platformExecutor = Executors.newFixedThreadPool(200);

// Virtual threads - can handle millions of concurrent requests
var virtualExecutor = Executors.newVirtualThreadPerTaskExecutor();

// Handle each request in separate virtual thread
virtualExecutor.submit(() -> handleRequest(request));
```

**Real-world impact:**

| Scenario                       | Platform Threads | Virtual Threads      |
| ------------------------------ | ---------------- | -------------------- |
| **Max concurrent connections** | ~5,000           | Millions             |
| **Memory per thread**          | 1-2 MB           | ~1 KB                |
| **Blocking I/O cost**          | Wastes OS thread | Free (thread parked) |

**Blocking I/O example:**

```java
// Old way - blocks expensive platform thread
Thread.ofPlatform().start(() -> {
    String data = readFromDatabase(); // Blocks OS thread
    processData(data);
});

// New way - blocks cheap virtual thread
Thread.ofVirtual().start(() -> {
    String data = readFromDatabase(); // Thread parked, OS thread freed
    processData(data);
});
```

**When virtual threads shine:**

- ✅ High-concurrency I/O-bound applications
- ✅ Web servers handling many concurrent requests
- ✅ Microservices with lots of network calls
- ✅ Database connection pooling (fewer connections needed)

**When NOT to use:**

- ❌ CPU-bound tasks (no benefit)
- ❌ Very short-lived tasks (overhead not worth it)
- ❌ Code with synchronized blocks (can pin threads)

### Structured Concurrency (JEP 428) - **Incubator**

Simplifies concurrent task management - treat related tasks as single unit.

**Old way (unstructured):**

```java
Future<String> user = executor.submit(() -> fetchUser());
Future<List<Order>> orders = executor.submit(() -> fetchOrders());

// Problems:
// - What if one fails?
// - Who cancels the other task?
// - How to handle timeouts?
// - Resource leaks if exception thrown
```

**New way (structured):**

```java
import jdk.incubator.concurrent.StructuredTaskScope;

record Response(String user, List<Order> orders) {}

Response getUserData() throws Exception {
    try (var scope = new StructuredTaskScope.ShutdownOnFailure()) {
        Future<String> user = scope.fork(() -> fetchUser());
        Future<List<Order>> orders = scope.fork(() -> fetchOrders());

        scope.join();           // Wait for all
        scope.throwIfFailed();  // Propagate exceptions

        return new Response(user.resultNow(), orders.resultNow());
    } // Cancels unfinished tasks automatically
}
```

**Benefits:**

- **Automatic cleanup**: Unfinished tasks cancelled when scope closes
- **Error handling**: Fail-fast on first error
- **Timeouts**: Built-in timeout support
- **Clear ownership**: Task lifetime matches scope

### Record Patterns (JEP 405) - **Preview**

Deconstruct record values in pattern matching.

**Example:**

```java
record Point(int x, int y) {}

static void printPoint(Object obj) {
    if (obj instanceof Point(int x, int y)) {
        System.out.println("X: " + x + ", Y: " + y);
    }
}

// Nested records
record Rectangle(Point topLeft, Point bottomRight) {}

static int area(Rectangle r) {
    if (r instanceof Rectangle(Point(int x1, int y1), Point(int x2, int y2))) {
        return Math.abs((x2 - x1) * (y2 - y1));
    }
    return 0;
}
```

**With switch:**

```java
sealed interface Shape permits Circle, Rectangle, Triangle {}
record Circle(double radius) implements Shape {}
record Rectangle(double width, double height) implements Shape {}
record Triangle(double base, double height) implements Shape {}

static double area(Shape shape) {
    return switch (shape) {
        case Circle(double r) -> Math.PI * r * r;
        case Rectangle(double w, double h) -> w * h;
        case Triangle(double b, double h) -> 0.5 * b * h;
    };
}
```

### Pattern Matching for switch (JEP 427) - **Third Preview**

Refinements to pattern matching in switch statements.

**Improvements:**

- Better null handling
- Refined exhaustiveness checking
- Guarded patterns

**Null handling:**

```java
static String describe(Object obj) {
    return switch (obj) {
        case null -> "null";
        case String s -> "String: " + s;
        case Integer i -> "Integer: " + i;
        default -> "Unknown";
    };
}
```

## Preview API Features

### Foreign Function & Memory API (JEP 424) - **Preview**

Merged Foreign-Memory Access and Foreign Linker APIs.

**Call native functions:**

```java
import java.lang.foreign.*;

Linker linker = Linker.nativeLinker();
SymbolLookup stdlib = linker.defaultLookup();

// Call C's strlen function
MethodHandle strlen = linker.downcallHandle(
    stdlib.find("strlen").orElseThrow(),
    FunctionDescriptor.of(ValueLayout.JAVA_LONG, ValueLayout.ADDRESS)
);

try (Arena arena = Arena.openConfined()) {
    MemorySegment str = arena.allocateUtf8String("Virtual Threads!");
    long length = (long) strlen.invoke(str);
    System.out.println("Length: " + length); // 16
}
```

**Memory segments:**

```java
try (Arena arena = Arena.openConfined()) {
    // Allocate native memory
    MemorySegment segment = arena.allocate(1024);

    // Write data
    segment.set(ValueLayout.JAVA_INT, 0, 42);
    segment.set(ValueLayout.JAVA_DOUBLE, 4, 3.14159);

    // Read data
    int value = segment.get(ValueLayout.JAVA_INT, 0);
    double pi = segment.get(ValueLayout.JAVA_DOUBLE, 4);
}
```

## Incubator Features

### Vector API (JEP 426) - **Fourth Incubator**

SIMD operations continue refinement.

**Matrix multiplication example:**

```java
import jdk.incubator.vector.*;

void matrixMultiply(float[] a, float[] b, float[] c, int n) {
    var species = FloatVector.SPECIES_PREFERRED;

    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            var sum = FloatVector.zero(species);
            int k = 0;
            for (; k < species.loopBound(n); k += species.length()) {
                var va = FloatVector.fromArray(species, a, i * n + k);
                var vb = FloatVector.fromArray(species, b, k * n + j);
                sum = va.mul(vb).add(sum);
            }
            c[i * n + j] = sum.reduceLanes(VectorOperators.ADD);
            // Handle remainder
            for (; k < n; k++) {
                c[i * n + j] += a[i * n + k] * b[k * n + j];
            }
        }
    }
}
```

## Platform Features

### Linux/RISC-V Port (JEP 422)

Java ported to RISC-V architecture on Linux.

**RISC-V**: Open-source instruction set architecture

**Use cases**: IoT devices, embedded systems, new hardware platforms

## Migration Considerations

**Upgrading from Java 17 LTS:**

1. **Evaluate Virtual Threads**: Test high-concurrency applications with virtual threads
2. **Structured Concurrency**: Refactor error-prone concurrent code
3. **Record Patterns**: Simplify record handling code
4. **Preview features**: Require `--enable-preview` and `--add-modules jdk.incubator.concurrent`

**Enable virtual threads:**

```bash
java --enable-preview --source 19 MyApp.java
```

## Related Topics

- [Java 18](/en/learn/software-engineering/programming-languages/java/release-highlights/java-18) - Previous release
- [Java 20](/en/learn/software-engineering/programming-languages/java/release-highlights/java-20) - Next release
- [Java 21 LTS](/en/learn/software-engineering/programming-languages/java/release-highlights/java-21-lts) - Virtual Threads become standard
- [Concurrency and Parallelism](/en/learn/software-engineering/programming-languages/java/in-the-field/concurrency-and-parallelism) - In-the-field guide

## References

**Sources:**

- [JEP 425: Virtual Threads (Preview) in JDK 19 | BellSoft](https://bell-sw.com/announcements/2022/06/17/jep-425-virtual-threads-preview/)
- [Embracing Virtual Threads](https://spring.io/blog/2022/10/11/embracing-virtual-threads/)
- [Java 19 Delivers Features for Projects Loom, Panama and Amber - InfoQ](https://www.infoq.com/news/2022/09/java19-released/)
- [Java 19 Features (with Examples)](https://www.happycoders.eu/java/java-19-features/)

**Official OpenJDK JEPs:**

- [JEP 425: Virtual Threads (Preview)](https://openjdk.org/jeps/425)
- [JEP 428: Structured Concurrency (Incubator)](https://openjdk.org/jeps/428)
- [JEP 405: Record Patterns (Preview)](https://openjdk.org/jeps/405)
- [JEP 427: Pattern Matching for switch (Third Preview)](https://openjdk.org/jeps/427)
- [JEP 424: Foreign Function & Memory API (Preview)](https://openjdk.org/jeps/424)
