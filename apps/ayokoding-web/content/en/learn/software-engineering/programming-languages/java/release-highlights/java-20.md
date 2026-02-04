---
title: "Java 20 (March 2023)"
date: 2026-02-04T00:00:00+07:00
draft: false
description: "Java 20 release highlights - Virtual Threads second preview, Scoped Values incubator, Record Patterns second preview, Pattern Matching for switch fourth preview"
weight: 1000005
tags: ["java", "java-20", "virtual-threads", "scoped-values", "record-patterns", "project-loom"]
---

## Release Information

- **Release Date**: March 21, 2023
- **Support Type**: Non-LTS (6-month support)
- **End of Support**: September 2023 (superseded by Java 21 LTS)
- **JEPs**: 7 enhancements

**Focus**: Refinement of Project Loom, Amber, and Panama features

## Preview Features

### Virtual Threads (JEP 436) - **Second Preview**

Refinements to virtual threads based on feedback.

**New features:**

- Better debugging support
- Improved JFR (Java Flight Recorder) events
- Thread-local variable optimizations

**Example (web server with virtual threads):**

```java
import java.net.*;
import java.io.*;
import java.util.concurrent.*;

void startServer() throws IOException {
    try (var executor = Executors.newVirtualThreadPerTaskExecutor();
         var serverSocket = new ServerSocket(8080)) {

        while (true) {
            Socket socket = serverSocket.accept();
            executor.submit(() -> handleClient(socket));
        }
    }
}

void handleClient(Socket socket) {
    try (socket;
         var in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
         var out = new PrintWriter(socket.getOutputStream())) {

        String request = in.readLine();
        // Blocking I/O is fine - virtual thread will park
        String response = processRequest(request);
        out.println(response);
    } catch (IOException e) {
        e.printStackTrace();
    }
}
```

**Performance comparison:**

| Implementation            | Max Concurrent | Memory | Throughput    |
| ------------------------- | -------------- | ------ | ------------- |
| Thread pool (200 threads) | 200            | 400 MB | 200 req/s     |
| Virtual threads           | 1,000,000+     | 100 MB | 10,000+ req/s |

### Record Patterns (JEP 432) - **Second Preview**

Improvements to record deconstruction.

**Enhanced nested patterns:**

```java
record Point(int x, int y) {}
record Circle(Point center, double radius) {}
record Rectangle(Point topLeft, Point bottomRight) {}

sealed interface Shape permits Circle, Rectangle {}

// Nested pattern matching
static void processShape(Shape shape) {
    switch (shape) {
        case Circle(Point(int x, int y), double r) ->
            System.out.printf("Circle at (%d,%d) radius %.2f%n", x, y, r);
        case Rectangle(Point(int x1, int y1), Point(int x2, int y2)) ->
            System.out.printf("Rectangle from (%d,%d) to (%d,%d)%n", x1, y1, x2, y2);
    }
}
```

**Generic record patterns:**

```java
record Box<T>(T value) {}

static <T> void printBox(Box<T> box) {
    if (box instanceof Box(String s)) {
        System.out.println("String box: " + s);
    } else if (box instanceof Box(Integer i)) {
        System.out.println("Integer box: " + i);
    }
}
```

### Pattern Matching for switch (JEP 433) - **Fourth Preview**

Further refinements to switch pattern matching.

**When clauses (guard patterns):**

```java
static String classify(Object obj) {
    return switch (obj) {
        case String s when s.length() < 5 -> "short string";
        case String s when s.length() >= 5 -> "long string";
        case Integer i when i > 0 -> "positive integer";
        case Integer i when i < 0 -> "negative integer";
        case Integer i -> "zero";
        default -> "unknown";
    };
}
```

**Null handling:**

```java
static String describe(Object obj) {
    return switch (obj) {
        case null -> "null value";
        case String s -> "string: " + s;
        case Integer i -> "integer: " + i;
        default -> obj.toString();
    };
}
```

## Incubator Features

### Scoped Values (JEP 429) - **Incubator** 🆕

**Better alternative to thread-local variables** for sharing data across method calls.

**Old way (ThreadLocal):**

```java
// ThreadLocal - mutable, error-prone
private static final ThreadLocal<User> CURRENT_USER = new ThreadLocal<>();

void processRequest(User user) {
    CURRENT_USER.set(user);
    try {
        doWork();
    } finally {
        CURRENT_USER.remove(); // Easy to forget!
    }
}

void doWork() {
    User user = CURRENT_USER.get();
    // Use user
}
```

**New way (ScopedValue):**

```java
import jdk.incubator.concurrent.ScopedValue;

// ScopedValue - immutable, safer
private static final ScopedValue<User> CURRENT_USER = ScopedValue.newInstance();

void processRequest(User user) {
    ScopedValue.where(CURRENT_USER, user)
               .run(() -> doWork());
    // CURRENT_USER automatically cleared after run()
}

void doWork() {
    User user = CURRENT_USER.get();
    // Use user
}
```

**Benefits over ThreadLocal:**

- **Immutable**: Value can't be changed once set
- **Automatic cleanup**: No manual remove() needed
- **Better with virtual threads**: Lower memory overhead
- **Easier to reason about**: Clear scope boundaries

**Nested scopes:**

```java
void handleRequest() {
    ScopedValue.where(REQUEST_ID, generateId())
               .where(USER, authenticateUser())
               .run(() -> {
                   String requestId = REQUEST_ID.get();
                   User user = USER.get();
                   processRequest();
               });
}
```

### Structured Concurrency (JEP 437) - **Second Incubator**

Refinements to structured task management.

**ShutdownOnSuccess pattern:**

```java
import jdk.incubator.concurrent.StructuredTaskScope;

// Return first successful result, cancel others
String fetchFromMultipleSources() throws Exception {
    try (var scope = new StructuredTaskScope.ShutdownOnSuccess<String>()) {
        scope.fork(() -> fetchFromSource1());
        scope.fork(() -> fetchFromSource2());
        scope.fork(() -> fetchFromSource3());

        scope.join();
        return scope.result(); // First successful result
    } // Other tasks automatically cancelled
}
```

**Custom policies:**

```java
class MyScope<T> extends StructuredTaskScope<T> {
    @Override
    protected void handleComplete(Future<T> future) {
        // Custom completion logic
        if (future.state() == Future.State.SUCCESS) {
            // Handle success
        } else if (future.state() == Future.State.FAILED) {
            // Handle failure
        }
    }
}
```

## Foreign Function & Memory API (JEP 434) - **Second Preview**

Continued refinement of native interop.

**Improved API ergonomics:**

```java
import java.lang.foreign.*;

// Load native library
Linker linker = Linker.nativeLinker();
SymbolLookup lib = SymbolLookup.libraryLookup("mylib", Arena.global());

// Call native function
MethodHandle hello = linker.downcallHandle(
    lib.find("say_hello").orElseThrow(),
    FunctionDescriptor.ofVoid(ValueLayout.ADDRESS)
);

try (Arena arena = Arena.openConfined()) {
    MemorySegment message = arena.allocateUtf8String("Hello from Java!");
    hello.invoke(message);
}
```

**Memory layout API:**

```java
// Define C struct layout in Java
StructLayout pointLayout = MemoryLayout.structLayout(
    ValueLayout.JAVA_INT.withName("x"),
    ValueLayout.JAVA_INT.withName("y")
);

// Allocate and access struct
try (Arena arena = Arena.openConfined()) {
    MemorySegment point = arena.allocate(pointLayout);
    point.set(ValueLayout.JAVA_INT, 0, 10);  // x = 10
    point.set(ValueLayout.JAVA_INT, 4, 20);  // y = 20
}
```

## Vector API (JEP 438) - **Fifth Incubator**

Continued SIMD API refinement.

**API improvements:**

- Better lane manipulation
- Improved mask operations
- Enhanced performance

**Example (image processing):**

```java
import jdk.incubator.vector.*;

void brighten(byte[] pixels, byte[] output, float factor) {
    var species = ByteVector.SPECIES_PREFERRED;
    int i = 0;

    for (; i < species.loopBound(pixels.length); i += species.length()) {
        var v = ByteVector.fromArray(species, pixels, i);
        var brightened = v.convertShape(VectorOperators.B2F, FloatVector.SPECIES_256, 0)
                          .mul(factor)
                          .convertShape(VectorOperators.F2B, species, 0);
        brightened.intoArray(output, i);
    }

    // Handle remainder
    for (; i < pixels.length; i++) {
        output[i] = (byte) Math.min(255, pixels[i] * factor);
    }
}
```

## Migration Considerations

**Upgrading from Java 17 LTS:**

1. **Virtual Threads maturity**: More stable for production testing
2. **Scoped Values**: Consider migrating from ThreadLocal
3. **Record Patterns**: Simplify nested data handling
4. **Preview features**: All features require `--enable-preview`

**Enable preview features:**

```bash
java --enable-preview --add-modules jdk.incubator.concurrent --source 20 MyApp.java
```

**Compatibility**: Binary compatible with Java 17

## Related Topics

- [Java 19](/en/learn/software-engineering/programming-languages/java/release-highlights/java-19) - Previous release (Virtual Threads debut)
- [Java 21 LTS](/en/learn/software-engineering/programming-languages/java/release-highlights/java-21) - Next release (LTS with Virtual Threads standard)
- [Concurrency and Parallelism](/en/learn/software-engineering/programming-languages/java/in-practice/concurrency-and-parallelism) - In-practice guide

## References

**Sources:**

- [Java 20 Features (with Examples)](https://www.happycoders.eu/java/java-20-features/)
- [The Arrival of Java 20! – Inside.java](https://inside.java/2023/03/21/the-arrival-of-java-20/)
- [Java 20 Delivers Features for Projects Amber, Loom and Panama - InfoQ](https://www.infoq.com/news/2023/03/java20-released/)
- [New Features in Java 20 | Baeldung](https://www.baeldung.com/java-20-new-features)

**Official OpenJDK JEPs:**

- [JEP 436: Virtual Threads (Second Preview)](https://openjdk.org/jeps/436)
- [JEP 429: Scoped Values (Incubator)](https://openjdk.org/jeps/429)
- [JEP 437: Structured Concurrency (Second Incubator)](https://openjdk.org/jeps/437)
- [JEP 432: Record Patterns (Second Preview)](https://openjdk.org/jeps/432)
- [JEP 433: Pattern Matching for switch (Fourth Preview)](https://openjdk.org/jeps/433)
- [JEP 434: Foreign Function & Memory API (Second Preview)](https://openjdk.org/jeps/434)
