---
title: "Java 14 (March 2020)"
date: 2026-02-04T00:00:00+07:00
draft: false
description: "Java 14 release highlights - Switch Expressions standard, Pattern Matching preview, Records preview, Text Blocks second preview, and helpful NullPointerExceptions"
weight: 1000001
tags: ["java", "java-14", "switch-expressions", "pattern-matching", "records", "text-blocks"]
---

## Release Information

- **Release Date**: March 17, 2020
- **Support Type**: Non-LTS (6-month support)
- **End of Support**: September 2020 (superseded by Java 15)
- **JEPs**: 16 enhancements

## Language Features

### Switch Expressions (JEP 361) - **Standard**

Switch expressions became a **standard feature** after two preview rounds (Java 12, 13).

**Before (switch statement):**

```java
int numLetters;
switch (day) {
    case MONDAY:
    case FRIDAY:
    case SUNDAY:
        numLetters = 6;
        break;
    case TUESDAY:
        numLetters = 7;
        break;
    case THURSDAY:
    case SATURDAY:
        numLetters = 8;
        break;
    case WEDNESDAY:
        numLetters = 9;
        break;
    default:
        throw new IllegalArgumentException("Invalid day: " + day);
}
```

**After (switch expression):**

```java
int numLetters = switch (day) {
    case MONDAY, FRIDAY, SUNDAY -> 6;
    case TUESDAY -> 7;
    case THURSDAY, SATURDAY -> 8;
    case WEDNESDAY -> 9;
};
```

**Benefits**:

- Expressions return values (can assign directly)
- No fall-through bugs (no break needed)
- Exhaustiveness checking (compiler ensures all cases handled)
- Arrow syntax (`->`) for cleaner code

### Pattern Matching for instanceof (JEP 305) - **Preview**

Simplifies the common pattern of instanceof test followed by cast.

**Before:**

```java
if (obj instanceof String) {
    String s = (String) obj;
    System.out.println(s.toUpperCase());
}
```

**After:**

```java
if (obj instanceof String s) {
    System.out.println(s.toUpperCase());
}
```

**Benefits**:

- Eliminates redundant cast
- Pattern variable (`s`) scoped to block
- Less boilerplate code

### Records (JEP 359) - **Preview**

Records provide compact syntax for immutable data carrier classes.

**Before (traditional class):**

```java
public final class Point {
    private final int x;
    private final int y;

    public Point(int x, int y) {
        this.x = x;
        this.y = y;
    }

    public int x() { return x; }
    public int y() { return y; }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof Point)) return false;
        Point other = (Point) obj;
        return x == other.x && y == other.y;
    }

    @Override
    public int hashCode() {
        return Objects.hash(x, y);
    }

    @Override
    public String toString() {
        return String.format("Point[x=%d, y=%d]", x, y);
    }
}
```

**After (record):**

```java
public record Point(int x, int y) {}
```

**Auto-generated**:

- Constructor
- Accessors (`x()`, `y()`)
- `equals()`, `hashCode()`, `toString()`
- Final class and fields (immutable)

**Use cases**: DTOs, value objects, API responses, configuration classes

### Text Blocks (JEP 368) - **Second Preview**

Multi-line string literals with automatic formatting.

**Before:**

```java
String html = "<html>\n" +
              "    <body>\n" +
              "        <p>Hello, World!</p>\n" +
              "    </body>\n" +
              "</html>\n";
```

**After:**

```java
String html = """
    <html>
        <body>
            <p>Hello, World!</p>
        </body>
    </html>
    """;
```

**Benefits**:

- Improved readability for JSON, XML, SQL, HTML
- No escape sequences for quotes
- Automatic indentation handling

## JVM and Tool Features

### Helpful NullPointerExceptions (JEP 358)

Improved NPE messages that pinpoint the exact variable that was null.

**Before (Java 13):**

```
Exception in thread "main" java.lang.NullPointerException
    at Main.main(Main.java:5)
```

**After (Java 14):**

```
Exception in thread "main" java.lang.NullPointerException:
    Cannot invoke "String.toUpperCase()" because "user.name" is null
    at Main.main(Main.java:5)
```

**Example code:**

```java
User user = getUser();
String upperName = user.name.toUpperCase(); // NPE here
```

**Message shows**: `"user.name"` is null (not just "user")

**How to enable**: `-XX:+ShowCodeDetailsInExceptionMessages`

### Foreign-Memory Access API (JEP 370) - **Incubator**

Safe and efficient access to off-heap memory (outside Java heap).

**Use cases**:

- Memory-mapped files
- Inter-process communication
- Native library integration

**Example:**

```java
import jdk.incubator.foreign.*;

try (MemorySegment segment = MemorySegment.allocateNative(100)) {
    MemoryAddress base = segment.baseAddress();
    MemoryAccess.setIntAtOffset(segment, 0, 42);
    int value = MemoryAccess.getIntAtOffset(segment, 0);
    System.out.println(value); // 42
}
```

**Benefits over sun.misc.Unsafe**:

- Safe (bounded memory access)
- Automatic resource management
- No security warnings

### Packaging Tool (JEP 343) - **Incubator**

Create platform-specific installers for Java applications.

**Supported formats**:

- **Windows**: msi, exe
- **macOS**: pkg, dmg
- **Linux**: deb, rpm

**Command:**

```bash
jpackage --input target/ \
         --name MyApp \
         --main-jar myapp.jar \
         --main-class com.example.Main \
         --type exe
```

**Output**: Native installer that bundles JRE with application

**Benefits**: End users don't need separate Java installation

### JFR Event Streaming (JEP 349)

Continuous monitoring with JDK Flight Recorder (JFR).

**Before**: JFR data accessible only in dumps (batch processing)

**After**: Stream JFR events in real-time for monitoring

**Example:**

```java
import jdk.jfr.consumer.RecordingStream;

try (RecordingStream rs = new RecordingStream()) {
    rs.onEvent("jdk.CPULoad", event -> {
        System.out.println("CPU Load: " + event.getFloat("machineTotal"));
    });
    rs.start();
}
```

**Use cases**: Real-time dashboards, alerting systems, APM tools

## Garbage Collection Improvements

### NUMA-Aware Memory Allocation for G1 (JEP 345)

Improves G1 GC performance on NUMA (Non-Uniform Memory Access) systems.

**What is NUMA**: Multi-socket servers where memory access cost varies by CPU socket

**Improvement**: G1 now allocates memory local to the requesting thread's CPU

**Performance gain**: Up to 30% throughput improvement on large NUMA systems

**How to enable**: `-XX:+UseNUMA` (with G1GC)

### ZGC on macOS (JEP 364) and Windows (JEP 365)

Z Garbage Collector (ZGC) expanded to macOS and Windows.

**Previously**: Linux only

**Now**: Available on all major platforms

**ZGC characteristics**:

- Pause times <10ms (even with TB-sized heaps)
- Concurrent (doesn't stop application threads)
- Scalable (handles 8MB to 16TB heaps)

**How to enable:**

```bash
java -XX:+UseZGC -Xmx16g MyApp
```

**Use cases**: Low-latency applications, large heap requirements

## Removals and Deprecations

### CMS Garbage Collector Removed (JEP 363)

Concurrent Mark Sweep (CMS) GC removed (deprecated in Java 9).

**Alternatives**:

- **G1 GC** (default since Java 9)
- **ZGC** (for ultra-low latency)
- **Shenandoah** (for low latency)

**Migration**: Remove `-XX:+UseConcMarkSweepGC` flag

### Pack200 Tools and API Removed (JEP 367)

pack200/unpack200 compression tools removed (deprecated in Java 11).

**Reason**: Low adoption, maintenance burden, superseded by jlink

**Alternative**: Use jar with compression or jlink for custom runtimes

## Other Notable Changes

**JEP 343**: Non-Volatile Mapped Byte Buffers - Support for NVM (non-volatile memory)

**JEP 352**: macOS Rendering Pipeline - Migrated from deprecated OpenGL to Apple Metal

**JEP 362**: Deprecate Solaris and SPARC Ports - Rarely used platforms marked for removal

## Migration Considerations

**Upgrading from Java 11 LTS:**

1. **CMS GC removed**: Migrate to G1 GC (usually drop-in replacement)
2. **Pack200 removed**: Update build scripts if used
3. **Preview features**: Enable with `--enable-preview` flag for Records, Text Blocks
4. **Deprecations**: Review code for usage of deprecated APIs

**Compatibility**: Binary compatible with Java 11 (no breaking changes in standard APIs)

## Related Topics

- [Java 15](/en/learn/software-engineering/programming-languages/java/release-highlights/java-15) - Next release
- [Java 17 LTS](/en/learn/software-engineering/programming-languages/java/release-highlights/java-17) - Next LTS release
- [Pattern Matching](/en/learn/software-engineering/programming-languages/java/in-the-field/functional-programming) - In-the-field guide
- [Records](/en/learn/software-engineering/programming-languages/java/by-example/intermediate/records) - By-example tutorial

## References

**Sources:**

- [New Features in Java 14 | Baeldung](https://www.baeldung.com/java-14-new-features)
- [JDK 14 - New Features in Java 14 - Whizlabs Blog](https://www.whizlabs.com/blog/java-14-features/)
- [What is new in Java 14 (for Developers)](https://loiane.com/2020/04/what-is-new-in-java-14-api-for-developers/)
- [Java 14 - New Features and Improvements](https://howtodoinjava.com/java14/java14-new-features/)

**Official OpenJDK JEPs:**

- [JEP 361: Switch Expressions](https://openjdk.org/jeps/361)
- [JEP 305: Pattern Matching for instanceof (Preview)](https://openjdk.org/jeps/305)
- [JEP 359: Records (Preview)](https://openjdk.org/jeps/359)
- [JEP 368: Text Blocks (Second Preview)](https://openjdk.org/jeps/368)
- [JEP 358: Helpful NullPointerExceptions](https://openjdk.org/jeps/358)
- [JEP 370: Foreign-Memory Access API (Incubator)](https://openjdk.org/jeps/370)
