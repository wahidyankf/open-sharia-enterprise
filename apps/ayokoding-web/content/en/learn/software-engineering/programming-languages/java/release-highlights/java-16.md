---
title: "Java 16 (March 2021)"
date: 2026-02-04T00:00:00+07:00
draft: false
description: "Java 16 release highlights - Pattern Matching and Records become standard, Packaging Tool final, Sealed Classes second preview, and significant GC improvements"
weight: 1000003
tags: ["java", "java-16", "records", "pattern-matching", "sealed-classes", "jpackage"]
---

## Release Information

- **Release Date**: March 16, 2021
- **Support Type**: Non-LTS (6-month support)
- **End of Support**: September 2021 (superseded by Java 17 LTS)
- **JEPs**: 17 enhancements

## Language Features

### Pattern Matching for instanceof (JEP 394) - **Standard**

After two preview rounds, Pattern Matching became a **standard feature**.

**Example:**

```java
// Concise type checking and casting
if (obj instanceof String s) {
    System.out.println("Length: " + s.length());
}

// Works with negation
if (!(obj instanceof String s)) {
    throw new IllegalArgumentException("Expected String");
}
// s not in scope here

// Combines with && operator
if (obj instanceof String s && s.length() > 10) {
    System.out.println(s.toUpperCase());
}
```

**Real-world example:**

```java
public double getPerimeter(Shape shape) {
    if (shape instanceof Circle c) {
        return 2 * Math.PI * c.radius();
    } else if (shape instanceof Rectangle r) {
        return 2 * (r.width() + r.height());
    } else if (shape instanceof Triangle t) {
        return t.side1() + t.side2() + t.side3();
    }
    throw new IllegalArgumentException("Unknown shape");
}
```

**Benefits**:

- Eliminates 90% of explicit casts in typical codebases
- Reduces boilerplate code
- Compile-time type safety

### Records (JEP 395) - **Standard**

Records became a **standard feature** after two preview rounds.

**Example:**

```java
public record Point(int x, int y) {}

// Auto-generated:
// - Constructor: Point(int x, int y)
// - Accessors: x(), y()
// - equals(), hashCode(), toString()
```

**Compact constructor:**

```java
public record Range(int min, int max) {
    // Compact constructor for validation
    public Range {
        if (min > max) {
            throw new IllegalArgumentException("min must be <= max");
        }
    }
}
```

**Record with custom methods:**

```java
public record Point(int x, int y) {
    public double distanceFromOrigin() {
        return Math.sqrt(x * x + y * y);
    }

    public Point translate(int dx, int dy) {
        return new Point(x + dx, y + dy);
    }
}
```

**Use cases**:

- DTOs (Data Transfer Objects)
- API responses/requests
- Configuration classes
- Domain value objects
- Map entries, tuples

### Sealed Classes (JEP 397) - **Second Preview**

**Improvements from first preview**:

- Narrowing primitive conversions in patterns
- Better integration with pattern matching

**Example:**

```java
public sealed interface Expression
    permits Value, Addition, Multiplication {
}

public record Value(int value) implements Expression {}
public record Addition(Expression left, Expression right) implements Expression {}
public record Multiplication(Expression left, Expression right) implements Expression {}

// Pattern matching with sealed types (preview)
public int eval(Expression expr) {
    return switch (expr) {
        case Value v -> v.value();
        case Addition a -> eval(a.left()) + eval(a.right());
        case Multiplication m -> eval(m.left()) * eval(m.right());
        // No default needed - compiler knows all cases covered!
    };
}
```

## Tool and API Features

### Packaging Tool (JEP 392) - **Standard**

`jpackage` became a **standard tool** (was incubator in Java 14-15).

**Create platform-specific installers:**

```bash
# Windows installer
jpackage --input target/ \
         --name MyApp \
         --main-jar myapp.jar \
         --main-class com.example.Main \
         --type msi \
         --icon myicon.ico \
         --win-menu \
         --win-shortcut

# macOS app bundle
jpackage --input target/ \
         --name MyApp \
         --main-jar myapp.jar \
         --type dmg \
         --icon myicon.icns

# Linux package
jpackage --input target/ \
         --name myapp \
         --main-jar myapp.jar \
         --type deb
```

**Benefits**:

- Native installers with bundled JRE
- No separate Java installation required
- Professional deployment experience
- Code signing support

**When to use**:

- Desktop applications
- End-user software distribution
- Enterprise software deployment

### Vector API (JEP 338) - **Incubator**

SIMD (Single Instruction, Multiple Data) operations for high-performance computing.

**Example:**

```java
import jdk.incubator.vector.*;

static final VectorSpecies<Float> SPECIES = FloatVector.SPECIES_PREFERRED;

void vectorComputation(float[] a, float[] b, float[] c) {
    int i = 0;
    int upperBound = SPECIES.loopBound(a.length);

    for (; i < upperBound; i += SPECIES.length()) {
        // Vectorized operations
        FloatVector va = FloatVector.fromArray(SPECIES, a, i);
        FloatVector vb = FloatVector.fromArray(SPECIES, b, i);
        FloatVector vc = va.mul(vb);
        vc.intoArray(c, i);
    }

    // Scalar tail
    for (; i < a.length; i++) {
        c[i] = a[i] * b[i];
    }
}
```

**Performance**: 4-8x faster than scalar code on modern CPUs

**Use cases**: Machine learning, scientific computing, image processing, cryptography

## Incubator and Preview Features

### Foreign Linker API (JEP 389) - **Incubator**

Call native libraries without JNI.

**Example:**

```java
import jdk.incubator.foreign.*;

CLinker linker = CLinker.getInstance();
SymbolLookup stdlib = CLinker.systemLookup();

// Call strlen from C standard library
MethodHandle strlen = linker.downcallHandle(
    stdlib.lookup("strlen").get(),
    MethodType.methodType(long.class, MemoryAddress.class),
    FunctionDescriptor.of(C_LONG, C_POINTER)
);

try (MemorySegment str = CLinker.toCString("Hello", ResourceScope.newConfinedScope())) {
    long len = (long) strlen.invoke(str.address());
    System.out.println("Length: " + len); // 5
}
```

**Benefits over JNI**:

- 10x less code
- 4-5x better performance
- Type-safe API
- Better error messages

### Foreign-Memory Access API (JEP 393) - **Third Incubator**

Safe and efficient off-heap memory access.

**Example:**

```java
import jdk.incubator.foreign.*;

try (ResourceScope scope = ResourceScope.newConfinedScope()) {
    MemorySegment segment = MemorySegment.allocateNative(1024, scope);

    // Write data
    MemoryAccess.setIntAtOffset(segment, 0, 42);
    MemoryAccess.setDoubleAtOffset(segment, 4, 3.14);

    // Read data
    int intValue = MemoryAccess.getIntAtOffset(segment, 0);
    double doubleValue = MemoryAccess.getDoubleAtOffset(segment, 4);
}
// Memory automatically freed when scope closes
```

## Garbage Collection Improvements

### ZGC: Concurrent Thread-Stack Processing (JEP 376)

Moved ZGC thread-stack processing from safepoints to concurrent phase.

**Impact**: Sub-millisecond pause times even more consistent

**Performance gain**: Reduces pause times by up to 50% in some workloads

### Elastic Metaspace (JEP 387)

Returns unused class metadata (metaspace) memory to OS more promptly.

**Before**: Metaspace memory mostly retained until JVM shutdown

**After**: Unused metaspace returned to OS, reducing memory footprint

**Benefit**: Lower memory usage for applications with dynamic class loading

**Impact**: Particularly beneficial for:

- Containers with memory limits
- Applications using OSGi or plugin architectures
- Long-running applications with class churn

## JVM and Runtime Improvements

### Enable C++14 Language Features (JEP 347)

OpenJDK source code can now use C++14 features.

**Impact**: Internal improvement, enables better JDK implementation

**User benefit**: More maintainable JVM code, foundation for future features

### Migrate from Mercurial to Git (JEP 357)

OpenJDK source code migrated from Mercurial to Git.

**New repository**: <https://github.com/openjdk/>

**Impact**: Better tooling, easier contributions, industry-standard workflow

### Migrate to GitHub (JEP 369)

OpenJDK source code hosted on GitHub.

**Benefits**:

- Easier for contributors
- Better CI/CD integration
- Familiar platform for developers

## Port to Alpine Linux (JEP 386) and Windows/AArch64 (JEP 388)

**Alpine Linux**: Lightweight Linux distribution using musl libc

**Windows/AArch64**: Support for ARM64 Windows (Microsoft Surface, AWS Graviton)

**Benefit**: Broader platform support, smaller Docker images with Alpine

## Warnings and Encapsulation

### Strongly Encapsulate JDK Internals (JEP 396)

Internal APIs (`sun.*`, `com.sun.*`) strongly encapsulated by default.

**Breaking change**: Code using internal APIs will fail

**Migration**: Use standard APIs or `--illegal-access=permit` temporarily

**Example affected code:**

```java
// ❌ FAILS in Java 16+
import sun.misc.Unsafe;
Unsafe unsafe = Unsafe.getUnsafe();

// ✅ Use standard alternatives
// - java.lang.invoke.MethodHandles for reflection
// - java.lang.invoke.VarHandle for low-level operations
// - Foreign-Memory Access API for off-heap memory
```

**Check your code:**

```bash
java --illegal-access=warn -jar myapp.jar
# Shows warnings for illegal access
```

## Migration Considerations

**Upgrading from Java 11 LTS:**

1. **Internal API usage**: Review warnings from `--illegal-access=warn`
2. **Records and Pattern Matching**: Refactor verbose code to use new features
3. **Packaging**: Consider jpackage for desktop app distribution
4. **Platform support**: Take advantage of Alpine Linux for smaller containers
5. **GC**: Evaluate ZGC improvements for low-latency requirements

**Compatibility**: Mostly binary compatible, but internal API encapsulation may break code

## Related Topics

- [Java 15](/en/learn/software-engineering/programming-languages/java/release-highlights/java-15) - Previous release
- [Java 17 LTS](/en/learn/software-engineering/programming-languages/java/release-highlights/java-17) - Next release (LTS)
- [Records](/en/learn/software-engineering/programming-languages/java/by-example/intermediate/records) - By-example tutorial
- [Pattern Matching](/en/learn/software-engineering/programming-languages/java/in-practice/functional-programming) - In-practice guide

## References

**Sources:**

- [Java 16 Features (with Examples)](https://www.happycoders.eu/java/java-16-features/)
- [Oracle Announces Java 16](https://www.oracle.com/news/announcement/oracle-announces-java-16-031621/)
- [What is new in Java 16 - Mkyong.com](https://mkyong.com/java/what-is-new-in-java-16/)
- [New Features in Java 16 - Java Code Geeks](https://examples.javacodegeeks.com/new-features-in-java-16/)

**Official OpenJDK JEPs:**

- [JEP 394: Pattern Matching for instanceof](https://openjdk.org/jeps/394)
- [JEP 395: Records](https://openjdk.org/jeps/395)
- [JEP 392: Packaging Tool](https://openjdk.org/jeps/392)
- [JEP 397: Sealed Classes (Second Preview)](https://openjdk.org/jeps/397)
- [JEP 387: Elastic Metaspace](https://openjdk.org/jeps/387)
- [JEP 376: ZGC: Concurrent Thread-Stack Processing](https://openjdk.org/jeps/376)
