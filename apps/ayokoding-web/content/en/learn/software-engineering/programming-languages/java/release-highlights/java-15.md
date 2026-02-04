---
title: "Java 15"
date: 2026-02-04T00:00:00+07:00
draft: false
description: "Java 15 release highlights - Text Blocks standard, Sealed Classes preview, ZGC and Shenandoah production-ready, Hidden Classes, and EdDSA cryptography"
weight: 1000002
tags: ["java", "java-15", "text-blocks", "sealed-classes", "zgc", "shenandoah"]
---

## Release Information

- **Release Date**: September 15, 2020
- **Support Type**: Non-LTS (6-month support)
- **End of Support**: March 2021 (superseded by Java 16)
- **JEPs**: 14 enhancements

## Language Features

### Text Blocks (JEP 378) - **Standard**

Multi-line string literals became a **standard feature** after three preview rounds.

**Example:**

```java
String json = """
    {
        "name": "Java 15",
        "type": "Programming Language",
        "features": ["Text Blocks", "Sealed Classes"]
    }
    """;
```

**New in final release**: Text block transformations with `String::formatted` and `String::stripIndent`

### Sealed Classes (JEP 360) - **Preview**

Restrict which classes can extend or implement a type.

**Example:**

```java
public sealed class Shape
    permits Circle, Rectangle, Triangle {
}

final class Circle extends Shape {
    double radius;
}

final class Rectangle extends Shape {
    double width, height;
}

final class Triangle extends Shape {
    double base, height;
}
```

**Use cases**:

- Domain modeling (closed set of possible types)
- Pattern matching exhaustiveness
- Library API design

**Permitted subclasses must be**:

- `final` (no further subclasses)
- `sealed` (restrict its own subclasses)
- `non-sealed` (open for extension)

### Pattern Matching for instanceof (JEP 375) - **Second Preview**

**Improvements from first preview**:

- Pattern variable scope refinement
- Compatibility with `&&` operator

**Example:**

```java
if (obj instanceof String s && s.length() > 5) {
    System.out.println(s.toUpperCase());
}
```

### Records (JEP 384) - **Second Preview**

**Improvements from first preview**:

- Records can now be local (declared inside methods)
- Support for annotations on record components

**Local record example:**

```java
public List<Point> filterPoints(List<Coordinate> coords) {
    record Point(int x, int y) {} // Local record

    return coords.stream()
        .map(c -> new Point(c.x(), c.y()))
        .toList();
}
```

## Garbage Collection Features

### ZGC (JEP 377) - **Production Ready**

Z Garbage Collector no longer experimental (was experimental since Java 11).

**Characteristics**:

- Pause times <10ms regardless of heap size
- Handles heaps from 8MB to 16TB
- Concurrent (minimal stop-the-world pauses)

**How to enable:**

```bash
java -XX:+UseZGC -Xmx16g MyApp
# No longer requires -XX:+UnlockExperimentalVMOptions
```

**When to use ZGC**:

- Applications requiring low latency (trading platforms, gaming servers)
- Large heap sizes (multi-GB to TB range)
- Predictable pause times more important than throughput

### Shenandoah GC (JEP 379) - **Production Ready**

Shenandoah became production-ready (was experimental since Java 12).

**Characteristics**:

- Ultra-low pause times (independent of heap size)
- Concurrent evacuation and compaction
- Lower memory overhead than ZGC

**How to enable:**

```bash
java -XX:+UseShenandoahGC -Xmx8g MyApp
```

**ZGC vs Shenandoah**:

| Feature             | ZGC           | Shenandoah                       |
| ------------------- | ------------- | -------------------------------- |
| **Max heap**        | 16TB          | 128GB (practical limit)          |
| **Pause times**     | <10ms         | <10ms                            |
| **CPU overhead**    | Lower         | Higher                           |
| **Memory overhead** | Higher        | Lower                            |
| **Best for**        | Massive heaps | Medium heaps, predictable pauses |

## Security Features

### Edwards-Curve Digital Signature Algorithm (JEP 339)

Implements EdDSA cryptographic signatures (RFC 8032).

**Example:**

```java
import java.security.*;

// Generate Ed25519 key pair
KeyPairGenerator kpg = KeyPairGenerator.getInstance("Ed25519");
KeyPair kp = kpg.generateKeyPair();

// Sign message
Signature sig = Signature.getInstance("Ed25519");
sig.initSign(kp.getPrivate());
sig.update("Hello, World!".getBytes());
byte[] signature = sig.sign();

// Verify signature
sig.initVerify(kp.getPublic());
sig.update("Hello, World!".getBytes());
boolean valid = sig.verify(signature);
```

**Benefits**:

- Faster than RSA and ECDSA
- Smaller keys and signatures
- Resistant to timing attacks

**Use cases**: TLS 1.3, SSH, blockchain, certificate signing

## JVM Features

### Hidden Classes (JEP 371)

Framework-generated classes invisible to reflection and class loaders.

**Purpose**: Improve framework efficiency (Spring, Hibernate, bytecode generation libraries)

**Characteristics**:

- Cannot be discovered by Class.forName() or reflection
- Automatically garbage collected when unreachable
- Cannot be extended or used as superclass

**Use case**: Dynamic proxy classes, lambda expressions implementation

**Example (framework internal use):**

```java
import java.lang.invoke.MethodHandles;

MethodHandles.Lookup lookup = MethodHandles.lookup();
Class<?> hiddenClass = lookup.defineHiddenClass(
    classBytes,
    true, // Initialize
    MethodHandles.Lookup.ClassOption.NESTMATE
).lookupClass();
```

## Removals and Deprecations

### Remove Nashorn JavaScript Engine (JEP 372)

Nashorn JavaScript engine removed (deprecated in Java 11).

**Reason**: ECMAScript rapid evolution, maintenance burden

**Alternative**: GraalVM JavaScript engine

**Migration:**

```bash
# Before (Java 8-14)
jjs script.js

# After (GraalVM)
# Use GraalVM's js or integrate via GraalVM SDK
```

### Disable and Deprecate Biased Locking (JEP 374)

Biased locking disabled by default (will be removed in future release).

**What is biased locking**: Optimization for uncontended locks

**Why disabled**: Complex implementation, minimal benefit on modern hardware

**Performance impact**: Negligible on most workloads

### Remove Solaris and SPARC Ports (JEP 381)

Support for Solaris and SPARC platforms removed.

**Reason**: Minimal usage, high maintenance cost

**Affected**: Only users running Java on Solaris/SPARC hardware

### Deprecate RMI Activation for Removal (JEP 385)

RMI Activation marked for future removal.

**Reason**: Obsolete technology, rare usage

**Alternative**: Modern alternatives (REST, gRPC, messaging)

## Other Notable Features

### Reimpl

ement the Legacy DatagramSocket API (JEP 373)

Improved maintainability and performance of UDP sockets.

**User-visible changes**: None (internal implementation only)

**Benefits**: Easier to maintain with Project Loom (virtual threads)

### Foreign-Memory Access API (JEP 383) - **Second Incubator**

Continued incubation with improvements:

- Better safety checks
- Improved API usability
- Performance optimizations

**Example:**

```java
import jdk.incubator.foreign.*;

try (MemorySegment segment = MemorySegment.allocateNative(100)) {
    MemoryAccess.setByteAtOffset(segment, 0, (byte) 127);
    byte value = MemoryAccess.getByteAtOffset(segment, 0);
}
```

## Migration Considerations

**Upgrading from Java 11 LTS:**

1. **Nashorn removed**: Migrate JavaScript integration to GraalVM
2. **Biased locking disabled**: Review if using `-XX:+UseBiasedLocking` (usually no action needed)
3. **Solaris/SPARC**: Migrate to Linux/x86 if on legacy platforms
4. **Preview features**: Enable with `--enable-preview` for Sealed Classes, Pattern Matching, Records
5. **GC options**: Consider ZGC or Shenandoah for low-latency requirements

**Compatibility**: Binary compatible with Java 11 (no breaking changes in standard APIs)

## Related Topics

- [Java 14](/en/learn/software-engineering/programming-languages/java/release-highlights/java-14) - Previous release
- [Java 16](/en/learn/software-engineering/programming-languages/java/release-highlights/java-16) - Next release
- [Java 17 LTS](/en/learn/software-engineering/programming-languages/java/release-highlights/java-17) - Next LTS release
- [Garbage Collection](/en/learn/software-engineering/programming-languages/java/in-the-field/performance) - In-the-field guide

## References

**Sources:**

- [Java 15 Features | DigitalOcean](https://www.digitalocean.com/community/tutorials/java-15-features)
- [Oracle Announces Java 15](https://www.oracle.com/news/announcement/oracle-announces-java-15-091520/)
- [Java 15 New Features](https://howtodoinjava.com/java15/java-15-new-features/)
- [New Features in Java 15 | Baeldung](https://www.baeldung.com/java-15-new)

**Official OpenJDK JEPs:**

- [JEP 378: Text Blocks](https://openjdk.org/jeps/378)
- [JEP 360: Sealed Classes (Preview)](https://openjdk.org/jeps/360)
- [JEP 377: ZGC: A Scalable Low-Latency Garbage Collector (Production)](https://openjdk.org/jeps/377)
- [JEP 379: Shenandoah: A Low-Pause-Time Garbage Collector (Production)](https://openjdk.org/jeps/379)
- [JEP 371: Hidden Classes](https://openjdk.org/jeps/371)
- [JEP 339: Edwards-Curve Digital Signature Algorithm (EdDSA)](https://openjdk.org/jeps/339)
