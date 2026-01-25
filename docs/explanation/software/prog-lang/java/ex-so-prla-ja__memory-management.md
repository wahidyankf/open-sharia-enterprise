---
title: "Java Memory Management and JVM Internals"
description: Comprehensive guide to Java's automatic memory management, garbage collection algorithms, JVM memory model, and optimization techniques for enterprise applications (Java 17-25)
category: explanation
subcategory: prog-lang
tags:
  - java
  - memory-management
  - garbage-collection
  - jvm
  - performance
  - heap
  - gc-tuning
  - java-17
  - java-21
  - java-25
  - zgc
  - g1gc
  - profiling
related:
  - ./ex-so-prla-ja__best-practices.md
principles:
  - explicit-over-implicit
last_updated: 2026-01-24
---

# Java Memory Management and JVM Internals

**Quick Reference**: [Overview](#overview) | [JVM Memory Model](#jvm-memory-model) | [Memory Regions](#memory-regions) | [Object Allocation](#object-allocation) | [Garbage Collection Algorithms](#garbage-collection-algorithms) | [GC Tuning](#gc-tuning) | [Memory Leaks](#memory-leaks) | [Memory Optimization](#memory-optimization) | [Profiling Tools](#profiling-tools) | [Best Practices](#best-practices) | [Memory Management Checklist](#memory-management-checklist) | [Related Documentation](#related-documentation) | [Sources](#sources)

## Overview

Java's automatic memory management through garbage collection (GC) is one of its defining features, freeing developers from manual memory allocation and deallocation. However, understanding how the Java Virtual Machine (JVM) manages memory is essential for building high-performance, scalable enterprise applications, particularly those handling sensitive financial workloads like Zakat calculation batches, donation_transaction processing, and bulk donation operations.

Unlike Go's simpler, single-collector approach with limited tuning options, Java offers multiple GC algorithms optimized for different workload characteristics. This flexibility enables fine-tuned performance for specific scenarios—from low-latency trading systems requiring sub-millisecond pause times (ZGC, Shenandoah) to high-throughput batch processing maximizing overall system efficiency (G1GC, Parallel GC).

This document explores Java's memory management system in depth: the JVM memory model, heap structure, object allocation strategies, garbage collection algorithms (G1GC, ZGC, Shenandoah, Generational ZGC), GC tuning techniques, memory leak detection and prevention, optimization strategies, and profiling tools. All examples use modern Java features (Java 17-25) and focus on real-world financial domain scenarios.

**Why memory management matters in finance**:

- **Latency sensitivity**: DonationPayment processing and donation_transaction validation require consistent response times without GC pauses
- **High throughput**: Bulk Zakat calculations across millions of accounts demand efficient memory allocation
- **Data integrity**: Memory leaks can corrupt financial calculations or exhaust resources during month-end processing
- **Predictability**: GC behavior must be predictable for SLA compliance in trading and settlement systems

**Key differences from Go**:

- **Multiple GC algorithms**: Java offers G1GC, ZGC, Shenandoah, Parallel GC, Serial GC (Go has one collector)
- **Extensive tuning**: Hundreds of JVM flags for GC tuning (Go has GOGC, GOMEMLIMIT)
- **Generational approach**: Most Java GCs use generational hypothesis (Go uses concurrent tri-color mark-sweep)
- **Heap regions**: Java's heap is divided into Young/Old generations, regions, or pages depending on GC
- **Explicit control**: Java provides finer control over memory management (Go optimizes for simplicity)

## Quick Reference

**Jump to:**

- [Overview](#overview) - Introduction and memory management principles
- [JVM Memory Model](#jvm-memory-model) - Memory regions and structure
- [Memory Regions](#memory-regions) - Heap, stack, metaspace, direct buffers
- [Object Allocation](#object-allocation) - TLAB, Eden space, promotion, escape analysis
- [Garbage Collection Algorithms](#garbage-collection-algorithms) - G1GC, ZGC, Shenandoah, Generational ZGC
- [GC Tuning](#gc-tuning) - JVM flags, heap sizing, financial workload optimization
- [Memory Leaks](#memory-leaks) - Detection, prevention, financial examples
- [Memory Optimization](#memory-optimization) - Object pooling, compact headers, value types
- [Profiling Tools](#profiling-tools) - JFR, jmap, VisualVM, async-profiler
- [Best Practices](#best-practices) - Hot path optimization, immutable reuse
- [Memory Management Checklist](#memory-management-checklist) - Quality verification
- [Related Documentation](#related-documentation) - Cross-references

**Related Documentation:**

- [Java Performance](./ex-so-prla-ja__performance.md) - Performance optimization including memory profiling
- [Java Concurrency](./ex-so-prla-ja__concurrency-and-parallelism.md) - Thread memory considerations
- [Java Best Practices](./ex-so-prla-ja__best-practices.md) - General best practices including memory efficiency
- [Java Anti-Patterns](./ex-so-prla-ja__anti-patterns.md) - Common memory-related mistakes
- [Go Memory Management](../golang/ex-so-prla-go__memory-management.md) - Comparison with Go's simpler GC

This document implements the following [software engineering principles](../../../../../governance/principles/software-engineering/README.md):

| Principle                                                                                                     | Application in Memory Management                                       |
| ------------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------- |
| [Explicit Over Implicit](../../../../../governance/principles/software-engineering/explicit-over-implicit.md) | Explicit GC tuning flags, heap sizing, allocation strategies           |
| [Simplicity Over Complexity](../../../../../governance/principles/general/simplicity-over-complexity.md)      | Start with default GC, tune only when profiling shows bottlenecks      |
| [Automation Over Manual](../../../../../governance/principles/software-engineering/automation-over-manual.md) | Automatic garbage collection, escape analysis, allocation optimization |

## JVM Memory Model

### Memory Architecture Overview

The JVM divides memory into several distinct regions, each serving specific purposes in the runtime environment. Understanding this structure is crucial for diagnosing memory issues and optimizing performance.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161, Gray #808080
%% All colors are color-blind friendly and meet WCAG AA contrast standards

graph TD
    JVM["JVM Memory Space"]:::blue
    Heap["Heap<br/>#40;Shared#41;"]:::teal
    NonHeap["Non-Heap<br/>#40;Shared#41;"]:::orange
    Thread["Thread-Local<br/>#40;Per-Thread#41;"]:::purple

    JVM --> Heap
    JVM --> NonHeap
    JVM --> Thread

    Heap --> Young["Young Generation<br/>Eden + Survivor"]:::teal
    Heap --> Old["Old Generation<br/>Tenured Objects"]:::teal
    Heap --> Humongous["Humongous<br/>Large Objects"]:::teal

    NonHeap --> Metaspace["Metaspace<br/>Class Metadata"]:::orange
    NonHeap --> CodeCache["Code Cache<br/>JIT Compiled Code"]:::orange
    NonHeap --> DirectMem["Direct Buffers<br/>Off-Heap NIO"]:::orange

    Thread --> Stack["Thread Stack<br/>Local Variables"]:::purple
    Thread --> PC["Program Counter<br/>Current Instruction"]:::purple
    Thread --> Native["Native Stack<br/>JNI Calls"]:::purple

    classDef blue fill:#0173B2,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef orange fill:#DE8F05,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef teal fill:#029E73,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef purple fill:#CC78BC,stroke:#000000,color:#FFFFFF,stroke-width:2px
```

**Memory regions summary**:

- **Heap**: Shared object storage, managed by GC, largest memory region
- **Non-Heap**: Shared metadata and code, not managed by object GC
- **Thread-Local**: Per-thread execution state, stack-allocated

### Memory Visibility and Sharing

**Heap Memory (Shared)**:

- All objects created with `new` keyword
- Accessible by all threads (requires synchronization)
- Managed by garbage collector
- Size controlled by `-Xms` (initial) and `-Xmx` (maximum)

**Stack Memory (Thread-Local)**:

- Method call frames, local variables, method parameters
- Each thread has its own stack (default 1MB per thread)
- Not shared between threads (no synchronization needed)
- Size controlled by `-Xss` flag

**Metaspace (Shared, Non-Heap)**:

- Class metadata, method definitions, constants
- Replaces PermGen (removed in Java 8)
- Native memory allocation (outside heap)
- Size controlled by `-XX:MetaspaceSize` and `-XX:MaxMetaspaceSize`

**Code Cache (Shared, Non-Heap)**:

- JIT-compiled native code
- Hotspot optimizations
- Size controlled by `-XX:ReservedCodeCacheSize`

## Memory Regions

### Heap Structure (G1GC Example)

The heap is divided into regions depending on the GC algorithm. G1GC (default since Java 9) uses a region-based approach.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC

graph TD
    Heap["G1 Heap<br/>#40;Divided into Equal Regions#41;"]:::blue

    Eden["Eden Regions<br/>New Objects"]:::teal
    Survivor["Survivor Regions<br/>Minor GC Survivors"]:::teal
    Old["Old Regions<br/>Long-Lived Objects"]:::orange
    Humongous["Humongous Regions<br/>Objects #62; 50#37; Region Size"]:::purple

    Heap --> Eden
    Heap --> Survivor
    Heap --> Old
    Heap --> Humongous

    Eden -->|Minor GC| Survivor
    Survivor -->|Aging| Old
    Eden -->|Large Objects| Humongous

    classDef blue fill:#0173B2,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef orange fill:#DE8F05,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef teal fill:#029E73,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef purple fill:#CC78BC,stroke:#000000,color:#FFFFFF,stroke-width:2px
```

**Region size**: Automatically calculated (1-32MB) based on heap size, or set with `-XX:G1HeapRegionSize`.

**Eden Regions**:

- New object allocation target
- Fastest allocation path (TLAB within Eden)
- Cleared during Minor GC
- Size: ~60% of Young generation

**Survivor Regions**:

- Hold objects that survived Minor GC
- Two survivor spaces (From/To) swap roles
- Objects age here before promotion
- Size: ~2.5% of Young generation

**Old Generation Regions**:

- Objects that survived multiple Minor GCs
- Cleared during Major/Full GC
- Size: ~60-70% of total heap

**Humongous Regions**:

- Objects larger than 50% of region size
- Allocated directly to Old generation
- Can span multiple contiguous regions
- Expensive to allocate and collect

### Stack Memory

Each thread has its own stack storing method call frames.

**Stack Frame Components**:

- **Local variables**: Primitive values, object references
- **Operand stack**: Temporary storage for computation
- **Frame data**: Method metadata, return address

```java
public void processPayment(BigDecimal amount) {
  // Stack frame created for processPayment()

  // Local variables on stack:
  // - amount (reference to BigDecimal object on heap)
  // - fee (reference to BigDecimal object on heap)
  // - total (reference to BigDecimal object on heap)

  BigDecimal fee = amount.multiply(new BigDecimal("0.01")); // 1% fee
  BigDecimal total = amount.add(fee);

  // Stack frame destroyed when method returns
}
```

**Stack allocation characteristics**:

- **Fast**: Push/pop operations (LIFO structure)
- **Automatic**: No GC needed, deallocated when method returns
- **Thread-safe**: Each thread has its own stack
- **Limited size**: Default 1MB per thread (`-Xss1m`)

**StackOverflowError**:

```java
// Infinite recursion exhausts stack space
public void calculateCompoundInterest(int years) {
  if (years > 0) {
    calculateCompoundInterest(years + 1); // Wrong: should be years - 1
  }
}

// Exception in thread "main" java.lang.StackOverflowError
```

### Metaspace

Metaspace stores class metadata in native memory (outside heap).

**Metaspace contents**:

- Class structure definitions
- Method bytecode
- Constants pool
- Field and method metadata

**Why Metaspace replaced PermGen** (Java 8+):

- PermGen was fixed-size, causing `OutOfMemoryError: PermGen space`
- Metaspace grows dynamically up to available native memory
- Better support for dynamic class loading/unloading
- No more "PermGen tuning" complexity

**Metaspace tuning flags**:

```bash
# Initial metaspace size (triggers GC when exceeded)
-XX:MetaspaceSize=128m

# Maximum metaspace size (prevents unbounded growth)
-XX:MaxMetaspaceSize=512m

# Trigger GC at specific metaspace usage
-XX:MinMetaspaceFreeRatio=40
-XX:MaxMetaspaceFreeRatio=70
```

**Metaspace memory leak example** (Spring Boot application with class reloading):

```java
// Problem: ClassLoader leaks during hot reload
public class DevelopmentServer {
  public void reloadApplication() {
    // New ClassLoader created for each reload
    ClassLoader classLoader = new URLClassLoader(urls);
    Class<?> clazz = classLoader.loadClass("com.sharia.finance.Application");

    // If old ClassLoader isn't garbage collected, metaspace grows
  }
}
```

**Fix**: Ensure ClassLoaders are properly released when no longer needed.

### Direct Buffers (Off-Heap Memory)

Direct buffers allocate memory outside the heap using native memory.

**Use cases**:

- NIO file/network operations
- High-throughput I/O avoiding heap copies
- Interop with native code (JNI)

**Direct buffer allocation**:

```java
import java.nio.ByteBuffer;

// Allocate 1MB direct buffer (off-heap)
ByteBuffer directBuffer = ByteBuffer.allocateDirect(1024 * 1024);

// Allocate 1MB heap buffer (on-heap)
ByteBuffer heapBuffer = ByteBuffer.allocate(1024 * 1024);
```

**Direct buffer characteristics**:

- **Faster I/O**: No copy between heap and native OS buffers
- **GC pressure**: Not in heap, reduces GC overhead
- **Limited size**: Controlled by `-XX:MaxDirectMemorySize`
- **Manual cleanup**: Must be explicitly deallocated or wait for GC

**Tuning direct buffer size**:

```bash
# Limit direct buffer memory to 512MB
-XX:MaxDirectMemorySize=512m
```

**Direct buffer memory leak example**:

```java
// Problem: Direct buffers not released
public class FileProcessor {
  public void processLargeFile(Path file) throws IOException {
    ByteBuffer buffer = ByteBuffer.allocateDirect(10 * 1024 * 1024); // 10MB

    try (FileChannel channel = FileChannel.open(file, StandardOpenOption.READ)) {
      channel.read(buffer);
      // Process buffer...
    }

    // Direct buffer not explicitly released
    // Relies on finalizer, which may not run promptly
  }
}

// Fix: Use sun.misc.Cleaner or wait for Java 14+ foreign memory API
```

**Modern approach (Java 14+)**:

```java
import jdk.incubator.foreign.*;

// Foreign Memory Access API (preview in Java 14-16, stable in Java 17+)
try (MemorySegment segment = MemorySegment.allocateNative(1024)) {
  // Use memory segment...
} // Automatically released when try block exits
```

## Object Allocation

### Allocation Path

Object allocation follows an optimized path designed for speed and efficiency.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Gray #808080

graph TD
    New["new Object#40;#41;"]:::blue
    Escape{"Escape<br/>Analysis"}:::orange
    Stack["Stack<br/>Allocation"]:::teal
    TLAB{"TLAB<br/>Available?"}:::orange
    TLABAlloc["Allocate in TLAB<br/>#40;Fast, Thread-Local#41;"]:::teal
    Eden{"Eden<br/>Available?"}:::orange
    EdenAlloc["Allocate in Eden<br/>#40;Synchronized#41;"]:::teal
    Large{"Large<br/>Object?"}:::orange
    Humongous["Allocate in<br/>Humongous Region"]:::purple
    OldGen["Allocate in<br/>Old Generation"]:::gray

    New --> Escape
    Escape -->|Doesn't Escape| Stack
    Escape -->|Escapes| TLAB
    TLAB -->|Yes| TLABAlloc
    TLAB -->|No| Eden
    Eden -->|Yes| EdenAlloc
    Eden -->|No| Large
    Large -->|Yes| Humongous
    Large -->|No| OldGen

    classDef blue fill:#0173B2,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef orange fill:#DE8F05,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef teal fill:#029E73,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef purple fill:#CC78BC,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef gray fill:#808080,stroke:#000000,color:#FFFFFF,stroke-width:2px
```

**Allocation priority** (fastest to slowest):

1. **Stack allocation**: Object doesn't escape method (JIT optimization)
2. **TLAB allocation**: Thread-local allocation buffer in Eden (no synchronization)
3. **Eden allocation**: Shared Eden space (requires synchronization)
4. **Humongous allocation**: Large objects (>50% region size) to dedicated regions
5. **Old generation allocation**: Fallback when Eden full or object already tenured

### Thread-Local Allocation Buffers (TLAB)

TLAB is a per-thread allocation area within Eden, enabling fast, lock-free object allocation.

**Why TLAB matters**:

- **Performance**: Avoids synchronization overhead on allocation
- **Cache locality**: Thread's objects allocated in contiguous memory
- **Scalability**: Eliminates contention in multi-threaded environments

**TLAB mechanics**:

```java
// Thread-local allocation (fast path)
public void processTransaction(DonationTransaction tx) {
  // Each thread allocates in its own TLAB
  TransactionResult result = new TransactionResult(); // TLAB allocation
  Money fee = tx.getAmount().multiply(0.01); // TLAB allocation

  // No synchronization needed for allocation
}
```

**TLAB tuning flags**:

```bash
# Enable TLAB (default: enabled)
-XX:+UseTLAB

# TLAB size as percentage of Eden
-XX:TLABSize=256k

# Minimum TLAB size
-XX:MinTLABSize=2k

# TLAB refill waste limit
-XX:TLABWasteTargetPercent=1
```

**TLAB allocation benefits for financial workloads**:

```java
// Bulk Zakat calculation (1 million accounts)
public void calculateZakatBulk(List<Account> accounts) {
  ExecutorService executor = Executors.newFixedThreadPool(16);

  accounts.forEach(donation_account -> executor.submit(() -> {
    // Each thread allocates in its own TLAB
    Money zakatable = donation_account.getZakatableWealth(); // TLAB
    Money nisab = getNisab(); // TLAB
    Money zakat = zakatable.multiply(0.025); // TLAB

    // Minimal contention on allocation
  }));
}
```

**Before TLAB** (synchronized allocation):

- Allocation throughput: ~10M objects/sec
- Contention: High on shared Eden lock

**After TLAB** (lock-free allocation):

- Allocation throughput: ~100M objects/sec (10x improvement)
- Contention: Minimal (thread-local)

### Escape Analysis

Escape analysis determines if an object's lifetime is confined to a method, enabling stack allocation.

**What is escape analysis?**

The JIT compiler analyzes object lifetimes to decide allocation location:

- **Stack allocation**: Object doesn't escape method (faster, no GC)
- **Heap allocation**: Object escapes method scope (slower, GC overhead)

**Escape scenarios**:

```java
// 1. Object doesn't escape (stack allocation possible)
public Money calculateFee(Money amount) {
  BigDecimal rate = new BigDecimal("0.01"); // Doesn't escape
  return amount.multiply(rate);
}

// 2. Object escapes via return (heap allocation)
public Money createFee(Money amount) {
  Money fee = amount.multiply(new BigDecimal("0.01"));
  return fee; // 'fee' escapes to caller
}

// 3. Object escapes via field assignment (heap allocation)
private Money lastFee;

public void recordFee(Money amount) {
  Money fee = amount.multiply(new BigDecimal("0.01"));
  this.lastFee = fee; // 'fee' escapes via field
}

// 4. Object escapes via method parameter (heap allocation)
public void processFee(Money amount, List<Money> fees) {
  Money fee = amount.multiply(new BigDecimal("0.01"));
  fees.add(fee); // 'fee' escapes via collection
}
```

**Enabling escape analysis** (default: enabled):

```bash
# Enable escape analysis (default in Java 8+)
-XX:+DoEscapeAnalysis

# Enable aggressive optimizations
-XX:+EliminateAllocations
-XX:+EliminateLocks
```

**Viewing escape analysis decisions**:

```bash
# Print JIT compilation details including escape analysis
java -XX:+UnlockDiagnosticVMOptions \
     -XX:+PrintEliminateAllocations \
     -XX:+PrintEscapeAnalysis \
     MyApp
```

**Escape analysis benefits**:

```java
// Before optimization (heap allocation)
public BigDecimal calculateTotal(BigDecimal... amounts) {
  BigDecimal total = BigDecimal.ZERO; // Heap allocation
  for (BigDecimal amount : amounts) {
    total = total.add(amount); // New BigDecimal each iteration
  }
  return total;
}

// After escape analysis + scalar replacement
// JIT converts to primitive operations (no allocation)
public BigDecimal calculateTotal(BigDecimal... amounts) {
  // Compiler replaces object with scalar values on stack
  // Equivalent to: long unscaledValue, int scale (primitives)
}
```

**Financial workload optimization** (Zakat calculation):

```java
// Original: Many temporary allocations
public Money calculateZakat(Money wealth) {
  Money nisab = Money.of(85, "g", "gold"); // Escapes
  if (wealth.isGreaterThanOrEqualTo(nisab)) {
    Money rate = Money.of(0.025); // Could be stack-allocated
    return wealth.multiply(rate); // Escapes
  }
  return Money.ZERO;
}

// Optimized: Reduced allocations via escape analysis
public Money calculateZakat(Money wealth) {
  // JIT inlines constants, eliminates intermediate allocations
  return wealth.isGreaterThanOrEqualTo(NISAB_CONSTANT)
    ? wealth.multiply(ZAKAT_RATE_CONSTANT)
    : Money.ZERO;
}
```

**Escape analysis limitations**:

- Only works with JIT compilation (not interpreted code)
- Complex object graphs may prevent optimization
- Polymorphic calls can block analysis
- Requires `-server` mode (default in modern Java)

### Object Promotion

Objects move from Young to Old generation based on age and survival.

**Promotion path**:

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73

graph LR
    Eden["Eden<br/>New Objects"]:::blue
    S0["Survivor 0<br/>Age 1-X"]:::teal
    S1["Survivor 1<br/>Age 1-X"]:::teal
    Old["Old Generation<br/>Tenured"]:::orange

    Eden -->|Minor GC| S0
    S0 -->|Minor GC| S1
    S1 -->|Minor GC| S0
    S0 -->|Age #62; Threshold| Old
    S1 -->|Age #62; Threshold| Old
    Eden -->|Survivor Full| Old

    classDef blue fill:#0173B2,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef orange fill:#DE8F05,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef teal fill:#029E73,stroke:#000000,color:#FFFFFF,stroke-width:2px
```

**Promotion triggers**:

1. **Age nisab**: Object survived MaxTenuringThreshold Minor GCs (default: 15)
2. **Survivor overflow**: Survivor space full, objects promoted prematurely
3. **Large object**: Object too large for Survivor, promoted directly

**Tuning promotion**:

```bash
# Maximum object age before promotion (default: 15)
-XX:MaxTenuringThreshold=15

# Initial survivor ratio (default: 8)
# SurvivorRatio=8 means Eden:Survivor = 8:1
-XX:SurvivorRatio=8

# Target survivor occupancy (default: 50%)
-XX:TargetSurvivorRatio=50
```

**Premature promotion problem**:

```java
// Problem: Short-lived objects promoted to Old generation
public void processBatch(List<DonationTransaction> transactions) {
  for (DonationTransaction tx : transactions) {
    // Temporary objects created each iteration
    TransactionResult result = new TransactionResult();
    Money fee = tx.getAmount().multiply(0.01);
    result.setFee(fee);

    // If Minor GC happens during batch processing,
    // these objects might survive and get promoted
  }
}
```

**Impact**: Old generation fills quickly, triggering expensive Full GCs.

**Solution**: Process smaller batches, increase Young generation size, or use object pooling.

```java
// Solution: Smaller batches reduce promotion
public void processBatch(List<DonationTransaction> transactions) {
  int batchSize = 1000;
  for (int i = 0; i < transactions.size(); i += batchSize) {
    List<DonationTransaction> batch = transactions.subList(
      i,
      Math.min(i + batchSize, transactions.size())
    );
    processBatchInternal(batch);
    // Minor GC between batches, temporary objects collected
  }
}
```

## Garbage Collection Algorithms

Java offers multiple GC algorithms, each optimized for different workload characteristics.

### GC Algorithm Comparison

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC

graph TD
    GCs["Java GC Algorithms"]:::blue

    Serial["Serial GC<br/>Single-threaded"]:::gray
    Parallel["Parallel GC<br/>Throughput-focused"]:::teal
    G1["G1GC<br/>Region-based, Balanced"]:::orange
    ZGC["ZGC<br/>Ultra-low Latency"]:::purple
    Shenandoah["Shenandoah<br/>Low Latency"]:::purple
    GenZGC["Generational ZGC<br/>Java 21+"]:::purple

    GCs --> Serial
    GCs --> Parallel
    GCs --> G1
    GCs --> ZGC
    GCs --> Shenandoah
    GCs --> GenZGC

    Serial --> SerialUse["Small apps<br/>#60; 100MB heap"]:::gray
    Parallel --> ParallelUse["Batch processing<br/>High throughput"]:::teal
    G1 --> G1Use["General purpose<br/>Default Java 9+"]:::orange
    ZGC --> ZGCUse["Latency-critical<br/>#60; 10ms pauses"]:::purple
    Shenandoah --> ShenUse["Low latency<br/>#60; 10ms pauses"]:::purple
    GenZGC --> GenZGCUse["Best of both<br/>Low latency + Throughput"]:::purple

    classDef blue fill:#0173B2,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef orange fill:#DE8F05,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef teal fill:#029E73,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef purple fill:#CC78BC,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef gray fill:#808080,stroke:#000000,color:#FFFFFF,stroke-width:2px
```

**Algorithm selection guide**:

| GC Algorithm     | Pause Time | Throughput | Heap Size | Use Case                       |
| ---------------- | ---------- | ---------- | --------- | ------------------------------ |
| Serial GC        | 10-100ms   | Medium     | < 100MB   | Small apps, single-core        |
| Parallel GC      | 100ms-1s   | Highest    | Any       | Batch processing, offline jobs |
| G1GC             | 10-200ms   | High       | > 4GB     | General purpose, default       |
| ZGC              | < 10ms     | High       | > 8GB     | Latency-critical, trading      |
| Shenandoah       | < 10ms     | High       | > 8GB     | Low latency, real-time         |
| Generational ZGC | < 10ms     | Higher     | > 8GB     | Best of both (Java 21+)        |

### G1GC (Garbage First)

G1GC is the default collector since Java 9, designed for balanced throughput and latency.

**Key characteristics**:

- **Region-based**: Heap divided into equal-sized regions (1-32MB)
- **Generational**: Young and Old generations, but not contiguous
- **Predictable pauses**: Target max pause time (default: 200ms)
- **Concurrent marking**: Marks live objects concurrently with application
- **Incremental compaction**: Compacts high-garbage regions first

**G1GC phases**:

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73

graph TD
    YoungGC["Young-Only GC<br/>#40;Evacuation Pause#41;"]:::blue
    Concurrent["Concurrent Marking"]:::teal
    Mixed["Mixed GC<br/>#40;Young + Old#41;"]:::orange
    Full["Full GC<br/>#40;Fallback, Stop-The-World#41;"]:::gray

    YoungGC -->|Threshold| Concurrent
    Concurrent -->|Complete| Mixed
    Mixed -->|Success| YoungGC
    Mixed -->|Old Full| Full
    Full --> YoungGC

    classDef blue fill:#0173B2,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef orange fill:#DE8F05,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef teal fill:#029E73,stroke:#000000,color:#FFFFFF,stroke-width:2px
    classDef gray fill:#808080,stroke:#000000,color:#FFFFFF,stroke-width:2px
```

**Enabling G1GC** (default in Java 9+):

```bash
# Explicit G1GC selection (not needed in Java 9+)
-XX:+UseG1GC

# Target max pause time (default: 200ms)
-XX:MaxGCPauseMillis=200

# GC pause interval target
-XX:GCPauseIntervalMillis=1000

# Region size (auto-calculated, or set manually)
-XX:G1HeapRegionSize=16m

# Initiating heap occupancy for concurrent marking (default: 45%)
-XX:InitiatingHeapOccupancyPercent=45
```

**G1GC optimization for financial workloads**:

```bash
# DonationTransaction processing (low latency)
java -Xms8g -Xmx8g \
     -XX:+UseG1GC \
     -XX:MaxGCPauseMillis=50 \
     -XX:G1HeapRegionSize=8m \
     -XX:InitiatingHeapOccupancyPercent=35 \
     TransactionProcessor

# Batch Zakat calculation (high throughput)
java -Xms16g -Xmx16g \
     -XX:+UseG1GC \
     -XX:MaxGCPauseMillis=500 \
     -XX:G1HeapRegionSize=32m \
     -XX:InitiatingHeapOccupancyPercent=50 \
     ZakatBatchCalculator
```

**Before G1GC tuning** (default settings):

- Average GC pause: 300ms
- 99th percentile pause: 800ms
- Throughput: 85%

**After G1GC tuning** (MaxGCPauseMillis=50ms, IHOP=35%):

- Average GC pause: 45ms (6.7x better)
- 99th percentile pause: 90ms (8.9x better)
- Throughput: 90% (slight improvement)

### ZGC (Z Garbage Collector)

ZGC is a scalable, low-latency collector designed for heaps up to 16TB with sub-10ms pause times.

**Key characteristics**:

- **Concurrent**: Most work done concurrently (marking, relocation, remapping)
- **Region-based**: Heap divided into pages (small 2MB, medium 32MB, large 2MB+)
- **Colored pointers**: 64-bit pointers encode metadata (marking, relocation)
- **Load barriers**: Read barriers maintain pointer consistency
- **No generational** (until Java 21): Treats all objects equally

**ZGC phases** (all mostly concurrent):

1. **Pause Mark Start** (STW, <1ms)
2. **Concurrent Mark**: Mark live objects
3. **Pause Mark End** (STW, <1ms)
4. **Concurrent Prepare for Relocation**
5. **Pause Relocate Start** (STW, <1ms)
6. **Concurrent Relocate**: Move objects, update references

**Enabling ZGC** (Java 15+):

```bash
# Enable ZGC (production-ready since Java 15)
java -XX:+UseZGC \
     -Xms16g -Xmx16g \
     MyApp

# ZGC tuning options
-XX:+UseZGC
-XX:ConcGCThreads=4           # Concurrent GC threads
-XX:ZCollectionInterval=5     # Min seconds between GC cycles
-XX:ZAllocationSpikeTolerance=2  # Allocation spike tolerance
```

**ZGC for ultra-low latency** (donation processing):

```bash
# DonationPayment processing with <10ms GC pauses
java -Xms32g -Xmx32g \
     -XX:+UseZGC \
     -XX:ConcGCThreads=8 \
     -XX:ZCollectionInterval=10 \
     PaymentGateway
```

**Before ZGC** (G1GC with 32GB heap):

- Average GC pause: 150ms
- 99th percentile pause: 400ms
- Max observed pause: 1.2s

**After ZGC**:

- Average GC pause: 2ms (75x better)
- 99th percentile pause: 5ms (80x better)
- Max observed pause: 8ms (150x better)

**ZGC trade-offs**:

- **Lower throughput**: ~5-15% lower than G1GC due to load barriers
- **Memory overhead**: Colored pointers, load barriers
- **Heap size requirement**: Works best with >8GB heaps
- **No generational** (pre-Java 21): Processes all objects equally

### Generational ZGC (Java 21+)

Generational ZGC combines ZGC's low latency with generational hypothesis benefits.

**Why generational?**

Most objects die young. Separate young/old generations focus GC effort on young objects, reducing work.

**Key improvements over non-generational ZGC**:

- **Higher throughput**: 10-2.5% improvement by focusing on young objects
- **Lower allocation stalls**: Better handling of allocation spikes
- **Reduced CPU usage**: Less concurrent marking/relocation work

**Enabling Generational ZGC** (Java 21+):

```bash
# Enable Generational ZGC (default in Java 21+)
java -XX:+UseZGC \
     -XX:+ZGenerational \
     -Xms16g -Xmx16g \
     MyApp
```

**Generational ZGC for financial workloads**:

```bash
# High-throughput, low-latency Zakat calculation
java -Xms64g -Xmx64g \
     -XX:+UseZGC \
     -XX:+ZGenerational \
     -XX:ConcGCThreads=16 \
     ZakatBatchProcessor
```

**Before Generational ZGC** (non-generational ZGC):

- GC CPU usage: 15%
- Allocation rate: 5GB/sec
- Throughput: 80%

**After Generational ZGC**:

- GC CPU usage: 10% (33% reduction)
- Allocation rate: 5GB/sec (same)
- Throughput: 90% (10% improvement)

### Shenandoah

Shenandoah is a low-latency collector focusing on predictable pause times.

**Key characteristics**:

- **Concurrent compaction**: Compacts heap concurrently with application
- **Brooks pointers**: Forwarding pointers for concurrent relocation
- **Load barriers**: Read barriers for consistent references
- **No generational** (as of Java 21): Treats all objects equally

**Shenandoah phases**:

1. **Init Mark** (STW, <1ms)
2. **Concurrent Marking**
3. **Final Mark** (STW, <1ms)
4. **Concurrent Cleanup**
5. **Concurrent Evacuation**: Relocate objects concurrently
6. **Init Update Refs** (STW, <1ms)
7. **Concurrent Update Refs**
8. **Final Update Refs** (STW, <1ms)

**Enabling Shenandoah**:

```bash
# Enable Shenandoah
java -XX:+UseShenandoahGC \
     -Xms16g -Xmx16g \
     MyApp

# Tuning options
-XX:ShenandoahGCHeuristics=adaptive  # Adaptive, static, compact, aggressive
-XX:ConcGCThreads=4                  # Concurrent GC threads
```

**Shenandoah for real-time processing**:

```bash
# Real-time donation_transaction validation
java -Xms16g -Xmx16g \
     -XX:+UseShenandoahGC \
     -XX:ShenandoahGCHeuristics=compact \
     -XX:ConcGCThreads=8 \
     TransactionValidator
```

**ZGC vs Shenandoah**:

| Feature         | ZGC             | Shenandoah               |
| --------------- | --------------- | ------------------------ |
| Pause times     | <10ms           | <10ms                    |
| Throughput      | Higher          | Slightly lower           |
| Memory overhead | Lower           | Higher (Brooks pointers) |
| Availability    | Java 15+ (prod) | Java 12+ (prod)          |
| Generational    | Yes (Java 21+)  | No                       |

## GC Tuning

### Heap Sizing

Proper heap sizing is crucial for GC performance.

**Basic heap flags**:

```bash
# Initial heap size
-Xms8g

# Maximum heap size
-Xmx8g

# Recommended: Set Xms = Xmx to avoid heap resizing
```

**Heap sizing guidelines**:

1. **Set Xms = Xmx**: Eliminates heap resizing overhead
2. **Leave headroom**: Don't use all physical RAM (leave 20-30% for OS)
3. **Consider live data**: Heap should be 3-4x larger than live data size
4. **Monitor after GC**: Old generation should be <75% full after Full GC

**Heap sizing for financial workloads**:

```bash
# DonationTransaction processing (low latency, predictable load)
# Live data: ~2GB, Target: 8GB heap (4x headroom)
-Xms8g -Xmx8g

# Batch Zakat calculation (high throughput, bursty)
# Live data: ~4GB, Target: 16GB heap (4x headroom)
-Xms16g -Xmx16g

# DonationPayment gateway (ultra-low latency, large cache)
# Live data: ~8GB, Target: 32GB heap (4x headroom)
-Xms32g -Xmx32g
```

### Young/Old Generation Sizing

**Young generation sizing** (G1GC):

```bash
# Young generation size as percentage of heap (default: 60%)
-XX:G1NewSizePercent=30
-XX:G1MaxNewSizePercent=60

# Young generation size (absolute)
-Xmn2g  # 2GB young generation
```

**Young generation sizing guidelines**:

- **High allocation rate**: Larger Young generation (40-60% of heap)
- **Low allocation rate**: Smaller Young generation (20-40% of heap)
- **Frequent Minor GCs**: Increase Young generation size
- **Premature promotion**: Increase Survivor space

**Old generation sizing**:

Old generation size = Total heap - Young generation - Survivor space

### GC Logging and Monitoring

**Enabling GC logging** (Java 9+):

```bash
# Unified GC logging (Java 9+)
-Xlog:gc*:file=gc.log:time,uptime,level,tags

# Detailed GC logging
-Xlog:gc*,gc+ref=debug,gc+heap=debug:file=gc-detailed.log

# Rotation and retention
-Xlog:gc*:file=gc-%t.log:time:filecount=10,filesize=100M
```

**Enabling GC logging** (Java 8):

```bash
# Basic GC logging (Java 8)
-XX:+PrintGCDetails
-XX:+PrintGCDateStamps
-Xloggc:gc.log

# GC log rotation
-XX:+UseGCLogFileRotation
-XX:NumberOfGCLogFiles=10
-XX:GCLogFileSize=100M
```

**Key GC metrics to monitor**:

- **GC pause time**: Average, 99th percentile, max
- **GC frequency**: Minor GC, Major GC, Full GC counts
- **Heap occupancy**: Before/after GC, Old generation usage
- **Promotion rate**: Objects promoted to Old generation
- **Allocation rate**: New objects per second

**Example GC log analysis**:

```
[0.123s][info][gc] GC(0) Pause Young (Normal) (G1 Evacuation Pause) 512M->98M(2048M) 15.234ms
```

Interpretation:

- **Timestamp**: 0.123s since JVM start
- **GC type**: Young generation GC
- **Heap change**: 512MB → 98MB (414MB reclaimed)
- **Total heap**: 2048MB
- **Pause time**: 15.234ms

### Tuning for Financial Workloads

**Scenario 1: DonationTransaction processing** (low latency, steady load):

```bash
# Goal: <50ms GC pauses, consistent latency
java -Xms16g -Xmx16g \
     -XX:+UseG1GC \
     -XX:MaxGCPauseMillis=50 \
     -XX:G1HeapRegionSize=8m \
     -XX:InitiatingHeapOccupancyPercent=35 \
     -XX:G1NewSizePercent=40 \
     -XX:G1MaxNewSizePercent=60 \
     -Xlog:gc*:file=gc-tx.log \
     TransactionProcessor
```

**Scenario 2: Bulk Zakat calculation** (high throughput, batch):

```bash
# Goal: Maximize throughput, tolerate longer pauses
java -Xms32g -Xmx32g \
     -XX:+UseParallelGC \
     -XX:ParallelGCThreads=16 \
     -XX:+UseParallelOldGC \
     -Xlog:gc*:file=gc-zakat.log \
     ZakatBatchCalculator
```

**Scenario 3: DonationPayment gateway** (ultra-low latency):

```bash
# Goal: <10ms GC pauses, 99.99% uptime SLA
java -Xms64g -Xmx64g \
     -XX:+UseZGC \
     -XX:+ZGenerational \
     -XX:ConcGCThreads=16 \
     -Xlog:gc*:file=gc-donation.log \
     PaymentGateway
```

**Before tuning** (default G1GC settings):

- Average GC pause: 200ms
- 99th percentile pause: 500ms
- Throughput: 85%

**After tuning** (scenario-specific):

| Workload                       | Avg Pause | P99 Pause | Throughput |
| ------------------------------ | --------- | --------- | ---------- |
| DonationTransaction Processing | 35ms      | 80ms      | 90%        |
| Bulk Zakat Calculation         | 400ms     | 800ms     | 95%        |
| DonationPayment Gateway        | 3ms       | 8ms       | 88%        |

## Memory Leaks

### Common Leak Patterns

**1. Unclosed resources**:

```java
// LEAK: Connection not closed
public void processPayments(List<DonationPayment> payments) {
  Connection conn = dataSource.getConnection();

  for (DonationPayment donation : payments) {
    processPayment(conn, donation);
  }

  // Missing: conn.close()
}

// FIX: Try-with-resources
public void processPayments(List<DonationPayment> payments) {
  try (Connection conn = dataSource.getConnection()) {
    for (DonationPayment donation : payments) {
      processPayment(conn, donation);
    }
  } // Connection automatically closed
}
```

**2. Static collections**:

```java
// LEAK: Static cache grows unbounded
public class TransactionCache {
  private static final Map<String, DonationTransaction> cache = new HashMap<>();

  public static void cache(DonationTransaction tx) {
    cache.put(tx.getId(), tx); // Never removed
  }
}

// FIX: Bounded cache with eviction
public class TransactionCache {
  private static final LoadingCache<String, DonationTransaction> cache =
    CacheBuilder.newBuilder()
      .maximumSize(10_000)
      .expireAfterWrite(1, TimeUnit.HOURS)
      .build(CacheLoader.from(TransactionCache::loadTransaction));
}
```

**3. Listener leaks**:

```java
// LEAK: Listeners not removed
public class PaymentUI {
  private final PaymentService service;

  public PaymentUI(PaymentService service) {
    this.service = service;
    service.addListener(this::onPaymentComplete); // Listener holds reference to UI
  }

  // When UI closed, listener still registered
}

// FIX: Explicit cleanup
public class PaymentUI implements AutoCloseable {
  private final PaymentService service;
  private final PaymentListener listener;

  public PaymentUI(PaymentService service) {
    this.service = service;
    this.listener = this::onPaymentComplete;
    service.addListener(listener);
  }

  @Override
  public void close() {
    service.removeListener(listener); // Cleanup
  }
}
```

**4. ThreadLocal leaks**:

```java
// LEAK: ThreadLocal not cleaned in thread pool
public class RequestContext {
  private static final ThreadLocal<Beneficiary> currentUser = new ThreadLocal<>();

  public static void setUser(Beneficiary beneficiary) {
    currentUser.set(beneficiary); // Leaked in thread pool
  }
}

// FIX: Explicit cleanup
public class RequestContext {
  private static final ThreadLocal<Beneficiary> currentUser = new ThreadLocal<>();

  public static void setUser(Beneficiary beneficiary) {
    currentUser.set(beneficiary);
  }

  public static void clear() {
    currentUser.remove(); // Explicit cleanup
  }
}

// Servlet filter pattern
public class RequestContextFilter implements Filter {
  @Override
  public void doFilter(ServletRequest req, ServletResponse res, FilterChain chain)
      throws IOException, ServletException {
    try {
      RequestContext.setUser(extractUser(req));
      chain.doFilter(req, res);
    } finally {
      RequestContext.clear(); // Cleanup
    }
  }
}
```

### Detecting Memory Leaks

**Heap dump on OutOfMemoryError**:

```bash
# Automatically dump heap when OOM occurs
-XX:+HeapDumpOnOutOfMemoryError
-XX:HeapDumpPath=/var/dumps/heap-dump.hprof

# Enable for production (minimal overhead)
```

**Periodic heap dumps for leak detection**:

```bash
# Capture heap dump using jmap
jmap -dump:live,format=b,file=heap-dump.hprof <pid>

# Example: Capture heap dump for PID 12345
jmap -dump:live,format=b,file=/tmp/heap-dump-$(date +%Y%m%d-%H%M%S).hprof 12345
```

**Analyzing heap dumps with Eclipse Memory Analyzer (MAT)**:

1. Open heap dump in MAT
2. Run "Leak Suspects Report"
3. Examine dominator tree for large retained sizes
4. Identify accumulation points (large collections)
5. Trace references to root (GC roots)

**Example leak detection** (donation_transaction cache):

```
// MAT Leak Suspects Report:

Problem Suspect 1:
  One instance of "com.sharia.finance.TransactionCache" loaded by
  "sun.misc.Launcher$AppClassLoader @ 0x7f8a9c000000" occupies
  1,234,567,890 bytes (75.43% of total heap).

  The instance is referenced by:
    - com.sharia.finance.TransactionCache.cache (static field)
    - java.util.HashMap$Node[] (backing array)

  Suspect: Unbounded static collection retaining 1.2GB of DonationTransaction objects.
```

**Using JVisualVM for live leak detection**:

1. Connect to running JVM
2. Take heap dump before operation
3. Run leak-inducing operation (e.g., 1000 transactions)
4. Take heap dump after operation
5. Compare dumps: Objects that grew significantly = leak candidates

**Memory leak detection in tests**:

```java
import org.junit.jupiter.api.Test;
import static org.assertj.core.api.Assertions.assertThat;

@Test
void shouldNotLeakMemoryWhenProcessingTransactions() {
  // Baseline
  System.gc();
  long memoryBefore = Runtime.getRuntime().totalMemory()
    - Runtime.getRuntime().freeMemory();

  // Run operation 10 times
  for (int i = 0; i < 10; i++) {
    processTransactions(generateTransactions(1000));
  }

  // Measure after GC
  System.gc();
  long memoryAfter = Runtime.getRuntime().totalMemory()
    - Runtime.getRuntime().freeMemory();

  // Memory should not grow significantly
  long delta = memoryAfter - memoryBefore;
  assertThat(delta).isLessThan(10 * 1024 * 1024); // <10MB growth
}
```

### Preventing Memory Leaks

**1. Use try-with-resources**:

```java
// Automatically closes AutoCloseable resources
try (Connection conn = dataSource.getConnection();
     PreparedStatement stmt = conn.prepareStatement(sql)) {

  ResultSet rs = stmt.executeQuery();
  // Process results...

} // Resources closed automatically, even on exception
```

**2. Weak references for caches**:

```java
import java.lang.ref.WeakReference;

// Cache using WeakReferences (GC-eligible)
public class TransactionCache {
  private final Map<String, WeakReference<DonationTransaction>> cache =
    new ConcurrentHashMap<>();

  public void put(String id, DonationTransaction tx) {
    cache.put(id, new WeakReference<>(tx));
  }

  public Optional<DonationTransaction> get(String id) {
    WeakReference<DonationTransaction> ref = cache.get(id);
    return ref != null
      ? Optional.ofNullable(ref.get())
      : Optional.empty();
  }
}
```

**3. Bounded collections with eviction**:

```java
import com.google.common.cache.CacheBuilder;

// Guava cache with size and time eviction
LoadingCache<String, DonationTransaction> cache = CacheBuilder.newBuilder()
  .maximumSize(10_000)              // Max 10k entries
  .expireAfterWrite(1, TimeUnit.HOURS)  // Expire after 1 hour
  .build(CacheLoader.from(this::loadTransaction));
```

**4. Explicit cleanup in lifecycle methods**:

```java
@Component
public class PaymentProcessor implements DisposableBean {
  private final ExecutorService executor = Executors.newFixedThreadPool(10);

  @Override
  public void destroy() {
    executor.shutdown();
    try {
      if (!executor.awaitTermination(60, TimeUnit.SECONDS)) {
        executor.shutdownNow();
      }
    } catch (InterruptedException e) {
      executor.shutdownNow();
      Thread.currentThread().interrupt();
    }
  }
}
```

## Memory Optimization

### Object Pooling

Object pooling reuses expensive-to-create objects, reducing allocation and GC pressure.

**When to use object pooling**:

- Object creation is expensive (database connections, threads)
- High allocation rate (millions of objects per second)
- Object size is significant (large buffers, complex structures)

**When NOT to use object pooling**:

- Modern GCs handle allocation efficiently
- Premature optimization (profile first)
- Objects are small and short-lived
- Complexity outweighs benefits

**Database connection pooling** (HikariCP):

```java
import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;

public class DatabaseConfig {
  public DataSource dataSource() {
    HikariConfig config = new HikariConfig();
    config.setJdbcUrl("jdbc:postgresql://localhost:5432/finance");
    config.setUsername("app");
    config.setPassword("secret");

    // Pool sizing
    config.setMaximumPoolSize(20);      // Max connections
    config.setMinimumIdle(5);           // Min idle connections
    config.setConnectionTimeout(30000); // 30s timeout
    config.setIdleTimeout(600000);      // 10min idle timeout
    config.setMaxLifetime(1800000);     // 30min max lifetime

    return new HikariDataSource(config);
  }
}
```

**Thread pooling** (ExecutorService):

```java
import java.util.concurrent.*;

public class PaymentProcessor {
  private final ExecutorService executor;

  public PaymentProcessor() {
    // Thread pool sizing: CPU-bound vs I/O-bound
    int processors = Runtime.getRuntime().availableProcessors();

    // CPU-bound: processors + 1
    this.executor = Executors.newFixedThreadPool(processors + 1);

    // I/O-bound: processors * 2 (more threads for blocking operations)
    // this.executor = Executors.newFixedThreadPool(processors * 2);
  }

  public CompletableFuture<PaymentResult> processAsync(DonationPayment donation) {
    return CompletableFuture.supplyAsync(() -> process(donation), executor);
  }
}
```

**ByteBuffer pooling**:

```java
import java.nio.ByteBuffer;
import java.util.concurrent.ArrayBlockingQueue;

public class ByteBufferPool {
  private final ArrayBlockingQueue<ByteBuffer> pool;
  private final int bufferSize;

  public ByteBufferPool(int poolSize, int bufferSize) {
    this.pool = new ArrayBlockingQueue<>(poolSize);
    this.bufferSize = bufferSize;

    // Pre-allocate buffers
    for (int i = 0; i < poolSize; i++) {
      pool.offer(ByteBuffer.allocateDirect(bufferSize));
    }
  }

  public ByteBuffer acquire() {
    ByteBuffer buffer = pool.poll();
    if (buffer == null) {
      // Pool exhausted, allocate new buffer
      buffer = ByteBuffer.allocateDirect(bufferSize);
    }
    buffer.clear();
    return buffer;
  }

  public void release(ByteBuffer buffer) {
    if (buffer.capacity() == bufferSize) {
      buffer.clear();
      pool.offer(buffer); // Return to pool
    }
    // Discard buffers with wrong size
  }
}
```

**Object pooling example** (DonationPayment validation):

```java
// Before: High allocation rate
public class PaymentValidator {
  public ValidationResult validate(DonationPayment donation) {
    // Creates new ValidationResult each time
    ValidationResult result = new ValidationResult();

    if (donation.getAmount().isNegative()) {
      result.addError("Amount must be positive");
    }

    return result;
  }
}

// After: Object pooling
import org.apache.commons.pool2.ObjectPool;
import org.apache.commons.pool2.impl.GenericObjectPool;

public class PaymentValidator {
  private final ObjectPool<ValidationResult> pool;

  public PaymentValidator() {
    this.pool = new GenericObjectPool<>(new ValidationResultFactory());
  }

  public ValidationResult validate(DonationPayment donation) {
    ValidationResult result = null;
    try {
      result = pool.borrowObject(); // Borrow from pool
      result.clear(); // Reset state

      if (donation.getAmount().isNegative()) {
        result.addError("Amount must be positive");
      }

      return result;
    } catch (Exception e) {
      throw new RuntimeException("Failed to validate donation", e);
    } finally {
      if (result != null) {
        pool.returnObject(result); // Return to pool
      }
    }
  }
}
```

**Before pooling**:

- Allocation rate: 5 million ValidationResult/sec
- GC overhead: 2.5% CPU time
- Throughput: 50,000 payments/sec

**After pooling**:

- Allocation rate: 100 ValidationResult/sec (reused)
- GC overhead: 5% CPU time (75% reduction)
- Throughput: 65,000 payments/sec (30% improvement)

### Compact Object Headers (Java 25+)

Java 25 introduces compact object headers (JEP 450), reducing object header size from 128 bits to 64 bits.

**Traditional object header** (Java 8-24):

```
Object Header (128 bits = 16 bytes):
  - Mark word (64 bits): Hash code, GC age, lock state
  - Class pointer (64 bits): Reference to class metadata
```

**Compact object header** (Java 25+):

```
Object Header (64 bits = 8 bytes):
  - Combined mark + class pointer (64 bits)
```

**Enabling compact headers** (Java 25+):

```bash
# Enable compact object headers (default in Java 25+)
-XX:+UseCompactObjectHeaders

# Disable compact headers (fallback to traditional)
-XX:-UseCompactObjectHeaders
```

**Memory savings**:

```java
// Example: Money value object
public final class Money {
  private final BigDecimal amount;  // 8 bytes reference
  private final String currency;    // 8 bytes reference
}

// Traditional header: 16 bytes header + 16 bytes fields = 32 bytes
// Compact header: 8 bytes header + 16 bytes fields = 24 bytes
// Savings: 25% reduction (8 bytes per Money object)
```

**Impact on financial workloads**:

```java
// Zakat calculation: 10 million Money objects
// Traditional: 10M * 32 bytes = 320MB
// Compact: 10M * 24 bytes = 240MB
// Savings: 80MB (25% reduction)
```

**Before compact headers** (1 billion objects):

- Heap usage: 16GB (object headers)
- GC frequency: Every 30 seconds

**After compact headers**:

- Heap usage: 12GB (25% reduction)
- GC frequency: Every 40 seconds (33% reduction)

### Value Types (Project Valhalla)

Project Valhalla introduces value types (inline classes), eliminating object headers entirely for value-like types.

**Current reference types** (heap-allocated):

```java
public final class Point {
  private final int x;
  private final int y;
}

// Point object layout (traditional):
// - Object header: 16 bytes
// - x: 4 bytes
// - y: 4 bytes
// - Padding: 8 bytes (alignment)
// Total: 32 bytes

Point[] points = new Point[1000];
// Array: 8000 bytes (1000 references)
// Objects: 32000 bytes (1000 Point objects)
// Total: 40000 bytes
```

**Value types** (stack/register-allocated):

```java
// Value type declaration (Project Valhalla syntax)
public value class Point {
  private final int x;
  private final int y;
}

// Point value layout:
// - x: 4 bytes
// - y: 4 bytes
// Total: 8 bytes (no object header)

Point[] points = new Point[1000];
// Array: 8000 bytes (1000 inline values)
// No separate objects (values inlined in array)
// Total: 8000 bytes (5x reduction)
```

**Financial domain value types**:

```java
// Current Money type (reference)
public final class Money {
  private final long amountInCents; // 8 bytes
  private final Currency currency;  // 8 bytes reference
}

// Money object: 16 (header) + 16 (fields) = 32 bytes

// Future Money value type
public value class Money {
  private final long amountInCents;
  private final short currencyCode; // ISO 4217 numeric code
}

// Money value: 8 + 2 = 10 bytes (no header)
// Savings: 68.75% reduction
```

**Value type benefits**:

- **No indirection**: Values stored inline (cache-friendly)
- **No GC overhead**: Values managed by stack/register, not GC
- **Reduced memory**: No object headers
- **Better performance**: CPU can vectorize operations

**Project Valhalla status**: Preview in future Java versions (post-Java 25).

## Profiling Tools

### Java Flight Recorder (JFR)

JFR is a low-overhead profiling tool built into the JVM.

**Enabling JFR**:

```bash
# Enable JFR (Java 11+, production-ready)
-XX:StartFlightRecording=duration=60s,filename=recording.jfr

# Continuous recording with circular buffer
-XX:StartFlightRecording=maxage=6h,maxsize=500M,filename=recording.jfr

# Enable specific events
-XX:StartFlightRecording=settings=profile
```

**Starting JFR on running JVM**:

```bash
# Start recording
jcmd <pid> JFR.start duration=60s filename=recording.jfr

# Stop recording
jcmd <pid> JFR.stop filename=recording.jfr

# Dump current recording
jcmd <pid> JFR.dump filename=recording.jfr
```

**Analyzing JFR recordings**:

```bash
# Open in JDK Mission Control (JMC)
jmc recording.jfr

# Or use jfr tool (Java 11+)
jfr print --events jdk.ObjectAllocationInNewTLAB recording.jfr
jfr print --events jdk.GarbageCollection recording.jfr
```

**Key JFR events for memory analysis**:

- `jdk.ObjectAllocationInNewTLAB`: TLAB allocations
- `jdk.ObjectAllocationOutsideTLAB`: Large object allocations
- `jdk.GarbageCollection`: GC events
- `jdk.OldGarbageCollection`: Old generation GCs
- `jdk.PromoteObjectInNewPLAB`: Object promotions

**Example JFR analysis** (memory leak detection):

1. Record JFR during leak scenario
2. Open in JMC
3. Navigate to "Memory" tab → "Allocations"
4. Sort by "Total Allocation" (descending)
5. Identify top allocation sites

```
Top Allocation Sites:
1. com.sharia.finance.TransactionCache.cache(DonationTransaction) - 1.2GB
2. com.sharia.finance.Money.<init>(BigDecimal, Currency) - 500MB
3. java.util.HashMap.resize() - 200MB
```

### jmap (Heap Dump Tool)

jmap generates heap dumps and heap statistics.

**Heap histogram**:

```bash
# Live objects only (triggers Full GC)
jmap -histo:live <pid>

# All objects (including garbage)
jmap -histo <pid>

# Example output:
# num     #instances         #bytes  class name
# 1:        123456        9876543  [C (char arrays)
# 2:         98765        6543210  java.lang.String
# 3:         45678        3210987  com.sharia.finance.DonationTransaction
```

**Heap dump**:

```bash
# Dump heap to file
jmap -dump:live,format=b,file=heap.hprof <pid>

# Dump with compression (Java 17+)
jmap -dump:live,format=b,gz=9,file=heap.hprof.gz <pid>
```

**Heap configuration**:

```bash
# Show heap configuration
jmap -heap <pid>

# Example output:
# Heap Configuration:
#    MinHeapFreeRatio         = 40
#    MaxHeapFreeRatio         = 70
#    MaxHeapSize              = 16106127360 (15360MB)
#    NewSize                  = 5368709120 (5120MB)
#    MaxNewSize               = 5368709120 (5120MB)
#    OldSize                  = 10737418240 (10240MB)
```

### jstat (GC Statistics)

jstat monitors GC and heap statistics in real-time.

**GC statistics**:

```bash
# GC summary (update every 1 second)
jstat -gc <pid> 1000

# Example output:
#  S0C    S1C    S0U    S1U      EC       EU        OC         OU       MC     MU    YGC     YGCT    FGC    FGCT     GCT
# 512.0  512.0  128.0    0.0  4096.0   2048.0   10240.0    5120.0  1024.0  512.0    42    0.315     2    0.143    0.458
```

**Column meanings**:

- `S0C/S1C`: Survivor 0/1 capacity
- `EC`: Eden capacity
- `EU`: Eden utilization
- `OC`: Old generation capacity
- `OU`: Old generation utilization
- `YGC`: Young GC count
- `YGCT`: Young GC time
- `FGC`: Full GC count
- `FGCT`: Full GC time
- `GCT`: Total GC time

**GC cause**:

```bash
# Show GC cause
jstat -gccause <pid> 1000

# Example output:
#  LGCC                 GCC
#  Allocation Failure   No GC
```

**Capacity statistics**:

```bash
# Heap capacity
jstat -gccapacity <pid>

# New generation capacity
jstat -gcnewcapacity <pid>

# Old generation capacity
jstat -gcoldcapacity <pid>
```

### VisualVM

VisualVM is a visual profiling tool with heap dump analysis.

**Launching VisualVM**:

```bash
# Start VisualVM
jvisualvm

# Or from JDK bin directory
$JAVA_HOME/bin/jvisualvm
```

**Key features**:

1. **Heap dump analysis**: Visual heap explorer, OQL queries
2. **Sampler**: CPU and memory sampling (low overhead)
3. **Profiler**: CPU and memory profiling (higher overhead)
4. **MBean browser**: JMX monitoring
5. **Thread dump**: Thread state analysis

**Heap dump analysis in VisualVM**:

1. Connect to JVM or open heap dump file
2. Take heap dump (right-click → Heap Dump)
3. Analyze:
   - **Classes view**: Class instances and sizes
   - **Objects view**: Individual object inspection
   - **OQL Console**: Query heap (SQL-like syntax)

**OQL queries** (Object Query Language):

```javascript
// Find all Money objects with amount > 1000
select m from com.sharia.finance.Money m where m.amount.doubleValue() > 1000

// Find all HashMap with size > 10000
select h from java.util.HashMap h where h.size > 10000

// Count instances by class
select heap.classes()
```

### async-profiler

async-profiler is a low-overhead sampling profiler for JVM.

**Installing async-profiler**:

```bash
# Download from https://github.com/async-profiler/async-profiler
wget https://github.com/async-profiler/async-profiler/releases/download/v2.9/async-profiler-2.9-linux-x64.tar.gz
tar xzf async-profiler-2.9-linux-x64.tar.gz
```

**Profiling CPU**:

```bash
# Profile CPU for 60 seconds, generate flamegraph
./profiler.sh -d 60 -f flamegraph-cpu.html <pid>
```

**Profiling allocations**:

```bash
# Profile allocations, generate flamegraph
./profiler.sh -d 60 -e alloc -f flamegraph-alloc.html <pid>
```

**Profiling specific events**:

```bash
# Profile lock contention
./profiler.sh -d 60 -e lock -f flamegraph-lock.html <pid>

# Profile page faults (memory access patterns)
./profiler.sh -d 60 -e page-faults -f flamegraph-pagefaults.html <pid>
```

**Interpreting flamegraphs**:

- **X-axis**: Alphabetical order (not time)
- **Y-axis**: Stack depth (call stack)
- **Width**: Proportion of samples (wider = more CPU/allocations)
- **Color**: Random (for differentiation)

**Example allocation flamegraph analysis**:

```
Top allocation sites (widest bars):
1. com.sharia.finance.TransactionCache.put() → HashMap.resize()
2. com.sharia.finance.Money.multiply() → BigDecimal.<init>()
3. com.sharia.finance.PaymentProcessor.validate() → ValidationResult.<init>()
```

**Optimization targets**: Focus on widest bars for maximum impact.

## Best Practices

### 1. Hot Path Optimization

Optimize allocation on hot paths (frequently executed code).

**Identifying hot paths**:

```bash
# Profile CPU with async-profiler
./profiler.sh -d 60 -f flamegraph-cpu.html <pid>

# Or use JFR
jcmd <pid> JFR.start duration=60s filename=recording.jfr
```

**Hot path example** (Zakat calculation):

```java
// Before: Allocates Money objects on hot path
public Money calculateZakat(Money wealth) {
  Money nisab = Money.of(85, "g", "gold"); // Allocation

  if (wealth.isGreaterThanOrEqualTo(nisab)) {
    Money rate = Money.of(0.025); // Allocation
    return wealth.multiply(rate); // Allocation in multiply()
  }

  return Money.ZERO;
}

// After: Pre-allocated constants, reduced allocations
private static final Money NISAB = Money.of(85, "g", "gold");
private static final BigDecimal ZAKAT_RATE = new BigDecimal("0.025");

public Money calculateZakat(Money wealth) {
  if (wealth.isGreaterThanOrEqualTo(NISAB)) {
    return wealth.multiply(ZAKAT_RATE); // Single allocation
  }

  return Money.ZERO;
}
```

**Before optimization**:

- Allocations per call: 3 Money objects
- Throughput: 100,000 calculations/sec
- GC overhead: 15% CPU time

**After optimization**:

- Allocations per call: 1 Money object (66% reduction)
- Throughput: 150,000 calculations/sec (50% improvement)
- GC overhead: 5% CPU time (66% reduction)

### 2. Immutable Object Reuse

Reuse immutable objects instead of creating duplicates.

**String interning**:

```java
// Before: Duplicate strings
public void processTransactions(List<DonationTransaction> txs) {
  for (DonationTransaction tx : txs) {
    String currency = tx.getCurrency(); // "USD" duplicated
    // ...
  }
}

// After: String interning (JVM automatically interns string literals)
public void processTransactions(List<DonationTransaction> txs) {
  for (DonationTransaction tx : txs) {
    String currency = tx.getCurrency().intern(); // Reuses canonical "USD"
    // ...
  }
}
```

**Immutable value object caching**:

```java
// Common Money values cache
public final class Money {
  private static final Money ZERO = new Money(BigDecimal.ZERO, Currency.USD);
  private static final Money ONE_USD = new Money(BigDecimal.ONE, Currency.USD);

  private static final Map<BigDecimal, Money> USD_CACHE = Map.of(
    BigDecimal.ZERO, ZERO,
    BigDecimal.ONE, ONE_USD,
    new BigDecimal("10"), new Money(new BigDecimal("10"), Currency.USD),
    new BigDecimal("100"), new Money(new BigDecimal("100"), Currency.USD)
  );

  public static Money ofUSD(BigDecimal amount) {
    Money cached = USD_CACHE.get(amount);
    return cached != null ? cached : new Money(amount, Currency.USD);
  }
}
```

**Before caching** (1 million transactions with common amounts):

- Memory: 32MB (1M Money objects)
- Allocation rate: 1M objects/sec

**After caching** (90% cache hit rate):

- Memory: 3.2MB (100k Money objects + cache)
- Allocation rate: 100k objects/sec (90% reduction)

### 3. StringBuilder for String Concatenation

Use StringBuilder for concatenating multiple strings.

**Avoid string concatenation in loops**:

```java
// Before: Creates new String each iteration
public String formatTransactions(List<DonationTransaction> txs) {
  String result = "";
  for (DonationTransaction tx : txs) {
    result += tx.getId() + ": " + tx.getAmount() + "\n"; // Inefficient
  }
  return result;
}

// After: StringBuilder (single allocation)
public String formatTransactions(List<DonationTransaction> txs) {
  StringBuilder sb = new StringBuilder(txs.size() * 50); // Pre-size
  for (DonationTransaction tx : txs) {
    sb.append(tx.getId()).append(": ").append(tx.getAmount()).append("\n");
  }
  return sb.toString();
}
```

**Before StringBuilder** (10,000 transactions):

- Allocations: 10,000 intermediate String objects
- Memory: 5MB temporary strings
- Time: 250ms

**After StringBuilder**:

- Allocations: 1 StringBuilder + 1 final String
- Memory: 500KB
- Time: 15ms (16.7x faster)

### 4. Lazy Initialization

Defer object creation until needed.

**Lazy field initialization**:

```java
// Before: Eager initialization (may not be needed)
public class TransactionValidator {
  private final List<ValidationRule> rules = loadRules(); // Always loaded

  public ValidationResult validate(DonationTransaction tx) {
    // Rules may not be needed for simple validations
    if (tx.getAmount().isNegative()) {
      return ValidationResult.error("Amount must be positive");
    }

    // Complex validation using rules
    return applyRules(tx);
  }
}

// After: Lazy initialization
public class TransactionValidator {
  private volatile List<ValidationRule> rules; // Loaded on demand

  public ValidationResult validate(DonationTransaction tx) {
    // Fast path: Simple validation without rules
    if (tx.getAmount().isNegative()) {
      return ValidationResult.error("Amount must be positive");
    }

    // Lazy load rules when needed
    if (rules == null) {
      synchronized (this) {
        if (rules == null) {
          rules = loadRules();
        }
      }
    }

    return applyRules(tx);
  }
}
```

**Before lazy initialization** (10,000 validators):

- Memory: 100MB (rules loaded for all validators)
- Startup time: 5 seconds

**After lazy initialization** (only 100 validators need rules):

- Memory: 1MB (rules loaded for 100 validators)
- Startup time: 500ms (10x faster)

### 5. Finalization Avoidance

Avoid finalizers and use try-with-resources instead.

**Don't use finalizers**:

```java
// BAD: Finalizer (deprecated, unpredictable, slow)
public class DatabaseConnection {
  @Override
  protected void finalize() throws Throwable {
    close(); // May never be called, or called too late
    super.finalize();
  }
}

// GOOD: AutoCloseable with try-with-resources
public class DatabaseConnection implements AutoCloseable {
  @Override
  public void close() {
    // Immediate, deterministic cleanup
  }
}

// Usage
try (DatabaseConnection conn = new DatabaseConnection()) {
  // Use connection...
} // Automatically closed
```

**Why finalizers are bad**:

- **Unpredictable**: No guarantee when (or if) finalizer runs
- **Slow GC**: Finalizable objects take longer to collect
- **Resource leaks**: Resources held until finalizer runs
- **Security risks**: Finalizer attacks

**Use Cleaner (Java 9+) if absolutely necessary**:

```java
import java.lang.ref.Cleaner;

public class NativeResource {
  private static final Cleaner CLEANER = Cleaner.create();

  private final Cleaner.Cleanable cleanable;
  private final State state;

  private static class State implements Runnable {
    private long nativePtr;

    @Override
    public void run() {
      if (nativePtr != 0) {
        freeNative(nativePtr);
        nativePtr = 0;
      }
    }
  }

  public NativeResource() {
    this.state = new State();
    this.state.nativePtr = allocateNative();
    this.cleanable = CLEANER.register(this, state);
  }

  public void close() {
    cleanable.clean(); // Explicit cleanup
  }
}
```

### 6. Avoid Memory Leaks

Common leak prevention strategies.

**Use weak references for listeners**:

```java
// Listener registration with WeakReference
public class EventPublisher {
  private final List<WeakReference<EventListener>> listeners =
    new CopyOnWriteArrayList<>();

  public void addListener(EventListener listener) {
    listeners.add(new WeakReference<>(listener));
  }

  public void fireEvent(Event event) {
    Iterator<WeakReference<EventListener>> it = listeners.iterator();
    while (it.hasNext()) {
      WeakReference<EventListener> ref = it.next();
      EventListener listener = ref.get();

      if (listener == null) {
        it.remove(); // Listener GC'd, remove reference
      } else {
        listener.onEvent(event);
      }
    }
  }
}
```

**Clear ThreadLocal variables**:

```java
// ThreadLocal cleanup in web application
public class RequestContextFilter implements Filter {
  @Override
  public void doFilter(ServletRequest req, ServletResponse res, FilterChain chain)
      throws IOException, ServletException {
    try {
      RequestContext.init(req);
      chain.doFilter(req, res);
    } finally {
      RequestContext.clear(); // Critical: Cleanup ThreadLocal
    }
  }
}
```

**Close resources properly**:

```java
// CORRECT: Try-with-resources guarantees cleanup
try (Connection conn = dataSource.getConnection();
     PreparedStatement stmt = conn.prepareStatement(sql);
     ResultSet rs = stmt.executeQuery()) {

  while (rs.next()) {
    // Process results...
  }
} // All resources closed automatically, even on exception
```

## Memory Management Checklist

Use this checklist when developing Java applications:

**Heap Configuration**:

- [ ] Set `-Xms` = `-Xmx` to avoid heap resizing
- [ ] Heap size is 3-4x larger than live data
- [ ] Leave 20-30% RAM for OS and non-heap memory
- [ ] Young generation sized appropriately for allocation rate

**GC Selection**:

- [ ] G1GC for general purpose (default, recommended)
- [ ] ZGC/Shenandoah for ultra-low latency (<10ms pauses)
- [ ] Parallel GC for batch processing (max throughput)
- [ ] Generational ZGC for best of both (Java 21+)

**GC Tuning**:

- [ ] MaxGCPauseMillis set based on SLA requirements
- [ ] GC logging enabled with rotation
- [ ] GC metrics monitored (pause time, frequency, throughput)
- [ ] Heap dump on OOM enabled for production

**Allocation Optimization**:

- [ ] Hot paths identified and optimized (profiling)
- [ ] Pre-allocate collections with known capacity
- [ ] Reuse immutable objects (constants, caching)
- [ ] Use StringBuilder for string concatenation
- [ ] Avoid allocations in loops

**Memory Leak Prevention**:

- [ ] Resources closed with try-with-resources
- [ ] ThreadLocal variables cleared in finally blocks
- [ ] Listeners removed when no longer needed
- [ ] Static collections bounded with eviction
- [ ] No finalizers used (use AutoCloseable instead)

**Profiling**:

- [ ] JFR enabled and regularly analyzed
- [ ] Heap dumps captured periodically
- [ ] Allocation hotspots identified (async-profiler)
- [ ] Memory leaks detected before production

**Modern Java Features**:

- [ ] Compact object headers enabled (Java 25+)
- [ ] Value types used for value-like types (Valhalla)
- [ ] Generational ZGC for latency+throughput (Java 21+)

## Related Documentation

- [Java Performance](./ex-so-prla-ja__performance.md) - Performance optimization including CPU profiling, JIT compilation
- [Java Concurrency](./ex-so-prla-ja__concurrency-and-parallelism.md) - Thread memory, concurrent collections, memory visibility
- [Java Best Practices](./ex-so-prla-ja__best-practices.md) - General coding standards including memory efficiency
- [Java Anti-Patterns](./ex-so-prla-ja__anti-patterns.md) - Memory leak patterns, resource management mistakes
- [Go Memory Management](../golang/ex-so-prla-go__memory-management.md) - Comparison with Go's simpler GC approach

## Sources

1. **JVM Specification** - [Java Virtual Machine Specification](https://docs.oracle.com/javase/specs/jvms/se21/html/index.html) - Official JVM memory model and GC specification (2024)
2. **G1GC Tuning Guide** - [Getting Started with the G1 Garbage Collector](https://www.oracle.com/technical-resources/articles/java/g1gc.html) - Oracle official G1GC tuning guide (2023)
3. **ZGC Documentation** - [The Z Garbage Collector](https://wiki.openjdk.org/display/zgc/Main) - OpenJDK ZGC wiki with implementation details (2024)
4. **JEP 450** - [Compact Object Headers](https://openjdk.org/jeps/450) - Java 25 compact object headers specification (2024)
5. **Project Valhalla** - [Value Objects and Primitive Classes](https://openjdk.org/projects/valhalla/) - OpenJDK Project Valhalla documentation (2024)

---

**Last Updated**: 2026-01-23
**Java Version**: 17+ (baseline), 21+ (recommended), 23 (latest)
**Maintainers**: Platform Documentation Team
