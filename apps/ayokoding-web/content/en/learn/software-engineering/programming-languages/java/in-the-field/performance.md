---
title: "Performance"
date: 2026-02-03T00:00:00+07:00
draft: false
description: Comprehensive guide to Java performance optimization from profiling fundamentals to JVM tuning and benchmarking
weight: 10000028
tags: ["java", "performance", "jvm", "profiling", "optimization", "jfr", "jmh"]
---

## Why Performance Matters

Performance optimization ensures applications meet user expectations for responsiveness, throughput, and resource efficiency. Poor performance wastes hardware resources, increases costs, and damages user experience.

**Core Benefits**:

- **User satisfaction**: Fast, responsive applications
- **Cost efficiency**: Utilize hardware effectively
- **Scalability**: Handle increasing load without proportional cost increase
- **Competitive advantage**: Performance differentiates products
- **Resource conservation**: Energy and infrastructure efficiency

**Problem**: Premature optimization wastes time on non-bottlenecks. Guessing at performance issues leads to ineffective changes.

**Solution**: Measure first with profiling tools, identify actual bottlenecks, optimize based on data, verify improvements.

## Performance Optimization Workflow

Follow systematic approach: measure, analyze, optimize, verify.

**The golden rule**:

1. **Make it work**: Correct, readable implementation first
2. **Make it right**: Clean, maintainable code
3. **Make it fast**: Profile, identify hotspots, optimize

**Critical principle**: Never optimize without measuring. 80% of execution time is spent in 20% of code (hotspots).

## Profiling Tools (Standard Library)

Java provides built-in profiling tools with minimal overhead for production use.

### Java Flight Recorder (JFR)

JFR is the modern, low-overhead profiling tool built into the JDK.

**Characteristics**:

- Built into JDK (no external dependencies)
- Low overhead (~1-2% in production)
- Continuous recording possible
- Records JVM events (GC, compilation, thread blocking, I/O)
- Analyzes with JDK Mission Control (JMC)

**Pattern**:

```bash
# Start application with JFR enabled
java -XX:StartFlightRecording=duration=60s,filename=recording.jfr \
     -XX:FlightRecorderOptions=stackdepth=128 \
     MyApplication

# Or enable at runtime with jcmd
jcmd <pid> JFR.start duration=60s filename=recording.jfr

# Dump recording
jcmd <pid> JFR.dump filename=recording.jfr

# Stop recording
jcmd <pid> JFR.stop
```

**Analyzing JFR recordings**:

```bash
# Download JDK Mission Control from https://jdk.java.net/jmc/
# Open recording.jfr in JMC GUI
# Analyze:
# - Method profiling (hot methods)
# - Allocations (memory pressure)
# - Lock contention (thread blocking)
# - GC activity (pause times)
# - I/O events (file/network)
```

**What JFR reveals**:

- **Hot methods**: CPU-intensive code paths
- **Allocation hotspots**: Where objects are created
- **Lock contention**: Where threads block waiting for locks
- **GC overhead**: Garbage collection pause times
- **Thread states**: Running, blocked, waiting, parked

### jconsole (Monitoring GUI)

jconsole provides real-time monitoring of JVM metrics.

**Pattern**:

```bash
# Launch jconsole
jconsole

# Connect to running application
# - Local process: Select from list
# - Remote: <hostname>:<port> (requires JMX configuration)

# Monitor:
# - Heap memory usage
# - Thread count
# - CPU usage
# - Class loading
# - GC activity (frequency, duration)
```

**JMX configuration for remote monitoring**:

```bash
java -Dcom.sun.management.jmxremote \
     -Dcom.sun.management.jmxremote.port=9010 \
     -Dcom.sun.management.jmxremote.authenticate=false \
     -Dcom.sun.management.jmxremote.ssl=false \
     MyApplication
```

### VisualVM (Profiling and Monitoring)

VisualVM provides comprehensive profiling capabilities.

**Download**: <https://visualvm.github.io/>

**Pattern**:

```bash
# Launch VisualVM
visualvm

# Connect to application
# - Automatic discovery for local processes
# - Remote: Add JMX connection

# Profiling capabilities:
# - CPU profiling (method-level)
# - Memory profiling (allocation tracking)
# - Thread analysis (deadlock detection)
# - Heap dumps (memory snapshots)
```

**CPU profiling workflow**:

1. Start CPU profiler
2. Exercise application (user scenarios, load testing)
3. Stop profiler
4. Analyze hot methods (methods consuming most time)
5. Investigate top CPU consumers

**Memory profiling workflow**:

1. Start memory profiler
2. Exercise application
3. Stop profiler
4. Analyze allocation hotspots (classes consuming most memory)
5. Investigate object creation patterns

### jmap (Heap Dump)

jmap captures heap snapshots for memory analysis.

**Pattern**:

```bash
# Generate heap dump
jmap -dump:live,format=b,file=heap.hprof <pid>

# Analyze with VisualVM or Eclipse Memory Analyzer (MAT)
# Download MAT: https://eclipse.dev/mat/

# What to look for:
# - Large objects consuming memory
# - Memory leaks (growing retained size)
# - Duplicate strings (candidates for interning)
# - Collections with excessive capacity
```

**Heap histogram**:

```bash
# Show object counts and sizes
jmap -histo <pid> | head -20

# Output shows:
# - Class name
# - Instance count
# - Total bytes
# - Sorted by total memory usage
```

### jstack (Thread Dump)

jstack captures thread states for analyzing blocking and deadlocks.

**Pattern**:

```bash
# Generate thread dump
jstack <pid> > threads.txt

# Multiple dumps reveal patterns
jstack <pid> > threads1.txt
sleep 5
jstack <pid> > threads2.txt
sleep 5
jstack <pid> > threads3.txt

# Analyze for:
# - BLOCKED threads (lock contention)
# - Deadlocks (circular waiting)
# - Threads stuck in same method (hotspots)
# - Thread pool saturation
```

**Thread states in dump**:

- **RUNNABLE**: Executing or ready to execute
- **BLOCKED**: Waiting to acquire lock
- **WAITING**: Waiting indefinitely (wait(), join())
- **TIMED_WAITING**: Waiting with timeout (sleep(), wait(timeout))

### jstat (GC and Memory Stats)

jstat provides real-time JVM statistics.

**Pattern**:

```bash
# Monitor GC activity (every 1 second)
jstat -gc <pid> 1000

# Columns show:
# - S0C, S1C: Survivor space capacity
# - S0U, S1U: Survivor space used
# - EC: Eden capacity
# - EU: Eden used
# - OC: Old generation capacity
# - OU: Old generation used
# - MC: Metaspace capacity
# - MU: Metaspace used
# - YGC: Young GC count
# - YGCT: Young GC time
# - FGC: Full GC count
# - FGCT: Full GC time

# GC pause time percentage
jstat -gcutil <pid> 1000

# Class loading statistics
jstat -class <pid> 1000
```

**Interpreting jstat output**:

- **High YGC frequency**: Young generation too small or excessive allocation
- **Increasing FGC count**: Memory leak or old generation too small
- **High YGCT/FGCT**: GC pauses affecting application
- **OU near OC**: Old generation approaching capacity

### CPU vs Memory Profiling

Understanding profiling types guides optimization strategy.

**CPU Profiling**:

- **Measures**: Method execution time
- **Identifies**: Computational hotspots
- **Solutions**: Algorithm optimization, caching, parallelization
- **Tools**: JFR, VisualVM, async-profiler

**Memory Profiling**:

- **Measures**: Object allocations and retention
- **Identifies**: Allocation hotspots, memory leaks
- **Solutions**: Object pooling (rarely), caching, data structure optimization
- **Tools**: JFR, VisualVM, jmap, Memory Analyzer

**Sampling vs Instrumentation**:

| Approach            | Overhead | Accuracy | Use Case              |
| ------------------- | -------- | -------- | --------------------- |
| **Sampling**        | Low      | Estimate | Production profiling  |
| **Instrumentation** | High     | Exact    | Development deep-dive |

## JVM Tuning

Configure JVM parameters to match workload characteristics.

### Heap Size Configuration

Set initial and maximum heap sizes appropriately.

**Pattern**:

```bash
# Set heap size
java -Xms2g -Xmx2g MyApplication

# -Xms: Initial heap size (start size)
# -Xmx: Maximum heap size (hard limit)

# Best practice: Set -Xms == -Xmx
# - Avoids heap resizing overhead
# - Predictable memory usage
# - Faster startup
```

**Heap sizing guidelines**:

- **Web applications**: 2-8GB typical
- **Data processing**: Based on dataset size
- **Rule of thumb**: 25-50% of system RAM
- **Leave headroom**: OS and other processes need memory

**Monitoring heap usage**:

```bash
# Current heap usage
jstat -gc <pid>

# If OU (old generation used) approaches OC (old generation capacity):
# - Increase heap size
# - Investigate memory leaks
# - Optimize object retention
```

### GC Selection

Choose garbage collector based on application requirements.

**G1GC (Garbage-First GC)** - Default since Java 9:

```bash
# G1GC (default, no flag needed)
java -XX:+UseG1GC -Xmx4g MyApplication

# Characteristics:
# - Low-latency collector
# - Predictable pause times
# - Good for large heaps (>4GB)
# - Pause time goal: -XX:MaxGCPauseMillis=200 (default 200ms)

# Tuning G1GC
java -XX:+UseG1GC \
     -XX:MaxGCPauseMillis=100 \
     -XX:G1HeapRegionSize=16m \
     -Xmx8g \
     MyApplication
```

**ZGC (Z Garbage Collector)** - Ultra-low latency (Java 15+):

```bash
# ZGC for sub-10ms pauses
java -XX:+UseZGC -Xmx16g MyApplication

# Characteristics:
# - Sub-10ms pause times (even with large heaps)
# - Scalable (handles multi-TB heaps)
# - Concurrent (minimal STW pauses)
# - Overhead: ~15% throughput cost
# - Use when: Latency more important than throughput
```

**Shenandoah GC** - Low-latency alternative:

```bash
# Shenandoah (OpenJDK builds)
java -XX:+UseShenandoahGC -Xmx8g MyApplication

# Characteristics:
# - Similar to ZGC (low latency)
# - Available in OpenJDK
# - Concurrent evacuation
# - 10ms or less pause times
```

**Serial GC** - Single-threaded (small heaps only):

```bash
# Serial GC for single-core or small heaps
java -XX:+UseSerialGC -Xmx512m MyApplication

# Characteristics:
# - Single-threaded collection
# - Low overhead
# - Use for: <100MB heaps or single-core systems
```

**Parallel GC** - Throughput-focused:

```bash
# Parallel GC (pre-Java 9 default)
java -XX:+UseParallelGC -Xmx8g MyApplication

# Characteristics:
# - Maximizes throughput
# - Multi-threaded collection
# - Higher pause times than G1/ZGC
# - Use for: Batch processing, offline workloads
```

**GC selection matrix**:

| Workload             | Heap Size | GC Choice      | Reason                  |
| -------------------- | --------- | -------------- | ----------------------- |
| **Web applications** | <4GB      | G1GC           | Balanced performance    |
| **Web applications** | >4GB      | ZGC            | Predictable low latency |
| **Batch processing** | Any       | Parallel GC    | Maximum throughput      |
| **Trading systems**  | Any       | ZGC/Shenandoah | Ultra-low latency       |
| **Embedded/IoT**     | <100MB    | Serial GC      | Minimal overhead        |

### GC Tuning Parameters

Fine-tune garbage collection behavior.

**G1GC tuning**:

```bash
java -XX:+UseG1GC \
     -Xms4g -Xmx4g \
     -XX:MaxGCPauseMillis=100 \         # Target pause time
     -XX:G1HeapRegionSize=16m \          # Region size (1-32MB)
     -XX:InitiatingHeapOccupancyPercent=45 \  # When to start concurrent marking
     -XX:G1ReservePercent=10 \           # Reserve space for promotions
     -XX:ConcGCThreads=4 \               # Concurrent GC threads
     -XX:ParallelGCThreads=8 \           # Parallel GC threads
     MyApplication
```

**ZGC tuning**:

```bash
java -XX:+UseZGC \
     -Xms16g -Xmx16g \
     -XX:ConcGCThreads=4 \               # Concurrent GC threads
     -XX:ZCollectionInterval=120 \       # Collection interval (seconds)
     MyApplication
```

**GC logging** (essential for tuning):

```bash
# Modern GC logging (Java 9+)
java -Xlog:gc*:file=gc.log:time,uptime,level,tags \
     -XX:+UseG1GC \
     MyApplication

# Legacy GC logging (Java 8)
java -XX:+PrintGCDetails \
     -XX:+PrintGCDateStamps \
     -Xloggc:gc.log \
     MyApplication

# Analyze logs with:
# - GCViewer: https://github.com/chewiebug/GCViewer
# - GCEasy: https://gceasy.io/ (online analyzer)
```

### Metaspace Configuration

Configure native memory for class metadata.

**Pattern**:

```bash
# Set metaspace sizes
java -XX:MetaspaceSize=128m \
     -XX:MaxMetaspaceSize=512m \
     MyApplication

# -XX:MetaspaceSize: Initial metaspace size
# -XX:MaxMetaspaceSize: Maximum metaspace size (unlimited if not set)

# When to tune:
# - Applications with many classes (frameworks, dynamic languages)
# - Frequent class loading/unloading
# - Out of memory errors in metaspace
```

**Monitoring metaspace**:

```bash
# Check metaspace usage
jstat -gc <pid>

# MC: Metaspace capacity
# MU: Metaspace used

# If MU approaches MC:
# - Increase MaxMetaspaceSize
# - Investigate class leaks (unused classes retained)
```

### Common JVM Flags

Essential performance-related flags.

**String deduplication** (reduces memory for duplicate strings):

```bash
java -XX:+UseG1GC \
     -XX:+UseStringDeduplication \
     MyApplication

# Automatically dedups identical String objects
# Particularly effective for apps with many duplicate strings
```

**Compressed ordinary object pointers** (enabled by default for heaps <32GB):

```bash
java -XX:+UseCompressedOops \
     -Xmx30g \
     MyApplication

# Reduces object reference size from 8 bytes to 4 bytes
# Automatically disabled for heaps >32GB
```

**Explicit GC control**:

```bash
# Disable explicit GC (System.gc() calls)
java -XX:+DisableExplicitGC MyApplication

# Use when: Application or libraries call System.gc() unnecessarily
```

**JIT compiler tuning**:

```bash
# Tiered compilation (default since Java 8)
java -XX:+TieredCompilation \
     -XX:TieredStopAtLevel=4 \
     MyApplication

# Compilation threshold
java -XX:CompileThreshold=10000 \
     MyApplication

# Level 4: C2 compiler (maximum optimization)
```

## Memory Optimization

Optimize memory allocation and usage patterns.

### Object Allocation Patterns

Understand allocation costs and patterns.

**Allocation overhead**:

- **Eden allocation**: Fast (thread-local allocation buffers - TLAB)
- **Large objects**: Allocated directly in old generation
- **Excessive allocation**: Triggers frequent young GC

**Pattern analysis with JFR**:

```bash
# Profile allocations
java -XX:StartFlightRecording=duration=60s,filename=alloc.jfr \
     -XX:FlightRecorderOptions=stackdepth=128 \
     MyApplication

# In JMC, check:
# - Memory → Allocations → Top Allocating Classes
# - Identify allocation hotspots
```

**Reducing allocations**:

```java
// BEFORE: Allocates on every call
public String formatUser(User user) {
    return "User: " + user.getName() + " (" + user.getEmail() + ")";
}

// AFTER: Reuse StringBuilder
public String formatUser(User user) {
    StringBuilder sb = new StringBuilder(100);  // Pre-size
    sb.append("User: ")
      .append(user.getName())
      .append(" (")
      .append(user.getEmail())
      .append(")");
    return sb.toString();
}

// EVEN BETTER: Use String.format (clearer, single allocation)
public String formatUser(User user) {
    return String.format("User: %s (%s)", user.getName(), user.getEmail());
}
```

### Escape Analysis

JVM optimizes objects that don't escape method scope.

**Escape analysis optimizations**:

- **Stack allocation**: Object allocated on stack (no GC)
- **Scalar replacement**: Object fields promoted to local variables
- **Lock elision**: Removes unnecessary synchronization

**Example**:

```java
public int calculateTotal(List<Integer> numbers) {
    // Point object doesn't escape method
    Point temp = new Point(0, 0);  // May be stack-allocated

    for (Integer num : numbers) {
        temp.x += num;
        temp.y += num * 2;
    }

    return temp.x + temp.y;
}

// JVM may optimize to:
public int calculateTotal(List<Integer> numbers) {
    int temp_x = 0;  // Scalar replacement
    int temp_y = 0;

    for (Integer num : numbers) {
        temp_x += num;
        temp_y += num * 2;
    }

    return temp_x + temp_y;
}
```

**Enabling escape analysis** (enabled by default):

```bash
java -XX:+DoEscapeAnalysis MyApplication
```

### String Deduplication

Reduce memory for duplicate String objects.

**Pattern**:

```bash
# Enable string deduplication (G1GC only)
java -XX:+UseG1GC \
     -XX:+UseStringDeduplication \
     -XX:StringDeduplicationAgeThreshold=3 \
     MyApplication

# How it works:
# - Identifies String objects with identical char arrays
# - Points duplicate Strings to same char array
# - Reduces memory usage
# - Runs concurrently during GC
```

**When effective**:

- Applications with many duplicate strings
- JSON/XML parsing
- String-heavy data processing
- Configuration/metadata

**Monitoring deduplication**:

```bash
# GC logs show deduplication stats
# [String Deduplication: 1234M→456M(778M) 1.5ms]
# - Before: 1234M
# - After: 456M
# - Freed: 778M
# - Time: 1.5ms
```

### Off-Heap Memory

Use direct buffers for large data or I/O.

**Pattern**:

```java
import java.nio.ByteBuffer;

public class OffHeapExample {
    public static void main(String[] args) {
        // Heap-allocated buffer (subject to GC)
        ByteBuffer heapBuffer = ByteBuffer.allocate(1024 * 1024);

        // Direct buffer (off-heap, not GC'd)
        ByteBuffer directBuffer = ByteBuffer.allocateDirect(1024 * 1024);

        // Direct buffer advantages:
        // - Not subject to GC
        // - Native I/O operations (zero-copy)
        // - Large datasets without heap pressure

        // Direct buffer disadvantages:
        // - Allocation/deallocation slower than heap
        // - Not automatically freed (must be GC'd when no references)
        // - Limited by -XX:MaxDirectMemorySize

        // Use when:
        // - Large, long-lived buffers
        // - Network I/O
        // - File I/O
    }
}
```

**Configuring direct memory**:

```bash
java -XX:MaxDirectMemorySize=1g MyApplication
```

## Performance Patterns

Common patterns for improving application performance.

### Object Pooling (Usually Not Needed)

Object pooling rarely improves performance in modern Java.

**Why pooling is usually unnecessary**:

- **JVM optimization**: Escape analysis and TLAB make allocation fast
- **GC efficiency**: Young generation collection is very fast
- **Complexity**: Pooling adds code complexity and potential bugs

**When pooling MAY help**:

- Expensive object creation (database connections, threads)
- Large objects causing frequent GC
- Objects with expensive initialization

**Pattern** (connections - use HikariCP instead):

```java
// DON'T: Implement object pool manually
public class ObjectPool<T> {
    private final Queue<T> pool = new ConcurrentLinkedQueue<>();

    public T acquire() {
        T obj = pool.poll();
        return obj != null ? obj : createNew();
    }

    public void release(T obj) {
        pool.offer(obj);
    }
}

// DO: Use established libraries for pooling
// - HikariCP for database connections
// - Thread pools (ExecutorService)
// - Avoid pooling regular objects
```

### Lazy Initialization

Defer expensive initialization until needed.

**Pattern**:

```java
public class ReportService {
    private volatile ExpensiveResource resource;
    // => volatile: Ensures visibility across threads (happens-before guarantee)

    // => DOUBLE-CHECKED LOCKING: Lazy initialization with minimal locking
    public ExpensiveResource getResource() {
        if (resource == null) {
            // => FIRST CHECK: Fast path (no locking if already initialized)
            // => Most calls take this path after first initialization
            synchronized (this) {
                // => LOCK: Only if resource null (rare after initialization)
                if (resource == null) {
                    // => SECOND CHECK: Another thread may have initialized
                    // => Between first check and acquiring lock
                    resource = new ExpensiveResource();
                    // => EXPENSIVE: Only called once
                }
            }
        }
        return resource;
        // => Returns: Initialized resource (thread-safe)
    }
}

// => BETTER: Initialization-on-demand holder idiom
public class ReportService {
    private static class ResourceHolder {
        // => STATIC INITIALIZER: JVM guarantees thread-safe initialization
        static final ExpensiveResource INSTANCE = new ExpensiveResource();
        // => Initialized on first access to ResourceHolder class
        // => NOT initialized when ReportService loaded
    }

    public ExpensiveResource getResource() {
        return ResourceHolder.INSTANCE;
        // => LAZY: ResourceHolder loaded on first getResource() call
        // => THREAD-SAFE: JVM class loading is synchronized
        // => NO LOCKING: After initialization, direct field access
        // => PERFORMANCE: Zero synchronization overhead
    }
}
```

### Caching Strategies

Cache expensive computations or data retrieval.

**In-memory caching**:

```java
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class UserService {
    private final Map<String, User> cache = new ConcurrentHashMap<>();

    public User getUser(String userId) {
        // Check cache first
        User cached = cache.get(userId);
        if (cached != null) {
            return cached;
        }

        // Cache miss: fetch from database
        User user = fetchFromDatabase(userId);
        cache.put(userId, user);

        return user;
    }

    private User fetchFromDatabase(String userId) {
        // Expensive database query
        return null;
    }
}
```

**Caffeine cache** (high-performance caching library):

```xml
<dependency>
    <groupId>com.github.ben-manes.caffeine</groupId>
    <artifactId>caffeine</artifactId>
    <version>3.1.8</version>
</dependency>
```

```java
import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import java.time.Duration;

public class UserService {
    private final Cache<String, User> cache = Caffeine.newBuilder()
        .maximumSize(10_000)                     // Max entries
        .expireAfterWrite(Duration.ofMinutes(5)) // TTL
        .build();

    public User getUser(String userId) {
        return cache.get(userId, this::fetchFromDatabase);
    }

    private User fetchFromDatabase(String userId) {
        // Expensive database query
        return null;
    }
}
```

**Distributed caching** (Redis, Memcached):

- Use for multi-instance applications
- Share cache across application servers
- Persistence and replication
- External caching infrastructure

### Batch Processing

Process multiple items together to amortize overhead.

**Pattern**:

```java
public class OrderProcessor {
    private final List<Order> batch = new ArrayList<>(100);
    private static final int BATCH_SIZE = 100;

    public void processOrder(Order order) {
        batch.add(order);

        if (batch.size() >= BATCH_SIZE) {
            flushBatch();
        }
    }

    private void flushBatch() {
        if (batch.isEmpty()) return;

        // Single database round-trip for 100 orders
        saveBatchToDatabase(batch);
        batch.clear();
    }

    private void saveBatchToDatabase(List<Order> orders) {
        // Batch INSERT or UPDATE
        // Much faster than individual operations
    }
}
```

**Benefits**:

- Fewer database round-trips
- Amortized connection overhead
- Reduced network latency
- Better throughput

## Avoiding Performance Anti-Patterns

Common performance mistakes are documented in anti-patterns guide.

**See**: [Anti-Patterns](/en/learn/software-engineering/programming-languages/java/in-the-field/anti-patterns) for detailed coverage of:

- **Premature Optimization**: Optimizing without measurement
- **N+1 Queries**: Multiple database queries in loop (see sql-database.md for solution)
- **Excessive Object Creation**: Allocation hotspots in loops
- **String Concatenation in Loops**: Use StringBuilder instead

**Don't duplicate** - Reference anti-patterns.md for recognition signals, examples, and solutions.

## JMH (Java Microbenchmark Harness)

JMH provides accurate microbenchmarking for performance validation.

### Maven Setup

Add JMH dependency and plugin.

**pom.xml**:

```xml
<dependencies>
    <dependency>
        <groupId>org.openjdk.jmh</groupId>
        <artifactId>jmh-core</artifactId>
        <version>1.37</version>
    </dependency>
    <dependency>
        <groupId>org.openjdk.jmh</groupId>
        <artifactId>jmh-generator-annprocess</artifactId>
        <version>1.37</version>
        <scope>provided</scope>
    </dependency>
</dependencies>

<build>
    <plugins>
        <plugin>
            <groupId>org.apache.maven.plugins</groupId>
            <artifactId>maven-shade-plugin</artifactId>
            <version>3.5.1</version>
            <executions>
                <execution>
                    <phase>package</phase>
                    <goals><goal>shade</goal></goals>
                    <configuration>
                        <finalName>benchmarks</finalName>
                        <transformers>
                            <transformer implementation="org.apache.maven.plugins.shade.resource.ManifestResourceTransformer">
                                <mainClass>org.openjdk.jmh.Main</mainClass>
                            </transformer>
                        </transformers>
                    </configuration>
                </execution>
            </executions>
        </plugin>
    </plugins>
</build>
```

### Writing Benchmarks

Basic benchmark structure with annotations.

**Pattern**:

```java
import org.openjdk.jmh.annotations.*;
import java.util.concurrent.TimeUnit;

@BenchmarkMode(Mode.AverageTime)        // Measure average time
@OutputTimeUnit(TimeUnit.NANOSECONDS)   // Report in nanoseconds
@State(Scope.Thread)                     // Benchmark state
@Warmup(iterations = 5, time = 1)       // Warmup: 5 iterations, 1 sec each
@Measurement(iterations = 10, time = 1) // Measurement: 10 iterations, 1 sec each
@Fork(2)                                 // Run in 2 separate JVM processes
public class StringConcatBenchmark {

    @Param({"10", "100", "1000"})       // Test with different sizes
    private int size;

    private String[] strings;

    @Setup
    public void setup() {
        strings = new String[size];
        for (int i = 0; i < size; i++) {
            strings[i] = "str" + i;
        }
    }

    @Benchmark
    public String concatenateWithPlus() {
        String result = "";
        for (String s : strings) {
            result = result + s;  // Inefficient
        }
        return result;
    }

    @Benchmark
    public String concatenateWithStringBuilder() {
        StringBuilder sb = new StringBuilder();
        for (String s : strings) {
            sb.append(s);         // Efficient
        }
        return sb.toString();
    }
}
```

**Annotations explained**:

- **@BenchmarkMode**: Throughput, AverageTime, SampleTime, SingleShotTime, All
- **@OutputTimeUnit**: Time unit for results
- **@State**: Benchmark state scope (Thread, Benchmark, Group)
- **@Warmup**: JIT warm-up iterations
- **@Measurement**: Actual measurement iterations
- **@Fork**: JVM forks (isolates benchmarks)
- **@Param**: Parameter variation
- **@Setup**: Setup before benchmark
- **@TearDown**: Cleanup after benchmark

### Avoiding JVM Optimizations

Prevent JVM from optimizing away benchmark code.

**Dead code elimination**:

```java
@Benchmark
public void badBenchmark() {
    int result = expensiveCalculation();  // Result unused - may be eliminated!
}

@Benchmark
public int goodBenchmark() {
    return expensiveCalculation();  // Return value prevents elimination
}
```

**Constant folding**:

```java
@State(Scope.Thread)
public class ConstantBenchmark {
    private int x = 10;

    @Benchmark
    public int badBenchmark() {
        return 10 * 10;  // Constant - folded at compile time
    }

    @Benchmark
    public int goodBenchmark() {
        return x * x;  // State field prevents constant folding
    }
}
```

**Loop unrolling**:

```java
@Benchmark
public int sumArray(@State ArrayState state) {
    int sum = 0;
    for (int i = 0; i < state.array.length; i++) {
        sum += state.array[i];
    }
    return sum;  // Return prevents elimination
}
```

**Use Blackhole**:

```java
import org.openjdk.jmh.infra.Blackhole;

@Benchmark
public void consumeWithBlackhole(Blackhole bh) {
    int result = expensiveCalculation();
    bh.consume(result);  // Prevents dead code elimination
}
```

### Interpreting Results

Understanding benchmark output.

**Run benchmarks**:

```bash
mvn clean package
java -jar target/benchmarks.jar
```

**Sample output**:

```
Benchmark                                       (size)  Mode  Cnt      Score      Error  Units
StringConcatBenchmark.concatenateWithPlus           10  avgt   20     45.123 ±    2.456  ns/op
StringConcatBenchmark.concatenateWithPlus          100  avgt   20   2345.678 ±   89.123  ns/op
StringConcatBenchmark.concatenateWithPlus         1000  avgt   20 234567.890 ± 5678.901  ns/op
StringConcatBenchmark.concatenateWithStringBuilder  10  avgt   20     12.345 ±    0.567  ns/op
StringConcatBenchmark.concatenateWithStringBuilder 100  avgt   20    123.456 ±    5.678  ns/op
StringConcatBenchmark.concatenateWithStringBuilder 1000 avgt  20   1234.567 ±   56.789  ns/op
```

**Interpreting columns**:

- **Benchmark**: Method name
- **(size)**: Parameter value
- **Mode**: Benchmark mode (avgt = average time)
- **Cnt**: Number of measurement iterations
- **Score**: Average result
- **Error**: 99.9% confidence interval
- **Units**: Time unit

**Analysis**:

- StringBuilder is 3-4x faster for size=10
- StringBuilder is 19x faster for size=100
- StringBuilder is 190x faster for size=1000
- String concatenation has O(n²) complexity (quadratic)
- StringBuilder has O(n) complexity (linear)

### Common Pitfalls

Mistakes that invalidate benchmarks.

**Insufficient warmup**:

```java
// BAD: No warmup - includes JIT compilation time
@Warmup(iterations = 0)

// GOOD: Allow JIT to optimize
@Warmup(iterations = 5, time = 1)
```

**Non-isolated benchmarks**:

```java
// BAD: Single JVM - benchmarks affect each other
@Fork(0)

// GOOD: Isolated JVM processes
@Fork(2)
```

**Shared mutable state**:

```java
// BAD: Shared state between threads
@State(Scope.Benchmark)
public class BadBenchmark {
    private int counter = 0;  // Race condition!

    @Benchmark
    public void increment() {
        counter++;
    }
}

// GOOD: Thread-local state
@State(Scope.Thread)
public class GoodBenchmark {
    private int counter = 0;  // Each thread has own counter

    @Benchmark
    public void increment() {
        counter++;
    }
}
```

## Database Performance

Performance optimization for database operations.

### Connection Pooling

Connection pooling dramatically improves database performance.

**See**: [Working with SQL Databases](/en/learn/software-engineering/programming-languages/java/in-the-field/sql-database) for comprehensive HikariCP configuration.

**Key points**:

- Use HikariCP for connection pooling (fastest pool)
- Configure pool size based on workload
- Set connection timeout and idle timeout
- Enable connection validation

**Why it matters**:

- Creating connection: ~50-100ms
- Getting from pool: ~1ms
- **6-10x performance improvement**

### Query Optimization

Optimize queries for minimal database load.

**Use prepared statements** (prevents SQL injection and improves performance):

```java
// BAD: String concatenation (SQL injection + no caching)
String sql = "SELECT * FROM users WHERE id = " + userId;

// GOOD: Prepared statement (secure + query plan cached)
String sql = "SELECT * FROM users WHERE id = ?";
PreparedStatement stmt = conn.prepareStatement(sql);
stmt.setString(1, userId);
```

**Batch operations** (reduce round-trips):

```java
// BAD: Individual inserts (1000 round-trips)
for (User user : users) {
    insertUser(user);  // Separate query each time
}

// GOOD: Batch insert (10 round-trips)
String sql = "INSERT INTO users (name, email) VALUES (?, ?)";
PreparedStatement stmt = conn.prepareStatement(sql);

for (User user : users) {
    stmt.setString(1, user.getName());
    stmt.setString(2, user.getEmail());
    stmt.addBatch();

    if (users.indexOf(user) % 100 == 0) {
        stmt.executeBatch();
    }
}
stmt.executeBatch();
```

**Avoid N+1 queries** (fetch related data in single query):

```java
// BAD: N+1 queries (1 + N database calls)
List<Order> orders = getOrders();
for (Order order : orders) {
    Customer customer = getCustomer(order.getCustomerId());  // N queries!
}

// GOOD: Join fetch (1 database call)
String sql = """
    SELECT o.*, c.*
    FROM orders o
    JOIN customers c ON o.customer_id = c.id
    """;
// Single query returns all data
```

**See**: [Anti-Patterns](/en/learn/software-engineering/programming-languages/java/in-the-field/anti-patterns) for detailed N+1 query examples and solutions.

### Index Usage

Ensure queries use appropriate indexes.

**Analyze query plans**:

```sql
EXPLAIN SELECT * FROM users WHERE email = 'alice@example.com';

-- Check for:
-- - Sequential scan (BAD: no index used)
-- - Index scan (GOOD: index used)
```

**Create indexes for frequently queried columns**:

```sql
CREATE INDEX idx_users_email ON users(email);
CREATE INDEX idx_orders_user_id ON orders(user_id);
CREATE INDEX idx_orders_created_at ON orders(created_at);
```

### Caching

Cache database query results.

**Query-level caching**:

```java
public class UserRepository {
    private final Cache<String, User> cache = Caffeine.newBuilder()
        .maximumSize(10_000)
        .expireAfterWrite(Duration.ofMinutes(5))
        .build();

    public User findById(String userId) {
        return cache.get(userId, this::fetchFromDatabase);
    }

    private User fetchFromDatabase(String userId) {
        // Database query
        return null;
    }
}
```

**Second-level cache** (JPA/Hibernate):

```java
@Entity
@Cacheable
@org.hibernate.annotations.Cache(usage = CacheConcurrencyStrategy.READ_WRITE)
public class User {
    // Entity fields
}
```

## Best Practices

### Measure Before Optimizing

Never optimize without profiling data.

**Workflow**:

1. **Profile**: Use JFR or VisualVM to find hotspots
2. **Analyze**: Identify actual bottlenecks
3. **Optimize**: Improve identified hotspots
4. **Verify**: Re-profile to confirm improvement

**Don't**:

- Guess at performance issues
- Optimize non-bottlenecks
- Sacrifice readability for speculative gains

### Focus on Hotspots

Apply 80/20 rule: 80% of time spent in 20% of code.

**Strategy**:

- Profile application under realistic load
- Sort methods by execution time
- Optimize top 5-10 methods
- Verify improvements with benchmarks

**Example hotspot analysis**:

```
Method                      Time    %
-----------------------------------------
processOrder()              45.2s   45%  ← HOTSPOT
validateUser()              12.3s   12%  ← HOTSPOT
calculateTotal()            8.7s    9%   ← HOTSPOT
formatOutput()              6.2s    6%
logActivity()               4.1s    4%
```

Focus on top 3 methods (66% of execution time).

### Trade-offs

Performance optimization involves trade-offs.

**Memory vs CPU**:

- Caching: Uses memory to reduce CPU/I/O
- Compression: Uses CPU to reduce memory/bandwidth

**Latency vs Throughput**:

- Low latency: ZGC (lower throughput)
- High throughput: Parallel GC (higher latency)

**Complexity vs Performance**:

- Simple code: Easier to maintain, may be slower
- Optimized code: Faster, harder to understand

**Guideline**: Optimize only when measurements justify complexity.

### Performance Testing in Production-Like Environments

Test performance with realistic conditions.

**Requirements**:

- **Similar hardware**: CPU cores, memory, disk speed
- **Similar data volume**: Production-scale datasets
- **Similar load patterns**: Realistic user behavior
- **Similar network**: Latency, bandwidth

**Why it matters**:

- Local machine: Fast CPU, low latency, no load
- Production: Shared resources, network latency, high concurrency
- Optimizations that work locally may not help in production

**Load testing tools**:

- JMeter: HTTP load testing
- Gatling: Scala-based load testing
- K6: JavaScript load testing

## Related Content

- [Anti-Patterns](/en/learn/software-engineering/programming-languages/java/in-the-field/anti-patterns) - Performance anti-patterns (premature optimization, N+1 queries, excessive allocation)
- [Working with SQL Databases](/en/learn/software-engineering/programming-languages/java/in-the-field/sql-database) - Connection pooling with HikariCP, query optimization
- [Concurrency and Parallelism](/en/learn/software-engineering/programming-languages/java/in-the-field/concurrency-and-parallelism) - Thread pool sizing, parallel streams, virtual threads

---

**Last Updated**: 2026-02-03
**Java Version**: 17+ (baseline), 21+ (recommended, includes virtual threads and ZGC improvements)
