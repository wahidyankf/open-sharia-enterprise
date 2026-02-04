---
title: "Memory Management"
date: 2026-02-04T00:00:00+07:00
draft: false
description: "Memory management in Go: GC overview, stack vs heap allocation, escape analysis, memory profiling, GOMEMLIMIT tuning"
weight: 10000076
tags: ["golang", "memory-management", "gc", "garbage-collection", "escape-analysis", "performance", "production"]
---

## Why Memory Management Matters

Memory management affects application performance, stability, and infrastructure costs. Excessive allocations cause frequent garbage collection pauses degrading response times. Memory leaks lead to OOM crashes. Understanding Go's garbage collector, stack vs heap allocation, escape analysis, and memory profiling optimizes memory usage, reduces GC overhead, and ensures stable production systems.

**Core benefits**:

- **Lower GC overhead**: Fewer pauses, better latency
- **Reduced memory footprint**: Lower infrastructure costs
- **Crash prevention**: Avoid OOM (out-of-memory) errors
- **Stable performance**: Consistent response times

**Problem**: Go's automatic garbage collector simplifies memory management but provides limited control. Manual tuning requires understanding GC mechanics, allocation patterns, and GOMEMLIMIT configuration.

**Solution**: Start with Go's default GC behavior and memory profiling to identify issues, then apply optimization techniques (reduce allocations, tune GOMEMLIMIT, understand escape analysis) based on profiling data.

## Go's Garbage Collector Overview

Go uses concurrent mark-and-sweep garbage collector with generational optimization.

**GC Phases**:

1. **Mark Phase** (STW - Stop The World): Scan roots (globals, stack), mark reachable objects
2. **Concurrent Mark**: Mark remaining objects while application runs
3. **Sweep Phase**: Reclaim unmarked memory

**Pattern: GC Metrics**:

```go
package main

import (
    "fmt"
    "runtime"
    // => Standard library for runtime statistics
    // => Provides GC metrics
    "time"
)

func printGCStats() {
    // => Prints garbage collector statistics

    var stats runtime.MemStats
    // => MemStats contains memory and GC statistics

    runtime.ReadMemStats(&stats)
    // => Populates stats with current values
    // => Includes allocations, GC runs, pause times

    fmt.Printf("Heap Alloc: %d MB\n", stats.HeapAlloc/1024/1024)
    // => Current heap memory allocated (in-use objects)
    // => HeapAlloc increases on allocation, decreases on GC

    fmt.Printf("Total Alloc: %d MB\n", stats.TotalAlloc/1024/1024)
    // => Cumulative bytes allocated (ever)
    // => Never decreases (includes freed objects)

    fmt.Printf("Sys: %d MB\n", stats.Sys/1024/1024)
    // => Total memory obtained from OS
    // => Includes heap, stack, metadata

    fmt.Printf("NumGC: %d\n", stats.NumGC)
    // => Number of GC cycles completed
    // => Increases on each GC run

    fmt.Printf("PauseTotal: %v\n", time.Duration(stats.PauseTotalNs))
    // => Total GC pause time
    // => Cumulative stop-the-world time

    if stats.NumGC > 0 {
        fmt.Printf("Last Pause: %v\n", time.Duration(stats.PauseNs[(stats.NumGC+255)%256]))
        // => Most recent GC pause duration
        // => PauseNs is circular buffer (256 entries)
    }
}

func allocateMemory() {
    // => Simulates memory allocations

    data := make([][]byte, 100)
    // => Slice of byte slices

    for i := 0; i < 100; i++ {
        data[i] = make([]byte, 1024*1024)
        // => Allocates 1 MB per iteration
        // => Total: 100 MB allocated
    }

    // data goes out of scope (eligible for GC)
}

func main() {
    fmt.Println("Before allocations:")
    printGCStats()
    // => Baseline memory stats

    allocateMemory()
    // => Allocates 100 MB

    runtime.GC()
    // => Forces garbage collection
    // => Normally automatic (don't call in production)

    fmt.Println("\nAfter GC:")
    printGCStats()
    // => Memory reclaimed by GC
    // => HeapAlloc lower than before GC
}
```

**Output**:

```
Before allocations:
Heap Alloc: 0 MB
Total Alloc: 0 MB
Sys: 8 MB
NumGC: 0
PauseTotal: 0s

After GC:
Heap Alloc: 1 MB
Total Alloc: 100 MB
Sys: 110 MB
NumGC: 1
PauseTotal: 2ms
Last Pause: 2ms
```

## Stack vs Heap Allocation

Go compiler decides allocation location based on escape analysis.

**Stack Allocation (Fast)**:

- Local variables that don't escape function scope
- No GC overhead (automatically freed on function return)
- Faster than heap (no pointer indirection)
- Limited size (typically 1-8 MB per goroutine)

**Heap Allocation (Slower)**:

- Variables that escape function scope (returned pointers, stored in globals, captured in closures)
- GC managed (contributes to GC overhead)
- Unlimited size (constrained by system memory)

**Pattern: Stack Allocation Example**:

```go
package main

import "fmt"

func stackExample() {
    // => All allocations stay on stack

    x := 42
    // => x allocated on stack
    // => Escape analysis: x does not escape

    y := 100
    // => y allocated on stack

    sum := x + y
    // => sum allocated on stack

    fmt.Println(sum)
    // => sum doesn't escape (fmt.Println doesn't store it)
    // => All variables freed when function returns
}
```

**Check with escape analysis**:

```bash
go build -gcflags='-m' main.go
# => Output: main.go:7:2: x does not escape
# => Stack allocation (efficient)
```

**Pattern: Heap Allocation Example**:

```go
package main

func heapExample() *int {
    // => Returns pointer (escapes to heap)

    x := 42
    // => x escapes to heap
    // => Outlives function (pointer returned)

    return &x
    // => Returns pointer to x
    // => x must be on heap (GC managed)
}
```

**Escape analysis**:

```bash
go build -gcflags='-m' main.go
# => Output: main.go:6:2: x escapes to heap
# => Heap allocation (GC overhead)
```

**Pattern: Reducing Heap Escapes**:

```go
// Slow: heap allocation (pointer returned)
func createUserSlow() *User {
    user := User{Name: "John", Age: 30}
    // => user escapes to heap (pointer returned)
    return &user
}

// Fast: stack allocation (value returned)
func createUserFast() User {
    user := User{Name: "John", Age: 30}
    // => user stays on stack (value copied)
    return user
    // => No GC overhead
}
```

## Memory Profiling with pprof

Go's pprof identifies memory allocation hotspots.

**Pattern: Memory Profile**:

```go
package main

import (
    "os"
    "runtime/pprof"
    // => Standard library memory profiler
)

func main() {
    // Run application code
    allocateData()

    // Write heap profile
    f, err := os.Create("mem.prof")
    // => Creates memory profile file

    if err != nil {
        panic(err)
    }
    defer f.Close()

    pprof.WriteHeapProfile(f)
    // => Writes heap allocations to file
    // => Shows allocation sites and sizes
}

func allocateData() {
    // => Function to profile

    data := make([]byte, 100*1024*1024)
    // => Allocates 100 MB
    // => pprof tracks this allocation

    _ = data
    // => Use data to prevent optimization
}
```

**Analyzing memory profile**:

```bash
# Run program to generate profile
go run main.go

# Analyze memory profile
go tool pprof mem.prof

# Show top allocators
(pprof) top
# => Output: Top functions by allocated memory
# => Shows cumulative allocations

# Show allocation sites
(pprof) list allocateData
# => Line-by-line allocation breakdown
# => Identifies specific allocation lines

# Show call graph
(pprof) web
# => Opens browser with allocation graph
# => Visual representation of allocation paths
```

**Pattern: Live Memory Profiling**:

```go
package main

import (
    "net/http"
    _ "net/http/pprof"
    // => Enables pprof HTTP endpoints
)

func main() {
    // Application endpoints
    http.HandleFunc("/", handler)

    // Start server (pprof endpoints automatically registered)
    http.ListenAndServe(":8080", nil)
    // => /debug/pprof/heap for memory profile
}

func handler(w http.ResponseWriter, r *http.Request) {
    // => Normal HTTP handler
    // => pprof tracks allocations

    data := processRequest()
    // => Allocates memory
    // => pprof records allocation site

    w.Write(data)
}

func processRequest() []byte {
    return make([]byte, 1024)
}
```

**Collecting live profile**:

```bash
# Memory profile from running server
curl http://localhost:8080/debug/pprof/heap > heap.prof

# Analyze
go tool pprof heap.prof
(pprof) top
# => Shows current heap allocations
```

## Production Pattern: GOMEMLIMIT Tuning

GOMEMLIMIT controls when GC runs based on memory target.

**Pattern: Setting GOMEMLIMIT**:

```bash
# Set memory limit to 512 MB
export GOMEMLIMIT=512MiB

# Run application
go run main.go
# => GC runs more frequently as memory approaches 512 MB
# => Prevents memory from exceeding limit
```

**GOMEMLIMIT Behavior**:

- **Below limit**: GC runs less frequently (better performance)
- **Near limit**: GC runs more frequently (prevents OOM)
- **At limit**: Aggressive GC to stay under limit

**Pattern: Monitoring GOMEMLIMIT**:

```go
package main

import (
    "fmt"
    "runtime/debug"
    // => debug package for GC tuning
)

func main() {
    // Read current memory limit
    limit := debug.SetMemoryLimit(-1)
    // => -1 reads current limit without changing
    // => Returns limit in bytes

    if limit == math.MaxInt64 {
        // => No limit set (default)
        fmt.Println("No memory limit set")
    } else {
        fmt.Printf("Memory limit: %d MB\n", limit/1024/1024)
        // => Output: Memory limit: 512 MB
    }

    // Set memory limit programmatically
    newLimit := 512 * 1024 * 1024
    // => 512 MB

    debug.SetMemoryLimit(newLimit)
    // => Sets memory limit to 512 MB
    // => GC adjusts behavior to stay under limit
}
```

**When to use GOMEMLIMIT**:

- **Container environments**: Match container memory limit (e.g., Kubernetes)
- **Shared hosting**: Prevent one app from consuming all memory
- **OOM prevention**: Force GC before OOM killer activates
- **Predictable latency**: Trade memory for consistent GC pauses

**Example for Kubernetes**:

```yaml
apiVersion: v1
kind: Pod
spec:
  containers:
    - name: app
      image: myapp:latest
      resources:
        limits:
          memory: "512Mi" # Container limit
      env:
        - name: GOMEMLIMIT
          value: "450MiB" # GOMEMLIMIT 88% of container limit (headroom for non-heap)
```

## Production Best Practices

**Set GOMEMLIMIT to 90% of container memory**:

```yaml
# GOOD: Leave headroom for non-heap memory
resources:
  limits:
    memory: "512Mi"
env:
- name: GOMEMLIMIT
  value: "460MiB"  # 90% of 512 MB

# BAD: GOMEMLIMIT equals container limit
env:
- name: GOMEMLIMIT
  value: "512MiB"  # No headroom (may OOM from stack, metadata)
```

**Profile memory in production**:

```go
// GOOD: Enable pprof in production
import _ "net/http/pprof"

go func() {
    http.ListenAndServe("localhost:6060", nil)  // Internal port
}()
// => /debug/pprof/heap for memory profile
```

**Avoid unnecessary heap allocations**:

```go
// GOOD: Prefer stack allocation
func process(data []byte) int {
    sum := 0  // Stack allocation
    for _, b := range data {
        sum += int(b)
    }
    return sum  // Return value (no pointer)
}

// BAD: Unnecessary heap allocation
func processBad(data []byte) *int {
    sum := 0  // Heap allocation (pointer returned)
    for _, b := range data {
        sum += int(b)
    }
    return &sum  // Escapes to heap
}
```

**Use sync.Pool for temporary buffers**:

```go
// GOOD: Reuse buffers (reduce allocations)
var bufferPool = sync.Pool{
    New: func() interface{} {
        return new(bytes.Buffer)
    },
}

func process() {
    buf := bufferPool.Get().(*bytes.Buffer)
    defer func() {
        buf.Reset()
        bufferPool.Put(buf)
    }()
    // Use buf...
}

// BAD: Allocate new buffer every call
func processBad() {
    buf := new(bytes.Buffer)  // GC pressure
    // Use buf...
}
```

**Monitor GC metrics**:

```go
// Track GC pause times
var stats runtime.MemStats
runtime.ReadMemStats(&stats)

avgPause := time.Duration(stats.PauseTotalNs / uint64(stats.NumGC))
// => Average GC pause
// => Target: < 10ms for low-latency apps

if avgPause > 10*time.Millisecond {
    // Alert: GC pauses too high
}
```

## Trade-offs: GC Tuning Strategies

**Comparison table**:

| Strategy                    | Memory Usage | GC Frequency        | Latency            | Use Case                   |
| --------------------------- | ------------ | ------------------- | ------------------ | -------------------------- |
| **Default GC**              | Dynamic      | Automatic           | Variable           | General applications       |
| **GOMEMLIMIT**              | Bounded      | Frequent near limit | Consistent         | Containers, shared hosting |
| **debug.SetGCPercent(50)**  | Lower        | Frequent            | Lower tail latency | Low-latency services       |
| **debug.SetGCPercent(200)** | Higher       | Infrequent          | Higher throughput  | Batch processing           |

**When to use default GC**:

- General applications (no special requirements)
- Development and testing
- Unlimited memory available

**When to use GOMEMLIMIT**:

- Container environments (Kubernetes, Docker)
- Shared hosting (prevent memory exhaustion)
- Predictable memory usage required

**When to tune GC percentage**:

- Low-latency services (SetGCPercent(50) for frequent GC)
- Batch processing (SetGCPercent(200) for less frequent GC)
- Fine-tuning after profiling

## Summary

Memory management affects performance, stability, and costs. Go's concurrent garbage collector automatically manages memory but provides tuning options. Stack allocation is faster than heap (no GC overhead). Use escape analysis to identify heap allocations, memory profiling with pprof to find hotspots, and GOMEMLIMIT to bound memory usage in containers. Set GOMEMLIMIT to 90% of container memory, reduce unnecessary heap allocations, use sync.Pool for temporary buffers, and monitor GC metrics in production. Profile before tuning and focus on hot paths identified by pprof.

**Key takeaways**:

- Go uses concurrent mark-and-sweep garbage collector
- Stack allocation is faster than heap (no GC overhead)
- Use escape analysis (go build -gcflags='-m') to identify heap allocations
- Profile memory with pprof (cpu.prof, mem.prof, /debug/pprof/heap)
- Set GOMEMLIMIT to 90% of container memory (prevents OOM)
- Reduce heap allocations to lower GC overhead
- Use sync.Pool to reuse temporary buffers
- Monitor GC pause times (target < 10ms for low-latency apps)
