---
title: "Performance Optimization"
date: 2026-02-04T00:00:00+07:00
draft: false
description: "Performance optimization in Go: profiling with pprof, benchmarking, memory allocation reduction, escape analysis"
weight: 10000074
tags: ["golang", "performance", "profiling", "pprof", "benchmarking", "optimization", "production"]
---

## Why Performance Optimization Matters

Performance optimization reduces response times, increases throughput, and lowers infrastructure costs. Slow applications lead to poor user experience, lost revenue, and higher cloud bills. Understanding profiling, benchmarking, memory allocation reduction, and escape analysis identifies bottlenecks, eliminates waste, and ensures efficient resource usage in production systems.

**Core benefits**:

- **Lower latency**: Faster response times (better UX)
- **Higher throughput**: Serve more requests per second
- **Cost reduction**: Less CPU/memory = smaller infrastructure
- **Scalability**: Efficient code handles more load

**Problem**: Standard library provides pprof for profiling and testing.B for benchmarks but no automatic optimization recommendations. Manual profiling analysis requires expertise.

**Solution**: Start with pprof CPU/memory profiling and testing.B benchmarks to identify bottlenecks, then apply optimization techniques (reduce allocations, use sync.Pool, understand escape analysis) based on profiling data.

## Standard Library: Profiling with pprof

Go's pprof profiler identifies CPU and memory hotspots.

**Pattern from standard library**:

```go
package main

import (
    "fmt"
    "os"
    "runtime/pprof"
    // => Standard library profiler
    // => CPU and memory profiling
    "time"
)

func main() {
    // Start CPU profiling
    cpuFile, err := os.Create("cpu.prof")
    // => Creates cpu.prof file
    // => Stores CPU profile data

    if err != nil {
        fmt.Println("Error creating CPU profile:", err)
        return
    }
    defer cpuFile.Close()

    pprof.StartCPUProfile(cpuFile)
    // => Starts CPU profiling
    // => Samples CPU usage every 10ms
    // => Records which functions consuming CPU

    defer pprof.StopCPUProfile()
    // => Stops CPU profiling
    // => Writes profile data to file

    // Run application code
    performWork()
    // => Function to profile
    // => pprof records CPU time spent in this function

    // Write memory profile
    memFile, err := os.Create("mem.prof")
    // => Creates mem.prof file

    if err != nil {
        fmt.Println("Error creating memory profile:", err)
        return
    }
    defer memFile.Close()

    pprof.WriteHeapProfile(memFile)
    // => Writes memory profile
    // => Shows memory allocations
}

func performWork() {
    // => Simulates CPU-intensive work

    data := make([]int, 0, 1000000)
    // => Allocates slice with capacity
    // => pprof tracks this allocation

    for i := 0; i < 1000000; i++ {
        data = append(data, i*2)
        // => CPU work (multiplication)
        // => pprof measures time spent here
    }

    time.Sleep(100 * time.Millisecond)
    // => Simulate other work
}
```

**Analyzing profiles**:

```bash
# Run program to generate profiles
go run main.go

# Analyze CPU profile
go tool pprof cpu.prof
# => Interactive pprof shell
# => Commands: top, list, web, pdf

# Show top CPU consumers
(pprof) top
# => Output: Top 10 functions by CPU time
# => Cumulative time includes callees

# Show line-by-line breakdown
(pprof) list performWork
# => Shows CPU time per line in performWork()
# => Identifies bottleneck lines

# Analyze memory profile
go tool pprof mem.prof
# => Memory allocations analysis

(pprof) top
# => Top memory allocators
# => Shows allocation size and count
```

**Pattern: HTTP Server Profiling**:

```go
package main

import (
    "fmt"
    "net/http"
    _ "net/http/pprof"
    // => Blank import registers pprof HTTP handlers
    // => Exposes /debug/pprof/* endpoints
)

func handler(w http.ResponseWriter, r *http.Request) {
    // => Example HTTP handler
    // => pprof profiles all requests

    result := compute()
    // => CPU-intensive work
    // => pprof tracks time spent

    fmt.Fprintf(w, "Result: %d", result)
}

func compute() int {
    // => CPU-intensive computation
    sum := 0
    for i := 0; i < 10000000; i++ {
        sum += i
    }
    return sum
}

func main() {
    http.HandleFunc("/", handler)

    fmt.Println("Server starting on :8080")
    fmt.Println("Profiler available at http://localhost:8080/debug/pprof/")
    // => Access pprof web UI
    // => /debug/pprof/ shows profile types
    // => /debug/pprof/profile for CPU (30s sample)
    // => /debug/pprof/heap for memory

    http.ListenAndServe(":8080", nil)
}
```

**Collecting profiles from running server**:

```bash
# CPU profile (30-second sample)
curl http://localhost:8080/debug/pprof/profile > cpu.prof
# => Samples CPU for 30 seconds
# => Saves to cpu.prof file

# Memory profile (current heap)
curl http://localhost:8080/debug/pprof/heap > mem.prof
# => Snapshot of current allocations

# Goroutine profile (goroutine stacks)
curl http://localhost:8080/debug/pprof/goroutine > goroutine.prof
# => Shows all goroutines and their stacks
# => Identifies goroutine leaks

# Analyze
go tool pprof cpu.prof
```

**Limitations for production profiling**:

- Manual profile collection (no continuous profiling)
- No production-safe sampling (30s CPU profiling blocks)
- No automated recommendations (manual analysis required)
- No historical data (point-in-time snapshots only)

## Standard Library: Benchmarking with testing.B

Go's testing package provides benchmark framework.

**Pattern: Basic Benchmark**:

```go
package main

import (
    "testing"
    // => Standard library testing package
    // => testing.B for benchmarks
)

func Fibonacci(n int) int {
    // => Function to benchmark
    // => Calculates nth Fibonacci number

    if n <= 1 {
        return n
    }
    return Fibonacci(n-1) + Fibonacci(n-2)
    // => Recursive implementation (slow for large n)
}

func BenchmarkFibonacci10(b *testing.B) {
    // => Benchmark function naming: Benchmark[Name]
    // => b *testing.B provides benchmark controls

    for i := 0; i < b.N; i++ {
        // => b.N is number of iterations
        // => testing package adjusts N to get reliable timing
        // => Runs function multiple times

        Fibonacci(10)
        // => Function under test
        // => Executed b.N times
    }
    // => testing framework measures total time
    // => Reports ns/op (nanoseconds per operation)
}

func BenchmarkFibonacci20(b *testing.B) {
    // => Benchmark with larger input

    for i := 0; i < b.N; i++ {
        Fibonacci(20)
        // => Slower than Fibonacci(10)
        // => Benchmarks show difference
    }
}
```

**Running benchmarks**:

```bash
# Run all benchmarks
go test -bench=.
# => Output:
# => BenchmarkFibonacci10-8   5000000   250 ns/op
# => BenchmarkFibonacci20-8   50000     28000 ns/op
# => -8: GOMAXPROCS (CPU cores)
# => 5000000: iterations (b.N)
# => 250 ns/op: nanoseconds per operation

# Run specific benchmark
go test -bench=BenchmarkFibonacci10
# => Only runs Fibonacci10 benchmark

# Run with memory allocation tracking
go test -bench=. -benchmem
# => Output includes:
# => 250 ns/op   64 B/op   2 allocs/op
# => 64 B/op: bytes allocated per operation
# => 2 allocs/op: allocations per operation
```

**Pattern: Comparative Benchmarks**:

```go
package main

import (
    "strings"
    "testing"
)

// Slow version: string concatenation with +
func concatSlow(strs []string) string {
    // => Uses + operator (creates new string each time)

    result := ""
    for _, s := range strs {
        result += s
        // => Allocates new string on each iteration
        // => O(n²) allocations
    }
    return result
}

// Fast version: strings.Builder
func concatFast(strs []string) string {
    // => Uses strings.Builder (pre-allocated buffer)

    var builder strings.Builder
    // => builder is strings.Builder (efficient concatenation)

    for _, s := range strs {
        builder.WriteString(s)
        // => Appends to buffer (no allocations)
    }
    return builder.String()
    // => Final allocation for result string
}

func BenchmarkConcatSlow(b *testing.B) {
    strs := []string{"hello", "world", "foo", "bar"}

    for i := 0; i < b.N; i++ {
        concatSlow(strs)
    }
}

func BenchmarkConcatFast(b *testing.B) {
    strs := []string{"hello", "world", "foo", "bar"}

    for i := 0; i < b.N; i++ {
        concatFast(strs)
    }
}
```

**Benchmark results comparison**:

```bash
go test -bench=. -benchmem
# => BenchmarkConcatSlow-8   500000   3000 ns/op   80 B/op   6 allocs/op
# => BenchmarkConcatFast-8   2000000  800 ns/op    32 B/op   1 allocs/op
# => Fast version: 3.75x faster, 2.5x less memory, 6x fewer allocations
```

## Production Pattern: Memory Allocation Reduction

Reducing allocations improves performance by reducing GC pressure.

**Pattern: Preallocate Slices**:

```go
package main

// Slow: append without capacity
func processDataSlow(count int) []int {
    // => Slice starts with zero capacity
    // => Grows by doubling (allocates new backing array)

    var results []int
    // => results is nil slice (zero capacity)

    for i := 0; i < count; i++ {
        results = append(results, i*2)
        // => append allocates when capacity exceeded
        // => Multiple allocations as slice grows
    }
    return results
}

// Fast: preallocate with make
func processDataFast(count int) []int {
    // => Preallocates exact capacity

    results := make([]int, 0, count)
    // => Allocates backing array once
    // => len=0, cap=count
    // => No reallocations during append

    for i := 0; i < count; i++ {
        results = append(results, i*2)
        // => append reuses capacity (no allocations)
    }
    return results
}
```

**Pattern: Reuse Buffers with sync.Pool**:

```go
package main

import (
    "bytes"
    "sync"
    // => Standard library for sync.Pool
)

var bufferPool = sync.Pool{
    New: func() interface{} {
        // => Factory function creates new buffer
        // => Called when pool empty

        return new(bytes.Buffer)
        // => Returns pointer to Buffer
    },
}
// => bufferPool reuses buffers across requests
// => Reduces GC pressure

func processRequest(data string) string {
    // => Gets buffer from pool, processes data, returns buffer

    buf := bufferPool.Get().(*bytes.Buffer)
    // => Get() returns interface{} (type assertion needed)
    // => Reuses existing buffer from pool
    // => Or creates new buffer if pool empty

    defer bufferPool.Put(buf)
    // => Returns buffer to pool when done
    // => Buffer reused by next request
    // => CRITICAL: must reset buffer state

    buf.Reset()
    // => Clears buffer contents
    // => Prepares buffer for reuse
    // => Retains underlying capacity

    buf.WriteString("Processed: ")
    buf.WriteString(data)
    // => Uses buffer (no allocations)

    return buf.String()
    // => Returns result
    // => Buffer returned to pool after function returns
}
```

## Production Pattern: Escape Analysis

Go's compiler performs escape analysis to decide stack vs heap allocation.

**Understanding escape analysis**:

```bash
# Run with escape analysis output
go build -gcflags='-m' main.go
# => Shows escape analysis decisions
# => "escapes to heap": allocated on heap
# => "does not escape": allocated on stack
```

**Pattern: Stack Allocation (Fast)**:

```go
package main

import "fmt"

func stackAllocation() {
    // => Local variable doesn't escape

    x := 42
    // => x allocated on stack (fast)
    // => Escape analysis: x does not escape
    // => No GC overhead

    fmt.Println(x)
}
```

**Escape analysis output**:

```bash
go build -gcflags='-m' main.go
# => Output: main.go:7:6: x does not escape
# => Stack allocation (efficient)
```

**Pattern: Heap Allocation (Slower)**:

```go
package main

func heapAllocation() *int {
    // => Returns pointer to local variable

    x := 42
    // => x escapes to heap
    // => Pointer returned, outlives function
    // => Must allocate on heap (GC managed)

    return &x
    // => Returns pointer to x
    // => Escape analysis: x escapes to heap
}
```

**Escape analysis output**:

```bash
go build -gcflags='-m' main.go
# => Output: main.go:7:2: x escapes to heap
# => Heap allocation (GC overhead)
```

**Pattern: Avoiding Unnecessary Escape**:

```go
// Slow: unnecessary heap allocation
func sumSlow(numbers []int) *int {
    // => Returns pointer to result

    sum := 0
    for _, n := range numbers {
        sum += n
    }
    return &sum
    // => sum escapes to heap (pointer returned)
}

// Fast: return value directly
func sumFast(numbers []int) int {
    // => Returns value directly

    sum := 0
    for _, n := range numbers {
        sum += n
    }
    return sum
    // => sum allocated on stack (no escape)
    // => No GC overhead
}
```

## Production Best Practices

**Use pprof continuously in production**:

```go
// GOOD: pprof HTTP endpoints in production (read-only)
import _ "net/http/pprof"
// => Enables /debug/pprof/* endpoints
// => Safe for production (read-only diagnostics)

// Start pprof server on separate port
go func() {
    http.ListenAndServe("localhost:6060", nil)
    // => pprof on port 6060 (not public)
}()
```

**Benchmark before and after optimization**:

```bash
# Baseline benchmark
go test -bench=. -benchmem > old.txt

# Make optimization changes

# New benchmark
go test -bench=. -benchmem > new.txt

# Compare results
benchstat old.txt new.txt
# => Shows improvement or regression
# => Statistical significance analysis
```

**Preallocate slices when size known**:

```go
// GOOD: preallocate capacity
results := make([]int, 0, 1000)
for i := 0; i < 1000; i++ {
    results = append(results, process(i))
}

// BAD: grow dynamically
var results []int  // Zero capacity
for i := 0; i < 1000; i++ {
    results = append(results, process(i))  // Multiple reallocations
}
```

**Use sync.Pool for temporary buffers**:

```go
// GOOD: reuse buffers with sync.Pool
buf := bufferPool.Get().(*bytes.Buffer)
defer func() {
    buf.Reset()  // Clear contents
    bufferPool.Put(buf)  // Return to pool
}()

// BAD: allocate new buffer every request
buf := new(bytes.Buffer)  // GC pressure
```

**Check escape analysis for hot paths**:

```bash
# Identify hot functions from pprof
go tool pprof cpu.prof
(pprof) top

# Check escape analysis for hot function
go build -gcflags='-m' main.go | grep hotFunction
# => Look for "escapes to heap"
# => Optimize to avoid unnecessary heap allocations
```

## Summary

Performance optimization reduces latency, increases throughput, and lowers costs. Standard library provides pprof for CPU/memory profiling and testing.B for benchmarks to identify bottlenecks. Production optimization techniques include reducing memory allocations through preallocation, reusing buffers with sync.Pool, and understanding escape analysis to favor stack allocation. Always profile before optimizing, benchmark changes, and focus on hot paths identified by pprof. Use pprof continuously in production and compare benchmarks before/after optimization.

**Key takeaways**:

- Use pprof for CPU and memory profiling (identifies bottlenecks)
- Benchmark with testing.B before and after optimization
- Preallocate slices when size known (reduce reallocations)
- Use sync.Pool to reuse temporary buffers (reduce GC pressure)
- Understand escape analysis (favor stack allocation over heap)
- Profile before optimizing (don't guess bottlenecks)
- Focus on hot paths (80/20 rule: optimize top 20% of functions)
- Enable pprof HTTP endpoints in production (continuous profiling)
