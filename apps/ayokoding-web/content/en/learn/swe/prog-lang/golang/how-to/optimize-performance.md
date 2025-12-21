---
title: "How to Optimize Performance"
date: 2025-12-21T00:00:00+07:00
draft: false
weight: 1000015
description: "Practical techniques for optimizing Go application performance through profiling, benchmarking, memory optimization, and goroutine management"
---

## Problem

Go applications can suffer from performance bottlenecks due to inefficient algorithms, excessive allocations, or goroutine leaks. Without proper profiling, developers waste time optimizing code that isn't the actual bottleneck.

```go
// Problematic approach - guessing at optimization
func processData(items []string) {
    // Is this slow? Without profiling, we don't know
    for _, item := range items {
        result := expensiveOperation(item)
        log.Println(result)
    }
}
```

This guide shows practical techniques for identifying and resolving performance bottlenecks in Go applications.

## Solution

### 1. CPU Profiling with pprof

Use Go's built-in profiling tools to identify CPU bottlenecks.

**CPU profiling**:

```go
package main

import (
    "log"
    "os"
    "runtime/pprof"
)

func main() {
    // Create CPU profile
    f, err := os.Create("cpu.prof")
    if err != nil {
        log.Fatal(err)
    }
    defer f.Close()

    // Start CPU profiling
    if err := pprof.StartCPUProfile(f); err != nil {
        log.Fatal(err)
    }
    defer pprof.StopCPUProfile()

    // Run your code
    heavyComputation()
}

func heavyComputation() {
    sum := 0
    for i := 0; i < 1_000_000_000; i++ {
        sum += i
    }
}

// Analyze profile:
// go run main.go
// go tool pprof cpu.prof
// (pprof) top10
// (pprof) list heavyComputation
// (pprof) web  // generates SVG call graph
```

**HTTP profiling endpoint**:

```go
import (
    "log"
    "net/http"
    _ "net/http/pprof"  // Registers /debug/pprof handlers
)

func main() {
    // Start profiling server
    go func() {
        log.Println(http.ListenAndServe("localhost:6060", nil))
    }()

    // Your application code
    runApplication()
}

// Access profiles:
// CPU: go tool pprof http://localhost:6060/debug/pprof/profile?seconds=30
// Heap: go tool pprof http://localhost:6060/debug/pprof/heap
// Goroutines: go tool pprof http://localhost:6060/debug/pprof/goroutine
// Block: go tool pprof http://localhost:6060/debug/pprof/block
// Mutex: go tool pprof http://localhost:6060/debug/pprof/mutex
```

### 2. Benchmarking with testing Package

Write benchmarks to measure performance scientifically.

**Basic benchmarks**:

```go
package main

import "testing"

// Function to benchmark
func fibonacci(n int) int {
    if n <= 1 {
        return n
    }
    return fibonacci(n-1) + fibonacci(n-2)
}

// Benchmark function
func BenchmarkFibonacci10(b *testing.B) {
    for i := 0; i < b.N; i++ {
        fibonacci(10)
    }
}

func BenchmarkFibonacci20(b *testing.B) {
    for i := 0; i < b.N; i++ {
        fibonacci(20)
    }
}

// Run benchmarks:
// go test -bench=. -benchmem
//
// Output:
// BenchmarkFibonacci10-8    3000000    450 ns/op    0 B/op    0 allocs/op
// BenchmarkFibonacci20-8       3000  450000 ns/op    0 B/op    0 allocs/op

// Benchmark with sub-benchmarks
func BenchmarkStringConcatenation(b *testing.B) {
    inputs := []struct {
        name  string
        count int
    }{
        {"Small", 10},
        {"Medium", 100},
        {"Large", 1000},
    }

    for _, input := range inputs {
        b.Run(input.name, func(b *testing.B) {
            for i := 0; i < b.N; i++ {
                result := ""
                for j := 0; j < input.count; j++ {
                    result += "x"
                }
            }
        })
    }
}

// Compare implementations
func BenchmarkStringBuilder(b *testing.B) {
    for i := 0; i < b.N; i++ {
        var builder strings.Builder
        for j := 0; j < 1000; j++ {
            builder.WriteString("x")
        }
        _ = builder.String()
    }
}

func BenchmarkStringJoin(b *testing.B) {
    parts := make([]string, 1000)
    for i := range parts {
        parts[i] = "x"
    }
    b.ResetTimer()  // Don't count setup time

    for i := 0; i < b.N; i++ {
        _ = strings.Join(parts, "")
    }
}
```

### 3. Memory Profiling and Optimization

Reduce allocations and memory usage.

**Memory profiling**:

```go
package main

import (
    "os"
    "runtime"
    "runtime/pprof"
)

func main() {
    // Memory-intensive work
    processLargeData()

    // Capture heap profile
    f, _ := os.Create("mem.prof")
    defer f.Close()
    runtime.GC()  // Force GC to get accurate stats
    pprof.WriteHeapProfile(f)
}

// Analyze:
// go tool pprof mem.prof
// (pprof) top10
// (pprof) list processLargeData
```

**Reducing allocations**:

```go
// Bad: Excessive allocations
func processItems(items []string) []string {
    var result []string
    for _, item := range items {
        result = append(result, strings.ToUpper(item))  // Reallocates
    }
    return result
}

// Good: Preallocate
func processItemsOptimized(items []string) []string {
    result := make([]string, 0, len(items))  // Preallocate capacity
    for _, item := range items {
        result = append(result, strings.ToUpper(item))
    }
    return result
}

// Object pooling for frequently allocated objects
var bufferPool = sync.Pool{
    New: func() interface{} {
        return new(bytes.Buffer)
    },
}

func processWithPool(data string) string {
    buf := bufferPool.Get().(*bytes.Buffer)
    defer bufferPool.Put(buf)

    buf.Reset()
    buf.WriteString(data)
    buf.WriteString(" processed")
    return buf.String()
}

// Benchmark comparison
func BenchmarkWithoutPool(b *testing.B) {
    for i := 0; i < b.N; i++ {
        buf := new(bytes.Buffer)
        buf.WriteString("data")
        _ = buf.String()
    }
}

func BenchmarkWithPool(b *testing.B) {
    for i := 0; i < b.N; i++ {
        buf := bufferPool.Get().(*bytes.Buffer)
        buf.Reset()
        buf.WriteString("data")
        _ = buf.String()
        bufferPool.Put(buf)
    }
}
```

### 4. Goroutine Management

Prevent goroutine leaks and manage concurrency efficiently.

**Detecting goroutine leaks**:

```go
package main

import (
    "context"
    "fmt"
    "runtime"
    "time"
)

// Bad: Goroutine leak
func leakyGoroutine() {
    ch := make(chan int)
    go func() {
        val := <-ch  // Blocks forever if nothing sends
        fmt.Println(val)
    }()
    // Goroutine never exits - leak!
}

// Good: Context-based cancellation
func properGoroutine() {
    ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
    defer cancel()

    ch := make(chan int)
    go func() {
        select {
        case val := <-ch:
            fmt.Println(val)
        case <-ctx.Done():
            fmt.Println("Context cancelled")
            return
        }
    }()

    // Goroutine exits when context cancelled
}

// Monitor goroutines
func monitorGoroutines() {
    ticker := time.NewTicker(5 * time.Second)
    defer ticker.Stop()

    for range ticker.C {
        count := runtime.NumGoroutine()
        fmt.Printf("Current goroutines: %d\n", count)
        if count > 1000 {
            fmt.Println("Warning: High goroutine count")
        }
    }
}

// Worker pool pattern
func workerPool(jobs <-chan int, results chan<- int) {
    const numWorkers = 10
    var wg sync.WaitGroup

    for i := 0; i < numWorkers; i++ {
        wg.Add(1)
        go func() {
            defer wg.Done()
            for job := range jobs {
                results <- processJob(job)
            }
        }()
    }

    wg.Wait()
    close(results)
}
```

## How It Works

### Profiling Workflow

```mermaid
%%{ Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC }%%
graph TD
    A[Start Application] --> B[Enable Profiling]
    B --> C{Profiling Type}
    C -->|CPU| D[pprof.StartCPUProfile]
    C -->|Memory| E[pprof.WriteHeapProfile]
    C -->|Goroutine| F[pprof.Lookup - goroutine]

    D --> G[Run Workload]
    E --> G
    F --> G

    G --> H[Collect Profile Data]
    H --> I[Analyze with pprof]

    I --> J{Bottleneck Found?}
    J -->|Yes| K[Optimize Code]
    J -->|No| L[Performance Acceptable]

    K --> M[Benchmark Changes]
    M --> N{Improvement?}
    N -->|Yes| O[Keep Changes]
    N -->|No| P[Revert/Try Different Approach]

    style A fill:#0173B2,stroke:#000000,color:#FFFFFF
    style I fill:#DE8F05,stroke:#000000,color:#FFFFFF
    style J fill:#029E73,stroke:#000000,color:#FFFFFF
    style K fill:#CC78BC,stroke:#000000,color:#FFFFFF
```

**Key concepts**:

1. **pprof**: Go's profiling tool for CPU, memory, goroutines, blocks, and mutex contention
2. **Benchmarking**: Measure performance with `testing.B` and `-benchmem` flag
3. **Allocation Reduction**: Preallocate slices/maps, use sync.Pool for temporary objects
4. **Goroutine Hygiene**: Use contexts for cancellation, monitor goroutine count

## Variations

### Trace Analysis

Capture execution trace for detailed analysis:

```go
import (
    "os"
    "runtime/trace"
)

func main() {
    f, _ := os.Create("trace.out")
    defer f.Close()

    trace.Start(f)
    defer trace.Stop()

    // Your code
    doWork()
}

// Analyze trace:
// go tool trace trace.out
// Opens web browser with timeline view
```

### Escape Analysis

Check if variables escape to heap:

```bash
# Build with escape analysis
go build -gcflags="-m" main.go

# Output shows allocations:
# ./main.go:10:13: ... escapes to heap
# ./main.go:15:2: moved to heap: x
```

### Compiler Optimizations

View compiler optimizations:

```bash
# Show inlining decisions
go build -gcflags="-m -m"

# Disable optimizations for debugging
go build -gcflags="-N -l"

# View assembly
go tool compile -S main.go
```

## Common Pitfalls

**Pitfall 1: Premature Optimization**

Profile before optimizing:

```go
// Bad: Optimizing without data
func processItems(items []int) {
    // Complex optimization that may not help
    // ...
}

// Good: Profile first
func processItems(items []int) {
    // Simple, clear code
    // Optimize only if profiling shows bottleneck
}
```

**Pitfall 2: Ignoring Benchmark Variance**

Run benchmarks multiple times:

```bash
# Single run - unreliable
go test -bench=.

# Better: Multiple runs and statistics
go test -bench=. -count=10 | tee bench.txt
benchstat bench.txt

# Compare before/after
go test -bench=. -count=10 > old.txt
# Make changes
go test -bench=. -count=10 > new.txt
benchstat old.txt new.txt
```

**Pitfall 3: Not Resetting Timer**

Exclude setup from benchmark:

```go
func BenchmarkProcess(b *testing.B) {
    // Setup - don't count this
    data := generateLargeDataset()

    b.ResetTimer()  // Start timing here

    for i := 0; i < b.N; i++ {
        process(data)
    }
}
```

**Pitfall 4: Goroutine Leaks**

Always provide exit paths:

```go
// Bad: No way to stop
func startWorker() {
    go func() {
        for {
            work()  // Runs forever
        }
    }()
}

// Good: Context-based cancellation
func startWorker(ctx context.Context) {
    go func() {
        for {
            select {
            case <-ctx.Done():
                return
            default:
                work()
            }
        }
    }()
}
```

## Related Patterns

**Related Tutorial**: See [Intermediate Tutorial - Performance](../tutorials/intermediate.md#performance) for performance fundamentals and [Advanced Tutorial - Profiling](../tutorials/advanced.md#profiling) for advanced profiling techniques.

**Related How-To**: See [Work with Concurrency](./work-with-concurrency.md) for goroutine best practices and [Use Channels Effectively](./use-channels-effectively.md) for channel patterns.

**Related Cookbook**: See Cookbook recipes "pprof Profiling Patterns", "Benchmark Comparison", and "Memory Optimization" for copy-paste ready optimization code.

**Related Explanation**: See [Best Practices - Performance](../explanation/best-practices.md#performance) for performance principles.

## Further Reading

- [Go Profiling Guide](https://go.dev/blog/pprof) - Official profiling documentation
- [Diagnostics Guide](https://go.dev/doc/diagnostics) - Complete diagnostics overview
- [High Performance Go](https://dave.cheney.net/high-performance-go-workshop/dotgo-paris.html) - Dave Cheney's workshop
- [Go Performance Tips](https://github.com/dgryski/go-perfbook) - Performance optimization book
