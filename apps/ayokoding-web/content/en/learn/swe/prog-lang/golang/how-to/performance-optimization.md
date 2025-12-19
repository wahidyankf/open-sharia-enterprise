---
title: Performance Optimization in Go
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 632
description: Optimize Go applications with profiling, memory management, and performance best practices
tags: ["golang", "go", "performance", "optimization", "profiling"]
---

## Profiling with pprof

Use Go's built-in profiler to identify bottlenecks.

```go
package main

import (
    "fmt"
    "log"
    "os"
    "runtime/pprof"
    "time"
)

func cpuIntensiveTask() {
    sum := 0
    for i := 0; i < 100000000; i++ {
        sum += i
    }
    fmt.Println("Sum:", sum)
}

func main() {
    // CPU profiling
    f, err := os.Create("cpu.prof")
    if err != nil {
        log.Fatal(err)
    }
    defer f.Close()

    pprof.StartCPUProfile(f)
    defer pprof.StopCPUProfile()

    cpuIntensiveTask()
}

// Run: go run main.go
// Analyze: go tool pprof cpu.prof
```

## Memory Profiling

Track memory allocations and identify leaks.

```go
package main

import (
    "log"
    "os"
    "runtime"
    "runtime/pprof"
)

func allocateMemory() {
    data := make([][]int, 1000)
    for i := range data {
        data[i] = make([]int, 10000)
    }
}

func main() {
    allocateMemory()

    // Memory profiling
    f, err := os.Create("mem.prof")
    if err != nil {
        log.Fatal(err)
    }
    defer f.Close()

    runtime.GC() // Get current stats
    if err := pprof.WriteHeapProfile(f); err != nil {
        log.Fatal(err)
    }
}

// Run: go run main.go
// Analyze: go tool pprof mem.prof
```

## HTTP Server Profiling

Enable pprof endpoints in HTTP servers.

```go
package main

import (
    "fmt"
    "log"
    "net/http"
    _ "net/http/pprof" // Import for side effects
    "time"
)

func handler(w http.ResponseWriter, r *http.Request) {
    time.Sleep(10 * time.Millisecond) // Simulate work
    fmt.Fprintln(w, "Hello!")
}

func main() {
    http.HandleFunc("/", handler)

    log.Println("Server starting on :6060")
    log.Println("Profiling available at http://localhost:6060/debug/pprof/")
    log.Fatal(http.ListenAndServe(":6060", nil))
}

// Access profiles:
// http://localhost:6060/debug/pprof/
// go tool pprof http://localhost:6060/debug/pprof/profile?seconds=30
// go tool pprof http://localhost:6060/debug/pprof/heap
```

## Benchmark-Driven Optimization

Use benchmarks to measure improvements.

```go
package optimization

import (
    "strings"
    "testing"
)

// Inefficient: Creates many intermediate strings
func concatInefficient(strs []string) string {
    result := ""
    for _, s := range strs {
        result += s
    }
    return result
}

// Efficient: Uses strings.Builder
func concatEfficient(strs []string) string {
    var builder strings.Builder
    for _, s := range strs {
        builder.WriteString(s)
    }
    return builder.String()
}

func BenchmarkConcatInefficient(b *testing.B) {
    strs := []string{"hello", "world", "foo", "bar"}
    for i := 0; i < b.N; i++ {
        concatInefficient(strs)
    }
}

func BenchmarkConcatEfficient(b *testing.B) {
    strs := []string{"hello", "world", "foo", "bar"}
    for i := 0; i < b.N; i++ {
        concatEfficient(strs)
    }
}

// Run: go test -bench=. -benchmem
```

## Reduce Allocations

Reuse buffers and objects to minimize garbage collection.

```go
package optimization

import (
    "bytes"
    "sync"
)

// Buffer pool for reuse
var bufferPool = sync.Pool{
    New: func() interface{} {
        return new(bytes.Buffer)
    },
}

func processDataWithPool(data []byte) string {
    buf := bufferPool.Get().(*bytes.Buffer)
    defer bufferPool.Put(buf)
    buf.Reset()

    buf.Write(data)
    buf.WriteString(" processed")
    return buf.String()
}

// Without pool (allocates each time)
func processDataNoPool(data []byte) string {
    buf := new(bytes.Buffer)
    buf.Write(data)
    buf.WriteString(" processed")
    return buf.String()
}
```

## Slice Pre-allocation

Pre-allocate slices when size is known to avoid reallocation.

```go
package optimization

// Inefficient: Multiple reallocations
func inefficientSlice(n int) []int {
    var result []int
    for i := 0; i < n; i++ {
        result = append(result, i)
    }
    return result
}

// Efficient: Pre-allocated capacity
func efficientSlice(n int) []int {
    result := make([]int, 0, n) // Pre-allocate
    for i := 0; i < n; i++ {
        result = append(result, i)
    }
    return result
}

// Most efficient: Direct indexing
func mostEfficientSlice(n int) []int {
    result := make([]int, n) // Pre-allocate length
    for i := 0; i < n; i++ {
        result[i] = i
    }
    return result
}
```

## Avoid String Concatenation in Loops

Use `strings.Builder` for efficient string building.

```go
package optimization

import (
    "fmt"
    "strings"
)

// Inefficient: Creates new string each iteration
func buildStringInefficient(n int) string {
    s := ""
    for i := 0; i < n; i++ {
        s += fmt.Sprintf("item%d ", i)
    }
    return s
}

// Efficient: Uses strings.Builder
func buildStringEfficient(n int) string {
    var builder strings.Builder
    builder.Grow(n * 10) // Pre-allocate approximate size
    for i := 0; i < n; i++ {
        fmt.Fprintf(&builder, "item%d ", i)
    }
    return builder.String()
}
```

## Channel Buffering

Use buffered channels to reduce goroutine blocking.

```go
package optimization

import (
    "sync"
)

// Unbuffered: Sender blocks until receiver ready
func unbufferedChannel() {
    ch := make(chan int)
    var wg sync.WaitGroup

    wg.Add(1)
    go func() {
        defer wg.Done()
        for i := 0; i < 100; i++ {
            ch <- i // Blocks frequently
        }
        close(ch)
    }()

    for v := range ch {
        _ = v
    }
    wg.Wait()
}

// Buffered: Reduces blocking
func bufferedChannel() {
    ch := make(chan int, 10) // Buffer of 10
    var wg sync.WaitGroup

    wg.Add(1)
    go func() {
        defer wg.Done()
        for i := 0; i < 100; i++ {
            ch <- i // Blocks less often
        }
        close(ch)
    }()

    for v := range ch {
        _ = v
    }
    wg.Wait()
}
```

## Avoid Interface Conversions

Direct type usage is faster than interface conversions.

```go
package optimization

import "testing"

type Adder interface {
    Add(int, int) int
}

type Calculator struct{}

func (c Calculator) Add(a, b int) int {
    return a + b
}

// Interface version (slower)
func addInterface(a Adder, x, y int) int {
    return a.Add(x, y)
}

// Direct version (faster)
func addDirect(c Calculator, x, y int) int {
    return c.Add(x, y)
}

func BenchmarkInterface(b *testing.B) {
    c := Calculator{}
    for i := 0; i < b.N; i++ {
        addInterface(c, 1, 2)
    }
}

func BenchmarkDirect(b *testing.B) {
    c := Calculator{}
    for i := 0; i < b.N; i++ {
        addDirect(c, 1, 2)
    }
}
```

## Use Sync.Map for Concurrent Access

For highly concurrent map access, consider `sync.Map`.

```go
package optimization

import (
    "sync"
)

// Regular map with mutex (write-heavy)
type SafeMap struct {
    mu sync.RWMutex
    m  map[string]int
}

func (sm *SafeMap) Store(key string, value int) {
    sm.mu.Lock()
    defer sm.mu.Unlock()
    sm.m[key] = value
}

func (sm *SafeMap) Load(key string) (int, bool) {
    sm.mu.RLock()
    defer sm.mu.RUnlock()
    val, ok := sm.m[key]
    return val, ok
}

// sync.Map (read-heavy, stable keys)
func useSyncMap() {
    var m sync.Map

    // Store
    m.Store("key1", 42)

    // Load
    if val, ok := m.Load("key1"); ok {
        _ = val.(int)
    }

    // Delete
    m.Delete("key1")

    // Range
    m.Range(func(key, value interface{}) bool {
        return true // Continue iteration
    })
}
```

## Defer Performance

Defer has overhead - avoid in tight loops.

```go
package optimization

import "testing"

func withDefer(n int) int {
    sum := 0
    for i := 0; i < n; i++ {
        func() {
            defer func() {}() // Defer in loop
            sum += i
        }()
    }
    return sum
}

func withoutDefer(n int) int {
    sum := 0
    for i := 0; i < n; i++ {
        sum += i // No defer
    }
    return sum
}

func BenchmarkWithDefer(b *testing.B) {
    for i := 0; i < b.N; i++ {
        withDefer(1000)
    }
}

func BenchmarkWithoutDefer(b *testing.B) {
    for i := 0; i < b.N; i++ {
        withoutDefer(1000)
    }
}
```

## Monitoring with runtime.ReadMemStats

Track memory usage in production.

```go
package main

import (
    "fmt"
    "runtime"
    "time"
)

func printMemStats() {
    var m runtime.MemStats
    runtime.ReadMemStats(&m)

    fmt.Printf("Alloc = %v MB", m.Alloc/1024/1024)
    fmt.Printf("\tTotalAlloc = %v MB", m.TotalAlloc/1024/1024)
    fmt.Printf("\tSys = %v MB", m.Sys/1024/1024)
    fmt.Printf("\tNumGC = %v\n", m.NumGC)
}

func main() {
    ticker := time.NewTicker(5 * time.Second)
    defer ticker.Stop()

    for range ticker.C {
        printMemStats()
    }
}
```

## Related Resources

**Learn more**:

- [Concurrency Patterns](/en/learn/swe/prog-lang/golang/how-to/concurrency-patterns) - Efficient goroutine usage
- [Best Practices](/en/learn/swe/prog-lang/golang/explanation/best-practices) - Performance guidelines
- [Resources](/en/learn/swe/prog-lang/golang/reference/resources) - Profiling tools
