---
title: "Advanced"
date: 2025-12-23T00:00:00+07:00
draft: false
weight: 10000003
description: "Examples 61-85: Advanced concurrency, generics, reflection, advanced patterns, testing strategies, and modern Go features (75-95% coverage)"
tags: ["golang", "go", "tutorial", "by-example", "advanced", "generics", "concurrency", "profiling"]
---

## Group 1: Advanced Concurrency Patterns

## Example 61: Pipeline Pattern

Pipelines process data through stages, each stage running concurrently. Each stage is a function that receives input from one channel and sends output to another. This composition enables elegant data processing.

```mermaid
graph TD
    A["Source<br/>generates values"]
    B["Stage 1<br/>transforms"]
    C["Stage 2<br/>filters"]
    D["Sink<br/>collects results"]

    A -->|channel| B
    B -->|channel| C
    C -->|channel| D

    style A fill:#0173B2,stroke:#000,color:#fff
    style B fill:#DE8F05,stroke:#000,color:#fff
    style C fill:#029E73,stroke:#000,color:#fff
    style D fill:#CC78BC,stroke:#000,color:#fff
```

**Code**:

```go
package main

import "fmt"

func main() {
    // Create pipeline: generate -> multiply by 2 -> print
    nums := generate(1, 5)         // => Stage 1: generate 1-5
                                    // => nums is <-chan int (receive-only channel)
                                    // => Goroutine starts running in background

    doubled := multiply(nums, 2)   // => Stage 2: multiply by 2
                                    // => doubled is <-chan int (new channel)
                                    // => Second goroutine starts processing nums

    square := multiply(doubled, 2) // => Stage 3: multiply by 2 again (square effect)
                                    // => square is <-chan int (final channel)
                                    // => Third goroutine processes doubled
                                    // => Pipeline: generate(1-5) -> *2 -> *2 = values*4

    // Consume results
    for result := range square {   // => Stage 4: consume results
                                    // => Blocks until value available from square
                                    // => result is 4, then 8, then 12, then 16, then 20
        fmt.Println(result)         // => Output: 4 (first iteration)
                                    // => Output: 8 (second iteration)
                                    // => Output: 12 (third iteration)
                                    // => Output: 16 (fourth iteration)
                                    // => Output: 20 (fifth iteration)
    }
    // => Loop exits when square channel closes
    // => All goroutines complete gracefully
}

// Generator stage - creates values
func generate(start, end int) <-chan int { // => Returns receive-only channel
                                            // => Caller can only receive, not send
    out := make(chan int)                   // => out is bidirectional chan int
                                            // => Unbuffered channel (blocks on send until receive)
    go func() {                             // => Spawn goroutine to generate values
                                            // => Goroutine continues after generate() returns
        for i := start; i <= end; i++ {     // => i is 1, then 2, then 3, then 4, then 5
            out <- i                        // => Send value to channel (blocks until multiply receives)
                                            // => Sends: 1, then 2, then 3, then 4, then 5
        }
        close(out)                          // => Signal completion, no more values
                                            // => Range loops will exit when channel closed
    }()
    return out                              // => Return channel immediately (goroutine runs async)
                                            // => Type converted from chan int to <-chan int
}

// Transform stage - multiplies values
func multiply(in <-chan int, factor int) <-chan int { // => in is receive-only (ensures we don't send)
                                                       // => factor is 2 in all calls
                                                       // => Returns receive-only channel
    out := make(chan int)                              // => Create new output channel
                                                        // => Unbuffered (synchronizes with consumer)
    go func() {                                         // => Spawn goroutine to transform values
                                                        // => Goroutine continues after multiply() returns
        for value := range in {                         // => Range until channel closes
                                                        // => Blocks waiting for input values
                                                        // => value is each input (1,2,3,4,5 or 2,4,6,8,10)
            out <- value * factor                       // => Send transformed value
                                                        // => First multiply: sends 2,4,6,8,10
                                                        // => Second multiply: sends 4,8,12,16,20
        }
        close(out)                                      // => Close output when input exhausted
                                                        // => Signals downstream consumers
    }()
    return out                                          // => Return channel immediately
                                                        // => Type converted from chan int to <-chan int
}
```

**Key Takeaway**: Pipelines compose concurrent stages. Each stage receives from one channel, processes, sends to next. Use channels with directional types (`<-chan` receive, `chan<-` send) to clarify data flow.

**Why It Matters**: Pipelines power production stream processing in services like Kubernetes event processing, where data flows through validation → transformation → persistence stages with automatic backpressure handling through unbuffered channels. This pattern enables building scalable ETL systems that process millions of records while maintaining bounded memory usage, unlike batch processing that requires loading entire datasets into RAM.

## Example 62: Context-Aware Pipelines

Pipeline stages should respect cancellation. When context is cancelled, all stages should exit gracefully. This enables cancelling long-running pipelines without leaking goroutines.

```mermaid
%% Context cancellation propagates through pipeline stages
sequenceDiagram
    participant Main
    participant Ctx as Context
    participant Gen as Generate Stage
    participant Sq as Square Stage
    participant Out as Output

    Main->>Ctx: WithTimeout(100ms)
    Main->>Gen: Start generation
    Main->>Sq: Start squaring
    Gen->>Sq: Send values (1,2,3...)
    Sq->>Out: Send squared (1,4,9...)

    Note over Ctx: 100ms elapsed
    Ctx-->>Gen: ctx.Done() signal
    Ctx-->>Sq: ctx.Done() signal
    Gen->>Gen: Exit gracefully
    Sq->>Sq: Exit gracefully
    Gen->>Sq: Close channel
    Sq->>Out: Close channel
    Out->>Main: Loop exits

    Note over Main,Out: All goroutines cleaned up
```

**Code**:

```go
package main

import (
    "context"
    "fmt"
    "time"
)

func main() {
    // Create cancellable context
    ctx, cancel := context.WithTimeout(context.Background(), 100*time.Millisecond)
                                                        // => ctx will cancel after 100ms
                                                        // => cancel is function to cancel early
                                                        // => Background() returns empty root context
    defer cancel()                                      // => Ensure cancel called (releases resources)
                                                        // => Even if context times out, call cancel()

    // Pipeline with context awareness
    nums := generateWithContext(ctx, 1, 100)            // => Generate 1-100 with cancellation
                                                        // => nums is <-chan int
                                                        // => Goroutine respects ctx.Done()

    squared := squareWithContext(ctx, nums)             // => Square each value from nums
                                                        // => squared is <-chan int
                                                        // => Goroutine respects ctx.Done()

    // Consume until context cancelled
    for result := range squared {                       // => Blocks waiting for squared values
                                                        // => result is 1, then 4, then 9, then 16...
        fmt.Println(result)                             // => Output: 1 (first value)
                                                        // => Output: 4 (second value)
                                                        // => Output: 9 (third value)
                                                        // => Outputs continue until context timeout
    }
                                                        // => Loop exits when squared channel closes
                                                        // => Context timeout (100ms) cancels pipeline
    fmt.Println("Pipeline cancelled")                   // => Output: Pipeline cancelled
                                                        // => Printed after all goroutines exit
}

func generateWithContext(ctx context.Context, start, end int) <-chan int {
                                                        // => ctx enables cancellation signal
                                                        // => start=1, end=100
                                                        // => Returns receive-only channel
    out := make(chan int)                               // => Create unbuffered channel
                                                        // => Blocks on send until receiver ready
    go func() {                                         // => Spawn goroutine for generation
                                                        // => Runs concurrently with caller
        defer close(out)                                // => Always close channel when done
                                                        // => Deferred: runs even if return early
        for i := start; i <= end; i++ {                 // => i is 1, then 2, then 3...
                                                        // => Loop continues until i > end or cancelled
            select {                                    // => Multiplexing: send or cancel
            case out <- i:                              // => Send value (blocks until receiver ready)
                                                        // => Sends: 1, 2, 3, 4... until timeout
            case <-ctx.Done():                          // => Context cancelled (timeout or manual)
                                                        // => Receives signal from closed Done() channel
                fmt.Println("Generate cancelled")       // => Output: Generate cancelled
                                                        // => Indicates graceful shutdown
                return                                  // => Exit goroutine early
                                                        // => Deferred close(out) runs
            }
            time.Sleep(10 * time.Millisecond)           // => Simulate work (10ms per value)
                                                        // => 100ms timeout = ~10 values generated
        }
                                                        // => If loop completes naturally, close(out) runs
    }()
    return out                                          // => Return channel immediately
                                                        // => Goroutine continues in background
}

func squareWithContext(ctx context.Context, in <-chan int) <-chan int {
                                                        // => ctx enables cancellation signal
                                                        // => in is input channel from generateWithContext
                                                        // => Returns receive-only channel
    out := make(chan int)                               // => Create unbuffered output channel
                                                        // => Synchronizes with consumer
    go func() {                                         // => Spawn goroutine for transformation
                                                        // => Runs concurrently with generator
        defer close(out)                                // => Always close when done
                                                        // => Signals consumer no more values
        for value := range in {                         // => Range over input channel
                                                        // => Blocks waiting for values from in
                                                        // => value is 1, then 2, then 3...
                                                        // => Exits when in closes
            select {                                    // => Multiplexing: send or cancel
            case out <- value * value:                  // => Send squared value
                                                        // => Sends: 1, 4, 9, 16, 25...
                                                        // => Blocks until consumer receives
            case <-ctx.Done():                          // => Context cancelled
                                                        // => Receives signal from closed Done() channel
                fmt.Println("Square cancelled")         // => Output: Square cancelled
                                                        // => Indicates graceful shutdown
                return                                  // => Exit goroutine early
                                                        // => Deferred close(out) runs
            }
        }
                                                        // => If input exhausted naturally, close(out) runs
    }()
    return out                                          // => Return channel immediately
                                                        // => Goroutine continues in background
}
```

**Key Takeaway**: Use `select` with `ctx.Done()` in every stage to enable graceful cancellation. When context is cancelled, all stages exit promptly without leaving goroutines running.

**Why It Matters**: Context-aware cancellation prevents goroutine leaks that plague long-running services, where a cancelled HTTP request must terminate all downstream processing to avoid wasting CPU and memory on orphaned work. Production systems like Prometheus use this pattern to abort expensive metric aggregation queries when clients disconnect, maintaining system stability under load spikes by immediately freeing resources.

## Example 63: Rate Limiting

Rate limiting restricts how fast operations occur. Token bucket pattern uses a channel - tokens arrive at a rate, operations consume tokens. When no tokens available, operations wait.

**Code**:

```go
package main

import (
    "fmt"
    "time"
)

func main() {
    // Create rate limiter - 2 operations per second
    limiter := make(chan struct{}, 2)               // => Channel with capacity 2 (tokens)
                                                    // => Buffered channel holds max 2 empty structs
                                                    // => struct{} uses zero bytes (efficient token)

    // Replenish tokens
    go func() {                                     // => Spawn goroutine to add tokens
                                                    // => Runs concurrently with main
        ticker := time.NewTicker(500 * time.Millisecond)
                                                    // => Add token every 500ms
                                                    // => ticker.C is <-chan time.Time
                                                    // => Sends current time every 500ms
        for range ticker.C {                        // => Receive from ticker every 500ms
                                                    // => Ignores time value (we just need timing)
            select {                                // => Non-blocking send attempt
            case limiter <- struct{}{}:             // => Add token if space available
                                                    // => Sends empty struct to channel
                                                    // => Succeeds if len(limiter) < 2
            default:                                // => Token buffer full, skip
                                                    // => Prevents blocking ticker goroutine
                                                    // => No action needed (already at capacity)
            }
        }
    }()

    // Use limited operations
    for i := 0; i < 5; i++ {                        // => i is 0, then 1, then 2, then 3, then 4
        <-limiter                                   // => Consume token (wait if none available)
                                                    // => Blocks until token available
                                                    // => First 2 operations immediate (initial capacity)
                                                    // => Remaining wait for token replenishment
        fmt.Printf("Operation %d at %v\n", i, time.Now().Unix())
                                                    // => Output: Operation 0 at 1234567890 (immediate)
                                                    // => Output: Operation 1 at 1234567890 (immediate)
                                                    // => Output: Operation 2 at 1234567890 (wait 500ms)
                                                    // => Output: Operation 3 at 1234567891 (wait 500ms)
                                                    // => Output: Operation 4 at 1234567891 (wait 500ms)
    }
}

// Alternative: time.Limit from golang.org/x/time/rate
import "golang.org/x/time/rate"

func limitedOperations() {
    limiter := rate.NewLimiter(rate.Every(time.Second), 5)
                                                    // => 1 op/sec sustained rate
                                                    // => burst capacity of 5 (initial tokens)
                                                    // => rate.Every(time.Second) = 1 operation per second
                                                    // => Allows bursts up to 5 operations

    for i := 0; i < 10; i++ {                       // => i is 0,1,2...9 (10 operations total)
        if !limiter.Allow() {                       // => Check if operation allowed (non-blocking)
                                                    // => Returns true if token available
                                                    // => Consumes token if available
                                                    // => First 5 return true (burst capacity)
                                                    // => Remaining 5 fail (no tokens yet)
            fmt.Println("Rate limit exceeded")      // => Output: Rate limit exceeded (for ops 5-9)
                                                    // => Indicates rejection, not wait
            continue                                // => Skip this operation
        }
        fmt.Printf("Operation %d\n", i)             // => Output: Operation 0 (first 5 operations)
                                                    // => Output: Operation 1
                                                    // => Output: Operation 2
                                                    // => Output: Operation 3
                                                    // => Output: Operation 4
                                                    // => Remaining operations rejected
    }
}
```

**Key Takeaway**: Token bucket pattern: channel of limited capacity represents tokens, operations consume tokens. Replenish tokens at fixed rate. This throttles operations smoothly.

**Why It Matters**: Rate limiting protects production APIs from overload, preventing cascading failures where unlimited client requests exhaust database connections or downstream API quotas. The token bucket pattern enables graceful degradation by allowing controlled bursts (handling traffic spikes) while enforcing sustained rate limits, the approach used by GitHub API (5,000 requests/hour) and Twitter API (15 requests/15-minute window) to maintain service quality for all users.

## Example 64: Semaphore Pattern

Semaphores limit concurrent access to resources. While `sync.Mutex` allows one goroutine at a time, semaphores allow N. Implement with buffered channel of capacity N.

**Code**:

```go
package main

import (
    "fmt"
    "sync"
)

func main() {
    // Semaphore - allow 3 concurrent operations
    sem := make(chan struct{}, 3)               // => Capacity 3 = 3 concurrent slots
                                                // => Buffered channel acts as counting semaphore
                                                // => struct{} uses zero memory per token
    var wg sync.WaitGroup                       // => WaitGroup to wait for all goroutines
                                                // => Counter starts at 0

    for i := 1; i <= 10; i++ {                  // => Launch 10 goroutines total
                                                // => i is 1,2,3...10
        wg.Add(1)                               // => Increment counter before goroutine starts
                                                // => Counter is now 1,2,3...10
        go func(id int) {                       // => Spawn goroutine with operation id
                                                // => id captured from i (avoids closure issue)
            defer wg.Done()                     // => Decrement counter when goroutine completes
                                                // => Always executes even if panic

            sem <- struct{}{}                   // => Acquire slot (blocks if all 3 slots full)
                                                // => Blocks if len(sem) == 3
                                                // => Only 3 goroutines can acquire simultaneously
            defer func() { <-sem }()            // => Release slot when done
                                                // => Deferred: runs after operation completes
                                                // => Allows waiting goroutines to proceed

            fmt.Printf("Operation %d running\n", id)
                                                // => Output: Operation 1 running (one of first 3)
                                                // => Output: Operation 2 running (one of first 3)
                                                // => Output: Operation 3 running (one of first 3)
                                                // => Remaining wait for slot release
            // Simulate work
        }(i)                                    // => Pass i as argument (prevents closure capture)
    }

    wg.Wait()                                   // => Block until counter reaches 0
                                                // => Waits for all 10 goroutines to complete
    fmt.Println("All operations complete")      // => Output: All operations complete
                                                // => Printed after all goroutines exit
}

// Weighted semaphore - operations require different numbers of slots
func weighSemaphore() {
    sem := make(chan int, 10)                   // => Capacity 10 "units"
                                                // => Can hold up to 10 int values
                                                // => Not a counting semaphore (simplified example)

    // Operation requiring 3 units
    go func() {                                 // => Spawn goroutine for operation
        n := 3                                  // => n is 3 (units required)
        sem <- n                                // => Acquire 3 units (conceptual)
                                                // => Actually sends int 3 to channel
                                                // => Blocks if channel full (len(sem) == 10)
        defer func() { <-sem }()                // => Release slot (receive from channel)
                                                // => Deferred: runs when function exits

        fmt.Println("Acquired 3 units")         // => Output: Acquired 3 units
                                                // => Printed after successfully acquiring
    }()

    // Operation requiring 7 units
    go func() {                                 // => Spawn goroutine for operation
        n := 7                                  // => n is 7 (units required)
        sem <- n                                // => Acquire 7 units (conceptual)
                                                // => Sends int 7 to channel
                                                // => Blocks if channel full
        defer func() { <-sem }()                // => Release slot when done
                                                // => Deferred execution

        fmt.Println("Acquired 7 units")         // => Output: Acquired 7 units
                                                // => Printed after successfully acquiring
    }()

    // Total capacity: 10 units, both operations can run concurrently
                                                // => Note: This is simplified example
                                                // => True weighted semaphore needs golang.org/x/sync/semaphore
                                                // => Real implementation tracks actual weight consumption
}
```

**Key Takeaway**: Semaphore = buffered channel. Capacity = maximum concurrent operations. Send before work, receive after. Useful for limiting concurrent database connections, API calls, or other bounded resources.

**Why It Matters**: Semaphores limit concurrent access to bounded resources like database connection pools (100 max connections) or external APIs with rate limits, preventing resource exhaustion that causes production outages. Unlike mutexes (N=1), semaphores enable controlled parallelism (N>1), allowing 10 concurrent S3 uploads while blocking the 11th until a slot frees, maximizing throughput without overwhelming external services.

## Example 65: Atomic Operations

Atomic operations ensure thread-safe modifications without mutexes. The `sync/atomic` package provides compare-and-swap (CAS) and atomic increments. Use when contention is low and operations are simple.

```mermaid
%% Compare-and-swap atomic operation flow
graph TD
    A["Start: value=5"]
    B["Call CompareAndSwap<br/>expected=5, new=10"]
    C{Current value<br/>equals expected?}
    D["Yes: value==5"]
    E["No: value!=5"]
    F["Atomically swap<br/>value=10"]
    G["Return true"]
    H["No change<br/>value unchanged"]
    I["Return false"]

    A --> B
    B --> C
    C -->|value==5| D
    C -->|value!=5| E
    D --> F
    E --> H
    F --> G
    H --> I

    style A fill:#0173B2,stroke:#000,color:#fff
    style B fill:#DE8F05,stroke:#000,color:#fff
    style C fill:#CC78BC,stroke:#000,color:#fff
    style D fill:#029E73,stroke:#000,color:#fff
    style E fill:#CA9161,stroke:#000,color:#fff
    style F fill:#029E73,stroke:#000,color:#fff
    style G fill:#0173B2,stroke:#000,color:#fff
    style H fill:#CA9161,stroke:#000,color:#fff
    style I fill:#DE8F05,stroke:#000,color:#fff
```

**Code**:

```go
package main

import (
    "fmt"
    "sync"
    "sync/atomic"
)

func main() {
    // Atomic counter - multiple goroutines increment safely
    var counter int64                               // => Must be int64 or int32 for atomic
                                                    // => Initialized to 0
                                                    // => Must be accessed via atomic package only
    var wg sync.WaitGroup                           // => WaitGroup to synchronize goroutines
                                                    // => Counter starts at 0

    for i := 0; i < 10; i++ {                       // => Launch 10 goroutines
                                                    // => i is 0,1,2...9
        wg.Add(1)                                   // => Increment WaitGroup counter
                                                    // => Counter reaches 10
        go func() {                                 // => Spawn goroutine
                                                    // => Runs concurrently with others
            defer wg.Done()                         // => Decrement WaitGroup when done
                                                    // => Always executes via defer
            for j := 0; j < 100; j++ {              // => Each goroutine increments 100 times
                                                    // => j is 0,1,2...99
                atomic.AddInt64(&counter, 1)        // => Atomic increment (thread-safe)
                                                    // => Adds 1 to counter atomically
                                                    // => No race condition (CPU-level atomic operation)
                                                    // => counter increases: 1,2,3...1000
            }
        }()
    }

    wg.Wait()                                       // => Block until all goroutines complete
                                                    // => Waits for counter to reach 0
    fmt.Println("Counter:", counter)                // => Output: Counter: 1000
                                                    // => Always 1000 (safe from race conditions)
                                                    // => Without atomic: unpredictable (race condition)

    // Atomic swap
    var value int64 = 10                            // => value is 10 (type: int64)
    old := atomic.SwapInt64(&value, 20)             // => Set value to 20, return old (10)
                                                    // => Atomic operation (swap happens atomically)
                                                    // => old is 10, value is now 20
    fmt.Println("Old:", old, "New:", value)         // => Output: Old: 10 New: 20
                                                    // => Demonstrates swap semantics

    // Compare-and-swap (CAS)
    var cas int64 = 5                               // => cas is 5 (type: int64)
    swapped := atomic.CompareAndSwapInt64(&cas, 5, 10)
                                                    // => If cas==5, set to 10 and return true
                                                    // => If cas!=5, no change and return false
                                                    // => cas is 5, so swap succeeds
                                                    // => swapped is true, cas is now 10
    fmt.Println("Swapped:", swapped, "Value:", cas) // => Output: Swapped: true Value: 10
                                                    // => CAS useful for lock-free algorithms

    // Failed CAS example
    swapped2 := atomic.CompareAndSwapInt64(&cas, 5, 15)
                                                    // => cas is 10 (not 5), so swap fails
                                                    // => swapped2 is false, cas unchanged (10)
                                                    // => Demonstrates conditional update

    // Load and store for safe reads
    var flag int32 = 0                              // => flag is 0 (type: int32)
                                                    // => Must use int32 for atomic operations
    atomic.StoreInt32(&flag, 1)                     // => Atomic write (sets flag to 1)
                                                    // => Ensures visibility across goroutines
                                                    // => Memory barrier guarantees ordering
    value32 := atomic.LoadInt32(&flag)              // => Atomic read (reads flag value)
                                                    // => value32 is 1
                                                    // => Ensures we see latest value
    fmt.Println("Flag:", value32)                   // => Output: Flag: 1
                                                    // => Load/Store prevent compiler reordering
}
```

**Key Takeaway**: Atomic operations are lock-free. Use `atomic.AddInt64()` for counters, `atomic.SwapInt64()` for updates, `atomic.CompareAndSwapInt64()` for conditional updates. Lower overhead than mutexes but limited to simple operations.

**Why It Matters**: Atomic operations eliminate mutex overhead for simple counters and flags in high-throughput services, where atomic.AddInt64() provides 10-100x better performance than mutex-protected increments for metrics collection that happens millions of times per second. Compare-and-swap (CAS) enables building lock-free data structures like concurrent queues used in Prometheus for metric ingestion, achieving microsecond latencies that mutexes cannot provide.

## Group 2: Advanced Standard Library

## Example 66: Reflection

Reflection inspects types and values at runtime. The `reflect` package enables dynamic code - examine struct fields, call methods, or build values whose type isn't known until runtime. Use sparingly - reflection is powerful but slow and hard to understand.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    A["Value<br/>Person{Name, Age}"]
    B["reflect.ValueOf(p)"]
    C["reflect.Type<br/>main.Person"]
    D["Iterate Fields"]
    E["Field 0: Name<br/>string"]
    F["Field 1: Age<br/>int"]

    A --> B
    B --> C
    B --> D
    D --> E
    D --> F

    style A fill:#0173B2,stroke:#000,color:#fff
    style B fill:#DE8F05,stroke:#000,color:#fff
    style C fill:#029E73,stroke:#000,color:#fff
    style D fill:#CC78BC,stroke:#000,color:#fff
    style E fill:#CA9161,stroke:#000,color:#fff
    style F fill:#CA9161,stroke:#000,color:#fff
```

**Code**:

```go
package main

import (
    "fmt"
    "reflect"
)

func main() {
    // Inspect struct type
    type Person struct {                        // => Define struct type
        Name string                             // => First field: Name (type: string)
        Age  int                                // => Second field: Age (type: int)
    }

    p := Person{"Alice", 30}                    // => p is Person{Name:"Alice", Age:30}
                                                // => Struct literal initialization
    v := reflect.ValueOf(p)                     // => Get reflected value wrapping p
                                                // => v is reflect.Value of Person
                                                // => Enables runtime type inspection
    t := v.Type()                               // => Get type information from value
                                                // => t is reflect.Type for main.Person
                                                // => Contains metadata about struct

    fmt.Println("Type:", t)                     // => Output: Type: main.Person
                                                // => Shows package and type name
    fmt.Println("Fields:")                      // => Header for field iteration

    // Iterate struct fields
    for i := 0; i < v.NumField(); i++ {         // => NumField() returns 2 (Name, Age)
                                                // => i is 0, then 1
        field := v.Field(i)                     // => Get field value by index
                                                // => field is reflect.Value for field
                                                // => i=0: field wraps "Alice", i=1: field wraps 30
        fieldType := t.Field(i)                 // => Get field type metadata
                                                // => fieldType is reflect.StructField
                                                // => Contains Name, Type, Tag, Offset
        fmt.Printf("  %s: %v (type: %s)\n", fieldType.Name, field.Interface(), field.Type())
                                                // => fieldType.Name is "Name" or "Age"
                                                // => field.Interface() converts back to interface{}
                                                // => field.Type() is "string" or "int"
    }
    // => Output:
    // =>   Name: Alice (type: string)          // => First field
    // =>   Age: 30 (type: int)                 // => Second field

    // Get value using field name
    nameField := v.FieldByName("Name")          // => Lookup field by string name
                                                // => nameField is reflect.Value wrapping "Alice"
                                                // => Returns zero Value if field not found
    fmt.Println("Name field:", nameField.String())
                                                // => Output: Name field: Alice
                                                // => .String() converts Value to string

    // Check kind
    if t.Kind() == reflect.Struct {             // => Kind() returns underlying type category
                                                // => reflect.Struct is one of 26 kinds
                                                // => Other kinds: Int, String, Slice, etc.
        fmt.Println("Type is a struct")         // => Output: Type is a struct
                                                // => Confirms t represents struct type
    }

    // Type assertion vs reflection
    // Reflection approach (dynamic):
    val := reflect.ValueOf(p)                   // => Create new reflect.Value for p
                                                // => val wraps Person struct
    if val.Kind() == reflect.Struct {           // => Runtime check using reflection
                                                // => Slower than static type check
        fmt.Println("It's a struct (reflection)")
                                                // => Output: It's a struct (reflection)
                                                // => Dynamic approach for unknown types
    }

    // Direct approach (static, faster):
    switch p := p.(type) {                      // => Type switch (compile-time)
                                                // => p shadows outer p variable
    case Person:                                // => Match Person type
                                                // => Compiler knows type at compile-time
        fmt.Println("It's a Person (direct)")   // => Output: It's a Person (direct)
                                                // => Faster than reflection (no runtime overhead)
                                                // => Preferred when types known at compile-time
    }
}
```

**Key Takeaway**: Use reflection sparingly - it's slow and reduces code clarity. Prefer direct type assertions when possible. Reflection is useful for libraries, JSON unmarshaling, or dynamic test fixtures. `reflect.ValueOf()` gets reflected value, `.Type()` gets type, `.NumField()` iterates struct fields.

**Why It Matters**: Reflection powers critical infrastructure like JSON marshaling (encoding/json), database ORMs (GORM, sqlx), and dependency injection frameworks that need to work with types unknown at compile-time. While slow (10-100x overhead) and fragile (bypasses compile-time type safety), reflection is essential for building generic libraries that inspect struct tags for validation rules or automatically map database columns to struct fields, reducing boilerplate in application code.

## Example 67: Binary Encoding

Binary protocols and data formats require reading/writing binary data. The `encoding/binary` package handles byte order (endianness) and converts between binary and Go types.

**Code**:

```go
package main

import (
    "bytes"
    "encoding/binary"
    "fmt"
    "io"
)

func main() {
    // Write binary data
    buf := new(bytes.Buffer)                        // => buf is *bytes.Buffer (in-memory buffer)
                                                    // => Implements io.Writer and io.Reader
                                                    // => Starts empty (len=0)

    // Write integer in big-endian format
    binary.Write(buf, binary.BigEndian, int32(42))  // => 42 as 4 bytes, big-endian
                                                    // => Writes [0x00 0x00 0x00 0x2A]
                                                    // => Big-endian: most significant byte first
                                                    // => buf now has 4 bytes
    binary.Write(buf, binary.BigEndian, float32(3.14))
                                                    // => Float as 4 bytes (IEEE 754 format)
                                                    // => Writes [0x40 0x48 0xF5 0xC3] (approx)
                                                    // => buf now has 8 bytes
    binary.Write(buf, binary.BigEndian, true)       // => Bool as 1 byte
                                                    // => true writes 0x01, false writes 0x00
                                                    // => buf now has 9 bytes total

    // Read back
    reader := bytes.NewReader(buf.Bytes())          // => Create reader from buffer bytes
                                                    // => reader is *bytes.Reader at position 0
                                                    // => buf.Bytes() returns []byte slice
    var num int32                                   // => num is 0 (zero value for int32)
    var f float32                                   // => f is 0.0 (zero value for float32)
    var b bool                                      // => b is false (zero value for bool)

    binary.Read(reader, binary.BigEndian, &num)     // => Read int32 from reader
                                                    // => Reads 4 bytes, converts to int32
                                                    // => num is now 42
                                                    // => reader position advances to byte 4
    binary.Read(reader, binary.BigEndian, &f)       // => Read float32 from reader
                                                    // => Reads 4 bytes, converts to float32
                                                    // => f is now 3.14 (approximately)
                                                    // => reader position advances to byte 8
    binary.Read(reader, binary.BigEndian, &b)       // => Read bool from reader
                                                    // => Reads 1 byte, converts to bool
                                                    // => b is now true
                                                    // => reader position advances to byte 9 (EOF)

    fmt.Printf("Num: %d, Float: %f, Bool: %v\n", num, f, b)
                                                    // => Output: Num: 42, Float: 3.140000, Bool: true
                                                    // => Demonstrates round-trip encoding/decoding

    // Endianness matters
    smallBuf := new(bytes.Buffer)                   // => New empty buffer
    binary.Write(smallBuf, binary.LittleEndian, int16(256))
                                                    // => 256 in decimal = 0x0100 in hex
                                                    // => Little-endian: least significant byte first
                                                    // => Writes [0x00 0x01] (low byte, high byte)
    fmt.Printf("Little-endian bytes: %v\n", smallBuf.Bytes())
                                                    // => Output: Little-endian bytes: [0 1]
                                                    // => [0x00 0x01] as decimal

    bigBuf := new(bytes.Buffer)                     // => New empty buffer
    binary.Write(bigBuf, binary.BigEndian, int16(256))
                                                    // => 256 in decimal = 0x0100 in hex
                                                    // => Big-endian: most significant byte first
                                                    // => Writes [0x01 0x00] (high byte, low byte)
    fmt.Printf("Big-endian bytes: %v\n", bigBuf.Bytes())
                                                    // => Output: Big-endian bytes: [1 0]
                                                    // => [0x01 0x00] as decimal
                                                    // => Demonstrates endianness difference
}
```

**Key Takeaway**: `binary.Write()` serializes values to binary format. `binary.Read()` deserializes from binary. Specify endianness (`BigEndian` or `LittleEndian`). Endianness is crucial for network protocols and file formats.

**Why It Matters**: Binary encoding enables interoperability with network protocols (TCP packet headers, DNS messages) and file formats (PNG, MP4) that require precise byte-level control and endianness awareness. Big-endian (network byte order) dominates internet protocols for historical compatibility, while little-endian matches modern CPU architectures (x86, ARM), making encoding/binary essential for implementing custom protocols or parsing binary file formats in production systems.

## Example 68: Cryptography Basics

Cryptography is essential for security. Go provides standard cryptographic functions in `crypto/*` packages. Hash for integrity, random for security, HMAC for authentication, encryption for confidentiality.

**Code**:

```go
package main

import (
    "crypto/hmac"
    "crypto/rand"
    "crypto/sha256"
    "encoding/hex"
    "fmt"
)

func main() {
    // SHA256 hash - integrity check
    data := "Important message"                 // => data is string to hash
                                                // => Fixed input produces fixed hash
    hash := sha256.Sum256([]byte(data))         // => Compute SHA-256 hash
                                                // => []byte(data) converts string to bytes
                                                // => hash is [32]byte array (256 bits)
                                                // => Deterministic: same input = same hash
    fmt.Printf("SHA256: %s\n", hex.EncodeToString(hash[:]))
                                                // => hash[:] converts array to slice
                                                // => hex.EncodeToString converts bytes to hex string
                                                // => Output: SHA256: a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3
                                                // => 64 hex characters (32 bytes * 2)

    // HMAC - authentication
    key := []byte("secret-key")                 // => key is secret shared key ([]byte)
                                                // => Same key needed for signing and verification
    h := hmac.New(sha256.New, key)              // => Create HMAC-SHA256 hasher
                                                // => sha256.New is hash function factory
                                                // => h implements hash.Hash interface
    h.Write([]byte(data))                       // => Add data to hash
                                                // => Can call Write multiple times
                                                // => []byte(data) converts string to bytes
    signature := hex.EncodeToString(h.Sum(nil)) // => Get signature as hex
                                                // => h.Sum(nil) returns []byte signature
                                                // => nil means no prefix bytes
                                                // => signature is 64-character hex string
    fmt.Println("HMAC:", signature)             // => Output: HMAC: <64 hex chars>
                                                // => Unique for this data+key combination

    // Verify HMAC
    h2 := hmac.New(sha256.New, key)             // => Create new HMAC hasher
                                                // => Same key and hash function
    h2.Write([]byte(data))                      // => Hash same data
                                                // => Should produce identical signature
    if hmac.Equal(h.Sum(nil), h2.Sum(nil)) {    // => Compare HMAC values
                                                // => hmac.Equal prevents timing attacks
                                                // => Constant-time comparison
                                                // => Returns true if signatures match
        fmt.Println("HMAC valid")               // => Output: HMAC valid
                                                // => Confirms data not tampered
    }

    // Random bytes - for tokens, nonces
    token := make([]byte, 16)                   // => token is 16-byte slice (128 bits)
                                                // => Initialized to zeros
    _, err := rand.Read(token)                  // => Read 16 random bytes from crypto/rand
                                                // => Cryptographically secure randomness
                                                // => Fills token slice with random data
                                                // => Returns (n int, err error)
    if err != nil {                             // => Check for error (rare)
        fmt.Println("Error generating random:", err)
                                                // => Output error if random generation fails
        return                                  // => Exit function early
    }
    fmt.Printf("Random token: %s\n", hex.EncodeToString(token))
                                                // => Convert random bytes to hex string
                                                // => Output: Random token: a1b2c3d4e5f6g7h8i9j0k1l2m3n4o5p6 (example)
                                                // => 32 hex characters (16 bytes * 2)
                                                // => Different every time (unpredictable)
}
```

**Key Takeaway**: Use `crypto/sha256.Sum256()` for hashing. Use `crypto/hmac` with hash function for authentication. Use `crypto/rand.Read()` for cryptographically secure random bytes. Never use `math/rand` for security-sensitive operations.

**Why It Matters**: Cryptographic primitives are non-negotiable for production security: SHA-256 hashes verify file integrity (git commits, Docker image layers), HMAC-SHA256 authenticates webhook payloads (GitHub webhooks, Stripe signatures) preventing tampering, and crypto/rand generates session tokens that withstand cryptanalysis unlike math/rand's predictable sequences. Using crypto/rand instead of math/rand for security tokens is the difference between safe authentication and immediate compromise.

## Example 69: Templates

Templates generate text (HTML, email, config files). The `text/template` package provides template syntax with variables, functions, and control flow. Use `html/template` for HTML to prevent injection attacks.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    A["Template String<br/>Hello {{.Name}}"]
    B["template.Parse()"]
    C["Compiled Template"]
    D["Data<br/>{Name: Alice}"]
    E["template.Execute()"]
    F["Output<br/>Hello Alice"]

    A --> B
    B --> C
    C --> E
    D --> E
    E --> F

    style A fill:#0173B2,stroke:#000,color:#fff
    style B fill:#DE8F05,stroke:#000,color:#fff
    style C fill:#029E73,stroke:#000,color:#fff
    style D fill:#CC78BC,stroke:#000,color:#fff
    style E fill:#CA9161,stroke:#000,color:#fff
    style F fill:#0173B2,stroke:#000,color:#fff
```

**Code**:

```go
package main

import (
    "fmt"
    "html/template"
    "os"
    "strings"
)

func main() {
    // Simple template with variables
    tmpl, err := template.New("test").Parse("Hello, {{.Name}}! You are {{.Age}} years old.")
                                                    // => Create new template named "test"
                                                    // => Parse template string with {{}} placeholders
                                                    // => {{.Name}} accesses Name field in data
                                                    // => {{.Age}} accesses Age field
                                                    // => Returns (*Template, error)
    if err != nil {                                 // => Check parse error
                                                    // => Syntax errors caught here
        fmt.Println("Parse error:", err)            // => Output: Parse error: <error>
        return                                      // => Exit on error
    }

    data := map[string]interface{}{                 // => Data to render template
        "Name": "Alice",                            // => Name field is "Alice"
        "Age":  30,                                 // => Age field is 30 (int)
    }
    tmpl.Execute(os.Stdout, data)                   // => Execute template with data
                                                    // => Writes to os.Stdout (stdout)
                                                    // => {{.Name}} replaced with "Alice"
                                                    // => {{.Age}} replaced with 30
    // => Output: Hello, Alice! You are 30 years old.
                                                    // => Template rendered to output

    // Conditional and loops
    tmpl2, _ := template.New("list").Parse(`
Users:
{{range .Users}}
  - {{.Name}} ({{.Age}})
{{end}}
`)                                                  // => Multiline template string
                                                    // => {{range .Users}} iterates slice
                                                    // => Inside range, . is each element
                                                    // => {{end}} closes range block
                                                    // => _ ignores error (for brevity)

    data2 := map[string]interface{}{                // => Data with Users slice
        "Users": []map[string]interface{}{          // => Slice of user maps
            {"Name": "Alice", "Age": 30},           // => First user
            {"Name": "Bob", "Age": 25},             // => Second user
        },
    }
    tmpl2.Execute(os.Stdout, data2)                 // => Execute template
                                                    // => {{range}} iterates Users slice
                                                    // => First iteration: . is {"Name": "Alice", "Age": 30}
                                                    // => Second iteration: . is {"Name": "Bob", "Age": 25}
    // => Output:
    // => Users:
    // =>   - Alice (30)                            // => First user rendered
    // =>   - Bob (25)                              // => Second user rendered

    // Custom functions
    funcMap := template.FuncMap{                    // => Map of custom template functions
                                                    // => FuncMap is map[string]interface{}
        "upper": strings.ToUpper,                   // => Add function to template
                                                    // => upper calls strings.ToUpper
        "add":   func(a, b int) int { return a + b }, // => Custom function
                                                    // => add accepts 2 ints, returns sum
    }

    tmpl3, _ := template.New("funcs").Funcs(funcMap).Parse(
        "{{upper .Name}} total is {{add .Age .Years}}",
    )                                               // => Create template with custom functions
                                                    // => Funcs() adds functions before Parse()
                                                    // => {{upper .Name}} calls upper function
                                                    // => {{add .Age .Years}} calls add function

    data3 := map[string]interface{}{                // => Data for function template
        "Name":  "alice",                           // => Name is lowercase "alice"
        "Age":   30,                                // => Age is 30
        "Years": 5,                                 // => Years is 5
    }
    tmpl3.Execute(os.Stdout, data3)                 // => Execute template
                                                    // => {{upper .Name}} becomes "ALICE"
                                                    // => {{add .Age .Years}} becomes 35 (30+5)
    // => Output: ALICE total is 35                 // => Custom functions applied
}
```

**Key Takeaway**: Use `template.Parse()` to create templates. Use `.Field` to access data. Use `{{range}}` for loops, `{{if}}` for conditions. Use `html/template` instead of `text/template` for HTML to prevent injection. Define custom functions with `FuncMap`.

**Why It Matters**: Templates generate dynamic HTML, emails, and configuration files while preventing injection attacks through automatic escaping in html/template. Production systems use templates for rendering web pages (Hugo static site generator), generating Kubernetes manifests from values, and composing email notifications with user data, where text/template's lack of escaping would allow XSS attacks if user input reaches the output.

## Group 3: Generics (Go 1.18+)

## Example 70: Generic Functions

Generics enable functions to work with different types while maintaining type safety. Type parameters in square brackets define constraints. Go 1.18+ introduces this powerful feature.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    A["Generic Function<br/>max[T Ordered]"]
    B["Called with []int"]
    C["Called with []string"]
    D["Called with []float64"]
    E["Compiler generates<br/>max_int version"]
    F["Compiler generates<br/>max_string version"]
    G["Compiler generates<br/>max_float64 version"]

    A --> B
    A --> C
    A --> D
    B --> E
    C --> F
    D --> G

    style A fill:#0173B2,stroke:#000,color:#fff
    style B fill:#DE8F05,stroke:#000,color:#fff
    style C fill:#DE8F05,stroke:#000,color:#fff
    style D fill:#DE8F05,stroke:#000,color:#fff
    style E fill:#029E73,stroke:#000,color:#fff
    style F fill:#029E73,stroke:#000,color:#fff
    style G fill:#029E73,stroke:#000,color:#fff
```

**Code**:

```go
package main

import "fmt"

func main() {
    // Generic function works with different types
    intSlice := []int{3, 1, 4, 1, 5}            // => intSlice is []int with 5 elements
                                                // => Contains: [3, 1, 4, 1, 5]
    fmt.Println("Max int:", max(intSlice))      // => Calls max with T=int
                                                // => Output: Max int: 5
                                                // => Compiler generates max_int version

    stringSlice := []string{"apple", "zebra", "banana"}
                                                // => stringSlice is []string with 3 elements
                                                // => Contains: ["apple", "zebra", "banana"]
    fmt.Println("Max string:", max(stringSlice))// => Calls max with T=string
                                                // => Output: Max string: zebra
                                                // => Compiler generates max_string version
}

// Generic function - [T any] is type parameter
// T is constrained to "any" type
func max[T any](slice []T) T {                  // => Type parameter T can be any type
                                                // => any is alias for interface{}
                                                // => slice is []T (slice of type T)
    // Compiler error: can't compare T values (no constraint)
    // This won't work because we need to define T must be comparable
    return slice[0]                             // => Returns first element (placeholder)
                                                // => Real implementation needs constraints
}

// Better: constrain T to be comparable
import "fmt"

// Comparable constraint - enables comparison operators
func betterMax[T interface{ int | float64 | string }](slice []T) T {
                                                // => T constrained to int OR float64 OR string
                                                // => Union type constraint
                                                // => Enables > operator for these types
    if len(slice) == 0 {                        // => Check for empty slice
                                                // => len(slice) returns 0 for empty
        var zero T                              // => zero is zero value for type T
                                                // => T=int: zero=0, T=string: zero=""
        return zero                             // => Return zero value
    }

    max := slice[0]                             // => max starts with first element
                                                // => max is type T
    for _, val := range slice[1:] {             // => Iterate from index 1 onwards
                                                // => slice[1:] creates subslice
                                                // => val is each element (type T)
        if val > max {                          // => Compare values (works with constraint)
                                                // => > operator enabled by union constraint
            max = val                           // => Update max if val is greater
        }
    }
    return max                                  // => Return maximum value (type T)
}

// Even better: use Ordered constraint (Go 1.21+)
import "golang.org/x/exp/constraints"

func bestMax[T constraints.Ordered](slice []T) T {
                                                // => T constrained to Ordered types
                                                // => Ordered includes: integers, floats, strings
                                                // => Standard constraint from constraints package
    if len(slice) == 0 {                        // => Handle empty slice
        var zero T                              // => zero value for type T
        return zero                             // => Return zero value
    }

    max := slice[0]                             // => Initialize max with first element
                                                // => max is type T
    for _, val := range slice[1:] {             // => Iterate remaining elements
                                                // => val is each element after first
        if val > max {                          // => Compare using > operator
                                                // => Ordered constraint enables comparisons
            max = val                           // => Update max if needed
        }
    }
    return max                                  // => Return maximum value found
                                                // => Type is T (preserves input type)
}
```

**Key Takeaway**: Generic functions use `[T TypeConstraint]` syntax. Type parameter T is replaced with actual type at compile-time. Constraints limit what operations T supports. `any` means no constraints (but limited what you can do).

**Why It Matters**: Generics (Go 1.18+) eliminate code duplication for data structures and algorithms, replacing brittle interface{}+reflection patterns with compile-time type safety. Generic max() functions work across int/float/string without type assertions, generic min-heaps work with any comparable type, and generic Result<T,E> types enable Rust-style error handling, all providing zero runtime overhead through monomorphization (compiler generates specialized versions per type).

## Example 71: Generic Types

Generic struct types work similarly to generic functions. Define type parameters, and the compiler instantiates them for each type used. Useful for containers, queues, trees, and data structures.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    A["Generic Type<br/>Stack[T any]"]
    B["Stack[int]<br/>items []int"]
    C["Stack[string]<br/>items []string"]
    D["Stack[Person]<br/>items []Person"]

    A -->|instantiate| B
    A -->|instantiate| C
    A -->|instantiate| D

    B --> E["Push(10)<br/>Pop() int"]
    C --> F["Push(\"hi\")<br/>Pop() string"]
    D --> G["Push(person)<br/>Pop() Person"]

    style A fill:#0173B2,stroke:#000,color:#fff
    style B fill:#DE8F05,stroke:#000,color:#fff
    style C fill:#DE8F05,stroke:#000,color:#fff
    style D fill:#DE8F05,stroke:#000,color:#fff
    style E fill:#029E73,stroke:#000,color:#fff
    style F fill:#029E73,stroke:#000,color:#fff
    style G fill:#029E73,stroke:#000,color:#fff
```

**Code**:

```go
package main

import "fmt"

func main() {
    // Generic stack of integers
    intStack := NewStack[int]()                     // => Create Stack[int] instance
                                                    // => T=int, instantiates Stack with int
                                                    // => intStack is *Stack[int]
    intStack.Push(10)                               // => Push 10 (type: int)
                                                    // => items is now []int{10}
    intStack.Push(20)                               // => Push 20 (type: int)
                                                    // => items is now []int{10, 20}
    fmt.Println("Pop:", intStack.Pop())             // => Pop returns 20 (last item)
                                                    // => Output: Pop: 20
                                                    // => items is now []int{10}

    // Generic stack of strings
    stringStack := NewStack[string]()               // => Create Stack[string] instance
                                                    // => T=string, instantiates Stack with string
                                                    // => stringStack is *Stack[string]
    stringStack.Push("hello")                       // => Push "hello" (type: string)
                                                    // => items is now []string{"hello"}
    stringStack.Push("world")                       // => Push "world" (type: string)
                                                    // => items is now []string{"hello", "world"}
    fmt.Println("Pop:", stringStack.Pop())          // => Pop returns "world" (last item)
                                                    // => Output: Pop: world
                                                    // => items is now []string{"hello"}
}

// Generic stack type
type Stack[T any] struct {                          // => Type parameter T (can be any type)
                                                    // => Stack is generic over T
                                                    // => Compiler generates type-specific versions
    items []T                                       // => Slice of type T (not interface{})
                                                    // => T=int: items is []int
                                                    // => T=string: items is []string
}

// Generic methods
func (s *Stack[T]) Push(item T) {                   // => Method on Stack[T]
                                                    // => item must be type T
                                                    // => Receiver is *Stack[T] (pointer)
    s.items = append(s.items, item)                 // => Append item to slice
                                                    // => Type-safe: item is T, items is []T
                                                    // => Grows slice if needed
}

func (s *Stack[T]) Pop() T {                        // => Method returns type T
                                                    // => Return type matches Stack type parameter
    if len(s.items) == 0 {                          // => Check for empty stack
                                                    // => Prevents out-of-bounds access
        var zero T                                  // => zero is zero value for type T
                                                    // => T=int: zero=0, T=string: zero=""
        return zero                                 // => Return zero value (safe default)
    }
    lastIdx := len(s.items) - 1                     // => Index of last element
                                                    // => len(s.items)-1 is last valid index
    item := s.items[lastIdx]                        // => Get last item
                                                    // => item is type T
    s.items = s.items[:lastIdx]                     // => Remove last item (slice truncation)
                                                    // => Creates new slice view
                                                    // => Original item unreachable (GC'd)
    return item                                     // => Return popped item (type T)
}

// Constructor
func NewStack[T any]() *Stack[T] {                  // => Generic constructor function
                                                    // => T is type parameter
                                                    // => Returns pointer to Stack[T]
    return &Stack[T]{                               // => Create Stack instance
                                                    // => Stack[T] instantiated with type T
        items: make([]T, 0),                        // => Initialize empty slice of type T
                                                    // => Capacity 0 (grows as needed)
    }
}

// Generic interface
type Container[T any] interface {                   // => Generic interface over type T
                                                    // => Types implementing must specify T
    Add(T)                                          // => Method accepting type T
                                                    // => T must match interface instantiation
    Remove() T                                      // => Method returning type T
                                                    // => Type-safe: always same T
}
                                                    // => Stack[T] satisfies Container[T]
                                                    // => Stack[int] implements Container[int]
                                                    // => Stack[string] implements Container[string]
```

**Key Takeaway**: Define generic types with `Type[T TypeParam]`. Methods on generic types use the type parameter. Construct with `NewGeneric[Type]()`. Generic types enable type-safe reusable containers.

**Why It Matters**: Generic containers like Stack[T], Queue[T], and Cache[K,V] provide type-safe reusable data structures without runtime type assertions or interface{} boxing overhead. Where pre-generics Go required separate IntStack/StringStack implementations or unsafe interface{} casts, generic types enable writing containers once and using everywhere with full compile-time type checking, eliminating entire classes of runtime type errors.

## Example 72: Constraints and Comparable

Go provides standard constraints in `constraints` package. The `comparable` constraint enables `==` and `!=` operators. Custom constraints combine types and interfaces.

**Code**:

```go
package main

import (
    "fmt"
    "golang.org/x/exp/constraints"
)

func main() {
    // Numbers constraint - int, float, complex types
    fmt.Println("Sum ints:", sum([]int{1, 2, 3}))         // => 6
    fmt.Println("Sum floats:", sum([]float64{1.5, 2.5})) // => 4.0

    // Comparable constraint - can use == and !=
    if contains([]string{"a", "b", "c"}, "b") {
        fmt.Println("Found")
    }

    // Custom constraint - combine types and interface
    var m map[string]int = make(map[string]int)
    m["key"] = 10
    fmt.Println("Value:", getValue(m, "key")) // => 10
}

// Ordered constraint - can use <, >, <=, >=, ==, !=
func sum[T constraints.Integer](nums []T) T { // => Integer: int types
    var total T
    for _, n := range nums {
        total += n
    }
    return total
}

// Comparable constraint - can use == and !=
func contains[T comparable](slice []T, target T) bool { // => comparable
    for _, item := range slice {
        if item == target {     // => Works with any comparable type
            return true
        }
    }
    return false
}

// Custom constraint
type MapKey interface { // => Custom constraint
    comparable          // => Must satisfy comparable (can use ==)
}

func getValue[K MapKey, V any](m map[K]V, key K) V { // => Two type parameters
    return m[key]
}
```

**Key Takeaway**: `constraints.Ordered` = types supporting comparison operators. `constraints.Integer` = integer types. `comparable` = types supporting `==` and `!=`. Custom constraints combine interfaces and types.

**Why It Matters**: Type constraints enable generic functions to use operators (comparable for ==, constraints.Ordered for <>) while maintaining type safety, solving the pre-generics problem where generic code couldn't perform comparisons without reflection. The comparable constraint powers generic contains() functions and Set[T] implementations, while constraints.Ordered enables generic min/max/sort functions, providing operator support that interface{} could never offer.

## Group 4: Advanced Patterns

## Example 73: Options Pattern

The options pattern provides flexible configuration through functional options. Each option function modifies configuration without requiring many constructors or mutating shared state.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    A["NewServer#40;#41;"]
    B["Default Config<br/>Host: 0.0.0.0<br/>Port: 80"]
    C["WithHost#40;localhost#41;"]
    D["WithPort#40;8080#41;"]
    E["WithTimeout#40;30#41;"]
    F["Final Config<br/>Host: localhost<br/>Port: 8080<br/>Timeout: 30"]

    A --> B
    B --> C
    C --> D
    D --> E
    E --> F

    style A fill:#0173B2,stroke:#000,color:#fff
    style B fill:#DE8F05,stroke:#000,color:#fff
    style C fill:#029E73,stroke:#000,color:#fff
    style D fill:#029E73,stroke:#000,color:#fff
    style E fill:#029E73,stroke:#000,color:#fff
    style F fill:#CC78BC,stroke:#000,color:#fff
```

**Code**:

```go
package main

import "fmt"

func main() {
    // Simple config without options
    server1 := NewServer()
    fmt.Println(server1)

    // Config with options
    server2 := NewServer(
        WithHost("localhost"),    // => Functional option
        WithPort(8080),           // => Functional option
        WithTimeout(30),          // => Functional option
    )
    fmt.Println(server2)

    // Mix options
    server3 := NewServer(
        WithPort(9000),
        // WithHost uses default
        WithTimeout(60),
    )
    fmt.Println(server3)
}

type Server struct {
    Host    string
    Port    int
    Timeout int
}

// Functional option type
type Option func(*Server) // => Option is function that modifies Server

// Constructor
func NewServer(opts ...Option) *Server { // => Variadic options
    s := &Server{
        Host:    "0.0.0.0",         // => Default values
        Port:    80,
        Timeout: 10,
    }

    for _, opt := range opts {
        opt(s)                      // => Apply each option
    }

    return s
}

// Option functions
func WithHost(host string) Option { // => Returns Option function
    return func(s *Server) {
        s.Host = host              // => Modify server
    }
}

func WithPort(port int) Option {
    return func(s *Server) {
        s.Port = port
    }
}

func WithTimeout(timeout int) Option {
    return func(s *Server) {
        s.Timeout = timeout
    }
}

func (s Server) String() string {
    return fmt.Sprintf("Server{Host: %s, Port: %d, Timeout: %d}", s.Host, s.Port, s.Timeout)
}
```

**Key Takeaway**: Options pattern accepts variadic functions that modify a config struct. Each option is a function that receives and modifies the struct. Enables flexible API without many constructors or mutating state.

**Why It Matters**: Functional options enable building flexible APIs with backward compatibility, where adding new server configuration options (timeouts, TLS settings, middleware) doesn't break existing code or require dozens of constructor variants. Used extensively in production libraries (gRPC, Kubernetes client-go), this pattern provides clean defaults, discoverability through named functions (WithTimeout()), and nil-safety compared to struct literals where missing fields silently use zero values.

## Example 74: Embed Directive (Go 1.16+)

The `//go:embed` directive embeds files into the binary at compile-time. Useful for static assets, templates, or configuration files that should be part of the executable.

**Code**:

```go
package main

import (
    "embed"
    "fmt"
)

func main() {
    // Embedded file
    fmt.Println("HTML template:")
    fmt.Println(string(htmlContent)) // => Content embedded at compile-time

    // Embedded file system
    entries, _ := fs.ReadDir("templates")
    fmt.Println("Embedded files:", len(entries))
    for _, entry := range entries {
        fmt.Println("  -", entry.Name())
    }

    // Read specific file from embedded FS
    content, _ := fs.ReadFile(fs.FS(templates), "index.html")
    fmt.Println("File content:", string(content))
}

// Single file
//go:embed templates/index.html
var htmlContent []byte // => Content embedded at compile-time

// File system
//go:embed templates/*
var templates embed.FS // => Entire directory embedded

// String content
//go:embed config.json
var config string

import "embed"
```

**Key Takeaway**: `//go:embed path` embeds files into the binary. Single file type is `[]byte` or `string`. Directory type is `embed.FS`. Files are embedded at compile-time, no runtime file system access needed.

**Why It Matters**: Embedding files into binaries eliminates deployment dependencies and version skew, enabling single-binary deployment where static assets (HTML templates, SQL migrations, configuration defaults) ship inside the executable. This powers Hugo's single-binary distribution with 300+ embedded templates, eliminates "template file not found" runtime errors, and enables hermetic builds where embedded files cannot be tampered with post-compilation, critical for security-sensitive applications.

## Example 75: Build Tags

Build tags enable conditional compilation. Platform-specific code, feature flags, or test-only code can be controlled with build tags. Tag expressions determine which files compile.

**Code**:

```go
// file: server_unix.go
//go:build unix || linux
// +build unix linux

package main

import "fmt"

func getPlatform() string {
    return "Unix/Linux"            // => Compiled only on Unix/Linux
}

// file: server_windows.go
//+build windows

package main

import "fmt"

func getPlatform() string {
    return "Windows"               // => Compiled only on Windows
}

// file: main.go
package main

func main() {
    fmt.Println("Platform:", getPlatform()) // => Platform-specific implementation
}

// Usage: go build, go build -tags=feature1, go run -tags="tag1,tag2"

// Multiple tags in single file
//go:build (linux || darwin) && !debug
// +build linux darwin
// +build !debug

package main

func shouldDebug() bool {
    return false                   // => Compiled on Linux/Mac without debug tag
}
```

**Key Takeaway**: `//go:build expression` (Go 1.16+) controls compilation. `-tags flag` enables tags at build-time. Use for platform-specific code, feature flags, and integration tests that require external services.

**Why It Matters**: Build tags enable platform-specific implementations without runtime checks, where Unix syscalls compile only on Linux/Mac and Windows equivalents compile only on Windows, producing optimized binaries without dead code. Production use cases include feature flags (build with/without premium features), integration tests that require external services (skip in CI with `//go:build !integration`), and specialized builds (embed debug symbols only in development builds).

## Example 76: Custom Sorting

Sorting requires implementing the `sort.Interface` or using `sort.Slice()`. Custom sorting enables ordering by different fields or complex criteria.

**Code**:

```go
package main

import (
    "fmt"
    "sort"
)

func main() {
    users := []User{
        {Name: "Alice", Age: 30, Score: 95},
        {Name: "Bob", Age: 25, Score: 87},
        {Name: "Charlie", Age: 30, Score: 92},
    }

    // Sort by name
    sort.Sort(ByName(users))
    fmt.Println("By name:", users)

    // Sort by score descending
    sort.Sort(ByScoreDesc(users))
    fmt.Println("By score desc:", users)

    // Custom sort with Slice (simpler for one-off sorts)
    sort.Slice(users, func(i, j int) bool {
        if users[i].Age == users[j].Age {
            return users[i].Score > users[j].Score // => Age then score desc
        }
        return users[i].Age < users[j].Age        // => Age ascending
    })
    fmt.Println("By age then score:", users)
}

type User struct {
    Name  string
    Age   int
    Score int
}

// Implement sort.Interface
type ByName []User

func (b ByName) Len() int           { return len(b) }
func (b ByName) Swap(i, j int)      { b[i], b[j] = b[j], b[i] }
func (b ByName) Less(i, j int) bool { return b[i].Name < b[j].Name }

type ByScoreDesc []User

func (b ByScoreDesc) Len() int           { return len(b) }
func (b ByScoreDesc) Swap(i, j int)      { b[i], b[j] = b[j], b[i] }
func (b ByScoreDesc) Less(i, j int) bool { return b[i].Score > b[j].Score }
```

**Key Takeaway**: Implement `sort.Interface` (Len, Swap, Less) for custom sorting. Use `sort.Slice()` for one-off sorts with custom comparator. Implement Less such that `Less(i, j)` returns true if element i should come before j.

**Why It Matters**: Custom sorting enables domain-specific ordering beyond simple field comparisons, where sorting users by age-then-score or products by price-then-rating requires multi-field comparison logic. While sort.Interface provides maximum flexibility, sort.Slice() reduces boilerplate for ad-hoc sorts, making production code that sorts API responses or database query results by complex criteria both maintainable and performant (O(n log n) quicksort).

## Example 77: Dependency Injection

Dependency injection passes dependencies to functions/types instead of creating them internally. Enables testing with mock dependencies and decouples implementations.

```mermaid
%% Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC, Brown #CA9161
graph TD
    A["UserService<br/>depends on Database"]
    B["Production:<br/>Inject RealDB"]
    C["Testing:<br/>Inject MockDB"]
    D["UserService uses<br/>Database interface"]
    E["RealDB.Query()<br/>Real SQL"]
    F["MockDB.Query()<br/>Hardcoded data"]

    A --> D
    B --> E
    C --> F
    D -.->|production| E
    D -.->|testing| F

    style A fill:#0173B2,stroke:#000,color:#fff
    style B fill:#DE8F05,stroke:#000,color:#fff
    style C fill:#DE8F05,stroke:#000,color:#fff
    style D fill:#029E73,stroke:#000,color:#fff
    style E fill:#CC78BC,stroke:#000,color:#fff
    style F fill:#CA9161,stroke:#000,color:#fff
```

**Code**:

```go
package main

import (
    "fmt"
)

func main() {
    // Real database
    db := &RealDB{}
    userService := NewUserService(db) // => Inject real DB

    fmt.Println(userService.GetUser(1))

    // Mock database for testing
    mockDB := &MockDB{}
    userService2 := NewUserService(mockDB) // => Inject mock DB

    fmt.Println(userService2.GetUser(1))
}

// Interface for testability
type Database interface {
    Get(id int) string
}

// Real implementation
type RealDB struct{}

func (r *RealDB) Get(id int) string {
    return fmt.Sprintf("User %d from database", id)
}

// Mock for testing
type MockDB struct{}

func (m *MockDB) Get(id int) string {
    return fmt.Sprintf("Mock user %d", id)
}

// Service - depends on Database interface, not concrete type
type UserService struct {
    db Database              // => Depend on interface, not concrete type
}

// Constructor injection
func NewUserService(db Database) *UserService { // => Inject dependency
    return &UserService{db: db}
}

func (s *UserService) GetUser(id int) string {
    return s.db.Get(id)     // => Use injected dependency
}
```

**Key Takeaway**: Depend on interfaces, not concrete types. Pass dependencies through constructors. Enables testing with mock implementations and decouples code from specific implementations.

**Why It Matters**: Dependency injection through interfaces enables testing with mock implementations, where production code uses real databases while tests use in-memory mocks, achieving 100x faster test execution without external dependencies. This pattern decouples business logic from infrastructure (database, HTTP client, message queue), enabling testing in isolation and swapping implementations without modifying core logic, the foundation of testable production systems.

## Group 5: Testing and Tooling

## Example 78: Subtests

Subtests organize tests hierarchically with `t.Run()`. Each subtest can have setup/teardown and reports individually. Subtests can run in parallel with `t.Parallel()`.

**Code**:

```go
package main

import "testing"

func TestUserService(t *testing.T) {
    // Parent test setup
    users := setupTestData()

    t.Run("GetUser", func(t *testing.T) {
        t.Run("ExistingUser", func(t *testing.T) {
            user := findUser(users, 1)
            if user.Name != "Alice" {
                t.Errorf("Expected Alice, got %s", user.Name)
            }
        })

        t.Run("NonExistentUser", func(t *testing.T) {
            user := findUser(users, 999)
            if user != nil {
                t.Errorf("Expected nil, got %v", user)
            }
        })
    })

    t.Run("CreateUser", func(t *testing.T) {
        newUser := User{Name: "David", Age: 28}
        created := createUser(newUser)
        if created.Name != "David" {
            t.Errorf("Expected David, got %s", created.Name)
        }
    })
}

// Parallel subtests
func TestParallel(t *testing.T) {
    t.Run("Sequential", func(t *testing.T) {
        // Parent doesn't use t.Parallel(), runs sequentially
    })

    t.Run("Parallel", func(t *testing.T) {
        t.Parallel()            // => Can run in parallel with other t.Parallel() tests
        // Parallel test code
    })
}

type User struct {
    Name string
    Age  int
}

func setupTestData() []*User {
    return []*User{
        {Name: "Alice", Age: 30},
        {Name: "Bob", Age: 25},
    }
}

func findUser(users []*User, id int) *User {
    if id > 0 && id <= len(users) {
        return users[id-1]
    }
    return nil
}

func createUser(u User) User {
    return u
}
```

**Key Takeaway**: `t.Run()` creates subtests that report individually. Use for organizing tests into logical groups. `t.Parallel()` enables parallel execution for tests without shared state. Subtests can have separate setup/teardown.

**Why It Matters**: Subtests organize complex test suites hierarchically with independent reporting, where TestUserService/GetUser/ExistingUser and TestUserService/GetUser/NonExistent run as separate test cases with isolated setup/teardown. Combined with t.Parallel(), subtests enable safe concurrent test execution, reducing CI time from minutes to seconds for large test suites while maintaining clarity through structured test names and granular failure reporting.

## Example 79: Mocking with Interfaces

Testing requires isolating code under test from external dependencies. Mock implementations of interfaces enable testing without real services like databases or APIs.

**Code**:

```go
package main

import (
    "testing"
)

func TestUserRepository(t *testing.T) {
    // Mock storage
    mock := &MockStorage{
        data: map[int]User{
            1: {ID: 1, Name: "Alice"},
        },
    }

    repo := NewUserRepository(mock)

    user, err := repo.Get(1)
    if err != nil {
        t.Errorf("Unexpected error: %v", err)
    }
    if user.Name != "Alice" {
        t.Errorf("Expected Alice, got %s", user.Name)
    }
}

type User struct {
    ID   int
    Name string
}

// Interface for testability
type Storage interface {
    Get(id int) (User, error)
    Save(u User) error
}

// Real storage
type Database struct{}

func (d *Database) Get(id int) (User, error) {
    // Real database query
    return User{}, nil
}

func (d *Database) Save(u User) error {
    // Real database write
    return nil
}

// Mock storage
type MockStorage struct {
    data map[int]User
}

func (m *MockStorage) Get(id int) (User, error) {
    if user, ok := m.data[id]; ok {
        return user, nil
    }
    return User{}, nil
}

func (m *MockStorage) Save(u User) error {
    m.data[u.ID] = u
    return nil
}

// Repository - depends on Storage interface
type UserRepository struct {
    storage Storage
}

func NewUserRepository(s Storage) *UserRepository {
    return &UserRepository{storage: s}
}

func (r *UserRepository) Get(id int) (User, error) {
    return r.storage.Get(id)
}
```

**Key Takeaway**: Mock implementations satisfy interfaces. Inject mocks into code under test. Mocks enable testing without real external services. Use simple in-memory mocks for fast tests.

**Why It Matters**: Mock implementations through interfaces enable fast, deterministic tests without external dependencies, where testing HTTP handlers doesn't require running real servers and testing database queries doesn't require PostgreSQL. Production codebases achieve sub-second test suites (vs multi-minute integration tests) by mocking Storage, HTTPClient, and MessageQueue interfaces with in-memory implementations that simulate success/failure scenarios without network latency or flaky external services.

## Example 80: Fuzzing (Go 1.18+)

Fuzzing automatically generates random inputs to find edge cases and crashes. Go's built-in fuzzing runs test function with generated and seed values.

**Code**:

```go
package main

import (
    "testing"
    "unicode/utf8"
)

// Run fuzzing: go test -fuzz=FuzzParseInt
func FuzzParseInt(f *testing.F) {
    // Seed values - good test cases to always include
    f.Add("0")
    f.Add("42")
    f.Add("-100")
    f.Add("2147483647")

    f.Fuzz(func(t *testing.T, input string) { // => Fuzz function
        // The fuzzer generates many values for input
        if len(input) == 0 {
            return                // => Skip empty input
        }

        result, err := parseInt(input) // => Test with generated input
        _ = result
        _ = err
        // No assertion - fuzzer looks for panics and crashes
    })
}

// Fuzzing UTF-8 strings
func FuzzValidUTF8(f *testing.F) {
    f.Add("hello")
    f.Add("世界")

    f.Fuzz(func(t *testing.T, input string) {
        // Fuzz will provide valid UTF-8 (f.Add only adds valid strings)
        // Fuzzer generates variations
        if !utf8.ValidString(input) {
            t.Errorf("Invalid UTF-8: %v", input)
        }
    })
}

func parseInt(s string) (int, error) {
    // Simple parser for fuzzing
    if s == "0" {
        return 0, nil
    }
    return 1, nil
}
```

**Key Takeaway**: Fuzzing tests provide generated inputs. Seed values with `f.Add()` include important test cases. The `f.Fuzz()` function receives generated inputs. Fuzzer looks for panics and crashes in your code.

**Why It Matters**: Fuzzing automatically discovers edge cases (empty inputs, Unicode boundary conditions, integer overflows) that manual testing misses, where fuzz testing parsers or validators finds crashes and security vulnerabilities from malformed input. Used extensively in security-critical code (parsers, decoders, validators), fuzzing has found vulnerabilities in stdlib packages and production services by generating millions of test inputs, achieving coverage that would take years of manual test writing.

## Example 81: CGO Basics

CGO enables calling C from Go. Use when you need external C libraries or performance-critical code. CGO adds complexity - prefer pure Go when possible.

```mermaid
%% Go-C interop boundary and type conversion
graph TD
    A["Go Code<br/>goString string"]
    B["C.CString()<br/>Type Conversion"]
    C["C Memory<br/>*char (malloc)"]
    D["C Function<br/>strlen_c()"]
    E["Return Value<br/>C.int"]
    F["Type Conversion<br/>int()"]
    G["Go Code<br/>length int"]
    H["C.free()<br/>Cleanup"]

    A -->|"Go string"| B
    B -->|"*C.char"| C
    C -->|"Pass pointer"| D
    D -->|"C.int result"| E
    E -->|"Convert"| F
    F -->|"Go int"| G
    C -.->|"Must free!"| H

    style A fill:#0173B2,stroke:#000,color:#fff
    style B fill:#DE8F05,stroke:#000,color:#fff
    style C fill:#CA9161,stroke:#000,color:#fff
    style D fill:#CC78BC,stroke:#000,color:#fff
    style E fill:#CA9161,stroke:#000,color:#fff
    style F fill:#DE8F05,stroke:#000,color:#fff
    style G fill:#029E73,stroke:#000,color:#fff
    style H fill:#CC78BC,stroke:#000,color:#fff,stroke-width:3px,stroke-dasharray: 5 5
```

**Code**:

```go
package main

/*
#include <stdio.h>
#include <string.h>

int add(int a, int b) {
    return a + b;
}

int strlen_c(const char* s) {
    return strlen(s);
}
*/
import "C"                                      // => Pseudo-package for CGO
                                                // => Must import immediately after comment block
                                                // => Enables calling C code from Go

import (
    "fmt"
)

func main() {
    // Call C function
    result := C.add(10, 20)                     // => Call C add function defined above
                                                // => C.add is Go binding to C function
                                                // => Arguments automatically converted (Go int to C int)
                                                // => result is C.int type
    fmt.Println("C.add(10, 20) =", result)      // => Output: C.add(10, 20) = 30
                                                // => fmt handles C.int printing

    // String from Go to C
    goString := "Hello"                         // => goString is Go string type
                                                // => Go strings are not null-terminated
    cString := C.CString(goString)              // => Convert Go string to C string
                                                // => Allocates memory with malloc
                                                // => cString is *C.char (pointer to C string)
                                                // => Null-terminated C-style string
    defer C.free(cString)                       // => Must free C-allocated memory
                                                // => C.free wraps stdlib free()
                                                // => Deferred: executes when function exits
                                                // => CRITICAL: forgetting this causes memory leak

    length := C.strlen_c(cString)               // => Call C strlen_c function
                                                // => cString passed as const char*
                                                // => length is C.int type (value 5)
    fmt.Println("Length:", length)              // => Output: Length: 5
                                                // => Demonstrates string passing to C

    // Complex example - calculate from Go
    a := 15                                     // => a is Go int
    b := 25                                     // => b is Go int
    sum := int(C.add(C.int(a), C.int(b)))       // => Multiple type conversions
                                                // => C.int(a) converts Go int to C int
                                                // => C.int(b) converts Go int to C int
                                                // => C.add returns C.int
                                                // => int(...) converts C.int back to Go int
                                                // => sum is 40 (type: Go int)
    fmt.Println("Sum:", sum)                    // => Output: Sum: 40
                                                // => Demonstrates type conversion dance
}
```

**Key Takeaway**: CGO imports C code and calls C functions. Use `C.CString()` to convert Go strings to C strings. Remember to `free()` C-allocated memory. CGO is complex - use only when necessary.

**Why It Matters**: CGO enables leveraging optimized C libraries (SQLite embedded database, OpenSSL cryptography) and platform-specific APIs unavailable in pure Go, where performance-critical code or legacy system integration requires calling C. However, CGO disables cross-compilation, prevents stack traces across the C boundary, and introduces manual memory management (remember to free!), making it a last resort after evaluating pure-Go alternatives for portability and safety.

## Group 6: Modern Go and Best Practices

## Example 82: Workspaces (Go 1.18+)

Workspaces enable multi-module development. Develop multiple modules together without publishing intermediate versions. Useful for monorepos or coordinating multiple packages.

**Code**:

```
// go.work file
go 1.21

use (
    ./cmd/api              // => Include local module
    ./cmd/cli
    ./libs/common
)

// Directory structure:
// project/
// ├── go.work            // Workspace definition
// ├── cmd/
// │   ├── api/
// │   │   └── go.mod
// │   └── cli/
// │       └── go.mod
// └── libs/
//     └── common/
//         └── go.mod

// Benefits:
// - Develop multiple modules together
// - Changes in libs/common reflected immediately in cmd/api
// - No need to publish intermediate versions
// - All modules tested together

// Usage: go test ./... (tests all modules)
//        go build ./cmd/api (builds api using local libs)
```

**Key Takeaway**: Workspaces allow multi-module development without publishing. Define workspace with `go.work` file. Use `use()` to include local modules. All modules use local versions instead of published versions.

**Why It Matters**: Workspaces enable monorepo development where multiple interdependent modules evolve together without publishing intermediate versions, solving the problem of testing changes across libs/common and cmd/api before committing. Production teams use workspaces to develop coordinated updates across packages, test breaking changes before release, and maintain local overrides for dependencies, all while preserving independent module versioning for public releases.

## Example 83: Memory Profiling

Memory profiling identifies allocations and memory leaks. The `runtime/pprof` package enables capturing profiles. Analyze with `go tool pprof`.

**Code**:

```go
package main

import (
    "os"
    "runtime/pprof"
)

func main() {
    // Memory profiling
    f, err := os.Create("mem.prof")
    if err != nil {
        panic(err)
    }
    defer f.Close()

    pprof.WriteHeapProfile(f)  // => Capture heap profile

    // Run program: go run main.go
    // Then analyze: go tool pprof mem.prof
    // Commands in pprof:
    //   top         - shows top memory allocators
    //   list        - shows source code with allocations
    //   web         - generates graph (requires Graphviz)

    // CPU profiling
    cpuFile, _ := os.Create("cpu.prof")
    defer cpuFile.Close()

    pprof.StartCPUProfile(cpuFile) // => Start profiling
    defer pprof.StopCPUProfile()

    // Run expensive operation here
    expensiveComputation()
}

func expensiveComputation() {
    // Simulation of work
    for i := 0; i < 1000000; i++ {
        _ = i * i
    }
}
```

**Key Takeaway**: Use `pprof.WriteHeapProfile()` to capture memory allocations. Use `pprof.StartCPUProfile()` for CPU profiling. Analyze profiles with `go tool pprof`. Profile helps identify bottlenecks and memory leaks.

**Why It Matters**: Memory profiling identifies allocation hotspots and memory leaks that degrade production performance, where analyzing heap profiles reveals that JSON marshaling allocates 80% of memory or goroutines leak due to unclosed channels. Production debugging uses pprof to find why memory usage grows from 100MB to 10GB over days, pinpointing the specific code paths causing allocations, enabling targeted optimization that reduces memory by 10x without guessing.

## Example 84: Race Detector

Go's race detector identifies concurrent access to shared memory without synchronization. Run with `-race` flag during development and testing.

**Code**:

```go
package main

import (
    "fmt"
    "sync"
)

func main() {
    var counter int
    var wg sync.WaitGroup

    // Race condition - multiple goroutines modify counter without sync
    for i := 0; i < 10; i++ {
        wg.Add(1)
        go func() {
            defer wg.Done()
            counter++               // => DATA RACE! No synchronization
        }()
    }

    wg.Wait()
    fmt.Println("Counter:", counter) // => Unpredictable value

    // Run with: go run -race main.go
    // Output includes race condition detection

    // Fixed version with mutex
    var mu sync.Mutex
    counter = 0

    for i := 0; i < 10; i++ {
        wg.Add(1)
        go func() {
            defer wg.Done()
            mu.Lock()
            counter++              // => Protected by mutex
            mu.Unlock()
        }()
    }

    wg.Wait()
    fmt.Println("Counter (safe):", counter) // => Always 10
}
```

**Key Takeaway**: Run tests with `-race` flag to detect concurrent data access without synchronization. Race detector finds most (but not all) race conditions. Use mutexes, channels, or atomic operations to fix races.

**Why It Matters**: The race detector finds data races that cause non-deterministic bugs in production, where concurrent map access crashes intermittently or counter increments produce wrong values under load. While adding 5-10x overhead (use in development/CI, not production), the race detector catches subtle concurrency bugs that are impossible to find through code review, preventing production incidents where race conditions manifest only under specific timing conditions.

## Example 85: Go Best Practices Synthesis

Go development succeeds by following core principles: explicit error handling, simple concurrency with channels, composition with interfaces, and extensive testing. This final example summarizes key practices for production-ready code.

**Code**:

```go
package main

import (
    "context"
    "fmt"
    "net/http"
    "sync"
)

func main() {
    // Production patterns synthesis
    // 1. Error handling - explicit, not exceptions
    if err := processData(); err != nil {           // => Check error immediately
                                                    // => Never ignore errors
                                                    // => err is error interface or nil
        fmt.Printf("Error: %v\n", err)              // => Always check errors
                                                    // => Output: Error: <error message>
                                                    // => %v formats error with Error() method
    }

    // 2. Concurrency - channels and goroutines
    results := make(chan int, 10)                   // => Buffered channel (capacity 10)
                                                    // => results is chan int
                                                    // => Buffer prevents blocking on send
    go func() {                                     // => Spawn goroutine
                                                    // => Runs concurrently with main
        results <- 42                               // => Send value to channel
                                                    // => Does not block (buffer has space)
        close(results)                              // => Close channel (no more values)
                                                    // => Signals receiver
    }()
    fmt.Println(<-results)                          // => Receive from channel
                                                    // => Blocks until value available
                                                    // => Output: 42
                                                    // => Channel closed after receive

    // 3. Interfaces for composition
    var w http.ResponseWriter                       // => Depend on interfaces
                                                    // => w is nil (no concrete implementation)
                                                    // => Interface enables multiple implementations
    _ = w                                           // => Blank identifier (suppress unused warning)

    // 4. Context for cancellation
    ctx, cancel := context.WithCancel(context.Background())
                                                    // => ctx is cancellable context
                                                    // => cancel is function to cancel context
                                                    // => Background() returns empty root context
    defer cancel()                                  // => Ensure cancel called
                                                    // => Releases resources
                                                    // => Always defer cancel to prevent leaks

    var wg sync.WaitGroup                           // => WaitGroup to synchronize
                                                    // => Counter starts at 0
    wg.Add(1)                                       // => Increment counter
                                                    // => Counter is now 1
    go func() {                                     // => Spawn goroutine
                                                    // => Runs concurrently
        defer wg.Done()                             // => Decrement counter when done
                                                    // => Always use defer for Done
        <-ctx.Done()                                // => Respect cancellation
                                                    // => Blocks until context cancelled
                                                    // => ctx.Done() returns closed channel
    }()

    wg.Wait()                                       // => Block until counter reaches 0
                                                    // => Waits for goroutine to complete

    // 5. Testing - write table-driven tests
    testCases := []struct {                         // => Slice of anonymous structs
        input    int                                // => Test input
        expected int                                // => Expected output
    }{
        {1, 2},                                     // => Test case 1: input=1, expected=2
        {5, 10},                                    // => Test case 2: input=5, expected=10
    }

    for _, tc := range testCases {                  // => Iterate test cases
                                                    // => tc is each test case struct
        result := tc.input * 2                      // => Compute result
                                                    // => result is tc.input * 2
        if result != tc.expected {                  // => Compare with expected
                                                    // => Reports failure if mismatch
            fmt.Printf("Test failed: %d\n", tc.input)
                                                    // => Output: Test failed: <input>
        }
    }

    // 6. Code organization - flat structure, small packages
                                                    // => Keep packages focused and small
                                                    // => Avoid deep nesting
    // 7. Documentation - export with doc comments
                                                    // => Exported identifiers start with capital
                                                    // => Add godoc comments before exports
}

// processData - process data with error handling
func processData() error {                          // => Returns error (nil on success)
    // Production-ready pattern:
    // - Return errors explicitly
                                                    // => Never use panic for normal errors
    // - Use interfaces for dependencies
                                                    // => Enables testing with mocks
    // - Make functions testable
                                                    // => Pure functions, dependency injection
    return nil                                      // => nil indicates success
                                                    // => Return specific error for failures
}

// Production-ready server pattern
func serverPattern() {
    server := &http.Server{                         // => Create HTTP server
        Addr:    ":8080",                           // => Listen on port 8080
                                                    // => ":8080" means all interfaces
        Handler: http.HandlerFunc(handler),         // => Set request handler
                                                    // => handler is function converted to Handler
    }

    // Graceful shutdown
    go func() {                                     // => Run server in goroutine
                                                    // => Allows shutdown handling
        if err := server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
                                                    // => Start server (blocking call)
                                                    // => Returns error on failure
                                                    // => http.ErrServerClosed is expected on shutdown
            fmt.Printf("Server error: %v\n", err)   // => Log unexpected errors
                                                    // => Output: Server error: <error>
        }
    }()

    // Shutdown handling (signal, timeout, cleanup)
                                                    // => Listen for OS signals (SIGINT, SIGTERM)
                                                    // => Call server.Shutdown(ctx) with timeout
                                                    // => Perform cleanup (close DB, flush logs)
}

func handler(w http.ResponseWriter, r *http.Request) {
                                                    // => HTTP request handler
                                                    // => w writes response
                                                    // => r contains request details
    // Handler pattern:
    // - Check request validity
                                                    // => Validate headers, body, parameters
    // - Perform work
                                                    // => Business logic here
    // - Return appropriate status
                                                    // => Use w.WriteHeader() for status codes
    // - Log important events
                                                    // => Log errors, slow requests, security events
    fmt.Fprint(w, "OK")                             // => Write response body
                                                    // => Default status is 200 OK
}
```

**Key Takeaway**: Go best practices emphasize explicit error handling, simple concurrency with channels, composition through interfaces, extensive testing, and clear code organization. These patterns make Go code reliable, maintainable, and performant in production environments.

**Why It Matters**: Go's production success stems from its opinionated design: explicit error handling prevents silent failures (no exceptions crashing servers), simple concurrency with channels scales to 100k+ goroutines on commodity hardware (Kubernetes, Docker), and composition through interfaces enables testing and extensibility without inheritance complexity. Following these patterns produces systems like etcd (distributed database), Prometheus (monitoring), and Traefik (proxy) that handle massive scale with small teams and minimal runtime overhead.
