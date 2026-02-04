---
title: "Concurrency and Goroutines"
date: 2026-02-04T00:00:00+07:00
draft: false
description: "Goroutines, channels, select statements, and context for production concurrency"
weight: 10000024
tags: ["golang", "concurrency", "goroutines", "channels", "context", "production"]
---

## Why Concurrency Matters

Go's concurrency model based on goroutines and channels enables building high-performance systems that handle thousands of concurrent operations efficiently. Understanding goroutines, channels, select statements, and the context package prevents deadlocks, goroutine leaks, and enables graceful shutdown in production systems.

**Core benefits**:

- **Scalability**: Handle thousands of concurrent requests efficiently
- **Simplicity**: Lightweight goroutines simpler than threads
- **Communication**: Channels enable safe data sharing
- **Cancellation**: Context package provides timeout and deadline control

**Problem**: Incorrect concurrency causes goroutine leaks (memory exhaustion), deadlocks (hung systems), race conditions (data corruption), and difficulty canceling long-running operations.

**Solution**: Start with standard library goroutines and channels, understand limitations (no built-in cancellation), then apply context package for production-grade timeout and cancellation handling.

## Standard Library: Goroutines and Channels

Goroutines are lightweight threads managed by Go runtime. Channels provide communication between goroutines.

**Pattern: Basic Goroutine**:

```go
package main

import (
    "fmt"
    // => Standard library for output
    "time"
    // => Standard library for timing
)

func sayHello(name string) {
    // => Regular function, can run as goroutine
    // => name is parameter passed when launched

    fmt.Printf("Hello, %s!\n", name)
    // => Output: Hello, Alice!
    // => Executes in goroutine (concurrent)
}

func main() {
    // SYNCHRONOUS: function call blocks
    sayHello("Sync")
    // => Executes immediately in main goroutine
    // => main waits for completion
    // => Output: Hello, Sync!

    // ASYNCHRONOUS: goroutine runs concurrently
    go sayHello("Async")
    // => go keyword launches new goroutine
    // => sayHello runs concurrently with main
    // => main continues immediately (doesn't wait)

    time.Sleep(100 * time.Millisecond)
    // => Wait for goroutine to complete
    // => HACK: proper solution uses channels or sync.WaitGroup
    // => Without sleep, main exits before goroutine runs

    fmt.Println("Done")
    // => Output: Done
    // => main goroutine continues while sayHello runs
}
```

**Pattern: Channels for Communication**:

```go
package main

import "fmt"

func sum(values []int, result chan int) {
    // => result is channel for sending int values
    // => Channel enables communication with goroutine

    total := 0
    // => Accumulator for sum

    for _, v := range values {
        // => Iterate over values slice
        total += v
        // => Add each value to total
    }

    result <- total
    // => SEND total to channel
    // => <- operator sends to channel
    // => Blocks until receiver ready
    // => Communicates result back to main
}

func main() {
    values := []int{1, 2, 3, 4, 5}
    // => values to sum

    result := make(chan int)
    // => Create unbuffered channel for int
    // => make(chan int) allocates channel
    // => Unbuffered: send blocks until receive

    go sum(values, result)
    // => Launch sum in goroutine
    // => Runs concurrently with main
    // => result channel shared between goroutines

    total := <-result
    // => RECEIVE from channel
    // => Blocks until value available
    // => Synchronizes goroutines
    // => total is 15 (sum of 1+2+3+4+5)

    fmt.Println("Total:", total)
    // => Output: Total: 15
}
```

**Pattern: Buffered Channels**:

```go
package main

import "fmt"

func main() {
    // UNBUFFERED: send blocks until receive
    unbuffered := make(chan int)
    // => make(chan int) creates unbuffered channel
    // => Send blocks until receiver ready

    // BUFFERED: send blocks only when buffer full
    buffered := make(chan int, 3)
    // => make(chan int, 3) creates channel with buffer size 3
    // => Can send 3 values without blocking
    // => 4th send blocks until receive

    buffered <- 1
    // => Send 1 (doesn't block, buffer has space)
    buffered <- 2
    // => Send 2 (doesn't block, buffer has space)
    buffered <- 3
    // => Send 3 (doesn't block, buffer now full)

    // buffered <- 4
    // => DEADLOCK: would block (buffer full, no receiver)
    // => Uncommenting causes deadlock

    fmt.Println(<-buffered)
    // => Receive 1 (first value sent)
    // => Output: 1

    fmt.Println(<-buffered)
    // => Receive 2
    // => Output: 2

    buffered <- 4
    // => Send 4 (now space in buffer after receives)
    // => Doesn't block (buffer has room)

    fmt.Println(<-buffered)
    // => Receive 3
    // => Output: 3

    fmt.Println(<-buffered)
    // => Receive 4
    // => Output: 4
}
```

**Pattern: Select Statement**:

```go
package main

import (
    "fmt"
    // => Standard library for output
    "time"
    // => Standard library for timing
)

func main() {
    ch1 := make(chan string)
    ch2 := make(chan string)
    // => Two unbuffered channels

    go func() {
        // => Anonymous goroutine for ch1
        time.Sleep(100 * time.Millisecond)
        // => Simulate work (100ms delay)
        ch1 <- "from ch1"
        // => Send to ch1 after delay
    }()

    go func() {
        // => Anonymous goroutine for ch2
        time.Sleep(50 * time.Millisecond)
        // => Faster than ch1 (50ms delay)
        ch2 <- "from ch2"
        // => Send to ch2 first
    }()

    // SELECT: receive from first ready channel
    select {
    // => select blocks until one case ready
    // => Receives from first available channel
    // => Non-deterministic if multiple ready

    case msg1 := <-ch1:
        // => Receive from ch1
        // => msg1 is value from ch1
        // => Case selected if ch1 has data

        fmt.Println(msg1)
        // => This case won't execute (ch2 faster)

    case msg2 := <-ch2:
        // => Receive from ch2
        // => msg2 is value from ch2
        // => Case selected if ch2 has data

        fmt.Println(msg2)
        // => Output: from ch2
        // => ch2 ready first (50ms < 100ms)

    case <-time.After(200 * time.Millisecond):
        // => Timeout case (200ms)
        // => time.After returns channel that sends after duration
        // => Prevents indefinite blocking

        fmt.Println("timeout")
        // => Won't execute (ch2 ready before timeout)
    }
}
```

**Limitations for production**:

- No built-in cancellation (goroutines run until completion)
- No timeout control (must implement manually with time.After)
- No propagation of cancellation across goroutine hierarchies
- Manual error propagation (channels carry only one type)
- Goroutine leaks if not properly managed

## Production Framework: Context Package

The context package provides cancellation, deadlines, and request-scoped values for goroutine trees.

**Pattern: Context with Timeout**:

```go
package main

import (
    "context"
    // => Standard library for cancellation and deadlines
    // => Core package for production concurrency
    "fmt"
    "time"
)

func performWork(ctx context.Context, id int) {
    // => ctx carries cancellation signal
    // => id identifies this worker

    select {
    case <-time.After(2 * time.Second):
        // => Simulate long-running work (2 seconds)
        // => time.After sends after duration

        fmt.Printf("Worker %d: completed work\n", id)
        // => Work finished before timeout

    case <-ctx.Done():
        // => ctx.Done() channel closed on cancellation/timeout
        // => Signals work should stop
        // => Returns immediately when context cancelled

        fmt.Printf("Worker %d: cancelled - %v\n", id, ctx.Err())
        // => ctx.Err() returns cancellation reason
        // => Output: Worker 1: cancelled - context deadline exceeded
        // => Graceful shutdown when context times out
    }
}

func main() {
    // Create context with 1-second timeout
    ctx, cancel := context.WithTimeout(context.Background(), 1*time.Second)
    // => context.Background() creates root context
    // => WithTimeout wraps with timeout deadline
    // => cancel is function to manually cancel context
    // => CRITICAL: must call cancel to release resources

    defer cancel()
    // => ALWAYS defer cancel() to prevent resource leak
    // => Cancels context when main exits
    // => Releases goroutines waiting on ctx.Done()

    go performWork(ctx, 1)
    // => Launch worker with context
    // => Worker monitors ctx.Done() for cancellation

    time.Sleep(3 * time.Second)
    // => Wait longer than worker runtime
    // => Observe timeout behavior

    fmt.Println("Main exiting")
    // => main exits after 3 seconds
    // => Worker cancelled after 1 second (timeout)
}
```

**Pattern: Context Cancellation**:

```go
package main

import (
    "context"
    // => Standard library for cancellation
    "fmt"
    "time"
)

func worker(ctx context.Context, id int) {
    // => Worker respects context cancellation

    for {
        // => Infinite loop (exits on cancellation)

        select {
        case <-ctx.Done():
            // => Context cancelled
            // => ctx.Done() closed when cancel() called

            fmt.Printf("Worker %d: stopping\n", id)
            // => Graceful shutdown message
            // => Output: Worker 1: stopping

            return
            // => Exit goroutine (cleanup)

        default:
            // => No cancellation yet, continue working
            // => default case prevents blocking

            fmt.Printf("Worker %d: working...\n", id)
            // => Simulate work

            time.Sleep(500 * time.Millisecond)
            // => Pause between iterations
        }
    }
}

func main() {
    // Create cancellable context
    ctx, cancel := context.WithCancel(context.Background())
    // => context.Background() is root context
    // => WithCancel wraps with cancellation capability
    // => cancel is function to trigger cancellation

    go worker(ctx, 1)
    go worker(ctx, 2)
    go worker(ctx, 3)
    // => Launch 3 workers sharing same context
    // => All receive cancellation signal when cancel() called

    time.Sleep(2 * time.Second)
    // => Let workers run for 2 seconds
    // => Workers print "working..." messages

    fmt.Println("Cancelling workers...")
    cancel()
    // => CRITICAL: call cancel() to stop workers
    // => Closes ctx.Done() channel
    // => All workers receive signal simultaneously
    // => Enables graceful shutdown

    time.Sleep(1 * time.Second)
    // => Wait for workers to finish cleanup

    fmt.Println("Main exiting")
    // => Output: Main exiting
}
```

**Pattern: Context with Values**:

```go
package main

import (
    "context"
    // => Standard library for request-scoped values
    "fmt"
)

// Key type for context values (prevents collisions)
type contextKey string

const (
    userIDKey  contextKey = "userID"
    requestIDKey contextKey = "requestID"
)
// => Custom key types prevent conflicts
// => Don't use string directly (collisions possible)

func processRequest(ctx context.Context) {
    // => ctx carries request-scoped values

    // Retrieve values from context
    userID, ok := ctx.Value(userIDKey).(string)
    // => Type assertion to string
    // => ok is false if key missing or wrong type

    if !ok {
        // => Value not found or wrong type
        fmt.Println("User ID not found")
        return
    }

    requestID, ok := ctx.Value(requestIDKey).(string)
    if !ok {
        fmt.Println("Request ID not found")
        return
    }

    fmt.Printf("Processing request %s for user %s\n", requestID, userID)
    // => Output: Processing request req-123 for user user-456
    // => Values retrieved from context
}

func main() {
    // Create context with values
    ctx := context.Background()
    // => Root context (no values)

    ctx = context.WithValue(ctx, userIDKey, "user-456")
    // => Add userID to context
    // => Returns new context with value
    // => Original context unchanged (immutable)

    ctx = context.WithValue(ctx, requestIDKey, "req-123")
    // => Add requestID to context
    // => Chain WithValue calls

    processRequest(ctx)
    // => Pass context with values
    // => processRequest extracts values
}
```

**Why context package matters**:

- Cancellation propagates through goroutine hierarchies
- Deadlines prevent operations running indefinitely
- Request-scoped values (user ID, trace ID) without globals
- Standard pattern across Go ecosystem (HTTP, DB, gRPC)

## Trade-offs: When to Use Each

**Comparison table**:

| Approach           | Cancellation | Timeout  | Error Propagation | Use Case                            |
| ------------------ | ------------ | -------- | ----------------- | ----------------------------------- |
| **Channels only**  | Manual       | Manual   | Manual            | Simple pipelines                    |
| **sync.WaitGroup** | No           | No       | Manual            | Wait for completion only            |
| **Context**        | Built-in     | Built-in | Manual            | Production systems (HTTP, DB, gRPC) |

**When to use channels**:

- Communication between goroutines (producer-consumer)
- Pipelines (stage 1 → stage 2 → stage 3)
- Synchronization without shared memory
- When cancellation not needed

**When to use sync.WaitGroup**:

- Wait for multiple goroutines to complete
- No cancellation required
- Fire-and-forget parallelism
- Batch processing

**When to use context**:

- HTTP request handling (cancellation on client disconnect)
- Database queries (timeout after 30 seconds)
- gRPC calls (deadline propagation)
- Hierarchical cancellation (parent cancels children)
- Request-scoped values (trace IDs, user IDs)

**When to combine all**:

```go
func processItems(ctx context.Context, items []Item) error {
    // Context for cancellation/timeout
    results := make(chan Result, len(items))
    // Channel for communication
    var wg sync.WaitGroup
    // WaitGroup for completion tracking

    for _, item := range items {
        wg.Add(1)
        go func(item Item) {
            defer wg.Done()

            select {
            case <-ctx.Done():
                return  // Cancelled
            case results <- process(item):
                // Send result
            }
        }(item)
    }

    wg.Wait()  // Wait for all goroutines
    close(results)

    return ctx.Err()  // Return cancellation error if any
}
```

## Production Best Practices

**Always call cancel() to prevent leaks**:

```go
// GOOD: defer cancel immediately
ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
defer cancel()  // Releases resources

// BAD: forget to call cancel
ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
// LEAK: context resources never released
```

**Respect context cancellation**:

```go
// GOOD: check ctx.Done() in loops
func worker(ctx context.Context) {
    for {
        select {
        case <-ctx.Done():
            return  // Graceful shutdown
        default:
            // Do work
        }
    }
}

// BAD: ignore context (goroutine leak)
func worker(ctx context.Context) {
    for {
        // Work forever (never stops)
    }
}
```

**Pass context as first parameter**:

```go
// GOOD: ctx as first parameter (convention)
func doWork(ctx context.Context, data Data) error { }

// BAD: ctx not first or missing
func doWork(data Data, ctx context.Context) error { }
func doWork(data Data) error { }  // No cancellation
```

**Use buffered channels to prevent goroutine leaks**:

```go
// GOOD: buffered channel (goroutine can exit)
results := make(chan Result, 1)
go func() {
    results <- compute()  // Doesn't block if no receiver
}()

// BAD: unbuffered channel (goroutine blocks forever)
results := make(chan Result)
go func() {
    results <- compute()  // LEAK if no receiver
}()
```

**Timeout for external calls**:

```go
// HTTP request with timeout
ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
defer cancel()

req, _ := http.NewRequestWithContext(ctx, "GET", url, nil)
resp, err := client.Do(req)  // Cancelled after 10s
```

## Summary

Go's concurrency model uses lightweight goroutines and channels for communication. Standard library provides goroutines, channels, and select for basic concurrency. Production systems use context package for cancellation, timeouts, and request-scoped values, preventing goroutine leaks and enabling graceful shutdown.

**Key takeaways**:

- Goroutines are lightweight (thousands feasible)
- Channels communicate between goroutines safely
- Select multiplexes channel operations with timeout
- Context provides cancellation, deadlines, request-scoped values
- Always defer cancel() to prevent resource leaks
- Pass context as first parameter (convention)
