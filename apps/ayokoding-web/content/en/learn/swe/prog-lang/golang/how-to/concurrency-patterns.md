---
title: Concurrency Patterns in Go
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 628
description: Master goroutines, channels, and concurrency patterns for production Go applications
tags: ["golang", "go", "concurrency", "goroutines", "channels"]
---

## Worker Pool Pattern

Distribute work across multiple goroutines for parallel processing.

```go
package main

import (
    "fmt"
    "sync"
    "time"
)

func worker(id int, jobs <-chan int, results chan<- int, wg *sync.WaitGroup) {
    defer wg.Done()
    for job := range jobs {
        fmt.Printf("Worker %d processing job %d\n", id, job)
        time.Sleep(time.Second) // Simulate work
        results <- job * 2
    }
}

func main() {
    const numJobs = 10
    const numWorkers = 3

    jobs := make(chan int, numJobs)
    results := make(chan int, numJobs)
    var wg sync.WaitGroup

    // Start workers
    for w := 1; w <= numWorkers; w++ {
        wg.Add(1)
        go worker(w, jobs, results, &wg)
    }

    // Send jobs
    for j := 1; j <= numJobs; j++ {
        jobs <- j
    }
    close(jobs)

    // Wait and close results
    go func() {
        wg.Wait()
        close(results)
    }()

    // Collect results
    for result := range results {
        fmt.Println("Result:", result)
    }
}
```

## Fan-Out, Fan-In Pattern

Distribute work to multiple goroutines (fan-out) and merge results (fan-in).

```go
package main

import (
    "fmt"
    "sync"
)

// Fan-out: distribute input to multiple workers
func generate(nums ...int) <-chan int {
    out := make(chan int)
    go func() {
        defer close(out)
        for _, n := range nums {
            out <- n
        }
    }()
    return out
}

// Worker function
func square(in <-chan int) <-chan int {
    out := make(chan int)
    go func() {
        defer close(out)
        for n := range in {
            out <- n * n
        }
    }()
    return out
}

// Fan-in: merge multiple channels into one
func merge(channels ...<-chan int) <-chan int {
    out := make(chan int)
    var wg sync.WaitGroup

    wg.Add(len(channels))
    for _, ch := range channels {
        go func(c <-chan int) {
            defer wg.Done()
            for n := range c {
                out <- n
            }
        }(ch)
    }

    go func() {
        wg.Wait()
        close(out)
    }()

    return out
}

func main() {
    // Fan-out
    in := generate(1, 2, 3, 4, 5)

    // Multiple workers
    c1 := square(in)
    c2 := square(in)
    c3 := square(in)

    // Fan-in
    for result := range merge(c1, c2, c3) {
        fmt.Println(result)
    }
}
```

## Pipeline Pattern

Chain operations where output of one stage becomes input of next.

```go
package main

import "fmt"

// Stage 1: Generate numbers
func generate(nums ...int) <-chan int {
    out := make(chan int)
    go func() {
        defer close(out)
        for _, n := range nums {
            out <- n
        }
    }()
    return out
}

// Stage 2: Square numbers
func square(in <-chan int) <-chan int {
    out := make(chan int)
    go func() {
        defer close(out)
        for n := range in {
            out <- n * n
        }
    }()
    return out
}

// Stage 3: Filter even numbers
func filterEven(in <-chan int) <-chan int {
    out := make(chan int)
    go func() {
        defer close(out)
        for n := range in {
            if n%2 == 0 {
                out <- n
            }
        }
    }()
    return out
}

func main() {
    // Build pipeline
    nums := generate(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    squared := square(nums)
    evens := filterEven(squared)

    // Consume results
    for result := range evens {
        fmt.Println(result)
    }
}
```

## Context for Cancellation

Use `context.Context` for graceful cancellation and timeout control.

```go
package main

import (
    "context"
    "fmt"
    "time"
)

func worker(ctx context.Context, id int) {
    for {
        select {
        case <-ctx.Done():
            fmt.Printf("Worker %d: cancelled (%v)\n", id, ctx.Err())
            return
        default:
            fmt.Printf("Worker %d: working...\n", id)
            time.Sleep(500 * time.Millisecond)
        }
    }
}

func main() {
    // Timeout after 2 seconds
    ctx, cancel := context.WithTimeout(context.Background(), 2*time.Second)
    defer cancel()

    for i := 1; i <= 3; i++ {
        go worker(ctx, i)
    }

    // Wait for timeout
    <-ctx.Done()
    fmt.Println("Main: shutting down")
    time.Sleep(100 * time.Millisecond) // Let workers print
}
```

## Rate Limiting with Ticker

Control goroutine execution rate using `time.Ticker`.

```go
package main

import (
    "fmt"
    "time"
)

func main() {
    // Process 1 request per second
    requests := make(chan int, 5)
    for i := 1; i <= 5; i++ {
        requests <- i
    }
    close(requests)

    limiter := time.NewTicker(time.Second)
    defer limiter.Stop()

    for req := range requests {
        <-limiter.C // Wait for tick
        fmt.Printf("Processing request %d at %v\n", req, time.Now())
    }
}
```

## Bounded Parallelism with Semaphore

Limit concurrent goroutines using semaphore pattern.

```go
package main

import (
    "fmt"
    "sync"
    "time"
)

func main() {
    const maxConcurrent = 3
    semaphore := make(chan struct{}, maxConcurrent)
    var wg sync.WaitGroup

    tasks := 10
    for i := 1; i <= tasks; i++ {
        wg.Add(1)
        go func(id int) {
            defer wg.Done()

            // Acquire semaphore
            semaphore <- struct{}{}
            defer func() { <-semaphore }() // Release

            fmt.Printf("Task %d started\n", id)
            time.Sleep(time.Second)
            fmt.Printf("Task %d completed\n", id)
        }(i)
    }

    wg.Wait()
}
```

## errgroup for Error Handling

Use `golang.org/x/sync/errgroup` for coordinated error handling.

```go
package main

import (
    "context"
    "fmt"
    "golang.org/x/sync/errgroup"
    "net/http"
)

func fetchURL(url string) error {
    resp, err := http.Get(url)
    if err != nil {
        return err
    }
    defer resp.Body.Close()
    fmt.Printf("Fetched %s: %d\n", url, resp.StatusCode)
    return nil
}

func main() {
    g, ctx := errgroup.WithContext(context.Background())

    urls := []string{
        "https://golang.org",
        "https://go.dev",
        "https://pkg.go.dev",
    }

    for _, url := range urls {
        url := url // Capture loop variable
        g.Go(func() error {
            select {
            case <-ctx.Done():
                return ctx.Err()
            default:
                return fetchURL(url)
            }
        })
    }

    if err := g.Wait(); err != nil {
        fmt.Printf("Error: %v\n", err)
    }
}
```

## Mutex vs RWMutex

Choose appropriate synchronization primitive for your use case.

```go
package main

import (
    "fmt"
    "sync"
    "time"
)

// Mutex - exclusive access
type Counter struct {
    mu    sync.Mutex
    value int
}

func (c *Counter) Increment() {
    c.mu.Lock()
    defer c.mu.Unlock()
    c.value++
}

func (c *Counter) Value() int {
    c.mu.Lock()
    defer c.mu.Unlock()
    return c.value
}

// RWMutex - multiple readers, single writer
type Cache struct {
    mu   sync.RWMutex
    data map[string]string
}

func (c *Cache) Get(key string) (string, bool) {
    c.mu.RLock() // Read lock
    defer c.mu.RUnlock()
    val, ok := c.data[key]
    return val, ok
}

func (c *Cache) Set(key, value string) {
    c.mu.Lock() // Write lock
    defer c.mu.Unlock()
    c.data[key] = value
}

func main() {
    // Counter example
    counter := &Counter{}
    var wg sync.WaitGroup

    for i := 0; i < 100; i++ {
        wg.Add(1)
        go func() {
            defer wg.Done()
            counter.Increment()
        }()
    }

    wg.Wait()
    fmt.Println("Counter:", counter.Value())

    // Cache example
    cache := &Cache{data: make(map[string]string)}
    cache.Set("key1", "value1")

    for i := 0; i < 10; i++ {
        wg.Add(1)
        go func(id int) {
            defer wg.Done()
            val, _ := cache.Get("key1")
            fmt.Printf("Reader %d: %s\n", id, val)
        }(i)
    }

    wg.Wait()
}
```

## Select with Default (Non-blocking)

Use `default` case in `select` for non-blocking operations.

```go
package main

import (
    "fmt"
    "time"
)

func main() {
    messages := make(chan string, 1)
    signals := make(chan bool)

    // Non-blocking send
    select {
    case messages <- "hello":
        fmt.Println("Sent message")
    default:
        fmt.Println("No message sent")
    }

    // Non-blocking receive
    select {
    case msg := <-messages:
        fmt.Println("Received:", msg)
    default:
        fmt.Println("No message received")
    }

    // Timeout pattern
    select {
    case <-signals:
        fmt.Println("Signal received")
    case <-time.After(1 * time.Second):
        fmt.Println("Timeout")
    }
}
```

## Related Resources

**Learn more**:

- [Glossary](/en/learn/swe/prog-lang/golang/reference/glossary) - Channel, Goroutine, Mutex
- [Cheat Sheet](/en/learn/swe/prog-lang/golang/reference/cheat-sheet) - Concurrency syntax
- [Overview](/en/learn/swe/prog-lang/golang/explanation/overview) - Go concurrency philosophy
