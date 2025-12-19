---
title: "How to Use Goroutines and Channels"
date: 2025-12-17T10:00:00+07:00
draft: false
weight: 608
description: "Practical patterns for concurrent programming with goroutines, channels, and synchronization primitives"
tags: ["golang", "concurrency", "goroutines", "channels", "sync"]
---

## Problem

Goroutines and channels are Go's concurrency primitives, but incorrect usage leads to deadlocks, goroutine leaks, and race conditions.

```go
// ❌ Goroutine leak - never exits
ch := make(chan int)
go func() {
  for i := 0; ; i++ {
    ch <- i // Blocks forever if no receiver
  }
}()
```

This guide shows patterns for using goroutines and channels safely and effectively.

## Solution Strategies

### When to Use Goroutines

Spawn goroutines for truly concurrent work, not just to make code asynchronous.

**When to use**: I/O-bound operations that can run independently, CPU-bound tasks to utilize multiple cores, background workers.

```go
// ❌ Unnecessary goroutine - adds complexity without benefit
func GetUser(id string) (*User, error) {
  result := make(chan *User)

  go func() {
    user, _ := db.Query(id)
    result <- user
  }()

  return <-result, nil
}

// ✅ Simple synchronous call
func GetUser(id string) (*User, error) {
  return db.Query(id)
}

// ✅ Goroutines for parallel work
func FetchMultipleUsers(ids []string) ([]*User, error) {
  type result struct {
    user *User
    err  error
  }

  results := make(chan result, len(ids))

  for _, id := range ids {
    go func(userID string) {
      user, err := db.Query(userID)
      results <- result{user, err}
    }(id)
  }

  users := make([]*User, 0, len(ids))
  for i := 0; i < len(ids); i++ {
    r := <-results
    if r.err != nil {
      return nil, r.err
    }
    users = append(users, r.user)
  }

  return users, nil
}
```

**Parallel processing with worker pool:**

```go
// ✅ Worker pool for CPU-intensive tasks
func ProcessItems(items []Item, numWorkers int) []Result {
  jobs := make(chan Item, len(items))
  results := make(chan Result, len(items))

  // Start workers
  for w := 0; w < numWorkers; w++ {
    go worker(jobs, results)
  }

  // Send jobs
  for _, item := range items {
    jobs <- item
  }
  close(jobs)

  // Collect results
  output := make([]Result, 0, len(items))
  for i := 0; i < len(items); i++ {
    output = append(output, <-results)
  }

  return output
}

func worker(jobs <-chan Item, results chan<- Result) {
  for item := range jobs {
    results <- process(item)
  }
}
```

### Buffered vs Unbuffered Channels

Choose channel buffer size based on coordination needs.

**When to use unbuffered**: Synchronization, ensuring receiver is ready, request-response patterns.

**When to use buffered**: Decoupling sender and receiver, preventing blocking, known capacity.

```go
// ✅ Unbuffered for synchronization
func Ping() {
  pong := make(chan string) // Unbuffered

  go func() {
    time.Sleep(1 * time.Second)
    pong <- "pong" // Blocks until main goroutine receives
  }()

  msg := <-pong // Blocks until goroutine sends
  fmt.Println(msg) // Guaranteed to print after 1 second
}

// ✅ Buffered to prevent blocking
func Generate(n int) <-chan int {
  out := make(chan int, n) // Buffer = n

  go func() {
    defer close(out)
    for i := 0; i < n; i++ {
      out <- i // Never blocks - buffer sized for all values
    }
  }()

  return out
}

// ✅ Buffered for result collection
func FetchURLs(urls []string) []Response {
  results := make(chan Response, len(urls)) // Buffer = num goroutines

  for _, url := range urls {
    go func(u string) {
      resp := fetch(u)
      results <- resp // Never blocks - buffer sized correctly
    }(url)
  }

  responses := make([]Response, len(urls))
  for i := 0; i < len(urls); i++ {
    responses[i] = <-results
  }

  return responses
}
```

**Buffer size guidelines:**

```go
// Unbuffered (0): Synchronization
ch := make(chan int)

// Buffer = 1: Latest value without blocking
ch := make(chan int, 1)

// Buffer = N: Known number of sends
ch := make(chan Result, len(items))

// Larger buffer: Smoothing burstiness
ch := make(chan Task, 100)
```

### Use Context for Cancellation

Pass context to goroutines for cancellation and timeout control.

```go
// ❌ No way to stop goroutine
func Generate() <-chan int {
  out := make(chan int)
  go func() {
    for i := 0; ; i++ {
      out <- i // Runs forever, even if receiver stops
    }
  }()
  return out
}

// ✅ Context enables cancellation
func Generate(ctx context.Context) <-chan int {
  out := make(chan int)

  go func() {
    defer close(out)
    for i := 0; ; i++ {
      select {
      case out <- i:
      case <-ctx.Done():
        return // Clean exit on cancellation
      }
    }
  }()

  return out
}

// Usage with cancellation
func main() {
  ctx, cancel := context.WithCancel(context.Background())
  defer cancel()

  numbers := Generate(ctx)

  for i := 0; i < 5; i++ {
    fmt.Println(<-numbers)
  }

  cancel() // Goroutine stops cleanly
}
```

**Timeout pattern:**

```go
// ✅ HTTP request with timeout
func FetchWithTimeout(url string, timeout time.Duration) (*Response, error) {
  ctx, cancel := context.WithTimeout(context.Background(), timeout)
  defer cancel()

  req, err := http.NewRequestWithContext(ctx, "GET", url, nil)
  if err != nil {
    return nil, err
  }

  resp, err := http.DefaultClient.Do(req)
  if err != nil {
    if ctx.Err() == context.DeadlineExceeded {
      return nil, fmt.Errorf("request timed out after %v", timeout)
    }
    return nil, err
  }

  return resp, nil
}
```

**Propagating cancellation:**

```go
// ✅ Pass context to child operations
func ProcessData(ctx context.Context, data []byte) error {
  // Check if already cancelled
  if ctx.Err() != nil {
    return ctx.Err()
  }

  // Pass context to database operation
  if err := saveToDatabase(ctx, data); err != nil {
    return err
  }

  // Pass context to HTTP request
  if err := notifyService(ctx, data); err != nil {
    return err
  }

  return nil
}
```

### Select Statement Patterns

Use select to coordinate multiple channel operations.

```go
// ✅ Timeout pattern
select {
case result := <-ch:
  fmt.Println("Received:", result)
case <-time.After(5 * time.Second):
  fmt.Println("Timeout")
}

// ✅ Non-blocking receive
select {
case msg := <-messages:
  fmt.Println("Received:", msg)
default:
  fmt.Println("No message ready")
}

// ✅ Non-blocking send
select {
case ch <- value:
  fmt.Println("Sent value")
default:
  fmt.Println("Channel full, could not send")
}

// ✅ Multiple channels
func Merge(ch1, ch2 <-chan int) <-chan int {
  out := make(chan int)

  go func() {
    defer close(out)
    for ch1 != nil || ch2 != nil {
      select {
      case v, ok := <-ch1:
        if !ok {
          ch1 = nil // Channel closed, ignore in future selects
        } else {
          out <- v
        }
      case v, ok := <-ch2:
        if !ok {
          ch2 = nil
        } else {
          out <- v
        }
      }
    }
  }()

  return out
}

// ✅ Context cancellation with select
func Worker(ctx context.Context, jobs <-chan Job) {
  for {
    select {
    case job := <-jobs:
      process(job)
    case <-ctx.Done():
      fmt.Println("Worker cancelled")
      return
    }
  }
}
```

### WaitGroup for Coordination

Use sync.WaitGroup to wait for multiple goroutines to complete.

```go
// ✅ Wait for all goroutines
func ProcessAll(items []Item) {
  var wg sync.WaitGroup

  for _, item := range items {
    wg.Add(1)
    go func(i Item) {
      defer wg.Done()
      process(i)
    }(item)
  }

  wg.Wait() // Blocks until all goroutines call Done()
  fmt.Println("All items processed")
}

// ❌ Common mistake - Add inside goroutine
func BadPattern(items []Item) {
  var wg sync.WaitGroup

  for _, item := range items {
    go func(i Item) {
      wg.Add(1) // Race condition! Might call Wait() before Add()
      defer wg.Done()
      process(i)
    }(item)
  }

  wg.Wait()
}

// ✅ Correct - Add before spawning goroutine
func GoodPattern(items []Item) {
  var wg sync.WaitGroup

  for _, item := range items {
    wg.Add(1) // Add BEFORE go keyword
    go func(i Item) {
      defer wg.Done()
      process(i)
    }(item)
  }

  wg.Wait()
}
```

**Bounded parallelism with WaitGroup:**

```go
// ✅ Limit concurrent goroutines
func ProcessWithLimit(items []Item, maxConcurrent int) {
  var wg sync.WaitGroup
  semaphore := make(chan struct{}, maxConcurrent)

  for _, item := range items {
    wg.Add(1)

    go func(i Item) {
      defer wg.Done()

      semaphore <- struct{}{} // Acquire
      defer func() { <-semaphore }() // Release

      process(i)
    }(item)
  }

  wg.Wait()
}
```

### Mutex for Shared State

Use sync.Mutex to protect shared mutable state.

```go
// ❌ Race condition - no synchronization
type Counter struct {
  count int
}

func (c *Counter) Increment() {
  c.count++ // Race condition!
}

// ✅ Mutex protects shared state
type Counter struct {
  mu    sync.Mutex
  count int
}

func (c *Counter) Increment() {
  c.mu.Lock()
  defer c.mu.Unlock()
  c.count++
}

func (c *Counter) Value() int {
  c.mu.Lock()
  defer c.mu.Unlock()
  return c.count
}
```

**RWMutex for read-heavy workloads:**

```go
// ✅ RWMutex allows multiple concurrent readers
type Cache struct {
  mu    sync.RWMutex
  items map[string]string
}

func (c *Cache) Get(key string) (string, bool) {
  c.mu.RLock() // Multiple goroutines can hold RLock
  defer c.mu.RUnlock()
  val, ok := c.items[key]
  return val, ok
}

func (c *Cache) Set(key, value string) {
  c.mu.Lock() // Exclusive lock for writes
  defer c.mu.Unlock()
  if c.items == nil {
    c.items = make(map[string]string)
  }
  c.items[key] = value
}
```

### Channel Direction

Use channel direction to document and enforce sending/receiving.

```go
// ✅ Direction makes intent clear
func Generate(nums ...int) <-chan int { // Send-only for caller
  out := make(chan int)
  go func() {
    defer close(out)
    for _, n := range nums {
      out <- n
    }
  }()
  return out
}

func Square(in <-chan int) <-chan int { // Receive from in, send-only for caller
  out := make(chan int)
  go func() {
    defer close(out)
    for n := range in {
      out <- n * n
    }
  }()
  return out
}

func Print(in <-chan int) { // Receive-only
  for n := range in {
    fmt.Println(n)
  }
}

// Pipeline
func main() {
  nums := Generate(1, 2, 3, 4)
  squares := Square(nums)
  Print(squares)
}
```

### Preventing Goroutine Leaks

Ensure all goroutines can exit cleanly.

```go
// ❌ Leak - goroutine blocks forever
func Search(query string) Result {
  result := make(chan Result) // Unbuffered

  go func() {
    result <- search(query) // Blocks if main doesn't receive
  }()

  timeout := time.After(5 * time.Second)
  select {
  case r := <-result:
    return r
  case <-timeout:
    return Result{} // Goroutine still blocked sending!
  }
}

// ✅ Buffered channel prevents blocking
func Search(query string) Result {
  result := make(chan Result, 1) // Buffer = 1

  go func() {
    result <- search(query) // Never blocks
  }()

  timeout := time.After(5 * time.Second)
  select {
  case r := <-result:
    return r
  case <-timeout:
    return Result{} // Goroutine completes and exits
  }
}

// ✅ Context for cancellation
func Search(ctx context.Context, query string) (Result, error) {
  result := make(chan Result)

  go func() {
    select {
    case result <- search(query):
    case <-ctx.Done():
      // Exit if context cancelled
    }
  }()

  select {
  case r := <-result:
    return r, nil
  case <-ctx.Done():
    return Result{}, ctx.Err()
  }
}
```

## Putting It All Together

When you're ready to write concurrent code, start by asking whether you need concurrency at all. Don't spawn goroutines just to make code asynchronous - use them when you have truly independent work that can run in parallel. I/O-bound operations like HTTP requests or database queries benefit from concurrency, as do CPU-intensive tasks you want to spread across cores.

Once you decide to use goroutines, give them a way to exit. Pass context for cancellation, use done channels, or ensure channels they're sending to have receivers. Every goroutine you spawn should have a clear exit condition visible in its code. This prevents leaks where goroutines accumulate over time.

Choose channel buffer sizes deliberately. Unbuffered channels (buffer size 0) synchronize sender and receiver - both must be ready for communication to happen. Buffered channels decouple them up to the buffer capacity. For collecting results from N goroutines, use a buffer of size N so senders never block. For pipeline stages, consider whether you want backpressure (unbuffered) or smoothing (buffered).

Use select statements to coordinate multiple channels and handle timeouts. When working with multiple channels, check if they're closed and set them to nil to prevent selecting them again. This pattern works well for merging channels or handling multiple event sources.

Protect shared mutable state with mutexes. If multiple goroutines access the same map, counter, or other mutable data, use sync.Mutex to serialize access. For read-heavy workloads, sync.RWMutex allows multiple concurrent readers. But prefer communicating via channels over sharing memory when possible - it makes concurrency bugs less likely.

## Common Mistakes to Avoid

**Don't start goroutines in a loop without capturing variables:**

```go
// ❌ All goroutines see last value
for _, item := range items {
  go func() {
    process(item) // Race! item changes every iteration
  }()
}

// ✅ Pass as parameter
for _, item := range items {
  go func(i Item) {
    process(i) // Each goroutine gets its own copy
  }(item)
}
```

**Don't close channels from receivers:**

```go
// ❌ Sender panics when sending to closed channel
go func() {
  for i := 0; i < 10; i++ {
    ch <- i // Panic if receiver closed channel!
  }
}()

close(ch) // Don't close from receiver side!

// ✅ Sender closes channel
go func() {
  defer close(ch) // Sender owns and closes
  for i := 0; i < 10; i++ {
    ch <- i
  }
}()
```

**Don't forget to close channels in generators:**

```go
// ❌ Range blocks forever - channel never closed
func Generate() <-chan int {
  ch := make(chan int)
  go func() {
    for i := 0; i < 10; i++ {
      ch <- i
    }
    // Forgot to close!
  }()
  return ch
}

for num := range Generate() {
  fmt.Println(num) // Hangs after receiving 10 numbers
}

// ✅ Close when done sending
func Generate() <-chan int {
  ch := make(chan int)
  go func() {
    defer close(ch) // Always close
    for i := 0; i < 10; i++ {
      ch <- i
    }
  }()
  return ch
}
```

## Summary

Effective concurrent programming in Go means using goroutines and channels deliberately rather than everywhere. Spawn goroutines when you have truly independent work that benefits from parallelism - I/O operations that would otherwise block sequentially or CPU-intensive tasks you want to spread across cores. For simple sequential operations, synchronous code is clearer and safer.

Always give goroutines a way to exit. Pass contexts for cancellation, use done channels, or ensure the channels they're sending to have receivers. Goroutine leaks accumulate slowly and cause mysterious resource exhaustion in production. Every goroutine should have a visible exit path in its code.

Choose channel buffer sizes based on coordination needs. Unbuffered channels synchronize sender and receiver, making both wait for each other. Buffered channels decouple them up to capacity - use this when you know how many values will be sent or want to prevent goroutines from blocking. Buffer size communicates intent about coordination.

Use select to coordinate multiple channels, handle timeouts, and implement non-blocking operations. Select makes it easy to wait for the first of several events or to bail out if an operation takes too long. Combined with context cancellation, select enables sophisticated coordination patterns with clean cancellation paths.

Protect shared mutable state with mutexes when you must share memory between goroutines. Prefer sync.RWMutex for read-heavy workloads where multiple readers can proceed concurrently. But when possible, prefer communicating via channels over sharing memory - it makes concurrency bugs less likely by making data flow explicit.

These patterns work together to create concurrent code that's correct, maintainable, and efficient. Understanding when to use each primitive and how they compose leads to programs that take full advantage of Go's concurrency model while avoiding its pitfalls.

## Related Content

- [Go Best Practices and Idioms](/en/learn/swe/prog-lang/golang/explanation/best-practices)
- [Common Go Anti-Patterns](/en/learn/swe/prog-lang/golang/explanation/anti-patterns)
- [How to Handle Errors Effectively](/en/learn/swe/prog-lang/golang/how-to/handle-errors-effectively)
- [How to Avoid Nil Panics](/en/learn/swe/prog-lang/golang/how-to/avoid-nil-panics)
