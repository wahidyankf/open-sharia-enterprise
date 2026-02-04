---
title: "Resilience Patterns"
date: 2026-02-04T00:00:00+07:00
draft: false
description: "Resilience patterns in Go: timeouts, retries, circuit breakers, and bulkheads for fault tolerance"
weight: 10000072
tags: ["golang", "resilience", "circuit-breaker", "retry", "timeout", "fault-tolerance", "production"]
---

## Why Resilience Patterns Matter

Resilience patterns prevent cascading failures in distributed systems by gracefully handling errors, timeouts, and service degradation. Without resilience, a slow or failing service brings down dependent services causing complete system outages. Understanding timeouts, retries, circuit breakers, and bulkheads prevents cascading failures, improves system stability, and ensures graceful degradation under load.

**Core benefits**:

- **Fault isolation**: Failures don't cascade to dependent services
- **Graceful degradation**: System remains partially functional
- **Fast failure detection**: Timeouts prevent hanging requests
- **Automatic recovery**: Retry logic handles transient errors

**Problem**: Standard library provides context.WithTimeout for basic timeouts but no circuit breaker, retry logic, or bulkhead patterns. Manual implementation is complex and error-prone.

**Solution**: Start with context.WithTimeout to understand timeout fundamentals, identify limitations (no retry, no circuit breaking), then use production libraries (gobreaker for circuit breakers, exponential backoff for retries) for comprehensive resilience.

## Standard Library: Timeouts with Context

Go's context package provides timeout and cancellation support.

**Pattern from standard library**:

```go
package main

import (
    "context"
    // => Standard library for timeout and cancellation
    // => context.WithTimeout creates timed context
    "fmt"
    "net/http"
    // => Standard library HTTP client
    "time"
    // => Standard library for time operations
)

func fetchWithTimeout(url string, timeout time.Duration) (string, error) {
    // => Makes HTTP request with timeout
    // => Returns response body or timeout error

    ctx, cancel := context.WithTimeout(context.Background(), timeout)
    // => ctx is context that expires after timeout
    // => cancel is function to cancel context early
    // => CRITICAL: always call cancel() to release resources

    defer cancel()
    // => Ensures cancel() called when function returns
    // => Releases context resources (goroutines, timers)
    // => Called even if function returns early

    req, err := http.NewRequestWithContext(ctx, "GET", url, nil)
    // => NewRequestWithContext creates request with context
    // => Request automatically cancelled when context expires
    // => ctx controls request lifecycle

    if err != nil {
        return "", err
    }

    client := &http.Client{}
    // => Creates HTTP client
    // => Production: reuse client (connection pooling)

    resp, err := client.Do(req)
    // => Executes request with timeout
    // => Returns error if timeout exceeded
    // => err is context.DeadlineExceeded on timeout

    if err != nil {
        // => Request failed or timed out

        if err == context.DeadlineExceeded {
            // => Timeout occurred
            // => err is context.DeadlineExceeded

            return "", fmt.Errorf("request timed out after %v", timeout)
        }

        return "", err
        // => Other error (network, DNS, etc.)
    }

    defer resp.Body.Close()
    // => Close response body to prevent resource leak

    body := make([]byte, 1024)
    // => Buffer for response body
    // => Production: use io.ReadAll or bufio

    n, _ := resp.Body.Read(body)
    // => Read response body
    // => n is bytes read

    return string(body[:n]), nil
    // => Return response body as string
}

func main() {
    // Fast response (within timeout)
    result, err := fetchWithTimeout("https://httpbin.org/delay/1", 3*time.Second)
    // => URL delays 1 second, timeout is 3 seconds
    // => Request completes successfully

    if err != nil {
        fmt.Println("Error:", err)
    } else {
        fmt.Println("Success:", len(result), "bytes")
        // => Output: Success: 329 bytes
    }

    // Slow response (exceeds timeout)
    result, err = fetchWithTimeout("https://httpbin.org/delay/5", 2*time.Second)
    // => URL delays 5 seconds, timeout is 2 seconds
    // => Request times out

    if err != nil {
        fmt.Println("Error:", err)
        // => Output: Error: request timed out after 2s
    }
}
```

**Pattern: Database Query Timeout**:

```go
package main

import (
    "context"
    "database/sql"
    "fmt"
    "time"
)

func queryUserWithTimeout(db *sql.DB, id int, timeout time.Duration) (string, error) {
    // => Queries database with timeout
    // => Prevents hanging queries

    ctx, cancel := context.WithTimeout(context.Background(), timeout)
    defer cancel()
    // => Context expires after timeout
    // => Query cancelled automatically

    var username string

    err := db.QueryRowContext(ctx, "SELECT username FROM users WHERE id = $1", id).Scan(&username)
    // => QueryRowContext uses context for timeout
    // => Query cancelled if context expires
    // => err is context.DeadlineExceeded on timeout

    if err != nil {
        if err == context.DeadlineExceeded {
            return "", fmt.Errorf("query timed out after %v", timeout)
        }
        return "", err
    }

    return username, nil
}
```

**Limitations for production resilience**:

- No retry logic (single attempt only)
- No exponential backoff (retries immediately or not at all)
- No circuit breaker (doesn't stop hammering failing service)
- No bulkhead (no resource isolation between services)
- Manual timeout handling (verbose context plumbing)

## Production Pattern: Exponential Backoff with Jitter

Exponential backoff with jitter prevents retry storms by spacing out retries with randomness.

**Pattern from standard library**:

```go
package main

import (
    "fmt"
    "math"
    "math/rand"
    "time"
)

func retryWithBackoff(maxRetries int, operation func() error) error {
    // => Retries operation with exponential backoff
    // => maxRetries is maximum retry attempts
    // => operation is function to retry

    var err error

    for attempt := 0; attempt < maxRetries; attempt++ {
        // => Retry loop (0 to maxRetries-1)

        err = operation()
        // => Execute operation
        // => err is nil on success

        if err == nil {
            // => Operation succeeded

            return nil
            // => Exit retry loop
        }

        // Operation failed, calculate backoff
        if attempt < maxRetries-1 {
            // => Not last attempt (still have retries left)

            backoff := calculateBackoff(attempt)
            // => Calculate wait time before retry
            // => Exponential: 1s, 2s, 4s, 8s, 16s...

            fmt.Printf("Attempt %d failed: %v. Retrying in %v...\n", attempt+1, err, backoff)

            time.Sleep(backoff)
            // => Wait before retry
            // => Gives service time to recover
        }
    }

    // All retries exhausted
    return fmt.Errorf("operation failed after %d attempts: %w", maxRetries, err)
}

func calculateBackoff(attempt int) time.Duration {
    // => Calculates exponential backoff with jitter
    // => attempt is retry number (0, 1, 2, ...)

    baseDelay := 1 * time.Second
    // => Base delay (starting point)

    maxDelay := 30 * time.Second
    // => Maximum delay (cap exponential growth)

    exponentialDelay := time.Duration(math.Pow(2, float64(attempt))) * baseDelay
    // => 2^0 * 1s = 1s
    // => 2^1 * 1s = 2s
    // => 2^2 * 1s = 4s
    // => 2^3 * 1s = 8s
    // => Doubles on each retry

    if exponentialDelay > maxDelay {
        exponentialDelay = maxDelay
        // => Cap at 30 seconds
        // => Prevents unbounded growth
    }

    jitter := time.Duration(rand.Int63n(int64(exponentialDelay / 2)))
    // => Jitter is random value: 0 to exponentialDelay/2
    // => Prevents retry storms (many clients retry simultaneously)
    // => Spreads retries over time

    return exponentialDelay + jitter
    // => Final backoff with randomness
    // => Example: 4s + 1.5s = 5.5s
}

func main() {
    rand.Seed(time.Now().UnixNano())
    // => Seed random number generator
    // => Different jitter on each run

    attemptCount := 0
    operation := func() error {
        // => Simulates failing operation
        // => Succeeds on 4th attempt

        attemptCount++
        if attemptCount < 4 {
            return fmt.Errorf("temporary error")
        }
        return nil
    }

    err := retryWithBackoff(5, operation)
    // => Retry up to 5 times

    if err != nil {
        fmt.Println("Final error:", err)
    } else {
        fmt.Println("Operation succeeded!")
        // => Output: Operation succeeded!
        // => After 3 retries
    }
}
```

## Production Framework: Circuit Breaker with gobreaker

Circuit breaker prevents cascading failures by stopping requests to failing services.

**Adding gobreaker**:

```bash
go get github.com/sony/gobreaker
# => Installs circuit breaker library
# => Industry-standard implementation
```

**Pattern: Circuit Breaker**:

```go
package main

import (
    "fmt"
    "time"

    "github.com/sony/gobreaker"
    // => Circuit breaker library
    // => Three states: Closed, Open, Half-Open
)

var cb *gobreaker.CircuitBreaker
// => Global circuit breaker instance

func init() {
    // => Initializes circuit breaker

    settings := gobreaker.Settings{
        Name: "API Circuit Breaker",
        // => Circuit breaker name (for logging)

        MaxRequests: 3,
        // => Max requests allowed in Half-Open state
        // => After 3 successes, circuit closes

        Interval: 10 * time.Second,
        // => Interval to reset failure count in Closed state
        // => Counts failures per 10-second window

        Timeout: 30 * time.Second,
        // => Timeout in Open state before transitioning to Half-Open
        // => After 30s, allows test requests

        ReadyToTrip: func(counts gobreaker.Counts) bool {
            // => Determines when to open circuit
            // => counts contains failure statistics

            failureRatio := float64(counts.TotalFailures) / float64(counts.Requests)
            // => Failure ratio: TotalFailures / Requests
            // => Example: 5 failures / 10 requests = 0.5 (50%)

            return counts.Requests >= 10 && failureRatio >= 0.5
            // => Open circuit if:
            // => - At least 10 requests (minimum sample size)
            // => - 50%+ failure rate
        },

        OnStateChange: func(name string, from gobreaker.State, to gobreaker.State) {
            // => Called when circuit state changes
            // => from: previous state (Closed, Open, Half-Open)
            // => to: new state

            fmt.Printf("Circuit breaker %s: %s -> %s\n", name, from, to)
            // => Output: Circuit breaker API Circuit Breaker: StateOpen -> StateHalfOpen
            // => Logging for monitoring
        },
    }

    cb = gobreaker.NewCircuitBreaker(settings)
    // => Creates circuit breaker with settings
}

func callExternalAPI() (string, error) {
    // => Calls external API through circuit breaker
    // => Returns response or circuit breaker error

    result, err := cb.Execute(func() (interface{}, error) {
        // => Execute wraps operation in circuit breaker
        // => Only executes if circuit Closed or Half-Open

        return fetchFromAPI()
        // => Actual API call
        // => Success or failure recorded by circuit breaker
    })
    // => result is interface{} (type assertion needed)
    // => err is operation error or gobreaker.ErrOpenState

    if err != nil {
        // => Operation failed or circuit open

        if err == gobreaker.ErrOpenState {
            // => Circuit is open (too many failures)
            // => Fast-fail without calling API

            return "", fmt.Errorf("circuit breaker open: service unavailable")
        }

        return "", err
        // => Operation error
    }

    return result.(string), nil
    // => Type assertion to string
}

func fetchFromAPI() (string, error) {
    // => Simulates external API call
    // => Production: actual HTTP request

    time.Sleep(100 * time.Millisecond)
    // => Simulate API latency

    // Simulate failures (70% failure rate)
    if time.Now().Unix()%10 < 7 {
        return "", fmt.Errorf("API error")
    }

    return "API response", nil
}

func main() {
    // Make multiple requests
    for i := 0; i < 50; i++ {
        result, err := callExternalAPI()

        if err != nil {
            fmt.Printf("Request %d failed: %v\n", i+1, err)
        } else {
            fmt.Printf("Request %d succeeded: %s\n", i+1, result)
        }

        time.Sleep(200 * time.Millisecond)
        // => Delay between requests
    }

    // Circuit breaker state transitions:
    // 1. Closed: Normal operation (all requests allowed)
    // 2. Open: Too many failures (fast-fail, no API calls)
    // 3. Half-Open: Test if service recovered (limited requests)
    // 4. Back to Closed if tests succeed
}
```

**Circuit Breaker States**:

1. **Closed** (Normal): All requests pass through, failures counted
2. **Open** (Failing): All requests rejected immediately (fast-fail), no API calls
3. **Half-Open** (Testing): Limited test requests to check if service recovered
4. **Closed** (Recovered): If test requests succeed, resume normal operation

## Production Pattern: Bulkhead

Bulkhead isolates resources (connections, goroutines) to prevent exhaustion.

**Pattern: Worker Pool Bulkhead**:

```go
package main

import (
    "fmt"
    "time"
)

type Bulkhead struct {
    semaphore chan struct{}
    // => Semaphore controls concurrent operations
    // => Buffer size limits concurrency
}

func NewBulkhead(maxConcurrent int) *Bulkhead {
    // => Creates bulkhead with concurrency limit
    // => maxConcurrent is max parallel operations

    return &Bulkhead{
        semaphore: make(chan struct{}, maxConcurrent),
        // => Buffered channel with maxConcurrent capacity
        // => Full channel blocks new operations
    }
}

func (b *Bulkhead) Execute(fn func() error) error {
    // => Executes function with concurrency control
    // => Blocks if concurrency limit reached

    b.semaphore <- struct{}{}
    // => Acquire semaphore (blocks if full)
    // => Adds token to channel
    // => Blocks when channel full (maxConcurrent operations running)

    defer func() {
        <-b.semaphore
        // => Release semaphore
        // => Removes token from channel
        // => Allows next operation to proceed
    }()

    return fn()
    // => Execute operation
    // => Guaranteed: at most maxConcurrent executing concurrently
}

func main() {
    bulkhead := NewBulkhead(5)
    // => Limit to 5 concurrent operations
    // => 6th operation blocks until slot available

    for i := 0; i < 20; i++ {
        go func(id int) {
            // => Launch goroutine (non-blocking)

            err := bulkhead.Execute(func() error {
                // => Operation controlled by bulkhead
                // => At most 5 operations running simultaneously

                fmt.Printf("Task %d started\n", id)
                time.Sleep(1 * time.Second)
                // => Simulate work
                fmt.Printf("Task %d completed\n", id)
                return nil
            })

            if err != nil {
                fmt.Printf("Task %d error: %v\n", id, err)
            }
        }(i)
    }

    time.Sleep(10 * time.Second)
    // => Wait for all tasks to complete
    // => Production: use sync.WaitGroup
}
```

**Why bulkhead matters**:

- Prevents resource exhaustion (connection pool, goroutines)
- Isolates failures (one service can't consume all resources)
- Maintains system stability under load
- Enables graceful degradation

## Trade-offs: When to Use Each

**Comparison table**:

| Pattern             | Purpose                  | Use Case                                   |
| ------------------- | ------------------------ | ------------------------------------------ |
| **Timeout**         | Limit operation duration | All external calls (HTTP, database, RPC)   |
| **Retry**           | Handle transient errors  | Network failures, temporary unavailability |
| **Circuit Breaker** | Stop cascading failures  | Degraded or failing services               |
| **Bulkhead**        | Resource isolation       | Prevent resource exhaustion                |

**When to use timeouts**:

- All external calls (HTTP, database, RPC)
- Long-running operations (file I/O, computation)
- User-facing requests (prevent hanging)
- Default: 30s for HTTP, 5s for database

**When to use retries**:

- Transient network errors (temporary DNS failures)
- Rate limiting (429 status codes)
- Temporary service unavailability (503 status)
- Idempotent operations (safe to retry)
- NOT for non-idempotent operations (payment processing)

**When to use circuit breakers**:

- Cascading failure prevention (service degradation)
- Fast-fail requirements (immediate error response)
- Service-to-service communication (microservices)
- External API calls (third-party services)

**When to use bulkheads**:

- Resource exhaustion prevention (connection pools)
- Multi-tenant systems (per-tenant resource limits)
- Mixed criticality workloads (prioritize critical operations)
- Goroutine pool management (limit concurrency)

## Production Best Practices

**Combine timeout + retry + circuit breaker**:

```go
// GOOD: defense in depth (multiple resilience layers)
func callServiceResilience(url string) (string, error) {
    return cb.Execute(func() (interface{}, error) {
        // => Layer 3: Circuit breaker

        return retryWithBackoff(3, func() error {
            // => Layer 2: Retry with backoff

            ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
            defer cancel()
            // => Layer 1: Timeout

            return fetchWithContext(ctx, url)
        })
    })
}

// BAD: timeout only (no retry, no circuit breaker)
func callServiceUnsafe(url string) (string, error) {
    ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
    defer cancel()
    return fetchWithContext(ctx, url)
}
```

**Set appropriate timeout values**:

```go
// GOOD: reasonable timeouts based on operation
httpTimeout := 30 * time.Second    // HTTP requests
dbTimeout := 5 * time.Second       // Database queries
rpcTimeout := 10 * time.Second     // RPC calls

// BAD: too short (false positives) or too long (hanging)
timeout := 100 * time.Millisecond  // Too short for HTTP
timeout := 5 * time.Minute         // Too long (blocks resources)
```

**Use idempotency keys for retries**:

```go
// GOOD: idempotency key prevents duplicate operations
func processPaymentWithRetry(paymentID string, amount float64) error {
    idempotencyKey := fmt.Sprintf("payment-%s", paymentID)
    // => Idempotency key identifies unique operation
    // => Same key = same operation (not duplicate)

    return retryWithBackoff(3, func() error {
        return processPayment(paymentID, amount, idempotencyKey)
        // => Server checks idempotency key
        // => Duplicate requests return original result
    })
}

// BAD: retry non-idempotent operation (double payment)
func processPaymentUnsafe(paymentID string, amount float64) error {
    return processPayment(paymentID, amount, "")  // No idempotency key
}
```

**Monitor circuit breaker state**:

```go
// Track circuit breaker metrics
// => Closed: healthy service
// => Open: failing service (alert ops team)
// => Half-Open: testing recovery

OnStateChange: func(name string, from gobreaker.State, to gobreaker.State) {
    metrics.RecordCircuitBreakerState(name, to)
    // => Send metrics to monitoring system (Prometheus, Datadog)

    if to == gobreaker.StateOpen {
        alerts.SendAlert(fmt.Sprintf("Circuit breaker %s is OPEN", name))
        // => Alert operations team immediately
    }
}
```

## Summary

Resilience patterns prevent cascading failures in distributed systems through fault isolation and graceful degradation. Standard library provides context.WithTimeout for basic timeouts but no circuit breaker, retry logic, or bulkhead patterns. Production systems combine timeouts, exponential backoff with jitter for retries, gobreaker for circuit breakers, and worker pools for bulkheads. Use timeouts for all external calls, retries for transient errors, circuit breakers to stop hammering failing services, and bulkheads to prevent resource exhaustion. Monitor circuit breaker state and combine multiple resilience layers for defense in depth.

**Key takeaways**:

- Use context.WithTimeout for all external calls (HTTP, database, RPC)
- Implement exponential backoff with jitter for retry logic
- Use gobreaker circuit breaker to stop cascading failures
- Implement bulkhead pattern to isolate resources
- Combine timeout + retry + circuit breaker for defense in depth
- Set appropriate timeouts based on operation type (30s HTTP, 5s database)
- Use idempotency keys for safe retries
- Monitor circuit breaker state transitions (alert on Open state)
