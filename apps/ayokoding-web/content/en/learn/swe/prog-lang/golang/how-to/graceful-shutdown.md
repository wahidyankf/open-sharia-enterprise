---
title: "How to Implement Graceful Shutdown"
date: 2025-12-21T00:00:00+07:00
draft: false
weight: 1000021
description: "Practical techniques for implementing graceful shutdown in Go applications using signal handling, context cancellation, and wait groups"
---

## Problem

Abrupt shutdowns can leave requests unfinished and resources unclosed. Proper shutdown ensures clean resource cleanup.

## Solution

### 1. HTTP Server Graceful Shutdown

```go
func main() {
    server := &http.Server{Addr: ":8080", Handler: handler}

    go func() {
        if err := server.ListenAndServe(); err != http.ErrServerClosed {
            log.Fatalf("Server failed: %v", err)
        }
    }()

    // Wait for interrupt signal
    quit := make(chan os.Signal, 1)
    signal.Notify(quit, os.Interrupt, syscall.SIGTERM)
    <-quit

    log.Println("Shutting down server...")

    ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
    defer cancel()

    if err := server.Shutdown(ctx); err != nil {
        log.Fatalf("Server forced to shutdown: %v", err)
    }

    log.Println("Server exited")
}
```

### 2. Worker Pool Shutdown

```go
func workerPool(ctx context.Context) {
    var wg sync.WaitGroup

    for i := 0; i < 10; i++ {
        wg.Add(1)
        go func(id int) {
            defer wg.Done()

            for {
                select {
                case <-ctx.Done():
                    log.Printf("Worker %d shutting down", id)
                    return
                default:
                    // Do work
                    time.Sleep(time.Second)
                }
            }
        }(i)
    }

    wg.Wait()
    log.Println("All workers stopped")
}

func main() {
    ctx, cancel := context.WithCancel(context.Background())

    go workerPool(ctx)

    quit := make(chan os.Signal, 1)
    signal.Notify(quit, os.Interrupt)
    <-quit

    cancel()
    time.Sleep(2 * time.Second)  // Allow cleanup
}
```

## How It Works

### Signal Handling in Go

Go's `os/signal` package provides cross-platform signal notification:

1. **Signal Registration**: `signal.Notify()` registers a channel to receive specific OS signals
2. **Signal Delivery**: OS sends signals (SIGINT from Ctrl+C, SIGTERM from kill)
3. **Channel Notification**: Go runtime writes signal to registered channel
4. **Handler Execution**: Application reads from channel and initiates shutdown
5. **Cleanup**: Resources close gracefully before process exits

### HTTP Server Shutdown Mechanism

`server.Shutdown()` performs graceful HTTP server shutdown:

1. **Stop Accepting**: Server stops accepting new connections
2. **Wait for Idle**: Waits for active connections to become idle
3. **Context Timeout**: Shutdown bounded by context timeout
4. **Force Close**: After timeout, forcefully closes remaining connections
5. **Return**: Returns when all connections closed or timeout reached

### Context Cancellation Propagation

Context cancellation cascades through goroutine hierarchy:

```
Parent Context (canceled)
    ↓
Child Context 1 (receives cancellation)
    ↓
    Worker Goroutine 1 (exits via ctx.Done())

Parent Context (canceled)
    ↓
Child Context 2 (receives cancellation)
    ↓
    Worker Goroutine 2 (exits via ctx.Done())
```

All child contexts automatically canceled when parent canceled.

### WaitGroup Synchronization

`sync.WaitGroup` tracks goroutine completion:

- `wg.Add(n)` - Increment counter before starting goroutines
- `wg.Done()` - Decrement counter when goroutine completes (use with `defer`)
- `wg.Wait()` - Block until counter reaches zero
- **Critical**: Must call `Add()` before goroutine starts (race condition otherwise)

### Shutdown Sequence

Typical shutdown follows this pattern:

```
1. Receive shutdown signal (SIGINT/SIGTERM)
2. Stop accepting new work (close listeners/channels)
3. Cancel context to notify goroutines
4. Wait for in-flight work to complete (WaitGroup)
5. Close resources (database connections, files)
6. Exit process
```

## Variations

### 1. Multi-Stage Shutdown with Timeout Stages

Implement phased shutdown with different timeouts per stage:

```go
func gracefulShutdown(server *http.Server, db *sql.DB) {
    quit := make(chan os.Signal, 1)
    signal.Notify(quit, os.Interrupt, syscall.SIGTERM)
    <-quit

    // Stage 1: Stop HTTP server (30s timeout)
    log.Println("Stage 1: Shutting down HTTP server...")
    ctx1, cancel1 := context.WithTimeout(context.Background(), 30*time.Second)
    defer cancel1()

    if err := server.Shutdown(ctx1); err != nil {
        log.Printf("HTTP server shutdown error: %v", err)
    }

    // Stage 2: Close database connections (10s timeout)
    log.Println("Stage 2: Closing database...")
    ctx2, cancel2 := context.WithTimeout(context.Background(), 10*time.Second)
    defer cancel2()

    done := make(chan error, 1)
    go func() {
        done <- db.Close()
    }()

    select {
    case err := <-done:
        if err != nil {
            log.Printf("Database close error: %v", err)
        }
    case <-ctx2.Done():
        log.Println("Database close timed out")
    }

    log.Println("Shutdown complete")
}
```

**Trade-offs**: Finer control over resource cleanup but more complex.

### 2. Shutdown Coordination with errgroup

Use `errgroup` to coordinate multiple shutdown tasks:

```go
import "golang.org/x/sync/errgroup"

func coordinatedShutdown(server *http.Server, workers *WorkerPool) error {
    quit := make(chan os.Signal, 1)
    signal.Notify(quit, os.Interrupt, syscall.SIGTERM)
    <-quit

    g, ctx := errgroup.WithContext(context.Background())

    // Shutdown HTTP server
    g.Go(func() error {
        shutdownCtx, cancel := context.WithTimeout(ctx, 30*time.Second)
        defer cancel()
        return server.Shutdown(shutdownCtx)
    })

    // Stop workers
    g.Go(func() error {
        return workers.Stop(ctx)
    })

    // Wait for all shutdown tasks
    return g.Wait()
}
```

**Trade-offs**: Parallel shutdown but requires error handling strategy.

### 3. State Machine Shutdown

Track shutdown state for complex applications:

```go
type ShutdownState int

const (
    StateRunning ShutdownState = iota
    StateShuttingDown
    StateShutdown
)

type Application struct {
    state     ShutdownState
    stateMux  sync.RWMutex
    server    *http.Server
    done      chan struct{}
}

func (app *Application) Shutdown() {
    app.stateMux.Lock()
    if app.state != StateRunning {
        app.stateMux.Unlock()
        return  // Already shutting down
    }
    app.state = StateShuttingDown
    app.stateMux.Unlock()

    ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
    defer cancel()

    app.server.Shutdown(ctx)

    app.stateMux.Lock()
    app.state = StateShutdown
    app.stateMux.Unlock()

    close(app.done)
}

func (app *Application) IsRunning() bool {
    app.stateMux.RLock()
    defer app.stateMux.RUnlock()
    return app.state == StateRunning
}
```

**Trade-offs**: Prevents duplicate shutdown but adds complexity.

### 4. Channel-Based Worker Shutdown

Use channels to signal worker shutdown:

```go
type Worker struct {
    jobs   chan Job
    quit   chan struct{}
    done   chan struct{}
}

func (w *Worker) Start() {
    go func() {
        defer close(w.done)

        for {
            select {
            case job := <-w.jobs:
                processJob(job)
            case <-w.quit:
                // Drain remaining jobs
                for job := range w.jobs {
                    processJob(job)
                }
                return
            }
        }
    }()
}

func (w *Worker) Stop() {
    close(w.quit)   // Signal shutdown
    close(w.jobs)   // Stop new jobs
    <-w.done        // Wait for completion
}
```

**Trade-offs**: Gracefully processes queued work but more channels to manage.

### 5. Shutdown Hooks Registry

Register cleanup functions to run on shutdown:

```go
type ShutdownHooks struct {
    hooks []func(context.Context) error
    mu    sync.Mutex
}

func (sh *ShutdownHooks) Register(hook func(context.Context) error) {
    sh.mu.Lock()
    defer sh.mu.Unlock()
    sh.hooks = append(sh.hooks, hook)
}

func (sh *ShutdownHooks) Shutdown(ctx context.Context) error {
    sh.mu.Lock()
    hooks := make([]func(context.Context) error, len(sh.hooks))
    copy(hooks, sh.hooks)
    sh.mu.Unlock()

    // Execute hooks in reverse order (LIFO)
    for i := len(hooks) - 1; i >= 0; i-- {
        if err := hooks[i](ctx); err != nil {
            return fmt.Errorf("shutdown hook %d failed: %w", i, err)
        }
    }
    return nil
}

// Usage
hooks := &ShutdownHooks{}
hooks.Register(func(ctx context.Context) error {
    return server.Shutdown(ctx)
})
hooks.Register(func(ctx context.Context) error {
    return db.Close()
})
```

**Trade-offs**: Flexible registration but execution order critical.

## Common Pitfalls

### 1. Forgetting to Stop Accepting New Work

**Problem**: Shutdown initiated but server still accepts new connections:

```go
// Bad: Server keeps accepting connections during shutdown
func badShutdown() {
    quit := make(chan os.Signal, 1)
    signal.Notify(quit, os.Interrupt)
    <-quit

    log.Println("Shutting down...")
    // Goroutines still running, server still accepting!
    time.Sleep(5 * time.Second)  // Hope they finish
}
```

**Solution**: Explicitly stop accepting new work before waiting:

```go
// Good: Stop server first, then wait for completion
func goodShutdown(server *http.Server) {
    quit := make(chan os.Signal, 1)
    signal.Notify(quit, os.Interrupt)
    <-quit

    ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
    defer cancel()

    // Stops accepting new connections immediately
    if err := server.Shutdown(ctx); err != nil {
        log.Printf("Shutdown error: %v", err)
    }
}
```

### 2. Not Using Timeout for Shutdown

**Problem**: Shutdown can hang indefinitely:

```go
// Bad: No timeout, can hang forever
func badShutdown(server *http.Server) {
    quit := make(chan os.Signal, 1)
    signal.Notify(quit, os.Interrupt)
    <-quit

    // No timeout - might never complete!
    if err := server.Shutdown(context.Background()); err != nil {
        log.Fatal(err)
    }
}
```

**Solution**: Always use context with timeout:

```go
// Good: Bounded shutdown time
func goodShutdown(server *http.Server) {
    quit := make(chan os.Signal, 1)
    signal.Notify(quit, os.Interrupt)
    <-quit

    // 30 second timeout
    ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
    defer cancel()

    if err := server.Shutdown(ctx); err != nil {
        log.Printf("Shutdown forced after timeout: %v", err)
    }
}
```

### 3. WaitGroup Add After Goroutine Start

**Problem**: Race condition between `Add()` and goroutine execution:

```go
// Bad: Race condition - goroutine might call Done() before Add()
var wg sync.WaitGroup

for i := 0; i < 10; i++ {
    go func(id int) {
        wg.Add(1)  // WRONG! Called inside goroutine
        defer wg.Done()
        doWork(id)
    }(i)
}

wg.Wait()  // Might not wait for all goroutines
```

**Solution**: Call `Add()` before starting goroutine:

```go
// Good: Add called before goroutine starts
var wg sync.WaitGroup

for i := 0; i < 10; i++ {
    wg.Add(1)  // Correct: Called before go keyword
    go func(id int) {
        defer wg.Done()
        doWork(id)
    }(i)
}

wg.Wait()  // Correctly waits for all
```

### 4. Ignoring Shutdown Errors

**Problem**: Not logging or handling shutdown errors:

```go
// Bad: Silent failure
func badShutdown(server *http.Server) {
    quit := make(chan os.Signal, 1)
    signal.Notify(quit, os.Interrupt)
    <-quit

    ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
    defer cancel()

    server.Shutdown(ctx)  // Error ignored!
    // No way to know if shutdown failed
}
```

**Solution**: Log and handle shutdown errors appropriately:

```go
// Good: Handle shutdown errors
func goodShutdown(server *http.Server) {
    quit := make(chan os.Signal, 1)
    signal.Notify(quit, os.Interrupt)
    <-quit

    log.Println("Shutdown initiated...")

    ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
    defer cancel()

    if err := server.Shutdown(ctx); err != nil {
        log.Printf("Error during shutdown: %v", err)
        // Could implement fallback strategy here
    } else {
        log.Println("Shutdown completed successfully")
    }
}
```

### 5. Leaking Goroutines During Shutdown

**Problem**: Goroutines don't respect context cancellation:

```go
// Bad: Goroutine doesn't check context
func badWorker(ctx context.Context) {
    go func() {
        for {
            // Never checks ctx.Done()!
            doWork()
            time.Sleep(1 * time.Second)
        }
    }()
}
```

**Solution**: Always check context cancellation:

```go
// Good: Respects context cancellation
func goodWorker(ctx context.Context) {
    go func() {
        for {
            select {
            case <-ctx.Done():
                log.Println("Worker shutting down")
                return  // Exit goroutine
            default:
                doWork()
                time.Sleep(1 * time.Second)
            }
        }
    }()
}
```

### 6. Resource Cleanup Without Error Handling

**Problem**: Resources fail to close silently:

```go
// Bad: Close errors ignored
func badCleanup(db *sql.DB, file *os.File) {
    db.Close()    // Might fail
    file.Close()  // Might fail
}
```

**Solution**: Handle cleanup errors explicitly:

```go
// Good: Log cleanup errors
func goodCleanup(db *sql.DB, file *os.File) error {
    var errs []error

    if err := db.Close(); err != nil {
        errs = append(errs, fmt.Errorf("database close: %w", err))
    }

    if err := file.Close(); err != nil {
        errs = append(errs, fmt.Errorf("file close: %w", err))
    }

    if len(errs) > 0 {
        return fmt.Errorf("cleanup errors: %v", errs)
    }
    return nil
}
```

## Related Patterns

**Related Tutorial**: See [Intermediate Tutorial - Concurrency](../tutorials/intermediate.md#concurrency).

**Related How-To**: See [Use Context](./use-context-effectively.md), [Handle Errors](./handle-errors-effectively.md).

**Related Cookbook**: See Cookbook recipe "Shutdown Patterns".
