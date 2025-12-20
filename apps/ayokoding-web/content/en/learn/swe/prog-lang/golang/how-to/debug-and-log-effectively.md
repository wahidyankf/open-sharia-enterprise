---
title: "How to Debug and Log Effectively"
date: 2025-12-17T13:19:07+07:00
draft: false
weight: 1000140
description: "Master Go debugging with Delve, structured logging with slog, and effective debugging strategies"
tags: ["golang", "debugging", "logging", "delve", "slog"]
categories: ["learn"]
---

## Problem

Go's simplicity means fewer debugging tools compared to other languages, but this forces developers to use debuggers and logging effectively. Print debugging clutters code, and poor logging creates noise instead of insights. Understanding when to use each debugging approach saves hours of investigation.

This guide shows effective debugging and logging practices in Go.

## Using Delve Debugger

### Basic Delve Commands

```bash
# Install Delve
go install github.com/go-delve/delve/cmd/dlv@latest

# Debug current package
dlv debug

# Debug with arguments
dlv debug -- --config=prod.json

# Debug test
dlv test

# Attach to running process
dlv attach <pid>
```

### Setting Breakpoints

```go
package main

import "fmt"

func processOrder(order *Order) error {
    // Set breakpoint here: (dlv) break main.processOrder:15
    validateOrder(order)

    // Conditional breakpoint: (dlv) break main.go:20 if order.Total > 1000
    total := calculateTotal(order)

    if total > 0 {
        // Break on this line: (dlv) break main.go:24
        processPayment(order, total)
    }

    return nil
}
```

**Common Delve commands:**

```bash
# Breakpoints
break main.go:25              # Break at line 25
break main.processOrder       # Break at function entry
break main.go:30 if x > 100   # Conditional breakpoint
breakpoints                   # List all breakpoints
clear 1                       # Remove breakpoint 1

# Execution control
continue                      # Resume execution
next                         # Step over
step                         # Step into
stepout                      # Step out of function

# Inspection
print order                   # Print variable
print order.Total            # Print field
locals                       # Show local variables
args                         # Show function arguments
stack                        # Show call stack

# Goroutines
goroutines                   # List all goroutines
goroutine 5                  # Switch to goroutine 5
```

**Why it matters**: Delve provides precise control over execution flow, variable inspection, and goroutine debugging. Essential for understanding complex bugs that print debugging can't solve.

### Debugging Goroutines

```go
func main() {
    done := make(chan bool)

    go func() {
        // (dlv) goroutines - see all goroutines
        // (dlv) goroutine 2 - switch to this goroutine
        for i := 0; i < 5; i++ {
            fmt.Println("Working:", i)
            time.Sleep(100 * time.Millisecond)
        }
        done <- true
    }()

    <-done
}
```

**Debugging race conditions:**

```bash
# Run with race detector
go run -race main.go

# Test with race detector
go test -race

# Build with race detector
go build -race
```

## Logging with log Package

### Standard Library Logging

```go
import "log"

func main() {
    // ✅ Basic logging
    log.Println("Application started")
    log.Printf("Processing order %s", orderID)

    // ✅ Custom logger with prefix
    errorLog := log.New(os.Stderr, "ERROR: ", log.Ldate|log.Ltime|log.Lshortfile)
    errorLog.Println("Payment failed")
    // Output: ERROR: 2025/12/17 13:19:07 main.go:25: Payment failed

    // ✅ Fatal logging (calls os.Exit(1))
    if err != nil {
        log.Fatal("Critical error:", err)
        // Code after this never runs
    }

    // ✅ Panic logging
    if invalidState {
        log.Panic("Invalid state detected")
        // Triggers panic with stack trace
    }
}

// ✅ Configure default logger
func init() {
    log.SetFlags(log.Ldate | log.Ltime | log.Lmicroseconds | log.Lshortfile)
    log.SetPrefix("APP: ")
}
```

## Structured Logging with slog

### Modern Structured Logging (Go 1.21+)

```go
import "log/slog"

func main() {
    // ✅ Default logger
    slog.Info("Application started")
    slog.Info("Processing order", "orderID", "order-123", "amount", 100.50)
    // Output: time=2025-12-17T13:19:07.123+07:00 level=INFO msg="Processing order" orderID=order-123 amount=100.5

    // ✅ Structured fields
    slog.Info("User login",
        slog.String("userID", "user-456"),
        slog.Int("age", 25),
        slog.Bool("premium", true),
    )

    // ✅ Different log levels
    slog.Debug("Debug information")
    slog.Info("Informational message")
    slog.Warn("Warning message")
    slog.Error("Error occurred")
}
```

### Custom slog Handler

```go
// ✅ JSON handler for production
func setupProductionLogger() {
    handler := slog.NewJSONHandler(os.Stdout, &slog.HandlerOptions{
        Level: slog.LevelInfo,
        AddSource: true,
    })
    logger := slog.New(handler)
    slog.SetDefault(logger)
}

// ✅ Text handler for development
func setupDevelopmentLogger() {
    handler := slog.NewTextHandler(os.Stdout, &slog.HandlerOptions{
        Level: slog.LevelDebug,
        AddSource: true,
    })
    logger := slog.New(handler)
    slog.SetDefault(logger)
}

func main() {
    if os.Getenv("ENV") == "production" {
        setupProductionLogger()
    } else {
        setupDevelopmentLogger()
    }

    slog.Info("Application started", "version", "1.0.0")
}
```

### Contextual Logging

```go
// ✅ Logger with default fields
func NewUserService(db *sql.DB) *UserService {
    logger := slog.Default().With(
        slog.String("service", "user"),
        slog.String("component", "backend"),
    )

    return &UserService{
        db:     db,
        logger: logger,
    }
}

type UserService struct {
    db     *sql.DB
    logger *slog.Logger
}

func (s *UserService) GetUser(id string) (*User, error) {
    // All logs include service and component fields
    s.logger.Info("Fetching user", "userID", id)

    user, err := s.db.QueryUser(id)
    if err != nil {
        s.logger.Error("Database error", "userID", id, "error", err)
        return nil, err
    }

    s.logger.Info("User fetched successfully", "userID", id)
    return user, nil
}
```

### Log Groups

```go
// ✅ Group related fields
func ProcessOrder(order *Order) {
    slog.Info("Processing order",
        slog.Group("order",
            slog.String("id", order.ID),
            slog.Float64("total", order.Total),
            slog.Int("items", len(order.Items)),
        ),
        slog.Group("customer",
            slog.String("id", order.CustomerID),
            slog.String("type", order.CustomerType),
        ),
    )
    // Output: msg="Processing order" order.id=order-123 order.total=100.5 order.items=3 customer.id=cust-456 customer.type=premium
}
```

## Debugging Strategies

### Print Debugging

```go
// ✅ Temporary print debugging
func ProcessData(data []int) []int {
    fmt.Printf("DEBUG: Input data: %v\n", data)

    result := make([]int, 0, len(data))
    for i, v := range data {
        fmt.Printf("DEBUG: Processing index %d, value %d\n", i, v)

        if v > 0 {
            result = append(result, v*2)
        }
    }

    fmt.Printf("DEBUG: Result: %v\n", result)
    return result
}

// ❌ Don't commit debug prints
// Remove before committing or use build tags
```

### Debug Build Tags

```go
// debug.go
//go:build debug
// +build debug

package main

import "fmt"

func debugLog(format string, args ...interface{}) {
    fmt.Printf("DEBUG: "+format+"\n", args...)
}

// release.go
//go:build !debug
// +build !debug

package main

func debugLog(format string, args ...interface{}) {
    // No-op in release builds
}

// Usage
func processOrder(order *Order) {
    debugLog("Processing order: %+v", order)
    // Only prints when built with -tags debug
}
```

**Build with debug:**

```bash
go build -tags debug
```

### Logging Best Practices

```go
// ✅ Log at appropriate levels
func ProcessPayment(payment *Payment) error {
    // DEBUG - Development diagnostics
    slog.Debug("Payment validation started", "paymentID", payment.ID)

    // INFO - Important business events
    slog.Info("Processing payment",
        "paymentID", payment.ID,
        "amount", payment.Amount,
        "userID", payment.UserID,
    )

    err := chargeCard(payment)
    if err != nil {
        // ERROR - Something went wrong
        slog.Error("Payment failed",
            "paymentID", payment.ID,
            "error", err,
        )
        return err
    }

    // INFO - Success event
    slog.Info("Payment completed successfully", "paymentID", payment.ID)
    return nil
}

// ✅ Include context in errors
func FetchUser(id string) (*User, error) {
    user, err := db.QueryUser(id)
    if err != nil {
        slog.Error("Failed to fetch user",
            "userID", id,
            "error", err,
            "database", dbConfig.Host,
        )
        return nil, fmt.Errorf("fetching user %s: %w", id, err)
    }
    return user, nil
}

// ❌ Don't log sensitive data
func LoginUser(email, password string) error {
    slog.Info("User login attempt", "email", email)
    // ❌ NEVER log passwords
    // slog.Debug("Password", "password", password)

    // ✅ Log non-sensitive data only
    token, err := authenticate(email, password)
    if err != nil {
        slog.Warn("Login failed", "email", email)
        return err
    }

    // ❌ Don't log full tokens
    // slog.Info("Token", "token", token)

    // ✅ Log token prefix only
    slog.Info("Login successful",
        "email", email,
        "tokenPrefix", token[:8],
    )
    return nil
}
```

## Error Handling and Logging

### Logging Errors with Context

```go
// ✅ Log error with full context
func UpdateUser(id string, updates map[string]interface{}) error {
    user, err := fetchUser(id)
    if err != nil {
        slog.Error("Failed to fetch user for update",
            "userID", id,
            "error", err,
        )
        return fmt.Errorf("update user: fetch failed: %w", err)
    }

    if err := validateUpdates(updates); err != nil {
        slog.Warn("Invalid update data",
            "userID", id,
            "updates", updates,
            "error", err,
        )
        return fmt.Errorf("update user: validation failed: %w", err)
    }

    if err := saveUser(user); err != nil {
        slog.Error("Failed to save user",
            "userID", id,
            "error", err,
        )
        return fmt.Errorf("update user: save failed: %w", err)
    }

    slog.Info("User updated successfully", "userID", id)
    return nil
}
```

### Panic Recovery with Logging

```go
// ✅ Recover from panics with logging
func SafeHandler(handler http.HandlerFunc) http.HandlerFunc {
    return func(w http.ResponseWriter, r *http.Request) {
        defer func() {
            if err := recover(); err != nil {
                slog.Error("Panic recovered",
                    "error", err,
                    "path", r.URL.Path,
                    "method", r.Method,
                    "stack", string(debug.Stack()),
                )

                http.Error(w, "Internal Server Error", http.StatusInternalServerError)
            }
        }()

        handler(w, r)
    }
}
```

## Performance Debugging

### Profiling

```go
import (
    "net/http"
    _ "net/http/pprof"
)

func main() {
    // ✅ Enable pprof
    go func() {
        log.Println(http.ListenAndServe("localhost:6060", nil))
    }()

    // Your application code
    runApplication()
}
```

**Access profiles:**

```bash
# CPU profile
go tool pprof http://localhost:6060/debug/pprof/profile?seconds=30

# Memory profile
go tool pprof http://localhost:6060/debug/pprof/heap

# Goroutine profile
go tool pprof http://localhost:6060/debug/pprof/goroutine

# View in browser
go tool pprof -http=:8080 http://localhost:6060/debug/pprof/profile
```

### Benchmarking with Logging

```go
// ✅ Benchmark different approaches
func BenchmarkStringConcat(b *testing.B) {
    for i := 0; i < b.N; i++ {
        _ = "hello" + "world"
    }
}

func BenchmarkStringBuilder(b *testing.B) {
    for i := 0; i < b.N; i++ {
        var builder strings.Builder
        builder.WriteString("hello")
        builder.WriteString("world")
        _ = builder.String()
    }
}
```

## Testing Logging

### Capturing Log Output

```go
import (
    "bytes"
    "log/slog"
    "testing"
)

func TestLogging(t *testing.T) {
    // ✅ Capture log output
    var buf bytes.Buffer
    handler := slog.NewTextHandler(&buf, nil)
    logger := slog.New(handler)

    logger.Info("Test message", "key", "value")

    output := buf.String()
    if !strings.Contains(output, "Test message") {
        t.Errorf("expected log message, got: %s", output)
    }
    if !strings.Contains(output, "key=value") {
        t.Errorf("expected key=value, got: %s", output)
    }
}

func TestServiceLogging(t *testing.T) {
    var buf bytes.Buffer
    handler := slog.NewTextHandler(&buf, nil)
    logger := slog.New(handler)

    service := &UserService{logger: logger}
    service.ProcessUser("user-123")

    output := buf.String()
    if !strings.Contains(output, "user-123") {
        t.Error("expected userID in log output")
    }
}
```

## Summary

Debugging in Go combines Delve debugger, structured logging, and strategic print debugging. Delve provides breakpoints, variable inspection, and goroutine debugging essential for complex issues. Set breakpoints with conditions, inspect variables at runtime, and navigate execution flow precisely.

Structured logging with slog produces machine-parseable logs with typed fields. Unlike printf-style logging that creates unstructured text, slog creates JSON or key-value output that monitoring tools can parse and query. Add context through fields, not string formatting.

Log levels communicate severity and audience. DEBUG for development diagnostics, INFO for business events, WARN for unexpected but handled situations, ERROR for failures requiring attention. Choose levels based on who needs the information and when.

Contextual logging attaches default fields to loggers. Service loggers include service name and component, request loggers include request ID and user ID. Every log from that logger automatically includes context fields without manual repetition.

Error logging requires both structured logging and error wrapping. Log errors with full context fields, wrap errors with fmt.Errorf and %w to preserve error chains. Logged context helps debugging, wrapped errors enable errors.Is and errors.As checking.

Never log sensitive data - passwords, API keys, full tokens, credit card numbers. Log email addresses and user IDs for correlation, token prefixes for debugging, but omit secrets. Structure logs to exclude sensitive fields automatically.

Print debugging suits quick investigations but clutters code. Use build tags to enable debug prints only in development builds. Remove debug prints before committing or replace with proper logging.

Profiling with pprof identifies performance bottlenecks. CPU profiles show where time is spent, memory profiles show allocation patterns, goroutine profiles reveal concurrency issues. Profile production systems to find real-world bottlenecks.

Testing logging output verifies correct messages and fields. Capture log output in buffers during tests, assert expected messages appear. Test that errors log appropriate context fields.

Effective debugging combines tools based on problem type. Delve for logic errors and state inspection, logging for production issues and distributed systems, profiling for performance problems, race detector for concurrency bugs. Master all approaches to debug efficiently.

## Related Content

- [Go Best Practices and Idioms](/en/learn/swe/prog-lang/golang/explanation/best-practices)
- [How to Handle Errors Effectively](/en/learn/swe/prog-lang/golang/how-to/handle-errors-effectively)
- [How to Write Effective Tests](/en/learn/swe/prog-lang/golang/how-to/write-effective-tests)
