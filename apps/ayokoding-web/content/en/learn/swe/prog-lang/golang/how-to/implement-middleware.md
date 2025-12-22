---
title: "How to Implement Middleware"
date: 2025-12-21T00:00:00+07:00
draft: false
weight: 1000020
description: "Practical techniques for implementing HTTP middleware patterns in Go for logging, authentication, rate limiting, and request processing"
---

## Problem

Cross-cutting concerns like logging, authentication, and CORS require repetitive code across handlers. Without middleware, this logic gets duplicated.

This guide shows middleware patterns in Go.

## Solution

### 1. Basic Middleware Pattern

```go
type Middleware func(http.Handler) http.Handler

func loggingMiddleware(next http.Handler) http.Handler {
    return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
        start := time.Now()
        log.Printf("Started %s %s", r.Method, r.URL.Path)

        next.ServeHTTP(w, r)

        log.Printf("Completed in %v", time.Since(start))
    })
}

func main() {
    handler := loggingMiddleware(http.HandlerFunc(helloHandler))
    http.ListenAndServe(":8080", handler)
}
```

### 2. Chaining Middleware

```go
func chainMiddleware(h http.Handler, middlewares ...Middleware) http.Handler {
    for i := len(middlewares) - 1; i >= 0; i-- {
        h = middlewares[i](h)
    }
    return h
}

func main() {
    handler := chainMiddleware(
        http.HandlerFunc(helloHandler),
        loggingMiddleware,
        authMiddleware,
        corsMiddleware,
    )
    http.ListenAndServe(":8080", handler)
}
```

### 3. Context-Based Middleware

```go
type contextKey string

const userIDKey contextKey = "userID"

func authMiddleware(next http.Handler) http.Handler {
    return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
        token := r.Header.Get("Authorization")
        userID := validateToken(token)

        ctx := context.WithValue(r.Context(), userIDKey, userID)
        next.ServeHTTP(w, r.WithContext(ctx))
    })
}

func handler(w http.ResponseWriter, r *http.Request) {
    userID := r.Context().Value(userIDKey).(int)
    fmt.Fprintf(w, "User ID: %d", userID)
}
```

## How It Works

### Middleware Execution Chain

Middleware wraps handlers in layers, creating an onion-like execution flow:

```
Request → Middleware 1 (before) → Middleware 2 (before) → Handler
          Middleware 1 (after) ← Middleware 2 (after) ← Handler
```

Each middleware:

1. **Receives** request from previous layer
2. **Executes** pre-processing logic
3. **Calls** `next.ServeHTTP(w, r)` to proceed
4. **Executes** post-processing logic after handler returns
5. **Returns** to previous layer

### Function Signature Pattern

Standard middleware signature:

```go
func(http.Handler) http.Handler
```

This pattern allows:

- **Wrapping**: Each middleware wraps the next handler
- **Composition**: Multiple middleware can be chained
- **Reusability**: Same middleware works with any handler
- **Type Safety**: Compiler enforces correct usage

### Context Propagation

`context.Context` passes request-scoped values through middleware chain:

1. **Original Context**: `r.Context()` starts with background context
2. **Add Values**: `context.WithValue(ctx, key, value)` creates new context
3. **Update Request**: `r.WithContext(ctx)` attaches context to request
4. **Read Values**: `r.Context().Value(key)` retrieves values downstream

**Important**: Context keys should be unexported types to avoid collisions.

### Early Exit Pattern

Middleware can stop the chain:

```go
func authMiddleware(next http.Handler) http.Handler {
    return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
        if !isAuthorized(r) {
            http.Error(w, "Unauthorized", http.StatusUnauthorized)
            return  // Don't call next.ServeHTTP
        }
        next.ServeHTTP(w, r)
    })
}
```

When middleware returns early, subsequent middleware and handler don't execute.

### Response Writer Wrapping

Capture response details by wrapping `http.ResponseWriter`:

```go
type responseWriter struct {
    http.ResponseWriter
    status int
    size   int
}

func (rw *responseWriter) WriteHeader(status int) {
    rw.status = status
    rw.ResponseWriter.WriteHeader(status)
}

func (rw *responseWriter) Write(b []byte) (int, error) {
    size, err := rw.ResponseWriter.Write(b)
    rw.size += size
    return size, err
}
```

This allows middleware to inspect response status and size.

## Variations

### 1. Middleware with Configuration

Create configurable middleware using closures:

```go
func rateLimitMiddleware(requestsPerSecond int) Middleware {
    limiter := rate.NewLimiter(rate.Limit(requestsPerSecond), 1)

    return func(next http.Handler) http.Handler {
        return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
            if !limiter.Allow() {
                http.Error(w, "Rate limit exceeded", http.StatusTooManyRequests)
                return
            }
            next.ServeHTTP(w, r)
        })
    }
}

// Usage:
handler := rateLimitMiddleware(100)(http.HandlerFunc(myHandler))
```

**Trade-offs**: Flexible configuration but adds function nesting.

### 2. Method-Based Middleware

Apply middleware only to specific HTTP methods:

```go
func methodMiddleware(method string, mw Middleware) Middleware {
    return func(next http.Handler) http.Handler {
        return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
            if r.Method == method {
                mw(next).ServeHTTP(w, r)
            } else {
                next.ServeHTTP(w, r)
            }
        })
    }
}

// Usage: Apply CSRF only to POST requests
handler := methodMiddleware("POST", csrfMiddleware)(http.HandlerFunc(createHandler))
```

**Trade-offs**: Conditional application but increases complexity.

### 3. Router-Level Middleware (with gorilla/mux)

Apply middleware to specific routes or route groups:

```go
import "github.com/gorilla/mux"

func main() {
    r := mux.NewRouter()

    // Global middleware
    r.Use(loggingMiddleware)

    // API subrouter with auth
    api := r.PathPrefix("/api").Subrouter()
    api.Use(authMiddleware)
    api.HandleFunc("/users", usersHandler)

    // Public routes (no auth)
    r.HandleFunc("/health", healthHandler)

    http.ListenAndServe(":8080", r)
}
```

**Trade-offs**: Fine-grained control but requires routing library.

### 4. Panic Recovery Middleware

Catch panics and return error responses:

```go
func recoveryMiddleware(next http.Handler) http.Handler {
    return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
        defer func() {
            if err := recover(); err != nil {
                log.Printf("panic: %v\n%s", err, debug.Stack())
                http.Error(w, "Internal Server Error", http.StatusInternalServerError)
            }
        }()

        next.ServeHTTP(w, r)
    })
}
```

**Trade-offs**: Prevents crashes but may hide bugs.

### 5. Metrics and Monitoring Middleware

Collect performance metrics:

```go
import "github.com/prometheus/client_golang/prometheus"

var (
    requestDuration = prometheus.NewHistogramVec(
        prometheus.HistogramOpts{
            Name: "http_request_duration_seconds",
            Help: "HTTP request duration",
        },
        []string{"method", "path", "status"},
    )
)

func metricsMiddleware(next http.Handler) http.Handler {
    return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
        start := time.Now()
        rw := &responseWriter{ResponseWriter: w, status: 200}

        next.ServeHTTP(rw, r)

        duration := time.Since(start).Seconds()
        requestDuration.WithLabelValues(
            r.Method,
            r.URL.Path,
            fmt.Sprintf("%d", rw.status),
        ).Observe(duration)
    })
}
```

**Trade-offs**: Valuable observability but adds overhead.

## Common Pitfalls

### 1. Forgetting to Call next.ServeHTTP

**Problem**: Middleware doesn't call the next handler:

```go
// Bad: Handler never executes
func badMiddleware(next http.Handler) http.Handler {
    return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
        log.Println("Request received")
        // Forgot to call next.ServeHTTP(w, r)
    })
}
```

**Solution**: Always call next.ServeHTTP unless intentionally stopping the chain:

```go
// Good: Always call next
func goodMiddleware(next http.Handler) http.Handler {
    return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
        log.Println("Before handler")
        next.ServeHTTP(w, r)
        log.Println("After handler")
    })
}
```

### 2. Wrong Middleware Order

**Problem**: Middleware applied in incorrect order causes issues:

```go
// Bad: Logging after auth means failed auth not logged
handler := chainMiddleware(
    http.HandlerFunc(myHandler),
    authMiddleware,      // Runs second
    loggingMiddleware,   // Runs first
)
```

**Solution**: Order middleware carefully (execution is reverse of declaration):

```go
// Good: Logging wraps auth, so all requests logged
handler := chainMiddleware(
    http.HandlerFunc(myHandler),
    loggingMiddleware,   // Runs first (outermost)
    authMiddleware,      // Runs second (inner)
)

// Execution order:
// 1. loggingMiddleware (before)
// 2. authMiddleware (before)
// 3. myHandler
// 4. authMiddleware (after)
// 5. loggingMiddleware (after)
```

### 3. Modifying Request After Calling next.ServeHTTP

**Problem**: Attempting to modify response after handler executed:

```go
// Bad: WriteHeader after next.ServeHTTP has no effect
func badMiddleware(next http.Handler) http.Handler {
    return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
        next.ServeHTTP(w, r)

        // Too late! Headers already sent
        w.Header().Set("X-Custom", "value")
        w.WriteHeader(http.StatusOK)
    })
}
```

**Solution**: Modify response before calling next.ServeHTTP:

```go
// Good: Set headers before next handler
func goodMiddleware(next http.Handler) http.Handler {
    return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
        w.Header().Set("X-Custom", "value")  // Before next
        next.ServeHTTP(w, r)
    })
}
```

### 4. Using String Keys for Context Values

**Problem**: String context keys can collide:

```go
// Bad: String keys can conflict
func badAuthMiddleware(next http.Handler) http.Handler {
    return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
        ctx := context.WithValue(r.Context(), "userID", 123)
        next.ServeHTTP(w, r.WithContext(ctx))
    })
}

// Another package might use same key!
func otherMiddleware(next http.Handler) http.Handler {
    return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
        ctx := context.WithValue(r.Context(), "userID", "different")
        // ...
    })
}
```

**Solution**: Use unexported type for context keys:

```go
// Good: Unexported type prevents collisions
type contextKey string

const userIDKey contextKey = "userID"

func goodAuthMiddleware(next http.Handler) http.Handler {
    return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
        ctx := context.WithValue(r.Context(), userIDKey, 123)
        next.ServeHTTP(w, r.WithContext(ctx))
    })
}

// Retrieve with same key type
userID := r.Context().Value(userIDKey).(int)
```

### 5. Not Handling Type Assertions

**Problem**: Context value type assertions panic on wrong type:

```go
// Bad: Panics if value is nil or wrong type
userID := r.Context().Value(userIDKey).(int)
```

**Solution**: Check type assertion:

```go
// Good: Safe type assertion
userID, ok := r.Context().Value(userIDKey).(int)
if !ok {
    http.Error(w, "Unauthorized", http.StatusUnauthorized)
    return
}
```

### 6. Blocking Middleware

**Problem**: Slow middleware blocks all requests:

```go
// Bad: Slow operation blocks handler
func badMiddleware(next http.Handler) http.Handler {
    return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
        // Slow external call blocks request
        time.Sleep(5 * time.Second)
        next.ServeHTTP(w, r)
    })
}
```

**Solution**: Use goroutines for async operations or optimize:

```go
// Good: Async logging
func goodLoggingMiddleware(next http.Handler) http.Handler {
    return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
        start := time.Now()

        next.ServeHTTP(w, r)

        // Log asynchronously
        go func() {
            duration := time.Since(start)
            log.Printf("%s %s - %v", r.Method, r.URL.Path, duration)
        }()
    })
}
```

## Related Patterns

**Related Tutorial**: See [Intermediate Tutorial - Web](/en/learn/swe/prog-lang/golang/tutorials/intermediate#web) for middleware fundamentals.

**Related How-To**: See [Build REST APIs](/en/learn/swe/prog-lang/golang/how-to/build-rest-apis) for API middleware patterns, [Use Context Effectively](/en/learn/swe/prog-lang/golang/how-to/use-context-effectively) for context propagation, [Graceful Shutdown](/en/learn/swe/prog-lang/golang/how-to/graceful-shutdown) for cleanup in middleware.

**Related Cookbook**: See Cookbook recipes "HTTP Middleware", "Authentication Middleware", "Logging Middleware" for ready-to-use middleware implementations.
