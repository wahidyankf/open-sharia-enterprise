---
title: "Error Handling"
date: 2026-02-04T00:00:00+07:00
draft: false
description: "Error interface, error wrapping, errors.Is/As, and sentinel errors in Go"
weight: 1000026
tags: ["golang", "error-handling", "errors", "sentinel-errors", "production"]
---

## Why Error Handling Matters

Go's explicit error handling prevents hidden control flow and forces developers to handle errors at each step. Understanding the error interface, error wrapping (Go 1.13+), errors.Is/As, and sentinel errors prevents error information loss and enables robust error handling in production systems.

**Core benefits**:

- **Explicit handling**: Errors visible at call site (no hidden exceptions)
- **Error context**: Wrapping preserves error chain with context
- **Type safety**: errors.As enables type-specific error handling
- **Sentinel matching**: errors.Is checks error identity through wrapping

**Problem**: Before Go 1.13, error wrapping lost original errors, preventing proper error identification. Manual string formatting destroyed error semantics.

**Solution**: Start with error interface and errors.New(), understand limitations (no wrapping), then use fmt.Errorf with %w and errors.Is/As for production error handling.

## Standard Library: Error Interface

Go's error type is an interface with single method.

**Pattern from standard library**:

```go
package main

import (
    "errors"
    // => Standard library for error creation
    // => errors.New creates error from string
    "fmt"
    // => Standard library for formatted output
)

// ERROR INTERFACE (from standard library):
// type error interface {
//     Error() string
// }
// => Any type with Error() string implements error
// => Simple interface enables flexible error types

func divide(a, b float64) (float64, error) {
    // => Returns result and error
    // => Second return value is error (convention)
    // => error is nil on success, non-nil on failure

    if b == 0 {
        // => Division by zero detected

        return 0, errors.New("division by zero")
        // => errors.New creates error from string
        // => Returns error interface
        // => Result is zero value (ignored on error)
    }

    return a / b, nil
    // => Success: return result and nil error
    // => nil indicates no error
}

func main() {
    result, err := divide(10, 2)
    // => err is nil (success case)

    if err != nil {
        // => Check error before using result
        // => Go convention: check error immediately

        fmt.Println("Error:", err)
        // => Won't execute (err is nil)
        return
    }

    fmt.Println("Result:", result)
    // => Output: Result: 5
    // => Only reached if err is nil

    // Error case
    result2, err2 := divide(10, 0)
    // => err2 is non-nil (error case)

    if err2 != nil {
        // => err2 is errors.errorString ("division by zero")

        fmt.Println("Error:", err2)
        // => Output: Error: division by zero
        // => err2.Error() called implicitly by fmt

        return
        // => Exit on error (result2 invalid)
    }

    fmt.Println("Result:", result2)
    // => Won't execute (returned above)
}
```

**Custom error types**:

```go
package main

import "fmt"

// Custom error type
type ValidationError struct {
    Field   string
    // => Field that failed validation
    Message string
    // => Error message
}

func (e *ValidationError) Error() string {
    // => Implements error interface
    // => Required method for error type

    return fmt.Sprintf("validation error on %s: %s", e.Field, e.Message)
    // => Formats error message
    // => Output: validation error on email: invalid format
}

func validateEmail(email string) error {
    // => Returns error interface
    // => Actual type is *ValidationError

    if email == "" {
        // => Empty email detected

        return &ValidationError{
            Field:   "email",
            Message: "cannot be empty",
        }
        // => Returns custom error type
        // => Satisfies error interface (*ValidationError has Error method)
    }

    if !contains(email, "@") {
        // => Basic email validation

        return &ValidationError{
            Field:   "email",
            Message: "invalid format",
        }
    }

    return nil
    // => Validation passed
}

func contains(s, substr string) bool {
    // => Helper function (simplified)
    for i := 0; i <= len(s)-len(substr); i++ {
        if s[i:i+len(substr)] == substr {
            return true
        }
    }
    return false
}

func main() {
    err := validateEmail("invalid")
    // => err is *ValidationError (concrete type)

    if err != nil {
        fmt.Println(err)
        // => Output: validation error on email: invalid format
        // => Calls Error() method
    }
}
```

**Limitations before Go 1.13**:

- No standard error wrapping (information lost)
- Cannot check if error is specific type after wrapping
- String concatenation destroys error semantics
- No error chain inspection

## Production Framework: Error Wrapping (Go 1.13+)

Go 1.13 introduced error wrapping with fmt.Errorf %w verb and errors.Is/As for error inspection.

**Pattern: Error Wrapping with %w**:

```go
package main

import (
    "errors"
    // => Standard library errors package
    // => errors.Is/As for error inspection
    "fmt"
    // => fmt.Errorf with %w for wrapping
)

var ErrNotFound = errors.New("not found")
// => SENTINEL ERROR: predefined error
// => Exported (capitalized) for external use
// => Used for error identity checking

func fetchUser(id string) error {
    // => Simulates database fetch
    // => Returns wrapped error on failure

    // Simulate database error
    if id == "invalid" {
        // => User not found case

        return fmt.Errorf("failed to fetch user %s: %w", id, ErrNotFound)
        // => %w wraps ErrNotFound (preserves error chain)
        // => Adds context ("failed to fetch user invalid")
        // => errors.Is can inspect wrapped error
        // => CRITICAL: %w not %v (only %w wraps)
    }

    return nil
    // => Success case
}

func processUser(id string) error {
    // => Calls fetchUser and adds more context

    err := fetchUser(id)
    // => err might be wrapped ErrNotFound

    if err != nil {
        // => Error occurred in fetchUser

        return fmt.Errorf("processUser: %w", err)
        // => Wrap again with additional context
        // => Error chain: processUser → fetchUser → ErrNotFound
        // => Each layer adds context
    }

    return nil
}

func main() {
    err := processUser("invalid")
    // => err is wrapped error chain

    if err != nil {
        fmt.Println("Error:", err)
        // => Output: Error: processUser: failed to fetch user invalid: not found
        // => Full error chain printed

        // CHECK ERROR IDENTITY with errors.Is
        if errors.Is(err, ErrNotFound) {
            // => errors.Is unwraps error chain
            // => Checks if any error in chain is ErrNotFound
            // => Returns true even through multiple wraps

            fmt.Println("User not found!")
            // => Output: User not found!
            // => Specific handling for not found case
        }
    }
}
```

**Pattern: Error Type Inspection with errors.As**:

```go
package main

import (
    "errors"
    // => Standard library for errors.As
    "fmt"
)

// Custom error type with additional fields
type NetworkError struct {
    StatusCode int
    // => HTTP status code
    URL        string
    // => URL that failed
    Err        error
    // => Underlying error (wrapped)
}

func (e *NetworkError) Error() string {
    // => Implements error interface

    return fmt.Sprintf("network error %d for %s: %v", e.StatusCode, e.URL, e.Err)
    // => Formatted error message
}

func (e *NetworkError) Unwrap() error {
    // => Unwrap enables errors.Is/As to inspect chain
    // => Returns wrapped error
    // => CRITICAL: implement for error wrapping to work

    return e.Err
}

func fetchData(url string) error {
    // => Simulates HTTP request

    if url == "http://example.com/error" {
        // => Simulate 404 error

        return &NetworkError{
            StatusCode: 404,
            URL:        url,
            Err:        errors.New("page not found"),
        }
        // => Returns custom error with context
    }

    return nil
}

func main() {
    err := fetchData("http://example.com/error")
    // => err is *NetworkError

    if err != nil {
        // => Error occurred

        var netErr *NetworkError
        // => Declare variable for error type
        // => Must be pointer to match error type

        if errors.As(err, &netErr) {
            // => errors.As checks if err is *NetworkError
            // => Unwraps error chain to find matching type
            // => Assigns found error to netErr
            // => Returns true if found

            fmt.Printf("Network error: status=%d, url=%s\n",
                netErr.StatusCode, netErr.URL)
            // => Output: Network error: status=404, url=http://example.com/error
            // => Access NetworkError-specific fields

            if netErr.StatusCode == 404 {
                // => Type-specific handling
                fmt.Println("Resource not found")
                // => Output: Resource not found
            }
        }
    }
}
```

**Pattern: Sentinel Errors vs Custom Error Types**:

```go
package main

import (
    "errors"
    "fmt"
)

// SENTINEL ERRORS: for simple cases
var (
    ErrInvalidInput = errors.New("invalid input")
    // => Predefined error (identity checking)
    ErrTimeout      = errors.New("operation timed out")
    // => Simple error without additional data
    ErrUnauthorized = errors.New("unauthorized")
)

// CUSTOM ERROR TYPE: when additional context needed
type DatabaseError struct {
    Query     string
    // => SQL query that failed
    Table     string
    // => Table name
    Err       error
    // => Underlying error
}

func (e *DatabaseError) Error() string {
    return fmt.Sprintf("database error on table %s: %v (query: %s)",
        e.Table, e.Err, e.Query)
}

func (e *DatabaseError) Unwrap() error {
    return e.Err
}

func queryUser(id string) error {
    // => Simulates database query

    if id == "" {
        // => Input validation

        return fmt.Errorf("queryUser: %w", ErrInvalidInput)
        // => Sentinel error wrapped with context
        // => Simple case: no additional fields needed
    }

    if id == "timeout" {
        // => Simulate database timeout

        return &DatabaseError{
            Query: "SELECT * FROM users WHERE id = ?",
            Table: "users",
            Err:   ErrTimeout,
        }
        // => Custom error with context
        // => Additional fields (Query, Table)
    }

    return nil
}

func main() {
    // Sentinel error case
    err1 := queryUser("")
    if errors.Is(err1, ErrInvalidInput) {
        // => errors.Is checks sentinel error
        fmt.Println("Invalid input detected")
        // => Output: Invalid input detected
    }

    // Custom error type case
    err2 := queryUser("timeout")

    var dbErr *DatabaseError
    if errors.As(err2, &dbErr) {
        // => errors.As extracts custom error type

        fmt.Printf("Database error: table=%s, query=%s\n",
            dbErr.Table, dbErr.Query)
        // => Output: Database error: table=users, query=SELECT * FROM users WHERE id = ?
        // => Access error-specific context

        if errors.Is(dbErr.Err, ErrTimeout) {
            // => Check wrapped sentinel error
            fmt.Println("Query timed out - retry later")
            // => Output: Query timed out - retry later
        }
    }
}
```

## Trade-offs: When to Use Each

**Comparison table**:

| Approach            | Context Preservation | Type Safety  | Use Case                      |
| ------------------- | -------------------- | ------------ | ----------------------------- |
| **errors.New()**    | None                 | Runtime      | Simple errors (no wrapping)   |
| **fmt.Errorf %w**   | Full chain           | Runtime      | Wrapping with context         |
| **Sentinel errors** | Identity             | Compile-time | Expected errors (ErrNotFound) |
| **Custom types**    | Rich context         | Compile-time | Errors with additional data   |

**When to use errors.New()**:

- Simple error messages
- No error wrapping needed
- Leaf errors (not wrapped further)
- Quick error returns

**When to use fmt.Errorf with %w**:

- Adding context to existing errors
- Building error chains through call stack
- Preserving original error for errors.Is/As
- Most common pattern in production

**When to use sentinel errors**:

- Expected errors with known identity (ErrNotFound, ErrTimeout)
- API boundaries (exported errors)
- Error equality checks
- Simple cases without additional context

**When to use custom error types**:

- Errors with additional fields (StatusCode, URL, Query)
- Type-specific error handling
- Rich error context for debugging
- Complex error cases

## Production Best Practices

**Always wrap errors with context**:

```go
// GOOD: wrap with %w (preserves chain)
if err := fetchUser(id); err != nil {
    return fmt.Errorf("failed to fetch user %s: %w", id, err)
}

// BAD: lose error chain with %v
if err := fetchUser(id); err != nil {
    return fmt.Errorf("failed to fetch user %s: %v", id, err)
    // %v converts to string (loses error identity)
}
```

**Use errors.Is for sentinel error checking**:

```go
// GOOD: errors.Is unwraps chain
if errors.Is(err, ErrNotFound) {
    // Handle not found
}

// BAD: direct comparison fails with wrapping
if err == ErrNotFound {
    // Won't match if err is wrapped
}
```

**Use errors.As for type inspection**:

```go
// GOOD: errors.As extracts type from chain
var netErr *NetworkError
if errors.As(err, &netErr) {
    // Use netErr fields
}

// BAD: type assertion fails with wrapping
if netErr, ok := err.(*NetworkError); ok {
    // Won't work if err is wrapped
}
```

**Implement Unwrap() for custom error types**:

```go
// GOOD: Unwrap enables errors.Is/As
type MyError struct {
    Err error
}

func (e *MyError) Unwrap() error {
    return e.Err  // Enable error chain inspection
}

// BAD: no Unwrap (breaks error chain)
type MyError struct {
    Err error
}
// errors.Is/As won't inspect wrapped errors
```

**Don't panic for expected errors**:

```go
// GOOD: return error
func parseConfig(data []byte) (*Config, error) {
    if len(data) == 0 {
        return nil, ErrInvalidConfig
    }
    // ...
}

// BAD: panic for expected errors
func parseConfig(data []byte) *Config {
    if len(data) == 0 {
        panic("invalid config")  // Don't panic
    }
    // ...
}
```

## Summary

Go's explicit error handling forces errors to be visible at call sites. Standard library provides error interface and errors.New() for simple errors. Go 1.13+ adds error wrapping with fmt.Errorf %w, errors.Is for identity checking, and errors.As for type inspection. Use sentinel errors for simple cases, custom types for rich context, and always wrap errors to preserve error chains.

**Key takeaways**:

- error is interface with Error() string method
- Wrap errors with fmt.Errorf %w to preserve chain
- Use errors.Is to check sentinel errors through wrapping
- Use errors.As to extract custom error types
- Implement Unwrap() for custom error types
- Return errors (don't panic) for expected failures
