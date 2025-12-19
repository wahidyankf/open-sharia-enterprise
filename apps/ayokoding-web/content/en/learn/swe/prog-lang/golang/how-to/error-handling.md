---
title: Error Handling Best Practices
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 629
description: Effective error handling strategies in Go using errors, wrapping, and sentinel errors
tags: ["golang", "go", "error-handling", "best-practices"]
---

## Basic Error Handling

Always check errors immediately and handle them appropriately.

```go
package main

import (
    "fmt"
    "os"
)

func readFile(filename string) error {
    file, err := os.Open(filename)
    if err != nil {
        return err
    }
    defer file.Close()

    // Process file...
    return nil
}

func main() {
    if err := readFile("data.txt"); err != nil {
        fmt.Fprintf(os.Stderr, "Error: %v\n", err)
        os.Exit(1)
    }
}
```

## Error Wrapping (Go 1.13+)

Add context to errors using `%w` verb with `fmt.Errorf`.

```go
package main

import (
    "errors"
    "fmt"
)

func processData(id string) error {
    err := fetchData(id)
    if err != nil {
        return fmt.Errorf("failed to process data for %s: %w", id, err)
    }
    return nil
}

func fetchData(id string) error {
    if id == "" {
        return errors.New("empty id")
    }
    // Fetch logic...
    return nil
}

func main() {
    err := processData("")
    if err != nil {
        fmt.Println(err)
        // Output: failed to process data for : empty id

        // Unwrap to check original error
        if errors.Is(err, errors.New("empty id")) {
            fmt.Println("Invalid ID provided")
        }
    }
}
```

## Sentinel Errors

Define package-level errors for specific conditions.

```go
package database

import (
    "errors"
    "fmt"
)

var (
    ErrNotFound     = errors.New("record not found")
    ErrDuplicate    = errors.New("duplicate record")
    ErrInvalidInput = errors.New("invalid input")
)

type DB struct{}

func (db *DB) Find(id string) error {
    if id == "" {
        return ErrInvalidInput
    }
    // Database lookup...
    return ErrNotFound
}

func main() {
    db := &DB{}
    err := db.Find("")

    // Check for specific error
    if errors.Is(err, ErrInvalidInput) {
        fmt.Println("Please provide valid ID")
    } else if errors.Is(err, ErrNotFound) {
        fmt.Println("Record doesn't exist")
    }
}
```

## Custom Error Types

Create custom error types with additional context.

```go
package main

import (
    "fmt"
)

// ValidationError with field-level details
type ValidationError struct {
    Field   string
    Message string
}

func (e *ValidationError) Error() string {
    return fmt.Sprintf("validation failed on %s: %s", e.Field, e.Message)
}

// DatabaseError with query details
type DatabaseError struct {
    Query string
    Err   error
}

func (e *DatabaseError) Error() string {
    return fmt.Sprintf("database error (query: %s): %v", e.Query, e.Err)
}

func (e *DatabaseError) Unwrap() error {
    return e.Err
}

// Usage
func validateUser(name string) error {
    if name == "" {
        return &ValidationError{
            Field:   "name",
            Message: "cannot be empty",
        }
    }
    return nil
}

func main() {
    err := validateUser("")
    if err != nil {
        if ve, ok := err.(*ValidationError); ok {
            fmt.Printf("Field: %s, Issue: %s\n", ve.Field, ve.Message)
        }
    }
}
```

## errors.Is vs errors.As

Use `errors.Is` for sentinel errors and `errors.As` for error types.

```go
package main

import (
    "errors"
    "fmt"
    "os"
)

var ErrPermission = errors.New("permission denied")

type NetworkError struct {
    Code int
}

func (e *NetworkError) Error() string {
    return fmt.Sprintf("network error: %d", e.Code)
}

func doOperation() error {
    return fmt.Errorf("operation failed: %w", &NetworkError{Code: 500})
}

func main() {
    err := doOperation()

    // errors.Is - check for sentinel error
    if errors.Is(err, ErrPermission) {
        fmt.Println("No permission")
    }

    // errors.As - extract error type
    var netErr *NetworkError
    if errors.As(err, &netErr) {
        fmt.Printf("Network error with code: %d\n", netErr.Code)
    }

    // Check for os.PathError
    _, err2 := os.Open("nonexistent.txt")
    var pathErr *os.PathError
    if errors.As(err2, &pathErr) {
        fmt.Printf("Path: %s, Op: %s\n", pathErr.Path, pathErr.Op)
    }
}
```

## Panic and Recover

Reserve panic for truly exceptional situations. Use recover sparingly.

```go
package main

import (
    "fmt"
    "log"
)

func riskyOperation() {
    defer func() {
        if r := recover(); r != nil {
            fmt.Println("Recovered from panic:", r)
        }
    }()

    // Code that might panic
    panic("something went wrong")
}

// Good use case: critical initialization
func mustConnect(dsn string) *DB {
    db, err := connect(dsn)
    if err != nil {
        panic(fmt.Sprintf("failed to connect to database: %v", err))
    }
    return db
}

func connect(dsn string) (*DB, error) {
    // Connection logic
    return &DB{}, nil
}

type DB struct{}

func main() {
    // Recover from panic
    riskyOperation()
    fmt.Println("Program continues")

    // Panic on critical failure
    db := mustConnect("postgres://...")
    _ = db
}
```

## Error Handling in HTTP Handlers

Centralized error handling for HTTP endpoints.

```go
package main

import (
    "encoding/json"
    "errors"
    "fmt"
    "log"
    "net/http"
)

type AppError struct {
    Error   error
    Message string
    Code    int
}

type AppHandler func(w http.ResponseWriter, r *http.Request) *AppError

func (fn AppHandler) ServeHTTP(w http.ResponseWriter, r *http.Request) {
    if err := fn(w, r); err != nil {
        log.Printf("Error: %v", err.Error)

        w.Header().Set("Content-Type", "application/json")
        w.WriteHeader(err.Code)
        json.NewEncoder(w).Encode(map[string]string{
            "error": err.Message,
        })
    }
}

func getUser(w http.ResponseWriter, r *http.Request) *AppError {
    id := r.URL.Query().Get("id")
    if id == "" {
        return &AppError{
            Error:   errors.New("missing id parameter"),
            Message: "User ID is required",
            Code:    http.StatusBadRequest,
        }
    }

    // Fetch user logic...
    user := map[string]string{"id": id, "name": "John"}
    json.NewEncoder(w).Encode(user)
    return nil
}

func main() {
    http.Handle("/user", AppHandler(getUser))
    fmt.Println("Server starting on :8080")
    log.Fatal(http.ListenAndServe(":8080", nil))
}
```

## Multi-Error Pattern

Collect multiple errors before returning.

```go
package main

import (
    "errors"
    "fmt"
    "strings"
)

type MultiError []error

func (m MultiError) Error() string {
    var msgs []string
    for _, err := range m {
        msgs = append(msgs, err.Error())
    }
    return strings.Join(msgs, "; ")
}

func validateForm(name, email string, age int) error {
    var errs MultiError

    if name == "" {
        errs = append(errs, errors.New("name is required"))
    }
    if email == "" {
        errs = append(errs, errors.New("email is required"))
    }
    if age < 18 {
        errs = append(errs, errors.New("must be 18 or older"))
    }

    if len(errs) > 0 {
        return errs
    }
    return nil
}

func main() {
    err := validateForm("", "", 15)
    if err != nil {
        fmt.Println("Validation errors:", err)
        // Output: Validation errors: name is required; email is required; must be 18 or older
    }
}
```

## Error Logging Best Practices

Log errors with sufficient context.

```go
package main

import (
    "context"
    "fmt"
    "log"
)

// Structured logging helper
func logError(ctx context.Context, err error, msg string, fields map[string]interface{}) {
    log.Printf("[ERROR] %s: %v | Context: %+v", msg, err, fields)
}

func processRequest(ctx context.Context, userID string) error {
    err := fetchData(userID)
    if err != nil {
        logError(ctx, err, "Failed to fetch data", map[string]interface{}{
            "userID":   userID,
            "function": "processRequest",
        })
        return fmt.Errorf("process request failed: %w", err)
    }
    return nil
}

func fetchData(userID string) error {
    if userID == "" {
        return fmt.Errorf("invalid user ID")
    }
    return nil
}

func main() {
    ctx := context.Background()
    if err := processRequest(ctx, ""); err != nil {
        fmt.Println("Request failed:", err)
    }
}
```

## Error Handling Patterns

Common patterns for different scenarios.

```go
package main

import (
    "errors"
    "fmt"
)

// Pattern 1: Early return
func validateInput(input string) error {
    if input == "" {
        return errors.New("empty input")
    }
    if len(input) > 100 {
        return errors.New("input too long")
    }
    return nil
}

// Pattern 2: Named return error
func divide(a, b float64) (result float64, err error) {
    if b == 0 {
        err = errors.New("division by zero")
        return
    }
    result = a / b
    return
}

// Pattern 3: Multiple return values
func parseConfig(filename string) (config map[string]string, warnings []string, err error) {
    config = make(map[string]string)
    warnings = []string{}

    if filename == "" {
        err = errors.New("filename required")
        return
    }

    // Parsing logic with warnings...
    warnings = append(warnings, "deprecated field found")
    return
}

func main() {
    // Early return
    if err := validateInput(""); err != nil {
        fmt.Println("Validation failed:", err)
    }

    // Named return
    result, err := divide(10, 0)
    if err != nil {
        fmt.Println("Division error:", err)
    } else {
        fmt.Println("Result:", result)
    }

    // Multiple returns
    cfg, warns, err := parseConfig("config.yml")
    if err != nil {
        fmt.Println("Config error:", err)
    } else {
        fmt.Println("Config loaded with warnings:", warns)
        _ = cfg
    }
}
```

## Related Resources

**Learn more**:

- [Cheat Sheet](/en/learn/swe/prog-lang/golang/reference/cheat-sheet) - Error handling syntax
- [Best Practices](/en/learn/swe/prog-lang/golang/explanation/best-practices) - Error handling guidelines
- [Resources](/en/learn/swe/prog-lang/golang/reference/resources) - Error handling books
