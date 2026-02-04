---
title: "Best Practices"
date: 2026-02-04T00:00:00+07:00
draft: false
description: "Go philosophy and idiomatic patterns for production code"
weight: 10000007
tags: ["golang", "best-practices", "idioms", "production"]
---

## Why Go Best Practices Matter

Go's design philosophy centers on simplicity, clarity, and practicality. Unlike languages that offer multiple ways to accomplish tasks, Go deliberately constrains choices to promote consistency across codebases. This "one way to do things" approach makes code review more effective, onboarding faster, and maintenance simpler.

**Core benefits**:

- **Team consistency**: All Go code looks similar, reducing cognitive load
- **Easy code review**: Reviewers recognize patterns instantly
- **Fast onboarding**: New team members read code without surprises
- **Maintainability**: Simple code requires less effort to modify

**Problem**: Without following Go idioms, codebases become inconsistent and difficult to maintain, losing Go's primary advantages.

**Solution**: Learn standard library patterns first, then apply them consistently across production code.

## Standard Library Patterns

### Error Handling Pattern

Go's standard library demonstrates consistent error handling patterns that should be followed in all production code.

**Pattern from standard library**:

```go
package main

import (
    "fmt"
    // => Standard library for formatted I/O
    "os"
    // => Standard library for OS operations
)

func readConfig(filename string) ([]byte, error) {
    // => Returns file contents and error
    // => Second return value is always error
    // => Follows Go convention for error handling

    data, err := os.ReadFile(filename)
    // => data contains file contents (empty if error)
    // => err is nil on success, non-nil on failure
    // => Standard library function returns ([]byte, error)

    if err != nil {
        // => Check error immediately after operation
        // => This is the Go idiom - check errors first
        return nil, fmt.Errorf("failed to read config: %w", err)
        // => nil for data (zero value for []byte)
        // => fmt.Errorf creates formatted error
        // => %w wraps original error for error chains
        // => Caller can inspect original error with errors.Unwrap
    }

    return data, nil
    // => Return data on success
    // => nil error indicates success (Go convention)
}

func main() {
    // => Entry point for demonstration

    data, err := readConfig("config.json")
    // => Call function following error pattern
    // => Always assign error to variable named 'err'

    if err != nil {
        // => Handle error immediately
        // => Don't ignore or defer error checking
        fmt.Fprintf(os.Stderr, "Error: %v\n", err)
        // => Write to stderr (not stdout)
        // => %v prints error message
        os.Exit(1)
        // => Exit with non-zero code
        // => Indicates failure to shell
        return
        // => Unreachable but documents intent
    }

    fmt.Printf("Loaded %d bytes\n", len(data))
    // => Only executed if no error
    // => Happy path after error check
}
```

**Key principles**:

1. **Return errors, don't panic**: Use `error` return value, not `panic()`
2. **Check immediately**: Verify error right after operation
3. **Wrap with context**: Use `fmt.Errorf("context: %w", err)` to add information
4. **Don't ignore**: Never use `_` for errors unless explicitly justified

### Interface Design from Standard Library

Go's `io` package demonstrates the power of small, focused interfaces.

**Standard library pattern**:

```go
package main

import (
    "bytes"
    // => Standard library for byte buffers
    "io"
    // => Standard library for I/O interfaces
    "os"
    // => Standard library for OS operations
)

// LogWriter writes log messages to any destination
// => Interface follows standard library style
// => Small and focused on one capability
type LogWriter interface {
    // => Embedding io.Writer (composition)
    // => Reuses standard library interface
    io.Writer
}

// Logger writes formatted log messages
// => Struct depends on interface, not concrete type
// => Follows dependency inversion principle
type Logger struct {
    writer LogWriter
    // => writer can be file, buffer, network socket
    // => Any type implementing io.Writer works
}

func NewLogger(w LogWriter) *Logger {
    // => Constructor pattern (Go convention)
    // => Accepts interface for flexibility
    return &Logger{writer: w}
    // => Returns pointer (common for structs with methods)
}

func (l *Logger) Log(message string) error {
    // => Method receiver (pointer for mutation)
    // => Returns error following Go convention

    _, err := l.writer.Write([]byte(message + "\n"))
    // => l.writer is interface, Write is from io.Writer
    // => []byte() converts string to bytes
    // => Newline added for line-oriented logging
    // => First return (bytes written) ignored with _
    // => err is nil on success, non-nil on failure

    if err != nil {
        // => Always check error from Write
        // => Standard library Write can fail
        return fmt.Errorf("log write failed: %w", err)
        // => Wrap error with context
    }

    return nil
    // => nil indicates success
}

func main() {
    // => Demonstration of interface flexibility

    // Log to buffer (in-memory)
    // => bytes.Buffer implements io.Writer
    buffer := &bytes.Buffer{}
    // => Heap allocation (address taken)
    logger1 := NewLogger(buffer)
    // => logger1 writes to memory buffer
    logger1.Log("Test message")
    // => Message stored in buffer

    // Log to file (disk)
    // => os.File implements io.Writer
    file, err := os.Create("app.log")
    // => Create or truncate file
    // => Returns (*os.File, error)
    if err != nil {
        // => Handle file creation error
        panic(err)
        // => panic() only for unrecoverable errors
        // => Not idiomatic for normal errors
    }
    defer file.Close()
    // => Ensures file closed when main() exits
    // => defer executes in reverse order
    // => Common pattern for resource cleanup

    logger2 := NewLogger(file)
    // => logger2 writes to disk file
    // => Same Logger type, different destination
    logger2.Log("Production message")
    // => Message written to app.log
}
```

**Key principles**:

1. **Accept interfaces, return concrete types**: Function parameters use interfaces for flexibility
2. **Keep interfaces small**: 1-3 methods per interface (often just 1)
3. **Use standard interfaces**: Prefer `io.Reader`, `io.Writer`, `io.Closer` over custom interfaces
4. **Compose interfaces**: Embed smaller interfaces to build larger ones

### Composition Over Inheritance

Go doesn't have inheritance. Instead, it uses struct embedding for composition.

**Standard library approach**:

```go
package main

import (
    "fmt"
    // => Standard library for formatted I/O
    "sync"
    // => Standard library for synchronization primitives
)

// Counter tracks a count with thread safety
// => Embeds sync.Mutex for synchronization
// => No inheritance, pure composition
type Counter struct {
    sync.Mutex
    // => Embedded field (anonymous field)
    // => Mutex methods become Counter methods
    // => Promotes Lock() and Unlock() to Counter
    value int
    // => Named field for counter value
    // => Unexported (lowercase) - private
}

func (c *Counter) Increment() {
    // => Pointer receiver required for mutation
    // => Method modifies Counter state

    c.Lock()
    // => Calls embedded Mutex.Lock()
    // => Promoted method from embedded field
    // => Blocks if another goroutine holds lock

    defer c.Unlock()
    // => Ensures unlock even if panic occurs
    // => defer runs when Increment() exits
    // => Unlocks mutex after function completes

    c.value++
    // => Increment protected by mutex
    // => Goroutine-safe operation
    // => Only one goroutine can increment at a time
}

func (c *Counter) Value() int {
    // => Value receiver acceptable (read-only)
    // => Returns copy of counter value

    c.Lock()
    // => Lock before reading (prevents race)
    // => Even reads need protection in Go
    // => Concurrent reads without lock cause data race

    defer c.Unlock()
    // => Unlock after reading value
    // => defer guarantees unlock

    return c.value
    // => Returns protected value
    // => Mutex ensures consistent read
}

func main() {
    // => Demonstrates composition pattern

    counter := &Counter{}
    // => Initialize Counter with zero value
    // => Embedded Mutex ready to use (no initialization needed)
    // => Zero value of Mutex is valid unlocked state

    var wg sync.WaitGroup
    // => WaitGroup for goroutine coordination
    // => Tracks number of active goroutines
    // => Zero value ready to use

    for i := 0; i < 10; i++ {
        // => Launch 10 concurrent goroutines
        // => Demonstrates thread-safe counter

        wg.Add(1)
        // => Increment WaitGroup counter
        // => Must call before launching goroutine
        // => Tracks one more goroutine to wait for

        go func() {
            // => Anonymous goroutine
            // => Runs concurrently with main
            defer wg.Done()
            // => Decrement WaitGroup when goroutine exits
            // => Signals completion to main

            counter.Increment()
            // => Thread-safe increment
            // => Mutex prevents race conditions
            // => Each goroutine safely increments
        }()
    }

    wg.Wait()
    // => Block until all goroutines complete
    // => Waits for WaitGroup counter to reach zero
    // => Ensures all increments finish

    fmt.Printf("Final value: %d\n", counter.Value())
    // => Output: Final value: 10
    // => All 10 increments completed safely
    // => No race conditions due to mutex
}
```

**Key principles**:

1. **Embed for reuse**: Embed structs to promote their methods
2. **No inheritance**: Use composition instead of subclassing
3. **Zero values work**: Embedded types initialize to valid zero values
4. **Pointer receivers for mutation**: Use `*Type` when modifying state

## Production Patterns

### Functional Options Pattern

Functional options provide flexible, backward-compatible configuration.

**Pattern**:

```go
package main

import (
    "fmt"
    // => Standard library for formatted I/O
    "time"
    // => Standard library for time operations
)

// ServerOption configures a Server
// => Function type for configuration
// => Returns nothing, modifies Server directly
type ServerOption func(*Server)
// => func(*Server) is function signature
// => Takes Server pointer, returns nothing
// => Used as variadic parameter

// Server represents HTTP server configuration
// => Contains optional configuration fields
type Server struct {
    host    string
    port    int
    timeout time.Duration
    // => Optional fields with sensible defaults
}

// WithHost sets server host
// => Returns ServerOption function
// => Follows "With" prefix convention
func WithHost(host string) ServerOption {
    // => host parameter captured by closure
    return func(s *Server) {
        // => Returns function that modifies Server
        // => Closure captures host from outer function
        s.host = host
        // => Modifies Server when option applied
    }
}

// WithPort sets server port
// => Same pattern as WithHost
func WithPort(port int) ServerOption {
    return func(s *Server) {
        s.port = port
    }
}

// WithTimeout sets server timeout
// => Same pattern for optional configuration
func WithTimeout(timeout time.Duration) ServerOption {
    return func(s *Server) {
        s.timeout = timeout
    }
}

// NewServer creates configured Server
// => Variadic options for flexible configuration
// => Default values for all fields
func NewServer(options ...ServerOption) *Server {
    // => options is []ServerOption slice
    // => ...ServerOption accepts variable arguments
    // => Can pass 0 or more options

    server := &Server{
        // => Initialize with sensible defaults
        // => All fields have default values
        host:    "localhost",
        port:    8080,
        timeout: 30 * time.Second,
        // => 30 * time.Second is 30 seconds
    }

    for _, option := range options {
        // => Iterate through provided options
        // => _ ignores index, option is function
        option(server)
        // => Apply each option to server
        // => Calls function with server pointer
        // => Modifies server configuration
    }

    return server
    // => Returns fully configured server
    // => Options override defaults
}

func (s *Server) Start() {
    // => Simplified server start for demonstration
    fmt.Printf("Starting server on %s:%d (timeout: %v)\n",
        s.host, s.port, s.timeout)
    // => %s for string, %d for int, %v for value
    // => Demonstrates final configuration
}

func main() {
    // => Demonstrates flexibility of options pattern

    // Use all defaults
    // => No options provided
    s1 := NewServer()
    // => s1 uses localhost:8080 with 30s timeout
    s1.Start()
    // => Output: Starting server on localhost:8080 (timeout: 30s)

    // Override specific options
    // => Pass only options you want to change
    s2 := NewServer(
        WithHost("0.0.0.0"),
        // => Override host, keep other defaults
        WithPort(9000),
        // => Override port, keep other defaults
    )
    // => Timeout remains default (30s)
    s2.Start()
    // => Output: Starting server on 0.0.0.0:9000 (timeout: 30s)

    // Override all options
    s3 := NewServer(
        WithHost("api.example.com"),
        WithPort(443),
        WithTimeout(60 * time.Second),
        // => All fields customized
    )
    s3.Start()
    // => Output: Starting server on api.example.com:443 (timeout: 1m0s)
}
```

**Benefits**:

1. **Backward compatible**: Adding new options doesn't break existing calls
2. **Self-documenting**: `WithTimeout(30)` clearer than positional argument
3. **Optional parameters**: Only specify what you need to change
4. **Type safe**: Compiler verifies option types

### Effective Error Wrapping

Error wrapping preserves error context while adding information.

**Pattern**:

```go
package main

import (
    "errors"
    // => Standard library for error handling
    "fmt"
    // => Standard library for formatted I/O
    "os"
    // => Standard library for OS operations
)

// Define sentinel errors for comparison
// => Sentinel errors are package-level variables
// => Used with errors.Is() for error checking
var (
    ErrInvalidInput = errors.New("invalid input")
    // => Public error (exported)
    // => Can be checked by callers
    ErrNotFound = errors.New("not found")
    // => Another sentinel error
    // => Distinct from ErrInvalidInput
)

// processFile demonstrates error wrapping chain
// => Shows how errors propagate up call stack
func processFile(filename string) error {
    // => Returns error or nil
    // => Single return value (error only)

    if filename == "" {
        // => Validate input before processing
        return fmt.Errorf("filename required: %w", ErrInvalidInput)
        // => Wrap sentinel error with context
        // => %w makes ErrInvalidInput inspectable
        // => Caller can use errors.Is(err, ErrInvalidInput)
    }

    data, err := os.ReadFile(filename)
    // => Attempt to read file
    // => Returns ([]byte, error)
    if err != nil {
        // => File read failed
        return fmt.Errorf("failed to read %s: %w", filename, err)
        // => Wrap os error with context
        // => Adds filename to error message
        // => Preserves original error for inspection
    }

    if len(data) == 0 {
        // => Validate file content
        return fmt.Errorf("file %s is empty: %w", filename, ErrNotFound)
        // => Wrap sentinel error with context
        // => Indicates file exists but unusable
    }

    return nil
    // => Success - no error
}

func main() {
    // => Demonstrates error inspection

    // Test with empty filename
    // => Triggers validation error
    err := processFile("")
    // => err wraps ErrInvalidInput

    if err != nil {
        // => Error occurred
        fmt.Println("Error:", err)
        // => Output: Error: filename required: invalid input
        // => Full error message with context

        if errors.Is(err, ErrInvalidInput) {
            // => Check if error is/wraps ErrInvalidInput
            // => errors.Is() unwraps error chain
            // => Works through multiple layers of wrapping
            fmt.Println("⚠ Input validation failed")
            // => Output: ⚠ Input validation failed
            // => Specific handling for validation errors
        }
    }

    // Test with nonexistent file
    err = processFile("missing.txt")
    // => err wraps os.ErrNotExist

    if err != nil {
        fmt.Println("Error:", err)
        // => Output: Error: failed to read missing.txt: no such file or directory

        if errors.Is(err, os.ErrNotExist) {
            // => Check for specific OS error
            // => Works despite error wrapping
            fmt.Println("⚠ File does not exist")
            // => Output: ⚠ File does not exist
            // => Can suggest creating file
        }
    }
}
```

**Key principles**:

1. **Wrap with %w**: Use `fmt.Errorf("context: %w", err)` to wrap errors
2. **Add context**: Include relevant variables (filename, ID) in error message
3. **Use errors.Is**: Check wrapped errors with `errors.Is(err, target)`
4. **Sentinel errors**: Define package-level errors for common cases

## Comparison Table

| Aspect                | Non-Idiomatic Approach                 | Go Best Practice                                    |
| --------------------- | -------------------------------------- | --------------------------------------------------- |
| **Error handling**    | Exceptions, try-catch                  | Explicit error returns, immediate checking          |
| **Inheritance**       | Class hierarchies                      | Composition via struct embedding                    |
| **Configuration**     | Many constructor parameters            | Functional options pattern                          |
| **Interfaces**        | Large interfaces (10+ methods)         | Small interfaces (1-3 methods)                      |
| **Dependencies**      | Depend on concrete types               | Accept interfaces, return concrete types            |
| **Concurrency**       | Shared memory with locks everywhere    | Share memory by communicating (channels preferred)  |
| **Error information** | Stack traces in errors                 | Error wrapping with context                         |
| **Nil handling**      | Null pointer exceptions surprise you   | Explicit nil checks before dereferencing            |
| **Resource cleanup**  | try-finally or RAII                    | defer statements                                    |
| **Code organization** | Deep package hierarchies               | Flat package structure                              |
| **Initialization**    | Constructors, dependency injection     | New() functions, functional options                 |
| **Method receivers**  | this/self implicit                     | Explicit receiver (value or pointer)                |
| **Visibility**        | public/private/protected keywords      | Exported (uppercase) vs unexported (lowercase)      |
| **Generics usage**    | Used extensively everywhere            | Used sparingly when type safety critical            |
| **Documentation**     | Separate documentation files           | Doc comments above declarations                     |
| **Testing**           | Test frameworks with complex setup     | Standard library testing package                    |
| **Package naming**    | Long descriptive names                 | Short, lowercase, single-word names                 |
| **Variable naming**   | camelCase or snake_case inconsistently | camelCase consistently, short names in small scopes |
| **Comments**          | Inline comments explaining code        | Code is clear, comments explain why                 |
| **File organization** | One public type per file               | Related types in same file                          |

## When to Apply Each Pattern

**Error handling pattern**:

- **Always**: Every function that can fail should return error
- **Immediate check**: Check error right after operation
- **Wrap for context**: Add information at each layer

**Interface design**:

- **Public APIs**: Accept interfaces for flexibility
- **Internal code**: Concrete types acceptable
- **Standard library interfaces**: Prefer `io.Reader`/`io.Writer` over custom

**Composition**:

- **Sharing behavior**: Embed types to promote methods
- **Synchronization**: Embed `sync.Mutex` for thread-safe structs
- **Delegation**: Embed to delegate method calls

**Functional options**:

- **Many optional parameters**: More than 3 optional configurations
- **Extensible APIs**: Public libraries needing backward compatibility
- **Default values**: When sensible defaults exist

**Error wrapping**:

- **Library boundaries**: Wrap errors at package boundaries
- **Add context**: Include variables that help debugging
- **Preserve errors**: Use `%w` to maintain error chain
