---
title: "Anti Patterns"
date: 2026-02-04T00:00:00+07:00
draft: false
description: "Common mistakes and anti-patterns to avoid in Go"
weight: 1000014
tags: ["golang", "anti-patterns", "mistakes", "goroutine-leaks"]
---

## Why Anti-Patterns Matter

Learning anti-patterns is as critical as learning best practices. While best practices show the right way, anti-patterns reveal common traps that even experienced developers fall into. Recognizing these patterns during code review prevents bugs from reaching production where they cause goroutine leaks, race conditions, and system crashes.

**Core benefits**:

- **Prevention**: Recognize patterns before writing them
- **Code review effectiveness**: Spot problems quickly during reviews
- **Debugging speed**: Identify root causes faster
- **Production reliability**: Fewer critical bugs reach users

**Problem**: Anti-patterns cause subtle bugs that manifest intermittently in production, making them expensive and difficult to debug.

**Solution**: Learn common anti-patterns and detection tools to prevent them during development.

## Critical Anti-Patterns

### Goroutine Leaks

**Problem**: Launching goroutines without proper lifecycle management causes them to accumulate and never terminate, leading to memory exhaustion and eventually system crashes.

**Recognition signals**:

- Memory usage grows continuously over time
- Number of goroutines increases without bound
- Application becomes slower as uptime increases
- Eventually crashes with out-of-memory errors

**Why this fails**:

- Each goroutine consumes memory (2KB+ stack)
- Thousands of leaked goroutines exhaust available memory
- No garbage collection for stuck goroutines
- System becomes unresponsive before crashing

**Problem code**:

```go
package main

import (
    "fmt"
    // => Standard library for formatted I/O
    "time"
    // => Standard library for time operations
)

// ProcessRequest launches goroutine without lifecycle management
// => ANTI-PATTERN: Goroutine leak
func ProcessRequest(id int) {
    // => id identifies the request

    go func() {
        // => Anonymous goroutine
        // => Launched without control mechanism
        // => No way to stop or signal completion

        fmt.Printf("Processing request %d\n", id)
        // => Simulated processing work
        // => %d formats integer

        time.Sleep(10 * time.Minute)
        // => PROBLEM: Blocks for 10 minutes
        // => Goroutine cannot be stopped
        // => If thousands launched, all wait indefinitely
        // => Consumes memory for entire duration

        fmt.Printf("Request %d complete\n", id)
        // => Never reached if program exits early
        // => No cleanup or resource release
    }()
    // => Function returns immediately
    // => Goroutine runs independently
    // => No mechanism to cancel or track it
    // => LEAK: Goroutine persists until sleep completes
}

func main() {
    // => Demonstrates goroutine leak

    for i := 0; i < 1000; i++ {
        // => Launch 1000 requests
        ProcessRequest(i)
        // => Each creates unmanaged goroutine
        // => All 1000 goroutines leak
        // => 1000 * 2KB = 2MB+ memory leaked minimum
    }

    time.Sleep(1 * time.Second)
    // => main exits after 1 second
    // => All 1000 goroutines still running
    // => No cleanup occurs
    // => Program exits, goroutines orphaned
}
```

**Why this leaks**:

- Goroutine blocks for 10 minutes with no cancellation mechanism
- No way to signal goroutine to stop early
- If main exits, goroutines are abandoned (not cleaned up)
- Each goroutine allocates stack memory that persists

**Fix with context cancellation**:

```go
package main

import (
    "context"
    // => Standard library for cancellation and deadlines
    "fmt"
    // => Standard library for formatted I/O
    "time"
    // => Standard library for time operations
)

// ProcessRequest handles request with cancellation support
// => SOLUTION: Goroutine with lifecycle management
func ProcessRequest(ctx context.Context, id int) {
    // => ctx provides cancellation signal
    // => Caller controls goroutine lifetime

    go func() {
        // => Anonymous goroutine with context

        fmt.Printf("Processing request %d\n", id)
        // => Start processing

        select {
        // => Multiplexes multiple channel operations
        // => Blocks until one case can proceed
        // => Whichever channel operation completes first

        case <-time.After(10 * time.Minute):
            // => time.After returns channel that receives after duration
            // => Normal completion case (10 minutes elapsed)
            fmt.Printf("Request %d complete\n", id)
            // => Work finished naturally

        case <-ctx.Done():
            // => ctx.Done() returns channel closed on cancellation
            // => Cancellation case (context cancelled)
            // => Immediately unblocks when context cancelled
            fmt.Printf("Request %d cancelled: %v\n", id, ctx.Err())
            // => ctx.Err() explains cancellation reason
            // => Could be DeadlineExceeded or Canceled
            // => Goroutine exits promptly
            return
            // => Exit goroutine immediately
            // => Stack memory released
            // => No leak
        }
    }()
    // => Function returns immediately
    // => Goroutine runs with cancellation support
}

func main() {
    // => Demonstrates proper goroutine management

    ctx, cancel := context.WithCancel(context.Background())
    // => ctx is cancellable context
    // => cancel is function to trigger cancellation
    // => context.Background() is root context (never cancelled)
    // => WithCancel creates derived context that CAN be cancelled

    defer cancel()
    // => Ensures cancel called when main exits
    // => Signals all goroutines to stop
    // => Prevents leaks even if main panics
    // => defer runs even on early returns

    for i := 0; i < 1000; i++ {
        // => Launch 1000 requests with context
        ProcessRequest(ctx, i)
        // => Each goroutine respects cancellation
        // => All receive cancellation signal via ctx
    }

    time.Sleep(1 * time.Second)
    // => Simulate some work in main
    // => After 1 second, main will exit
    // => defer cancel() triggers cancellation
    // => All goroutines exit promptly via <-ctx.Done()
    // => NO LEAK: Goroutines cleaned up

    fmt.Println("Main exiting, all goroutines will be cancelled")
    // => When main exits, defer cancel() runs
    // => All 1000 goroutines receive cancellation
    // => They exit immediately, releasing resources
}
```

**Key fixes**:

1. **Accept context.Context**: Pass cancellation signal to goroutines
2. **Use select with ctx.Done()**: Check cancellation in blocking operations
3. **Defer cancel()**: Always call cancel() to release resources
4. **Prompt cleanup**: Exit goroutine immediately on cancellation

### Ignoring Errors

**Problem**: Silently ignoring errors with blank identifier `_` causes bugs to propagate through the system, leading to corrupt data or crashes far from the actual error source.

**Recognition signals**:

- Use of `_` for error return values
- Nil pointer dereferences downstream
- Corrupt data in database
- Crashes in unrelated code

**Problem code**:

```go
package main

import (
    "encoding/json"
    // => Standard library for JSON encoding/decoding
    "fmt"
    // => Standard library for formatted I/O
)

type User struct {
    // => User data structure
    Name  string `json:"name"`
    // => json:"name" is struct tag for JSON field mapping
    Email string `json:"email"`
}

func ParseUser(data []byte) *User {
    // => ANTI-PATTERN: Ignores error
    // => Returns *User even if parsing fails

    var user User
    // => user initialized to zero value
    // => Name and Email are empty strings

    _ = json.Unmarshal(data, &user)
    // => PROBLEM: Error ignored with _
    // => json.Unmarshal may fail (invalid JSON)
    // => Error discarded without checking
    // => user remains partially initialized on failure
    // => No indication parsing failed

    return &user
    // => Returns pointer to potentially invalid user
    // => Caller has no way to know parsing failed
    // => Zero values returned on error (empty strings)
    // => BUG: Caller assumes valid user
}

func main() {
    // => Demonstrates silent error propagation

    invalidJSON := []byte(`{"name": "John"`)
    // => Invalid JSON (missing closing brace)
    // => json.Unmarshal will fail

    user := ParseUser(invalidJSON)
    // => ParseUser returns user with zero values
    // => No error returned or logged
    // => main has no indication of failure

    fmt.Printf("User: %s (%s)\n", user.Name, user.Email)
    // => Output: User: John ()
    // => Email empty because JSON incomplete
    // => Looks like valid user with missing email
    // => BUG: Cannot distinguish from intentional empty email
    // => Corrupt data propagates into system

    // Later code assumes valid email
    // => Would crash or fail silently
    // => Far from original error (parsing)
    // => Difficult to debug
}
```

**Why this fails**:

- Parsing errors go undetected
- Zero values look like valid data
- Bugs appear far from error source
- No way to distinguish valid empty strings from parsing failures

**Fix with proper error handling**:

```go
package main

import (
    "encoding/json"
    // => Standard library for JSON encoding/decoding
    "fmt"
    // => Standard library for formatted I/O
)

type User struct {
    // => User data structure
    Name  string `json:"name"`
    Email string `json:"email"`
}

func ParseUser(data []byte) (*User, error) {
    // => SOLUTION: Returns error for caller to handle
    // => Explicit error in function signature

    var user User
    // => user initialized to zero value

    err := json.Unmarshal(data, &user)
    // => err assigned (not ignored)
    // => json.Unmarshal returns error on invalid JSON

    if err != nil {
        // => Check error immediately
        // => Go idiom: check errors before proceeding
        return nil, fmt.Errorf("failed to parse user: %w", err)
        // => nil for user (no valid data)
        // => Wrap error with context using %w
        // => Caller receives wrapped error
        // => Can inspect with errors.Is or errors.As
    }

    return &user, nil
    // => Return valid user on success
    // => nil error indicates success
    // => Go convention: error is last return value
}

func main() {
    // => Demonstrates proper error handling

    invalidJSON := []byte(`{"name": "John"`)
    // => Invalid JSON (missing closing brace)

    user, err := ParseUser(invalidJSON)
    // => Receive both user and error
    // => Go convention: check error before using value

    if err != nil {
        // => Handle error explicitly
        // => Error detected at parse site
        fmt.Printf("Error: %v\n", err)
        // => Output: Error: failed to parse user: unexpected end of JSON input
        // => %v formats error message
        // => Clear indication of problem
        return
        // => Exit early on error
        // => Don't proceed with invalid data
    }

    fmt.Printf("User: %s (%s)\n", user.Name, user.Email)
    // => Only reached if parsing succeeded
    // => user guaranteed valid
    // => No risk of nil pointer dereference
    // => No corrupt data in system
}
```

**Key fixes**:

1. **Return errors**: Add `error` to return signature
2. **Check immediately**: Verify error after every operation
3. **Early return**: Exit on error, don't proceed
4. **Wrap with context**: Use `fmt.Errorf("context: %w", err)`

### Race Conditions

**Problem**: Multiple goroutines accessing shared memory without synchronization causes race conditions where reads and writes interleave unpredictably, leading to corrupt data and non-deterministic bugs.

**Recognition signals**:

- Race detector warnings (`go run -race`)
- Intermittent bugs that disappear with debugging
- Different results on different runs
- Crashes with corrupt memory

**Problem code**:

```go
package main

import (
    "fmt"
    // => Standard library for formatted I/O
    "sync"
    // => Standard library for synchronization primitives
)

type Counter struct {
    // => ANTI-PATTERN: No synchronization for shared state
    value int
    // => Shared mutable state
    // => Multiple goroutines access without protection
}

func (c *Counter) Increment() {
    // => PROBLEM: Non-atomic read-modify-write
    c.value++
    // => value++ is three operations:
    // => 1. Read current value
    // => 2. Add 1
    // => 3. Write new value
    // => Race: Another goroutine can interleave between steps
    // => Both goroutines read same value, both write same new value
    // => Result: Increment lost
}

func (c *Counter) Value() int {
    // => PROBLEM: Unprotected read during concurrent writes
    return c.value
    // => May return partially written value
    // => May return value in middle of increment
    // => Race detector catches this
}

func main() {
    // => Demonstrates race condition

    counter := &Counter{}
    // => Shared counter accessed by multiple goroutines
    // => No synchronization mechanism

    var wg sync.WaitGroup
    // => WaitGroup for goroutine coordination
    // => Only for waiting, not for synchronization

    for i := 0; i < 1000; i++ {
        // => Launch 1000 concurrent goroutines
        wg.Add(1)
        // => Track goroutine for waiting

        go func() {
            // => Anonymous goroutine
            defer wg.Done()
            // => Signal completion

            counter.Increment()
            // => RACE: Unprotected concurrent write
            // => Multiple goroutines modify value simultaneously
            // => Increments lost due to race
        }()
    }

    wg.Wait()
    // => Wait for all goroutines to complete
    // => Ensures all increments attempted (not guaranteed successful)

    fmt.Printf("Expected: 1000, Got: %d\n", counter.Value())
    // => Output varies: Expected: 1000, Got: 987 (for example)
    // => Got < 1000 due to lost increments
    // => Different result on every run
    // => RACE: Reading during concurrent writes
    // => go run -race detects this
}
```

**Why this fails**:

- `value++` is not atomic (read-modify-write)
- Multiple goroutines read same value before writing
- Increments lost when goroutines interleave
- Result depends on timing (non-deterministic)

**Fix with mutex synchronization**:

```go
package main

import (
    "fmt"
    // => Standard library for formatted I/O
    "sync"
    // => Standard library for synchronization primitives
)

type Counter struct {
    // => SOLUTION: Protected shared state
    mu    sync.Mutex
    // => Mutex protects value field
    // => Only one goroutine can hold lock at a time
    value int
    // => Shared mutable state protected by mu
}

func (c *Counter) Increment() {
    // => SOLUTION: Atomic increment with mutex
    c.mu.Lock()
    // => Acquire lock before accessing value
    // => Blocks if another goroutine holds lock
    // => Only one goroutine in critical section

    defer c.mu.Unlock()
    // => Release lock when function exits
    // => defer ensures unlock even if panic occurs
    // => Critical: Always unlock to prevent deadlock

    c.value++
    // => Protected increment
    // => No other goroutine can access value
    // => Read-modify-write completes atomically
    // => No race condition
}

func (c *Counter) Value() int {
    // => SOLUTION: Protected read
    c.mu.Lock()
    // => Lock before reading value
    // => Prevents reading during write
    // => Even reads need protection in Go

    defer c.mu.Unlock()
    // => Unlock after reading
    // => defer ensures cleanup

    return c.value
    // => Return protected value
    // => Guaranteed consistent read
    // => No torn reads or partial writes
}

func main() {
    // => Demonstrates synchronized counter

    counter := &Counter{}
    // => Counter with embedded mutex
    // => Zero value of mutex is valid unlocked state

    var wg sync.WaitGroup
    // => WaitGroup for goroutine coordination

    for i := 0; i < 1000; i++ {
        // => Launch 1000 concurrent goroutines
        wg.Add(1)

        go func() {
            defer wg.Done()

            counter.Increment()
            // => NO RACE: Mutex protects access
            // => Each increment completes atomically
            // => All 1000 increments succeed
        }()
    }

    wg.Wait()
    // => Wait for all goroutines to complete

    fmt.Printf("Expected: 1000, Got: %d\n", counter.Value())
    // => Output: Expected: 1000, Got: 1000
    // => Result always 1000 (deterministic)
    // => NO RACE: Read protected by mutex
    // => go run -race reports no races
}
```

**Key fixes**:

1. **Add sync.Mutex**: Protect shared state with mutex field
2. **Lock before access**: Call `Lock()` before read/write
3. **Defer unlock**: Use `defer Unlock()` to guarantee release
4. **Protect all access**: Both reads and writes need locks

### Nil Pointer Dereference

**Problem**: Dereferencing nil pointers causes panics that crash the program, often due to forgotten error checks or assumptions about initialization.

**Recognition signals**:

- Panic: "runtime error: invalid memory address or nil pointer dereference"
- Crashes in production
- Missing error checks before pointer use
- Uninitialized pointer fields

**Problem code**:

```go
package main

import (
    "encoding/json"
    // => Standard library for JSON encoding/decoding
    "fmt"
    // => Standard library for formatted I/O
)

type Config struct {
    // => Configuration with nested pointer
    Database *DatabaseConfig
    // => Pointer field (can be nil)
}

type DatabaseConfig struct {
    Host string
    Port int
}

func LoadConfig(data []byte) *Config {
    // => ANTI-PATTERN: Returns nil on error without indication
    var config Config
    // => config initialized with zero values
    // => Database field is nil (zero value of pointer)

    if err := json.Unmarshal(data, &config); err != nil {
        // => Parsing failed
        return nil
        // => PROBLEM: Returns nil without explicit error
        // => Caller must remember to check nil
        // => Easy to forget
    }

    return &config
    // => Returns valid config
    // => Or config with nil Database field if JSON incomplete
}

func main() {
    // => Demonstrates nil pointer dereference

    invalidJSON := []byte(`{"database": null}`)
    // => JSON with null database field
    // => Valid JSON, but Database will be nil

    config := LoadConfig(invalidJSON)
    // => config is &Config{Database: nil}
    // => No error returned or checked

    // PROBLEM: Dereference without nil check
    fmt.Printf("Database host: %s\n", config.Database.Host)
    // => PANIC: nil pointer dereference
    // => config.Database is nil
    // => .Host access dereferences nil pointer
    // => Runtime panic: invalid memory address
    // => Program crashes
}
```

**Why this fails**:

- No explicit error return for nil config
- Pointer fields can be nil even in "valid" config
- Easy to forget nil checks before dereferencing
- Panics crash entire program

**Fix with explicit error handling and nil checks**:

```go
package main

import (
    "encoding/json"
    // => Standard library for JSON encoding/decoding
    "errors"
    // => Standard library for error handling
    "fmt"
    // => Standard library for formatted I/O
)

type Config struct {
    Database *DatabaseConfig
    // => Pointer field (can be nil)
}

type DatabaseConfig struct {
    Host string
    Port int
}

func LoadConfig(data []byte) (*Config, error) {
    // => SOLUTION: Returns explicit error
    var config Config

    if err := json.Unmarshal(data, &config); err != nil {
        // => Parsing failed
        return nil, fmt.Errorf("failed to parse config: %w", err)
        // => Explicit error return
        // => Caller must handle error
        // => Type system enforces check
    }

    if config.Database == nil {
        // => SOLUTION: Validate critical fields
        // => Check nil before caller uses it
        return nil, errors.New("database configuration required")
        // => Fail fast on invalid config
        // => Prevents nil pointer dereference later
    }

    return &config, nil
    // => Return valid config with non-nil Database
}

func main() {
    // => Demonstrates safe pointer handling

    invalidJSON := []byte(`{"database": null}`)
    // => JSON with null database field

    config, err := LoadConfig(invalidJSON)
    // => SOLUTION: Receives explicit error
    // => Type system forces error check

    if err != nil {
        // => Handle error before using config
        fmt.Printf("Error: %v\n", err)
        // => Output: Error: database configuration required
        // => Clear error message
        // => No panic, graceful handling
        return
        // => Exit early, don't use invalid config
    }

    // Safe: config.Database guaranteed non-nil
    fmt.Printf("Database host: %s\n", config.Database.Host)
    // => Only reached if config valid
    // => config.Database guaranteed non-nil
    // => No panic possible
}
```

**Key fixes**:

1. **Return explicit errors**: Add `error` return value
2. **Validate nil pointers**: Check critical pointer fields
3. **Fail fast**: Return error for nil required fields
4. **Check before dereference**: Verify non-nil before accessing fields

## Detection Tools

**Race detector**:

```bash
go run -race main.go       # Run with race detection
go test -race ./...        # Test with race detection
go build -race             # Build with race detection
```

- Detects unsynchronized concurrent access
- Reports source file and line numbers
- May have false negatives (doesn't catch all races)
- Zero false positives (reported races are real)

**Vet tool**:

```bash
go vet ./...               # Static analysis for common mistakes
```

- Checks for common mistakes (nil dereference, format errors)
- Catches some error handling issues
- Fast (no runtime overhead)
- Run as part of CI/CD pipeline

**Staticcheck**:

```bash
staticcheck ./...          # Advanced static analysis
```

- More thorough than `go vet`
- Catches subtle bugs and anti-patterns
- Install: `go install honnef.co/go/tools/cmd/staticcheck@latest`

## Summary

**Critical anti-patterns to avoid**:

1. **Goroutine leaks**: Use `context.Context` for cancellation
2. **Ignoring errors**: Always check error return values
3. **Race conditions**: Protect shared state with `sync.Mutex`
4. **Nil pointer dereference**: Validate pointers before dereferencing

**Detection workflow**:

1. Run `go vet ./...` on every commit
2. Run `go test -race ./...` before deployment
3. Use `staticcheck` in CI/CD pipeline
4. Review code for patterns in this guide

**Prevention practices**:

- Enable race detector during development (`go run -race`)
- Return explicit errors (avoid nil returns without error)
- Use context.Context for goroutine lifecycle
- Lock all shared state (reads and writes)
- Validate all pointer fields before dereferencing
- Run static analysis tools in CI/CD
