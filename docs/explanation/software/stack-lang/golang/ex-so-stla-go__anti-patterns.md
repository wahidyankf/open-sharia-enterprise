---
title: Go Anti-Patterns
description: Common mistakes and anti-patterns in Go - what to avoid and why
category: explanation
subcategory: stack-lang
tags:
  - golang
  - anti-patterns
  - common-mistakes
  - goroutine-leaks
  - nil-pointers
  - race-conditions
  - resource-leaks
  - bad-practices
  - go-1.18
  - go-1.21
  - go-1.22
  - go-1.23
  - go-1.24
  - go-1.25
created: 2026-01-22
updated: 2026-01-22
---

# Go Anti-Patterns

**Understanding-oriented guide** to Go anti-patterns - common mistakes, bad practices, and patterns to avoid.

## Table of Contents

1. [Overview](#overview)
2. [Error Handling Anti-Patterns](#error-handling-anti-patterns)
3. [Goroutine Leaks](#goroutine-leaks)
4. [Nil Pointer Dereferences](#nil-pointer-dereferences)
5. [Race Conditions](#race-conditions)
6. [Resource Leaks](#resource-leaks)
7. [Concurrency Anti-Patterns](#concurrency-anti-patterns)
8. [API Design Anti-Patterns](#api-design-anti-patterns)
9. [Performance Anti-Patterns](#performance-anti-patterns)
10. [Testing Anti-Patterns](#testing-anti-patterns)
11. [Security Anti-Patterns](#security-anti-patterns)
12. [Code Organization Anti-Patterns](#code-organization-anti-patterns)
13. [Summary](#summary)
14. [Additional Resources](#additional-resources)

## Overview

Anti-patterns are common solutions to recurring problems that are ineffective, counterproductive, or lead to bugs. Recognizing anti-patterns helps you avoid them and write better Go code.

### Why Learn Anti-Patterns?

```go
// ANTI-PATTERN: Many issues
func GetUser(id int) *User {
    var user *User
    user = db.QueryUser(id)  // What if this fails?
    return user               // Might return nil without indication
}

// CORRECT: Clear error handling
func GetUser(id int) (*User, error) {
    user, err := db.QueryUser(id)
    if err != nil {
        return nil, fmt.Errorf("query user: %w", err)
    }
    return user, nil
}
```

**Learning anti-patterns helps you**:

1. **Avoid bugs** before they happen
2. **Review code** more effectively
3. **Understand trade-offs** of different approaches
4. **Write maintainable code** that others can work with
5. **Debug faster** by recognizing common issues

### Go Version Context

This guide covers Go 1.18-1.25, highlighting version-specific anti-patterns.

## Error Handling Anti-Patterns

### Ignoring Errors

```go
// ANTI-PATTERN: Ignoring errors
func ProcessFile(path string) {
    data, _ := os.ReadFile(path)  // What if it fails?
    _ = ProcessData(data)          // What if processing fails?
    _ = SaveResult(data)           // What if save fails?
}

// CORRECT: Check all errors
func ProcessFile(path string) error {
    data, err := os.ReadFile(path)
    if err != nil {
        return fmt.Errorf("read file: %w", err)
    }

    if err := ProcessData(data); err != nil {
        return fmt.Errorf("process data: %w", err)
    }

    if err := SaveResult(data); err != nil {
        return fmt.Errorf("save result: %w", err)
    }

    return nil
}
```

**Why it's bad**: Silent failures lead to hard-to-debug issues. Users get no indication something went wrong.

**When to ignore errors**: Only when truly intentional and documented:

```go
// Acceptable: Ignoring close error when already handling another error
if err := processFile(f); err != nil {
    _ = f.Close()  // Ignore close error, already have failure
    return err
}
```

### Using Panic for Expected Errors

```go
// ANTI-PATTERN: Panic for expected errors
func ReadConfig(path string) *Config {
    data, err := os.ReadFile(path)
    if err != nil {
        panic(err)  // File might not exist!
    }

    var cfg Config
    if err := json.Unmarshal(data, &cfg); err != nil {
        panic(err)  // JSON might be malformed!
    }

    return &cfg
}

// CORRECT: Return errors
func ReadConfig(path string) (*Config, error) {
    data, err := os.ReadFile(path)
    if err != nil {
        return nil, fmt.Errorf("read config: %w", err)
    }

    var cfg Config
    if err := json.Unmarshal(data, &cfg); err != nil {
        return nil, fmt.Errorf("parse config: %w", err)
    }

    return &cfg, nil
}
```

**Why it's bad**: Panics crash programs. Use panic only for:

- Programmer errors (not runtime errors)
- Initialization failures (init() functions)
- Impossible situations

### Losing Error Context

```go
// ANTI-PATTERN: Losing context
func GetUser(id int) (*User, error) {
    user, err := db.Query(id)
    if err != nil {
        return nil, errors.New("failed")  // What failed? Where?
    }
    return user, nil
}

// ANTI-PATTERN: Breaking error chain
func GetUser(id int) (*User, error) {
    user, err := db.Query(id)
    if err != nil {
        return nil, fmt.Errorf("database error: %v", err)  // %v breaks chain!
    }
    return user, nil
}

// CORRECT: Preserve context and chain
func GetUser(id int) (*User, error) {
    user, err := db.Query(id)
    if err != nil {
        return nil, fmt.Errorf("query user %d: %w", id, err)  // %w preserves chain
    }
    return user, nil
}
```

**Why it's bad**: Lost context makes debugging impossible. Can't identify which database call failed or why.

### Logging And Returning Error

```go
// ANTI-PATTERN: Both logging and returning
func ProcessData(data []byte) error {
    result, err := parse(data)
    if err != nil {
        log.Printf("parse error: %v", err)  // Logging...
        return err                           // ...and returning
    }
    return save(result)
}

// CORRECT: Either log or return
func ProcessData(data []byte) error {
    result, err := parse(data)
    if err != nil {
        return fmt.Errorf("parse data: %w", err)  // Just return
    }
    return save(result)
}

// Log at top level
func main() {
    if err := ProcessData(data); err != nil {
        log.Printf("process data failed: %v", err)  // Log once
    }
}
```

**Why it's bad**: Creates duplicate log entries, making logs noisy and hard to read.

## Goroutine Leaks

Goroutine leaks occur when goroutines don't terminate, consuming memory and resources.

### Blocking Forever on Channel

```go
// ANTI-PATTERN: Goroutine blocks forever
func Process() {
    ch := make(chan int)

    go func() {
        result := expensiveOperation()
        ch <- result  // Blocks forever if no receiver!
    }()

    // Forgot to receive from ch
}

// CORRECT: Always receive
func Process() int {
    ch := make(chan int)

    go func() {
        result := expensiveOperation()
        ch <- result
    }()

    return <-ch  // Receive result
}

// BETTER: Use buffered channel for fire-and-forget
func Process() {
    ch := make(chan int, 1)  // Buffered

    go func() {
        result := expensiveOperation()
        ch <- result  // Won't block even if no receiver
    }()

    // Can return immediately without receiving
}
```

**Why it's bad**: Goroutine never terminates, leaking memory. In long-running programs, this accumulates.

### No Context Cancellation

```go
// ANTI-PATTERN: No way to stop goroutine
func Monitor() {
    go func() {
        for {
            checkStatus()
            time.Sleep(time.Second)
            // No way to stop this loop!
        }
    }()
}

// CORRECT: Use context for cancellation
func Monitor(ctx context.Context) {
    go func() {
        ticker := time.NewTicker(time.Second)
        defer ticker.Stop()

        for {
            select {
            case <-ticker.C:
                checkStatus()
            case <-ctx.Done():
                return  // Clean exit
            }
        }
    }()
}

// Usage
ctx, cancel := context.WithCancel(context.Background())
defer cancel()

Monitor(ctx)
// When done, cancel() stops the goroutine
```

**Why it's bad**: Goroutine runs forever even after it's no longer needed.

### Forgetting to Wait

```go
// ANTI-PATTERN: Main exits before goroutines finish
func main() {
    go processItem(1)
    go processItem(2)
    go processItem(3)
    // Main exits immediately, goroutines killed!
}

// CORRECT: Wait for goroutines
func main() {
    var wg sync.WaitGroup

    for i := 1; i <= 3; i++ {
        wg.Add(1)
        go func(id int) {
            defer wg.Done()
            processItem(id)
        }(i)
    }

    wg.Wait()  // Wait for all to complete
}
```

**Why it's bad**: Goroutines are killed mid-execution when main exits, losing work or leaving inconsistent state.

## Nil Pointer Dereferences

### Not Checking for Nil

```go
// ANTI-PATTERN: Assuming non-nil
func ProcessUser(user *User) {
    fmt.Println(user.Name)  // Panic if user is nil!
}

// CORRECT: Check for nil
func ProcessUser(user *User) error {
    if user == nil {
        return errors.New("user is nil")
    }
    fmt.Println(user.Name)
    return nil
}

// BETTER: Use value type if possible
func ProcessUser(user User) {
    fmt.Println(user.Name)  // Can't be nil
}
```

**Why it's bad**: Nil pointer dereference crashes your program. Always check or use value types.

### Returning Nil Interface

```go
// ANTI-PATTERN: Returning nil interface value
func GetUser(id int) error {
    var err *MyError  // nil *MyError
    // ... some logic ...
    return err  // Returns interface{} containing nil *MyError, not nil interface!
}

func main() {
    if err := GetUser(1); err != nil {  // True! Interface is not nil
        fmt.Println("Error!")  // Executes even though err is nil!
    }
}

// CORRECT: Return nil explicitly
func GetUser(id int) error {
    var err *MyError
    // ... some logic ...
    if err != nil {
        return err
    }
    return nil  // Explicitly return nil interface
}
```

**Why it's bad**: nil interface ≠ interface containing nil pointer. Confusing behavior causes bugs.

### Nil Map or Slice

```go
// ANTI-PATTERN: Using nil map
var m map[string]int  // nil map

m["key"] = 42  // Panic! Can't assign to nil map

// CORRECT: Initialize before use
m := make(map[string]int)
m["key"] = 42

// Nil slice is safe to read but not to write
var s []int  // nil slice
len(s)  // OK: 0
s[0]    // Panic! Index out of range

// But appending works
s = append(s, 1)  // OK, allocates if needed
```

**Why it's bad**: Nil maps panic on write. Nil slices panic on index access.

## Race Conditions

### Concurrent Map Access

```go
// ANTI-PATTERN: Concurrent reads and writes
var cache = make(map[string]string)

func Get(key string) string {
    return cache[key]  // Race!
}

func Set(key, value string) {
    cache[key] = value  // Race!
}

// CORRECT: Use sync.RWMutex
type Cache struct {
    mu   sync.RWMutex
    data map[string]string
}

func (c *Cache) Get(key string) string {
    c.mu.RLock()
    defer c.mu.RUnlock()
    return c.data[key]
}

func (c *Cache) Set(key, value string) {
    c.mu.Lock()
    defer c.mu.Unlock()
    c.data[key] = value
}

// ALTERNATIVE: Use sync.Map
var cache sync.Map

cache.Store("key", "value")
value, ok := cache.Load("key")
```

**Why it's bad**: Concurrent map access crashes with "concurrent map iteration and map write". Use race detector: `go test -race`

### Shared Variable Without Synchronization

```go
// ANTI-PATTERN: Shared counter without sync
var counter int

func Increment() {
    counter++  // Race!
}

// CORRECT: Use sync.Mutex
var (
    counter int
    mu      sync.Mutex
)

func Increment() {
    mu.Lock()
    defer mu.Unlock()
    counter++
}

// BETTER: Use sync/atomic
var counter int64

func Increment() {
    atomic.AddInt64(&counter, 1)
}
```

**Why it's bad**: Data race. Read and write operations aren't atomic, leading to lost updates.

### Loop Variable Capture (Pre-Go 1.22)

```go
// ANTI-PATTERN: Capturing loop variable (Go < 1.22)
for i := 0; i < 10; i++ {
    go func() {
        fmt.Println(i)  // All goroutines see final value (10)!
    }()
}

// CORRECT: Pass as parameter
for i := 0; i < 10; i++ {
    go func(id int) {
        fmt.Println(id)  // Each goroutine gets its own copy
    }(i)
}

// NOTE: Go 1.22+ fixes this automatically
// Each iteration creates new variable
```

**Why it's bad**: All goroutines reference same variable, seeing only final value.

## Resource Leaks

### Not Closing Resources

```go
// ANTI-PATTERN: Forgetting to close
func ReadFile(path string) ([]byte, error) {
    f, err := os.Open(path)
    if err != nil {
        return nil, err
    }
    return io.ReadAll(f)  // File never closed!
}

// CORRECT: Use defer
func ReadFile(path string) ([]byte, error) {
    f, err := os.Open(path)
    if err != nil {
        return nil, err
    }
    defer f.Close()  // Always closes

    return io.ReadAll(f)
}
```

**Why it's bad**: File descriptors are limited. Eventually, OS refuses to open more files.

### Defer in Loop

```go
// ANTI-PATTERN: Defer in loop accumulates
func ProcessFiles(paths []string) error {
    for _, path := range paths {
        f, err := os.Open(path)
        if err != nil {
            return err
        }
        defer f.Close()  // Defers accumulate!

        process(f)
    }
    return nil  // All files close here, not after each iteration
}

// CORRECT: Close in loop or extract function
func ProcessFiles(paths []string) error {
    for _, path := range paths {
        if err := processFile(path); err != nil {
            return err
        }
    }
    return nil
}

func processFile(path string) error {
    f, err := os.Open(path)
    if err != nil {
        return err
    }
    defer f.Close()  // Closes when function returns

    return process(f)
}
```

**Why it's bad**: Files stay open until function returns. With many files, you hit file descriptor limit.

### HTTP Response Body Not Closed

```go
// ANTI-PATTERN: Not closing response body
resp, err := http.Get(url)
if err != nil {
    return err
}
// Body never closed! Connection leak

// CORRECT: Always close body
resp, err := http.Get(url)
if err != nil {
    return err
}
defer resp.Body.Close()

// Must read body to completion for connection reuse
io.Copy(io.Discard, resp.Body)
```

**Why it's bad**: Connection pool exhaustion. Can't make new HTTP requests.

## Concurrency Anti-Patterns

### Starting Too Many Goroutines

```go
// ANTI-PATTERN: Goroutine per item
func ProcessAll(items []Item) {
    for _, item := range items {
        go ProcessItem(item)  // 1 million items = 1 million goroutines!
    }
}

// CORRECT: Worker pool
func ProcessAll(items []Item) {
    const numWorkers = 10
    jobs := make(chan Item, len(items))
    var wg sync.WaitGroup

    // Start workers
    for i := 0; i < numWorkers; i++ {
        wg.Add(1)
        go func() {
            defer wg.Done()
            for item := range jobs {
                ProcessItem(item)
            }
        }()
    }

    // Send jobs
    for _, item := range items {
        jobs <- item
    }
    close(jobs)

    wg.Wait()
}
```

**Why it's bad**: Goroutines aren't free. Too many goroutines waste memory and thrash scheduler.

### Mixing Synchronous and Asynchronous

```go
// ANTI-PATTERN: Inconsistent API
func ProcessA() error {
    // Synchronous
    return doWork()
}

func ProcessB() error {
    // Asynchronous
    go doWork()
    return nil
}

// CORRECT: Consistent API
func ProcessA() error {
    return doWork()  // Both synchronous
}

func ProcessB() error {
    return doWork()
}

// Or both asynchronous with explicit goroutine at call site
go ProcessA()
go ProcessB()
```

**Why it's bad**: Inconsistent API confuses users. Hard to reason about control flow.

### Using Goroutines for Everything

```go
// ANTI-PATTERN: Unnecessary goroutine
func Add(a, b int) int {
    result := make(chan int)
    go func() {
        result <- a + b  // Overkill!
    }()
    return <-result
}

// CORRECT: Just do it
func Add(a, b int) int {
    return a + b  // Goroutines aren't magic
}
```

**Why it's bad**: Goroutines have overhead. Use them for I/O-bound or long-running operations, not simple calculations.

## API Design Anti-Patterns

### Returning Interfaces

```go
// ANTI-PATTERN: Returning interface
func GetConfig() Configurer {
    return &Config{}  // Hard to add methods to Configurer later
}

// CORRECT: Return concrete type
func GetConfig() *Config {
    return &Config{}  // Users can use as Configurer if they want
}

// Accept interfaces, return structs
```

**Why it's bad**: Forces all configs to implement interface. Can't add methods without breaking callers.

### Large Interfaces

```go
// ANTI-PATTERN: Kitchen sink interface
type DataStore interface {
    Create() error
    Read() error
    Update() error
    Delete() error
    List() error
    Search() error
    Count() error
    // ... 20 more methods
}

// CORRECT: Small, focused interfaces
type Reader interface {
    Read() error
}

type Writer interface {
    Write() error
}

type ReadWriter interface {
    Reader
    Writer
}
```

**Why it's bad**: Hard to implement, hard to test, violates Interface Segregation Principle.

### Context as Struct Field

```go
// ANTI-PATTERN: Context in struct
type Server struct {
    ctx context.Context  // Don't do this!
}

// CORRECT: Pass context as first parameter
type Server struct {
    // Other fields
}

func (s *Server) Process(ctx context.Context, data []byte) error {
    // Use ctx
}
```

**Why it's bad**: Context is per-request, not per-struct. Storing it leads to incorrect cancellation.

## Performance Anti-Patterns

### String Concatenation in Loop

```go
// ANTI-PATTERN: String concatenation
var result string
for i := 0; i < 1000; i++ {
    result += strconv.Itoa(i)  // Allocates new string each time
}

// CORRECT: Use strings.Builder
var builder strings.Builder
builder.Grow(1000 * 10)  // Pre-allocate
for i := 0; i < 1000; i++ {
    builder.WriteString(strconv.Itoa(i))
}
result := builder.String()
```

**Why it's bad**: Strings are immutable. Each concatenation creates new string, copying all previous data.

### Growing Slice Dynamically

```go
// ANTI-PATTERN: Dynamic growth
var result []int
for i := 0; i < 1000; i++ {
    result = append(result, i)  // May reallocate multiple times
}

// CORRECT: Pre-allocate
result := make([]int, 0, 1000)
for i := 0; i < 1000; i++ {
    result = append(result, i)  // No reallocation
}
```

**Why it's bad**: Slice grows by doubling. Multiple reallocations copy all data.

### Unnecessary []byte to string Conversion

```go
// ANTI-PATTERN: Converting back and forth
func ProcessBytes(data []byte) []byte {
    s := string(data)  // Allocation
    // Process string
    return []byte(s)   // Another allocation
}

// CORRECT: Work with []byte directly
func ProcessBytes(data []byte) []byte {
    // Process []byte directly
    return data
}
```

**Why it's bad**: Each conversion allocates and copies data.

## Testing Anti-Patterns

### Not Using Table-Driven Tests

```go
// ANTI-PATTERN: Repetitive tests
func TestAdd1(t *testing.T) {
    result := Add(2, 3)
    if result != 5 {
        t.Error("expected 5")
    }
}

func TestAdd2(t *testing.T) {
    result := Add(-1, -2)
    if result != -3 {
        t.Error("expected -3")
    }
}
// ... many more tests

// CORRECT: Table-driven
func TestAdd(t *testing.T) {
    tests := []struct {
        name string
        a, b int
        want int
    }{
        {"positive", 2, 3, 5},
        {"negative", -1, -2, -3},
        {"zero", 0, 0, 0},
    }

    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            got := Add(tt.a, tt.b)
            if got != tt.want {
                t.Errorf("got %d, want %d", got, tt.want)
            }
        })
    }
}
```

**Why it's bad**: Repetitive, hard to maintain, doesn't scale.

### Testing Implementation Instead of Behavior

```go
// ANTI-PATTERN: Testing internal implementation
func TestGetUser(t *testing.T) {
    service := NewService(repo)

    // Testing internal method calls
    if !service.wasValidateCalled {
        t.Error("validate not called")
    }
}

// CORRECT: Test behavior
func TestGetUser(t *testing.T) {
    service := NewService(repo)

    user, err := service.GetUser(1)
    if err != nil {
        t.Fatal(err)
    }

    if user.ID != 1 {
        t.Errorf("got user %d, want 1", user.ID)
    }
}
```

**Why it's bad**: Tests break when refactoring internal implementation. Test behavior, not implementation.

### Global State in Tests

```go
// ANTI-PATTERN: Global state
var db *sql.DB  // Shared across tests

func TestUserService(t *testing.T) {
    service := NewService(db)  // Uses global state
    // Test...
}

// CORRECT: Isolate test state
func TestUserService(t *testing.T) {
    db := setupTestDB(t)  // Each test gets own DB
    defer db.Close()

    service := NewService(db)
    // Test...
}
```

**Why it's bad**: Tests interfere with each other. Hard to run in parallel.

## Security Anti-Patterns

### SQL Injection

```go
// ANTI-PATTERN: String concatenation
query := "SELECT * FROM users WHERE id = " + userID
db.Query(query)  // SQL injection!

// CORRECT: Parameterized query
db.Query("SELECT * FROM users WHERE id = ?", userID)
```

**Why it's bad**: Attacker can inject malicious SQL. Critical security vulnerability.

### Using math/rand for Security

```go
// ANTI-PATTERN: Predictable random
import "math/rand"

token := make([]byte, 32)
rand.Read(token)  // Predictable!

// CORRECT: Cryptographic random
import "crypto/rand"

token := make([]byte, 32)
rand.Read(token)  // Cryptographically secure
```

**Why it's bad**: math/rand is predictable. Attacker can guess tokens.

### Hardcoded Secrets

```go
// ANTI-PATTERN: Hardcoded credentials
const (
    APIKey = "sk-abc123"
    DBPassword = "password123"
)

// CORRECT: Load from environment
apiKey := os.Getenv("API_KEY")
if apiKey == "" {
    log.Fatal("API_KEY not set")
}
```

**Why it's bad**: Secrets in code end up in version control. Anyone with access can see them.

## Code Organization Anti-Patterns

### God Object

```go
// ANTI-PATTERN: God object doing everything
type App struct {
    db          *sql.DB
    cache       *Cache
    logger      *Logger
    httpClient  *http.Client
    // ... 20 more fields
}

func (a *App) HandleRequest() {}
func (a *App) ProcessData() {}
func (a *App) SendEmail() {}
// ... 50 more methods

// CORRECT: Separate concerns
type UserService struct {
    repo Repository
    log  Logger
}

type EmailService struct {
    client *smtp.Client
    log    Logger
}
```

**Why it's bad**: Hard to test, hard to maintain, violates Single Responsibility Principle.

### Util Package

```go
// ANTI-PATTERN: Generic util package
package util

func StringToInt(s string) int
func IntToString(i int) string
func IsEmail(s string) bool
// ... unrelated functions

// CORRECT: Specific packages
package convert

func StringToInt(s string) int
func IntToString(i int) string

package validate

func IsEmail(s string) bool
```

**Why it's bad**: Util becomes dumping ground for unrelated functions. Hard to find and use.

### Circular Dependencies

```go
// ANTI-PATTERN: Circular imports
// package a
import "b"

// package b
import "a"  // Compilation error!

// CORRECT: Introduce interface or third package
// package a
type BInterface interface {
    DoSomething()
}

// package b implements BInterface
// No import of a

// package c coordinates both
import "a"
import "b"
```

**Why it's bad**: Go doesn't allow circular imports. Forces bad architecture.

## Summary

### Most Common Anti-Patterns

1. **Ignoring errors**: Check all error returns
2. **Goroutine leaks**: Use context, close channels
3. **Race conditions**: Use mutexes or channels
4. **Resource leaks**: Use defer for cleanup
5. **Nil pointer dereference**: Check for nil
6. **String concatenation in loop**: Use strings.Builder
7. **Not pre-allocating slices**: Use make with capacity
8. **SQL injection**: Use parameterized queries
9. **Hardcoded secrets**: Use environment variables
10. **Testing implementation**: Test behavior

### How to Avoid Anti-Patterns

1. **Run tools**:
   - `go vet` - Find common mistakes
   - `golangci-lint` - Run many linters
   - `go test -race` - Detect race conditions
2. **Code review**: Fresh eyes catch issues
3. **Learn from examples**: Read standard library code
4. **Test thoroughly**: Tests reveal anti-patterns
5. **Profile**: Don't optimize prematurely, but profile real bottlenecks

### Quick Checklist

Before committing:

- [ ] All errors checked
- [ ] No goroutine leaks
- [ ] All resources closed with defer
- [ ] No race conditions (run with -race)
- [ ] Context used for cancellation
- [ ] Nil checks where needed
- [ ] No panic for expected errors
- [ ] Tests pass
- [ ] golangci-lint clean
- [ ] No hardcoded secrets

## Additional Resources

### Detection Tools

- [go vet](https://pkg.go.dev/cmd/vet) - Built-in analyzer
- [golangci-lint](https://golangci-lint.run/) - Meta-linter
- [staticcheck](https://staticcheck.io/) - Advanced analysis
- [race detector](https://go.dev/doc/articles/race_detector) - Find data races

### Learning Resources

- [Go Code Review Comments](https://github.com/golang/go/wiki/CodeReviewComments)
- [Effective Go](https://go.dev/doc/effective_go)
- [Common Mistakes](https://github.com/golang/go/wiki/CommonMistakes)
- [Go Anti-Patterns (Reddit)](https://www.reddit.com/r/golang/wiki/iamnotexperienced)

### Related Documentation

- [Best Practices](./ex-so-stla-go__best-practices.md) - What to do
- [Go Idioms](./ex-so-stla-go__idioms.md) - Idiomatic patterns
- [Error Handling](./ex-so-stla-go__error-handling.md) - Error patterns
- [Concurrency and Parallelism](./ex-so-stla-go__concurrency-and-parallelism.md) - Concurrency patterns

---

**Navigation**: [← Back to Golang Overview](./README.md)
