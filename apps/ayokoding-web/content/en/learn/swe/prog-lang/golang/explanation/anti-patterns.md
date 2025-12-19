---
title: "Common Go Anti-Patterns"
date: 2025-12-17T10:00:00+07:00
draft: false
weight: 704
description: "Recognizing and avoiding common Go anti-patterns that lead to bugs and unmaintainable code"
tags: ["golang", "anti-patterns", "code-smells", "refactoring"]
---

## Overview

Anti-patterns in Go often arise from applying patterns from other languages or misunderstanding Go's idioms. Recognizing these patterns helps you write cleaner, more maintainable Go code that works with the language rather than against it.

## Goroutine Anti-Patterns

### Goroutine Leaks

Starting goroutines that never exit, consuming resources indefinitely.

**Why it's bad:**

- Memory leaks as goroutines accumulate
- Goroutine count grows unbounded
- Hard to debug (manifests slowly)
- Eventually crashes application

**Example:**

```go
// ❌ Goroutine leak - generator never exits
func GenerateNumbers() <-chan int {
  out := make(chan int)

  go func() {
    for i := 0; ; i++ {
      out <- i // Blocks forever if receiver stops reading
    }
  }()

  return out
}

// Consumer reads only 5 numbers
func main() {
  numbers := GenerateNumbers()
  for i := 0; i < 5; i++ {
    fmt.Println(<-numbers)
  }
  // Generator goroutine still running, leaking resources!
}

// ✅ Use context for cancellation
func GenerateNumbers(ctx context.Context) <-chan int {
  out := make(chan int)

  go func() {
    defer close(out)
    for i := 0; ; i++ {
      select {
      case out <- i:
        // Sent successfully
      case <-ctx.Done():
        return // Clean exit when context cancelled
      }
    }
  }()

  return out
}

// Consumer can cancel when done
func main() {
  ctx, cancel := context.WithCancel(context.Background())
  defer cancel() // Goroutine exits when we're done

  numbers := GenerateNumbers(ctx)
  for i := 0; i < 5; i++ {
    fmt.Println(<-numbers)
  }
  // Goroutine cleans up properly
}
```

**More leak examples:**

```go
// ❌ Leak - HTTP request goroutine never cancelled
func FetchData(url string) (*Data, error) {
  result := make(chan *Data)
  go func() {
    resp, _ := http.Get(url) // Hangs if request takes too long
    result <- parseResponse(resp)
  }()
  return <-result, nil
}

// ✅ Use context with timeout
func FetchData(ctx context.Context, url string) (*Data, error) {
  result := make(chan *Data, 1) // Buffered to prevent goroutine leak

  go func() {
    req, _ := http.NewRequestWithContext(ctx, "GET", url, nil)
    resp, err := http.DefaultClient.Do(req)
    if err != nil {
      return
    }
    result <- parseResponse(resp)
  }()

  select {
  case data := <-result:
    return data, nil
  case <-ctx.Done():
    return nil, ctx.Err() // Timeout or cancellation
  }
}
```

### Unbuffered Channels Causing Deadlocks

Using unbuffered channels without coordinating sender and receiver.

**Why it's bad:**

- Goroutines block forever waiting for each other
- Causes deadlock panics
- Subtle bugs in error handling paths
- Hard to debug without understanding channel mechanics

**Example:**

```go
// ❌ Deadlock - sender blocks, no receiver
func ProcessItems(items []Item) {
  results := make(chan Result) // Unbuffered

  for _, item := range items {
    go func(i Item) {
      result := process(i)
      results <- result // Blocks until someone receives
    }(item)
  }

  // If we don't receive from results, goroutines block forever
}

// ❌ Deadlock - only reads partial results
func ProcessItems(items []Item) []Result {
  results := make(chan Result) // Unbuffered

  for _, item := range items {
    go func(i Item) {
      results <- process(i) // Last goroutine blocks forever
    }(item)
  }

  // Only read first result, rest blocked
  return []Result{<-results}
}

// ✅ Buffer matches number of sends
func ProcessItems(items []Item) []Result {
  results := make(chan Result, len(items)) // Buffered

  for _, item := range items {
    go func(i Item) {
      results <- process(i) // Never blocks - buffer sized correctly
    }(item)
  }

  output := make([]Result, len(items))
  for i := 0; i < len(items); i++ {
    output[i] = <-results
  }

  return output
}

// ✅ Alternative: Use WaitGroup + slice
func ProcessItems(items []Item) []Result {
  results := make([]Result, len(items))
  var wg sync.WaitGroup

  for i, item := range items {
    wg.Add(1)
    go func(idx int, itm Item) {
      defer wg.Done()
      results[idx] = process(itm)
    }(i, item)
  }

  wg.Wait()
  return results
}
```

### Ignoring Goroutine Errors

Spawning goroutines without capturing their errors.

**Why it's bad:**

- Errors disappear silently
- No way to know if operation failed
- Makes debugging impossible
- Violates fail-fast principle

**Example:**

```go
// ❌ Errors lost in goroutines
func ProcessMany(items []Item) {
  for _, item := range items {
    go func(i Item) {
      err := process(i)
      if err != nil {
        // Error lost - no one sees it!
        log.Println(err)
      }
    }(item)
  }
}

// ✅ Collect errors from goroutines
func ProcessMany(items []Item) error {
  errs := make(chan error, len(items))

  for _, item := range items {
    go func(i Item) {
      errs <- process(i) // Send error (or nil)
    }(item)
  }

  var firstErr error
  for i := 0; i < len(items); i++ {
    if err := <-errs; err != nil && firstErr == nil {
      firstErr = err
    }
  }

  return firstErr
}

// ✅ Use errgroup for coordinated error handling
import "golang.org/x/sync/errgroup"

func ProcessMany(ctx context.Context, items []Item) error {
  g, ctx := errgroup.WithContext(ctx)

  for _, item := range items {
    item := item // Capture for goroutine
    g.Go(func() error {
      return process(item) // Returns error to errgroup
    })
  }

  return g.Wait() // Returns first error encountered
}
```

## Error Handling Anti-Patterns

### Ignoring Errors

Using `_` to discard errors without handling them.

**Why it's bad:**

- Silent failures corrupt data
- Impossible to debug when things go wrong
- Violates Go's explicit error handling philosophy
- Makes code unreliable

**Example:**

```go
// ❌ Ignoring errors - silent failures
func SaveUser(user *User) {
  data, _ := json.Marshal(user) // Might fail
  _ = os.WriteFile("user.json", data, 0644) // Might fail
  // Caller thinks it succeeded!
}

// ✅ Handle errors properly
func SaveUser(user *User) error {
  data, err := json.Marshal(user)
  if err != nil {
    return fmt.Errorf("marshaling user: %w", err)
  }

  err = os.WriteFile("user.json", data, 0644)
  if err != nil {
    return fmt.Errorf("writing file: %w", err)
  }

  return nil
}

// ❌ Ignoring errors in defers
func ProcessFile(path string) error {
  f, err := os.Open(path)
  if err != nil {
    return err
  }
  defer f.Close() // Ignoring Close error

  // Process file...
  return nil
}

// ✅ Check defer errors when they matter
func ProcessFile(path string) (err error) {
  f, err := os.Open(path)
  if err != nil {
    return err
  }
  defer func() {
    closeErr := f.Close()
    if err == nil {
      err = closeErr // Preserve close error if no other error
    }
  }()

  // Process file...
  return nil
}
```

### Panic for Normal Errors

Using panic/recover for control flow instead of returning errors.

**Why it's bad:**

- Panics are expensive (stack unwinding)
- Makes error handling invisible
- Violates caller expectations
- Hard to test and debug

**Example:**

```go
// ❌ Using panic for normal errors
func FindUser(id string) *User {
  user, err := db.Query(id)
  if err != nil {
    panic(fmt.Sprintf("user not found: %s", id)) // Don't panic!
  }
  return user
}

// Caller forced to use recover
func HandleRequest(w http.ResponseWriter, r *http.Request) {
  defer func() {
    if r := recover(); r != nil {
      http.Error(w, fmt.Sprint(r), 500)
    }
  }()

  user := FindUser(r.URL.Query().Get("id"))
  // ...
}

// ✅ Return errors normally
func FindUser(id string) (*User, error) {
  user, err := db.Query(id)
  if err != nil {
    return nil, fmt.Errorf("finding user %s: %w", id, err)
  }
  return user, nil
}

// Clean error handling
func HandleRequest(w http.ResponseWriter, r *http.Request) {
  user, err := FindUser(r.URL.Query().Get("id"))
  if err != nil {
    http.Error(w, err.Error(), 500)
    return
  }
  // ...
}
```

**When panic is appropriate:**

```go
// ✅ Panic for programmer errors (bugs)
func NewServer(port int) *Server {
  if port < 0 || port > 65535 {
    panic("invalid port number") // Bug in calling code
  }
  return &Server{port: port}
}

// ✅ Panic in init() for configuration errors
func init() {
  if os.Getenv("API_KEY") == "" {
    panic("API_KEY environment variable required")
  }
}
```

### Not Wrapping Errors

Losing error context by not wrapping errors with `%w`.

**Why it's bad:**

- Loses original error information
- Breaks error checking with errors.Is
- Makes debugging harder
- Violates Go 1.13+ error wrapping conventions

**Example:**

```go
// ❌ Lost context - creates new error
func LoadConfig(path string) (*Config, error) {
  data, err := os.ReadFile(path)
  if err != nil {
    return nil, errors.New("failed to read config") // Lost original error!
  }

  var cfg Config
  err = json.Unmarshal(data, &cfg)
  if err != nil {
    return nil, errors.New("failed to parse config") // Lost original error!
  }

  return &cfg, nil
}

// Cannot check for specific errors
err := LoadConfig("config.json")
if errors.Is(err, os.ErrNotExist) {
  // Never matches - original error was discarded!
}

// ✅ Wrap errors with %w
func LoadConfig(path string) (*Config, error) {
  data, err := os.ReadFile(path)
  if err != nil {
    return nil, fmt.Errorf("reading config file: %w", err)
  }

  var cfg Config
  err = json.Unmarshal(data, &cfg)
  if err != nil {
    return nil, fmt.Errorf("parsing config: %w", err)
  }

  return &cfg, nil
}

// Can check original error
err := LoadConfig("config.json")
if errors.Is(err, os.ErrNotExist) {
  // Works! Original error preserved
}
```

## Interface Anti-Patterns

### Interface Pollution

Creating interfaces with too many methods or unnecessary abstractions.

**Why it's bad:**

- Hard to implement (many methods required)
- Hard to test (must mock many methods)
- Violates Interface Segregation Principle
- Adds complexity without value

**Example:**

```go
// ❌ Large interface - hard to implement
type UserService interface {
  Create(user *User) error
  Update(user *User) error
  Delete(id string) error
  FindByID(id string) (*User, error)
  FindByEmail(email string) (*User, error)
  FindAll() ([]*User, error)
  Authenticate(email, password string) (*User, error)
  ResetPassword(email string) error
  ValidateEmail(email string) bool
  SendWelcomeEmail(user *User) error
  // 15 more methods...
}

// Must implement all 25 methods to satisfy interface!

// ✅ Small, focused interfaces
type UserCreator interface {
  Create(user *User) error
}

type UserFinder interface {
  FindByID(id string) (*User, error)
}

type UserAuthenticator interface {
  Authenticate(email, password string) (*User, error)
}

// Functions accept only what they need
func RegisterUser(creator UserCreator, user *User) error {
  return creator.Create(user)
}

// Easy to test - implement just one method
type MockCreator struct{}

func (m *MockCreator) Create(user *User) error {
  return nil
}
```

### Unnecessary Interface Abstraction

Defining interfaces when concrete types would work fine.

**Why it's bad:**

- Adds complexity without benefit
- Makes code harder to navigate
- Premature abstraction
- Violates YAGNI principle

**Example:**

```go
// ❌ Unnecessary interface - only one implementation
type Logger interface {
  Log(message string)
}

type ConsoleLogger struct{}

func (c *ConsoleLogger) Log(message string) {
  fmt.Println(message)
}

func ProcessData(logger Logger, data []byte) {
  logger.Log("Processing...")
  // ...
}

// ✅ Use concrete type until you need abstraction
type Logger struct{}

func (l *Logger) Log(message string) {
  fmt.Println(message)
}

func ProcessData(logger *Logger, data []byte) {
  logger.Log("Processing...")
  // ...
}

// Add interface when you have multiple implementations or need mocking
```

## Pointer and Value Semantics Anti-Patterns

### Not Using Zero Values

Requiring explicit initialization instead of making zero value useful.

**Why it's bad:**

- Forces callers to use constructors
- More verbose code
- Goes against Go idioms
- Increases chance of nil pointer bugs

**Example:**

```go
// ❌ Zero value not useful - requires constructor
type Counter struct {
  mu    *sync.Mutex // nil pointer!
  count int
}

func NewCounter() *Counter {
  return &Counter{
    mu: &sync.Mutex{}, // Must initialize
  }
}

func (c *Counter) Increment() {
  c.mu.Lock() // Panic if zero value used!
  defer c.mu.Unlock()
  c.count++
}

// ✅ Zero value works immediately
type Counter struct {
  mu    sync.Mutex // Not a pointer - zero value works!
  count int
}

// No constructor needed
var c Counter // Ready to use
c.Increment() // Works!

func (c *Counter) Increment() {
  c.mu.Lock() // Safe with zero value
  defer c.mu.Unlock()
  c.count++
}
```

### Pointer Receivers on Value Types

Using pointer receivers when value receivers would work better.

**Why it's bad:**

- Unnecessary indirection
- Prevents some compiler optimizations
- Forces heap allocation
- Makes nil receiver bugs possible

**Example:**

```go
// ❌ Unnecessary pointer receiver on small value
type Point struct {
  X, Y int
}

func (p *Point) Add(other Point) Point {
  return Point{p.X + other.X, p.Y + other.Y}
}

// ✅ Value receiver for small values
func (p Point) Add(other Point) Point {
  return Point{p.X + other.X, p.Y + other.Y}
}

// ✅ Pointer receiver when you need to modify
type Counter struct {
  count int
}

func (c *Counter) Increment() { // Needs pointer to modify
  c.count++
}
```

**When to use pointer receivers:**

- Method modifies the receiver
- Receiver is large struct (avoid copying)
- Some methods need pointer receiver (consistency)

## Channel Anti-Patterns

### Closing Channels from Receiver Side

Closing a channel from the receiver instead of sender.

**Why it's bad:**

- Sender panics when sending to closed channel
- Receiver doesn't know when sender is done
- Violates channel ownership principle
- Causes race conditions

**Example:**

```go
// ❌ Receiver closes channel - sender panics
func BadPattern() {
  ch := make(chan int)

  go func() {
    for i := 0; i < 10; i++ {
      time.Sleep(100 * time.Millisecond)
      ch <- i // Panic! Channel closed by receiver
    }
  }()

  for i := 0; i < 5; i++ {
    fmt.Println(<-ch)
  }
  close(ch) // Bad! Sender still writing
}

// ✅ Sender closes channel
func GoodPattern() {
  ch := make(chan int)

  go func() {
    defer close(ch) // Sender owns and closes channel
    for i := 0; i < 10; i++ {
      ch <- i
    }
  }()

  for num := range ch { // Reads until channel closed
    fmt.Println(num)
  }
}

// ✅ Use context for early termination
func GoodPatternWithContext(ctx context.Context) {
  ch := make(chan int)

  go func() {
    defer close(ch)
    for i := 0; ; i++ {
      select {
      case ch <- i:
      case <-ctx.Done():
        return // Sender stops and closes channel
      }
    }
  }()

  // Receiver can cancel context to stop sender
}
```

### Not Checking if Channel is Closed

Reading from closed channel without checking.

**Example:**

```go
// ❌ Doesn't check if channel is closed
func ProcessMessages(ch <-chan Message) {
  for {
    msg := <-ch // Returns zero value when closed, keeps looping!
    process(msg)
  }
}

// ✅ Use range to handle closure
func ProcessMessages(ch <-chan Message) {
  for msg := range ch { // Exits when channel closed
    process(msg)
  }
}

// ✅ Or check closed status explicitly
func ProcessMessages(ch <-chan Message) {
  for {
    msg, ok := <-ch
    if !ok {
      return // Channel closed
    }
    process(msg)
  }
}
```

## Package Design Anti-Patterns

### God Packages

Packages that do too many unrelated things.

**Why it's bad:**

- Hard to understand purpose
- Difficult to maintain
- Cyclic dependency risks
- Violates Single Responsibility Principle

**Example:**

```go
// ❌ God package - too many responsibilities
package app

// User management
type User struct{}
func CreateUser() {}
func DeleteUser() {}

// Payment processing
type Payment struct{}
func ProcessPayment() {}

// Email sending
type Email struct{}
func SendEmail() {}

// Logging
func LogMessage() {}

// Configuration
func LoadConfig() {}

// ... 50 more unrelated things

// ✅ Focused packages
package user
type User struct{}
func Create() {}
func Delete() {}

package payment
type Payment struct{}
func Process() {}

package email
type Message struct{}
func Send() {}

package log
func Message() {}

package config
func Load() {}
```

### Global Mutable State

Using package-level mutable variables.

**Why it's bad:**

- Hidden dependencies (not visible in function signatures)
- Breaks testability
- Not thread-safe without synchronization
- Makes code unpredictable

**Example:**

```go
// ❌ Global mutable state
package db

var Connection *sql.DB // Global mutable variable

func Init(dsn string) error {
  var err error
  Connection, err = sql.Open("postgres", dsn)
  return err
}

func QueryUser(id string) (*User, error) {
  // Hidden dependency on global Connection
  return Connection.Query(id)
}

// Tests interfere with each other
func TestQueryUser(t *testing.T) {
  Init("test_db") // Mutates global state
  // ...
}

// ✅ Explicit dependencies via parameters
package db

type DB struct {
  conn *sql.DB
}

func New(dsn string) (*DB, error) {
  conn, err := sql.Open("postgres", dsn)
  if err != nil {
    return nil, err
  }
  return &DB{conn: conn}, nil
}

func (db *DB) QueryUser(id string) (*User, error) {
  return db.conn.Query(id) // Explicit dependency
}

// Tests are isolated
func TestQueryUser(t *testing.T) {
  db, _ := New("test_db") // Independent instance
  // ...
}
```

## Recognition and Prevention

### How to Spot Anti-Patterns

Anti-patterns in Go reveal themselves through specific symptoms. Goroutine count increasing over time indicates goroutine leaks - check for goroutines that never receive cancellation signals or never finish sending to channels. Deadlock panics with "all goroutines are asleep" messages point to channel coordination problems where senders and receivers aren't properly synchronized.

Error handling issues show up as silent failures where things stop working without explanation. If you find yourself adding log statements everywhere to understand what went wrong, your code is probably ignoring errors somewhere. Panic-recover patterns that look like try-catch blocks from other languages indicate someone fighting against Go's error handling philosophy.

Watch for tests that are difficult to write. If you cannot test a function without spinning up databases, HTTP servers, or other components, tight coupling has taken over. When you need to modify global state or use package-level variables to make tests work, your code depends on hidden dependencies that should be explicit parameters.

Performance problems often indicate anti-patterns. If goroutine count grows without bound, you have leaks. If your program eats memory despite processing fixed-size data, check for unbuffered channels with backed-up goroutines. High allocation rates sometimes reveal unnecessary pointer usage on small value types.

### Prevention Strategies

Use Go's race detector during development and testing (`go test -race`, `go run -race`). It catches data races that indicate improper goroutine synchronization, shared state without proper locking, or channel usage bugs. Run it regularly - race conditions are notoriously hard to debug when they occur in production.

Follow Go's code review comments and effective Go guidelines religiously. These documents codify years of community wisdom about what works in Go. When you're unsure whether a pattern is idiomatic, search the standard library - it demonstrates canonical Go style and patterns.

Embrace Go's static analysis tools. Run `go vet` as part of your build process - it catches common mistakes like lost errors, copying locks, unreachable code, and suspicious constructs. Use tools like `staticcheck` and `golangci-lint` for deeper analysis of code quality issues.

Keep functions small and focused. When a function grows beyond a screen or two, extract smaller functions. This makes testing easier, reveals hidden dependencies, and prevents god functions that do too much. Small functions compose naturally into larger behaviors while remaining individually testable.

Write tests that verify goroutines clean up properly. Use `goleak` package to detect goroutine leaks in tests. Add timeouts to tests involving goroutines - if a test hangs, you probably have a deadlock or leak. Tests should fail fast, not hang indefinitely.

## Summary

Go anti-patterns typically stem from fighting the language rather than embracing its idioms. Goroutine leaks occur when you spawn concurrent work without providing cleanup mechanisms - always give goroutines a way to exit using contexts or done channels. Deadlocks happen when channel senders and receivers aren't properly coordinated - buffer channels appropriately or use synchronization primitives like WaitGroup.

Error handling anti-patterns emerge from ignoring Go's explicit error philosophy. Discarding errors with underscore creates silent failures that corrupt data and make debugging impossible. Using panic for normal error conditions violates caller expectations and makes error handling invisible. Not wrapping errors loses critical context that helps identify the root cause of failures.

Interface anti-patterns involve creating unnecessary abstractions. Large interfaces with many methods are hard to implement and test - keep interfaces small and focused. Defining interfaces before you have multiple implementations violates YAGNI and adds complexity without value. Let interfaces emerge naturally when you need abstraction.

Pointer and value semantics mistakes make code less idiomatic and more error-prone. Not designing for useful zero values forces callers to use constructors and increases nil pointer risk. Using pointer receivers when value receivers would work adds unnecessary indirection and prevents compiler optimizations.

Channel anti-patterns create subtle bugs. Closing channels from receivers instead of senders causes sender panics. Not checking if channels are closed leads to infinite loops processing zero values. These bugs are particularly insidious because they often manifest intermittently depending on timing.

Package design anti-patterns create maintenance nightmares. God packages that do everything make code hard to understand and modify. Global mutable state creates hidden dependencies that break testability and make code unpredictable. Explicit dependencies through parameters make code clearer and easier to test.

The key to avoiding anti-patterns lies in learning Go's idioms and understanding why they exist. Each idiom solves a category of problems that plagues other approaches. When you find yourself fighting the language or writing complex code to solve simple problems, step back and look for the simpler, more idiomatic solution.

## Related Content

- [Go Best Practices and Idioms](/en/learn/swe/prog-lang/golang/explanation/best-practices)
- [How to Avoid Nil Panics](/en/learn/swe/prog-lang/golang/how-to/avoid-nil-panics)
- [How to Use Goroutines and Channels](/en/learn/swe/prog-lang/golang/how-to/use-goroutines-and-channels)
- [How to Handle Errors Effectively](/en/learn/swe/prog-lang/golang/how-to/handle-errors-effectively)
