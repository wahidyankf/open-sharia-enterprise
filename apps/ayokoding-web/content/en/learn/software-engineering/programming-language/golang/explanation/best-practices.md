---
title: "Best Practices"
date: 2025-12-17T10:00:00+07:00
draft: false
weight: 1000030
description: "Essential Go idioms and best practices for writing clean, maintainable, and idiomatic code"
tags: ["golang", "best-practices", "idioms", "code-quality"]
categories: ["learn"]
---

## Overview

Writing quality Go code requires understanding the language's philosophy and idioms. Unlike languages with many ways to solve problems, Go emphasizes simplicity and clarity through well-established patterns. These best practices emerge from Go's design principles and community wisdom.

## Core Idioms

### Accept Interfaces, Return Structs

Depend on abstractions for parameters, but return concrete types from functions.

**Why it matters:**

- Enables callers to pass any implementation
- Keeps your API flexible
- Avoids forcing callers to import implementation packages
- Return concrete types for clarity and performance

**Example:**

```go
// ❌ Takes concrete type - inflexible
func ProcessData(db *PostgresDB) error {
  // Can only work with PostgresDB
  data, err := db.Query("SELECT * FROM users")
  if err != nil {
    return err
  }
  // Process data...
  return nil
}

// ✅ Accepts interface - flexible
type DataStore interface {
  Query(sql string) ([]byte, error)
}

func ProcessData(store DataStore) error {
  // Works with any DataStore implementation
  data, err := store.Query("SELECT * FROM users")
  if err != nil {
    return err
  }
  // Process data...
  return nil
}

// Usage - can pass PostgresDB, MySQL, MockDB, etc.
func main() {
  db := &PostgresDB{} // Implements DataStore
  ProcessData(db)

  mock := &MockDB{} // Also implements DataStore
  ProcessData(mock)
}
```

**Trade-offs:**

- Accepting interfaces adds flexibility but requires defining interface types
- Returning structs gives clear documentation and better performance
- Don't create interfaces until you need abstraction

### Make Zero Values Useful

Design types so their zero value is ready to use without initialization.

**Why it matters:**

- Eliminates need for constructors
- Prevents nil pointer dereferences
- Makes code simpler and safer
- Follows Go's philosophy of "make the zero value useful"

**Example:**

```go
// ❌ Zero value is not useful - requires initialization
type Buffer struct {
  data []byte
  size int
}

func (b *Buffer) Write(p []byte) {
  // Panic if data is nil!
  b.data = append(b.data, p...)
}

// Must use constructor
func NewBuffer() *Buffer {
  return &Buffer{
    data: make([]byte, 0),
    size: 0,
  }
}

// ✅ Zero value works immediately
type Buffer struct {
  data []byte // nil slice works with append!
  size int    // zero works for initial size
}

func (b *Buffer) Write(p []byte) {
  // Safe - append handles nil slice
  b.data = append(b.data, p...)
  b.size += len(p)
}

// No constructor needed
func main() {
  var buf Buffer // Zero value ready to use
  buf.Write([]byte("hello"))
}
```

**Standard library examples:**

```go
var buf bytes.Buffer // Ready to use
buf.WriteString("hello")

var wg sync.WaitGroup // Ready to use
wg.Add(1)

var mu sync.Mutex // Ready to use
mu.Lock()
```

### Composition Through Embedding

Build functionality by embedding types rather than inheritance.

**Why it matters:**

- Go has no inheritance - composition is the way
- Embedded fields promote their methods automatically
- Clearer than inheritance hierarchies
- Enables mixing multiple behaviors

**Example:**

```go
// ❌ Cannot use inheritance in Go
// type ElectricCar extends Car { } // Doesn't exist!

// ✅ Composition via embedding
type Engine struct {
  Horsepower int
}

func (e *Engine) Start() {
  fmt.Println("Engine starting...")
}

type Battery struct {
  Capacity int
}

func (b *Battery) Charge() {
  fmt.Println("Charging battery...")
}

type ElectricCar struct {
  Engine  // Embedded - promotes Engine methods
  Battery // Embedded - promotes Battery methods
  Model string
}

// ElectricCar has both Start() and Charge() methods
func main() {
  car := ElectricCar{
    Engine:  Engine{Horsepower: 300},
    Battery: Battery{Capacity: 75},
    Model:   "Tesla Model 3",
  }

  car.Start()  // Promoted from Engine
  car.Charge() // Promoted from Battery
}
```

**When to embed vs compose:**

```go
// Embed when you want to promote methods
type LoggingDB struct {
  *sql.DB // Promotes all DB methods
  logger  Logger
}

func (db *LoggingDB) Query(query string) (*sql.Rows, error) {
  db.logger.Log("Executing: " + query)
  return db.DB.Query(query) // Use embedded field
}

// Explicit field when you don't want promotion
type UserService struct {
  db Database // Not embedded - no method promotion
}

func (s *UserService) FindUser(id string) (*User, error) {
  return s.db.Query(id) // Must use field explicitly
}
```

### Explicit Error Handling

Go requires explicit error handling through return values.

**Why it matters:**

- Errors are values, not exceptions
- Forces you to handle errors at each call site
- Makes error paths visible in code
- Prevents silent failures

**Example:**

```go
// ❌ Other languages hide errors with exceptions
// try {
//   data = readFile(path)
//   result = processData(data)
// } catch (Exception e) {
//   // Handle any error
// }

// ✅ Go makes errors explicit
func ProcessFile(path string) error {
  // Must handle error from readFile
  data, err := readFile(path)
  if err != nil {
    return fmt.Errorf("failed to read file: %w", err)
  }

  // Must handle error from processData
  result, err := processData(data)
  if err != nil {
    return fmt.Errorf("failed to process data: %w", err)
  }

  // Use result...
  return nil
}
```

**Error wrapping with context:**

```go
// ✅ Wrap errors to preserve context
func LoadUser(id string) (*User, error) {
  data, err := db.Query(id)
  if err != nil {
    // Wrap with %w to preserve original error
    return nil, fmt.Errorf("loading user %s: %w", id, err)
  }

  user, err := parseUser(data)
  if err != nil {
    return nil, fmt.Errorf("parsing user %s: %w", id, err)
  }

  return user, nil
}

// Check specific errors
err := LoadUser("123")
if errors.Is(err, sql.ErrNoRows) {
  // Handle not found
}
```

### Keep Interfaces Small

Define minimal interfaces with few methods.

**Why it matters:**

- Easier to implement (fewer methods required)
- Easier to test (mock fewer methods)
- More reusable (broader applicability)
- Follows Interface Segregation Principle

**Example:**

```go
// ❌ Large interface - hard to implement
type DataStore interface {
  Query(sql string) ([]Row, error)
  Insert(table string, data map[string]interface{}) error
  Update(table string, id string, data map[string]interface{}) error
  Delete(table string, id string) error
  BeginTransaction() (Transaction, error)
  Commit(tx Transaction) error
  Rollback(tx Transaction) error
  Migrate(version int) error
  Backup(path string) error
}

// Must implement all 9 methods to satisfy interface

// ✅ Small, focused interfaces
type Querier interface {
  Query(sql string) ([]Row, error)
}

type Inserter interface {
  Insert(table string, data map[string]interface{}) error
}

type Transactional interface {
  BeginTransaction() (Transaction, error)
}

// Functions accept only what they need
func LoadUsers(q Querier) ([]User, error) {
  rows, err := q.Query("SELECT * FROM users")
  // ...
}

// Easy to mock - just implement Query
type MockQuerier struct{}

func (m *MockQuerier) Query(sql string) ([]Row, error) {
  return []Row{{ID: "1"}}, nil
}
```

**Standard library examples (all have 1-2 methods):**

```go
type Reader interface {
  Read(p []byte) (n int, err error)
}

type Writer interface {
  Write(p []byte) (n int, err error)
}

type Stringer interface {
  String() string
}

type error interface {
  Error() string
}
```

## Resource Management

### Use Defer for Cleanup

Defer statements guarantee cleanup regardless of how function exits.

**Why it matters:**

- Cleanup happens even if panic occurs
- Keeps resource acquisition and release together
- Makes code safer and easier to read
- Stacks multiple defers in reverse order

**Example:**

```go
// ❌ Manual cleanup - error-prone
func ProcessFile(path string) error {
  file, err := os.Open(path)
  if err != nil {
    return err
  }

  data, err := readAll(file)
  if err != nil {
    file.Close() // Must remember to close
    return err
  }

  result, err := process(data)
  if err != nil {
    file.Close() // Must remember to close
    return err
  }

  file.Close() // Must remember to close
  return nil
}

// ✅ Defer guarantees cleanup
func ProcessFile(path string) error {
  file, err := os.Open(path)
  if err != nil {
    return err
  }
  defer file.Close() // Guaranteed to run

  data, err := readAll(file)
  if err != nil {
    return err // file.Close() runs automatically
  }

  result, err := process(data)
  if err != nil {
    return err // file.Close() runs automatically
  }

  return nil // file.Close() runs automatically
}
```

**Multiple defers execute in LIFO order:**

```go
func Transaction() error {
  tx, err := db.Begin()
  if err != nil {
    return err
  }
  defer tx.Rollback() // Runs second

  mu.Lock()
  defer mu.Unlock() // Runs first

  // Do work...

  return tx.Commit() // If commit succeeds, rollback is no-op
}
```

**Defer with parameters evaluated immediately:**

```go
func ProcessMultiple() {
  for i := 0; i < 5; i++ {
    file, _ := os.Open(fmt.Sprintf("file%d.txt", i))

    // ❌ All defers capture same 'i' variable
    defer file.Close()
    defer fmt.Println("Closing file", i) // Prints 4, 4, 4, 4, 4
  }
}

// ✅ Pass variable as parameter
func ProcessMultiple() {
  for i := 0; i < 5; i++ {
    file, _ := os.Open(fmt.Sprintf("file%d.txt", i))
    defer file.Close()

    defer func(idx int) {
      fmt.Println("Closing file", idx) // Prints 4, 3, 2, 1, 0
    }(i)
  }
}
```

## Goroutines and Channels

### Use Goroutines for Concurrency, Not Parallelism

Goroutines enable concurrent design, but parallelism depends on GOMAXPROCS.

**Why it matters:**

- Concurrency is about structure, parallelism is about execution
- Goroutines are cheap (thousands work fine)
- Don't create goroutines unnecessarily
- Always ensure goroutines can exit

**Example:**

```go
// ❌ Unnecessary goroutine
func GetUser(id string) (*User, error) {
  done := make(chan *User)

  go func() {
    user, _ := db.Query(id)
    done <- user
  }()

  return <-done, nil
}

// ✅ Simple synchronous call
func GetUser(id string) (*User, error) {
  return db.Query(id)
}

// ✅ Goroutines for truly concurrent work
func FetchMultipleUsers(ids []string) ([]*User, error) {
  type result struct {
    user *User
    err  error
  }

  results := make(chan result, len(ids))

  for _, id := range ids {
    go func(userID string) {
      user, err := db.Query(userID)
      results <- result{user, err}
    }(id)
  }

  users := make([]*User, 0, len(ids))
  for i := 0; i < len(ids); i++ {
    r := <-results
    if r.err != nil {
      return nil, r.err
    }
    users = append(users, r.user)
  }

  return users, nil
}
```

### Use Buffered Channels Appropriately

Choose channel buffer size based on coordination needs.

**Why it matters:**

- Unbuffered channels synchronize sender and receiver
- Buffered channels decouple them up to capacity
- Wrong choice causes deadlocks or memory waste
- Buffer size communicates intent

**Example:**

```go
// ✅ Unbuffered for synchronization
func Worker(tasks <-chan Task, results chan<- Result) {
  for task := range tasks {
    result := process(task)
    results <- result // Waits for receiver
  }
}

// ✅ Buffered to prevent blocking
func Generate(n int) <-chan int {
  out := make(chan int, n) // Buffer matches expected output

  go func() {
    for i := 0; i < n; i++ {
      out <- i // Won't block until buffer full
    }
    close(out)
  }()

  return out
}

// ✅ Buffered for result collection
func FetchAll(urls []string) []Response {
  results := make(chan Response, len(urls)) // Buffer = num goroutines

  for _, url := range urls {
    go func(u string) {
      resp := fetch(u)
      results <- resp // Never blocks - buffer sized correctly
    }(url)
  }

  responses := make([]Response, len(urls))
  for i := 0; i < len(urls); i++ {
    responses[i] = <-results
  }

  return responses
}
```

## Naming Conventions

### Use Short, Clear Names in Limited Scope

Short names for limited scope, longer names for wider visibility.

**Why it matters:**

- Short names reduce noise in small scopes
- Longer names provide context in large scopes
- Follows Go community conventions
- Makes code more readable

**Example:**

```go
// ✅ Short names in limited scope
func (s *Server) ServeHTTP(w http.ResponseWriter, r *http.Request) {
  // w, r are conventional and clear in this small scope
  w.WriteHeader(http.StatusOK)
  fmt.Fprintf(w, "Request: %s", r.URL.Path)
}

// ✅ Longer names for package-level functions
func ProcessCustomerPayment(customerID string, amount decimal.Decimal) error {
  // Clear what this does at package level
}

// ❌ Verbose names in small scope
func process(items []Item) {
  for indexPosition := 0; indexPosition < len(items); indexPosition++ {
    currentItem := items[indexPosition]
    // Too verbose for this small loop
  }
}

// ✅ Conventional short names
func process(items []Item) {
  for i, item := range items {
    // i and item are clear and conventional
  }
}
```

**Common abbreviations:**

- `i, j, k` - loop indices
- `n` - count/length
- `err` - error
- `ctx` - context.Context
- `r` - http.Request
- `w` - http.ResponseWriter
- `db` - database connection

### Package Names Should Be Singular

Use singular, lowercase, no underscores.

**Example:**

```go
// ❌ Plural or underscores
package users
package user_service

// ✅ Singular, clear
package user
package http
package sql
```

## Error Patterns

### Sentinel Errors for Expected Conditions

Define package-level error variables for common error conditions.

**Example:**

```go
// ✅ Sentinel errors
var (
  ErrNotFound     = errors.New("user not found")
  ErrUnauthorized = errors.New("unauthorized")
  ErrInvalidInput = errors.New("invalid input")
)

func FindUser(id string) (*User, error) {
  user, err := db.Query(id)
  if err != nil {
    return nil, err
  }
  if user == nil {
    return nil, ErrNotFound
  }
  return user, nil
}

// Callers can check with errors.Is
user, err := FindUser("123")
if errors.Is(err, ErrNotFound) {
  // Handle not found specifically
}
```

### Custom Error Types for Rich Context

Implement error interface for errors needing structured data.

**Example:**

```go
// ✅ Custom error type
type ValidationError struct {
  Field   string
  Message string
}

func (e *ValidationError) Error() string {
  return fmt.Sprintf("validation failed for %s: %s", e.Field, e.Message)
}

func ValidateUser(u *User) error {
  if u.Email == "" {
    return &ValidationError{
      Field:   "email",
      Message: "email is required",
    }
  }
  return nil
}

// Callers can extract structured data
err := ValidateUser(user)
var validErr *ValidationError
if errors.As(err, &validErr) {
  fmt.Printf("Field: %s, Message: %s\n", validErr.Field, validErr.Message)
}
```

## When to Break the Rules

Go idioms are strong conventions, not absolute laws. Break them when:

- **Performance critical paths**: Profiling shows the idiom hurts performance significantly
- **External constraints**: Third-party libraries require different patterns
- **Practical trade-offs**: Cost of following idiom exceeds benefit
- **Team consensus**: Team agrees on different approach with clear reasoning

Always document deviations with clear comments explaining why.

## Summary

Writing idiomatic Go means embracing simplicity and explicitness over cleverness and magic. Accept interfaces to keep your functions flexible about what they consume, but return concrete types so callers know exactly what they get. Design your types so their zero value works immediately without requiring constructors or initialization - this prevents entire categories of nil pointer bugs while making your API simpler to use.

Composition through embedding provides Go's answer to inheritance, letting you mix behaviors by combining types rather than building deep hierarchies. When errors occur, handle them explicitly at each call site rather than hoping they'll bubble up through exception handlers - this makes error paths visible in your code and prevents silent failures.

Keep your interfaces small, ideally one or two methods, making them easy to implement and test. Use defer to guarantee cleanup happens regardless of how your function exits, keeping resource acquisition and release code together. When you need concurrency, spawn goroutines freely but ensure they can all exit - goroutine leaks are a common source of bugs.

Choose channel buffer sizes deliberately: unbuffered for synchronization, buffered to match expected capacity. Name variables with scope in mind - short names like `i`, `err`, and `ctx` work perfectly in small scopes while package-level functions need descriptive names. Define sentinel errors as package variables for common conditions, but create custom error types when you need to attach structured data.

These idioms reinforce Go's philosophy of clarity over cleverness, making code that's straightforward to read, test, and maintain. The patterns may feel constraining at first, but they create consistency across Go codebases that makes reading other people's code feel familiar rather than foreign.

## Related Content

- [Common Go Anti-Patterns](/en/learn/software-engineering/programming-language/golang/explanation/anti-patterns)
- [How to Avoid Nil Panics](/en/learn/software-engineering/programming-language/golang/how-to/avoid-nil-panics)
- [How to Handle Errors Effectively](/en/learn/software-engineering/programming-language/golang/how-to/handle-errors-effectively)
- [How to Use Goroutines and Channels](/en/learn/software-engineering/programming-language/golang/how-to/use-goroutines-and-channels)
