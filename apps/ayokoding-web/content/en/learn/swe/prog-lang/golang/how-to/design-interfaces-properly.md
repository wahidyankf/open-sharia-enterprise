---
title: "How to Design Interfaces Properly"
date: 2025-12-17T10:00:00+07:00
draft: false
weight: 611
description: "Practical patterns for designing small, focused interfaces that follow Go idioms"
tags: ["golang", "interfaces", "design", "best-practices"]
---

## Problem

Interfaces in Go work best when they're small and focused, but developers from other languages often create large interfaces that are hard to implement and test.

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
}
```

This guide shows patterns for designing Go interfaces effectively.

## Solution Strategies

### Keep Interfaces Small

Design interfaces with one or two methods.

**When to use**: Always prefer small interfaces unless you have specific reasons for larger ones.

```go
// ❌ Large interface requiring many methods
type UserService interface {
  Create(user *User) error
  Update(user *User) error
  Delete(id string) error
  FindByID(id string) (*User, error)
  FindByEmail(email string) (*User, error)
  FindAll() ([]*User, error)
  Authenticate(email, password string) (*User, error)
  ResetPassword(email string) error
  ChangePassword(id, oldPass, newPass string) error
}

// Must implement all 9 methods to satisfy interface

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

func GetUser(finder UserFinder, id string) (*User, error) {
  return finder.FindByID(id)
}

// Easy to test - mock only one method
type MockCreator struct{}

func (m *MockCreator) Create(user *User) error {
  return nil
}
```

**Standard library examples (all small):**

```go
type Reader interface {
  Read(p []byte) (n int, err error)
}

type Writer interface {
  Write(p []byte) (n int, err error)
}

type Closer interface {
  Close() error
}

type error interface {
  Error() string
}

type Stringer interface {
  String() string
}
```

### Define Interfaces Where They're Used

Consumers define interfaces, not providers.

**When to use**: Always define interfaces in consuming packages.

```go
// ❌ Provider defines interface
package database

type UserRepository interface {
  FindByID(id string) (*User, error)
  Save(user *User) error
}

type PostgresUserRepository struct{}

func (r *PostgresUserRepository) FindByID(id string) (*User, error) {}
func (r *PostgresUserRepository) Save(user *User) error {}

// ✅ Consumer defines interface
package user

// Define only the methods you need
type UserFinder interface {
  FindByID(id string) (*User, error)
}

type Service struct {
  finder UserFinder // Depends on interface
}

func (s *Service) GetUser(id string) (*User, error) {
  return s.finder.FindByID(id)
}

// ✅ Provider implements interface implicitly
package database

type PostgresRepository struct {
  db *sql.DB
}

// Implements user.UserFinder without explicitly saying so
func (r *PostgresRepository) FindByID(id string) (*user.User, error) {
  // Implementation...
}

// ✅ Main package wires them together
package main

import (
  "yourproject/user"
  "yourproject/database"
)

func main() {
  repo := database.NewPostgresRepository(db)
  service := user.NewService(repo) // PostgresRepository satisfies UserFinder
}
```

**Benefits:**

- Consumers get exactly what they need
- Providers don't dictate interfaces
- Easy to swap implementations
- Follows Dependency Inversion Principle

### Accept Interfaces, Return Structs

Parameters should be interfaces, return types should be concrete.

**When to use**: Public APIs, library functions.

```go
// ❌ Returns interface - forces all implementations to match
func NewUserService(db *sql.DB) UserService {
  return &DefaultUserService{db: db}
}

// Caller receives interface
service := NewUserService(db)
// Cannot access any methods beyond interface

// ✅ Returns concrete type
func NewUserService(db *sql.DB) *UserService {
  return &UserService{db: db}
}

// Caller receives concrete type
service := NewUserService(db)
// Can access all methods, not just interface

// ✅ Accepts interface for flexibility
func ProcessUsers(finder UserFinder) error {
  users, err := finder.FindAll()
  // Works with any UserFinder implementation
}

// Can pass any implementation
ProcessUsers(postgresRepo)
ProcessUsers(mockRepo)
ProcessUsers(memoryRepo)
```

**Why return concrete types:**

- Callers see all available methods
- Easier to add methods without breaking compatibility
- Better documentation and IDE support
- Callers can create interfaces they need

### Use Empty Interface Sparingly

Avoid `interface{}` (or `any` in Go 1.18+) unless necessary.

**When to use**: Generic containers, JSON unmarshaling, reflection-based libraries.

```go
// ❌ Loses type safety
func Process(data interface{}) error {
  // No type safety - could be anything!
  if user, ok := data.(*User); ok {
    // Type assertion everywhere
  } else if payment, ok := data.(*Payment); ok {
    // More type assertions
  }
  return nil
}

// ✅ Use specific types
func ProcessUser(user *User) error {
  // Type-safe
}

func ProcessPayment(payment *Payment) error {
  // Type-safe
}

// ✅ Use generics (Go 1.18+) for type-safe containers
func Process[T any](data T) error {
  // Type-safe with generics
  return nil
}

// ✅ Empty interface for truly generic scenarios
func ToJSON(v interface{}) ([]byte, error) {
  // JSON encoding works with any type
  return json.Marshal(v)
}

func FromJSON(data []byte, v interface{}) error {
  // JSON decoding needs pointer to any type
  return json.Unmarshal(data, v)
}
```

### Compose Interfaces from Smaller Ones

Build complex interfaces from simple ones.

```go
// ✅ Small building blocks
type Reader interface {
  Read(p []byte) (n int, err error)
}

type Writer interface {
  Write(p []byte) (n int, err error)
}

type Closer interface {
  Close() error
}

// ✅ Compose larger interfaces
type ReadWriter interface {
  Reader
  Writer
}

type ReadWriteCloser interface {
  Reader
  Writer
  Closer
}

// Usage: Accept smallest interface needed
func CopyData(dst Writer, src Reader) error {
  // Only needs Reader and Writer, not full ReadWriteCloser
}

// Larger interface can be passed to functions needing smaller ones
var rwc ReadWriteCloser = file
CopyData(rwc, rwc) // Satisfies both Writer and Reader
```

**Common compositions:**

```go
type ReadSeeker interface {
  Reader
  Seeker
}

type WriteSeeker interface {
  Writer
  Seeker
}

type ReadWriteSeeker interface {
  Reader
  Writer
  Seeker
}
```

### Use Type Assertions and Type Switches Judiciously

Check for optional behavior when needed.

```go
// ✅ Type assertion for optional interface
func WriteData(w io.Writer, data []byte) error {
  n, err := w.Write(data)
  if err != nil {
    return err
  }

  // Check if writer can be flushed
  if flusher, ok := w.(interface{ Flush() error }); ok {
    return flusher.Flush()
  }

  return nil
}

// ✅ Type switch for multiple types
func Process(v interface{}) string {
  switch val := v.(type) {
  case string:
    return val
  case int:
    return fmt.Sprintf("%d", val)
  case error:
    return val.Error()
  default:
    return fmt.Sprintf("%v", val)
  }
}

// ✅ Check for optional capabilities
type Validator interface {
  Validate() error
}

func Save(db Database, entity interface{}) error {
  // Check if entity wants validation
  if validator, ok := entity.(Validator); ok {
    if err := validator.Validate(); err != nil {
      return err
    }
  }

  return db.Save(entity)
}
```

### Document Interface Contracts

Clearly document what implementations must do.

```go
// ✅ Well-documented interface
// UserFinder retrieves users from persistent storage.
type UserFinder interface {
  // FindByID returns a user by their unique identifier.
  // Returns ErrNotFound if user doesn't exist.
  // Returns other errors for system failures.
  FindByID(id string) (*User, error)

  // FindByEmail returns a user by their email address.
  // Email comparison is case-insensitive.
  // Returns ErrNotFound if user doesn't exist.
  FindByEmail(email string) (*User, error)
}

// Errors that implementations should return
var (
  ErrNotFound = errors.New("user not found")
  ErrInvalidEmail = errors.New("invalid email")
)

// ✅ Document edge cases and expectations
// Cache stores values with TTL.
type Cache interface {
  // Get retrieves a value by key.
  // Returns (nil, false) if key doesn't exist or is expired.
  // Returns (value, true) if key exists and is not expired.
  Get(key string) (interface{}, bool)

  // Set stores a value with a time-to-live.
  // TTL of 0 means no expiration.
  // Overwrites existing value if key already exists.
  Set(key string, value interface{}, ttl time.Duration) error
}
```

### Avoid Interface Pollution

Don't create interfaces until you need abstraction.

**When to use**: Wait until you have two implementations or need to mock for testing.

```go
// ❌ Premature interface - only one implementation exists
type EmailSender interface {
  Send(to, subject, body string) error
}

type SMTPEmailSender struct {
  host string
  port int
}

func (s *SMTPEmailSender) Send(to, subject, body string) error {
  // Implementation...
}

// Only one implementation - interface adds no value yet

// ✅ Start with concrete type
type EmailSender struct {
  host string
  port int
}

func (s *EmailSender) Send(to, subject, body string) error {
  // Implementation...
}

// ✅ Add interface when needed (testing or multiple implementations)
type Sender interface {
  Send(to, subject, body string) error
}

// Now we have multiple implementations
type SMTPSender struct { /* ... */ }
type SendGridSender struct { /* ... */ }
type MockSender struct { /* ... */ } // For testing
```

## Putting It All Together

When designing interfaces in Go, start by asking whether you need an interface at all. Interfaces add abstraction, which has a cost in complexity and indirection. If you only have one implementation and aren't testing with mocks, work with the concrete type. Add interfaces when you have multiple implementations or need abstraction for testing.

Define interfaces in consuming packages, not in providing packages. The user package should define what it needs from a repository, not the database package dictating what repositories must do. This lets consumers get exactly what they need rather than depending on large interfaces with methods they'll never call.

Keep interfaces small - one or two methods ideally. Small interfaces are easier to implement, easier to test, and more flexible. You can always compose larger interfaces from smaller ones when needed. The standard library demonstrates this with Reader, Writer, and Closer combining into ReadWriteCloser.

Accept interfaces as parameters to make functions flexible about what they work with. Return concrete types so callers see all available methods and have full documentation. This asymmetry - interfaces in, structs out - gives flexibility where needed while keeping return types concrete and discoverable.

Use type assertions to check for optional capabilities when appropriate. If a writer might support flushing, check with a type assertion and call flush if available. This allows implementations to provide extra functionality without forcing all implementations to support it.

## Common Mistakes to Avoid

**Don't create interfaces with many methods:**

```go
// ❌ Too many methods
type Repository interface {
  Create(entity interface{}) error
  Read(id string) (interface{}, error)
  Update(entity interface{}) error
  Delete(id string) error
  List(filter Filter) ([]interface{}, error)
  Count(filter Filter) (int, error)
  Migrate() error
}

// ✅ Small, focused interfaces
type Creator interface {
  Create(entity interface{}) error
}

type Reader interface {
  Read(id string) (interface{}, error)
}
```

**Don't export interfaces from packages providing implementations:**

```go
// ❌ Provider exports interface
package database

type UserRepository interface {
  FindByID(id string) (*User, error)
}

// ✅ Consumer defines interface
package user

type Finder interface {
  FindByID(id string) (*User, error)
}
```

**Don't use empty interface when specific types work:**

```go
// ❌ Loses type safety
func Add(a, b interface{}) interface{} {
  // Type assertions everywhere
}

// ✅ Specific types
func AddInt(a, b int) int {
  return a + b
}

// Or generics (Go 1.18+)
func Add[T int | float64](a, b T) T {
  return a + b
}
```

## Summary

Designing interfaces properly in Go means embracing small, focused contracts rather than large, comprehensive ones. Keep interfaces to one or two methods so they're easy to implement and test. When you need more functionality, compose larger interfaces from smaller building blocks like the standard library does with Reader, Writer, and Closer.

Define interfaces in consuming packages where they're used, not in providing packages where they're implemented. This inverts dependencies and gives consumers exactly what they need. A user service should define what it needs from storage rather than the database package dictating what repositories must do.

Accept interfaces as parameters to make functions flexible, but return concrete types so callers see all available methods and have complete documentation. This asymmetry provides flexibility at call sites while keeping return types concrete and discoverable.

Add interfaces only when you need abstraction - when you have multiple implementations or need to mock for testing. Starting with concrete types and extracting interfaces when needed prevents premature abstraction that adds complexity without value.

Use type assertions to check for optional capabilities when appropriate. This allows implementations to provide extra functionality without forcing all implementations to support it. Document interface contracts clearly so implementers understand what they must do and what callers can expect.

These patterns work together to create interfaces that are idiomatic, flexible, and easy to work with - hallmarks of well-designed Go code.

## Related Content

- [Go Best Practices and Idioms](/en/learn/swe/prog-lang/golang/explanation/best-practices)
- [Common Go Anti-Patterns](/en/learn/swe/prog-lang/golang/explanation/anti-patterns)
- [How to Refactor God Packages](/en/learn/swe/prog-lang/golang/how-to/refactor-god-packages)
- [How to Avoid Nil Panics](/en/learn/swe/prog-lang/golang/how-to/avoid-nil-panics)
