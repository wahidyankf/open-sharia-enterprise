---
title: Go Best Practices
description: Comprehensive guide to Go best practices for code organization, naming, testing, performance, and maintainability
category: explanation
subcategory: stack-lang
tags:
  - golang
  - best-practices
  - code-organization
  - naming-conventions
  - testing
  - performance
  - security
  - maintainability
  - go-1.18
  - go-1.21
  - go-1.22
  - go-1.23
  - go-1.24
  - go-1.25
created: 2026-01-22
updated: 2026-01-22
---

# Go Best Practices

**Understanding-oriented guide** to Go best practices - proven patterns and conventions for writing maintainable, performant, and idiomatic Go code.

## Table of Contents

1. [Overview](#overview)
2. [Code Organization](#code-organization)
3. [Naming Conventions](#naming-conventions)
4. [Code Style](#code-style)
5. [Package Design](#package-design)
6. [Error Handling](#error-handling)
7. [Testing](#testing)
8. [Performance](#performance)
9. [Concurrency](#concurrency)
10. [Security](#security)
11. [Dependency Management](#dependency-management)
12. [Build and Deployment](#build-and-deployment)
13. [Documentation](#documentation)
14. [Code Review](#code-review)
15. [Refactoring](#refactoring)
16. [Summary](#summary)
17. [Additional Resources](#additional-resources)

## Overview

Go best practices are established patterns that help you write clear, maintainable, and efficient code. These practices have emerged from the Go community and are reflected in the standard library.

### Why Best Practices Matter

```go
// Following best practices
func (s *UserService) GetUser(ctx context.Context, id int) (*User, error) {
    if id <= 0 {
        return nil, ErrInvalidUserID
    }

    user, err := s.repo.FindByID(ctx, id)
    if err != nil {
        return nil, fmt.Errorf("find user: %w", err)
    }

    return user, nil
}

// Ignoring best practices
func get_user(ID int) *User {
    u := getUserFromDb(ID) // No context, no error handling
    return u
}
```

**Benefits**:

1. **Readability**: Code is easier to understand
2. **Maintainability**: Changes are easier to make
3. **Reliability**: Fewer bugs and edge cases
4. **Performance**: Efficient resource usage
5. **Collaboration**: Consistent style across team

### Go Version Context

This guide covers Go 1.18-1.25 with emphasis on:

- **Go 1.18+**: Generics, workspace mode, fuzzing
- **Go 1.21+**: PGO, min/max/clear built-ins
- **Go 1.22+**: Loop variable scoping, enhanced routing
- **Go 1.23+**: Iterators, range over func
- **Go 1.24+**: Swiss Tables, runtime.AddCleanup
- **Go 1.25**: Current stable release

## Code Organization

### Project Structure

```
myapp/
├── cmd/                   # Main applications
│   ├── server/
│   │   └── main.go
│   └── worker/
│       └── main.go
├── internal/              # Private code
│   ├── auth/
│   ├── user/
│   └── order/
├── pkg/                   # Public libraries
│   └── api/
├── api/                   # API definitions (protobuf, OpenAPI)
├── web/                   # Web assets
├── scripts/               # Build/deploy scripts
├── test/                  # Additional test data
├── vendor/                # Vendored dependencies (if used)
├── go.mod
├── go.sum
├── Makefile
└── README.md
```

### File Organization

```go
// user/user.go - Core types and interfaces
package user

type User struct {
    ID    int
    Name  string
    Email string
}

type Repository interface {
    FindByID(ctx context.Context, id int) (*User, error)
    Save(ctx context.Context, user *User) error
}

// user/service.go - Business logic
package user

type Service struct {
    repo Repository
}

func NewService(repo Repository) *Service {
    return &Service{repo: repo}
}

func (s *Service) GetUser(ctx context.Context, id int) (*User, error) {
    // Implementation
}

// user/repository.go - Data access
package user

type PostgresRepository struct {
    db *sql.DB
}

func NewPostgresRepository(db *sql.DB) *PostgresRepository {
    return &PostgresRepository{db: db}
}

// user/user_test.go - Tests
package user_test

import (
    "testing"
    "myapp/internal/user"
)

func TestService_GetUser(t *testing.T) {
    // Test implementation
}
```

### Package Organization Best Practices

```go
// CORRECT: Organize by domain/feature
myapp/
├── user/       # User domain
├── order/      # Order domain
├── payment/    # Payment domain
└── shipping/   # Shipping domain

// INCORRECT: Organize by layer (creates coupling)
myapp/
├── models/     # All models
├── services/   # All services
├── repositories/ # All repositories
└── handlers/   # All handlers

// CORRECT: Flat package structure
myapp/
├── auth/
├── user/
└── order/

// INCORRECT: Deep nesting
myapp/
├── internal/
│   └── app/
│       └── domain/
│           └── user/
│               └── service/
│                   └── user_service.go  // Too deep!
```

### Internal Packages

```go
// Use internal/ to hide implementation details
myapp/
├── internal/
│   ├── auth/        # Only myapp/* can import
│   └── database/    # Only myapp/* can import
└── cmd/
    └── server/
        └── main.go  // Can import internal packages

// External packages cannot import internal/*
```

### File Naming

```go
// CORRECT: Lowercase with underscores for separation
user_service.go
http_handler.go
postgres_repository.go

// INCORRECT: CamelCase or other conventions
UserService.go
httpHandler.go
postgres-repository.go

// Test files
user_test.go         // Tests
user_integration_test.go  // Integration tests
example_test.go      // Examples
```

## Naming Conventions

### General Principles

```go
// Use camelCase for unexported names
var userName string
func fetchUser() {}

// Use PascalCase for exported names
var UserName string
func FetchUser() {}

// Short names for local variables
for i := range users {
    u := users[i]
    // Use i and u locally
}

// Descriptive names for package-level variables
var ErrUserNotFound = errors.New("user not found")
var DefaultTimeout = 30 * time.Second
```

### Variables

```go
// CORRECT: Short, clear names
user := FetchUser()
config := LoadConfig()
ctx := context.Background()

// Context: always ctx
func ProcessUser(ctx context.Context) error

// Error: always err
data, err := ReadFile(path)

// Index: i, j, k
for i := 0; i < len(items); i++ {
    for j := 0; j < len(items[i]); j++ {
        // Use i, j
    }
}

// INCORRECT: Overly long names
userInformationFromDatabase := FetchUser()

// INCORRECT: Abbreviations (unless standard)
usr := FetchUser()  // Use user
cfg := LoadConfig() // Use config (or cfg if very common in codebase)
```

### Functions and Methods

```go
// CORRECT: Verb or verb phrase
func GetUser() *User
func UpdateUserEmail() error
func isValid() bool

// CORRECT: Common patterns
func NewService() *Service          // Constructor
func (s *Service) Run() error      // Start operation
func (s *Service) Close() error    // Cleanup
func (s *Service) String() string  // Stringer interface

// CORRECT: Getters don't use "Get" prefix
type User struct {
    name string
}

func (u *User) Name() string { return u.name }  // Not GetName()

// CORRECT: Setters use "Set" prefix
func (u *User) SetName(name string) { u.name = name }

// INCORRECT: Noun for function name
func User() *User  // Confusing, use NewUser or GetUser

// INCORRECT: Redundant package name
func UserGetUser() *User  // Just GetUser()
```

### Types

```go
// CORRECT: Noun or noun phrase
type User struct{}
type UserService struct{}
type HTTPHandler struct{}

// CORRECT: Interface names
type Reader interface{}      // -er suffix
type Writer interface{}
type Closer interface{}
type UserRepository interface{}

// INCORRECT: "I" prefix (not idiomatic in Go)
type IUser interface{}  // Just User

// CORRECT: Avoid stutter with package name
package user

type User struct{}       // user.User (good)
type Service struct{}    // user.Service (good)

// INCORRECT: Stutter
type UserUser struct{}   // user.UserUser (bad!)
type UserService struct{} // Could be just Service if in user package
```

### Constants

```go
// CORRECT: PascalCase for exported
const MaxRetries = 3
const DefaultTimeout = 30 * time.Second

// CORRECT: camelCase for unexported
const maxRetries = 3
const defaultTimeout = 30 * time.Second

// CORRECT: Group related constants
const (
    StatusPending   = "pending"
    StatusApproved  = "approved"
    StatusRejected  = "rejected"
)

// CORRECT: Use iota for enums
type Status int

const (
    StatusPending Status = iota
    StatusApproved
    StatusRejected
)

// INCORRECT: ALL_CAPS (not idiomatic in Go)
const MAX_RETRIES = 3
```

### Packages

```go
// CORRECT: Short, lowercase, single word
package user
package auth
package http

// CORRECT: Abbreviations are acceptable
package fmt
package io
package db

// INCORRECT: Multiple words with separator
package user_service  // Use userservice or split into separate packages
package http-handler

// INCORRECT: Plural
package users  // Use user

// INCORRECT: Generic names
package util
package common
package helpers
```

### Acronyms

```go
// CORRECT: All uppercase or all lowercase
type HTTPServer struct{}    // HTTP
type URLPath string         // URL
var userID int             // ID

// INCORRECT: Mixed case
type HttpServer struct{}    // Should be HTTPServer
type UrlPath string         // Should be URLPath
var userId int             // Should be userID
```

## Code Style

### Formatting

```go
// Always run gofmt (or goimports)
// No need to debate formatting - use gofmt!

// CORRECT: gofmt formatted
func Add(a, b int) int {
    return a + b
}

// Tabs for indentation (gofmt default)
func Process() {
    if condition {
        // Indented with tabs
    }
}

// Line length: No hard limit, but be reasonable (~80-120 chars)
```

### Comments

```go
// CORRECT: Package documentation
// Package user provides user management functionality.
//
// It handles user creation, authentication, and authorization.
package user

// CORRECT: Exported function documentation
// GetUser retrieves a user by ID.
// It returns ErrNotFound if the user doesn't exist.
func GetUser(id int) (*User, error) {
    // Implementation
}

// CORRECT: Inline comments for complex logic
func Calculate(n int) int {
    // Apply the Sieve of Eratosthenes algorithm
    primes := make([]bool, n+1)
    // ...
}

// INCORRECT: Obvious comments
// Increment i
i++

// Add two numbers
result := a + b

// INCORRECT: Commented code (delete it!)
// oldImplementation := func() {
//     // ...
// }
```

### Function Length

```go
// CORRECT: Short, focused functions
func ValidateUser(user *User) error {
    if user == nil {
        return ErrNilUser
    }
    if user.Email == "" {
        return ErrEmptyEmail
    }
    return nil
}

// CORRECT: Extract complex logic
func ProcessOrder(order *Order) error {
    if err := validateOrder(order); err != nil {
        return err
    }
    if err := checkInventory(order); err != nil {
        return err
    }
    if err := processPayment(order); err != nil {
        return err
    }
    return shipOrder(order)
}

// INCORRECT: Long, monolithic functions (>50 lines)
func ProcessOrderMonolithic(order *Order) error {
    // 200 lines of inline logic...
}
```

### Variable Declaration

```go
// CORRECT: Short declaration when possible
user := GetUser()
count := len(items)

// CORRECT: var for zero values
var buf bytes.Buffer
var mu sync.Mutex

// CORRECT: var for explicit type
var timeout time.Duration = 30 * time.Second

// CORRECT: Group related declarations
var (
    ErrNotFound = errors.New("not found")
    ErrInvalid  = errors.New("invalid")
)

// INCORRECT: Unnecessary type specification
var user *User = GetUser()  // Just user := GetUser()
```

### Control Flow

```go
// CORRECT: Early return
func Process(data []byte) error {
    if len(data) == 0 {
        return ErrEmptyData
    }

    result, err := parse(data)
    if err != nil {
        return err
    }

    return save(result)
}

// INCORRECT: Nested conditions
func Process(data []byte) error {
    if len(data) > 0 {
        result, err := parse(data)
        if err == nil {
            return save(result)
        } else {
            return err
        }
    } else {
        return ErrEmptyData
    }
}

// CORRECT: Switch without expression
switch {
case age < 18:
    return "minor"
case age < 65:
    return "adult"
default:
    return "senior"
}

// CORRECT: Type switch
switch v := i.(type) {
case int:
    return v * 2
case string:
    return v + v
}
```

## Package Design

### Interface Design

```go
// CORRECT: Small, focused interfaces
type Reader interface {
    Read(p []byte) (n int, err error)
}

type Writer interface {
    Write(p []byte) (n int, err error)
}

// CORRECT: Interface in consumer package
// Package http defines handler interface
package http

type Handler interface {
    ServeHTTP(ResponseWriter, *Request)
}

// Implementations can be in other packages

// INCORRECT: Large, kitchen-sink interface
type DataAccess interface {
    Create() error
    Read() error
    Update() error
    Delete() error
    List() error
    Count() error
    Search() error
    // ... 20 more methods
}
```

### Accept Interfaces, Return Structs

```go
// CORRECT
func SaveUser(w io.Writer, user *User) error {
    // Accepts interface (flexible)
}

func GetConfig() *Config {
    // Returns struct (specific)
}

// INCORRECT
func SaveUser(w *os.File, user *User) error {
    // Too specific, hard to test
}

func GetConfig() Configurer {
    // Returning interface limits future changes
}
```

### Dependency Injection

```go
// CORRECT: Constructor injection
type Service struct {
    repo UserRepository
    log  Logger
}

func NewService(repo UserRepository, log Logger) *Service {
    return &Service{
        repo: repo,
        log:  log,
    }
}

// CORRECT: Interface dependencies
type UserRepository interface {
    FindByID(ctx context.Context, id int) (*User, error)
}

// INCORRECT: Global dependencies
var globalDB *sql.DB  // Hard to test

func GetUser(id int) (*User, error) {
    return globalDB.Query(id)
}
```

### Package-Level State

```go
// AVOID: Package-level mutable state
var cache = make(map[string]string)  // Shared, mutable

func Set(key, value string) {
    cache[key] = value  // Race condition!
}

// CORRECT: Encapsulated state
type Cache struct {
    mu   sync.RWMutex
    data map[string]string
}

func NewCache() *Cache {
    return &Cache{
        data: make(map[string]string),
    }
}

// ACCEPTABLE: Package-level constants
var (
    ErrNotFound = errors.New("not found")
    DefaultTimeout = 30 * time.Second
)
```

## Error Handling

See [Error Handling Documentation](./ex-so-stla-go__error-handling.md) for comprehensive coverage.

### Key Practices

```go
// Check errors immediately
result, err := DoSomething()
if err != nil {
    return fmt.Errorf("do something: %w", err)
}

// Wrap errors with context
if err != nil {
    return fmt.Errorf("failed to process user %d: %w", userID, err)
}

// Use sentinel errors for expected conditions
var ErrNotFound = errors.New("not found")

if errors.Is(err, ErrNotFound) {
    // Handle not found
}

// Custom error types for rich information
type ValidationError struct {
    Field string
    Err   error
}

var validationErr *ValidationError
if errors.As(err, &validationErr) {
    // Handle validation error
}

// Don't ignore errors
_ = file.Close()  // Document why if intentional

// Don't panic on expected errors
// Panic only for programmer errors or initialization
```

## Testing

### Test Organization

```go
// Test file naming: _test.go suffix
// user.go -> user_test.go

// Package naming
package user_test  // Black box testing (preferred for exported API)
// or
package user       // White box testing (for internals)

// Test function naming
func TestGetUser(t *testing.T)              // Basic test
func TestGetUser_NotFound(t *testing.T)     // Specific scenario
func TestUserService_GetUser_InvalidID(t *testing.T)  // Very specific

// Benchmark naming
func BenchmarkGetUser(b *testing.B)
```

### Table-Driven Tests

```go
func TestAdd(t *testing.T) {
    tests := []struct {
        name string
        a, b int
        want int
    }{
        {"positive numbers", 2, 3, 5},
        {"negative numbers", -1, -2, -3},
        {"mixed signs", -1, 2, 1},
        {"with zero", 5, 0, 5},
    }

    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            got := Add(tt.a, tt.b)
            if got != tt.want {
                t.Errorf("Add(%d, %d) = %d, want %d", tt.a, tt.b, got, tt.want)
            }
        })
    }
}
```

### Test Helpers

```go
// Helper marks function as test helper
func assertNoError(t *testing.T, err error) {
    t.Helper()  // Failure shows caller's line
    if err != nil {
        t.Fatalf("unexpected error: %v", err)
    }
}

// Setup/teardown
func setupTestDB(t *testing.T) *sql.DB {
    t.Helper()

    db := createTestDB()

    t.Cleanup(func() {
        db.Close()  // Cleanup after test
    })

    return db
}
```

### Test Coverage

```bash
# Run tests with coverage
go test -cover ./...

# Generate coverage report
go test -coverprofile=coverage.out ./...
go tool cover -html=coverage.out

# Coverage by function
go tool cover -func=coverage.out
```

### Mocking

```go
// Interface-based mocking
type UserRepository interface {
    GetUser(id int) (*User, error)
}

// Mock implementation
type MockUserRepository struct {
    GetUserFunc func(id int) (*User, error)
}

func (m *MockUserRepository) GetUser(id int) (*User, error) {
    if m.GetUserFunc != nil {
        return m.GetUserFunc(id)
    }
    return nil, errors.New("not implemented")
}

// Usage in tests
func TestService(t *testing.T) {
    repo := &MockUserRepository{
        GetUserFunc: func(id int) (*User, error) {
            return &User{ID: id, Name: "Test"}, nil
        },
    }

    service := NewService(repo)
    // Test service with mocked repository
}
```

### Test Naming

```go
// CORRECT: Descriptive test names
func TestGetUser_ValidID_ReturnsUser(t *testing.T)
func TestGetUser_InvalidID_ReturnsError(t *testing.T)
func TestGetUser_NotFound_ReturnsNotFoundError(t *testing.T)

// CORRECT: Use t.Run for subtests
func TestGetUser(t *testing.T) {
    t.Run("valid ID returns user", func(t *testing.T) {
        // Test
    })

    t.Run("invalid ID returns error", func(t *testing.T) {
        // Test
    })
}

// INCORRECT: Vague test names
func TestGetUser1(t *testing.T)
func TestGetUser2(t *testing.T)
```

## Performance

### Benchmarking

```go
func BenchmarkGetUser(b *testing.B) {
    repo := setupRepo()

    b.ResetTimer()  // Reset after setup

    for i := 0; i < b.N; i++ {
        _, _ = repo.GetUser(1)
    }
}

// Run benchmarks
// go test -bench=. -benchmem
```

### Pre-allocation

```go
// CORRECT: Pre-allocate slices when size known
result := make([]int, 0, len(input))
for _, v := range input {
    result = append(result, v*2)
}

// INCORRECT: Dynamic growth
var result []int  // Starts with zero capacity
for _, v := range input {
    result = append(result, v*2)  // May reallocate
}

// CORRECT: Pre-allocate maps
m := make(map[string]int, 100)  // Hint capacity

// CORRECT: strings.Builder for concatenation
var builder strings.Builder
builder.Grow(estimatedSize)
for _, s := range strings {
    builder.WriteString(s)
}
```

### Avoid Unnecessary Allocations

```go
// CORRECT: Reuse buffers
var buf bytes.Buffer
for _, item := range items {
    buf.Reset()  // Reuse buffer
    fmt.Fprintf(&buf, "Item: %v", item)
    process(buf.Bytes())
}

// CORRECT: Use pointers for large structs
type LargeStruct struct {
    data [1000]int
}

func Process(s *LargeStruct) {  // Pass pointer
    // No copy
}

// INCORRECT: Pass large struct by value
func Process(s LargeStruct) {  // Copies 8KB
    // ...
}
```

### Profiling

```bash
# CPU profiling
go test -cpuprofile=cpu.prof -bench=.
go tool pprof cpu.prof

# Memory profiling
go test -memprofile=mem.prof -bench=.
go tool pprof mem.prof

# Trace
go test -trace=trace.out
go tool trace trace.out

# Profile running application
import _ "net/http/pprof"

http.ListenAndServe("localhost:6060", nil)
# Visit http://localhost:6060/debug/pprof/
```

### Common Optimizations

```go
// Use sync.Pool for temporary objects
var bufferPool = sync.Pool{
    New: func() interface{} {
        return new(bytes.Buffer)
    },
}

func Process() {
    buf := bufferPool.Get().(*bytes.Buffer)
    defer bufferPool.Put(buf)

    buf.Reset()
    // Use buf
}

// Use string interning for many duplicate strings (Go 1.23+)
import "unique"

var stringPool = make(map[string]unique.Handle[string])

func internString(s string) string {
    if h, ok := stringPool[s]; ok {
        return h.Value()
    }
    h := unique.Make(s)
    stringPool[s] = h
    return h.Value()
}

// Avoid string <-> []byte conversions
func ProcessBytes(data []byte) {
    // Work with []byte directly
}

// Better than
func ProcessString(data string) {
    bytes := []byte(data)  // Allocation
    // Work with bytes
    result := string(bytes)  // Another allocation
}
```

## Concurrency

See [Concurrency and Parallelism Documentation](./ex-so-stla-go__concurrency-and-parallelism.md) for comprehensive coverage.

### Key Practices

```go
// Always check for goroutine leaks
func Process(ctx context.Context) {
    done := make(chan struct{})

    go func() {
        defer close(done)
        // Work
    }()

    select {
    case <-done:
        return
    case <-ctx.Done():
        return  // Goroutine continues! Potential leak!
    }
}

// BETTER: Signal cancellation
func Process(ctx context.Context) {
    done := make(chan struct{})
    cancel := make(chan struct{})

    go func() {
        defer close(done)
        select {
        case <-cancel:
            return
        default:
            // Work
        }
    }()

    select {
    case <-done:
    case <-ctx.Done():
        close(cancel)  // Signal goroutine to stop
        <-done         // Wait for cleanup
    }
}

// Use errgroup for coordinated error handling
import "golang.org/x/sync/errgroup"

func ProcessAll(items []Item) error {
    g := new(errgroup.Group)

    for _, item := range items {
        item := item  // Capture
        g.Go(func() error {
            return ProcessItem(item)
        })
    }

    return g.Wait()  // Returns first error
}

// Avoid shared mutable state
// Use channels or sync primitives
```

## Security

### Input Validation

```go
// Validate all inputs
func GetUser(id int) (*User, error) {
    if id <= 0 {
        return nil, ErrInvalidID
    }
    // Continue
}

// Sanitize strings
import "html"

func SaveComment(comment string) error {
    sanitized := html.EscapeString(comment)
    return db.Save(sanitized)
}
```

### SQL Injection Prevention

```go
// CORRECT: Use parameterized queries
db.Query("SELECT * FROM users WHERE id = ?", userID)

// INCORRECT: String concatenation
query := fmt.Sprintf("SELECT * FROM users WHERE id = %d", userID)
db.Query(query)  // SQL injection risk!
```

### Sensitive Data

```go
// Don't log sensitive data
log.Printf("User password: %s", password)  // Never!

// Redact in logs
type User struct {
    Name     string
    Password string `json:"-"`  // Never serialize
}

// Use secrets management
// Don't hardcode credentials
const apiKey = "abc123"  // Bad!

// Load from environment or secrets manager
apiKey := os.Getenv("API_KEY")
```

### Crypto

```go
// Use crypto/rand, not math/rand for security
import "crypto/rand"

token := make([]byte, 32)
_, err := rand.Read(token)

// Use bcrypt for passwords
import "golang.org/x/crypto/bcrypt"

hash, err := bcrypt.GenerateFromPassword([]byte(password), bcrypt.DefaultCost)

// Compare securely
err = bcrypt.CompareHashAndPassword(hash, []byte(password))
```

## Dependency Management

### Go Modules

```bash
# Initialize module
go mod init github.com/user/repo

# Add dependency
go get github.com/pkg/errors

# Update dependencies
go get -u ./...

# Tidy dependencies
go mod tidy

# Vendor dependencies (optional)
go mod vendor
```

### Version Selection

```go
// Use semantic versioning
require (
    github.com/pkg/errors v0.9.1
    github.com/stretchr/testify v1.8.0
)

// Pin to specific version
require github.com/pkg/errors v0.9.1

// Use latest
go get -u github.com/pkg/errors

// Use specific version
go get github.com/pkg/errors@v0.9.1

// Use commit
go get github.com/pkg/errors@commit-hash
```

### Private Modules

```bash
# Configure private module prefix
go env -w GOPRIVATE=github.com/mycompany/*

# Use SSH for Git
git config --global url."git@github.com:".insteadOf "https://github.com/"
```

### Minimal Dependencies

```go
// CORRECT: Use standard library when possible
import "encoding/json"  // Standard library

// INCORRECT: Add dependency for simple functionality
import "github.com/pkg/json"  // Unnecessary if encoding/json works
```

## Build and Deployment

### Build Configuration

```bash
# Basic build
go build -o myapp ./cmd/server

# Build with flags
go build -ldflags="-s -w" -o myapp

# Cross-compilation
GOOS=linux GOARCH=amd64 go build -o myapp-linux

# Multiple platforms
GOOS=darwin GOARCH=amd64 go build -o myapp-darwin
GOOS=windows GOARCH=amd64 go build -o myapp.exe

# Build tags
go build -tags=integration
```

### Version Information

```go
// Inject version at build time
var (
    Version   = "dev"
    GitCommit = "unknown"
    BuildTime = "unknown"
)

// Build with:
// go build -ldflags "-X main.Version=1.0.0 -X main.GitCommit=$(git rev-parse HEAD)"

func main() {
    fmt.Printf("Version: %s, Commit: %s\n", Version, GitCommit)
}
```

### Docker

```dockerfile
# Multi-stage build
FROM golang:1.25 AS builder

WORKDIR /app
COPY go.mod go.sum ./
RUN go mod download

COPY . .
RUN CGO_ENABLED=0 GOOS=linux go build -o server ./cmd/server

# Final stage
FROM alpine:latest

RUN apk --no-cache add ca-certificates
WORKDIR /root/

COPY --from=builder /app/server .

EXPOSE 8080
CMD ["./server"]
```

### Makefile

```makefile
.PHONY: build test lint clean

build:
 go build -o bin/app ./cmd/app

test:
 go test -v ./...

lint:
 golangci-lint run

clean:
 rm -rf bin/

install-tools:
 go install github.com/golangci/golangci-lint/cmd/golangci-lint@latest
```

## Documentation

### Package Documentation

```go
// Package user provides user management functionality.
//
// It handles user authentication, authorization, and profile management.
// Users can be created, retrieved, updated, and deleted through the
// Service interface.
//
// Example usage:
//
// service := user.NewService(repo)
// user, err := service.GetUser(ctx, 123)
// if err != nil {
//     log.Fatal(err)
// }
package user
```

### Function Documentation

```go
// GetUser retrieves a user by ID.
//
// It returns the user if found, or an error if the user doesn't exist
// or if there's a database error.
//
// Example:
//
// user, err := service.GetUser(ctx, 123)
// if errors.Is(err, ErrNotFound) {
//     // Handle not found
// }
func (s *Service) GetUser(ctx context.Context, id int) (*User, error) {
    // Implementation
}
```

### Examples

```go
// Example function (shows up in godoc)
func ExampleGetUser() {
    service := NewService(repo)
    user, err := service.GetUser(context.Background(), 123)
    if err != nil {
        log.Fatal(err)
    }
    fmt.Println(user.Name)
    // Output: John Doe
}
```

### README

```markdown
# Project Name

Brief description of the project.

## Installation

\`\`\`bash
go get github.com/user/project
\`\`\`

## Usage

\`\`\`go
package main

import "github.com/user/project"

func main() {
// Example usage
}
\`\`\`

## Features

- Feature 1
- Feature 2

## Documentation

See [godoc](https://pkg.go.dev/github.com/user/project).

## Contributing

Contributions welcome! Please read CONTRIBUTING.md.

## License

MIT License
```

## Code Review

### Review Checklist

**Correctness**:

- [ ] Code works as intended
- [ ] Edge cases handled
- [ ] Error handling is proper
- [ ] No nil pointer dereferences

**Style**:

- [ ] Follows Go conventions
- [ ] gofmt/goimports applied
- [ ] Meaningful names
- [ ] Clear comments

**Testing**:

- [ ] Tests included
- [ ] Tests pass
- [ ] Edge cases tested
- [ ] Good test coverage

**Performance**:

- [ ] No obvious performance issues
- [ ] Proper use of goroutines
- [ ] No unnecessary allocations
- [ ] Database queries optimized

**Security**:

- [ ] Input validation
- [ ] No hardcoded secrets
- [ ] SQL injection prevention
- [ ] Proper error messages (don't leak internals)

**Documentation**:

- [ ] Public API documented
- [ ] Complex logic explained
- [ ] README updated if needed
- [ ] Examples provided

### Common Review Comments

```go
// "Consider using early return"
// BEFORE
func Process(data []byte) error {
    if len(data) > 0 {
        result, err := parse(data)
        if err == nil {
            return save(result)
        } else {
            return err
        }
    } else {
        return ErrEmptyData
    }
}

// AFTER
func Process(data []byte) error {
    if len(data) == 0 {
        return ErrEmptyData
    }

    result, err := parse(data)
    if err != nil {
        return err
    }

    return save(result)
}

// "Use consistent naming"
// BEFORE
func (s *Service) getUser() {}
func (s *Service) FetchOrder() {}

// AFTER
func (s *Service) GetUser() {}  // Or both unexported
func (s *Service) GetOrder() {}

// "Add error context"
// BEFORE
return err

// AFTER
return fmt.Errorf("fetch user %d: %w", id, err)

// "Pre-allocate slice"
// BEFORE
var result []int
for i := 0; i < 1000; i++ {
    result = append(result, i)
}

// AFTER
result := make([]int, 0, 1000)
for i := 0; i < 1000; i++ {
    result = append(result, i)
}
```

## Refactoring

### When to Refactor

- Before adding new features
- After fixing bugs (to prevent recurrence)
- When code becomes hard to understand
- When tests are difficult to write
- During code review

### Safe Refactoring Steps

1. **Add tests** if missing
2. **Make one change** at a time
3. **Run tests** after each change
4. **Commit often** with clear messages
5. **Review carefully** before merging

### Common Refactorings

```go
// Extract function
// BEFORE
func Process() error {
    // 50 lines of validation logic
    // 50 lines of transformation logic
    // 50 lines of saving logic
}

// AFTER
func Process() error {
    if err := validate(); err != nil {
        return err
    }
    data, err := transform()
    if err != nil {
        return err
    }
    return save(data)
}

// Extract interface
// BEFORE
type Service struct {
    db *sql.DB  // Concrete dependency
}

// AFTER
type Service struct {
    repo Repository  // Interface dependency
}

type Repository interface {
    GetUser(id int) (*User, error)
}

// Introduce parameter object
// BEFORE
func CreateUser(name, email, phone, address string, age int) error

// AFTER
type CreateUserRequest struct {
    Name    string
    Email   string
    Phone   string
    Address string
    Age     int
}

func CreateUser(req CreateUserRequest) error
```

## Summary

### Key Best Practices by Category

**Code Organization**:

- Organize by domain, not layer
- Use internal/ for private packages
- Keep package structure flat
- One package, one responsibility

**Naming**:

- Short names for local scope
- Descriptive names for package scope
- camelCase for unexported, PascalCase for exported
- Avoid stuttering with package name

**Code Style**:

- Run gofmt/goimports
- Early return for errors
- Keep functions short (<50 lines)
- Comment exported symbols

**Testing**:

- Table-driven tests
- Black-box testing (package_test)
- Use test helpers
- Aim for good coverage

**Performance**:

- Pre-allocate when size known
- Avoid unnecessary allocations
- Use profiling before optimizing
- Benchmark critical paths

**Concurrency**:

- Check for goroutine leaks
- Use errgroup for coordination
- Avoid shared mutable state
- Always handle context cancellation

**Security**:

- Validate all inputs
- Use parameterized queries
- Never log sensitive data
- Use crypto/rand for security

### Quick Checklist

Before committing code:

- [ ] Code formatted with gofmt
- [ ] Passes golangci-lint
- [ ] All tests pass
- [ ] New tests added
- [ ] Documentation updated
- [ ] No hardcoded secrets
- [ ] Error handling complete
- [ ] Context properly used
- [ ] Interfaces over structs where appropriate
- [ ] Pre-allocation where beneficial

### Common Anti-Patterns to Avoid

- Ignoring errors
- Not using context
- Goroutine leaks
- Shared mutable state without synchronization
- Returning interface types
- Generic package names (util, common)
- Deep package nesting
- Large interfaces
- Hardcoded configuration
- String concatenation in loops

## Additional Resources

### Official Resources

- [Effective Go](https://go.dev/doc/effective_go)
- [Go Code Review Comments](https://github.com/golang/go/wiki/CodeReviewComments)
- [Go Blog](https://go.dev/blog/)
- [Go Wiki](https://github.com/golang/go/wiki)

### Style Guides

- [Uber Go Style Guide](https://github.com/uber-go/guide/blob/master/style.md)
- [Google Go Style Guide](https://google.github.io/styleguide/go/)
- [Go Standard Project Layout](https://github.com/golang-standards/project-layout)

### Tools

- [golangci-lint](https://golangci-lint.run/) - Meta-linter
- [staticcheck](https://staticcheck.io/) - Advanced static analysis
- [go vet](https://pkg.go.dev/cmd/vet) - Built-in analyzer
- [gofmt](https://pkg.go.dev/cmd/gofmt) - Formatter
- [goimports](https://pkg.go.dev/golang.org/x/tools/cmd/goimports) - Import manager

### Community Resources

- [Awesome Go](https://github.com/avelino/awesome-go)
- [Go Forum](https://forum.golangbridge.org/)
- [r/golang](https://reddit.com/r/golang)
- [Gophers Slack](https://gophers.slack.com/)

---

**Related Documentation**:

- [Go Idioms](./ex-so-stla-go__idioms.md)
- [Error Handling](./ex-so-stla-go__error-handling.md)
- [Concurrency and Parallelism](./ex-so-stla-go__concurrency-and-parallelism.md)
- [Interfaces and Composition](./ex-so-stla-go__interfaces-and-composition.md)

**Navigation**: [← Back to Golang Overview](./README.md)
