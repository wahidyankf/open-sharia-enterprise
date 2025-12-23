---
title: "Document Code Effectively"
date: 2025-12-17T13:19:07+07:00
draft: false
weight: 1000013
description: "Write clear godoc comments, meaningful documentation, and comprehensive package documentation"
tags: ["golang", "documentation", "godoc", "comments", "code-quality"]
categories: ["learn"]
---

## Problem

Go's minimal documentation tooling means developers must write clear, concise comments following specific conventions. Poor documentation forces readers to study implementation details. Over-documentation clutters code with noise. Balancing thoroughness with brevity while following godoc conventions is essential.

This guide shows effective documentation practices in Go.

## Godoc Comment Conventions

### Package Documentation

```go
// Package user provides user management and authentication services.
//
// This package handles user registration, authentication, password management,
// and session handling. All operations are designed for concurrent use.
//
// Basic usage:
//
//	service := user.NewService(db)
//	user, err := service.Register("alice@example.com", "password123")
//	if err != nil {
//	    log.Fatal(err)
//	}
//
//	authenticated, err := service.Authenticate("alice@example.com", "password123")
//
// Thread Safety:
//
// All exported functions and methods are safe for concurrent use.
package user
```

**Why it matters**: Package documentation appears first in godoc output. Provides overview, usage examples, and important constraints. Start with "Package name" for proper godoc rendering.

### Function Documentation

```go
// Register creates a new user account with the specified email and password.
//
// The password is hashed using bcrypt before storage. A verification email
// is sent to the provided email address. The user account remains inactive
// until email verification completes.
//
// Returns an error if:
//   - email is invalid or already registered
//   - password is less than 8 characters
//   - email sending fails
//
// Example:
//
//	user, err := Register("alice@example.com", "securepass123")
//	if err != nil {
//	    log.Fatal(err)
//	}
func Register(email, password string) (*User, error) {
    // Implementation...
}

// ✅ Short functions can have brief comments
// IsValidEmail checks if the email format is valid.
func IsValidEmail(email string) bool {
    return emailRegex.MatchString(email)
}
```

### Type Documentation

```go
// User represents a registered user account.
//
// Users have a unique email address and encrypted password. The account
// may be active or inactive depending on email verification status.
type User struct {
    // ID is the unique user identifier, generated on creation.
    ID string

    // Email is the user's email address (unique and validated).
    Email string

    // Active indicates if the user has verified their email.
    Active bool

    // hashedPassword stores the bcrypt hash (not exported).
    hashedPassword string
}

// UserRepository defines storage operations for users.
//
// Implementations must be safe for concurrent use.
type UserRepository interface {
    // FindByID retrieves a user by their unique ID.
    // Returns nil if the user doesn't exist.
    FindByID(id string) (*User, error)

    // FindByEmail retrieves a user by email address.
    // Returns nil if no user with that email exists.
    FindByEmail(email string) (*User, error)

    // Save persists a user, creating or updating as needed.
    Save(user *User) error
}
```

### Method Documentation

```go
// Validate checks if the user has all required fields.
//
// Returns an error if email is invalid or empty, or if the
// password hash is missing.
func (u *User) Validate() error {
    // Implementation...
}

// SetPassword hashes and stores the provided password.
//
// The password must be at least 8 characters. Returns an error
// if the password is too short or hashing fails.
func (u *User) SetPassword(password string) error {
    // Implementation...
}
```

## Writing Effective Comments

### When to Comment

```go
// ✅ Explain WHY, not WHAT
func CalculateDiscount(total float64, customerType string) float64 {
    // Premium customers get 10% discount to match competitor pricing
    if customerType == "premium" {
        return total * 0.10
    }

    // First-time customers get 5% to encourage purchases
    if customerType == "new" {
        return total * 0.05
    }

    return 0
}

// ✅ Document non-obvious algorithms
// Hash uses FNV-1a algorithm for speed over cryptographic strength.
// Suitable for hash tables but NOT for security purposes.
func Hash(data []byte) uint64 {
    h := fnv.New64a()
    h.Write(data)
    return h.Sum64()
}

// ✅ Warn about important constraints
// ProcessOrders modifies the input slice by removing invalid orders.
// Pass a copy if you need to preserve the original.
func ProcessOrders(orders []*Order) error {
    // Implementation...
}

// ✅ Document concurrency safety
// Cache is safe for concurrent use by multiple goroutines.
type Cache struct {
    mu    sync.RWMutex
    items map[string]interface{}
}

// ❌ Don't state the obvious
// GetName returns the name
func (u *User) GetName() string {
    return u.Name
}

// ❌ Outdated comments are worse than no comments
// TODO: Remove this after migration (migration was 2 years ago!)
func legacyFunction() {}
```

### Code Examples in Comments

```go
// ProcessPayment charges a card and records the transaction.
//
// Example usage:
//
//	payment := &Payment{
//	    Amount:  100.00,
//	    CardNum: "4532-1234-5678-9010",
//	}
//
//	result, err := ProcessPayment(payment)
//	if err != nil {
//	    log.Fatal(err)
//	}
//
//	fmt.Printf("Transaction ID: %s\n", result.TransactionID)
func ProcessPayment(payment *Payment) (*PaymentResult, error) {
    // Implementation...
}
```

## Documentation for Exported vs Unexported

### Exported Symbols

```go
// ✅ All exported symbols should be documented

// Service provides user management operations.
type Service struct {
    repo UserRepository
}

// NewService creates a new user service with the given repository.
func NewService(repo UserRepository) *Service {
    return &Service{repo: repo}
}

// GetUser retrieves a user by ID.
// Returns nil if the user doesn't exist.
func (s *Service) GetUser(id string) (*User, error) {
    // Implementation...
}
```

### Unexported Symbols

```go
// ✅ Document complex unexported functions
// validatePasswordStrength checks password meets security requirements.
// Requires minimum 8 characters with mix of letters and numbers.
func validatePasswordStrength(password string) error {
    // Implementation...
}

// ✅ Simple unexported functions may not need comments
func isEmailTaken(email string) bool {
    // Implementation obvious from name
}
```

## Package-Level Documentation

### doc.go File

```go
// Package config provides application configuration management.
//
// Configuration loads from environment variables, config files, or defaults,
// in that priority order. The package supports hot-reloading configuration
// from files without restarting the application.
//
// # Environment Variables
//
// The following environment variables are recognized:
//
//   - DATABASE_URL: PostgreSQL connection string (required)
//   - PORT: HTTP server port (default: 8080)
//   - LOG_LEVEL: Logging verbosity: debug, info, warn, error (default: info)
//   - MAX_CONNECTIONS: Maximum database connections (default: 10)
//
// # Configuration Files
//
// Configuration files use YAML format. Place config.yaml in the current
// directory, $HOME/.myapp/, or /etc/myapp/.
//
// Example config.yaml:
//
//	database:
//	  url: postgresql://localhost/mydb
//	  max_connections: 20
//	server:
//	  port: 8080
//	  timeout: 30s
//
// # Usage
//
// Load configuration at application startup:
//
//	cfg, err := config.Load()
//	if err != nil {
//	    log.Fatal(err)
//	}
//
//	server := NewServer(cfg)
//	server.Start()
//
// Watch for configuration changes:
//
//	config.Watch(func(cfg *Config) {
//	    log.Println("Configuration reloaded")
//	    server.UpdateConfig(cfg)
//	})
package config
```

### Grouping Constants and Variables

```go
// HTTP status codes for API responses.
const (
    StatusOK       = 200
    StatusCreated  = 201
    StatusBadRequest = 400
    StatusNotFound = 404
)

// Common error messages.
var (
    ErrNotFound      = errors.New("resource not found")
    ErrUnauthorized  = errors.New("unauthorized access")
    ErrInvalidInput  = errors.New("invalid input data")
)
```

## README Files

### Package README

````markdown
# User Package

User management and authentication for MyApp.

## Features

- User registration with email verification
- Password hashing with bcrypt
- Session management
- Thread-safe operations

## Installation

```bash
go get github.com/myorg/myapp/user
```
````

## Quick Start

```go
import "github.com/myorg/myapp/user"

func main() {
    service := user.NewService(db)

    // Register new user
    user, err := service.Register("alice@example.com", "password123")
    if err != nil {
        log.Fatal(err)
    }

    // Authenticate
    authenticated, err := service.Authenticate("alice@example.com", "password123")
    if err != nil {
        log.Fatal(err)
    }
}
```

## Documentation

See [godoc](https://pkg.go.dev/github.com/myorg/myapp/user) for detailed API documentation.

## License

MIT License

````

## TODO and FIXME Comments

```go
// ✅ TODO comments for future work
// TODO: Add rate limiting to prevent brute force attacks
func Authenticate(email, password string) (bool, error) {
    // Implementation...
}

// ✅ FIXME for known issues
// FIXME: Race condition when multiple goroutines access cache
func (c *Cache) Get(key string) (interface{}, bool) {
    // Implementation...
}

// ✅ HACK for temporary workarounds
// HACK: Workaround for third-party library bug #123
// Remove once library is updated to v2.0
func processData(data []byte) error {
    // Implementation...
}

// ✅ NOTE for important information
// NOTE: This function assumes data is already validated.
// Caller must validate before calling.
func saveData(data *Data) error {
    // Implementation...
}
````

## Deprecation Notices

```go
// ✅ Mark deprecated functions
// Deprecated: Use NewServiceV2 instead. This function will be removed in v2.0.
func NewService(db *sql.DB) *Service {
    return &Service{db: db}
}

// NewServiceV2 creates a service with improved error handling.
func NewServiceV2(db *sql.DB) (*Service, error) {
    // New implementation...
}
```

## Generating Documentation

### godoc Command

```bash
# Install godoc
go install golang.org/x/tools/cmd/godoc@latest

# Run local godoc server
godoc -http=:6060

# View in browser
open http://localhost:6060/pkg/github.com/yourorg/yourproject/
```

### pkgsite (Modern Alternative)

```bash
# Install pkgsite
go install golang.org/x/pkgsite/cmd/pkgsite@latest

# Run local pkgsite server
pkgsite -http=:8080

# View in browser
open http://localhost:8080/github.com/yourorg/yourproject
```

## Documentation Testing

### Example Tests

```go
// Example functions appear in godoc
func ExampleService_Register() {
    service := NewService(db)
    user, err := service.Register("alice@example.com", "password123")
    if err != nil {
        log.Fatal(err)
    }

    fmt.Println(user.Email)
    // Output: alice@example.com
}

func ExampleService_Authenticate() {
    service := NewService(db)

    authenticated, err := service.Authenticate("alice@example.com", "password123")
    if err != nil {
        log.Fatal(err)
    }

    fmt.Println(authenticated)
    // Output: true
}

// Examples without output comments are compiled but not executed
func ExampleService_advanced() {
    service := NewService(db)

    // Complex example showing advanced usage
    service.Configure(Options{
        MaxAttempts: 3,
        Timeout:     30 * time.Second,
    })
}
```

## Summary

Go documentation follows godoc conventions where comments directly preceding exported declarations become documentation. Start package comments with "Package name" and provide overview, usage examples, and constraints. Document all exported types, functions, constants, and variables.

Function documentation describes what the function does, parameters it accepts, return values it produces, and errors it may return. Include code examples in comments using indented blocks. Focus on behavior and usage, not implementation details.

Comments explain why code exists and why it's written a particular way, not what the code does. Document non-obvious algorithms, important constraints, concurrency safety, and business logic rationale. Avoid stating the obvious - clear code needs fewer comments.

Package-level documentation in doc.go files provides comprehensive package overview. Include sections for environment variables, configuration format, usage examples, and thread safety guarantees. Use Markdown-style headings and formatting.

Exported symbols require documentation, unexported symbols document only when complex or non-obvious. This balances thoroughness with maintainability. Public API needs complete documentation, internal implementation needs clarity through code structure and selective comments.

README files complement godoc with installation instructions, quick start examples, and project overview. Keep examples working and up-to-date. Link to godoc for detailed API reference.

TODO and FIXME comments track future work and known issues. Include context about why the work is needed and when it should be addressed. Remove or update stale comments - outdated comments mislead more than they help.

Example tests demonstrate API usage and appear in godoc automatically. Write examples for common use cases. Use Output comments to verify examples compile and run correctly. Examples serve as both documentation and tests.

godoc and pkgsite generate browsable documentation from comments. Run locally during development to verify documentation renders correctly. Clear, comprehensive documentation makes packages easier to use and maintain.

## Related Content

- [Go Best Practices and Idioms](/en/learn/software-engineering/programming-language/golang/explanation/best-practices)
- [How to Write Effective Tests](/en/learn/software-engineering/programming-language/golang/how-to/write-effective-tests)
- [How to Design Interfaces Properly](/en/learn/software-engineering/programming-language/golang/how-to/design-interfaces-properly)
