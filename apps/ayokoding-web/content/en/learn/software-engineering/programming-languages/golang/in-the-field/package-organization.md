---
title: "Package Organization"
date: 2026-02-04T00:00:00+07:00
draft: false
description: "Project layout and package structure patterns for Go applications"
weight: 10000034
tags: ["golang", "architecture", "package-structure", "project-layout"]
---

## Why Package Organization Matters

Package organization is critical for Go projects because it determines code discoverability, prevents circular dependencies, enforces architectural boundaries, and communicates system structure. Go's flat import system and explicit dependencies make package design a primary architectural tool.

**Core benefits**:

- **Clear boundaries**: Packages enforce separation of concerns
- **Prevent circular deps**: Good structure eliminates import cycles
- **Easy navigation**: Well-named packages guide developers
- **Testability**: Proper boundaries enable better testing

**Problem**: Without systematic package organization, codebases become tangled with circular dependencies, unclear boundaries, and difficult refactoring.

**Solution**: Follow Go's standard package layout conventions, starting with basic structure before adopting production patterns.

## Standard Library Package Pattern

Go's standard library demonstrates flat, focused packages with clear responsibilities.

**Basic package structure**:

```
myapp/
├── main.go           # Entry point
├── config.go         # Configuration
├── handler.go        # HTTP handlers
└── repository.go     # Data access
```

**Simple main package**:

```go
// File: main.go
package main
// => main package is required for executable
// => Only main package can produce binary

import (
    "fmt"
    // => Standard library import
    "log"
    // => Standard library logging
    "net/http"
    // => Standard library HTTP server
)

func main() {
    // => Entry point for application
    // => Only one main() allowed

    http.HandleFunc("/", handleRoot)
    // => Registers handler for root path
    // => handleRoot defined in same package

    log.Println("Starting server on :8080")
    // => Log to console
    // => Standard library logger

    if err := http.ListenAndServe(":8080", nil); err != nil {
        // => Starts HTTP server on port 8080
        // => Blocks until server stops
        // => Returns error if server fails
        log.Fatal(err)
        // => Logs error and exits with code 1
    }
}

func handleRoot(w http.ResponseWriter, r *http.Request) {
    // => HTTP handler function
    // => w writes response, r contains request
    fmt.Fprintf(w, "Hello, World!")
    // => Writes response body
}
```

**Multiple files, same package**:

```go
// File: config.go
package main
// => Same package as main.go
// => All files in directory share namespace

type Config struct {
    // => Config struct accessible in all main package files
    Port     int
    Database string
}

func LoadConfig() *Config {
    // => Exported function (starts with capital)
    // => Callable from main.go
    return &Config{
        Port:     8080,
        Database: "localhost:5432",
    }
}
```

**Limitations for larger projects**:

- Everything in one package (no boundaries)
- No code reuse across projects
- Difficult to test in isolation
- Name collisions in large teams

## Production Pattern: cmd/ vs pkg/ vs internal/

The standard Go project layout separates concerns into distinct directories.

**Standard project layout**:

```
myproject/
├── cmd/                    # Application entry points
│   ├── server/            # Server binary
│   │   └── main.go        # Server entry point
│   └── cli/               # CLI binary
│       └── main.go        # CLI entry point
├── internal/              # Private application code
│   ├── handler/           # HTTP handlers
│   ├── repository/        # Data access
│   └── service/           # Business logic
├── pkg/                   # Public library code
│   └── calculator/        # Reusable calculator
└── go.mod                 # Module definition
```

**cmd/ directory** (multiple binaries):

```go
// File: cmd/server/main.go
package main
// => main package for server binary
// => Produces executable: ./myproject/server

import (
    "log"
    "net/http"

    "myproject/internal/handler"
    // => Import internal package
    // => Path starts with module name from go.mod
    "myproject/internal/service"
    // => Another internal import
)

func main() {
    // => Server entry point
    // => Minimal logic - delegates to internal packages

    svc := service.NewUserService()
    // => Creates business logic service
    // => service.NewUserService from internal/service

    h := handler.NewUserHandler(svc)
    // => Creates HTTP handler with service dependency
    // => handler.NewUserHandler from internal/handler

    http.Handle("/users", h)
    // => Registers handler
    // => Wiring dependencies in main

    log.Fatal(http.ListenAndServe(":8080", nil))
    // => Starts server
    // => main() only orchestrates, doesn't implement
}
```

```go
// File: cmd/cli/main.go
package main
// => Different main package
// => Produces separate executable: ./myproject/cli

import (
    "fmt"
    "os"

    "myproject/internal/service"
    // => Reuses same internal service
    // => Shared code between server and CLI
)

func main() {
    // => CLI entry point
    // => Different binary, shared internal code

    svc := service.NewUserService()
    // => Same service as server
    // => Code reuse via internal/

    users, err := svc.ListUsers()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error: %v\n", err)
        os.Exit(1)
    }

    for _, user := range users {
        fmt.Println(user.Name)
        // => Prints user names to stdout
    }
}
```

**internal/ directory** (private packages):

```go
// File: internal/service/user.go
package service
// => service package in internal/
// => Only importable by this project
// => External projects cannot import internal/

import (
    "myproject/internal/repository"
    // => Internal package can import other internal packages
    // => Builds dependency graph
)

type UserService struct {
    // => Business logic service
    // => Exported type (capital letter)
    repo repository.UserRepository
    // => Depends on repository interface
    // => Lowercase field (private)
}

func NewUserService() *UserService {
    // => Constructor function (Go convention)
    // => Exported (capital N)
    return &UserService{
        repo: repository.NewPostgresUserRepository(),
        // => Wires concrete repository implementation
        // => Could be swapped for testing
    }
}

func (s *UserService) ListUsers() ([]User, error) {
    // => Method on UserService
    // => s is receiver (pointer for mutations)
    // => Exported method (capital L)

    return s.repo.FindAll()
    // => Delegates to repository
    // => Service coordinates, repository executes
}
```

```go
// File: internal/repository/user.go
package repository
// => repository package for data access
// => Separate from service (layered architecture)

import (
    "database/sql"
    // => Standard library database/sql
)

type UserRepository interface {
    // => Interface defines contract
    // => Enables testing with mocks
    FindAll() ([]User, error)
}

type PostgresUserRepository struct {
    // => Concrete implementation
    // => Implements UserRepository interface
    db *sql.DB
    // => Database connection
}

func NewPostgresUserRepository() *PostgresUserRepository {
    // => Constructor for concrete repository
    return &PostgresUserRepository{
        // Initialize db connection here
    }
}

func (r *PostgresUserRepository) FindAll() ([]User, error) {
    // => Implements UserRepository.FindAll
    // => r is receiver
    rows, err := r.db.Query("SELECT id, name FROM users")
    // => Executes SQL query
    // => rows contains result set

    if err != nil {
        return nil, err
        // => Returns error immediately
    }
    defer rows.Close()
    // => Cleanup result set

    var users []User
    // => Accumulator slice

    for rows.Next() {
        // => Iterates over result rows
        var u User
        if err := rows.Scan(&u.ID, &u.Name); err != nil {
            return nil, err
            // => Scans row into User struct
        }
        users = append(users, u)
        // => Appends to result slice
    }

    return users, nil
    // => Returns all users
}
```

**pkg/ directory** (public library code):

```go
// File: pkg/calculator/calculator.go
package calculator
// => Public package (not in internal/)
// => External projects can import this
// => Import path: myproject/pkg/calculator

// Add returns the sum of two integers
// => Public function (exported)
// => Comment documents behavior (godoc)
func Add(a, b int) int {
    return a + b
    // => Simple calculation
    // => No internal dependencies
}

// Multiply returns the product of two integers
// => Another public function
func Multiply(a, b int) int {
    return a * b
}
```

**Trade-offs**:

| Approach                          | Pros                                               | Cons                             |
| --------------------------------- | -------------------------------------------------- | -------------------------------- |
| Single package (all in main)      | Simple, fast iteration                             | No boundaries, testing difficult |
| cmd/internal/pkg layout (standard | Clear boundaries, testable, prevents circular deps | More files, upfront design       |

**When to use each**:

- **Single package**: Prototypes, scripts, tools under 500 lines
- **cmd/internal/pkg**: Production applications, multi-binary projects, team codebases

## Circular Dependency Prevention

Go's compiler rejects circular imports, forcing good design.

**Problem example** (would fail to compile):

```
package service imports package repository
package repository imports package service
→ Circular dependency detected
```

**Solution 1: Extract interface**:

```go
// File: internal/model/user.go
package model
// => Shared model package
// => Defines types used by multiple layers

type User struct {
    // => User model
    // => No dependencies on service or repository
    ID   int
    Name string
}
```

```go
// File: internal/repository/user.go
package repository

import "myproject/internal/model"
// => Imports model, not service
// => One-way dependency

type UserRepository interface {
    // => Interface in repository package
    FindByID(id int) (*model.User, error)
}
```

```go
// File: internal/service/user.go
package service

import (
    "myproject/internal/model"
    "myproject/internal/repository"
    // => Imports repository, not vice versa
    // => One-way dependency
)

type UserService struct {
    repo repository.UserRepository
    // => Depends on interface, not concrete type
}
```

**Solution 2: Dependency inversion**:

```go
// File: internal/service/user.go
package service

import "myproject/internal/model"

// EmailSender is defined in service package
// => Service owns the interface
// => Repository implements it (dependency inversion)
type EmailSender interface {
    Send(to string, subject string, body string) error
    // => Service defines what it needs
}

type UserService struct {
    emailSender EmailSender
    // => Depends on interface
    // => Concrete implementation injected
}
```

```go
// File: internal/notification/email.go
package notification

import "myproject/internal/service"
// => notification imports service (for interface)
// => Service does NOT import notification
// => Breaks potential circular dependency

type SMTPEmailSender struct {
    // => Concrete implementation
}

func (s *SMTPEmailSender) Send(to, subject, body string) error {
    // => Implements service.EmailSender interface
    // => No explicit "implements" keyword in Go
    return nil // SMTP logic here
}
```

**Dependency direction rules**:

- **Handler → Service → Repository** (one-way, no reverse imports)
- **Service defines interfaces**, Repository/External packages implement them
- **Shared types** go in model or separate package

## Domain-Driven Package Organization

For complex domains, organize by feature rather than layer.

**Standard layered structure** (by technical layer):

```
internal/
├── handler/       # All HTTP handlers
├── service/       # All business logic
└── repository/    # All data access
```

**Limitation**: Related code scattered across layers, hard to see feature boundaries.

**Domain-driven structure** (by feature):

```
internal/
├── user/                  # User domain
│   ├── handler.go        # User HTTP handlers
│   ├── service.go        # User business logic
│   ├── repository.go     # User data access
│   └── user.go           # User models
├── order/                # Order domain
│   ├── handler.go        # Order HTTP handlers
│   ├── service.go        # Order business logic
│   ├── repository.go     # Order data access
│   └── order.go          # Order models
└── shared/               # Shared utilities
    └── database.go       # Shared DB connection
```

**User domain example**:

```go
// File: internal/user/user.go
package user
// => user package owns User domain
// => All user-related code in one package

type User struct {
    // => User model
    // => Private fields (lowercase)
    id    int
    name  string
    email string
}

func NewUser(name, email string) *User {
    // => Constructor with validation
    // => Encapsulates User creation
    return &User{name: name, email: email}
}

func (u *User) Name() string {
    // => Getter method
    // => Encapsulates field access
    return u.name
}
```

```go
// File: internal/user/service.go
package user
// => Same package, different file
// => All user domain files share namespace

type Service struct {
    // => User service
    // => Coordinates user operations
    repo Repository
    // => Depends on user.Repository interface
}

func NewService(repo Repository) *Service {
    // => Service constructor
    return &Service{repo: repo}
}

func (s *Service) CreateUser(name, email string) (*User, error) {
    // => Business logic method
    // => Creates and persists user

    user := NewUser(name, email)
    // => Uses user.NewUser constructor

    if err := s.repo.Save(user); err != nil {
        return nil, err
    }

    return user, nil
}
```

**Trade-offs**:

| Approach                   | Pros                                   | Cons                                               |
| -------------------------- | -------------------------------------- | -------------------------------------------------- |
| Layered (by layer)         | Clear separation of concerns           | Related code scattered, feature changes touch many |
| Domain-driven (by feature) | Related code together, feature-focused | Less obvious technical layers, shared code design  |

**When to use each**:

- **Layered**: CRUD applications, simple domains, small teams
- **Domain-driven**: Complex domains, microservices, large teams, DDD practices

## Common Patterns

**Package naming conventions**:

- Use short, descriptive names (user, not usermanagement)
- Avoid stutter (user.User, not user.UserModel)
- Singular names (user, not users)
- Avoid generic names (util, common, base)

**Export rules**:

```go
// Exported (accessible from other packages)
type User struct {}      // Capital letter
func NewUser() *User {}  // Capital letter

// Unexported (package-private)
type validator struct {} // Lowercase
func validate() bool {}  // Lowercase
```

**Interface placement**:

- Define interfaces where they're used, not where they're implemented
- Service defines interface, repository implements it
- Promotes dependency inversion

**Vendor directory** (optional):

```bash
go mod vendor
# => Copies dependencies to vendor/
# => Commits to version control if needed
# => Ensures reproducible builds without network
```

## Best Practices

**Start simple, refactor when needed**:

1. Begin with single package for prototypes
2. Split into cmd/internal when building real application
3. Add pkg/ only for code meant to be imported by other projects
4. Adopt domain-driven structure when features become complex

**Avoid over-engineering**:

- Don't create packages until you need them
- Don't extract shared code until 3+ uses
- Don't optimize for imagined future requirements

**Testing organization**:

```
internal/user/
├── user.go           # Implementation
├── user_test.go      # Tests in same package (white-box)
├── service.go        # Service implementation
└── service_test.go   # Service tests
```

**Black-box testing** (testing as external consumer):

```go
// File: internal/user/user_test.go
package user_test
// => _test suffix makes this external package
// => Can only use exported API (black-box testing)

import (
    "testing"
    "myproject/internal/user"
    // => Import package being tested
)

func TestNewUser(t *testing.T) {
    // => Tests user.NewUser as external consumer
    u := user.NewUser("Alice", "alice@example.com")
    // => Can only call exported functions
}
```

## Summary

Go package organization principles:

- **Start simple**: Single package for small projects
- **cmd/internal/pkg**: Standard layout for production applications
- **internal/**: Enforces privacy at compiler level
- **Circular deps**: Prevented by extracting interfaces and shared types
- **Domain-driven**: Group by feature for complex domains
- **Flat imports**: Go prefers flat structure over deep nesting

**Progressive adoption**:

1. Prototype: Single package (main)
2. Application: cmd/ + internal/ structure
3. Library: Add pkg/ for reusable code
4. Complex domain: Reorganize by feature (domain-driven)
