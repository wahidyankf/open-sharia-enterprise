---
title: "How to Use Dependency Injection Patterns"
date: 2025-12-21T00:00:00+07:00
draft: false
weight: 1000200
description: "Practical techniques for implementing dependency injection in Go using constructor injection, interface-based DI, and Wire for scalable applications"
---

## Problem

Hardcoded dependencies make code difficult to test and tightly coupled. Creating dependencies inside structs prevents mocking and violates the dependency inversion principle.

```go
// Problematic - tight coupling
type UserService struct {
    repo *PostgresRepository  // Hardcoded concrete type
}

func NewUserService() *UserService {
    return &UserService{
        repo: &PostgresRepository{},  // Created inside
    }
}
```

This guide shows practical dependency injection patterns in Go.

## Solution

### 1. Constructor Injection

**Basic constructor injection**:

```go
// Define interfaces
type UserRepository interface {
    FindByID(id int) (*User, error)
    Save(user *User) error
}

type EmailService interface {
    Send(to, subject, body string) error
}

// Service depends on interfaces
type UserService struct {
    repo  UserRepository
    email EmailService
}

// Constructor injects dependencies
func NewUserService(repo UserRepository, email EmailService) *UserService {
    return &UserService{
        repo:  repo,
        email: email,
    }
}

func (s *UserService) CreateUser(username, email string) error {
    user := &User{Username: username, Email: email}

    if err := s.repo.Save(user); err != nil {
        return err
    }

    return s.email.Send(email, "Welcome", "Welcome to our platform")
}

// Concrete implementations
type PostgresRepository struct {
    db *sql.DB
}

func NewPostgresRepository(db *sql.DB) *PostgresRepository {
    return &PostgresRepository{db: db}
}

func (r *PostgresRepository) FindByID(id int) (*User, error) {
    // Implementation
    return nil, nil
}

func (r *PostgresRepository) Save(user *User) error {
    // Implementation
    return nil
}

type SMTPEmailService struct {
    host string
    port int
}

func NewSMTPEmailService(host string, port int) *SMTPEmailService {
    return &SMTPEmailService{host: host, port: port}
}

func (s *SMTPEmailService) Send(to, subject, body string) error {
    // Implementation
    return nil
}

// Manual wiring
func main() {
    db, _ := sql.Open("postgres", "...")
    repo := NewPostgresRepository(db)
    email := NewSMTPEmailService("smtp.example.com", 587)
    userService := NewUserService(repo, email)

    userService.CreateUser("john", "john@example.com")
}
```

### 2. Interface-Based Dependency Injection

**Dependency inversion**:

```go
// Application layer - defines interfaces
package app

type UserRepository interface {
    Save(user User) error
    Find(id int) (User, error)
}

type NotificationService interface {
    Notify(userID int, message string) error
}

type UserService struct {
    repo         UserRepository
    notification NotificationService
}

func NewUserService(repo UserRepository, notif NotificationService) *UserService {
    return &UserService{
        repo:         repo,
        notification: notif,
    }
}

// Infrastructure layer - implements interfaces
package infrastructure

type PostgresUserRepository struct {
    db *sql.DB
}

func (r *PostgresUserRepository) Save(user app.User) error {
    // PostgreSQL implementation
    return nil
}

func (r *PostgresUserRepository) Find(id int) (app.User, error) {
    // PostgreSQL implementation
    return app.User{}, nil
}

type EmailNotificationService struct {
    client *smtp.Client
}

func (n *EmailNotificationService) Notify(userID int, message string) error {
    // Email notification implementation
    return nil
}

// Wiring
func main() {
    db, _ := sql.Open("postgres", "...")
    repo := &infrastructure.PostgresUserRepository{db: db}
    notif := &infrastructure.EmailNotificationService{}

    userService := app.NewUserService(repo, notif)
}
```

### 3. Google Wire Code Generation

**Automated dependency injection with Wire**:

```go
// wire.go
//go:build wireinject
// +build wireinject

package main

import (
    "github.com/google/wire"
)

// Providers
func NewDatabase(config Config) (*sql.DB, error) {
    return sql.Open("postgres", config.DatabaseURL)
}

func NewUserRepository(db *sql.DB) UserRepository {
    return &PostgresUserRepository{db: db}
}

func NewEmailService(config Config) EmailService {
    return &SMTPEmailService{
        host: config.SMTPHost,
        port: config.SMTPPort,
    }
}

func NewUserService(repo UserRepository, email EmailService) *UserService {
    return &UserService{
        repo:  repo,
        email: email,
    }
}

// Wire set
var AppSet = wire.NewSet(
    NewDatabase,
    NewUserRepository,
    NewEmailService,
    NewUserService,
)

// Injector
func InitializeUserService(config Config) (*UserService, error) {
    wire.Build(AppSet)
    return nil, nil  // Wire generates the actual implementation
}

// Generated code (wire_gen.go) - created by 'wire' command
// func InitializeUserService(config Config) (*UserService, error) {
//     db, err := NewDatabase(config)
//     if err != nil {
//         return nil, err
//     }
//     repo := NewUserRepository(db)
//     email := NewEmailService(config)
//     service := NewUserService(repo, email)
//     return service, nil
// }

// Usage
func main() {
    config := LoadConfig()
    userService, err := InitializeUserService(config)
    if err != nil {
        log.Fatal(err)
    }

    userService.CreateUser("john", "john@example.com")
}
```

### 4. Testing with Dependency Injection

**Mock implementations for testing**:

```go
// Mock repository for testing
type MockUserRepository struct {
    SaveFunc    func(user *User) error
    FindByIDFunc func(id int) (*User, error)
}

func (m *MockUserRepository) Save(user *User) error {
    if m.SaveFunc != nil {
        return m.SaveFunc(user)
    }
    return nil
}

func (m *MockUserRepository) FindByID(id int) (*User, error) {
    if m.FindByIDFunc != nil {
        return m.FindByIDFunc(id)
    }
    return &User{ID: id}, nil
}

// Mock email service
type MockEmailService struct {
    SendFunc func(to, subject, body string) error
}

func (m *MockEmailService) Send(to, subject, body string) error {
    if m.SendFunc != nil {
        return m.SendFunc(to, subject, body)
    }
    return nil
}

// Test with mocks
func TestUserService_CreateUser(t *testing.T) {
    mockRepo := &MockUserRepository{
        SaveFunc: func(user *User) error {
            if user.Username == "" {
                return errors.New("username required")
            }
            return nil
        },
    }

    mockEmail := &MockEmailService{
        SendFunc: func(to, subject, body string) error {
            if to == "" {
                return errors.New("email required")
            }
            return nil
        },
    }

    service := NewUserService(mockRepo, mockEmail)

    err := service.CreateUser("john", "john@example.com")
    if err != nil {
        t.Errorf("unexpected error: %v", err)
    }
}

// Using testify/mock
import "github.com/stretchr/testify/mock"

type MockRepository struct {
    mock.Mock
}

func (m *MockRepository) Save(user *User) error {
    args := m.Called(user)
    return args.Error(0)
}

func TestWithTestify(t *testing.T) {
    mockRepo := new(MockRepository)
    mockRepo.On("Save", mock.Anything).Return(nil)

    service := NewUserService(mockRepo, nil)
    service.CreateUser("john", "john@example.com")

    mockRepo.AssertExpectations(t)
}
```

## How It Works

### Dependency Injection Flow

```mermaid
%%{ Color Palette: Blue #0173B2, Orange #DE8F05, Teal #029E73, Purple #CC78BC }%%
graph TD
    A[Main/Injector] --> B[Create Dependencies]
    B --> C[Database Connection]
    B --> D[Repository]
    B --> E[Email Service]

    C --> D
    D --> F[UserService]
    E --> F

    F --> G[Application Logic]

    style A fill:#0173B2,stroke:#000000,color:#FFFFFF
    style B fill:#DE8F05,stroke:#000000,color:#FFFFFF
    style F fill:#029E73,stroke:#000000,color:#FFFFFF
    style G fill:#CC78BC,stroke:#000000,color:#FFFFFF
```

**Key concepts**:

1. **Constructor Injection**: Dependencies passed through constructor
2. **Interface Abstraction**: Depend on interfaces, not concrete types
3. **Dependency Inversion**: High-level modules don't depend on low-level modules
4. **Testability**: Easy to mock dependencies for unit tests

## Variations

### Option Pattern

```go
type UserServiceOption func(*UserService)

func WithCache(cache Cache) UserServiceOption {
    return func(s *UserService) {
        s.cache = cache
    }
}

func WithLogger(logger Logger) UserServiceOption {
    return func(s *UserService) {
        s.logger = logger
    }
}

func NewUserService(repo UserRepository, opts ...UserServiceOption) *UserService {
    s := &UserService{repo: repo}

    for _, opt := range opts {
        opt(s)
    }

    return s
}

// Usage
service := NewUserService(repo,
    WithCache(redisCache),
    WithLogger(logger),
)
```

### Provider Pattern

```go
type Provider struct {
    db     *sql.DB
    config Config
}

func NewProvider(config Config) (*Provider, error) {
    db, err := sql.Open("postgres", config.DatabaseURL)
    if err != nil {
        return nil, err
    }

    return &Provider{
        db:     db,
        config: config,
    }, nil
}

func (p *Provider) UserRepository() UserRepository {
    return &PostgresUserRepository{db: p.db}
}

func (p *Provider) EmailService() EmailService {
    return &SMTPEmailService{
        host: p.config.SMTPHost,
        port: p.config.SMTPPort,
    }
}

func (p *Provider) UserService() *UserService {
    return NewUserService(
        p.UserRepository(),
        p.EmailService(),
    )
}
```

## Common Pitfalls

**Pitfall 1: Circular Dependencies**

```go
// Bad - circular dependency
type ServiceA struct {
    b *ServiceB
}

type ServiceB struct {
    a *ServiceA
}

// Good - introduce interface
type BInterface interface {
    DoSomething()
}

type ServiceA struct {
    b BInterface
}
```

**Pitfall 2: Too Many Dependencies**

```go
// Bad - god object
func NewUserService(
    repo UserRepository,
    email EmailService,
    cache Cache,
    logger Logger,
    metrics Metrics,
    queue Queue,
    // ... 10 more dependencies
) *UserService

// Good - split into smaller services
func NewUserService(repo UserRepository, email EmailService) *UserService
func NewUserCache(repo UserRepository, cache Cache) *UserCache
```

**Pitfall 3: Dependency on Concrete Types**

```go
// Bad
type UserService struct {
    repo *PostgresRepository  // Concrete type
}

// Good
type UserService struct {
    repo UserRepository  // Interface
}
```

## Related Patterns

**Related Tutorial**: See [Intermediate Tutorial - Design Patterns](../tutorials/intermediate.md#design-patterns) and [Advanced Tutorial - Architecture](../tutorials/advanced.md#architecture).

**Related How-To**: See [Write Testable Code](./write-testable-code.md) and [Use Interfaces Effectively](./use-interfaces-effectively.md).

**Related Cookbook**: See Cookbook recipes "DI Patterns", "Mock Implementation", "Wire Setup".

## Further Reading

- [Google Wire](https://github.com/google/wire)
- [Go DI Patterns](https://blog.drewolson.org/dependency-injection-in-go)
- [Uber's dig](https://github.com/uber-go/dig)
