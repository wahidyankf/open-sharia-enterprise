---
title: "How to Refactor God Packages"
date: 2025-12-17T10:00:00+07:00
draft: false
weight: 612
description: "Practical steps for breaking down large packages into focused, maintainable modules"
tags: ["golang", "refactoring", "package-design", "architecture"]
categories: ["learn"]
---

## Problem

God packages accumulate too many responsibilities, making code hard to understand, test, and maintain.

```go
// ❌ God package - does everything
package app

// User management (50 files)
type User struct{} func CreateUser() {} func DeleteUser() {}

// Payment processing (30 files)
type Payment struct{} func ProcessPayment() {}

// Email (20 files)
type Email struct{} func SendEmail() {}

// Logging, Config, Database, HTTP handlers... (200+ files total)
```

This guide shows practical steps for refactoring god packages into focused modules.

## Solution Strategies

### Identify Responsibilities

Map out what the package actually does before refactoring.

**When to use**: Before starting any refactoring work.

```go
// ✅ Audit current package
// 1. List all types in the package
// 2. Group related types together
// 3. Identify dependencies between groups
// 4. Look for natural boundaries

// Current god package structure:
package app

// Group 1: User management
type User struct{}
type UserRepository interface{}
type UserService struct{}
func CreateUser() {}
func UpdateUser() {}

// Group 2: Authentication
type Session struct{}
type Token struct{}
func Login() {}
func Logout() {}

// Group 3: Payment
type Payment struct{}
type Invoice struct{}
func ProcessPayment() {}
func GenerateInvoice() {}

// Group 4: Notification
type Email struct{}
type SMS struct{}
func SendEmail() {}
func SendSMS() {}
```

**Create dependency diagram:**

```go
// Visualization (using comments or external tool)
// user -> auth (users need authentication)
// payment -> user (payments belong to users)
// notification -> user (send notifications to users)
// notification -> payment (payment confirmations)
```

### Extract Focused Sub-Packages

Move related types and functions into dedicated packages.

```go
// ❌ Before - everything in app/
app/
  user.go              // 500 lines
  payment.go           // 400 lines
  email.go             // 300 lines
  config.go            // 200 lines
  database.go          // 350 lines
  // ... 50 more files

// ✅ After - focused packages
user/
  user.go              // Domain model
  repository.go        // Data access
  service.go           // Business logic

auth/
  session.go
  token.go
  service.go

payment/
  payment.go
  invoice.go
  processor.go
  repository.go

notification/
  email.go
  sms.go
  sender.go
```

**Step-by-step extraction:**

```go
// Step 1: Create new package directory
mkdir -p user

// Step 2: Move related files
mv user.go user/
mv user_repository.go user/repository.go
mv user_service.go user/service.go

// Step 3: Update package declaration
// In user/user.go:
package user // Changed from package app

// Step 4: Fix imports in original package
// In app/main.go:
import "yourproject/user"

// Step 5: Update callers
u := user.CreateUser() // Was app.CreateUser()
```

### Handle Circular Dependencies

Break circular dependencies using interfaces and dependency injection.

**Problem scenario:**

```go
// ❌ Circular dependency
package user
import "yourproject/payment"

type User struct {
  Payments []*payment.Payment // user depends on payment
}

package payment
import "yourproject/user"

type Payment struct {
  User *user.User // payment depends on user - circular!
}
```

**Solution 1: Define interface where it's used**

```go
// ✅ User package defines interface
package user

type PaymentProvider interface {
  FindByUser(userID string) ([]PaymentInfo, error)
}

type PaymentInfo struct {
  ID     string
  Amount int
  Status string
}

type Service struct {
  payments PaymentProvider
}

func (s *Service) GetUserWithPayments(userID string) (*User, error) {
  payments, err := s.payments.FindByUser(userID)
  // Use payments...
}

// ✅ Payment package implements interface
package payment

import "yourproject/user"

type Repository struct {
  db *sql.DB
}

// Implements user.PaymentProvider
func (r *Repository) FindByUser(userID string) ([]user.PaymentInfo, error) {
  // Query database...
  return []user.PaymentInfo{}, nil
}
```

**Solution 2: Extract shared types to separate package**

```go
// ✅ Create domain package for shared types
package domain

type UserID string
type PaymentID string

// Shared value objects
type Money struct {
  Amount   int
  Currency string
}

// ✅ User package uses domain types
package user

import "yourproject/domain"

type User struct {
  ID domain.UserID
}

// ✅ Payment package uses domain types
package payment

import "yourproject/domain"

type Payment struct {
  ID     domain.PaymentID
  UserID domain.UserID // No circular dependency!
  Amount domain.Money
}
```

**Solution 3: Use dependency injection**

```go
// ✅ Main package wires dependencies
package main

import (
  "yourproject/user"
  "yourproject/payment"
)

func main() {
  // Create repositories
  userRepo := user.NewRepository(db)
  paymentRepo := payment.NewRepository(db)

  // Inject dependencies
  userService := user.NewService(userRepo, paymentRepo) // Pass payment repo
  paymentService := payment.NewService(paymentRepo, userRepo) // Pass user repo

  // No circular imports!
}
```

### Use Internal Packages for Shared Code

Use `internal/` directory for code shared only within your project.

```go
// ✅ Internal package structure
yourproject/
  user/
    user.go
    service.go
  payment/
    payment.go
    service.go
  internal/
    database/
      connection.go  // Shared database utilities
    validation/
      validator.go   // Shared validation logic
    errors/
      errors.go      // Shared error types

// ✅ Internal packages can be imported within project
package user
import "yourproject/internal/database"
import "yourproject/internal/validation"

// ❌ Cannot be imported from outside project
// External projects cannot import yourproject/internal/*
```

**Benefits:**

- Clearly marks private implementation details
- Prevents external packages from depending on internals
- Allows refactoring internals without breaking external API

### Progressive Refactoring Strategy

Refactor incrementally to minimize risk.

**Phase 1: Extract domain types**

```go
// Week 1: Move just the domain models
package user

type User struct {
  ID    string
  Email string
  Name  string
}

// Keep everything else in app/ for now
package app

import "yourproject/user"

func CreateUser(email, name string) (*user.User, error) {
  // Business logic still here
}
```

**Phase 2: Extract repositories**

```go
// Week 2: Move data access
package user

type Repository interface {
  Find(id string) (*User, error)
  Save(user *User) error
}

type PostgresRepository struct {
  db *sql.DB
}

// Business logic still in app/
```

**Phase 3: Extract services**

```go
// Week 3: Move business logic
package user

type Service struct {
  repo Repository
}

func (s *Service) Create(email, name string) (*User, error) {
  // Business logic here
}

// Update app/ to use service
package app
import "yourproject/user"

var userService *user.Service

func init() {
  userService = user.NewService(userRepo)
}
```

**Phase 4: Update all callers**

```go
// Week 4: Migrate all call sites
// Old code (deprecated but still works)
user, err := app.CreateUser(email, name)

// New code
user, err := userService.Create(email, name)

// Mark old code as deprecated
// Deprecated: Use user.Service.Create instead
func CreateUser(email, name string) (*User, error) {
  return userService.Create(email, name)
}
```

### Package Naming Conventions

Choose clear, focused package names.

```go
// ❌ Vague names
package utils
package common
package helpers
package lib
package misc

// ✅ Specific names describing purpose
package user
package payment
package notification
package http
package postgres

// ✅ Package name should complete this sentence:
// "This package provides..."
// - user management
// - payment processing
// - notification delivery
// - HTTP server
// - PostgreSQL database access
```

### Avoid Over-Nesting

Keep package hierarchy flat when possible.

```go
// ❌ Over-nested packages
yourproject/
  internal/
    domain/
      model/
        entity/
          user/
            user.go  // Too deep!

// ✅ Flat structure
yourproject/
  user/
    user.go
  payment/
    payment.go

// ✅ One level of nesting when needed
yourproject/
  user/
    user.go
    internal/
      validation.go  // Implementation detail
```

## Putting It All Together

When you're ready to refactor a god package, start by understanding what you have. List all types, functions, and files. Group related functionality together by drawing boxes around things that change together or represent the same business concept. User registration, login, and profile updates belong together. Payment processing, invoicing, and refunds form another group.

Create a dependency diagram showing how these groups relate. If user management needs authentication, draw an arrow. If payments need users, draw another arrow. Look for circular dependencies - they'll force you to make architectural decisions before you can extract packages successfully.

Begin extraction with the groups that have fewest dependencies. Domain models often work well as a starting point - they're referenced by everything but don't depend on much. Move user types into a user package, payment types into a payment package. Update imports in the original package and verify tests still pass.

Handle circular dependencies by defining interfaces where they're used rather than where they're implemented. The user package can define a PaymentProvider interface without depending on the payment package. The payment package implements this interface. This inversion of dependencies breaks the cycle while maintaining flexibility.

Extract progressively rather than all at once. Move domain types first, then repositories, then services, then finally update all call sites. Each phase should leave the code in a working state. Keep deprecated wrapper functions in the original package temporarily to ease migration - remove them once all callers have migrated.

## Common Mistakes to Avoid

**Don't create utility packages:**

```go
// ❌ Generic utility packages become new god packages
package utils

func FormatUser(user User) string {}
func ValidateEmail(email string) bool {}
func CalculateTotal(items []Item) int {}
func SendNotification(user User) error {}

// ✅ Put utilities with related functionality
package user

func (u User) Format() string {} // User formatting with users
func ValidateEmail(email string) bool {} // Email validation with users

package cart

func Total(items []Item) int {} // Total calculation with cart
```

**Don't split packages by layer alone:**

```go
// ❌ Layer-based organization
models/
  user.go
  payment.go
  order.go
services/
  user_service.go
  payment_service.go
  order_service.go
repositories/
  user_repository.go
  payment_repository.go
  order_repository.go

// ✅ Feature-based organization
user/
  user.go
  service.go
  repository.go
payment/
  payment.go
  service.go
  repository.go
order/
  order.go
  service.go
  repository.go
```

**Don't extract prematurely:**

```go
// ❌ Too many tiny packages
email/validator/validator.go  // 20 lines
email/formatter/formatter.go  // 15 lines
email/sender/sender.go        // 30 lines

// ✅ One focused package
email/
  email.go       // Types
  validator.go   // Validation
  formatter.go   // Formatting
  sender.go      // Sending
```

## Summary

Refactoring god packages into focused modules requires systematic understanding of existing responsibilities before making changes. List all types and functions, group them by business concept, and map their dependencies. This analysis reveals natural boundaries where you can split the package and identifies circular dependencies you'll need to break.

Extract progressively rather than attempting a big-bang refactoring. Start with domain types that have minimal dependencies, then move data access layers, followed by business logic. Each phase should leave the code in a working state with passing tests. Use deprecated wrappers temporarily to ease migration of call sites.

Break circular dependencies using interfaces defined where they're used rather than where they're implemented. This inverts the dependency direction and allows packages to depend on abstractions rather than concrete implementations. For shared types that create cycles, extract them to a separate domain package or rethink whether the dependency is necessary.

Organize by feature rather than by layer - user package contains user models, services, and repositories together rather than scattering them across separate model/service/repository packages. This keeps related code together and makes the codebase easier to navigate.

Keep package hierarchies relatively flat. Deep nesting like domain/model/entity/user creates navigation overhead without adding value. One or two levels of nesting with the internal directory for implementation details usually suffices.

Choose focused package names that describe what the package provides - user, payment, notification - rather than vague names like utils, common, or helpers. Generic packages tend to become new god packages over time as convenient dumping grounds for unrelated functionality.

These strategies work together to transform tangled god packages into well-organized, maintainable codebases where each package has a clear purpose and minimal dependencies.

## Related Content

- [Go Best Practices and Idioms](/en/learn/swe/prog-lang/golang/explanation/best-practices)
- [Common Go Anti-Patterns](/en/learn/swe/prog-lang/golang/explanation/anti-patterns)
- [How to Design Interfaces Properly](/en/learn/swe/prog-lang/golang/how-to/design-interfaces-properly)
- [How to Handle Errors Effectively](/en/learn/swe/prog-lang/golang/how-to/handle-errors-effectively)
