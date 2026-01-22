---
title: Go Entity Template
description: Template for creating Domain-Driven Design entities in Go with identity, lifecycle management, and business logic
category: template
tags:
  - golang
  - ddd
  - domain-model
  - entity
  - aggregate
  - identity
  - lifecycle
  - go-1.18
  - go-1.21
  - go-1.22
  - go-1.23
  - go-1.24
  - go-1.25
related:
  - value-object-template.md
  - ex-so-stla-go__best-practices.md
  - ex-so-stla-go__idioms.md
principles:
  - simplicity-over-complexity
  - explicit-over-implicit
created: 2026-01-22
updated: 2026-01-22
---

# Go Entity Template

This template provides a standardized structure for creating Domain-Driven Design (DDD) entities in Go. Entities have identity, lifecycle, and business logic while maintaining domain invariants.

## Table of Contents

1. [Overview](#overview)
2. [Template Structure](#template-structure)
3. [Identity in Go](#identity-in-go)
4. [Factory Functions](#factory-functions)
5. [Lifecycle Management](#lifecycle-management)
6. [Version Tracking](#version-tracking)
7. [Business Logic](#business-logic)
8. [Complete Example](#complete-example)
9. [Entity vs Value Object](#entity-vs-value-object)
10. [Testing Entities](#testing-entities)
11. [Related Documentation](#related-documentation)

## Overview

Entities in Go differ from Java implementations due to Go's lack of classes and different approach to encapsulation. Go entities follow these principles:

**Key Characteristics**:

- **Identity-based equality**: Two entities are equal if their IDs match
- **Mutable state**: Unlike value objects, entities can change over time
- **Lifecycle tracking**: CreatedAt, UpdatedAt, DeletedAt timestamps
- **Version tracking**: Optimistic locking support with version field
- **Business methods**: Domain logic encapsulated in entity methods
- **Factory functions**: Constructor functions with validation

**Go vs Java Differences**:

```go
// Go: Struct with factory functions
type User struct {
  id        UserID       // Unexported fields
  name      string
  email     Email
  createdAt time.Time
  updatedAt time.Time
  version   int
}

// Factory function
func NewUser(id UserID, name string, email Email) (*User, error) {
  // Returns pointer for mutability
}

// Business methods use pointer receivers (can modify state)
func (u *User) UpdateEmail(email Email) error {
  u.email = email
  u.updatedAt = time.Now()
  u.version++
  return nil
}
```

```java
// Java: Class with private constructor
public class User {
    private final UserId id; // final identity
    private String name;     // mutable state
    private Email email;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
    private int version;

    // Private constructor
    private User(UserId id, String name, Email email) {
        // ...
    }

    // Factory method
    public static User create(UserId id, String name, Email email) {
        return new User(id, name, email);
    }

    // Business methods
    public void updateEmail(Email email) {
        this.email = email;
        this.updatedAt = LocalDateTime.now();
        this.version++;
    }
}
```

**Critical Differences**:

- Go uses **structs and factory functions** instead of classes with constructors
- Go uses **pointer receivers** for methods that modify state
- Go has **no `final` keyword** - identity immutability enforced by convention
- Go uses **time.Time** instead of Java's LocalDateTime
- Go entities use **nil pointers** for soft deletes (optional DeletedAt field)

## Template Structure

```go
package model

import (
  "errors"
  "fmt"
  "time"
)

// EntityName represents [brief description of what this entity models].
//
// Characteristics:
//   - Identity: Defined by ID field (equality based on ID only)
//   - Mutable: State can change through business methods
//   - Lifecycle: Created -> Updated -> Deleted (soft delete)
//   - Version tracking: Optimistic locking support
//
// Invariants:
//   - [Invariant 1]
//   - [Invariant 2]
//   - [Invariant 3]
//
// Responsibilities:
//   - [Primary responsibility 1]
//   - [Primary responsibility 2]
//   - [Primary responsibility 3]
//
// Example:
//
//  entity, err := NewEntityName(id, "name", valueObject, createdBy)
//  if err != nil {
//    return err
//  }
//  err = entity.UpdateName("newName", updatedBy)
type EntityName struct {
  // ========================================
  // Identity (should never change after creation)
  // ========================================

  id EntityID

  // ========================================
  // Essential State (can change via business methods)
  // ========================================

  name        string
  valueObject ValueObject
  status      EntityStatus

  // ========================================
  // Audit Fields (lifecycle tracking)
  // ========================================

  createdAt time.Time
  createdBy UserID
  updatedAt time.Time
  updatedBy UserID
  deletedAt *time.Time // nil = not deleted, non-nil = soft deleted
  version   int        // Optimistic locking

  // ========================================
  // Associations (relationships to other entities)
  // ========================================

  relatedEntities []RelatedEntityID
}

// ========================================
// Factory Functions (Constructors)
// ========================================

// NewEntityName creates a new EntityName with validation.
//
// Returns error if any invariant is violated.
func NewEntityName(
  id EntityID,
  name string,
  valueObject ValueObject,
  createdBy UserID,
) (*EntityName, error) {
  // Validate all invariants
  if id == (EntityID{}) {
    return nil, errors.New("entity ID required")
  }

  if err := validateName(name); err != nil {
    return nil, fmt.Errorf("invalid name: %w", err)
  }

  if err := valueObject.Validate(); err != nil {
    return nil, fmt.Errorf("invalid value object: %w", err)
  }

  if createdBy == (UserID{}) {
    return nil, errors.New("created by user ID required")
  }

  now := time.Now()

  return &EntityName{
    id:              id,
    name:            name,
    valueObject:     valueObject,
    status:          StatusActive,
    createdAt:       now,
    createdBy:       createdBy,
    updatedAt:       now,
    updatedBy:       createdBy,
    deletedAt:       nil,
    version:         0,
    relatedEntities: []RelatedEntityID{},
  }, nil
}

// MustNewEntityName creates a new EntityName, panicking on invalid input.
//
// Use only when input is guaranteed valid (e.g., hard-coded values, config).
func MustNewEntityName(id EntityID, name string, valueObject ValueObject, createdBy UserID) *EntityName {
  entity, err := NewEntityName(id, name, valueObject, createdBy)
  if err != nil {
    panic(fmt.Sprintf("invalid EntityName: %v", err))
  }
  return entity
}

// ReconstituteEntityName reconstitutes an entity from persistence.
//
// Bypasses validation for data already in database.
// Use only when loading from trusted persistence layer.
func ReconstituteEntityName(
  id EntityID,
  name string,
  valueObject ValueObject,
  status EntityStatus,
  createdAt time.Time,
  createdBy UserID,
  updatedAt time.Time,
  updatedBy UserID,
  deletedAt *time.Time,
  version int,
  relatedEntities []RelatedEntityID,
) *EntityName {
  return &EntityName{
    id:              id,
    name:            name,
    valueObject:     valueObject,
    status:          status,
    createdAt:       createdAt,
    createdBy:       createdBy,
    updatedAt:       updatedAt,
    updatedBy:       updatedBy,
    deletedAt:       deletedAt,
    version:         version,
    relatedEntities: relatedEntities,
  }
}

// ========================================
// Validation (Private Functions)
// ========================================

func validateName(name string) error {
  if name == "" {
    return errors.New("must not be empty")
  }

  if len(name) < 3 || len(name) > 100 {
    return errors.New("must be between 3 and 100 characters")
  }

  return nil
}

func (e *EntityName) validateUpdatable() error {
  if e.deletedAt != nil {
    return errors.New("entity is deleted")
  }

  if e.status == StatusArchived {
    return errors.New("entity is archived")
  }

  return nil
}

func (e *EntityName) canTransitionTo(newStatus EntityStatus) bool {
  switch e.status {
  case StatusActive:
    return newStatus == StatusSuspended || newStatus == StatusArchived
  case StatusSuspended:
    return newStatus == StatusActive || newStatus == StatusArchived
  case StatusArchived:
    return newStatus == StatusActive
  default:
    return false
  }
}

// ========================================
// Business Logic (Pointer Receivers)
// ========================================

// UpdateName updates the entity name with validation.
//
// Returns error if name is invalid or entity is not updatable.
func (e *EntityName) UpdateName(newName string, updatedBy UserID) error {
  if err := e.validateUpdatable(); err != nil {
    return err
  }

  if err := validateName(newName); err != nil {
    return fmt.Errorf("invalid name: %w", err)
  }

  if updatedBy == (UserID{}) {
    return errors.New("updated by user ID required")
  }

  e.name = newName
  e.markUpdated(updatedBy)

  return nil
}

// ChangeStatus changes entity status with state transition validation.
//
// Returns error if transition is not allowed.
func (e *EntityName) ChangeStatus(newStatus EntityStatus, updatedBy UserID) error {
  if updatedBy == (UserID{}) {
    return errors.New("updated by user ID required")
  }

  if !e.canTransitionTo(newStatus) {
    return fmt.Errorf("cannot transition from %s to %s", e.status, newStatus)
  }

  e.status = newStatus
  e.markUpdated(updatedBy)

  return nil
}

// AddRelatedEntity adds a related entity to this entity's collection.
//
// Returns error if entity is not updatable or related entity already exists.
func (e *EntityName) AddRelatedEntity(relatedID RelatedEntityID, updatedBy UserID) error {
  if err := e.validateUpdatable(); err != nil {
    return err
  }

  if relatedID == (RelatedEntityID{}) {
    return errors.New("related entity ID required")
  }

  if updatedBy == (UserID{}) {
    return errors.New("updated by user ID required")
  }

  // Check for duplicates
  for _, existing := range e.relatedEntities {
    if existing == relatedID {
      return errors.New("related entity already exists")
    }
  }

  e.relatedEntities = append(e.relatedEntities, relatedID)
  e.markUpdated(updatedBy)

  return nil
}

// RemoveRelatedEntity removes a related entity from this entity's collection.
//
// Returns true if entity was removed, false if not found.
func (e *EntityName) RemoveRelatedEntity(relatedID RelatedEntityID, updatedBy UserID) (bool, error) {
  if err := e.validateUpdatable(); err != nil {
    return false, err
  }

  if relatedID == (RelatedEntityID{}) {
    return false, errors.New("related entity ID required")
  }

  if updatedBy == (UserID{}) {
    return false, errors.New("updated by user ID required")
  }

  // Find and remove
  for i, existing := range e.relatedEntities {
    if existing == relatedID {
      // Remove by replacing with last element and truncating
      e.relatedEntities[i] = e.relatedEntities[len(e.relatedEntities)-1]
      e.relatedEntities = e.relatedEntities[:len(e.relatedEntities)-1]
      e.markUpdated(updatedBy)
      return true, nil
    }
  }

  return false, nil
}

// Delete soft-deletes this entity.
//
// Returns error if entity is already deleted.
func (e *EntityName) Delete(deletedBy UserID) error {
  if e.deletedAt != nil {
    return errors.New("entity already deleted")
  }

  if deletedBy == (UserID{}) {
    return errors.New("deleted by user ID required")
  }

  now := time.Now()
  e.deletedAt = &now
  e.status = StatusArchived
  e.markUpdated(deletedBy)

  return nil
}

// Restore restores a soft-deleted entity.
//
// Returns error if entity is not deleted.
func (e *EntityName) Restore(restoredBy UserID) error {
  if e.deletedAt == nil {
    return errors.New("entity is not deleted")
  }

  if restoredBy == (UserID{}) {
    return errors.New("restored by user ID required")
  }

  e.deletedAt = nil
  e.status = StatusActive
  e.markUpdated(restoredBy)

  return nil
}

// ========================================
// State Modification Helpers
// ========================================

func (e *EntityName) markUpdated(updatedBy UserID) {
  e.updatedAt = time.Now()
  e.updatedBy = updatedBy
  e.version++
}

// ========================================
// Query Methods (Getters)
// ========================================

// ID returns the entity's unique identifier.
func (e *EntityName) ID() EntityID {
  return e.id
}

// Name returns the entity's name.
func (e *EntityName) Name() string {
  return e.name
}

// ValueObject returns the entity's value object.
func (e *EntityName) ValueObject() ValueObject {
  return e.valueObject
}

// Status returns the entity's current status.
func (e *EntityName) Status() EntityStatus {
  return e.status
}

// CreatedAt returns when the entity was created.
func (e *EntityName) CreatedAt() time.Time {
  return e.createdAt
}

// CreatedBy returns who created the entity.
func (e *EntityName) CreatedBy() UserID {
  return e.createdBy
}

// UpdatedAt returns when the entity was last updated.
func (e *EntityName) UpdatedAt() time.Time {
  return e.updatedAt
}

// UpdatedBy returns who last updated the entity.
func (e *EntityName) UpdatedBy() UserID {
  return e.updatedBy
}

// DeletedAt returns when the entity was deleted (nil if not deleted).
func (e *EntityName) DeletedAt() *time.Time {
  return e.deletedAt
}

// Version returns the entity's version for optimistic locking.
func (e *EntityName) Version() int {
  return e.version
}

// RelatedEntities returns a copy of related entity IDs.
func (e *EntityName) RelatedEntities() []RelatedEntityID {
  // Defensive copy
  result := make([]RelatedEntityID, len(e.relatedEntities))
  copy(result, e.relatedEntities)
  return result
}

// IsActive returns true if the entity is active.
func (e *EntityName) IsActive() bool {
  return e.status == StatusActive && e.deletedAt == nil
}

// IsDeleted returns true if the entity is soft-deleted.
func (e *EntityName) IsDeleted() bool {
  return e.deletedAt != nil
}

// ========================================
// Equality (Identity-Based)
// ========================================

// Equals compares this entity with another for identity equality.
//
// Entities are equal if their IDs match (state doesn't matter).
func (e *EntityName) Equals(other *EntityName) bool {
  if e == nil || other == nil {
    return e == other
  }
  return e.id == other.id
}

// ========================================
// String Representation
// ========================================

// String returns a string representation of the entity.
func (e *EntityName) String() string {
  return fmt.Sprintf("EntityName{id=%v, name=%q, status=%s, version=%d, deleted=%t}",
    e.id, e.name, e.status, e.version, e.deletedAt != nil)
}

// ========================================
// Status Enumeration
// ========================================

type EntityStatus string

const (
  StatusActive    EntityStatus = "ACTIVE"
  StatusSuspended EntityStatus = "SUSPENDED"
  StatusArchived  EntityStatus = "ARCHIVED"
)

func (s EntityStatus) String() string {
  return string(s)
}
```

## Identity in Go

Go entities use **identity-based equality** where two entities are equal if their IDs match, regardless of their state.

### Identity Field Pattern

```go
// CORRECT: ID as unexported field
type User struct {
  id UserID // Never changes after creation
  // ... other fields
}

// Constructor ensures ID is set
func NewUser(id UserID, name string) (*User, error) {
  if id == (UserID{}) {
    return nil, errors.New("user ID required")
  }

  return &User{
    id:   id,
    name: name,
  }, nil
}

// Getter provides read-only access
func (u *User) ID() UserID {
  return u.id
}

// INCORRECT: ID as exported field (can be modified)
type MutableUser struct {
  ID UserID // Can be changed externally!
}

// INCORRECT: ID setter (breaks identity)
func (u *User) SetID(id UserID) {
  u.id = id // Identity should never change!
}
```

### Identity-Based Equality

```go
// CORRECT: Equality based on ID only
func (u *User) Equals(other *User) bool {
  if u == nil || other == nil {
    return u == other
  }
  return u.id == other.id
}

// Usage: Same ID = equal entities, even with different state
user1, _ := NewUser(UserID{value: "123"}, "John")
user2, _ := NewUser(UserID{value: "123"}, "Jane")

user1.Equals(user2) // true - same ID

// INCORRECT: Equality based on all fields (this is for value objects!)
func (u *User) ValueEquals(other *User) bool {
  return u.id == other.id &&
    u.name == other.name &&
    u.email == other.email // Don't do this for entities!
}
```

### Pointer Semantics for Entities

```go
// CORRECT: Return pointer from factory (entities are mutable)
func NewUser(id UserID, name string) (*User, error) {
  return &User{
    id:   id,
    name: name,
  }, nil
}

// CORRECT: Pointer receiver for mutations
func (u *User) UpdateName(name string) error {
  u.name = name
  u.version++
  return nil
}

// CORRECT: Pass entity by pointer
func SaveUser(user *User) error {
  // Work with pointer
}

// INCORRECT: Return value from factory (entities should be pointers)
func NewUserByValue(id UserID, name string) (User, error) {
  return User{id: id, name: name}, nil
}

// INCORRECT: Value receiver for mutations (won't modify original)
func (u User) BrokenUpdate(name string) {
  u.name = name // Modifies copy, not original!
}
```

## Factory Functions

Go uses **factory functions** instead of constructors to create entities with validation.

### Basic Factory Pattern

```go
// Pattern 1: Error-returning factory (preferred)
func NewUser(id UserID, name string, email Email) (*User, error) {
  // Validate all inputs
  if id == (UserID{}) {
    return nil, errors.New("user ID required")
  }

  if name == "" {
    return nil, errors.New("name required")
  }

  if err := email.Validate(); err != nil {
    return nil, fmt.Errorf("invalid email: %w", err)
  }

  now := time.Now()

  return &User{
    id:        id,
    name:      name,
    email:     email,
    status:    StatusActive,
    createdAt: now,
    updatedAt: now,
    version:   0,
  }, nil
}

// Pattern 2: Panic factory (for guaranteed valid input)
func MustNewUser(id UserID, name string, email Email) *User {
  user, err := NewUser(id, name, email)
  if err != nil {
    panic(fmt.Sprintf("invalid user: %v", err))
  }
  return user
}

// Usage: Error return for runtime validation
user, err := NewUser(userID, userName, email)
if err != nil {
  return fmt.Errorf("create user: %w", err)
}

// Usage: Panic for compile-time constants
var adminUser = MustNewUser(adminID, "Admin", adminEmail)
```

### Reconstitution Pattern

```go
// ReconstituteUser loads entity from persistence without validation.
//
// Use ONLY when loading from trusted persistence layer.
// Bypasses validation because data is already in database.
func ReconstituteUser(
  id UserID,
  name string,
  email Email,
  status UserStatus,
  createdAt time.Time,
  updatedAt time.Time,
  deletedAt *time.Time,
  version int,
) *User {
  return &User{
    id:        id,
    name:      name,
    email:     email,
    status:    status,
    createdAt: createdAt,
    updatedAt: updatedAt,
    deletedAt: deletedAt,
    version:   version,
  }
}

// Repository uses reconstitution
type UserRepository struct {
  db *sql.DB
}

func (r *UserRepository) FindByID(ctx context.Context, id UserID) (*User, error) {
  var (
    name      string
    email     string
    status    string
    createdAt time.Time
    updatedAt time.Time
    deletedAt *time.Time
    version   int
  )

  err := r.db.QueryRowContext(ctx,
    "SELECT name, email, status, created_at, updated_at, deleted_at, version FROM users WHERE id = ?",
    id.Value(),
  ).Scan(&name, &email, &status, &createdAt, &updatedAt, &deletedAt, &version)

  if err != nil {
    return nil, err
  }

  emailVO, _ := NewEmail(email) // Safe: already validated on insert

  return ReconstituteUser(id, name, emailVO, UserStatus(status), createdAt, updatedAt, deletedAt, version), nil
}
```

## Lifecycle Management

Entities track their lifecycle through audit fields: CreatedAt, UpdatedAt, DeletedAt.

### Audit Fields Pattern

```go
type Order struct {
  id OrderID

  // Business state
  items  []OrderItem
  total  Money
  status OrderStatus

  // Audit fields
  createdAt time.Time
  createdBy UserID
  updatedAt time.Time
  updatedBy UserID
  deletedAt *time.Time // nil = not deleted
  version   int
}

func NewOrder(id OrderID, items []OrderItem, createdBy UserID) (*Order, error) {
  if id == (OrderID{}) {
    return nil, errors.New("order ID required")
  }

  if len(items) == 0 {
    return nil, errors.New("order must have at least one item")
  }

  if createdBy == (UserID{}) {
    return nil, errors.New("created by user ID required")
  }

  now := time.Now()

  return &Order{
    id:        id,
    items:     items,
    total:     calculateTotal(items),
    status:    OrderStatusPending,
    createdAt: now,
    createdBy: createdBy,
    updatedAt: now,
    updatedBy: createdBy,
    deletedAt: nil,
    version:   0,
  }, nil
}

// Business methods track who made changes
func (o *Order) AddItem(item OrderItem, updatedBy UserID) error {
  if o.deletedAt != nil {
    return errors.New("cannot modify deleted order")
  }

  if updatedBy == (UserID{}) {
    return errors.New("updated by user ID required")
  }

  o.items = append(o.items, item)
  o.total = calculateTotal(o.items)
  o.markUpdated(updatedBy)

  return nil
}

func (o *Order) markUpdated(updatedBy UserID) {
  o.updatedAt = time.Now()
  o.updatedBy = updatedBy
  o.version++
}
```

### Soft Delete Pattern

```go
// Soft delete: Set deletedAt instead of removing from database
func (o *Order) Delete(deletedBy UserID) error {
  if o.deletedAt != nil {
    return errors.New("order already deleted")
  }

  if o.status == OrderStatusShipped {
    return errors.New("cannot delete shipped order")
  }

  if deletedBy == (UserID{}) {
    return errors.New("deleted by user ID required")
  }

  now := time.Now()
  o.deletedAt = &now
  o.status = OrderStatusCancelled
  o.markUpdated(deletedBy)

  return nil
}

// Restore: Clear deletedAt
func (o *Order) Restore(restoredBy UserID) error {
  if o.deletedAt == nil {
    return errors.New("order is not deleted")
  }

  if restoredBy == (UserID{}) {
    return errors.New("restored by user ID required")
  }

  o.deletedAt = nil
  o.status = OrderStatusPending
  o.markUpdated(restoredBy)

  return nil
}

// Query methods
func (o *Order) IsDeleted() bool {
  return o.deletedAt != nil
}

func (o *Order) DeletedAt() *time.Time {
  return o.deletedAt
}
```

### Lifecycle Queries

```go
// Repository with lifecycle filtering
func (r *OrderRepository) FindActive(ctx context.Context) ([]*Order, error) {
  rows, err := r.db.QueryContext(ctx,
    "SELECT * FROM orders WHERE deleted_at IS NULL ORDER BY created_at DESC",
  )
  if err != nil {
    return nil, err
  }
  defer rows.Close()

  var orders []*Order
  for rows.Next() {
    order := scanOrder(rows)
    orders = append(orders, order)
  }

  return orders, rows.Err()
}

// Find with optional soft-deleted
func (r *OrderRepository) FindByID(ctx context.Context, id OrderID, includeDeleted bool) (*Order, error) {
  query := "SELECT * FROM orders WHERE id = ?"
  if !includeDeleted {
    query += " AND deleted_at IS NULL"
  }

  row := r.db.QueryRowContext(ctx, query, id.Value())
  return scanOrder(row)
}
```

## Version Tracking

Entities use a **version field** for optimistic locking to prevent concurrent modification conflicts.

### Version Field Pattern

```go
type Account struct {
  id      AccountID
  balance Money
  version int // Optimistic locking version
}

func (a *Account) Deposit(amount Money, updatedBy UserID) error {
  // Business logic
  newBalance, err := a.balance.Add(amount)
  if err != nil {
    return err
  }

  a.balance = newBalance
  a.markUpdated(updatedBy)

  return nil
}

func (a *Account) markUpdated(updatedBy UserID) {
  a.updatedAt = time.Now()
  a.updatedBy = updatedBy
  a.version++ // Increment on every modification
}

func (a *Account) Version() int {
  return a.version
}
```

### Repository with Optimistic Locking

```go
type AccountRepository struct {
  db *sql.DB
}

// Save with optimistic locking
func (r *AccountRepository) Save(ctx context.Context, account *Account) error {
  result, err := r.db.ExecContext(ctx, `
    UPDATE accounts
    SET balance = ?, updated_at = ?, updated_by = ?, version = ?
    WHERE id = ? AND version = ?
  `,
    account.balance.Amount(),
    account.UpdatedAt(),
    account.UpdatedBy().Value(),
    account.Version(),        // New version
    account.ID().Value(),
    account.Version()-1,      // Expected version
  )

  if err != nil {
    return fmt.Errorf("update account: %w", err)
  }

  rowsAffected, err := result.RowsAffected()
  if err != nil {
    return fmt.Errorf("check rows affected: %w", err)
  }

  if rowsAffected == 0 {
    return errors.New("optimistic locking conflict: account was modified by another transaction")
  }

  return nil
}

// Usage
account, err := repo.FindByID(ctx, accountID)
if err != nil {
  return err
}

err = account.Deposit(amount, userID)
if err != nil {
  return err
}

err = repo.Save(ctx, account) // Fails if version changed
if err != nil {
  // Handle conflict: reload and retry
  return err
}
```

### Conflict Resolution

```go
// Retry pattern for optimistic locking conflicts
func DepositWithRetry(
  ctx context.Context,
  repo *AccountRepository,
  accountID AccountID,
  amount Money,
  userID UserID,
  maxRetries int,
) error {
  for i := 0; i < maxRetries; i++ {
    // Load current state
    account, err := repo.FindByID(ctx, accountID)
    if err != nil {
      return err
    }

    // Apply business logic
    if err := account.Deposit(amount, userID); err != nil {
      return err
    }

    // Try to save
    err = repo.Save(ctx, account)
    if err == nil {
      return nil // Success!
    }

    // Check if it's an optimistic locking conflict
    if !isOptimisticLockingError(err) {
      return err // Different error, don't retry
    }

    // Retry after brief delay
    time.Sleep(time.Millisecond * 100)
  }

  return errors.New("max retries exceeded for optimistic locking")
}

func isOptimisticLockingError(err error) bool {
  return err != nil && err.Error() == "optimistic locking conflict: account was modified by another transaction"
}
```

## Business Logic

Entity business logic encapsulates domain rules and state transitions.

### State Transition Pattern

```go
type OrderStatus string

const (
  OrderStatusPending   OrderStatus = "PENDING"
  OrderStatusConfirmed OrderStatus = "CONFIRMED"
  OrderStatusShipped   OrderStatus = "SHIPPED"
  OrderStatusDelivered OrderStatus = "DELIVERED"
  OrderStatusCancelled OrderStatus = "CANCELLED"
)

type Order struct {
  id     OrderID
  status OrderStatus
  // ... other fields
}

// State transition with validation
func (o *Order) Confirm(confirmedBy UserID) error {
  if err := o.validateUpdatable(); err != nil {
    return err
  }

  if !o.canTransitionTo(OrderStatusConfirmed) {
    return fmt.Errorf("cannot confirm order in %s state", o.status)
  }

  o.status = OrderStatusConfirmed
  o.markUpdated(confirmedBy)

  return nil
}

func (o *Order) Ship(shippedBy UserID, trackingNumber string) error {
  if err := o.validateUpdatable(); err != nil {
    return err
  }

  if !o.canTransitionTo(OrderStatusShipped) {
    return fmt.Errorf("cannot ship order in %s state", o.status)
  }

  if trackingNumber == "" {
    return errors.New("tracking number required")
  }

  o.status = OrderStatusShipped
  o.trackingNumber = trackingNumber
  o.markUpdated(shippedBy)

  return nil
}

func (o *Order) Cancel(cancelledBy UserID, reason string) error {
  if !o.canTransitionTo(OrderStatusCancelled) {
    return fmt.Errorf("cannot cancel order in %s state", o.status)
  }

  if reason == "" {
    return errors.New("cancellation reason required")
  }

  o.status = OrderStatusCancelled
  o.cancellationReason = reason
  o.markUpdated(cancelledBy)

  return nil
}

// State transition rules
func (o *Order) canTransitionTo(target OrderStatus) bool {
  switch o.status {
  case OrderStatusPending:
    return target == OrderStatusConfirmed || target == OrderStatusCancelled
  case OrderStatusConfirmed:
    return target == OrderStatusShipped || target == OrderStatusCancelled
  case OrderStatusShipped:
    return target == OrderStatusDelivered
  case OrderStatusDelivered:
    return false // Terminal state
  case OrderStatusCancelled:
    return false // Terminal state
  default:
    return false
  }
}

func (o *Order) validateUpdatable() error {
  if o.deletedAt != nil {
    return errors.New("order is deleted")
  }
  return nil
}
```

### Domain Validation

```go
type Product struct {
  id    ProductID
  name  string
  price Money
  stock int
}

// Business method with validation
func (p *Product) UpdatePrice(newPrice Money, updatedBy UserID) error {
  if err := p.validateUpdatable(); err != nil {
    return err
  }

  if updatedBy == (UserID{}) {
    return errors.New("updated by user ID required")
  }

  // Domain rule: Price cannot be negative
  if newPrice.IsNegative() {
    return errors.New("price must not be negative")
  }

  // Domain rule: Price must be in valid range
  minPrice := MustNewMoney(100, "USD") // $1.00 minimum
  maxPrice := MustNewMoney(100000000, "USD") // $1M maximum

  if newPrice.IsLessThan(minPrice) {
    return errors.New("price below minimum allowed")
  }

  if newPrice.IsGreaterThan(maxPrice) {
    return errors.New("price exceeds maximum allowed")
  }

  p.price = newPrice
  p.markUpdated(updatedBy)

  return nil
}

// Business method with complex validation
func (p *Product) DecreaseStock(quantity int, updatedBy UserID) error {
  if err := p.validateUpdatable(); err != nil {
    return err
  }

  if updatedBy == (UserID{}) {
    return errors.New("updated by user ID required")
  }

  // Domain rule: Quantity must be positive
  if quantity <= 0 {
    return errors.New("quantity must be positive")
  }

  // Domain rule: Cannot decrease below zero
  if p.stock < quantity {
    return fmt.Errorf("insufficient stock: have %d, need %d", p.stock, quantity)
  }

  p.stock -= quantity
  p.markUpdated(updatedBy)

  return nil
}

func (p *Product) validateUpdatable() error {
  if p.deletedAt != nil {
    return errors.New("product is deleted")
  }
  return nil
}
```

## Complete Example

### Zakat Account Entity (Financial Domain)

Complete implementation with identity, lifecycle, versioning, and business logic:

```go
package model

import (
  "errors"
  "fmt"
  "time"
)

// ZakatAccount entity represents an account for tracking Zakatable wealth and calculating Zakat obligations.
//
// Characteristics:
//   - Identity: Defined by account ID
//   - Lifecycle: Opened -> Active -> Closed
//   - Mutable: Balance and status change over time
//   - Version tracking: Optimistic locking support
//
// Invariants:
//   - Balance must not be negative
//   - Nisab threshold must be positive
//   - Haul period must be one lunar year (354-355 days)
//   - Zakat rate must be 2.5% for wealth
//
// Responsibilities:
//   - Track wealth subject to Zakat
//   - Calculate Zakat obligations when conditions are met
//   - Maintain payment history
//   - Enforce Islamic finance rules
//
// Example:
//
//  account, err := NewZakatAccount(id, holderID, initialBalance, nisab, openedBy)
//  if err != nil {
//    return err
//  }
//  calc := account.CalculateZakat()
//  if calc.IsEligible() {
//    err = account.RecordPayment(payment, userID)
//  }
type ZakatAccount struct {
  // Constants
  zakatRate Money // 2.5% rate
  haulDays  int   // 354 days (one lunar year)

  // Identity
  id AccountID

  // Essential State
  holderID      HolderID
  balance       Money
  nisabThreshold Money
  status        ZakatAccountStatus

  // Haul Tracking
  haulStartDate         time.Time
  lastZakatPaymentDate  *time.Time

  // Audit Fields
  openedAt  time.Time
  openedBy  UserID
  updatedAt time.Time
  updatedBy UserID
  closedAt  *time.Time
  version   int

  // Payments
  payments []ZakatPaymentID
}

// ========================================
// Factory Functions
// ========================================

// NewZakatAccount opens a new Zakat account with validation.
func NewZakatAccount(
  id AccountID,
  holderID HolderID,
  initialBalance Money,
  nisabThreshold Money,
  openedBy UserID,
) (*ZakatAccount, error) {
  // Validate inputs
  if id == (AccountID{}) {
    return nil, errors.New("account ID required")
  }

  if holderID == (HolderID{}) {
    return nil, errors.New("holder ID required")
  }

  if err := validateBalance(initialBalance); err != nil {
    return nil, fmt.Errorf("invalid initial balance: %w", err)
  }

  if err := validateNisab(nisabThreshold); err != nil {
    return nil, fmt.Errorf("invalid nisab threshold: %w", err)
  }

  if openedBy == (UserID{}) {
    return nil, errors.New("opened by user ID required")
  }

  // Ensure same currency
  if initialBalance.Currency() != nisabThreshold.Currency() {
    return nil, errors.New("balance and nisab must be in same currency")
  }

  now := time.Now()

  return &ZakatAccount{
    zakatRate:            MustNewMoney(25, initialBalance.Currency()), // 2.5% = 0.025
    haulDays:             354,
    id:                   id,
    holderID:             holderID,
    balance:              initialBalance,
    nisabThreshold:       nisabThreshold,
    status:               ZakatAccountStatusActive,
    haulStartDate:        now,
    lastZakatPaymentDate: nil,
    openedAt:             now,
    openedBy:             openedBy,
    updatedAt:            now,
    updatedBy:            openedBy,
    closedAt:             nil,
    version:              0,
    payments:             []ZakatPaymentID{},
  }, nil
}

// ReconstituteZakatAccount reconstitutes from persistence.
func ReconstituteZakatAccount(
  id AccountID,
  holderID HolderID,
  balance Money,
  nisabThreshold Money,
  status ZakatAccountStatus,
  haulStartDate time.Time,
  lastZakatPaymentDate *time.Time,
  openedAt time.Time,
  openedBy UserID,
  updatedAt time.Time,
  updatedBy UserID,
  closedAt *time.Time,
  version int,
  payments []ZakatPaymentID,
) *ZakatAccount {
  return &ZakatAccount{
    zakatRate:            MustNewMoney(25, balance.Currency()),
    haulDays:             354,
    id:                   id,
    holderID:             holderID,
    balance:              balance,
    nisabThreshold:       nisabThreshold,
    status:               status,
    haulStartDate:        haulStartDate,
    lastZakatPaymentDate: lastZakatPaymentDate,
    openedAt:             openedAt,
    openedBy:             openedBy,
    updatedAt:            updatedAt,
    updatedBy:            updatedBy,
    closedAt:             closedAt,
    version:              version,
    payments:             payments,
  }
}

// ========================================
// Business Logic
// ========================================

// CalculateZakat calculates Zakat due for this account.
func (a *ZakatAccount) CalculateZakat() ZakatCalculation {
  if err := a.validateActive(); err != nil {
    return ZakatCalculation{
      eligible: false,
      reason:   err.Error(),
    }
  }

  // Check if balance meets nisab
  meetsNisab, _ := a.balance.IsGreaterThanOrEqual(a.nisabThreshold)
  if !meetsNisab {
    return ZakatCalculation{
      eligible: false,
      reason:   "balance below nisab threshold",
    }
  }

  // Check if haul (one lunar year) has passed
  if !a.hasCompletedHaul() {
    return ZakatCalculation{
      eligible: false,
      reason:   "haul period not completed",
    }
  }

  // Calculate 2.5% of wealth
  zakatAmount, err := a.balance.Multiply(25)
  if err != nil {
    return ZakatCalculation{
      eligible: false,
      reason:   fmt.Sprintf("calculation error: %v", err),
    }
  }

  zakatAmount, err = zakatAmount.Divide(1000) // 25/1000 = 0.025 = 2.5%
  if err != nil {
    return ZakatCalculation{
      eligible: false,
      reason:   fmt.Sprintf("calculation error: %v", err),
    }
  }

  return ZakatCalculation{
    eligible:       true,
    zakatAmount:    zakatAmount,
    balance:        a.balance,
    nisabThreshold: a.nisabThreshold,
  }
}

// RecordPayment records a Zakat payment.
func (a *ZakatAccount) RecordPayment(paymentID ZakatPaymentID, amount Money, recordedBy UserID) error {
  if err := a.validateActive(); err != nil {
    return err
  }

  if paymentID == (ZakatPaymentID{}) {
    return errors.New("payment ID required")
  }

  if recordedBy == (UserID{}) {
    return errors.New("recorded by user ID required")
  }

  // Validate amount
  if amount.IsNegativeOrZero() {
    return errors.New("payment amount must be positive")
  }

  // Validate currency
  if amount.Currency() != a.balance.Currency() {
    return errors.New("payment currency must match account currency")
  }

  // Check sufficient balance
  exceedsBalance, _ := amount.IsGreaterThan(a.balance)
  if exceedsBalance {
    return errors.New("payment amount exceeds account balance")
  }

  // Record payment
  a.payments = append(a.payments, paymentID)

  // Update balance
  newBalance, err := a.balance.Subtract(amount)
  if err != nil {
    return fmt.Errorf("subtract payment: %w", err)
  }
  a.balance = newBalance

  // Reset haul tracking
  now := time.Now()
  a.lastZakatPaymentDate = &now
  a.haulStartDate = now

  a.markUpdated(recordedBy)

  return nil
}

// Deposit adds funds to the account.
func (a *ZakatAccount) Deposit(amount Money, depositedBy UserID) error {
  if err := a.validateActive(); err != nil {
    return err
  }

  if depositedBy == (UserID{}) {
    return errors.New("deposited by user ID required")
  }

  if amount.IsNegativeOrZero() {
    return errors.New("deposit amount must be positive")
  }

  if amount.Currency() != a.balance.Currency() {
    return errors.New("deposit currency must match account currency")
  }

  newBalance, err := a.balance.Add(amount)
  if err != nil {
    return fmt.Errorf("add deposit: %w", err)
  }

  a.balance = newBalance
  a.markUpdated(depositedBy)

  return nil
}

// Withdraw removes funds from the account.
func (a *ZakatAccount) Withdraw(amount Money, withdrawnBy UserID) error {
  if err := a.validateActive(); err != nil {
    return err
  }

  if withdrawnBy == (UserID{}) {
    return errors.New("withdrawn by user ID required")
  }

  if amount.IsNegativeOrZero() {
    return errors.New("withdrawal amount must be positive")
  }

  if amount.Currency() != a.balance.Currency() {
    return errors.New("withdrawal currency must match account currency")
  }

  newBalance, err := a.balance.Subtract(amount)
  if err != nil {
    return fmt.Errorf("subtract withdrawal: %w", err)
  }

  if newBalance.IsNegative() {
    return errors.New("insufficient balance")
  }

  a.balance = newBalance
  a.markUpdated(withdrawnBy)

  return nil
}

// Close closes the account.
func (a *ZakatAccount) Close(closedBy UserID) error {
  if err := a.validateActive(); err != nil {
    return err
  }

  if closedBy == (UserID{}) {
    return errors.New("closed by user ID required")
  }

  if a.balance.IsPositive() {
    return errors.New("cannot close account with positive balance")
  }

  now := time.Now()
  a.closedAt = &now
  a.status = ZakatAccountStatusClosed
  a.markUpdated(closedBy)

  return nil
}

// ========================================
// Validation
// ========================================

func validateBalance(balance Money) error {
  if balance.IsNegative() {
    return errors.New("must not be negative")
  }
  return nil
}

func validateNisab(nisab Money) error {
  if nisab.IsNegativeOrZero() {
    return errors.New("must be positive")
  }
  return nil
}

func (a *ZakatAccount) validateActive() error {
  if a.closedAt != nil {
    return errors.New("account is closed")
  }

  if a.status != ZakatAccountStatusActive {
    return fmt.Errorf("account is not active: %s", a.status)
  }

  return nil
}

func (a *ZakatAccount) hasCompletedHaul() bool {
  daysSinceHaulStart := int(time.Since(a.haulStartDate).Hours() / 24)
  return daysSinceHaulStart >= a.haulDays
}

func (a *ZakatAccount) markUpdated(updatedBy UserID) {
  a.updatedAt = time.Now()
  a.updatedBy = updatedBy
  a.version++
}

// ========================================
// Getters
// ========================================

func (a *ZakatAccount) ID() AccountID {
  return a.id
}

func (a *ZakatAccount) HolderID() HolderID {
  return a.holderID
}

func (a *ZakatAccount) Balance() Money {
  return a.balance
}

func (a *ZakatAccount) NisabThreshold() Money {
  return a.nisabThreshold
}

func (a *ZakatAccount) Status() ZakatAccountStatus {
  return a.status
}

func (a *ZakatAccount) HaulStartDate() time.Time {
  return a.haulStartDate
}

func (a *ZakatAccount) LastZakatPaymentDate() *time.Time {
  return a.lastZakatPaymentDate
}

func (a *ZakatAccount) OpenedAt() time.Time {
  return a.openedAt
}

func (a *ZakatAccount) OpenedBy() UserID {
  return a.openedBy
}

func (a *ZakatAccount) UpdatedAt() time.Time {
  return a.updatedAt
}

func (a *ZakatAccount) UpdatedBy() UserID {
  return a.updatedBy
}

func (a *ZakatAccount) ClosedAt() *time.Time {
  return a.closedAt
}

func (a *ZakatAccount) Version() int {
  return a.version
}

func (a *ZakatAccount) Payments() []ZakatPaymentID {
  // Defensive copy
  result := make([]ZakatPaymentID, len(a.payments))
  copy(result, a.payments)
  return result
}

func (a *ZakatAccount) IsActive() bool {
  return a.status == ZakatAccountStatusActive && a.closedAt == nil
}

func (a *ZakatAccount) IsClosed() bool {
  return a.closedAt != nil
}

func (a *ZakatAccount) IsEligibleForZakat() bool {
  calc := a.CalculateZakat()
  return calc.IsEligible()
}

// ========================================
// Equality (Identity-Based)
// ========================================

func (a *ZakatAccount) Equals(other *ZakatAccount) bool {
  if a == nil || other == nil {
    return a == other
  }
  return a.id == other.id
}

// ========================================
// String Representation
// ========================================

func (a *ZakatAccount) String() string {
  return fmt.Sprintf("ZakatAccount{id=%v, balance=%s, status=%s, eligible=%t, version=%d}",
    a.id, a.balance, a.status, a.IsEligibleForZakat(), a.version)
}

// ========================================
// Supporting Types
// ========================================

type ZakatAccountStatus string

const (
  ZakatAccountStatusActive    ZakatAccountStatus = "ACTIVE"
  ZakatAccountStatusSuspended ZakatAccountStatus = "SUSPENDED"
  ZakatAccountStatusClosed    ZakatAccountStatus = "CLOSED"
)

func (s ZakatAccountStatus) String() string {
  return string(s)
}

// ZakatCalculation represents Zakat calculation result.
type ZakatCalculation struct {
  eligible       bool
  zakatAmount    Money
  balance        Money
  nisabThreshold Money
  reason         string
}

func (c ZakatCalculation) IsEligible() bool {
  return c.eligible
}

func (c ZakatCalculation) ZakatAmount() Money {
  return c.zakatAmount
}

func (c ZakatCalculation) Balance() Money {
  return c.balance
}

func (c ZakatCalculation) NisabThreshold() Money {
  return c.nisabThreshold
}

func (c ZakatCalculation) Reason() string {
  return c.reason
}
```

## Entity vs Value Object

Understanding the difference between entities and value objects is crucial for proper domain modeling.

### Key Differences

| Aspect         | Entity                          | Value Object              |
| -------------- | ------------------------------- | ------------------------- |
| **Identity**   | Has unique ID                   | No identity               |
| **Equality**   | Based on ID only                | Based on all attributes   |
| **Mutability** | Mutable state                   | Immutable (by convention) |
| **Lifecycle**  | CreatedAt, UpdatedAt, DeletedAt | No lifecycle tracking     |
| **Version**    | Version field for locking       | No versioning             |
| **Pointers**   | Always use pointers (`*Entity`) | Use values (no pointer)   |
| **Receivers**  | Pointer receivers for mutations | Value receivers           |

### Side-by-Side Comparison

```go
// ENTITY: User (identity-based, mutable)
type User struct {
  id        UserID    // Identity
  email     Email     // Can change
  createdAt time.Time // Lifecycle
  updatedAt time.Time
  version   int       // Versioning
}

func NewUser(id UserID, email Email) (*User, error) {
  return &User{ // Returns POINTER
    id:        id,
    email:     email,
    createdAt: time.Now(),
    updatedAt: time.Now(),
    version:   0,
  }, nil
}

// Pointer receiver - can mutate
func (u *User) UpdateEmail(email Email) error {
  u.email = email     // Mutates state
  u.updatedAt = time.Now()
  u.version++
  return nil
}

// Identity-based equality
func (u *User) Equals(other *User) bool {
  return u.id == other.id // ID only!
}

// VALUE OBJECT: Email (value-based, immutable)
type Email struct {
  value string // No ID
}

func NewEmail(value string) (Email, error) {
  if !isValidEmail(value) {
    return Email{}, errors.New("invalid email")
  }
  return Email{value: value}, nil // Returns VALUE
}

// Value receiver - cannot mutate
func (e Email) WithDomain(domain string) (Email, error) {
  // Returns NEW instance, doesn't modify e
  local := strings.Split(e.value, "@")[0]
  return NewEmail(local + "@" + domain)
}

// Value-based equality
func (e Email) Equals(other Email) bool {
  return e.value == other.value // All fields!
}

func (e Email) Value() string {
  return e.value
}
```

### When to Use Each

**Use Entity when**:

- Object has a unique identity that persists over time
- Same object can have different attributes at different times
- Need to track changes and lifecycle
- Need optimistic locking for concurrent updates
- Examples: User, Order, Account, Product

**Use Value Object when**:

- Object is defined by its attributes, not identity
- Two objects with same attributes are interchangeable
- Object should be immutable
- No need for lifecycle tracking
- Examples: Email, Money, Address, DateRange

## Testing Entities

### Basic Entity Tests

```go
package model_test

import (
  "testing"
  "time"

  "myapp/model"
)

func TestZakatAccount_NewZakatAccount(t *testing.T) {
  tests := []struct {
    name          string
    accountID     model.AccountID
    holderID      model.HolderID
    balance       model.Money
    nisab         model.Money
    openedBy      model.UserID
    wantErr       bool
    expectedError string
  }{
    {
      "valid account",
      mustAccountID("ACC-001"),
      mustHolderID("HOLDER-001"),
      mustMoney(100000, "USD"),
      mustMoney(85000, "USD"),
      mustUserID("USER-001"),
      false,
      "",
    },
    {
      "missing account ID",
      model.AccountID{},
      mustHolderID("HOLDER-001"),
      mustMoney(100000, "USD"),
      mustMoney(85000, "USD"),
      mustUserID("USER-001"),
      true,
      "account ID required",
    },
    {
      "negative balance",
      mustAccountID("ACC-002"),
      mustHolderID("HOLDER-001"),
      mustMoney(-10000, "USD"),
      mustMoney(85000, "USD"),
      mustUserID("USER-001"),
      true,
      "invalid initial balance",
    },
    {
      "currency mismatch",
      mustAccountID("ACC-003"),
      mustHolderID("HOLDER-001"),
      mustMoney(100000, "USD"),
      mustMoney(85000, "EUR"),
      mustUserID("USER-001"),
      true,
      "balance and nisab must be in same currency",
    },
  }

  for _, tt := range tests {
    t.Run(tt.name, func(t *testing.T) {
      got, err := model.NewZakatAccount(
        tt.accountID,
        tt.holderID,
        tt.balance,
        tt.nisab,
        tt.openedBy,
      )

      if (err != nil) != tt.wantErr {
        t.Errorf("NewZakatAccount() error = %v, wantErr %v", err, tt.wantErr)
        return
      }

      if tt.wantErr && err != nil {
        if !contains(err.Error(), tt.expectedError) {
          t.Errorf("error message = %v, want substring %v", err.Error(), tt.expectedError)
        }
        return
      }

      // Verify initial state
      if got.ID() != tt.accountID {
        t.Errorf("ID() = %v, want %v", got.ID(), tt.accountID)
      }

      if got.Balance().Amount() != tt.balance.Amount() {
        t.Errorf("Balance() = %v, want %v", got.Balance(), tt.balance)
      }

      if got.Status() != model.ZakatAccountStatusActive {
        t.Errorf("Status() = %v, want %v", got.Status(), model.ZakatAccountStatusActive)
      }

      if got.Version() != 0 {
        t.Errorf("Version() = %v, want 0", got.Version())
      }
    })
  }
}

func TestZakatAccount_Deposit(t *testing.T) {
  // Setup
  account := mustNewZakatAccount(
    mustAccountID("ACC-001"),
    mustHolderID("HOLDER-001"),
    mustMoney(100000, "USD"),
    mustMoney(85000, "USD"),
    mustUserID("USER-001"),
  )

  userID := mustUserID("USER-002")

  tests := []struct {
    name      string
    amount    model.Money
    wantErr   bool
    wantFinal int64
  }{
    {"valid deposit", mustMoney(50000, "USD"), false, 150000},
    {"negative amount", mustMoney(-10000, "USD"), true, 100000},
    {"wrong currency", mustMoney(10000, "EUR"), true, 100000},
  }

  for _, tt := range tests {
    t.Run(tt.name, func(t *testing.T) {
      // Clone account for test isolation
      testAccount := cloneAccount(account)
      initialVersion := testAccount.Version()

      err := testAccount.Deposit(tt.amount, userID)

      if (err != nil) != tt.wantErr {
        t.Errorf("Deposit() error = %v, wantErr %v", err, tt.wantErr)
        return
      }

      if tt.wantErr {
        // Version should not increment on error
        if testAccount.Version() != initialVersion {
          t.Errorf("Version incremented on error: %d -> %d", initialVersion, testAccount.Version())
        }
        return
      }

      // Verify balance updated
      if testAccount.Balance().Amount() != tt.wantFinal {
        t.Errorf("Balance() = %d, want %d", testAccount.Balance().Amount(), tt.wantFinal)
      }

      // Verify version incremented
      if testAccount.Version() != initialVersion+1 {
        t.Errorf("Version() = %d, want %d", testAccount.Version(), initialVersion+1)
      }

      // Verify updatedBy set
      if testAccount.UpdatedBy() != userID {
        t.Errorf("UpdatedBy() = %v, want %v", testAccount.UpdatedBy(), userID)
      }
    })
  }
}

func TestZakatAccount_CalculateZakat(t *testing.T) {
  now := time.Now()
  yearAgo := now.AddDate(0, 0, -365) // One year ago

  tests := []struct {
    name          string
    balance       int64
    nisab         int64
    haulStartDate time.Time
    wantEligible  bool
    wantAmount    int64
  }{
    {
      "eligible - meets nisab and haul",
      100000, // $1000.00
      85000,  // $850.00 nisab
      yearAgo,
      true,
      2500, // 2.5% = $25.00
    },
    {
      "below nisab",
      50000, // $500.00
      85000, // $850.00 nisab
      yearAgo,
      false,
      0,
    },
    {
      "haul not completed",
      100000, // $1000.00
      85000,  // $850.00 nisab
      now.AddDate(0, 0, -100), // 100 days ago
      false,
      0,
    },
  }

  for _, tt := range tests {
    t.Run(tt.name, func(t *testing.T) {
      account := model.ReconstituteZakatAccount(
        mustAccountID("ACC-001"),
        mustHolderID("HOLDER-001"),
        mustMoney(tt.balance, "USD"),
        mustMoney(tt.nisab, "USD"),
        model.ZakatAccountStatusActive,
        tt.haulStartDate,
        nil,
        now,
        mustUserID("USER-001"),
        now,
        mustUserID("USER-001"),
        nil,
        0,
        []model.ZakatPaymentID{},
      )

      calc := account.CalculateZakat()

      if calc.IsEligible() != tt.wantEligible {
        t.Errorf("IsEligible() = %v, want %v", calc.IsEligible(), tt.wantEligible)
      }

      if tt.wantEligible && calc.ZakatAmount().Amount() != tt.wantAmount {
        t.Errorf("ZakatAmount() = %d, want %d", calc.ZakatAmount().Amount(), tt.wantAmount)
      }
    })
  }
}

func TestZakatAccount_Equals(t *testing.T) {
  id1 := mustAccountID("ACC-001")
  id2 := mustAccountID("ACC-002")

  account1 := mustNewZakatAccount(
    id1,
    mustHolderID("HOLDER-001"),
    mustMoney(100000, "USD"),
    mustMoney(85000, "USD"),
    mustUserID("USER-001"),
  )

  account2 := mustNewZakatAccount(
    id1, // Same ID
    mustHolderID("HOLDER-002"), // Different holder
    mustMoney(200000, "USD"),   // Different balance
    mustMoney(85000, "USD"),
    mustUserID("USER-002"),
  )

  account3 := mustNewZakatAccount(
    id2, // Different ID
    mustHolderID("HOLDER-001"),
    mustMoney(100000, "USD"),
    mustMoney(85000, "USD"),
    mustUserID("USER-001"),
  )

  // Same ID = equal (even with different state)
  if !account1.Equals(account2) {
    t.Error("accounts with same ID should be equal")
  }

  // Different ID = not equal (even with same state)
  if account1.Equals(account3) {
    t.Error("accounts with different IDs should not be equal")
  }

  // Nil handling
  if account1.Equals(nil) {
    t.Error("non-nil account should not equal nil")
  }

  var nilAccount *model.ZakatAccount
  if !nilAccount.Equals(nil) {
    t.Error("nil account should equal nil")
  }
}

// Helper functions
func mustAccountID(s string) model.AccountID {
  id, _ := model.NewAccountID(s)
  return id
}

func mustHolderID(s string) model.HolderID {
  id, _ := model.NewHolderID(s)
  return id
}

func mustUserID(s string) model.UserID {
  id, _ := model.NewUserID(s)
  return id
}

func mustMoney(amount int64, currency string) model.Money {
  return model.MustNewMoney(amount, currency)
}

func mustNewZakatAccount(
  id model.AccountID,
  holderID model.HolderID,
  balance model.Money,
  nisab model.Money,
  openedBy model.UserID,
) *model.ZakatAccount {
  account, err := model.NewZakatAccount(id, holderID, balance, nisab, openedBy)
  if err != nil {
    panic(err)
  }
  return account
}

func cloneAccount(a *model.ZakatAccount) *model.ZakatAccount {
  return model.ReconstituteZakatAccount(
    a.ID(),
    a.HolderID(),
    a.Balance(),
    a.NisabThreshold(),
    a.Status(),
    a.HaulStartDate(),
    a.LastZakatPaymentDate(),
    a.OpenedAt(),
    a.OpenedBy(),
    a.UpdatedAt(),
    a.UpdatedBy(),
    a.ClosedAt(),
    a.Version(),
    a.Payments(),
  )
}

func contains(s, substr string) bool {
  return len(s) >= len(substr) && s[:len(substr)] == substr
}
```

## Related Documentation

**Go Language Documentation**:

- [Best Practices](../ex-so-stla-go__best-practices.md) - Go coding standards
- [Idioms](../ex-so-stla-go__idioms.md) - Go-specific patterns
- [Interfaces and Composition](../ex-so-stla-go__interfaces-and-composition.md) - Interface design

**Template Documentation**:

- [Value Object Template](./value-object-template.md) - Immutable value objects
- Entity vs Value Object comparison

**DDD Concepts**:

- Aggregate pattern (combines entities and value objects)
- Repository pattern (persistence)
- Domain events (entity state changes)

**Principles**:

- [Simplicity Over Complexity](../../../../../../governance/principles/general/simplicity-over-complexity.md)
- [Explicit Over Implicit](../../../../../../governance/principles/software-engineering/explicit-over-implicit.md)

---

**Principles Applied**: Simplicity Over Complexity, Explicit Over Implicit
