---
title: Go Repository Template
description: Template for creating Domain-Driven Design repositories in Go with persistence abstraction, CRUD operations, and business queries
category: template
tags:
  - golang
  - ddd
  - repository
  - persistence
  - database
  - sql
  - transactions
  - specification
  - go-1.18
  - go-1.21
  - go-1.22
  - go-1.23
  - go-1.24
  - go-1.25
related:
  - aggregate-template.md
  - entity-template.md
  - ex-so-stla-go__domain-driven-design.md
  - ex-so-stla-go__best-practices.md
principles:
  - simplicity-over-complexity
  - explicit-over-implicit
created: 2026-01-22
updated: 2026-01-22
---

# Go Repository Template

This template provides a standardized structure for creating Domain-Driven Design (DDD) repositories in Go. Repositories provide a clean abstraction over data persistence, allowing domain models to remain independent of database implementation details.

## Table of Contents

1. [Overview](#overview)
2. [Template Structure](#template-structure)
3. [Repository Interface](#repository-interface)
4. [SQL Implementation](#sql-implementation)
5. [Business Queries](#business-queries)
6. [Specification Pattern](#specification-pattern)
7. [Transaction Support](#transaction-support)
8. [Complete Example: Zakat Account Repository](#complete-example-zakat-account-repository)
9. [Before/After Comparison](#beforeafter-comparison)
10. [Usage Guidelines](#usage-guidelines)
11. [Testing Repositories](#testing-repositories)
12. [Related Documentation](#related-documentation)

## Overview

Repositories in Go serve as the **boundary between domain models and persistence infrastructure**. They provide a collection-like interface for accessing and storing aggregates, hiding database complexity from business logic.

**Key Characteristics**:

- **Interface-first design**: Domain layer defines interface, infrastructure implements
- **Aggregate-focused**: One repository per aggregate root (not per entity)
- **Context-based transactions**: Use `context.Context` for transaction propagation
- **Business query methods**: Domain-specific queries beyond basic CRUD
- **Standard error handling**: Return explicit errors, no panics
- **Reconstitution**: Load aggregates from persistence using reconstitution functions

**Go vs Java Differences**:

```go
// Go: Interface in domain package, implementation in infrastructure
package domain

// Repository interface (domain layer)
type ZakatAccountRepository interface {
 Save(ctx context.Context, account *ZakatAccount) error
 FindByID(ctx context.Context, id AccountID) (*ZakatAccount, error)
 FindByOwner(ctx context.Context, ownerID HolderID) ([]*ZakatAccount, error)
 Delete(ctx context.Context, id AccountID) error
}

// SQL implementation (infrastructure layer)
package infrastructure

type SQLZakatAccountRepository struct {
 db *sql.DB
}

func NewSQLZakatAccountRepository(db *sql.DB) *SQLZakatAccountRepository {
 return &SQLZakatAccountRepository{db: db}
}

func (r *SQLZakatAccountRepository) Save(ctx context.Context, account *ZakatAccount) error {
 // Implementation using database/sql
}
```

```java
// Java: Interface in domain, implementation in infrastructure
package com.example.domain;

// Repository interface (domain layer)
public interface ZakatAccountRepository {
    void save(ZakatAccount account);
    Optional<ZakatAccount> findById(AccountId id);
    List<ZakatAccount> findByOwner(HolderId ownerId);
    void delete(AccountId id);
}

// JPA implementation (infrastructure layer)
package com.example.infrastructure;

@Repository
public class JpaZakatAccountRepository implements ZakatAccountRepository {
    @PersistenceContext
    private EntityManager entityManager;

    @Override
    public void save(ZakatAccount account) {
        entityManager.persist(account);
    }
}
```

**Critical Differences**:

- Go uses **`context.Context` for transaction propagation** (no ThreadLocal)
- Go returns **explicit errors** instead of throwing exceptions
- Go uses **`database/sql` package** instead of ORM frameworks
- Go repositories use **reconstitution functions** to load aggregates
- Go has **no automatic dependency injection** (manual wiring)
- Go uses **prepared statements** for SQL safety

## Template Structure

```go
package domain

import "context"

// ========================================
// Repository Interface (Domain Layer)
// ========================================

// AggregateRepository provides persistence operations for AggregateRoot entities.
//
// Responsibilities:
//   - Save and retrieve aggregates
//   - Execute domain-specific queries
//   - Hide persistence implementation details
//   - Maintain aggregate boundaries
//
// Transactional Operations:
//   - All methods accept context.Context for transaction support
//   - Use WithTransaction to execute multiple operations atomically
//
// Error Handling:
//   - Returns ErrNotFound when aggregate doesn't exist
//   - Returns ErrOptimisticLockConflict when version mismatch occurs
//   - Returns infrastructure errors wrapped with context
//
// Example:
//
// repo := NewSQLAggregateRepository(db)
// aggregate, err := repo.FindByID(ctx, id)
// if err != nil {
//  return err
// }
// err = repo.Save(ctx, aggregate)
type AggregateRepository interface {
 // ========================================
 // Standard CRUD Operations
 // ========================================

 // Save creates or updates an aggregate.
 //
 // Uses optimistic locking based on version field.
 // Returns ErrOptimisticLockConflict if version mismatch.
 Save(ctx context.Context, aggregate *AggregateRoot) error

 // FindByID retrieves an aggregate by its ID.
 //
 // Returns ErrNotFound if aggregate doesn't exist.
 // Excludes soft-deleted aggregates by default.
 FindByID(ctx context.Context, id AggregateID) (*AggregateRoot, error)

 // FindByIDIncludingDeleted retrieves an aggregate including soft-deleted.
 //
 // Returns ErrNotFound if aggregate doesn't exist.
 FindByIDIncludingDeleted(ctx context.Context, id AggregateID) (*AggregateRoot, error)

 // Delete removes an aggregate (hard delete).
 //
 // Returns ErrNotFound if aggregate doesn't exist.
 // Consider using soft delete via aggregate method instead.
 Delete(ctx context.Context, id AggregateID) error

 // ========================================
 // Business Query Methods
 // ========================================

 // FindByOwner retrieves all aggregates for a specific owner.
 //
 // Returns empty slice if no aggregates found (not an error).
 // Excludes soft-deleted aggregates.
 FindByOwner(ctx context.Context, ownerID OwnerID) ([]*AggregateRoot, error)

 // FindByStatus retrieves all aggregates with a specific status.
 //
 // Returns empty slice if no aggregates found (not an error).
 // Excludes soft-deleted aggregates.
 FindByStatus(ctx context.Context, status AggregateStatus) ([]*AggregateRoot, error)

 // FindActive retrieves all active aggregates.
 //
 // Active = not deleted AND status is active.
 // Returns empty slice if no aggregates found (not an error).
 FindActive(ctx context.Context) ([]*AggregateRoot, error)

 // ========================================
 // List Operations
 // ========================================

 // List retrieves all aggregates with pagination.
 //
 // limit: Maximum number of results (0 = no limit)
 // offset: Number of results to skip
 // Returns empty slice if no aggregates found (not an error).
 List(ctx context.Context, limit, offset int) ([]*AggregateRoot, error)

 // Count returns the total number of aggregates.
 //
 // Excludes soft-deleted aggregates.
 Count(ctx context.Context) (int, error)

 // ========================================
 // Transaction Support
 // ========================================

 // WithTransaction executes a function within a database transaction.
 //
 // Commits if function returns nil, rolls back on error.
 // Transaction context is propagated to repository methods.
 WithTransaction(ctx context.Context, fn func(context.Context) error) error
}
```

## Repository Interface

The repository interface lives in the **domain layer** and defines all persistence operations without implementation details.

### Standard CRUD Methods

```go
package domain

import (
 "context"
 "errors"
)

// Standard repository errors
var (
 ErrNotFound               = errors.New("aggregate not found")
 ErrOptimisticLockConflict = errors.New("optimistic locking conflict")
 ErrInvalidID              = errors.New("invalid aggregate ID")
 ErrDuplicateKey           = errors.New("duplicate key violation")
)

// UserRepository manages User aggregate persistence.
type UserRepository interface {
 // Save inserts or updates a user
 Save(ctx context.Context, user *User) error

 // FindByID retrieves a user by ID
 FindByID(ctx context.Context, id UserID) (*User, error)

 // Delete removes a user (hard delete)
 Delete(ctx context.Context, id UserID) error

 // Count returns total number of users
 Count(ctx context.Context) (int, error)
}
```

### Business Query Methods

```go
// UserRepository with business queries
type UserRepository interface {
 // Standard CRUD
 Save(ctx context.Context, user *User) error
 FindByID(ctx context.Context, id UserID) (*User, error)
 Delete(ctx context.Context, id UserID) error

 // Business queries
 FindByEmail(ctx context.Context, email Email) (*User, error)
 FindByRole(ctx context.Context, role UserRole) ([]*User, error)
 FindActive(ctx context.Context) ([]*User, error)
 FindRegisteredAfter(ctx context.Context, date time.Time) ([]*User, error)

 // Existence checks
 ExistsByEmail(ctx context.Context, email Email) (bool, error)
 ExistsActiveByEmail(ctx context.Context, email Email) (bool, error)
}
```

### Pagination Support

```go
// Repository with pagination
type OrderRepository interface {
 // Basic operations
 Save(ctx context.Context, order *Order) error
 FindByID(ctx context.Context, id OrderID) (*Order, error)

 // Paginated queries
 List(ctx context.Context, limit, offset int) ([]*Order, error)
 FindByCustomer(ctx context.Context, customerID CustomerID, limit, offset int) ([]*Order, error)

 // Count for pagination
 Count(ctx context.Context) (int, error)
 CountByCustomer(ctx context.Context, customerID CustomerID) (int, error)
}
```

## SQL Implementation

Repository implementations use Go's **`database/sql`** package with prepared statements for safety and performance.

### Basic SQL Repository

```go
package infrastructure

import (
 "context"
 "database/sql"
 "errors"
 "fmt"

 "myapp/domain"
)

// SQLUserRepository implements UserRepository using database/sql.
type SQLUserRepository struct {
 db *sql.DB
}

func NewSQLUserRepository(db *sql.DB) *SQLUserRepository {
 return &SQLUserRepository{db: db}
}

// Save inserts or updates a user with optimistic locking
func (r *SQLUserRepository) Save(ctx context.Context, user *domain.User) error {
 // Check if user exists
 var exists bool
 err := r.db.QueryRowContext(ctx,
  "SELECT EXISTS(SELECT 1 FROM users WHERE id = $1)",
  user.ID().Value(),
 ).Scan(&exists)
 if err != nil {
  return fmt.Errorf("check user existence: %w", err)
 }

 if exists {
  // Update with optimistic locking
  return r.update(ctx, user)
 }

 // Insert new user
 return r.insert(ctx, user)
}

func (r *SQLUserRepository) insert(ctx context.Context, user *domain.User) error {
 query := `
  INSERT INTO users (
   id, email, name, role, status,
   created_at, created_by, updated_at, updated_by, version
  ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)
 `

 _, err := r.db.ExecContext(ctx, query,
  user.ID().Value(),
  user.Email().Value(),
  user.Name(),
  user.Role().String(),
  user.Status().String(),
  user.CreatedAt(),
  user.CreatedBy().Value(),
  user.UpdatedAt(),
  user.UpdatedBy().Value(),
  user.Version(),
 )

 if err != nil {
  // Check for duplicate key
  if isDuplicateKeyError(err) {
   return domain.ErrDuplicateKey
  }
  return fmt.Errorf("insert user: %w", err)
 }

 return nil
}

func (r *SQLUserRepository) update(ctx context.Context, user *domain.User) error {
 query := `
  UPDATE users
  SET email = $1, name = $2, role = $3, status = $4,
      updated_at = $5, updated_by = $6, version = $7
  WHERE id = $8 AND version = $9
 `

 result, err := r.db.ExecContext(ctx, query,
  user.Email().Value(),
  user.Name(),
  user.Role().String(),
  user.Status().String(),
  user.UpdatedAt(),
  user.UpdatedBy().Value(),
  user.Version(),        // New version
  user.ID().Value(),
  user.Version()-1,      // Expected old version
 )

 if err != nil {
  return fmt.Errorf("update user: %w", err)
 }

 rowsAffected, err := result.RowsAffected()
 if err != nil {
  return fmt.Errorf("check rows affected: %w", err)
 }

 // No rows affected = version mismatch (optimistic lock conflict)
 if rowsAffected == 0 {
  return domain.ErrOptimisticLockConflict
 }

 return nil
}

// FindByID retrieves a user by ID
func (r *SQLUserRepository) FindByID(ctx context.Context, id domain.UserID) (*domain.User, error) {
 query := `
  SELECT id, email, name, role, status,
         created_at, created_by, updated_at, updated_by, deleted_at, version
  FROM users
  WHERE id = $1 AND deleted_at IS NULL
 `

 var (
  userID    string
  email     string
  name      string
  role      string
  status    string
  createdAt time.Time
  createdBy string
  updatedAt time.Time
  updatedBy string
  deletedAt *time.Time
  version   int
 )

 err := r.db.QueryRowContext(ctx, query, id.Value()).Scan(
  &userID, &email, &name, &role, &status,
  &createdAt, &createdBy, &updatedAt, &updatedBy, &deletedAt, &version,
 )

 if err != nil {
  if errors.Is(err, sql.ErrNoRows) {
   return nil, domain.ErrNotFound
  }
  return nil, fmt.Errorf("query user: %w", err)
 }

 // Reconstitute domain object
 return r.reconstitute(
  userID, email, name, role, status,
  createdAt, createdBy, updatedAt, updatedBy, deletedAt, version,
 )
}

// FindByEmail retrieves a user by email address
func (r *SQLUserRepository) FindByEmail(ctx context.Context, email domain.Email) (*domain.User, error) {
 query := `
  SELECT id, email, name, role, status,
         created_at, created_by, updated_at, updated_by, deleted_at, version
  FROM users
  WHERE email = $1 AND deleted_at IS NULL
 `

 var (
  userID    string
  emailStr  string
  name      string
  role      string
  status    string
  createdAt time.Time
  createdBy string
  updatedAt time.Time
  updatedBy string
  deletedAt *time.Time
  version   int
 )

 err := r.db.QueryRowContext(ctx, query, email.Value()).Scan(
  &userID, &emailStr, &name, &role, &status,
  &createdAt, &createdBy, &updatedAt, &updatedBy, &deletedAt, &version,
 )

 if err != nil {
  if errors.Is(err, sql.ErrNoRows) {
   return nil, domain.ErrNotFound
  }
  return nil, fmt.Errorf("query user by email: %w", err)
 }

 return r.reconstitute(
  userID, emailStr, name, role, status,
  createdAt, createdBy, updatedAt, updatedBy, deletedAt, version,
 )
}

// Delete removes a user (hard delete)
func (r *SQLUserRepository) Delete(ctx context.Context, id domain.UserID) error {
 result, err := r.db.ExecContext(ctx,
  "DELETE FROM users WHERE id = $1",
  id.Value(),
 )

 if err != nil {
  return fmt.Errorf("delete user: %w", err)
 }

 rowsAffected, err := result.RowsAffected()
 if err != nil {
  return fmt.Errorf("check rows affected: %w", err)
 }

 if rowsAffected == 0 {
  return domain.ErrNotFound
 }

 return nil
}

// Count returns total number of users
func (r *SQLUserRepository) Count(ctx context.Context) (int, error) {
 var count int
 err := r.db.QueryRowContext(ctx,
  "SELECT COUNT(*) FROM users WHERE deleted_at IS NULL",
 ).Scan(&count)

 if err != nil {
  return 0, fmt.Errorf("count users: %w", err)
 }

 return count, nil
}

// reconstitute creates a User from database fields
func (r *SQLUserRepository) reconstitute(
 userID, email, name, role, status string,
 createdAt time.Time, createdBy string,
 updatedAt time.Time, updatedBy string,
 deletedAt *time.Time, version int,
) (*domain.User, error) {
 id, err := domain.NewUserID(userID)
 if err != nil {
  return nil, fmt.Errorf("invalid user ID: %w", err)
 }

 emailVO, err := domain.NewEmail(email)
 if err != nil {
  return nil, fmt.Errorf("invalid email: %w", err)
 }

 createdByID, err := domain.NewUserID(createdBy)
 if err != nil {
  return nil, fmt.Errorf("invalid created by ID: %w", err)
 }

 updatedByID, err := domain.NewUserID(updatedBy)
 if err != nil {
  return nil, fmt.Errorf("invalid updated by ID: %w", err)
 }

 return domain.ReconstituteUser(
  id,
  emailVO,
  name,
  domain.UserRole(role),
  domain.UserStatus(status),
  createdAt,
  createdByID,
  updatedAt,
  updatedByID,
  deletedAt,
  version,
 ), nil
}

// Helper: Check if error is duplicate key constraint
func isDuplicateKeyError(err error) bool {
 // PostgreSQL duplicate key error code: 23505
 // MySQL duplicate key error code: 1062
 // Implement based on your database driver
 return false // Placeholder
}
```

## Business Queries

Repository business queries encapsulate domain-specific data access patterns.

### Domain-Specific Queries

```go
package infrastructure

import (
 "context"
 "database/sql"
 "errors"
 "fmt"
 "time"

 "myapp/domain"
)

// FindByRole retrieves all users with a specific role
func (r *SQLUserRepository) FindByRole(ctx context.Context, role domain.UserRole) ([]*domain.User, error) {
 query := `
  SELECT id, email, name, role, status,
         created_at, created_by, updated_at, updated_by, deleted_at, version
  FROM users
  WHERE role = $1 AND deleted_at IS NULL
  ORDER BY created_at DESC
 `

 rows, err := r.db.QueryContext(ctx, query, role.String())
 if err != nil {
  return nil, fmt.Errorf("query users by role: %w", err)
 }
 defer rows.Close()

 var users []*domain.User
 for rows.Next() {
  user, err := r.scanUser(rows)
  if err != nil {
   return nil, err
  }
  users = append(users, user)
 }

 if err := rows.Err(); err != nil {
  return nil, fmt.Errorf("iterate users: %w", err)
 }

 return users, nil
}

// FindActive retrieves all active users
func (r *SQLUserRepository) FindActive(ctx context.Context) ([]*domain.User, error) {
 query := `
  SELECT id, email, name, role, status,
         created_at, created_by, updated_at, updated_by, deleted_at, version
  FROM users
  WHERE status = 'ACTIVE' AND deleted_at IS NULL
  ORDER BY name ASC
 `

 rows, err := r.db.QueryContext(ctx, query)
 if err != nil {
  return nil, fmt.Errorf("query active users: %w", err)
 }
 defer rows.Close()

 var users []*domain.User
 for rows.Next() {
  user, err := r.scanUser(rows)
  if err != nil {
   return nil, err
  }
  users = append(users, user)
 }

 return users, rows.Err()
}

// FindRegisteredAfter retrieves users registered after a specific date
func (r *SQLUserRepository) FindRegisteredAfter(ctx context.Context, date time.Time) ([]*domain.User, error) {
 query := `
  SELECT id, email, name, role, status,
         created_at, created_by, updated_at, updated_by, deleted_at, version
  FROM users
  WHERE created_at > $1 AND deleted_at IS NULL
  ORDER BY created_at DESC
 `

 rows, err := r.db.QueryContext(ctx, query, date)
 if err != nil {
  return nil, fmt.Errorf("query users registered after: %w", err)
 }
 defer rows.Close()

 var users []*domain.User
 for rows.Next() {
  user, err := r.scanUser(rows)
  if err != nil {
   return nil, err
  }
  users = append(users, user)
 }

 return users, rows.Err()
}

// ExistsByEmail checks if a user with email exists
func (r *SQLUserRepository) ExistsByEmail(ctx context.Context, email domain.Email) (bool, error) {
 var exists bool
 err := r.db.QueryRowContext(ctx,
  "SELECT EXISTS(SELECT 1 FROM users WHERE email = $1 AND deleted_at IS NULL)",
  email.Value(),
 ).Scan(&exists)

 if err != nil {
  return false, fmt.Errorf("check user exists by email: %w", err)
 }

 return exists, nil
}

// scanUser scans a single row into a User
func (r *SQLUserRepository) scanUser(scanner interface {
 Scan(dest ...interface{}) error
}) (*domain.User, error) {
 var (
  userID    string
  email     string
  name      string
  role      string
  status    string
  createdAt time.Time
  createdBy string
  updatedAt time.Time
  updatedBy string
  deletedAt *time.Time
  version   int
 )

 err := scanner.Scan(
  &userID, &email, &name, &role, &status,
  &createdAt, &createdBy, &updatedAt, &updatedBy, &deletedAt, &version,
 )

 if err != nil {
  return nil, fmt.Errorf("scan user: %w", err)
 }

 return r.reconstitute(
  userID, email, name, role, status,
  createdAt, createdBy, updatedAt, updatedBy, deletedAt, version,
 )
}
```

### Financial Domain Example: Zakat Account Queries

```go
package infrastructure

// FindByOwner retrieves all Zakat accounts for a specific owner
func (r *SQLZakatAccountRepository) FindByOwner(ctx context.Context, ownerID domain.HolderID) ([]*domain.ZakatAccount, error) {
 query := `
  SELECT id, holder_id, balance_amount, balance_currency,
         nisab_amount, nisab_currency, status,
         haul_start_date, last_payment_date,
         opened_at, opened_by, updated_at, updated_by, closed_at, version
  FROM zakat_accounts
  WHERE holder_id = $1 AND closed_at IS NULL
  ORDER BY opened_at DESC
 `

 rows, err := r.db.QueryContext(ctx, query, ownerID.Value())
 if err != nil {
  return nil, fmt.Errorf("query accounts by owner: %w", err)
 }
 defer rows.Close()

 var accounts []*domain.ZakatAccount
 for rows.Next() {
  account, err := r.scanAccount(rows)
  if err != nil {
   return nil, err
  }
  accounts = append(accounts, account)
 }

 return accounts, rows.Err()
}

// FindEligibleForDistribution retrieves accounts eligible for Zakat distribution
func (r *SQLZakatAccountRepository) FindEligibleForDistribution(ctx context.Context) ([]*domain.ZakatAccount, error) {
 query := `
  SELECT id, holder_id, balance_amount, balance_currency,
         nisab_amount, nisab_currency, status,
         haul_start_date, last_payment_date,
         opened_at, opened_by, updated_at, updated_by, closed_at, version
  FROM zakat_accounts
  WHERE status = 'ACTIVE'
    AND closed_at IS NULL
    AND balance_amount >= nisab_amount
    AND haul_start_date <= NOW() - INTERVAL '354 days'
  ORDER BY haul_start_date ASC
 `

 rows, err := r.db.QueryContext(ctx, query)
 if err != nil {
  return nil, fmt.Errorf("query eligible accounts: %w", err)
 }
 defer rows.Close()

 var accounts []*domain.ZakatAccount
 for rows.Next() {
  account, err := r.scanAccount(rows)
  if err != nil {
   return nil, err
  }
  accounts = append(accounts, account)
 }

 return accounts, rows.Err()
}

// FindByStatus retrieves accounts with a specific status
func (r *SQLZakatAccountRepository) FindByStatus(ctx context.Context, status domain.ZakatAccountStatus) ([]*domain.ZakatAccount, error) {
 query := `
  SELECT id, holder_id, balance_amount, balance_currency,
         nisab_amount, nisab_currency, status,
         haul_start_date, last_payment_date,
         opened_at, opened_by, updated_at, updated_by, closed_at, version
  FROM zakat_accounts
  WHERE status = $1 AND closed_at IS NULL
  ORDER BY opened_at DESC
 `

 rows, err := r.db.QueryContext(ctx, query, status.String())
 if err != nil {
  return nil, fmt.Errorf("query accounts by status: %w", err)
 }
 defer rows.Close()

 var accounts []*domain.ZakatAccount
 for rows.Next() {
  account, err := r.scanAccount(rows)
  if err != nil {
   return nil, err
  }
  accounts = append(accounts, account)
 }

 return accounts, rows.Err()
}
```

## Specification Pattern

The specification pattern enables flexible query composition for complex filtering scenarios.

### Specification Interface

```go
package domain

// Specification defines a filtering predicate
type Specification interface {
 // ToSQL returns SQL WHERE clause and parameters
 ToSQL() (clause string, params []interface{})

 // And combines this specification with another using AND
 And(other Specification) Specification

 // Or combines this specification with another using OR
 Or(other Specification) Specification

 // Not negates this specification
 Not() Specification
}

// Base specification implementation
type baseSpec struct {
 clause string
 params []interface{}
}

func (s *baseSpec) ToSQL() (string, []interface{}) {
 return s.clause, s.params
}

func (s *baseSpec) And(other Specification) Specification {
 otherClause, otherParams := other.ToSQL()
 return &baseSpec{
  clause: fmt.Sprintf("(%s) AND (%s)", s.clause, otherClause),
  params: append(s.params, otherParams...),
 }
}

func (s *baseSpec) Or(other Specification) Specification {
 otherClause, otherParams := other.ToSQL()
 return &baseSpec{
  clause: fmt.Sprintf("(%s) OR (%s)", s.clause, otherClause),
  params: append(s.params, otherParams...),
 }
}

func (s *baseSpec) Not() Specification {
 return &baseSpec{
  clause: fmt.Sprintf("NOT (%s)", s.clause),
  params: s.params,
 }
}
```

### Concrete Specifications

```go
package domain

// User specifications

// UserByRoleSpec filters users by role
type UserByRoleSpec struct {
 baseSpec
}

func NewUserByRoleSpec(role UserRole) Specification {
 return &UserByRoleSpec{
  baseSpec: baseSpec{
   clause: "role = ?",
   params: []interface{}{role.String()},
  },
 }
}

// UserByStatusSpec filters users by status
type UserByStatusSpec struct {
 baseSpec
}

func NewUserByStatusSpec(status UserStatus) Specification {
 return &UserByStatusSpec{
  baseSpec: baseSpec{
   clause: "status = ?",
   params: []interface{}{status.String()},
  },
 }
}

// UserRegisteredAfterSpec filters users registered after date
type UserRegisteredAfterSpec struct {
 baseSpec
}

func NewUserRegisteredAfterSpec(date time.Time) Specification {
 return &UserRegisteredAfterSpec{
  baseSpec: baseSpec{
   clause: "created_at > ?",
   params: []interface{}{date},
  },
 }
}
```

### Repository with Specification Support

```go
package infrastructure

// FindBySpecification retrieves users matching specification
func (r *SQLUserRepository) FindBySpecification(ctx context.Context, spec domain.Specification) ([]*domain.User, error) {
 whereClause, params := spec.ToSQL()

 query := fmt.Sprintf(`
  SELECT id, email, name, role, status,
         created_at, created_by, updated_at, updated_by, deleted_at, version
  FROM users
  WHERE deleted_at IS NULL AND %s
  ORDER BY created_at DESC
 `, whereClause)

 rows, err := r.db.QueryContext(ctx, query, params...)
 if err != nil {
  return nil, fmt.Errorf("query users by specification: %w", err)
 }
 defer rows.Close()

 var users []*domain.User
 for rows.Next() {
  user, err := r.scanUser(rows)
  if err != nil {
   return nil, err
  }
  users = append(users, user)
 }

 return users, rows.Err()
}

// Usage example
func ExampleSpecification() {
 // Find active admin users registered in the last 30 days
 spec := domain.NewUserByRoleSpec(domain.RoleAdmin).
  And(domain.NewUserByStatusSpec(domain.StatusActive)).
  And(domain.NewUserRegisteredAfterSpec(time.Now().AddDate(0, 0, -30)))

 users, err := repo.FindBySpecification(ctx, spec)
 // ...
}
```

## Transaction Support

Repositories support transactions through **context propagation** using `context.Context`.

### Transaction Context Pattern

```go
package infrastructure

import (
 "context"
 "database/sql"
 "fmt"
)

// Transaction key for context
type txKey struct{}

// WithTransaction executes a function within a database transaction
func (r *SQLUserRepository) WithTransaction(ctx context.Context, fn func(context.Context) error) error {
 // Start transaction
 tx, err := r.db.BeginTx(ctx, nil)
 if err != nil {
  return fmt.Errorf("begin transaction: %w", err)
 }

 // Store transaction in context
 ctx = context.WithValue(ctx, txKey{}, tx)

 // Execute function
 err = fn(ctx)
 if err != nil {
  // Rollback on error
  if rbErr := tx.Rollback(); rbErr != nil {
   return fmt.Errorf("rollback failed: %v (original error: %w)", rbErr, err)
  }
  return err
 }

 // Commit on success
 if err := tx.Commit(); err != nil {
  return fmt.Errorf("commit transaction: %w", err)
 }

 return nil
}

// getTx retrieves transaction from context or returns db
func (r *SQLUserRepository) getTx(ctx context.Context) interface {
 ExecContext(ctx context.Context, query string, args ...interface{}) (sql.Result, error)
 QueryContext(ctx context.Context, query string, args ...interface{}) (*sql.Rows, error)
 QueryRowContext(ctx context.Context, query string, args ...interface{}) *sql.Row
} {
 if tx, ok := ctx.Value(txKey{}).(*sql.Tx); ok {
  return tx
 }
 return r.db
}

// Save using transaction-aware execution
func (r *SQLUserRepository) Save(ctx context.Context, user *domain.User) error {
 tx := r.getTx(ctx)

 // Check existence
 var exists bool
 err := tx.QueryRowContext(ctx,
  "SELECT EXISTS(SELECT 1 FROM users WHERE id = $1)",
  user.ID().Value(),
 ).Scan(&exists)

 if err != nil {
  return fmt.Errorf("check user existence: %w", err)
 }

 if exists {
  return r.updateTx(ctx, tx, user)
 }

 return r.insertTx(ctx, tx, user)
}

// Usage: Multiple operations in one transaction
func TransferMoney(ctx context.Context, userRepo *SQLUserRepository, fromID, toID domain.UserID, amount domain.Money) error {
 return userRepo.WithTransaction(ctx, func(txCtx context.Context) error {
  // Load sender
  sender, err := userRepo.FindByID(txCtx, fromID)
  if err != nil {
   return err
  }

  // Debit sender
  if err := sender.Debit(amount); err != nil {
   return err
  }

  // Save sender (within transaction)
  if err := userRepo.Save(txCtx, sender); err != nil {
   return err
  }

  // Load receiver
  receiver, err := userRepo.FindByID(txCtx, toID)
  if err != nil {
   return err
  }

  // Credit receiver
  if err := receiver.Credit(amount); err != nil {
   return err
  }

  // Save receiver (within transaction)
  if err := userRepo.Save(txCtx, receiver); err != nil {
   return err
  }

  // Both operations succeed or both rollback
  return nil
 })
}
```

### Unit of Work Pattern

```go
package infrastructure

import (
 "context"
 "database/sql"
)

// UnitOfWork coordinates multiple repository operations in one transaction
type UnitOfWork struct {
 db       *sql.DB
 userRepo *SQLUserRepository
 orderRepo *SQLOrderRepository
}

func NewUnitOfWork(db *sql.DB) *UnitOfWork {
 return &UnitOfWork{
  db:       db,
  userRepo: NewSQLUserRepository(db),
  orderRepo: NewSQLOrderRepository(db),
 }
}

// Execute runs a function with all repositories in one transaction
func (uow *UnitOfWork) Execute(ctx context.Context, fn func(context.Context, *SQLUserRepository, *SQLOrderRepository) error) error {
 tx, err := uow.db.BeginTx(ctx, nil)
 if err != nil {
  return fmt.Errorf("begin transaction: %w", err)
 }

 ctx = context.WithValue(ctx, txKey{}, tx)

 err = fn(ctx, uow.userRepo, uow.orderRepo)
 if err != nil {
  tx.Rollback()
  return err
 }

 return tx.Commit()
}

// Usage
func CreateOrderWithUser(ctx context.Context, uow *UnitOfWork, userData UserData, orderData OrderData) error {
 return uow.Execute(ctx, func(txCtx context.Context, userRepo *SQLUserRepository, orderRepo *SQLOrderRepository) error {
  // Create user
  user, err := domain.NewUser(userData.ID, userData.Email, userData.Name)
  if err != nil {
   return err
  }

  if err := userRepo.Save(txCtx, user); err != nil {
   return err
  }

  // Create order
  order, err := domain.NewOrder(orderData.ID, user.ID(), orderData.Items)
  if err != nil {
   return err
  }

  if err := orderRepo.Save(txCtx, order); err != nil {
   return err
  }

  return nil
 })
}
```

## Complete Example: Zakat Account Repository

Complete implementation with CRUD operations, business queries, and transaction support:

```go
package infrastructure

import (
 "context"
 "database/sql"
 "errors"
 "fmt"
 "time"

 "myapp/domain"
)

// SQLZakatAccountRepository implements ZakatAccountRepository using database/sql
type SQLZakatAccountRepository struct {
 db *sql.DB
}

func NewSQLZakatAccountRepository(db *sql.DB) *SQLZakatAccountRepository {
 return &SQLZakatAccountRepository{db: db}
}

// ========================================
// CRUD Operations
// ========================================

// Save creates or updates a Zakat account with optimistic locking
func (r *SQLZakatAccountRepository) Save(ctx context.Context, account *domain.ZakatAccount) error {
 tx := r.getTx(ctx)

 var exists bool
 err := tx.QueryRowContext(ctx,
  "SELECT EXISTS(SELECT 1 FROM zakat_accounts WHERE id = $1)",
  account.ID().Value(),
 ).Scan(&exists)

 if err != nil {
  return fmt.Errorf("check account existence: %w", err)
 }

 if exists {
  return r.updateTx(ctx, tx, account)
 }

 return r.insertTx(ctx, tx, account)
}

func (r *SQLZakatAccountRepository) insertTx(ctx context.Context, tx txExecutor, account *domain.ZakatAccount) error {
 query := `
  INSERT INTO zakat_accounts (
   id, holder_id,
   balance_amount, balance_currency,
   nisab_amount, nisab_currency,
   status, haul_start_date, last_payment_date,
   opened_at, opened_by, updated_at, updated_by, closed_at, version
  ) VALUES (
   $1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15
  )
 `

 _, err := tx.ExecContext(ctx, query,
  account.ID().Value(),
  account.HolderID().Value(),
  account.Balance().Amount(),
  account.Balance().Currency(),
  account.NisabThreshold().Amount(),
  account.NisabThreshold().Currency(),
  account.Status().String(),
  account.HaulStartDate(),
  account.LastZakatPaymentDate(),
  account.OpenedAt(),
  account.OpenedBy().Value(),
  account.UpdatedAt(),
  account.UpdatedBy().Value(),
  account.ClosedAt(),
  account.Version(),
 )

 if err != nil {
  return fmt.Errorf("insert zakat account: %w", err)
 }

 return nil
}

func (r *SQLZakatAccountRepository) updateTx(ctx context.Context, tx txExecutor, account *domain.ZakatAccount) error {
 query := `
  UPDATE zakat_accounts
  SET balance_amount = $1, balance_currency = $2,
      nisab_amount = $3, nisab_currency = $4,
      status = $5, haul_start_date = $6, last_payment_date = $7,
      updated_at = $8, updated_by = $9, closed_at = $10, version = $11
  WHERE id = $12 AND version = $13
 `

 result, err := tx.ExecContext(ctx, query,
  account.Balance().Amount(),
  account.Balance().Currency(),
  account.NisabThreshold().Amount(),
  account.NisabThreshold().Currency(),
  account.Status().String(),
  account.HaulStartDate(),
  account.LastZakatPaymentDate(),
  account.UpdatedAt(),
  account.UpdatedBy().Value(),
  account.ClosedAt(),
  account.Version(),        // New version
  account.ID().Value(),
  account.Version()-1,      // Expected old version
 )

 if err != nil {
  return fmt.Errorf("update zakat account: %w", err)
 }

 rowsAffected, err := result.RowsAffected()
 if err != nil {
  return fmt.Errorf("check rows affected: %w", err)
 }

 if rowsAffected == 0 {
  return domain.ErrOptimisticLockConflict
 }

 return nil
}

// FindByID retrieves a Zakat account by ID
func (r *SQLZakatAccountRepository) FindByID(ctx context.Context, id domain.AccountID) (*domain.ZakatAccount, error) {
 query := `
  SELECT id, holder_id,
         balance_amount, balance_currency,
         nisab_amount, nisab_currency,
         status, haul_start_date, last_payment_date,
         opened_at, opened_by, updated_at, updated_by, closed_at, version
  FROM zakat_accounts
  WHERE id = $1 AND closed_at IS NULL
 `

 account, err := r.scanSingleAccount(ctx, query, id.Value())
 if err != nil {
  if errors.Is(err, sql.ErrNoRows) {
   return nil, domain.ErrNotFound
  }
  return nil, err
 }

 return account, nil
}

// FindByIDIncludingClosed retrieves a Zakat account including closed accounts
func (r *SQLZakatAccountRepository) FindByIDIncludingClosed(ctx context.Context, id domain.AccountID) (*domain.ZakatAccount, error) {
 query := `
  SELECT id, holder_id,
         balance_amount, balance_currency,
         nisab_amount, nisab_currency,
         status, haul_start_date, last_payment_date,
         opened_at, opened_by, updated_at, updated_by, closed_at, version
  FROM zakat_accounts
  WHERE id = $1
 `

 account, err := r.scanSingleAccount(ctx, query, id.Value())
 if err != nil {
  if errors.Is(err, sql.ErrNoRows) {
   return nil, domain.ErrNotFound
  }
  return nil, err
 }

 return account, nil
}

// Delete removes a Zakat account (hard delete)
func (r *SQLZakatAccountRepository) Delete(ctx context.Context, id domain.AccountID) error {
 tx := r.getTx(ctx)

 result, err := tx.ExecContext(ctx,
  "DELETE FROM zakat_accounts WHERE id = $1",
  id.Value(),
 )

 if err != nil {
  return fmt.Errorf("delete zakat account: %w", err)
 }

 rowsAffected, err := result.RowsAffected()
 if err != nil {
  return fmt.Errorf("check rows affected: %w", err)
 }

 if rowsAffected == 0 {
  return domain.ErrNotFound
 }

 return nil
}

// ========================================
// Business Queries
// ========================================

// FindByOwner retrieves all accounts for a specific owner
func (r *SQLZakatAccountRepository) FindByOwner(ctx context.Context, ownerID domain.HolderID) ([]*domain.ZakatAccount, error) {
 query := `
  SELECT id, holder_id,
         balance_amount, balance_currency,
         nisab_amount, nisab_currency,
         status, haul_start_date, last_payment_date,
         opened_at, opened_by, updated_at, updated_by, closed_at, version
  FROM zakat_accounts
  WHERE holder_id = $1 AND closed_at IS NULL
  ORDER BY opened_at DESC
 `

 return r.scanMultipleAccounts(ctx, query, ownerID.Value())
}

// FindByStatus retrieves accounts with a specific status
func (r *SQLZakatAccountRepository) FindByStatus(ctx context.Context, status domain.ZakatAccountStatus) ([]*domain.ZakatAccount, error) {
 query := `
  SELECT id, holder_id,
         balance_amount, balance_currency,
         nisab_amount, nisab_currency,
         status, haul_start_date, last_payment_date,
         opened_at, opened_by, updated_at, updated_by, closed_at, version
  FROM zakat_accounts
  WHERE status = $1 AND closed_at IS NULL
  ORDER BY opened_at DESC
 `

 return r.scanMultipleAccounts(ctx, query, status.String())
}

// FindActive retrieves all active accounts
func (r *SQLZakatAccountRepository) FindActive(ctx context.Context) ([]*domain.ZakatAccount, error) {
 query := `
  SELECT id, holder_id,
         balance_amount, balance_currency,
         nisab_amount, nisab_currency,
         status, haul_start_date, last_payment_date,
         opened_at, opened_by, updated_at, updated_by, closed_at, version
  FROM zakat_accounts
  WHERE status = 'ACTIVE' AND closed_at IS NULL
  ORDER BY opened_at DESC
 `

 return r.scanMultipleAccounts(ctx, query)
}

// FindEligibleForDistribution retrieves accounts eligible for Zakat distribution
func (r *SQLZakatAccountRepository) FindEligibleForDistribution(ctx context.Context) ([]*domain.ZakatAccount, error) {
 query := `
  SELECT id, holder_id,
         balance_amount, balance_currency,
         nisab_amount, nisab_currency,
         status, haul_start_date, last_payment_date,
         opened_at, opened_by, updated_at, updated_by, closed_at, version
  FROM zakat_accounts
  WHERE status = 'ACTIVE'
    AND closed_at IS NULL
    AND balance_amount >= nisab_amount
    AND haul_start_date <= NOW() - INTERVAL '354 days'
  ORDER BY haul_start_date ASC
 `

 return r.scanMultipleAccounts(ctx, query)
}

// ========================================
// List Operations
// ========================================

// List retrieves all accounts with pagination
func (r *SQLZakatAccountRepository) List(ctx context.Context, limit, offset int) ([]*domain.ZakatAccount, error) {
 query := `
  SELECT id, holder_id,
         balance_amount, balance_currency,
         nisab_amount, nisab_currency,
         status, haul_start_date, last_payment_date,
         opened_at, opened_by, updated_at, updated_by, closed_at, version
  FROM zakat_accounts
  WHERE closed_at IS NULL
  ORDER BY opened_at DESC
  LIMIT $1 OFFSET $2
 `

 return r.scanMultipleAccounts(ctx, query, limit, offset)
}

// Count returns total number of accounts
func (r *SQLZakatAccountRepository) Count(ctx context.Context) (int, error) {
 var count int
 err := r.db.QueryRowContext(ctx,
  "SELECT COUNT(*) FROM zakat_accounts WHERE closed_at IS NULL",
 ).Scan(&count)

 if err != nil {
  return 0, fmt.Errorf("count zakat accounts: %w", err)
 }

 return count, nil
}

// ========================================
// Transaction Support
// ========================================

// WithTransaction executes a function within a database transaction
func (r *SQLZakatAccountRepository) WithTransaction(ctx context.Context, fn func(context.Context) error) error {
 tx, err := r.db.BeginTx(ctx, nil)
 if err != nil {
  return fmt.Errorf("begin transaction: %w", err)
 }

 ctx = context.WithValue(ctx, txKey{}, tx)

 err = fn(ctx)
 if err != nil {
  if rbErr := tx.Rollback(); rbErr != nil {
   return fmt.Errorf("rollback failed: %v (original error: %w)", rbErr, err)
  }
  return err
 }

 return tx.Commit()
}

// ========================================
// Scanning Helpers
// ========================================

func (r *SQLZakatAccountRepository) scanSingleAccount(ctx context.Context, query string, args ...interface{}) (*domain.ZakatAccount, error) {
 var (
  id                   string
  holderID             string
  balanceAmount        int64
  balanceCurrency      string
  nisabAmount          int64
  nisabCurrency        string
  status               string
  haulStartDate        time.Time
  lastPaymentDate      *time.Time
  openedAt             time.Time
  openedBy             string
  updatedAt            time.Time
  updatedBy            string
  closedAt             *time.Time
  version              int
 )

 err := r.db.QueryRowContext(ctx, query, args...).Scan(
  &id, &holderID,
  &balanceAmount, &balanceCurrency,
  &nisabAmount, &nisabCurrency,
  &status, &haulStartDate, &lastPaymentDate,
  &openedAt, &openedBy, &updatedAt, &updatedBy, &closedAt, &version,
 )

 if err != nil {
  return nil, err
 }

 return r.reconstitute(
  id, holderID,
  balanceAmount, balanceCurrency,
  nisabAmount, nisabCurrency,
  status, haulStartDate, lastPaymentDate,
  openedAt, openedBy, updatedAt, updatedBy, closedAt, version,
 )
}

func (r *SQLZakatAccountRepository) scanMultipleAccounts(ctx context.Context, query string, args ...interface{}) ([]*domain.ZakatAccount, error) {
 rows, err := r.db.QueryContext(ctx, query, args...)
 if err != nil {
  return nil, fmt.Errorf("query zakat accounts: %w", err)
 }
 defer rows.Close()

 var accounts []*domain.ZakatAccount
 for rows.Next() {
  account, err := r.scanAccountRow(rows)
  if err != nil {
   return nil, err
  }
  accounts = append(accounts, account)
 }

 if err := rows.Err(); err != nil {
  return nil, fmt.Errorf("iterate accounts: %w", err)
 }

 return accounts, nil
}

func (r *SQLZakatAccountRepository) scanAccountRow(scanner interface {
 Scan(dest ...interface{}) error
}) (*domain.ZakatAccount, error) {
 var (
  id                   string
  holderID             string
  balanceAmount        int64
  balanceCurrency      string
  nisabAmount          int64
  nisabCurrency        string
  status               string
  haulStartDate        time.Time
  lastPaymentDate      *time.Time
  openedAt             time.Time
  openedBy             string
  updatedAt            time.Time
  updatedBy            string
  closedAt             *time.Time
  version              int
 )

 err := scanner.Scan(
  &id, &holderID,
  &balanceAmount, &balanceCurrency,
  &nisabAmount, &nisabCurrency,
  &status, &haulStartDate, &lastPaymentDate,
  &openedAt, &openedBy, &updatedAt, &updatedBy, &closedAt, &version,
 )

 if err != nil {
  return nil, fmt.Errorf("scan account: %w", err)
 }

 return r.reconstitute(
  id, holderID,
  balanceAmount, balanceCurrency,
  nisabAmount, nisabCurrency,
  status, haulStartDate, lastPaymentDate,
  openedAt, openedBy, updatedAt, updatedBy, closedAt, version,
 )
}

func (r *SQLZakatAccountRepository) reconstitute(
 id, holderID string,
 balanceAmount int64, balanceCurrency string,
 nisabAmount int64, nisabCurrency string,
 status string,
 haulStartDate time.Time, lastPaymentDate *time.Time,
 openedAt time.Time, openedBy string,
 updatedAt time.Time, updatedBy string,
 closedAt *time.Time, version int,
) (*domain.ZakatAccount, error) {
 accountID, err := domain.NewAccountID(id)
 if err != nil {
  return nil, fmt.Errorf("invalid account ID: %w", err)
 }

 holder, err := domain.NewHolderID(holderID)
 if err != nil {
  return nil, fmt.Errorf("invalid holder ID: %w", err)
 }

 balance := domain.MustNewMoney(balanceAmount, balanceCurrency)
 nisab := domain.MustNewMoney(nisabAmount, nisabCurrency)

 openedByID, err := domain.NewUserID(openedBy)
 if err != nil {
  return nil, fmt.Errorf("invalid opened by ID: %w", err)
 }

 updatedByID, err := domain.NewUserID(updatedBy)
 if err != nil {
  return nil, fmt.Errorf("invalid updated by ID: %w", err)
 }

 return domain.ReconstituteZakatAccount(
  accountID,
  holder,
  balance,
  nisab,
  domain.ZakatAccountStatus(status),
  haulStartDate,
  lastPaymentDate,
  openedAt,
  openedByID,
  updatedAt,
  updatedByID,
  closedAt,
  version,
  []domain.ZakatPaymentID{}, // Payments loaded separately if needed
 ), nil
}

// ========================================
// Transaction Helper Types
// ========================================

type txExecutor interface {
 ExecContext(ctx context.Context, query string, args ...interface{}) (sql.Result, error)
 QueryContext(ctx context.Context, query string, args ...interface{}) (*sql.Rows, error)
 QueryRowContext(ctx context.Context, query string, args ...interface{}) *sql.Row
}

type txKey struct{}

func (r *SQLZakatAccountRepository) getTx(ctx context.Context) txExecutor {
 if tx, ok := ctx.Value(txKey{}).(*sql.Tx); ok {
  return tx
 }
 return r.db
}
```

## Before/After Comparison

### Before: No Repository Pattern

```go
// PROBLEM: Domain logic mixed with database code
package service

import "database/sql"

type OrderService struct {
 db *sql.DB
}

func (s *OrderService) CreateOrder(customerID string, items []OrderItem) error {
 // Domain logic mixed with SQL
 tx, err := s.db.Begin()
 if err != nil {
  return err
 }
 defer tx.Rollback()

 // Insert order directly in service
 result, err := tx.Exec(`
  INSERT INTO orders (id, customer_id, status, created_at)
  VALUES (?, ?, ?, ?)
 `, generateID(), customerID, "PENDING", time.Now())

 if err != nil {
  return err
 }

 orderID, _ := result.LastInsertId()

 // Insert order items
 for _, item := range items {
  _, err = tx.Exec(`
   INSERT INTO order_items (order_id, product_id, quantity, price)
   VALUES (?, ?, ?, ?)
  `, orderID, item.ProductID, item.Quantity, item.Price)

  if err != nil {
   return err
  }
 }

 return tx.Commit()
}

// ISSUES:
// - Service knows SQL implementation details
// - Hard to test (requires actual database)
// - Hard to switch databases
// - No domain model (primitive obsession)
// - Business logic scattered across SQL statements
```

### After: Repository Pattern

```go
// SOLUTION: Clean separation of concerns

// Domain layer: Business logic
package domain

type Order struct {
 id       OrderID
 customer CustomerID
 items    []OrderItem
 status   OrderStatus
 // ... business methods
}

func (o *Order) AddItem(item OrderItem) error {
 // Business validation
 return nil
}

// Domain layer: Repository interface
type OrderRepository interface {
 Save(ctx context.Context, order *Order) error
 FindByID(ctx context.Context, id OrderID) (*Order, error)
 FindByCustomer(ctx context.Context, customerID CustomerID) ([]*Order, error)
}

// Application layer: Service uses repository
package service

type OrderService struct {
 orderRepo domain.OrderRepository
}

func (s *OrderService) CreateOrder(ctx context.Context, customerID domain.CustomerID, items []domain.OrderItem) error {
 // Pure domain logic
 order, err := domain.NewOrder(customerID, items)
 if err != nil {
  return err
 }

 // Repository handles persistence
 return s.orderRepo.Save(ctx, order)
}

// Infrastructure layer: SQL implementation
package infrastructure

type SQLOrderRepository struct {
 db *sql.DB
}

func (r *SQLOrderRepository) Save(ctx context.Context, order *domain.Order) error {
 // SQL implementation details hidden here
 return nil
}

// BENEFITS:
// - Service only knows domain models
// - Easy to test with mock repository
// - Easy to switch databases
// - Rich domain model with business logic
// - Clear separation of concerns
```

## Usage Guidelines

### When to Create a Repository

**Create a repository for**:

- Aggregate roots (one repository per aggregate)
- Domain entities that need persistence
- Objects with identity and lifecycle

**Do NOT create repositories for**:

- Value objects (persist as part of aggregate)
- Child entities within aggregates (access through root)
- DTOs or view models

### Repository Method Naming

**Standard CRUD**:

- `Save` - Insert or update
- `FindByID` - Retrieve by ID
- `Delete` - Remove from persistence
- `Count` - Count total records

**Business Queries**:

- `FindByOwner` - Retrieve by owner
- `FindActive` - Retrieve active records
- `FindEligibleFor...` - Domain-specific queries
- `ExistsByEmail` - Check existence

### Error Handling

**Standard repository errors**:

```go
var (
 ErrNotFound               = errors.New("aggregate not found")
 ErrOptimisticLockConflict = errors.New("optimistic locking conflict")
 ErrInvalidID              = errors.New("invalid aggregate ID")
 ErrDuplicateKey           = errors.New("duplicate key violation")
)
```

**Wrap infrastructure errors**:

```go
if err != nil {
 return fmt.Errorf("query user by email: %w", err)
}
```

### Transaction Guidelines

**Use transactions when**:

- Multiple aggregates must be saved atomically
- Aggregate save requires multiple tables
- Business operation spans multiple repositories

**Avoid transactions for**:

- Single aggregate save (optimistic locking sufficient)
- Read-only operations
- Independent operations that can tolerate partial failure

## Testing Repositories

### Mock Repository for Unit Tests

```go
package domain_test

import (
 "context"
 "testing"

 "myapp/domain"
)

// MockUserRepository for testing
type MockUserRepository struct {
 users      map[string]*domain.User
 saveError  error
 findError  error
}

func NewMockUserRepository() *MockUserRepository {
 return &MockUserRepository{
  users: make(map[string]*domain.User),
 }
}

func (m *MockUserRepository) Save(ctx context.Context, user *domain.User) error {
 if m.saveError != nil {
  return m.saveError
 }
 m.users[user.ID().Value()] = user
 return nil
}

func (m *MockUserRepository) FindByID(ctx context.Context, id domain.UserID) (*domain.User, error) {
 if m.findError != nil {
  return nil, m.findError
 }

 user, exists := m.users[id.Value()]
 if !exists {
  return nil, domain.ErrNotFound
 }

 return user, nil
}

func (m *MockUserRepository) SetSaveError(err error) {
 m.saveError = err
}

func (m *MockUserRepository) SetFindError(err error) {
 m.findError = err
}

// Usage in tests
func TestCreateOrder(t *testing.T) {
 // Setup mock repository
 userRepo := NewMockUserRepository()
 orderService := NewOrderService(userRepo)

 // Test with mock
 err := orderService.CreateOrder(ctx, userID, items)
 if err != nil {
  t.Fatalf("CreateOrder failed: %v", err)
 }

 // Verify repository was called correctly
 if len(userRepo.users) != 1 {
  t.Error("expected 1 user saved")
 }
}
```

### Integration Tests with Real Database

```go
package infrastructure_test

import (
 "context"
 "database/sql"
 "testing"

 _ "github.com/lib/pq"
 "myapp/domain"
 "myapp/infrastructure"
)

func setupTestDB(t *testing.T) *sql.DB {
 db, err := sql.Open("postgres", "postgres://localhost/test?sslmode=disable")
 if err != nil {
  t.Fatalf("connect to test database: %v", err)
 }

 // Run migrations
 if err := runMigrations(db); err != nil {
  t.Fatalf("run migrations: %v", err)
 }

 return db
}

func TestSQLUserRepository_Save(t *testing.T) {
 db := setupTestDB(t)
 defer db.Close()

 repo := infrastructure.NewSQLUserRepository(db)
 ctx := context.Background()

 // Create test user
 userID, _ := domain.NewUserID("test-user-001")
 email, _ := domain.NewEmail("test@example.com")
 createdBy, _ := domain.NewUserID("admin-001")

 user, err := domain.NewUser(userID, email, "Test User", createdBy)
 if err != nil {
  t.Fatalf("create user: %v", err)
 }

 // Test Save
 err = repo.Save(ctx, user)
 if err != nil {
  t.Fatalf("save user: %v", err)
 }

 // Test FindByID
 found, err := repo.FindByID(ctx, userID)
 if err != nil {
  t.Fatalf("find user: %v", err)
 }

 // Verify
 if !found.Equals(user) {
  t.Error("found user doesn't match saved user")
 }

 if found.Email().Value() != email.Value() {
  t.Errorf("email = %v, want %v", found.Email().Value(), email.Value())
 }
}

func TestSQLUserRepository_OptimisticLocking(t *testing.T) {
 db := setupTestDB(t)
 defer db.Close()

 repo := infrastructure.NewSQLUserRepository(db)
 ctx := context.Background()

 // Create and save user
 user := createTestUser(t)
 repo.Save(ctx, user)

 // Load user twice
 user1, _ := repo.FindByID(ctx, user.ID())
 user2, _ := repo.FindByID(ctx, user.ID())

 // Modify and save first instance
 user1.UpdateName("Updated Name", user.ID())
 err := repo.Save(ctx, user1)
 if err != nil {
  t.Fatalf("save user1: %v", err)
 }

 // Try to save second instance (should fail)
 user2.UpdateName("Different Name", user.ID())
 err = repo.Save(ctx, user2)
 if err != domain.ErrOptimisticLockConflict {
  t.Errorf("expected optimistic lock conflict, got %v", err)
 }
}
```

## Related Documentation

**Go Language Documentation**:

- [Best Practices](../ex-so-stla-go__best-practices.md) - Go coding standards
- [Idioms](../ex-so-stla-go__idioms.md) - Go-specific patterns
- [Error Handling](../ex-so-stla-go__error-handling.md) - Error handling patterns
- [Interfaces and Composition](../ex-so-stla-go__interfaces-and-composition.md) - Interface design
- [Domain-Driven Design](../ex-so-stla-go__domain-driven-design.md) - DDD in Go

**Template Documentation**:

- [Aggregate Template](./aggregate-template.md) - Aggregate root pattern
- [Entity Template](./entity-template.md) - Entity pattern
- [Value Object Template](./value-object-template.md) - Value object pattern

**DDD Concepts**:

- Bounded Context (module boundaries)
- Ubiquitous Language (domain terminology)
- Aggregate boundaries (consistency boundaries)

**Principles**:

- [Simplicity Over Complexity](../../../../../../governance/principles/general/simplicity-over-complexity.md)
- [Explicit Over Implicit](../../../../../../governance/principles/software-engineering/explicit-over-implicit.md)

---

**Principles Applied**: Simplicity Over Complexity, Explicit Over Implicit
