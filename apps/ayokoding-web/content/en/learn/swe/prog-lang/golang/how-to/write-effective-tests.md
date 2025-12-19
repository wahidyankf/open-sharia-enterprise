---
title: "How to Write Effective Tests"
date: 2025-12-17T13:19:07+07:00
draft: false
weight: 613
description: "Master Go testing with table-driven tests, subtests, mocking, and effective test organization"
tags: ["golang", "testing", "table-driven", "testify", "mocking"]
---

## Problem

Go's testing package provides minimal structure compared to other languages, leading to verbose tests or inconsistent patterns. Without guidance, developers write repetitive test functions or miss important test cases. The challenge is writing idiomatic Go tests that are maintainable and comprehensive.

This guide shows effective testing patterns in Go.

## Testing Package Basics

### Simple Test Function

```go
// user.go
package user

type User struct {
    Name  string
    Email string
    Age   int
}

func (u *User) IsAdult() bool {
    return u.Age >= 18
}

// user_test.go
package user

import "testing"

// ✅ Basic test function
func TestUserIsAdult(t *testing.T) {
    user := &User{Name: "Alice", Email: "alice@example.com", Age: 25}

    if !user.IsAdult() {
        t.Errorf("expected user to be adult, got false")
    }
}

// ✅ Test with multiple assertions
func TestUserValidation(t *testing.T) {
    user := &User{Name: "Bob", Email: "bob@example.com", Age: 16}

    if user.IsAdult() {
        t.Errorf("expected user to not be adult, got true")
    }

    if user.Name != "Bob" {
        t.Errorf("expected name Bob, got %s", user.Name)
    }
}
```

**Test naming**: `Test` + function/type name + specific scenario. Descriptive names document what's being tested.

## Table-Driven Tests

### Idiomatic Go Pattern

```go
// ✅ Table-driven test - idiomatic Go
func TestIsValidEmail(t *testing.T) {
    tests := []struct {
        name  string
        email string
        want  bool
    }{
        {name: "valid email", email: "alice@example.com", want: true},
        {name: "missing @", email: "invalid.email", want: false},
        {name: "missing domain", email: "user@", want: false},
        {name: "missing user", email: "@example.com", want: false},
        {name: "empty string", email: "", want: false},
        {name: "multiple @", email: "user@@example.com", want: false},
    }

    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            got := IsValidEmail(tt.email)
            if got != tt.want {
                t.Errorf("IsValidEmail(%q) = %v, want %v", tt.email, got, tt.want)
            }
        })
    }
}

// ❌ Repetitive individual tests
func TestValidEmail(t *testing.T) {
    if !IsValidEmail("alice@example.com") {
        t.Error("expected valid email to pass")
    }
}

func TestInvalidEmailMissingAt(t *testing.T) {
    if IsValidEmail("invalid.email") {
        t.Error("expected invalid email to fail")
    }
}

func TestInvalidEmailMissingDomain(t *testing.T) {
    if IsValidEmail("user@") {
        t.Error("expected invalid email to fail")
    }
}
// ... many more repetitive functions
```

**Why table-driven tests**: Test multiple scenarios with minimal code. Add new test cases by adding table entries. One test function covers many cases. Failures show which specific case failed.

### Complex Table Tests

```go
func TestCalculateDiscount(t *testing.T) {
    tests := []struct {
        name           string
        total          float64
        customerType   string
        wantDiscount   float64
        wantErr        bool
        wantErrMessage string
    }{
        {
            name:         "premium customer over 100",
            total:        150.00,
            customerType: "premium",
            wantDiscount: 15.00,
            wantErr:      false,
        },
        {
            name:         "regular customer",
            total:        100.00,
            customerType: "regular",
            wantDiscount: 0.00,
            wantErr:      false,
        },
        {
            name:           "negative total",
            total:          -50.00,
            customerType:   "premium",
            wantDiscount:   0.00,
            wantErr:        true,
            wantErrMessage: "total cannot be negative",
        },
        {
            name:           "unknown customer type",
            total:          100.00,
            customerType:   "unknown",
            wantDiscount:   0.00,
            wantErr:        true,
            wantErrMessage: "invalid customer type",
        },
    }

    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            gotDiscount, err := CalculateDiscount(tt.total, tt.customerType)

            if tt.wantErr {
                if err == nil {
                    t.Errorf("expected error, got nil")
                    return
                }
                if err.Error() != tt.wantErrMessage {
                    t.Errorf("error message = %q, want %q", err.Error(), tt.wantErrMessage)
                }
                return
            }

            if err != nil {
                t.Errorf("unexpected error: %v", err)
                return
            }

            if gotDiscount != tt.wantDiscount {
                t.Errorf("discount = %.2f, want %.2f", gotDiscount, tt.wantDiscount)
            }
        })
    }
}
```

## Subtests with t.Run

### Organizing Related Tests

```go
func TestUserOperations(t *testing.T) {
    // ✅ Group related tests with subtests
    t.Run("Create", func(t *testing.T) {
        user := CreateUser("alice@example.com", "password123")
        if user == nil {
            t.Fatal("expected user, got nil")
        }
        if user.Email != "alice@example.com" {
            t.Errorf("email = %s, want alice@example.com", user.Email)
        }
    })

    t.Run("Validate", func(t *testing.T) {
        t.Run("valid user", func(t *testing.T) {
            user := &User{Email: "alice@example.com", Age: 25}
            if err := user.Validate(); err != nil {
                t.Errorf("unexpected error: %v", err)
            }
        })

        t.Run("invalid email", func(t *testing.T) {
            user := &User{Email: "invalid", Age: 25}
            if err := user.Validate(); err == nil {
                t.Error("expected validation error, got nil")
            }
        })

        t.Run("under age", func(t *testing.T) {
            user := &User{Email: "alice@example.com", Age: 15}
            if err := user.Validate(); err == nil {
                t.Error("expected validation error, got nil")
            }
        })
    })

    t.Run("Update", func(t *testing.T) {
        user := &User{Email: "alice@example.com", Age: 25}
        user.Update("newemail@example.com", 26)

        if user.Email != "newemail@example.com" {
            t.Errorf("email = %s, want newemail@example.com", user.Email)
        }
    })
}
```

**Running specific subtests:**

```bash
# Run all tests
go test

# Run specific test function
go test -run TestUserOperations

# Run specific subtest
go test -run TestUserOperations/Create

# Run nested subtest
go test -run TestUserOperations/Validate/valid_user
```

## Test Helpers and Setup

### Setup and Teardown

```go
// ✅ Test helper for common setup
func setupTestUser(t *testing.T) *User {
    t.Helper() // Marks function as test helper

    user := &User{
        Name:  "Test User",
        Email: "test@example.com",
        Age:   25,
    }

    return user
}

// ✅ Teardown with cleanup
func setupDatabase(t *testing.T) *sql.DB {
    t.Helper()

    db, err := sql.Open("sqlite3", ":memory:")
    if err != nil {
        t.Fatalf("failed to open database: %v", err)
    }

    // Register cleanup function
    t.Cleanup(func() {
        db.Close()
    })

    return db
}

// Usage
func TestWithDatabase(t *testing.T) {
    db := setupDatabase(t) // Automatically closed after test

    // Use database...
    if err := createTables(db); err != nil {
        t.Fatalf("failed to create tables: %v", err)
    }

    // Test operations...
}
```

### Table Test Helpers

```go
// ✅ Helper for comparing slices
func assertEqual(t *testing.T, got, want []string) {
    t.Helper()

    if len(got) != len(want) {
        t.Errorf("length mismatch: got %d, want %d", len(got), len(want))
        return
    }

    for i := range got {
        if got[i] != want[i] {
            t.Errorf("index %d: got %q, want %q", i, got[i], want[i])
        }
    }
}

// Usage
func TestProcessItems(t *testing.T) {
    got := ProcessItems([]string{"a", "b", "c"})
    want := []string{"A", "B", "C"}

    assertEqual(t, got, want)
}
```

## Testify for Better Assertions

### Using testify/assert

```go
import (
    "testing"
    "github.com/stretchr/testify/assert"
)

// ✅ Cleaner assertions with testify
func TestWithTestify(t *testing.T) {
    user := &User{Name: "Alice", Age: 25}

    assert.NotNil(t, user)
    assert.Equal(t, "Alice", user.Name)
    assert.True(t, user.IsAdult())
    assert.Greater(t, user.Age, 18)
}

// ✅ Table test with testify
func TestCalculateTax(t *testing.T) {
    tests := []struct {
        name   string
        amount float64
        rate   float64
        want   float64
    }{
        {"10% tax", 100.0, 0.10, 10.0},
        {"20% tax", 50.0, 0.20, 10.0},
        {"no tax", 100.0, 0.0, 0.0},
    }

    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            got := CalculateTax(tt.amount, tt.rate)
            assert.Equal(t, tt.want, got)
        })
    }
}

// ✅ Assert collections
func TestFilterItems(t *testing.T) {
    items := []string{"apple", "banana", "cherry"}
    filtered := FilterByPrefix(items, "b")

    assert.Len(t, filtered, 1)
    assert.Contains(t, filtered, "banana")
    assert.NotContains(t, filtered, "apple")
}
```

### testify/require for Fatal Assertions

```go
import (
    "testing"
    "github.com/stretchr/testify/require"
)

// ✅ require stops test on failure
func TestDatabaseOperations(t *testing.T) {
    db, err := OpenDatabase()
    require.NoError(t, err) // Stops if error (can't continue without DB)

    user, err := db.FindUser("123")
    require.NoError(t, err)
    require.NotNil(t, user) // Stops if nil (can't check fields)

    // Safe to access user fields
    assert.Equal(t, "Alice", user.Name)
}

// ❌ Without require - panic on nil
func TestWithoutRequire(t *testing.T) {
    db, err := OpenDatabase()
    assert.NoError(t, err)

    user, err := db.FindUser("123")
    assert.NoError(t, err)
    assert.NotNil(t, user)

    // Panics if user is nil!
    assert.Equal(t, "Alice", user.Name)
}
```

## Mocking Interfaces

### Manual Mocks

```go
// Interface to mock
type UserRepository interface {
    FindByID(id string) (*User, error)
    Save(user *User) error
}

// ✅ Manual mock implementation
type MockUserRepository struct {
    FindByIDFunc func(id string) (*User, error)
    SaveFunc     func(user *User) error

    FindByIDCalls []string // Track calls
    SaveCalls     []*User
}

func (m *MockUserRepository) FindByID(id string) (*User, error) {
    m.FindByIDCalls = append(m.FindByIDCalls, id)

    if m.FindByIDFunc != nil {
        return m.FindByIDFunc(id)
    }

    return nil, nil
}

func (m *MockUserRepository) Save(user *User) error {
    m.SaveCalls = append(m.SaveCalls, user)

    if m.SaveFunc != nil {
        return m.SaveFunc(user)
    }

    return nil
}

// Usage in tests
func TestUserService(t *testing.T) {
    mockRepo := &MockUserRepository{
        FindByIDFunc: func(id string) (*User, error) {
            if id == "123" {
                return &User{ID: "123", Name: "Alice"}, nil
            }
            return nil, errors.New("not found")
        },
    }

    service := NewUserService(mockRepo)
    user, err := service.GetUser("123")

    assert.NoError(t, err)
    assert.Equal(t, "Alice", user.Name)
    assert.Equal(t, []string{"123"}, mockRepo.FindByIDCalls)
}
```

### testify/mock for Advanced Mocking

```go
import (
    "testing"
    "github.com/stretchr/testify/mock"
)

// ✅ Mock with testify/mock
type MockRepository struct {
    mock.Mock
}

func (m *MockRepository) FindByID(id string) (*User, error) {
    args := m.Called(id)
    if args.Get(0) == nil {
        return nil, args.Error(1)
    }
    return args.Get(0).(*User), args.Error(1)
}

func (m *MockRepository) Save(user *User) error {
    args := m.Called(user)
    return args.Error(0)
}

// Usage
func TestWithMock(t *testing.T) {
    mockRepo := new(MockRepository)

    // Set expectations
    expectedUser := &User{ID: "123", Name: "Alice"}
    mockRepo.On("FindByID", "123").Return(expectedUser, nil)
    mockRepo.On("Save", mock.Anything).Return(nil)

    service := NewUserService(mockRepo)

    // Run test
    user, err := service.GetUser("123")
    assert.NoError(t, err)
    assert.Equal(t, "Alice", user.Name)

    service.UpdateUser(user)

    // Verify expectations
    mockRepo.AssertExpectations(t)
    mockRepo.AssertCalled(t, "FindByID", "123")
    mockRepo.AssertCalled(t, "Save", expectedUser)
}
```

## Benchmark Tests

### Writing Benchmarks

```go
// ✅ Benchmark function
func BenchmarkStringConcat(b *testing.B) {
    for i := 0; i < b.N; i++ {
        _ = ConcatenateStrings("hello", "world")
    }
}

// ✅ Table-driven benchmarks
func BenchmarkStringOperations(b *testing.B) {
    tests := []struct {
        name string
        fn   func() string
    }{
        {"concat", func() string { return "hello" + "world" }},
        {"sprintf", func() string { return fmt.Sprintf("%s%s", "hello", "world") }},
        {"builder", func() string {
            var builder strings.Builder
            builder.WriteString("hello")
            builder.WriteString("world")
            return builder.String()
        }},
    }

    for _, tt := range tests {
        b.Run(tt.name, func(b *testing.B) {
            for i := 0; i < b.N; i++ {
                _ = tt.fn()
            }
        })
    }
}
```

**Running benchmarks:**

```bash
# Run all benchmarks
go test -bench=.

# Run specific benchmark
go test -bench=BenchmarkStringConcat

# With memory stats
go test -bench=. -benchmem

# Compare benchmarks
go test -bench=. -benchmem > old.txt
# Make changes
go test -bench=. -benchmem > new.txt
benchcmp old.txt new.txt
```

## Test Organization

### File Structure

```
user/
├── user.go              # Implementation
├── user_test.go         # Tests in same package
├── export_test.go       # Export internals for testing
└── integration_test.go  # Integration tests
```

### Package Test vs External Test

```go
// user_test.go - same package (can test unexported)
package user

import "testing"

func TestUnexportedFunction(t *testing.T) {
    result := internalHelper("test") // Can access unexported
    if result != "expected" {
        t.Errorf("got %s", result)
    }
}

// user_external_test.go - external package (only exported)
package user_test

import (
    "testing"
    "myapp/user"
)

func TestPublicAPI(t *testing.T) {
    u := user.New("alice@example.com")
    // Can only access exported fields and methods
}
```

## Summary

Effective Go testing centers on table-driven tests and subtests. Table-driven tests use slices of test cases with inputs and expected outputs, enabling comprehensive coverage with minimal code. Adding test cases means adding table entries, not new functions. Subtests with t.Run organize related tests and enable selective test execution.

The testing package provides minimal structure intentionally. Test functions start with Test, take \*testing.T parameter, and use t.Error for non-fatal failures or t.Fatal for fatal failures. t.Helper marks functions as test helpers so failure messages show correct line numbers.

Setup and teardown use helper functions and t.Cleanup. Helper functions return initialized test data, t.Cleanup registers functions to run after tests complete. This pattern ensures proper cleanup even when tests fail, preventing resource leaks and test interference.

Testify provides convenient assertion functions reducing boilerplate. assert functions continue test on failure, require functions stop immediately. Use require when subsequent code depends on the assertion passing - checking database connection before running queries, verifying non-nil before accessing fields.

Mocking in Go uses interfaces. Define interfaces for dependencies, create mock implementations for testing. Manual mocks track calls and allow configurable return values. testify/mock provides assertion helpers for verifying mock interactions. Test behavior not implementation by mocking only external dependencies.

Benchmarks measure performance with b.N iterations. Go's benchmark framework automatically determines appropriate iteration count. Table-driven benchmarks compare different implementations. Run with -benchmem to see memory allocation statistics, essential for optimization work.

Test organization follows package structure with \_test.go suffix. Same-package tests access unexported functions, external package tests verify public API. Use external tests to catch issues where package exports are insufficient for real usage.

Idiomatic Go testing favors simplicity over frameworks. Table-driven tests handle multiple scenarios, subtests organize related cases, helper functions reduce duplication, and interfaces enable testability. These patterns produce maintainable test suites that grow with codebases.

## Related Content

- [Go Best Practices and Idioms](/en/learn/swe/prog-lang/golang/explanation/best-practices)
- [How to Handle Errors Effectively](/en/learn/swe/prog-lang/golang/how-to/handle-errors-effectively)
- [How to Design Interfaces Properly](/en/learn/swe/prog-lang/golang/how-to/design-interfaces-properly)
