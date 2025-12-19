---
title: Testing Strategies in Go
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 631
description: Write effective tests using Go's testing package, table-driven tests, and mocking
tags: ["golang", "go", "testing", "unit-tests", "integration-tests"]
---

## Basic Unit Test

Go's built-in `testing` package provides everything needed for unit tests.

```go
// math.go
package math

func Add(a, b int) int {
    return a + b
}

func Divide(a, b float64) (float64, error) {
    if b == 0 {
        return 0, errors.New("division by zero")
    }
    return a / b, nil
}
```

```go
// math_test.go
package math

import "testing"

func TestAdd(t *testing.T) {
    result := Add(2, 3)
    expected := 5

    if result != expected {
        t.Errorf("Add(2, 3) = %d; want %d", result, expected)
    }
}

func TestDivide(t *testing.T) {
    result, err := Divide(10, 2)
    if err != nil {
        t.Fatalf("unexpected error: %v", err)
    }

    expected := 5.0
    if result != expected {
        t.Errorf("Divide(10, 2) = %f; want %f", result, expected)
    }
}

func TestDivideByZero(t *testing.T) {
    _, err := Divide(10, 0)
    if err == nil {
        t.Error("expected error for division by zero")
    }
}
```

## Table-Driven Tests

Test multiple scenarios with a single test function.

```go
package math

import "testing"

func TestAddTable(t *testing.T) {
    tests := []struct {
        name     string
        a        int
        b        int
        expected int
    }{
        {"positive numbers", 2, 3, 5},
        {"negative numbers", -2, -3, -5},
        {"mixed signs", -2, 3, 1},
        {"zeros", 0, 0, 0},
    }

    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            result := Add(tt.a, tt.b)
            if result != tt.expected {
                t.Errorf("Add(%d, %d) = %d; want %d",
                    tt.a, tt.b, result, tt.expected)
            }
        })
    }
}
```

## Subtests with t.Run

Organize related tests using subtests.

```go
package user

import "testing"

func TestUserValidation(t *testing.T) {
    t.Run("valid user", func(t *testing.T) {
        user := User{Name: "Alice", Email: "alice@example.com"}
        if err := user.Validate(); err != nil {
            t.Errorf("unexpected error: %v", err)
        }
    })

    t.Run("empty name", func(t *testing.T) {
        user := User{Name: "", Email: "alice@example.com"}
        if err := user.Validate(); err == nil {
            t.Error("expected validation error for empty name")
        }
    })

    t.Run("invalid email", func(t *testing.T) {
        user := User{Name: "Alice", Email: "invalid"}
        if err := user.Validate(); err == nil {
            t.Error("expected validation error for invalid email")
        }
    })
}
```

## Test Helpers

Create helper functions to reduce test boilerplate.

```go
package database

import "testing"

func assertEqual(t *testing.T, got, want interface{}) {
    t.Helper() // Mark as helper
    if got != want {
        t.Errorf("got %v; want %v", got, want)
    }
}

func assertNoError(t *testing.T, err error) {
    t.Helper()
    if err != nil {
        t.Fatalf("unexpected error: %v", err)
    }
}

func TestDatabase(t *testing.T) {
    db := &DB{}
    err := db.Connect()
    assertNoError(t, err)

    count := db.Count()
    assertEqual(t, count, 0)
}
```

## Mocking with Interfaces

Use interfaces to create testable code.

```go
// service.go
package service

type Database interface {
    GetUser(id string) (*User, error)
}

type UserService struct {
    db Database
}

func (s *UserService) FindUser(id string) (*User, error) {
    return s.db.GetUser(id)
}
```

```go
// service_test.go
package service

import (
    "errors"
    "testing"
)

// Mock implementation
type MockDatabase struct {
    users map[string]*User
    err   error
}

func (m *MockDatabase) GetUser(id string) (*User, error) {
    if m.err != nil {
        return nil, m.err
    }
    user, ok := m.users[id]
    if !ok {
        return nil, errors.New("user not found")
    }
    return user, nil
}

func TestUserService_FindUser(t *testing.T) {
    mockDB := &MockDatabase{
        users: map[string]*User{
            "1": {ID: "1", Name: "Alice"},
        },
    }

    service := &UserService{db: mockDB}

    t.Run("existing user", func(t *testing.T) {
        user, err := service.FindUser("1")
        if err != nil {
            t.Fatalf("unexpected error: %v", err)
        }
        if user.Name != "Alice" {
            t.Errorf("got %s; want Alice", user.Name)
        }
    })

    t.Run("non-existing user", func(t *testing.T) {
        _, err := service.FindUser("999")
        if err == nil {
            t.Error("expected error for non-existing user")
        }
    })
}
```

## HTTP Handler Testing

Test HTTP handlers using `httptest` package.

```go
// handler.go
package api

import (
    "encoding/json"
    "net/http"
)

func HelloHandler(w http.ResponseWriter, r *http.Request) {
    w.Header().Set("Content-Type", "application/json")
    json.NewEncoder(w).Encode(map[string]string{
        "message": "Hello, World!",
    })
}
```

```go
// handler_test.go
package api

import (
    "encoding/json"
    "net/http"
    "net/http/httptest"
    "testing"
)

func TestHelloHandler(t *testing.T) {
    req := httptest.NewRequest(http.MethodGet, "/hello", nil)
    w := httptest.NewRecorder()

    HelloHandler(w, req)

    res := w.Result()
    defer res.Body.Close()

    if res.StatusCode != http.StatusOK {
        t.Errorf("status = %d; want %d", res.StatusCode, http.StatusOK)
    }

    var response map[string]string
    if err := json.NewDecoder(res.Body).Decode(&response); err != nil {
        t.Fatalf("failed to decode response: %v", err)
    }

    expected := "Hello, World!"
    if response["message"] != expected {
        t.Errorf("message = %s; want %s", response["message"], expected)
    }
}
```

## Benchmarking

Measure performance using benchmark tests.

```go
package math

import "testing"

func BenchmarkAdd(b *testing.B) {
    for i := 0; i < b.N; i++ {
        Add(2, 3)
    }
}

func BenchmarkAddParallel(b *testing.B) {
    b.RunParallel(func(pb *testing.PB) {
        for pb.Next() {
            Add(2, 3)
        }
    })
}

// Run: go test -bench=. -benchmem
```

## Coverage Analysis

Generate and view test coverage.

```bash
# Run tests with coverage
go test -coverprofile=coverage.out

# View coverage report
go tool cover -html=coverage.out

# Show coverage percentage
go test -cover
```

## Integration Tests with Build Tags

Separate integration tests from unit tests.

```go
// +build integration

package database

import (
    "testing"
)

func TestDatabaseIntegration(t *testing.T) {
    if testing.Short() {
        t.Skip("skipping integration test")
    }

    // Integration test code
    db := ConnectToRealDatabase()
    defer db.Close()

    // Test against real database
}
```

```bash
# Run only unit tests
go test -short

# Run integration tests
go test -tags=integration

# Run all tests
go test -tags=integration ./...
```

## Test Fixtures and Setup

Use test fixtures for complex setup.

```go
package api

import (
    "net/http/httptest"
    "testing"
)

type testFixture struct {
    server *httptest.Server
    client *http.Client
}

func setupTest(t *testing.T) *testFixture {
    t.Helper()

    server := httptest.NewServer(createRouter())
    client := server.Client()

    t.Cleanup(func() {
        server.Close()
    })

    return &testFixture{
        server: server,
        client: client,
    }
}

func TestAPI(t *testing.T) {
    f := setupTest(t)

    t.Run("GET /users", func(t *testing.T) {
        resp, err := f.client.Get(f.server.URL + "/users")
        if err != nil {
            t.Fatalf("request failed: %v", err)
        }
        defer resp.Body.Close()

        if resp.StatusCode != 200 {
            t.Errorf("status = %d; want 200", resp.StatusCode)
        }
    })
}
```

## Parallel Tests

Run tests concurrently to speed up execution.

```go
package math

import (
    "testing"
)

func TestAddParallel(t *testing.T) {
    tests := []struct {
        name     string
        a, b     int
        expected int
    }{
        {"test1", 1, 2, 3},
        {"test2", 5, 10, 15},
        {"test3", -1, 1, 0},
    }

    for _, tt := range tests {
        tt := tt // Capture range variable
        t.Run(tt.name, func(t *testing.T) {
            t.Parallel() // Run in parallel

            result := Add(tt.a, tt.b)
            if result != tt.expected {
                t.Errorf("Add(%d, %d) = %d; want %d",
                    tt.a, tt.b, result, tt.expected)
            }
        })
    }
}
```

## Related Resources

**Learn more**:

- [Cheat Sheet](/en/learn/swe/prog-lang/golang/reference/cheat-sheet) - Testing syntax
- [Best Practices](/en/learn/swe/prog-lang/golang/explanation/best-practices) - Testing guidelines
- [Resources](/en/learn/swe/prog-lang/golang/reference/resources) - Testing tools
