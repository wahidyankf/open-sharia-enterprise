---
title: "How to Write Table-Driven Tests"
date: 2025-12-21T00:00:00+07:00
draft: false
weight: 1000023
description: "Practical techniques for implementing table-driven tests in Go using subtests, test coverage, and golden files"
---

## Problem

Writing separate test functions for each input case leads to repetitive code and poor maintainability.

## Solution

### 1. Basic Table-Driven Test

```go
func TestAdd(t *testing.T) {
    tests := []struct {
        name     string
        a, b     int
        expected int
    }{
        {"positive numbers", 2, 3, 5},
        {"negative numbers", -2, -3, -5},
        {"mixed", 2, -3, -1},
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

### 2. Complex Test Cases

```go
func TestUserValidation(t *testing.T) {
    tests := []struct {
        name    string
        user    User
        wantErr bool
        errMsg  string
    }{
        {
            name:    "valid user",
            user:    User{Username: "john", Email: "john@example.com", Age: 25},
            wantErr: false,
        },
        {
            name:    "empty username",
            user:    User{Username: "", Email: "john@example.com", Age: 25},
            wantErr: true,
            errMsg:  "username required",
        },
        {
            name:    "invalid email",
            user:    User{Username: "john", Email: "invalid", Age: 25},
            wantErr: true,
            errMsg:  "invalid email",
        },
    }

    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            err := ValidateUser(tt.user)

            if tt.wantErr {
                if err == nil {
                    t.Error("expected error but got nil")
                } else if !strings.Contains(err.Error(), tt.errMsg) {
                    t.Errorf("error = %v; want %v", err, tt.errMsg)
                }
            } else {
                if err != nil {
                    t.Errorf("unexpected error: %v", err)
                }
            }
        })
    }
}
```

### 3. Test Helpers

```go
func assertEqual(t *testing.T, got, want interface{}) {
    t.Helper()
    if got != want {
        t.Errorf("got %v; want %v", got, want)
    }
}

func TestWithHelpers(t *testing.T) {
    tests := []struct {
        name string
        input int
        want int
    }{
        {"double 2", 2, 4},
        {"double 5", 5, 10},
    }

    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            got := Double(tt.input)
            assertEqual(t, got, tt.want)
        })
    }
}
```

## How It Works

### Test Table Structure

Table-driven tests use a slice of structs defining test cases:

```go
tests := []struct {
    name     string    // Test case name
    input    InputType // Input data
    expected OutputType // Expected result
    wantErr  bool      // Should error occur
}{
    {"case 1", input1, output1, false},
    {"case 2", input2, output2, true},
}
```

Each struct field defines:

- **name**: Descriptive test case identifier (appears in output)
- **input**: Function arguments or test data
- **expected**: Anticipated function output
- **wantErr**: Whether error expected (for error testing)

### Subtest Execution

`t.Run()` creates isolated subtests:

1. **Iteration**: Loop through test table
2. **Subtest Creation**: `t.Run(name, func)` spawns independent test
3. **Isolation**: Each subtest runs independently (one failure doesn't stop others)
4. **Reporting**: Each subtest reports separately in output
5. **Parallelization**: Subtests can run in parallel with `t.Parallel()`

**Output format**:

```
=== RUN   TestAdd
=== RUN   TestAdd/positive_numbers
=== RUN   TestAdd/negative_numbers
```

### t.Helper() Mechanism

`t.Helper()` marks helper functions to improve error reporting:

```go
func assertEqual(t *testing.T, got, want int) {
    t.Helper()  // Marks this as helper
    if got != want {
        t.Errorf("got %d; want %d", got, want)
    }
}
```

**Without t.Helper()**:

```
helper.go:3: got 5; want 10  // Points to helper function
```

**With t.Helper()**:

```
test.go:15: got 5; want 10   // Points to actual test line
```

### Test Coverage Tracking

Go's test coverage tracks executed code:

```bash
go test -cover              # Show coverage percentage
go test -coverprofile=c.out # Generate coverage data
go tool cover -html=c.out   # View coverage in browser
```

Table-driven tests naturally improve coverage by testing multiple scenarios in single function.

### Parallel Test Execution

Mark subtests for parallel execution:

```go
for _, tt := range tests {
    tt := tt  // Capture range variable
    t.Run(tt.name, func(t *testing.T) {
        t.Parallel()  // Run this subtest in parallel
        // Test logic...
    })
}
```

**Benefits**: Faster test execution
**Requirement**: Must capture loop variable (`tt := tt`) to avoid race conditions

## Variations

### 1. Anonymous Struct Test Tables

Inline test table definition:

```go
func TestParse(t *testing.T) {
    for _, tt := range []struct {
        input string
        want  int
    }{
        {"123", 123},
        {"456", 456},
        {"0", 0},
    } {
        t.Run(tt.input, func(t *testing.T) {
            got := Parse(tt.input)
            if got != tt.want {
                t.Errorf("Parse(%q) = %d; want %d", tt.input, got, tt.want)
            }
        })
    }
}
```

**Trade-offs**: More concise but less readable for complex cases.

### 2. Golden Files Testing

Compare output against saved "golden" files:

```go
func TestRender(t *testing.T) {
    tests := []struct {
        name     string
        input    Template
        goldFile string
    }{
        {"simple", simpleTemplate, "testdata/simple.golden"},
        {"complex", complexTemplate, "testdata/complex.golden"},
    }

    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            got := Render(tt.input)

            if *update {
                os.WriteFile(tt.goldFile, []byte(got), 0644)
            }

            want, _ := os.ReadFile(tt.goldFile)
            if got != string(want) {
                t.Errorf("mismatch:\ngot:\n%s\nwant:\n%s", got, want)
            }
        })
    }
}

// Run with: go test -update to regenerate golden files
```

**Trade-offs**: Great for large outputs but requires file management.

### 3. Benchmark Table Tests

Apply table-driven pattern to benchmarks:

```go
func BenchmarkSort(b *testing.B) {
    tests := []struct {
        name string
        size int
    }{
        {"small", 100},
        {"medium", 1000},
        {"large", 10000},
    }

    for _, tt := range tests {
        b.Run(tt.name, func(b *testing.B) {
            data := makeData(tt.size)
            b.ResetTimer()

            for i := 0; i < b.N; i++ {
                Sort(data)
            }
        })
    }
}
```

**Trade-offs**: Consistent benchmarking across scenarios but adds complexity.

### 4. Setup and Teardown per Case

Include setup/teardown in test table:

```go
func TestDatabase(t *testing.T) {
    tests := []struct {
        name    string
        setup   func(*sql.DB)
        test    func(*sql.DB) error
        cleanup func(*sql.DB)
    }{
        {
            name: "insert",
            setup: func(db *sql.DB) {
                db.Exec("CREATE TABLE users (...)")
            },
            test: func(db *sql.DB) error {
                return insertUser(db, "john")
            },
            cleanup: func(db *sql.DB) {
                db.Exec("DROP TABLE users")
            },
        },
    }

    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            db := setupDB()
            defer tt.cleanup(db)

            tt.setup(db)
            if err := tt.test(db); err != nil {
                t.Errorf("test failed: %v", err)
            }
        })
    }
}
```

**Trade-offs**: Flexible per-case setup but verbose.

### 5. Testify Library Integration

Use testify for assertions:

```go
import "github.com/stretchr/testify/assert"

func TestWithTestify(t *testing.T) {
    tests := []struct {
        name string
        a, b int
        want int
    }{
        {"add positive", 2, 3, 5},
        {"add negative", -2, -3, -5},
    }

    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            got := Add(tt.a, tt.b)
            assert.Equal(t, tt.want, got)
        })
    }
}
```

**Trade-offs**: Cleaner assertions but external dependency.

## Common Pitfalls

### 1. Not Using t.Run()

**Problem**: Test failures don't indicate which case failed:

```go
// Bad: All cases in one test
func TestAdd(t *testing.T) {
    tests := []struct{a, b, want int}{
        {2, 3, 5},
        {-2, -3, -5},
    }

    for _, tt := range tests {
        got := Add(tt.a, tt.b)
        if got != tt.want {
            t.Errorf("failed")  // Which case failed?
        }
    }
}
```

**Solution**: Use t.Run() with descriptive names:

```go
// Good: Each case identified
func TestAdd(t *testing.T) {
    tests := []struct{
        name string
        a, b, want int
    }{
        {"positive", 2, 3, 5},
        {"negative", -2, -3, -5},
    }

    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            got := Add(tt.a, tt.b)
            if got != tt.want {
                t.Errorf("Add(%d, %d) = %d; want %d", tt.a, tt.b, got, tt.want)
            }
        })
    }
}
```

### 2. Forgetting Variable Capture in Parallel Tests

**Problem**: Loop variable reused causes race condition:

```go
// Bad: Race condition
for _, tt := range tests {
    t.Run(tt.name, func(t *testing.T) {
        t.Parallel()
        result := Compute(tt.input)  // tt changes during iteration!
    })
}
```

**Solution**: Capture loop variable:

```go
// Good: Capture variable
for _, tt := range tests {
    tt := tt  // Capture!
    t.Run(tt.name, func(t *testing.T) {
        t.Parallel()
        result := Compute(tt.input)  // Safe - tt is local copy
    })
}
```

### 3. Missing t.Helper() in Test Helpers

**Problem**: Error messages point to helper function instead of test:

```go
// Bad: No t.Helper()
func checkEqual(t *testing.T, got, want int) {
    if got != want {
        t.Errorf("got %d; want %d", got, want)
        // Error shows this line, not calling test
    }
}
```

**Solution**: Always use t.Helper() in helpers:

```go
// Good: t.Helper() marks as helper
func checkEqual(t *testing.T, got, want int) {
    t.Helper()  // Error points to calling test
    if got != want {
        t.Errorf("got %d; want %d", got, want)
    }
}
```

### 4. Overly Complex Test Tables

**Problem**: Test table becomes harder to read than separate tests:

```go
// Bad: Too complex
tests := []struct{
    name string
    setupDB func() *sql.DB
    setupCache func() *Cache
    setupHTTP func() *http.Server
    input ComplexInput
    mockResponses map[string]string
    validateOutput func(interface{}) bool
    wantErr bool
    errContains string
}{
    // Each case is 20+ lines...
}
```

**Solution**: Keep test tables simple, use separate tests for complex scenarios:

```go
// Good: Simple table for simple cases
func TestSimpleCases(t *testing.T) {
    tests := []struct{name string; input int; want int}{
        {"case1", 1, 2},
        {"case2", 2, 4},
    }
    // ...
}

// Good: Separate test for complex scenario
func TestComplexScenario(t *testing.T) {
    db := setupDB()
    cache := setupCache()
    // Complex test logic without table
}
```

### 5. Not Testing Error Messages

**Problem**: Only checking if error occurred, not what error:

```go
// Bad: Any error passes
if tt.wantErr && err != nil {
    continue  // Got an error, good enough?
}
```

**Solution**: Verify error content:

```go
// Good: Check error message
if tt.wantErr {
    if err == nil {
        t.Error("expected error, got nil")
    } else if !strings.Contains(err.Error(), tt.errMsg) {
        t.Errorf("error = %q; want substring %q", err.Error(), tt.errMsg)
    }
} else if err != nil {
    t.Errorf("unexpected error: %v", err)
}
```

### 6. Mutable Test Data

**Problem**: Tests modify shared test data causing interdependencies:

```go
// Bad: Tests modify shared slice
var testData = []int{1, 2, 3}

func TestModify(t *testing.T) {
    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            Modify(testData)  // Modifies shared data!
        })
    }
}
```

**Solution**: Create fresh data per test:

```go
// Good: Fresh data each test
func TestModify(t *testing.T) {
    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            data := []int{1, 2, 3}  // Fresh copy
            Modify(data)
        })
    }
}
```

## Related Patterns

**Related Tutorial**: See [Intermediate Tutorial - Testing](/en/learn/software-engineering/programming-language/golang/tutorials/intermediate#testing) for testing fundamentals.

**Related How-To**: See [Write Effective Tests](/en/learn/software-engineering/programming-language/golang/how-to/write-effective-tests) for general testing practices, [Optimize Performance](/en/learn/software-engineering/programming-language/golang/how-to/optimize-performance) for benchmarking.

**Related Cookbook**: See Cookbook recipes "Table-Driven Tests", "Test Helpers", "Golden Files" for ready-to-use patterns.
