---
title: "Testing Qa"
date: 2026-02-04T00:00:00+07:00
draft: false
description: "Static analysis, linting, race detection, and code coverage tools"
weight: 10000086
tags: ["golang", "testing", "qa", "linting", "static-analysis"]
---

## Why Testing & QA Matters

Testing and quality assurance tools catch bugs before production, enforce code standards, detect race conditions, and measure test coverage. Go's built-in testing tools (vet, race detector) combined with production linters (golangci-lint) provide comprehensive quality gates essential for reliable systems.

**Core benefits**:

- **Early bug detection**: Find issues before code review
- **Consistent style**: Automated enforcement of team standards
- **Race condition detection**: Catch concurrency bugs in tests
- **Coverage measurement**: Identify untested code paths

**Problem**: Without systematic QA, bugs reach production, code quality degrades, and concurrency issues cause intermittent failures.

**Solution**: Layer Go's built-in tools (vet, race detector, coverage) with production linters for comprehensive quality checks.

## Standard Library: go vet

`go vet` is Go's built-in static analysis tool that detects suspicious code patterns.

**Basic usage**:

```bash
go vet
# => Analyzes current package
# => Checks for common mistakes
# => Exit code 0 if no issues, 1 if problems found

go vet ./...
# => Analyzes all packages recursively
# => Common in CI/CD pipelines
```

**What go vet catches**:

```go
// Example 1: Printf format mismatch
package main

import "fmt"

func main() {
    name := "Alice"
    age := 30
    // => name is string, age is int

    fmt.Printf("Name: %d, Age: %s\n", name, age)
    // => Wrong: %d expects int, got string
    // => Wrong: %s expects string, got int
}
```

```bash
go vet
# => Output: ./main.go:8:2: Printf format %d has arg name of wrong type string
#           ./main.go:8:2: Printf format %s has arg age of wrong type int
# => Caught at compile-time, not runtime
```

**Example 2: Unreachable code**:

```go
package main

func process() int {
    return 42
    // => Returns immediately

    println("This never runs")
    // => Unreachable code
    // => Dead code, waste of maintenance
}
```

```bash
go vet
# => Output: ./main.go:5:2: unreachable code
# => Detects code after return statement
```

**Example 3: Lost cancel function**:

```go
package main

import "context"

func doWork() {
    ctx, _ := context.WithCancel(context.Background())
    // => Underscore discards cancel function
    // => Context leak (never cancelled)
    // => Goroutines may leak

    // Use ctx without ever calling cancel
    _ = ctx
}
```

```bash
go vet
# => Output: ./main.go:6:2: the cancel function returned by context.WithCancel should be called, not discarded
# => Detects missing cancel call
```

**Example 4: Invalid struct tags**:

```go
package main

type User struct {
    Name  string `json:"name"`
    Email string `json:"email,omitempty"`
    // => Valid JSON tags

    Age   int    `json:age`
    // => Invalid: missing quotes
    // => Will fail at runtime during JSON encoding
}
```

```bash
go vet
# => Output: ./main.go:9:2: struct field tag `json:age` not compatible with reflect.StructTag.Get: bad syntax for struct tag value
# => Catches malformed struct tags
```

**go vet checks** (partial list):

- Printf-like format string verification
- Unreachable code detection
- Context usage (cancel functions)
- Struct tag validation
- Suspicious conversions
- Shadow variable detection
- Tests that don't call t.Fatal correctly

**Limitations of go vet**:

- Conservative (few false positives, some false negatives)
- Limited to built-in analyzers
- No style checking (formatting, naming)

## Standard Library: Race Detector

Go's race detector finds data races during test execution.

**What is a data race**:

```go
package main

import (
    "fmt"
    "sync"
)

var counter int
// => Shared variable (global)
// => Multiple goroutines may access

func increment() {
    counter++
    // => Race condition: read + write not atomic
    // => Two goroutines can read same value simultaneously
}

func main() {
    var wg sync.WaitGroup

    for i := 0; i < 1000; i++ {
        wg.Add(1)
        go func() {
            defer wg.Done()
            increment()
            // => 1000 goroutines all increment counter
        }()
    }

    wg.Wait()
    fmt.Println("Counter:", counter)
    // => Expected: 1000
    // => Actual: varies (900-1000) due to race
}
```

**Running with race detector**:

```bash
go run -race main.go
# => Runs with race detection enabled
# => Output: WARNING: DATA RACE
#           Read at 0x... by goroutine 42:
#             main.increment()
#           Previous write at 0x... by goroutine 41:
#             main.increment()
# => Shows exact lines where race occurs
```

**Testing with race detector**:

```bash
go test -race
# => Runs tests with race detection
# => Slower execution (5-10x overhead)
# => Use in CI/CD, not every local test

go test -race ./...
# => Tests all packages with race detection
# => Standard in continuous integration
```

**Race detector example** (test):

```go
// File: counter_test.go
package main

import (
    "sync"
    "testing"
)

func TestConcurrentIncrement(t *testing.T) {
    // => Test concurrent access to counter
    counter = 0
    // => Reset global

    var wg sync.WaitGroup

    for i := 0; i < 100; i++ {
        wg.Add(1)
        go func() {
            defer wg.Done()
            increment()
            // => Race: multiple goroutines increment
        }()
    }

    wg.Wait()

    if counter != 100 {
        t.Errorf("Expected 100, got %d", counter)
    }
}
```

```bash
go test -race
# => Output: WARNING: DATA RACE
#           Race detected in counter variable
#           Test fails
```

**Fixed version** (using mutex):

```go
package main

import "sync"

var (
    counter int
    mu      sync.Mutex
    // => Mutex protects counter
)

func increment() {
    mu.Lock()
    // => Acquires lock (blocks if held)
    defer mu.Unlock()
    // => Releases lock when function returns

    counter++
    // => Safe: only one goroutine can execute this
}
```

```bash
go test -race
# => No race detected
# => Test passes
```

**Race detector limitations**:

- Only detects races that execute during test run
- Performance overhead (5-10x slower)
- Memory overhead (5-10x more memory)
- Not exhaustive (missed races possible if untested code paths)

**When to use race detector**:

- Always in CI/CD for projects with goroutines
- During development when writing concurrent code
- Before releasing concurrent features
- Skip for CPU-bound benchmarks (overhead distorts results)

## Standard Library: Code Coverage

Go's built-in coverage tool measures test coverage percentage.

**Basic coverage**:

```bash
go test -cover
# => Runs tests and reports coverage
# => Output: coverage: 85.7% of statements
# => Quick summary of test coverage
```

**Detailed coverage report**:

```bash
go test -coverprofile=coverage.out
# => Generates coverage profile file
# => coverage.out contains per-line coverage data

go tool cover -html=coverage.out
# => Opens HTML report in browser
# => Shows which lines covered (green) and uncovered (red)
```

**Coverage example**:

```go
// File: math.go
package math

func Add(a, b int) int {
    return a + b
    // => Covered if TestAdd runs
}

func Subtract(a, b int) int {
    return a - b
    // => Uncovered if no test for Subtract
}

func Divide(a, b int) (int, error) {
    if b == 0 {
        return 0, errors.New("division by zero")
        // => Error path: covered if test checks divide-by-zero
    }
    return a / b, nil
    // => Happy path: covered if test divides valid numbers
}
```

```go
// File: math_test.go
package math

import "testing"

func TestAdd(t *testing.T) {
    // => Tests Add function
    result := Add(2, 3)
    if result != 5 {
        t.Errorf("Expected 5, got %d", result)
    }
}

func TestDivide(t *testing.T) {
    // => Tests Divide happy path only
    result, err := Divide(10, 2)
    if err != nil || result != 5 {
        t.Errorf("Expected 5, got %d with error %v", result, err)
    }
}
```

```bash
go test -cover
# => Output: coverage: 66.7% of statements
# => Add: covered (1/1 lines)
# => Subtract: uncovered (0/1 lines)
# => Divide: partially covered (1/2 branches - missing error path)
```

**Coverage by package**:

```bash
go test -coverprofile=coverage.out ./...
# => Tests all packages with coverage

go tool cover -func=coverage.out
# => Shows coverage per function
# => Output:
#   math.go:3:    Add         100.0%
#   math.go:7:    Subtract    0.0%
#   math.go:11:   Divide      50.0%
#   total:        (statements) 66.7%
```

**Coverage thresholds** (CI/CD):

```bash
#!/bin/bash
# File: check-coverage.sh

threshold=80
# => Minimum coverage requirement

coverage=$(go test -coverprofile=coverage.out ./... | grep coverage: | awk '{print $2}' | sed 's/%//')
# => Extracts coverage percentage

if (( $(echo "$coverage < $threshold" | bc -l) )); then
    echo "Coverage $coverage% is below threshold $threshold%"
    exit 1
fi

echo "Coverage $coverage% meets threshold"
```

**Coverage best practices**:

- Aim for 70-80% coverage (diminishing returns above)
- Focus on critical paths, not 100%
- Test error paths and edge cases
- Ignore generated code (add `//go:generate` comment)

**Coverage gotchas**:

```go
// Coverage doesn't measure:
// 1. Logic correctness (can have 100% coverage with wrong logic)
// 2. All execution paths (branches inside conditions)
// 3. Concurrency issues (race conditions)
```

## Production Tool: golangci-lint

golangci-lint aggregates 50+ linters into a single, fast tool.

**Installation**:

```bash
# Linux/macOS
curl -sSfL https://raw.githubusercontent.com/golangci/golangci-lint/master/install.sh | sh -s -- -b $(go env GOPATH)/bin

# macOS (Homebrew)
brew install golangci-lint

# Verify
golangci-lint version
```

**Basic usage**:

```bash
golangci-lint run
# => Runs with default linters (fast, essential checks)
# => ~10 linters enabled by default

golangci-lint run --enable-all
# => Runs all 50+ linters (slow, comprehensive)
# => Use for initial codebase audit

golangci-lint run --fix
# => Auto-fixes issues where possible
# => Applies formatting, removes unused imports
```

**Configuration** (.golangci.yml):

```yaml
# File: .golangci.yml
run:
  timeout: 5m
  tests: true
  # => Include test files in linting

linters:
  enable:
    - gofmt # => Format checking
    - goimports # => Import organization
    - govet # => Built-in vet
    - errcheck # => Unchecked error returns
    - staticcheck # => Advanced static analysis
    - unused # => Unused code detection
    - gosimple # => Simplification suggestions
    - ineffassign # => Ineffectual assignments
    - misspell # => Spelling errors
    - revive # => Replacement for golint

linters-settings:
  errcheck:
    check-blank: true
    # => Enforce checking errors assigned to _

  revive:
    rules:
      - name: exported
        # => Exported functions must have comments

issues:
  exclude-rules:
    - path: _test\.go
      linters:
        - errcheck
        # => Allow unchecked errors in tests

  max-issues-per-linter: 0
  max-same-issues: 0
  # => Report all issues (no limits)
```

**Key linters**:

```go
// errcheck: Detects unchecked errors
package main

import "os"

func main() {
    os.Remove("file.txt")
    // => Error ignored
    // => errcheck: Error return value of `os.Remove` is not checked
}

// Fix:
func main() {
    if err := os.Remove("file.txt"); err != nil {
        // Handle error
    }
}
```

```go
// staticcheck: Advanced analysis
package main

func example() {
    s := "hello"
    s = s[:0]
    // => staticcheck: this value of s is never used
    // => Ineffectual assignment
}
```

```go
// gosimple: Simplification suggestions
package main

func check(b bool) bool {
    if b == true {
        return true
    }
    return false
    // => gosimple: should omit comparison to bool constant
}

// Fix:
func check(b bool) bool {
    return b
}
```

**CI/CD integration**:

```bash
# Run in CI
golangci-lint run --out-format=github-actions
# => Formats output for GitHub Actions annotations
# => Inline comments on PR

# Exit code
golangci-lint run
echo $?
# => 0 if no issues, 1 if issues found
# => Fails CI build on linting violations
```

**Performance**:

```bash
time go vet ./...
# => ~5 seconds

time golangci-lint run
# => ~10 seconds (runs multiple linters in parallel)
# => Faster than running linters individually
```

**Trade-offs**:

| Approach                | Pros                                  | Cons                               |
| ----------------------- | ------------------------------------- | ---------------------------------- |
| go vet only             | Fast, built-in, no dependencies       | Limited checks                     |
| golangci-lint (default) | Comprehensive, fast, configurable     | External dependency (20MB)         |
| golangci-lint (all)     | Exhaustive checks, finds obscure bugs | Slow, noisy (many false positives) |

**When to use**:

- **go vet**: Always (built-in, fast)
- **golangci-lint (default)**: CI/CD pipelines, pre-commit hooks
- **golangci-lint --enable-all**: Initial codebase audit, refactoring sprints

## Fuzzing (Go 1.18+)

Go's built-in fuzzing generates random inputs to find edge cases.

**Basic fuzz test**:

```go
// File: parse_test.go
package main

import (
    "testing"
    "unicode/utf8"
)

func FuzzReverse(f *testing.F) {
    // => Fuzz test function (starts with Fuzz)
    // => f is *testing.F for fuzzing control

    testcases := []string{"Hello", "世界", " "}
    // => Seed inputs for fuzzing engine

    for _, tc := range testcases {
        f.Add(tc)
        // => Adds seed to corpus
        // => Fuzzer mutates these inputs
    }

    f.Fuzz(func(t *testing.T, input string) {
        // => Fuzz target function
        // => Called with random inputs

        if !utf8.ValidString(input) {
            return
            // => Skip invalid UTF-8 (not interesting)
        }

        rev := Reverse(input)
        // => Calls function under test

        doubleRev := Reverse(rev)
        // => Reverse of reverse should equal original

        if input != doubleRev {
            t.Errorf("Reverse(Reverse(%q)) = %q, want %q", input, doubleRev, input)
            // => Found a bug!
        }
    })
}

func Reverse(s string) string {
    // => Function under test
    b := []byte(s)
    // => Bug: doesn't handle Unicode properly

    for i, j := 0, len(b)-1; i < j; i, j = i+1, j-1 {
        b[i], b[j] = b[j], b[i]
    }
    return string(b)
}
```

**Running fuzzing**:

```bash
go test -fuzz=Fuzz
# => Runs fuzz tests indefinitely until failure or Ctrl+C
# => Generates random inputs
# => Stores failing inputs in testdata/fuzz/

go test -fuzz=FuzzReverse -fuzztime=30s
# => Fuzzes for 30 seconds, then stops
# => Useful in CI/CD (time-limited)
```

**Fuzzing output** (when bug found):

```
fuzz: elapsed: 0s, execs: 245 (0/sec), new interesting: 0 (total: 1)
fuzz: elapsed: 3s, execs: 85421 (28473/sec), new interesting: 2 (total: 3)
--- FAIL: FuzzReverse (3.12s)
    --- FAIL: FuzzReverse (0.00s)
        parse_test.go:24: Reverse(Reverse("👋")) = "\xbd\xf0\x9f", want "👋"

    Failing input written to testdata/fuzz/FuzzReverse/abc123
```

**Fuzzing best practices**:

- Define property-based assertions (not exact output checks)
- Handle invalid inputs gracefully (return instead of panic)
- Limit fuzz time in CI (30s-1m)
- Commit generated failing cases to testdata/fuzz/

## Best Practices

**Quality pipeline** (run in order):

```bash
# 1. Format check
gofmt -l .
# => Lists files not properly formatted

# 2. Import organization
goimports -l .
# => Checks import order

# 3. Static analysis
go vet ./...
# => Built-in checks

# 4. Linting
golangci-lint run
# => Comprehensive linting

# 5. Tests with race detection
go test -race ./...
# => Catches concurrency bugs

# 6. Coverage check
go test -coverprofile=coverage.out ./...
go tool cover -func=coverage.out | grep total
# => Verify coverage threshold

# 7. Fuzzing (optional, CI only)
go test -fuzz=Fuzz -fuzztime=30s ./...
# => Time-limited fuzzing
```

**Pre-commit hook** (.git/hooks/pre-commit):

```bash
#!/bin/bash
# Exit on error
set -e

echo "Running quality checks..."

# Format
if [ -n "$(gofmt -l .)" ]; then
    echo "Code not formatted. Run: gofmt -w ."
    exit 1
fi

# Vet
go vet ./...

# Lint
golangci-lint run

echo "Quality checks passed"
```

**CI/CD configuration** (GitHub Actions):

```yaml
# File: .github/workflows/test.yml
name: Test

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: actions/setup-go@v4
        with:
          go-version: "1.23"

      - name: Install golangci-lint
        run: curl -sSfL https://raw.githubusercontent.com/golangci/golangci-lint/master/install.sh | sh -s -- -b $(go env GOPATH)/bin

      - name: Vet
        run: go vet ./...

      - name: Lint
        run: golangci-lint run

      - name: Test
        run: go test -race -coverprofile=coverage.out ./...

      - name: Coverage
        run: go tool cover -func=coverage.out
```

## Summary

Go QA toolkit:

- **go vet**: Built-in static analysis (always use)
- **Race detector**: Finds concurrency bugs (use in tests)
- **Coverage**: Measures test coverage (aim for 70-80%)
- **golangci-lint**: Aggregates 50+ linters (CI/CD essential)
- **Fuzzing**: Generates inputs to find edge cases (Go 1.18+)

**Quality gate checklist**:

```bash
# Required (fast, always run)
go fmt ./...
go vet ./...
golangci-lint run
go test ./...

# Recommended (slower, CI/CD)
go test -race ./...
go test -coverprofile=coverage.out ./...

# Optional (specific scenarios)
go test -fuzz=Fuzz -fuzztime=30s  # New code with complex inputs
```

**Progressive adoption**:

1. Start with `go vet` and `go test`
2. Add `golangci-lint` with default linters
3. Enable race detector in CI/CD
4. Enforce coverage thresholds
5. Add fuzzing for security-critical code

**Tool comparison**:

| Tool          | Speed | Checks                | When to Use                |
| ------------- | ----- | --------------------- | -------------------------- |
| go vet        | Fast  | Basic static analysis | Always (pre-commit, CI)    |
| race detector | Slow  | Data races            | CI/CD, concurrent code     |
| golangci-lint | Fast  | 50+ linters           | CI/CD, pre-commit          |
| coverage      | Fast  | Test coverage         | CI/CD, coverage reports    |
| fuzzing       | Slow  | Edge case inputs      | Security-critical code, CI |
