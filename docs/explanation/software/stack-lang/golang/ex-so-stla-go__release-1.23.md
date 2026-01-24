---
title: Go 1.23 Release Notes
description: New features and improvements in Go 1.23
category: explanation
subcategory: stack-lang
tags:
  - golang
  - release-notes
  - go-1.23
related:
  - ./ex-so-stla-go__best-practices.md
last_updated: 2026-01-24
---

# Go 1.23 Release: Iterators, Unique Package, and Timer Improvements

**Quick Reference**: [Overview](#overview) | [Iterator Functions](#iterator-functions) | [unique Package](#unique-package) | [Timer Behavior Improvements](#timer-behavior-improvements) | [Generic Type Aliases (Preview)](#generic-type-aliases-preview) | [Other Go 1.23 Improvements](#other-go-123-improvements) | [Migration Guide](#migration-guide) | [Conclusion](#conclusion) | [Related Documentation](#related-documentation)
Understanding the powerful features introduced in Go 1.23, including iterator functions with range-over-func support, the unique package for value canonicalization, timer behavior improvements, and generic type aliases preview.

## Overview

Go 1.23, released in August 2024, introduces iterator functions that enable custom iteration patterns with for-range loops, a unique package for efficient value deduplication, and important timer behavior improvements for better garbage collection.

Key features:

1. **Iterator Functions**: Range over custom iteration patterns with `for-range` syntax
2. **iter Package**: `iter.Seq` and `iter.Seq2` types for single and two-value iteration
3. **unique Package**: Canonicalization and interning for memory optimization
4. **Timer Improvements**: Unbuffered channels and GC-eligible timers
5. **Generic Type Aliases Preview**: Type aliases with type parameters (experimental)

This release empowers developers to create expressive, memory-efficient code with custom iteration patterns and value deduplication.

## Iterator Functions

### What Are Iterator Functions?

Go 1.23 allows ranging over functions, enabling custom iteration patterns:

```go
// Iterator function: yields values by calling provided function
func Count(n int) func(func(int) bool) {
    return func(yield func(int) bool) {
        for i := 0; i < n; i++ {
            if !yield(i) {
                return  // Stop iteration if yield returns false
            }
        }
    }
}

// Range over iterator function
for i := range Count(5) {
    fmt.Println(i)  // 0, 1, 2, 3, 4
}
```

### Iterator Function Signatures

Three patterns for iterator functions:

```go
// 1. No-value iterator (for range only)
func Repeat(n int) func(func() bool) {
    return func(yield func() bool) {
        for i := 0; i < n; i++ {
            if !yield() {
                return
            }
        }
    }
}

// Usage: Execute n times
for range Repeat(3) {
    fmt.Println("hello")
}
// Output: hello, hello, hello

// 2. Single-value iterator
func Countdown(from int) func(func(int) bool) {
    return func(yield func(int) bool) {
        for i := from; i >= 0; i-- {
            if !yield(i) {
                return
            }
        }
    }
}

// Usage: Single loop variable
for n := range Countdown(5) {
    fmt.Println(n)  // 5, 4, 3, 2, 1, 0
}

// 3. Two-value iterator (key-value pairs)
func Enumerate[T any](slice []T) func(func(int, T) bool) {
    return func(yield func(int, T) bool) {
        for i, v := range slice {
            if !yield(i, v) {
                return
            }
        }
    }
}

// Usage: Two loop variables
words := []string{"hello", "world"}
for i, word := range Enumerate(words) {
    fmt.Printf("%d: %s\n", i, word)
}
// Output:
// 0: hello
// 1: world
```

### The iter Package

Go 1.23 provides `iter.Seq` and `iter.Seq2` types for iterator functions:

```go
import "iter"

// iter.Seq[V]: Single-value iterator
type Seq[V any] func(yield func(V) bool)

// iter.Seq2[K, V]: Two-value iterator
type Seq2[K, V any] func(yield func(K, V) bool)
```

Using iter types:

```go
import "iter"

// Single-value iterator using iter.Seq
func Fibonacci(max int) iter.Seq[int] {
    return func(yield func(int) bool) {
        a, b := 0, 1
        for a <= max {
            if !yield(a) {
                return
            }
            a, b = b, a+b
        }
    }
}

// Usage
for n := range Fibonacci(100) {
    fmt.Println(n)  // 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89
}

// Two-value iterator using iter.Seq2
func Lines(text string) iter.Seq2[int, string] {
    return func(yield func(int, string) bool) {
        lines := strings.Split(text, "\n")
        for i, line := range lines {
            if !yield(i, line) {
                return
            }
        }
    }
}

// Usage
text := "first\nsecond\nthird"
for lineNum, line := range Lines(text) {
    fmt.Printf("Line %d: %s\n", lineNum, line)
}
```

### slices Package Iterator Functions

The `slices` package adds iterator functions:

```go
import "slices"

// All: Iterator over index-value pairs
s := []string{"a", "b", "c"}
for i, v := range slices.All(s) {
    fmt.Printf("%d: %s\n", i, v)
}
// Output:
// 0: a
// 1: b
// 2: c

// Values: Iterator over values only
for v := range slices.Values(s) {
    fmt.Println(v)
}
// Output: a, b, c

// Backward: Iterate in reverse
for i, v := range slices.Backward(s) {
    fmt.Printf("%d: %s\n", i, v)
}
// Output:
// 2: c
// 1: b
// 0: a

// Collect: Convert iterator to slice
numbers := func(yield func(int) bool) {
    for i := 0; i < 5; i++ {
        if !yield(i * i) {
            return
        }
    }
}
result := slices.Collect(numbers)  // []int{0, 1, 4, 9, 16}
```

### maps Package Iterator Functions

The `maps` package adds iterator functions for maps:

```go
import "maps"

m := map[string]int{
    "apple":  1,
    "banana": 2,
    "cherry": 3,
}

// All: Iterator over key-value pairs
for k, v := range maps.All(m) {
    fmt.Printf("%s: %d\n", k, v)
}

// Keys: Iterator over keys only
for k := range maps.Keys(m) {
    fmt.Println(k)
}
// Output: apple, banana, cherry (unordered)

// Values: Iterator over values only
for v := range maps.Values(m) {
    fmt.Println(v)
}
// Output: 1, 2, 3 (unordered)

// Collect: Convert iterator to map
pairs := func(yield func(string, int) bool) {
    for i, fruit := range []string{"apple", "banana"} {
        if !yield(fruit, i) {
            return
        }
    }
}
result := maps.Collect(pairs)  // map[string]int{"apple": 0, "banana": 1}
```

### Practical Iterator Examples

#### Tree Traversal

```go
type TreeNode[T any] struct {
    Value T
    Left  *TreeNode[T]
    Right *TreeNode[T]
}

// In-order traversal iterator
func (n *TreeNode[T]) InOrder() iter.Seq[T] {
    return func(yield func(T) bool) {
        n.inOrderHelper(yield)
    }
}

func (n *TreeNode[T]) inOrderHelper(yield func(T) bool) bool {
    if n == nil {
        return true
    }
    if !n.Left.inOrderHelper(yield) {
        return false
    }
    if !yield(n.Value) {
        return false
    }
    return n.Right.inOrderHelper(yield)
}

// Usage
tree := &TreeNode[int]{
    Value: 5,
    Left:  &TreeNode[int]{Value: 3},
    Right: &TreeNode[int]{Value: 7},
}

for value := range tree.InOrder() {
    fmt.Println(value)  // 3, 5, 7
}
```

#### File Line Iterator

```go
import (
    "bufio"
    "iter"
    "os"
)

// Iterator over file lines
func ReadLines(filename string) iter.Seq2[int, string] {
    return func(yield func(int, string) bool) {
        file, err := os.Open(filename)
        if err != nil {
            return
        }
        defer file.Close()

        scanner := bufio.NewScanner(file)
        lineNum := 0
        for scanner.Scan() {
            if !yield(lineNum, scanner.Text()) {
                return
            }
            lineNum++
        }
    }
}

// Usage
for lineNum, line := range ReadLines("input.txt") {
    fmt.Printf("%d: %s\n", lineNum, line)
}
```

#### Database Result Iterator

```go
import (
    "database/sql"
    "iter"
)

type Beneficiary struct {
    ID   int
    Name string
}

// Iterator over database results
func QueryUsers(db *sql.DB, query string) iter.Seq[Beneficiary] {
    return func(yield func(Beneficiary) bool) {
        rows, err := db.Query(query)
        if err != nil {
            return
        }
        defer rows.Close()

        for rows.Next() {
            var beneficiary Beneficiary
            if err := rows.Scan(&beneficiary.ID, &beneficiary.Name); err != nil {
                return
            }
            if !yield(beneficiary) {
                return
            }
        }
    }
}

// Usage
for beneficiary := range QueryUsers(db, "SELECT id, name FROM users") {
    fmt.Printf("Beneficiary %d: %s\n", beneficiary.ID, beneficiary.Name)
}
```

#### Lazy Transformation Pipeline

```go
import "iter"

// Filter iterator
func Filter[T any](seq iter.Seq[T], predicate func(T) bool) iter.Seq[T] {
    return func(yield func(T) bool) {
        for v := range seq {
            if predicate(v) {
                if !yield(v) {
                    return
                }
            }
        }
    }
}

// Map iterator
func Map[T, U any](seq iter.Seq[T], fn func(T) U) iter.Seq[U] {
    return func(yield func(U) bool) {
        for v := range seq {
            if !yield(fn(v)) {
                return
            }
        }
    }
}

// Take iterator (limit)
func Take[T any](seq iter.Seq[T], n int) iter.Seq[T] {
    return func(yield func(T) bool) {
        count := 0
        for v := range seq {
            if count >= n {
                return
            }
            if !yield(v) {
                return
            }
            count++
        }
    }
}

// Usage: Lazy transformation pipeline
numbers := slices.Values([]int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})

// Chain transformations (lazy evaluation)
result := Take(
    Map(
        Filter(numbers, func(n int) bool { return n%2 == 0 }),
        func(n int) int { return n * n },
    ),
    3,
)

// Only computes first 3 results
for n := range result {
    fmt.Println(n)  // 4, 16, 36
}
```

#### Infinite Sequences

```go
import "iter"

// Infinite sequence of natural numbers
func Naturals() iter.Seq[int] {
    return func(yield func(int) bool) {
        for i := 0; ; i++ {
            if !yield(i) {
                return
            }
        }
    }
}

// Infinite sequence of primes
func Primes() iter.Seq[int] {
    return func(yield func(int) bool) {
        for n := range Naturals() {
            if isPrime(n) {
                if !yield(n) {
                    return
                }
            }
        }
    }
}

func isPrime(n int) bool {
    if n < 2 {
        return false
    }
    for i := 2; i*i <= n; i++ {
        if n%i == 0 {
            return false
        }
    }
    return true
}

// Usage: Take first 5 primes
count := 0
for p := range Primes() {
    fmt.Println(p)
    count++
    if count >= 5 {
        break
    }
}
// Output: 2, 3, 5, 7, 11
```

## unique Package

The `unique` package provides value canonicalization (interning) for memory optimization.

### What Is Canonicalization?

Canonicalization ensures only one copy of each distinct value exists in memory. Multiple references to logically equal values share the same underlying storage.

### unique.Make Function

```go
import "unique"

// Canonicalize a value
h1 := unique.Make("hello")  // Returns unique.Handle[string]
h2 := unique.Make("hello")  // Returns same Handle (shares storage)

// h1 and h2 refer to same underlying value
fmt.Println(h1 == h2)  // true (same Handle)
```

### unique.Handle Type

```go
// Handle[T] is a canonical reference to a value of type T
type Handle[T comparable] struct {
    // Internal implementation
}

// Value returns the canonical value
h := unique.Make("world")
s := h.Value()  // "world"
```

### String Interning Example

```go
import "unique"

type Logger struct {
    messages []unique.Handle[string]
}

func (l *Logger) Log(message string) {
    // Intern string to save memory
    handle := unique.Make(message)
    l.messages = append(l.messages, handle)
}

func (l *Logger) PrintLogs() {
    for _, h := range l.messages {
        fmt.Println(h.Value())
    }
}

// Usage
logger := &Logger{}

// Repeated strings share storage
logger.Log("Error: Connection failed")
logger.Log("Error: Connection failed")  // Reuses same Handle
logger.Log("Info: Retrying")
logger.Log("Error: Connection failed")  // Reuses same Handle

// Memory saved: Only one copy of "Error: Connection failed"
```

### Struct Canonicalization

```go
import "unique"

type Config struct {
    Host string
    Port int
}

var configCache = make(map[Config]unique.Handle[Config])

func GetConfig(host string, port int) unique.Handle[Config] {
    cfg := Config{Host: host, Port: port}

    // Return cached Handle if exists
    if h, ok := configCache[cfg]; ok {
        return h
    }

    // Create and cache new Handle
    h := unique.Make(cfg)
    configCache[cfg] = h
    return h
}

// Usage
c1 := GetConfig("localhost", 8080)
c2 := GetConfig("localhost", 8080)  // Same Handle
c3 := GetConfig("example.com", 443)  // Different Handle

fmt.Println(c1 == c2)  // true (same config)
fmt.Println(c1 == c3)  // false (different config)
```

### Memory Optimization Use Cases

```go
// 1. Deduplicating log messages
type LogEntry struct {
    Timestamp time.Time
    Level     string
    Message   unique.Handle[string]  // Deduplicate messages
}

func NewLogEntry(level, message string) LogEntry {
    return LogEntry{
        Timestamp: time.Now(),
        Level:     level,
        Message:   unique.Make(message),  // Intern message
    }
}

// 2. Deduplicating beneficiary agents
type Request struct {
    Path      string
    UserAgent unique.Handle[string]  // Many requests share same UA
}

// 3. Deduplicating configuration values
type ServerConfig struct {
    Environment unique.Handle[string]  // "production", "staging", "dev"
    Region      unique.Handle[string]  // "us-east-1", "eu-west-1", etc.
}
```

### Performance Considerations

```go
// Canonicalization has overhead
// Use when:
// - Same values repeated many times
// - Memory is constrained
// - Values are long-lived

// Don't use when:
// - Values are unique or rarely repeated
// - Values are short-lived
// - Canonicalization overhead exceeds memory savings

// Example: Good use case
type Cache struct {
    entries map[string]unique.Handle[[]byte]  // Large values, many duplicates
}

// Example: Bad use case
type Counter struct {
    value unique.Handle[int]  // Small value, unlikely to repeat
}
```

## Timer Behavior Improvements

Go 1.23 improves timer behavior for better garbage collection and predictability.

### Timer Channel Changes

#### Before Go 1.23: Buffered Channels

```go
// Pre-Go 1.23: Timer channels had capacity 1
timer := time.NewTimer(time.Second)
// timer.C has capacity 1 (buffered)

// Race condition possible:
// 1. Timer expires, sends to buffered channel
// 2. Stop() called
// 3. Channel still contains expired value
```

#### Go 1.23: Unbuffered Channels

```go
// Go 1.23: Timer channels have capacity 0 (unbuffered)
timer := time.NewTimer(time.Second)
// timer.C has capacity 0 (unbuffered)

// No race: Timer only sends when receiver is ready
// Stop() prevents send if called before expiration
```

### GC-Eligible Timers

#### Before Go 1.23: Manual Stop Required

```go
// Pre-Go 1.23: Timers not GC'd even if unreachable
func createTimer() {
    timer := time.NewTimer(time.Hour)
    // Timer not stopped - stays in memory until expiration!
}

// Memory leak: Timer stays in global timer heap
```

#### Go 1.23: Automatic GC

```go
// Go 1.23: Timers GC'd when unreferenced
func createTimer() {
    timer := time.NewTimer(time.Hour)
    // Timer becomes unreachable - GC reclaims it
    // No need to explicitly stop for GC
}

// No memory leak: Timer removed from heap when GC'd
```

### Migration and Compatibility

```bash
# Opt into old behavior (for compatibility)
GODEBUG=asynctimerchan=1 go run main.go

# Or in code:
//go:debug asynctimerchan=1
package main
```

### Practical Impact

```go
// Pattern 1: Timeout with context (improved)
func doWorkWithTimeout(ctx context.Context) error {
    timer := time.NewTimer(5 * time.Second)
    // No need to defer timer.Stop() for GC
    // (still recommended for immediate cleanup)

    select {
    case <-timer.C:
        return errors.New("timeout")
    case <-ctx.Done():
        return ctx.Err()
    }
}

// Pattern 2: Repeated timers (no change needed)
func periodicWork() {
    ticker := time.NewTicker(time.Second)
    defer ticker.Stop()  // Still recommended

    for range ticker.C {
        doWork()
    }
}

// Pattern 3: Short-lived timers (improved)
func sleep(d time.Duration) {
    <-time.After(d)
    // Timer automatically GC'd after firing
}
```

## Generic Type Aliases (Preview)

Go 1.23 includes a preview of generic type aliases (experimental).

### Basic Generic Type Alias

```go
// Generic type alias (experimental)
type Pair[T any] = struct {
    First  T
    Second T
}

// Usage
p := Pair[int]{First: 1, Second: 2}
fmt.Println(p.First, p.Second)  // 1 2

// Generic type alias for function type
type Predicate[T any] = func(T) bool

// Usage
var isEven Predicate[int] = func(n int) bool {
    return n%2 == 0
}
fmt.Println(isEven(4))  // true
```

### Complex Generic Aliases

```go
// Alias for generic map type
type Cache[K comparable, V any] = map[K]V

// Usage
var userCache Cache[int, string]
userCache = make(Cache[int, string])
userCache[1] = "Alice"

// Alias for generic slice type
type List[T any] = []T

// Usage
var numbers List[int] = List[int]{1, 2, 3}
```

### Current Limitations (Go 1.23)

```go
// Generic type aliases are experimental
// GOEXPERIMENT=aliastypeparams required to enable

// Build with experiment enabled:
// GOEXPERIMENT=aliastypeparams go build

// Not yet stable - syntax and behavior may change
```

## Other Go 1.23 Improvements

### Standard Library Enhancements

```go
// time.Sleep minimum duration clamped to 0
// Negative duration treated as 0
time.Sleep(-1 * time.Second)  // Sleeps 0 (no panic)

// math.Rand deprecated - use math/rand/v2
// (Guidance to migrate to rand/v2)
```

### Performance Improvements

- Better compiler optimizations for iterator functions
- Improved inlining decisions for generic code
- Reduced allocations in hot paths

## Migration Guide

### Adopting Iterator Functions

```go
// Step 1: Identify custom iteration needs
// Example: Iterating over tree nodes

// Before: Manual iteration
func (tree *Tree) VisitAll(fn func(*Node)) {
    var visit func(*Node)
    visit = func(n *Node) {
        if n == nil {
            return
        }
        visit(n.Left)
        fn(n)
        visit(n.Right)
    }
    visit(tree.Root)
}

// Usage (before)
tree.VisitAll(func(n *Node) {
    fmt.Println(n.Value)
})

// After: Iterator function
func (tree *Tree) InOrder() iter.Seq[*Node] {
    return func(yield func(*Node) bool) {
        var visit func(*Node) bool
        visit = func(n *Node) bool {
            if n == nil {
                return true
            }
            if !visit(n.Left) {
                return false
            }
            if !yield(n) {
                return false
            }
            return visit(n.Right)
        }
        visit(tree.Root)
    }
}

// Usage (after)
for node := range tree.InOrder() {
    fmt.Println(node.Value)
}
```

### Using unique Package

```go
// Step 1: Identify repeated values
// Example: Log messages

// Before: Duplicated strings
type LogEntry struct {
    Timestamp time.Time
    Message   string  // Duplicated for repeated messages
}

// After: Canonicalized strings
import "unique"

type LogEntry struct {
    Timestamp time.Time
    Message   unique.Handle[string]  // Shared storage
}

func NewLogEntry(message string) LogEntry {
    return LogEntry{
        Timestamp: time.Now(),
        Message:   unique.Make(message),  // Canonicalize
    }
}
```

### Timer Updates

```go
// Review timer usage patterns
// Ensure Stop() is still called where needed for immediate cleanup

// Pattern that benefits from new behavior:
func shortLivedTimer() {
    timer := time.NewTimer(100 * time.Millisecond)
    select {
    case <-timer.C:
        // Handle timeout
    case result := <-workChan:
        // Got result, timer will be GC'd automatically
        _ = result
    }
    // No defer timer.Stop() needed for GC
    // (but still recommended for immediate resource cleanup)
}
```

## Conclusion

Go 1.23 introduces powerful abstractions for iteration and memory optimization:

- **Iterator functions** enable expressive, composable iteration patterns
- **iter package** provides standard types for single and two-value iterators
- **unique package** offers efficient value deduplication for memory savings
- **Timer improvements** simplify resource management and prevent leaks
- **Generic type aliases** preview future language capabilities

These features empower developers to write more expressive, memory-efficient code while maintaining Go's simplicity and performance.

## Related Documentation

- Release Documentation: Go 1.18 (Generics), Go 1.21 (PGO), Go 1.22 (Loop Variables, Routing), Go 1.24, Go 1.25
- Core Concepts: Generics, Iteration, Memory Management
- Advanced Topics: Performance Optimization, Standard Library

---

**Last Updated**: 2026-01-23
**Go Version**: 1.21+ (baseline), 1.22+ (recommended), 1.23 (latest)
**Maintainers**: Platform Documentation Team
