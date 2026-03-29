---
title: "Type System"
date: 2026-02-04T00:00:00+07:00
draft: false
description: "Named vs unnamed types, interface satisfaction, type assertions, and generics in Go"
weight: 1000020
tags: ["golang", "type-system", "interfaces", "generics", "production"]
---

## Why Go's Type System Matters

Go's type system is deliberately simple compared to other languages, but this simplicity enables powerful compile-time safety without complex type hierarchies. Understanding named vs unnamed types, interface satisfaction, and when to use type parameters (generics) prevents runtime type errors and enables flexible, type-safe APIs.

**Core benefits**:

- **Compile-time safety**: Type errors caught before deployment
- **Structural typing**: Interfaces satisfied implicitly without declarations
- **Refactoring confidence**: Compiler verifies all type usages
- **Generic algorithms**: Reusable code without sacrificing type safety

**Problem**: Without understanding Go's type system, developers create overly complex inheritance-like hierarchies, miss interface opportunities, or misuse type assertions causing runtime panics.

**Solution**: Start with standard library type patterns, understand limitations, then leverage generics appropriately for type-safe abstractions.

## Standard Library: Named vs Unnamed Types

Go distinguishes between named and unnamed types. This distinction affects type equality and interface satisfaction.

**Pattern from standard library**:

```go
package main

import (
    "fmt"
    // => Standard library for formatted output
)

// NAMED TYPE: defined with type keyword
type UserID int64
// => UserID is a distinct type, not int64
// => Cannot assign int64 to UserID without conversion
// => Enables type safety for domain modeling

// UNNAMED TYPE: used directly
var age int64
// => age is type int64 (unnamed)
// => Can assign any int64 value without conversion

func processUser(id UserID) {
    // => id must be UserID, not int64
    // => Compiler enforces domain type safety
    // => Prevents mixing user IDs with other int64 values

    fmt.Printf("Processing user %d\n", id)
    // => Output: Processing user 12345
    // => Underlying value accessible (int64)
}

func main() {
    var rawID int64 = 12345
    // => rawID is int64

    // COMPILE ERROR: cannot use rawID (type int64) as type UserID
    // processUser(rawID)
    // => Type safety prevents accidental misuse

    // SOLUTION: explicit conversion required
    processUser(UserID(rawID))
    // => UserID(rawID) converts int64 to UserID
    // => Makes domain intent explicit
    // => Conversion is free at runtime (same representation)
}
```

**Type equality rules**:

```go
package main

// Named types are only equal to themselves
type Celsius float64
// => Celsius is distinct type
type Fahrenheit float64
// => Fahrenheit is distinct type (different from Celsius)

func convert(c Celsius) Fahrenheit {
    // => Cannot return c directly (different types)
    // => Must convert explicitly

    return Fahrenheit((c * 9 / 5) + 32)
    // => Fahrenheit() converts Celsius to Fahrenheit
    // => Calculation uses underlying float64
    // => Return value is Fahrenheit type
}

// Unnamed types are equal to their literal form
func add(a, b int) int {
    // => a and b are int (unnamed)
    // => Any int value accepted

    return a + b
    // => No conversion needed (same type)
}
```

**Limitations for production**:

- No exhaustiveness checking (unlike sealed types in other languages)
- No union types (must use interfaces with type assertions)
- No compile-time enforcement of type constraints (before generics)
- Manual type assertions required for type narrowing

## Production Framework: Type Assertions and Type Switches

Go provides type assertions and type switches for working with interface values and narrowing types at runtime.

**Pattern: Type Assertions**:

```go
package main

import (
    "fmt"
    // => Standard library for output
    "io"
    // => Standard library for I/O interfaces
    "os"
    // => Standard library for file operations
)

func processReader(r io.Reader) {
    // => r is io.Reader interface
    // => Actual type unknown at compile time
    // => Could be *os.File, *strings.Reader, bytes.Buffer, etc.

    // TYPE ASSERTION: check if r is *os.File
    if file, ok := r.(*os.File); ok {
        // => ok is true if assertion succeeds
        // => file is *os.File (type-narrowed)
        // => Safe two-value assertion (no panic)

        fmt.Printf("File descriptor: %d\n", file.Fd())
        // => file.Fd() available (*os.File method)
        // => Would not compile without type assertion
    } else {
        // => Assertion failed: r is not *os.File
        // => ok is false
        // => file is nil

        fmt.Println("Not a file")
        // => Output: Not a file
    }

    // DANGEROUS: single-value assertion panics if wrong type
    // file := r.(*os.File)
    // => Panics if r is not *os.File
    // => Only use when type guaranteed (rare)
}
```

**Pattern: Type Switches**:

```go
package main

import (
    "fmt"
    // => Standard library for output
    "io"
    // => Standard library for I/O types
)

func describe(val interface{}) string {
    // => val is interface{} (any type)
    // => Empty interface accepts any value
    // => Requires type switching to access concrete type

    switch v := val.(type) {
    // => v is val narrowed to matched type in each case
    // => val.(type) only valid in type switch

    case string:
        // => v is string in this case
        return fmt.Sprintf("String of length %d", len(v))
        // => len(v) works (v is string)
        // => Would not compile outside this case

    case int:
        // => v is int in this case
        return fmt.Sprintf("Integer: %d", v)
        // => v treated as int

    case io.Reader:
        // => v is io.Reader interface
        // => Matches any type implementing Read method
        return "Implements io.Reader"
        // => Interface matching (structural typing)

    default:
        // => No match: v has original interface{} type
        return fmt.Sprintf("Unknown type: %T", v)
        // => %T prints type name
        // => Output: Unknown type: bool (for boolean values)
    }
}

func main() {
    fmt.Println(describe("hello"))
    // => Output: String of length 5

    fmt.Println(describe(42))
    // => Output: Integer: 42

    fmt.Println(describe(true))
    // => Output: Unknown type: bool
}
```

**Why type assertions/switches matter**:

- Handle different types dynamically (deserialization, plugin systems)
- Work with `interface{}` safely (before generics)
- Implement type-specific optimizations
- Safe runtime type narrowing with `ok` idiom

**Trade-offs**:

| Approach            | Type Safety    | Flexibility | Performance                |
| ------------------- | -------------- | ----------- | -------------------------- |
| Type assertions     | Runtime checks | High        | Fast (type check overhead) |
| Generics (Go 1.18+) | Compile-time   | Medium      | Zero overhead              |
| Interface{}         | Runtime only   | Maximum     | Allocation overhead        |

## Production Framework: Generics (Go 1.18+)

Go 1.18 introduced type parameters (generics) for compile-time type safety without runtime overhead.

**Pattern: Generic Functions**:

```go
package main

import "fmt"

// STANDARD LIBRARY APPROACH (before generics): interface{}
func findInterface(slice []interface{}, target interface{}) int {
    // => slice is []interface{} (any type)
    // => Requires type assertions to use values
    // => Allocates on heap (interface conversion)

    for i, v := range slice {
        if v == target {
            // => Comparison works (interface equality)
            // => But type-unsafe (compares any to any)
            return i
        }
    }
    return -1
    // => Returns -1 if not found
}

// PRODUCTION APPROACH (Go 1.18+): generics
func find[T comparable](slice []T, target T) int {
    // => [T comparable] is type parameter
    // => comparable constraint: T must support == and !=
    // => T resolved at compile time (monomorphization)
    // => No interface conversion (zero overhead)

    for i, v := range slice {
        // => v is type T (known at compile time)
        // => == works (comparable constraint)

        if v == target {
            // => Type-safe comparison (T == T)
            return i
        }
    }
    return -1
}

func main() {
    numbers := []int{1, 2, 3, 4, 5}
    // => numbers is []int

    index := find(numbers, 3)
    // => find[int] inferred from argument types
    // => Compiler generates optimized int version
    // => No runtime type checking or conversion

    fmt.Println(index)
    // => Output: 2 (index of 3 in slice)

    strings := []string{"a", "b", "c"}
    // => strings is []string

    strIndex := find(strings, "b")
    // => find[string] inferred
    // => Different specialized version generated
    // => Type-safe at compile time

    fmt.Println(strIndex)
    // => Output: 1
}
```

**Pattern: Generic Types**:

```go
package main

import "fmt"

// Generic stack implementation
type Stack[T any] struct {
    // => [T any] type parameter with any constraint
    // => any means no restrictions (accepts all types)
    // => T used throughout struct definition

    items []T
    // => items is slice of T
    // => Type resolved when Stack instantiated
}

func (s *Stack[T]) Push(item T) {
    // => Method receiver includes type parameter [T]
    // => item must be type T
    // => Type-safe push operation

    s.items = append(s.items, item)
    // => append works with []T
}

func (s *Stack[T]) Pop() (T, bool) {
    // => Returns T and bool
    // => T is zero value if stack empty

    if len(s.items) == 0 {
        var zero T
        // => zero is zero value of T
        // => 0 for numbers, "" for strings, nil for pointers

        return zero, false
        // => false indicates empty stack
    }

    item := s.items[len(s.items)-1]
    // => item is type T (last element)

    s.items = s.items[:len(s.items)-1]
    // => Remove last element (slice reslicing)

    return item, true
    // => true indicates success
}

func main() {
    // Integer stack
    intStack := &Stack[int]{}
    // => Stack[int] instantiated with int type
    // => items is []int internally

    intStack.Push(42)
    // => 42 must be int (compile-time check)
    intStack.Push(7)

    val, ok := intStack.Pop()
    // => val is int (not interface{})
    fmt.Printf("Popped: %d, ok: %v\n", val, ok)
    // => Output: Popped: 7, ok: true

    // String stack (different type)
    strStack := &Stack[string]{}
    // => Stack[string] is separate type
    // => Cannot mix with Stack[int]

    strStack.Push("hello")
    // => "hello" must be string

    // COMPILE ERROR: cannot use 42 (int) as string
    // strStack.Push(42)
    // => Type safety enforced at compile time
}
```

**Custom constraints**:

```go
package main

import "fmt"

// Custom constraint: numeric types
type Number interface {
    // => Interface as constraint
    // => Types must match one of listed types

    int | int64 | float64
    // => Union of types (Go 1.18+)
    // => Constraint allows int OR int64 OR float64
}

func sum[T Number](values []T) T {
    // => T constrained to Number types
    // => Only int, int64, float64 allowed

    var total T
    // => total is zero value of T (0)

    for _, v := range values {
        // => v is type T

        total += v
        // => += works because Number types support it
        // => Would not compile for unsupported operations
    }

    return total
}

func main() {
    ints := []int{1, 2, 3}
    fmt.Println(sum(ints))
    // => Output: 6 (sum[int] inferred)

    floats := []float64{1.5, 2.5, 3.5}
    fmt.Println(sum(floats))
    // => Output: 7.5 (sum[float64] inferred)

    // COMPILE ERROR: string does not satisfy Number
    // strings := []string{"a", "b"}
    // sum(strings)
    // => Constraint violation caught at compile time
}
```

## Trade-offs: When to Use Each

**Comparison table**:

| Approach            | Type Safety  | Runtime Cost | Flexibility | Use Case                                    |
| ------------------- | ------------ | ------------ | ----------- | ------------------------------------------- |
| **Named types**     | Compile-time | Zero         | Low         | Domain modeling (UserID, Currency)          |
| **Type assertions** | Runtime      | Type check   | High        | Dynamic typing (plugins, deserialization)   |
| **Generics**        | Compile-time | Zero         | Medium      | Reusable algorithms (containers, utilities) |
| **interface{}**     | Runtime      | Allocation   | Maximum     | Last resort (pre-generics compatibility)    |

**When to use named types**:

- Domain modeling: `type UserID int64` prevents mixing IDs
- Units: `type Celsius float64` vs `type Fahrenheit float64`
- Type safety for primitives: `type Password string` vs plain `string`
- When you need distinct types for same underlying type

**When to use type assertions**:

- Handling unknown types (JSON deserialization)
- Plugin systems with interface boundaries
- Type-specific optimizations (checking for `io.WriterTo`)
- Working with `interface{}` before generics

**When to use generics**:

- Container types (Stack, Queue, Tree)
- Utility functions (Map, Filter, Reduce)
- Type-safe APIs without interface{} overhead
- When interface{} would require runtime type assertions

**When to use interface{}**:

- Backward compatibility (APIs before Go 1.18)
- Truly heterogeneous collections (different types)
- Integration with reflection-based libraries
- Last resort when generics too restrictive

## Production Best Practices

**Named types for domain safety**:

```go
type UserID string      // Not plain string
type OrderID int64      // Not plain int64
type Amount float64     // Not plain float64

func chargeUser(userID UserID, amount Amount) error {
    // => Cannot pass string or float64 by accident
    // => Compiler enforces domain boundaries
    // ...
}
```

**Safe type assertions with ok idiom**:

```go
// GOOD: two-value assertion (safe)
if file, ok := reader.(*os.File); ok {
    // Use file
}

// BAD: single-value assertion (panics on failure)
file := reader.(*os.File)  // Avoid unless type guaranteed
```

**Prefer generics over interface{} (Go 1.18+)**:

```go
// BEFORE Go 1.18: interface{} (allocates)
func keys(m map[string]interface{}) []string { /* ... */ }

// AFTER Go 1.18: generics (zero overhead)
func keys[K comparable, V any](m map[K]V) []K { /* ... */ }
```

**Constraints for meaningful APIs**:

```go
// TOO PERMISSIVE: any allows all types
func process[T any](val T) { /* ... */ }

// BETTER: constraint expresses requirements
func process[T io.Reader](val T) { /* ... */ }
```

## Summary

Go's type system prioritizes simplicity and compile-time safety. Named types enable domain modeling, type assertions handle dynamic scenarios, and generics provide zero-cost abstractions. Start with standard library patterns (named types, interfaces), understand when runtime type checking necessary (assertions), then apply generics for reusable, type-safe code.

**Key takeaways**:

- Named types create domain boundaries at compile time
- Type assertions enable dynamic typing safely with `ok` idiom
- Generics eliminate interface{} overhead with compile-time specialization
- Choose approach based on safety requirements and performance constraints
