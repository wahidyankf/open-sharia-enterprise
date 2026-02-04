---
title: "Generics"
date: 2026-02-04T00:00:00+07:00
draft: false
description: "Type parameters, constraints, and when to use generics vs interfaces in Go"
weight: 10000030
tags: ["golang", "generics", "type-parameters", "constraints", "production"]
---

## Why Generics Matter

Go 1.18 introduced type parameters (generics) enabling compile-time type safety for algorithms and data structures without interface{} overhead. Understanding when to use generics vs interfaces, how to write constraints, and the trade-offs prevents over-abstraction while enabling reusable, type-safe code.

**Core benefits**:

- **Compile-time type safety**: Catch type errors before runtime
- **Zero overhead**: No interface conversion or type assertions
- **Code reuse**: Write once, use with multiple types
- **Better performance**: Avoid boxing/unboxing of interface{}

**Problem**: Before Go 1.18, developers used interface{} for generic code, requiring runtime type assertions, heap allocations, and losing type safety. Generics eliminate these costs.

**Solution**: Understand interface{} limitations (allocations, runtime checks), then apply type parameters with appropriate constraints for type-safe, zero-overhead abstractions.

## Before Generics: interface{} Approach

Before Go 1.18, interface{} was the only option for generic algorithms.

**Pattern: interface{} for Generic Code (Pre-1.18)**:

```go
package main

import "fmt"

// Generic stack using interface{} (before Go 1.18)
type Stack struct {
    items []interface{}
    // => []interface{} accepts any type
    // => Each element allocated on heap (boxing)
    // => Type information lost
}

func (s *Stack) Push(item interface{}) {
    // => item is interface{} (any type)
    // => Boxing: value wrapped in interface
    // => Heap allocation occurs

    s.items = append(s.items, item)
    // => Append interface{} value
}

func (s *Stack) Pop() (interface{}, bool) {
    // => Returns interface{} (requires type assertion)
    // => Caller must know expected type

    if len(s.items) == 0 {
        return nil, false
        // => nil and false indicate empty stack
    }

    item := s.items[len(s.items)-1]
    // => item is interface{}

    s.items = s.items[:len(s.items)-1]
    // => Remove last element

    return item, true
    // => Return interface{} value
}

func main() {
    stack := &Stack{}

    stack.Push(42)
    // => 42 boxed into interface{}
    // => Heap allocation (overhead)

    stack.Push("hello")
    // => "hello" boxed into interface{}
    // => Type mixing allowed (no compile-time safety)

    // Pop requires type assertion
    val, ok := stack.Pop()
    // => val is interface{} (unknown type)

    if ok {
        // Type assertion required (runtime check)
        if str, ok := val.(string); ok {
            // => Type assertion to string
            // => ok is true if val is string

            fmt.Println("String:", str)
            // => Output: String: hello
        } else if num, ok := val.(int); ok {
            // => Type assertion to int
            fmt.Println("Int:", num)
        }
    }
}
```

**Limitations of interface{} approach**:

- **Heap allocations**: Boxing values into interface{} allocates
- **Runtime type checks**: Type assertions happen at runtime
- **Type mixing**: Can mix int and string (no compile-time safety)
- **Verbose**: Requires type assertions everywhere
- **Performance**: Slower than typed code (allocations, indirection)

## Production Framework: Type Parameters (Go 1.18+)

Go 1.18 introduced type parameters for compile-time generic code.

**Pattern: Generic Stack with Type Parameters**:

```go
package main

import "fmt"

// Generic stack using type parameters (Go 1.18+)
type Stack[T any] struct {
    // => [T any] is type parameter
    // => T is placeholder for concrete type
    // => any is constraint (accepts all types)

    items []T
    // => items is slice of T (not interface{})
    // => No boxing: direct value storage
}

func (s *Stack[T]) Push(item T) {
    // => Method with type parameter [T]
    // => item must be type T (compile-time checked)

    s.items = append(s.items, item)
    // => Append T value (no interface conversion)
    // => No heap allocation (unless T is pointer)
}

func (s *Stack[T]) Pop() (T, bool) {
    // => Returns T directly (no interface{})
    // => Type-safe return value

    if len(s.items) == 0 {
        var zero T
        // => zero is zero value of T
        // => 0 for int, "" for string, nil for pointers

        return zero, false
    }

    item := s.items[len(s.items)-1]
    // => item is type T (known at compile time)

    s.items = s.items[:len(s.items)-1]
    return item, true
}

func main() {
    // Create integer stack
    intStack := &Stack[int]{}
    // => Stack[int] instantiated with int type
    // => items is []int internally
    // => Type-safe: only int allowed

    intStack.Push(42)
    // => 42 must be int (compile-time check)
    // => No boxing (stored as int directly)

    intStack.Push(7)

    val, ok := intStack.Pop()
    // => val is int (not interface{})
    // => No type assertion needed

    if ok {
        fmt.Printf("Popped: %d\n", val)
        // => Output: Popped: 7
        // => val is int (type-safe)
    }

    // COMPILE ERROR: cannot use string as int
    // intStack.Push("hello")
    // => Type safety enforced at compile time

    // Create string stack (different type)
    strStack := &Stack[string]{}
    // => Stack[string] is separate type
    // => items is []string internally

    strStack.Push("hello")
    // => Type-safe: must be string

    str, _ := strStack.Pop()
    // => str is string (no assertion)
    fmt.Println(str)
    // => Output: hello
}
```

**Why type parameters matter**:

- **Compile-time safety**: Type errors caught before deployment
- **Zero overhead**: No interface conversion or allocations
- **Type inference**: Compiler infers type parameters from arguments
- **Multiple instantiations**: Stack[int] and Stack[string] are distinct types

## Constraints: Restricting Type Parameters

Constraints limit which types can be used as type parameters.

**Pattern: Comparable Constraint**:

```go
package main

import "fmt"

// Find element in slice (requires comparable types)
func find[T comparable](slice []T, target T) int {
    // => [T comparable] constrains T to comparable types
    // => comparable: types supporting == and !=
    // => Includes: int, string, float64, pointers, structs (if all fields comparable)
    // => Excludes: slices, maps, functions

    for i, v := range slice {
        // => v is type T

        if v == target {
            // => == works because T is comparable
            // => Would not compile without comparable constraint
            return i
        }
    }

    return -1
    // => Not found
}

func main() {
    ints := []int{1, 2, 3, 4, 5}
    index := find(ints, 3)
    // => find[int] inferred from argument types
    // => int is comparable (allowed)

    fmt.Println("Index:", index)
    // => Output: Index: 2

    strings := []string{"a", "b", "c"}
    strIndex := find(strings, "b")
    // => find[string] inferred
    // => string is comparable

    fmt.Println("Index:", strIndex)
    // => Output: Index: 1

    // COMPILE ERROR: slice is not comparable
    // slices := [][]int{{1}, {2}, {3}}
    // find(slices, []int{2})
    // => []int is not comparable (slices don't support ==)
}
```

**Pattern: Custom Constraints**:

```go
package main

import "fmt"

// Custom constraint: numeric types
type Number interface {
    // => Interface defines constraint
    // => Union of allowed types

    int | int64 | float64
    // => | is type union (Go 1.18+)
    // => T must be int OR int64 OR float64
    // => Constraint limits type parameter
}

func sum[T Number](values []T) T {
    // => T constrained to Number types
    // => Only int, int64, float64 allowed

    var total T
    // => total is zero value of T (0)

    for _, v := range values {
        total += v
        // => += works (Number types support it)
        // => Would not compile for unsupported types
    }

    return total
}

func main() {
    ints := []int{1, 2, 3, 4, 5}
    fmt.Println("Sum:", sum(ints))
    // => Output: Sum: 15
    // => sum[int] inferred

    floats := []float64{1.5, 2.5, 3.5}
    fmt.Println("Sum:", sum(floats))
    // => Output: Sum: 7.5
    // => sum[float64] inferred

    // COMPILE ERROR: string not in Number constraint
    // strings := []string{"a", "b"}
    // sum(strings)
    // => Constraint violation caught at compile time
}
```

**Pattern: Interface Constraints**:

```go
package main

import (
    "fmt"
    "io"
    "strings"
)

// Constraint using existing interface
func readAll[T io.Reader](r T) (string, error) {
    // => T constrained to io.Reader interface
    // => T must have Read method
    // => More specific than any constraint

    buffer := make([]byte, 1024)
    n, err := r.Read(buffer)
    // => r.Read works (T implements io.Reader)

    if err != nil && err != io.EOF {
        return "", err
    }

    return string(buffer[:n]), nil
}

func main() {
    reader := strings.NewReader("hello generics")
    // => strings.Reader implements io.Reader

    content, err := readAll(reader)
    // => readAll[*strings.Reader] inferred
    // => *strings.Reader satisfies io.Reader constraint

    if err != nil {
        fmt.Println("Error:", err)
        return
    }

    fmt.Println("Content:", content)
    // => Output: Content: hello generics
}
```

**Built-in constraints** (Go 1.18+):

```go
// any: accepts all types (alias for interface{})
func print[T any](val T) { }

// comparable: types supporting == and !=
func contains[T comparable](slice []T, val T) bool { }

// Custom constraints: define your own
type Numeric interface {
    int | int64 | float64
}
```

## Trade-offs: Generics vs Interfaces

**Comparison table**:

| Approach        | Type Safety  | Performance                  | Flexibility | Use Case                   |
| --------------- | ------------ | ---------------------------- | ----------- | -------------------------- |
| **Generics**    | Compile-time | Zero overhead                | Medium      | Containers, algorithms     |
| **Interfaces**  | Compile-time | Interface call overhead      | High        | Polymorphism, dependencies |
| **interface{}** | Runtime      | Heap allocation + assertions | Maximum     | Last resort (pre-generics) |

**When to use generics**:

- Container types (Stack, Queue, Map, Set)
- Utility functions (Map, Filter, Reduce on slices)
- Algorithms (Sort, Search, Find)
- Type-safe APIs without interface{} overhead
- When interface{} would require type assertions

**When to use interfaces**:

- Dependency injection (Logger, Database)
- Polymorphism (io.Reader, http.Handler)
- Behavior abstraction (Notifier, Validator)
- Multiple implementations with different behaviors
- When runtime polymorphism needed

**When NOT to use generics**:

- Simple functions (type-specific often clearer)
- Over-abstraction (don't generic everything)
- When interfaces already provide needed flexibility
- Premature abstraction (wait for actual need)

## Production Best Practices

**Use generics for containers**:

```go
// GOOD: generic container
type Queue[T any] struct {
    items []T
}

// BAD: interface{} container (heap allocations)
type Queue struct {
    items []interface{}
}
```

**Use interfaces for dependencies**:

```go
// GOOD: interface for dependency
type Logger interface {
    Log(msg string)
}

func Process(logger Logger) { }

// BAD: generic for dependency (over-abstraction)
func Process[T Logger](logger T) { }
// Generics add no value here
```

**Prefer specific constraints over any**:

```go
// GOOD: specific constraint
func sum[T int | int64 | float64](values []T) T { }

// BAD: too permissive constraint
func sum[T any](values []T) T {
    // Can't use + operator on any
}
```

**Don't over-generic**:

```go
// GOOD: simple specific function
func sumInts(values []int) int { }

// BAD: unnecessary generic (adds complexity)
func sum[T int](values []T) T { }
// Generic version adds no value for single type
```

**Combine generics and interfaces when appropriate**:

```go
// Generic container with interface constraint
type Repository[T Entity] struct {
    // => T constrained to Entity interface
    // => Generic benefits + interface flexibility
    items []T
}

type Entity interface {
    ID() string
}
```

## Summary

Go 1.18 generics enable compile-time type safety without interface{} overhead. Type parameters eliminate heap allocations and runtime type assertions while maintaining zero-cost abstractions. Use generics for containers and algorithms, interfaces for dependencies and polymorphism, and constraints to express type requirements. Avoid over-abstraction by applying generics only when interface{} would require type assertions or when building reusable containers.

**Key takeaways**:

- Type parameters provide compile-time type safety
- Zero overhead (no boxing, no type assertions)
- Constraints limit allowed types (comparable, custom unions, interfaces)
- Use generics for containers/algorithms, interfaces for dependencies
- Avoid over-abstraction (don't generic everything)
- Combine generics and interfaces when both benefits needed
- Prefer specific constraints over any for better type safety
