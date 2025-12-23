---
title: "Generics Patterns"
date: 2025-12-21T00:00:00+07:00
draft: false
weight: 1000024
description: "Practical techniques for using Go 1.18+ generics with type parameters, constraints, and generic data structures"
---

## Problem

Before Go 1.18, code reuse required interfaces and type assertions, leading to runtime errors and loss of type safety.

## Solution

### 1. Generic Functions

```go
func Map[T any, U any](slice []T, fn func(T) U) []U {
    result := make([]U, len(slice))
    for i, v := range slice {
        result[i] = fn(v)
    }
    return result
}

// Usage
numbers := []int{1, 2, 3, 4}
doubled := Map(numbers, func(n int) int { return n * 2 })
// [2, 4, 6, 8]

strings := Map(numbers, func(n int) string { return fmt.Sprintf("%d", n) })
// ["1", "2", "3", "4"]
```

### 2. Generic Data Structures

```go
type Stack[T any] struct {
    items []T
}

func NewStack[T any]() *Stack[T] {
    return &Stack[T]{items: make([]T, 0)}
}

func (s *Stack[T]) Push(item T) {
    s.items = append(s.items, item)
}

func (s *Stack[T]) Pop() (T, bool) {
    if len(s.items) == 0 {
        var zero T
        return zero, false
    }
    item := s.items[len(s.items)-1]
    s.items = s.items[:len(s.items)-1]
    return item, true
}

// Usage
intStack := NewStack[int]()
intStack.Push(1)
intStack.Push(2)
val, _ := intStack.Pop()  // val = 2
```

### 3. Type Constraints

```go
func Min[T constraints.Ordered](a, b T) T {
    if a < b {
        return a
    }
    return b
}

// Custom constraint
type Number interface {
    int | int64 | float64
}

func Sum[T Number](numbers []T) T {
    var total T
    for _, n := range numbers {
        total += n
    }
    return total
}
```

## How It Works

### Type Parameter Instantiation

When calling a generic function, Go's type inference determines type arguments:

1. **Explicit Type Arguments**: `Map[int, string](numbers, fn)` - manually specify types
2. **Type Inference**: `Map(numbers, fn)` - compiler infers types from arguments
3. **Partial Inference**: Not supported - must specify all or none
4. **Constraint Checking**: Compiler verifies type arguments satisfy constraints
5. **Monomorphization**: Separate compiled code for each type instantiation

### Constraint Resolution

Type constraints define allowed operations on type parameters:

**Built-in constraints**:

- `any` (alias for `interface{}`) - no restrictions
- `comparable` - supports `==` and `!=` operators
- `constraints.Ordered` - supports `<`, `<=`, `>`, `>=` (from golang.org/x/exp/constraints)

**Union constraints**:

```go
type Integer interface {
    int | int8 | int16 | int32 | int64
}
```

**Method constraints**:

```go
type Stringer interface {
    String() string
}

func Print[T Stringer](v T) {
    fmt.Println(v.String())  // OK - constraint guarantees method
}
```

### Generic Type Instantiation

Generic types create new types when instantiated:

```go
type Box[T any] struct {
    value T
}

// These are different types:
var intBox Box[int]      // Box instantiated with int
var strBox Box[string]   // Box instantiated with string

// Cannot assign between them:
intBox = strBox  // Compile error!
```

Each instantiation is a distinct type with its own method set.

### Type Inference Algorithm

Go's type inference follows these rules:

1. **Unification**: Match formal and actual parameter types
2. **Constraint Satisfaction**: Verify inferred types satisfy constraints
3. **Substitution**: Replace type parameters with inferred types
4. **Verification**: Check result is well-typed

**Example**:

```go
func Filter[T any](slice []T, predicate func(T) bool) []T

numbers := []int{1, 2, 3, 4}
evens := Filter(numbers, func(n int) bool { return n%2 == 0 })

// Inference:
// 1. slice is []int, so T must be int
// 2. predicate must be func(int) bool
// 3. Result type is []int
```

### Zero Value Handling

Generic functions must handle zero values correctly:

```go
func Pop[T any]() T {
    var zero T  // Zero value of type parameter
    return zero
}

// Different zero values:
Pop[int]()       // Returns 0
Pop[string]()    // Returns ""
Pop[*int]()      // Returns nil
Pop[[]int]()     // Returns nil
```

## Variations

### 1. Generic Interfaces

Define interfaces with type parameters:

```go
type Container[T any] interface {
    Add(item T)
    Remove() (T, bool)
    Size() int
}

type ListContainer[T any] struct {
    items []T
}

func (lc *ListContainer[T]) Add(item T) {
    lc.items = append(lc.items, item)
}

func (lc *ListContainer[T]) Remove() (T, bool) {
    if len(lc.items) == 0 {
        var zero T
        return zero, false
    }
    item := lc.items[0]
    lc.items = lc.items[1:]
    return item, true
}

func (lc *ListContainer[T]) Size() int {
    return len(lc.items)
}

// Usage with interface:
func ProcessContainer[T any](c Container[T], items []T) {
    for _, item := range items {
        c.Add(item)
    }
}
```

**Trade-offs**: More flexible but adds interface overhead.

### 2. Multi-Parameter Type Constraints

Constrain multiple type parameters with relationships:

```go
type Mapper[T any, U any] interface {
    Map(T) U
}

func Transform[T any, U any, M Mapper[T, U]](
    items []T,
    mapper M,
) []U {
    result := make([]U, len(items))
    for i, item := range items {
        result[i] = mapper.Map(item)
    }
    return result
}

// Implementation:
type IntToString struct{}

func (IntToString) Map(n int) string {
    return fmt.Sprintf("%d", n)
}

// Usage:
nums := []int{1, 2, 3}
strs := Transform(nums, IntToString{})
```

**Trade-offs**: Type-safe transformations but more verbose.

### 3. Generic Methods (Type Parameter on Receiver)

Methods can use type parameters from receiver:

```go
type Pair[T, U any] struct {
    First  T
    Second U
}

func (p Pair[T, U]) Swap() Pair[U, T] {
    return Pair[U, T]{
        First:  p.Second,
        Second: p.First,
    }
}

// Usage:
p := Pair[int, string]{First: 1, Second: "one"}
swapped := p.Swap()  // Pair[string, int]{First: "one", Second: 1}
```

**Trade-offs**: Elegant for type-safe data structures but cannot add type parameters to methods (must be on receiver).

### 4. Comparable Constraint for Map Keys

Use `comparable` for generic maps:

```go
type Cache[K comparable, V any] struct {
    data map[K]V
}

func NewCache[K comparable, V any]() *Cache[K, V] {
    return &Cache[K, V]{
        data: make(map[K]V),
    }
}

func (c *Cache[K, V]) Set(key K, value V) {
    c.data[key] = value
}

func (c *Cache[K, V]) Get(key K) (V, bool) {
    v, ok := c.data[key]
    return v, ok
}

// Usage:
intCache := NewCache[int, string]()
intCache.Set(1, "one")

strCache := NewCache[string, []int]()
strCache.Set("key", []int{1, 2, 3})
```

**Trade-offs**: Enforces key comparability at compile time but limits key types.

### 5. Approximate Constraints with ~

Use `~` for underlying type constraints:

```go
type MyInt int

type Integer interface {
    ~int | ~int8 | ~int16 | ~int32 | ~int64
}

func Add[T Integer](a, b T) T {
    return a + b
}

// Works with MyInt (underlying type is int):
var x MyInt = 5
var y MyInt = 10
result := Add(x, y)  // OK with ~ constraint

// Without ~ in constraint:
type IntegerStrict interface {
    int | int8 | int16 | int32 | int64
}

func AddStrict[T IntegerStrict](a, b T) T {
    return a + b
}

// AddStrict(x, y)  // Error! MyInt not in union
```

**Trade-offs**: `~` allows named types but may accept unintended types.

## Common Pitfalls

### 1. Overusing Generics

**Problem**: Using generics where interfaces or simple functions suffice:

```go
// Bad: Unnecessary generic
func PrintGeneric[T any](v T) {
    fmt.Println(v)  // No type-specific operations!
}

// Worse: Generic wrapper for built-in
func LenGeneric[T any](slice []T) int {
    return len(slice)  // Built-in len already works!
}
```

**Solution**: Use generics only when you need type safety for operations:

```go
// Good: Type-safe operation on constrained types
func Max[T constraints.Ordered](a, b T) T {
    if a > b {
        return a
    }
    return b
}

// Good: Generic data structure with type safety
type Queue[T any] struct {
    items []T
}
```

### 2. Forgetting Zero Values

**Problem**: Not handling zero values when type parameter is unknown:

```go
// Bad: Doesn't handle empty case
func First[T any](slice []T) T {
    return slice[0]  // Panics if empty!
}

// Bad: Returns wrong zero value
func FirstOrDefault[T any](slice []T, defaultVal T) T {
    if len(slice) == 0 {
        return nil  // Compile error! nil not valid for all T
    }
    return slice[0]
}
```

**Solution**: Properly handle zero values and empty cases:

```go
// Good: Returns zero value explicitly
func First[T any](slice []T) (T, bool) {
    if len(slice) == 0 {
        var zero T
        return zero, false
    }
    return slice[0], true
}

// Good: Use provided default
func FirstOrDefault[T any](slice []T, defaultVal T) T {
    if len(slice) == 0 {
        return defaultVal
    }
    return slice[0]
}
```

### 3. Constraint Not Matching Usage

**Problem**: Type parameter doesn't satisfy constraint requirements:

```go
// Bad: Constraint doesn't match operations
func Sum[T any](numbers []T) T {
    var total T
    for _, n := range numbers {
        total += n  // Error! any doesn't support +
    }
    return total
}

// Bad: Too restrictive constraint
func Process[T int](value T) {
    // Only works with int, not int64, float64, etc.
}
```

**Solution**: Match constraint to actual operations needed:

```go
// Good: Constraint allows addition
type Numeric interface {
    int | int64 | float64 | float32
}

func Sum[T Numeric](numbers []T) T {
    var total T
    for _, n := range numbers {
        total += n  // OK - constraint allows +
    }
    return total
}

// Good: Broader constraint
type Number interface {
    ~int | ~int64 | ~float64 | ~float32
}

func Process[T Number](value T) T {
    return value * 2  // Works with all numeric types
}
```

### 4. Trying to Use Type Parameters in Non-Generic Context

**Problem**: Attempting to use type parameters outside their scope:

```go
// Bad: Type parameter in package-level variable
type Container[T any] struct {
    value T
}

var globalContainer Container[T]  // Error! T not in scope

// Bad: Type parameter in non-generic method
func (c Container[int]) GenericMethod[U any](u U) {
    // Error! Cannot add type parameters to methods
}
```

**Solution**: Keep type parameters in proper scope:

```go
// Good: Instantiate at package level
var globalIntContainer Container[int]
var globalStrContainer Container[string]

// Good: Type parameters on receiver only
type Container[T any] struct {
    value T
}

func (c Container[T]) Process(fn func(T) T) Container[T] {
    return Container[T]{value: fn(c.value)}
}
```

### 5. Ignoring Type Inference Limitations

**Problem**: Expecting inference in cases where it doesn't work:

```go
// Bad: Cannot infer return type
func MakeSlice[T any]() []T {
    return make([]T, 0)
}

result := MakeSlice()  // Error! Cannot infer T

// Bad: Ambiguous inference
func Convert[T any, U any](v T) U {
    // Complex conversion logic
}

x := Convert(42)  // Error! Cannot infer U
```

**Solution**: Provide explicit type arguments when inference fails:

```go
// Good: Explicit type argument
result := MakeSlice[int]()  // OK

// Good: Additional parameter for inference
func Convert[T any, U any](v T, zero U) U {
    // Conversion logic using zero as type hint
    // ...
}

x := Convert(42, "")  // Infers U as string

// Better: Redesign to avoid inference issues
func ConvertToString[T any](v T) string {
    return fmt.Sprintf("%v", v)
}
```

### 6. Performance Assumptions

**Problem**: Assuming generics have zero overhead:

```go
// Bad: Assuming no cost
func Process[T any](items []T) {
    // Large generic function
    // Compiler generates separate code for each T
    // Can increase binary size
}

// Bad: Generic in hot path without benchmarking
func HotPathOperation[T comparable](a, b T) bool {
    return a == b  // May be slower than type-specific comparison
}
```

**Solution**: Benchmark and profile generic code:

```go
// Good: Benchmark generic vs non-generic
func BenchmarkGenericSum(b *testing.B) {
    numbers := make([]int, 1000)
    for i := 0; i < b.N; i++ {
        _ = Sum(numbers)  // Generic version
    }
}

func BenchmarkDirectSum(b *testing.B) {
    numbers := make([]int, 1000)
    for i := 0; i < b.N; i++ {
        total := 0
        for _, n := range numbers {
            total += n  // Direct version
        }
    }
}

// Use profiling to identify hot spots
// go test -bench=. -cpuprofile=cpu.prof
```

## Related Patterns

**Related Tutorial**: See [Advanced Tutorial - Generics](/en/learn/software-engineering/programming-language/golang/tutorials/advanced#generics) for generic fundamentals.

**Related How-To**: See [Design Interfaces Properly](/en/learn/software-engineering/programming-language/golang/how-to/design-interfaces-properly) for interface design, [Handle Errors Effectively](/en/learn/software-engineering/programming-language/golang/how-to/handle-errors-effectively) for generic error handling.

**Related Cookbook**: See Cookbook recipes "Generic Data Structures", "Type Constraints", "Generic Algorithms" for ready-to-use generic patterns.
