---
title: "How to Use Slices and Maps Effectively"
date: 2025-12-17T10:00:00+07:00
draft: false
weight: 1000004
description: "Practical patterns for working with Go's built-in slice and map data structures efficiently"
tags: ["golang", "slices", "maps", "data-structures", "performance"]
categories: ["learn"]
---

## Problem

Slices and maps are Go's primary collection types, but misunderstanding their internals leads to bugs, performance issues, and memory leaks.

```go
// ❌ Modifying slice during iteration
for i, item := range items {
  items = append(items, item) // Infinite loop!
}

// ❌ Nil map panic
var users map[string]*User
users["john"] = &User{} // panic: assignment to entry in nil map
```

This guide shows patterns for using slices and maps effectively.

## Slice Fundamentals

### Understanding Slice Internals

Slices are references to underlying arrays with length and capacity.

```go
// Slice structure (conceptual)
type slice struct {
  ptr *array  // Pointer to underlying array
  len int     // Current length
  cap int     // Capacity of underlying array
}

// ✅ Understanding length vs capacity
s := make([]int, 3, 5) // len=3, cap=5
fmt.Println(len(s))    // 3
fmt.Println(cap(s))    // 5

// Visualization:
// [0, 0, 0, _, _]
//  ^len=3   ^cap=5

s = append(s, 4, 5) // Fits in capacity
// [0, 0, 0, 4, 5]
//  ^len=5 ^cap=5

s = append(s, 6) // Exceeds capacity - allocates new array!
// [0, 0, 0, 4, 5, 6, _, _, _, _]
//  ^len=6         ^cap=10 (doubles for cap < 256)
// Note: For cap ≥ 256, growth factor approaches 1.25x
```

**Key insights:**

- Slice header is small (24 bytes) - cheap to pass by value
- Modifying slice elements modifies underlying array
- Append may or may not allocate - depends on capacity
- Multiple slices can share the same underlying array

## Slice Solution Strategies

### Pre-allocate Slices When Size is Known

Avoid repeated allocations by pre-allocating to known capacity.

```go
// ❌ Starts empty, grows repeatedly
func ProcessItems(n int) []Result {
  var results []Result // nil slice, cap=0

  for i := 0; i < n; i++ {
    // Each append may allocate and copy
    results = append(results, process(i))
  }

  return results
}

// ✅ Pre-allocate with known size
func ProcessItems(n int) []Result {
  results := make([]Result, 0, n) // len=0, cap=n

  for i := 0; i < n; i++ {
    results = append(results, process(i)) // No allocations needed
  }

  return results
}

// ✅ Pre-allocate and use indexing
func ProcessItems(n int) []Result {
  results := make([]Result, n) // len=n, cap=n

  for i := 0; i < n; i++ {
    results[i] = process(i) // Direct assignment
  }

  return results
}
```

**Performance comparison:**

```go
// Performance comparison (n=10000):
// No pre-allocation:     Multiple allocations as slice grows
// Pre-allocated append:  ~3x faster, single allocation
// Pre-allocated index:   ~3-4x faster, single allocation
// (Results vary by hardware and Go version)
```

### Avoid Slice Modifications During Iteration

Don't modify slice length while iterating with range.

```go
// ❌ Modifying slice during iteration
items := []int{1, 2, 3}
for i, item := range items {
  items = append(items, item) // Infinite loop or undefined behavior!
}

// ❌ Index-based iteration with changing length
for i := 0; i < len(items); i++ {
  items = append(items, items[i]) // Infinite loop!
}

// ✅ Copy to new slice
items := []int{1, 2, 3}
newItems := make([]int, 0, len(items)*2)
for _, item := range items {
  newItems = append(newItems, item, item) // Duplicate each
}
items = newItems

// ✅ Iterate backwards when removing
items := []int{1, 2, 3, 4, 5}
for i := len(items) - 1; i >= 0; i-- {
  if items[i]%2 == 0 {
    items = append(items[:i], items[i+1:]...) // Remove even numbers
  }
}
```

### Copying Slices Correctly

Use copy() or explicit slicing to avoid sharing underlying arrays.

```go
// ❌ Both slices share underlying array
original := []int{1, 2, 3, 4, 5}
slice := original // Same underlying array
slice[0] = 99
fmt.Println(original) // [99, 2, 3, 4, 5] - modified!

// ✅ Deep copy with copy()
original := []int{1, 2, 3, 4, 5}
copied := make([]int, len(original))
copy(copied, original)
copied[0] = 99
fmt.Println(original) // [1, 2, 3, 4, 5] - unchanged

// ✅ Append to nil creates independent copy
original := []int{1, 2, 3, 4, 5}
copied := append([]int(nil), original...)
copied[0] = 99
fmt.Println(original) // [1, 2, 3, 4, 5] - unchanged
```

**Partial copies:**

```go
// ✅ Copy subset
original := []int{1, 2, 3, 4, 5}
subset := make([]int, 3)
copy(subset, original[1:4]) // [2, 3, 4]

// ✅ Copy with different size
src := []int{1, 2, 3, 4, 5}
dst := make([]int, 3)
n := copy(dst, src) // Copies min(len(dst), len(src)) elements
fmt.Println(dst, n) // [1, 2, 3], 3
```

### Removing Elements from Slices

Several patterns for removing elements efficiently.

```go
// ✅ Remove by index (preserves order)
func Remove(slice []int, index int) []int {
  return append(slice[:index], slice[index+1:]...)
}

items := []int{1, 2, 3, 4, 5}
items = Remove(items, 2) // [1, 2, 4, 5]

// ✅ Remove by index (doesn't preserve order, faster)
func RemoveFast(slice []int, index int) []int {
  slice[index] = slice[len(slice)-1] // Move last to index
  return slice[:len(slice)-1]         // Truncate
}

items := []int{1, 2, 3, 4, 5}
items = RemoveFast(items, 2) // [1, 2, 5, 4] - last element moved to index 2

// ✅ Filter (remove by condition)
func Filter(slice []int, keep func(int) bool) []int {
  result := slice[:0] // Reuse underlying array
  for _, item := range slice {
    if keep(item) {
      result = append(result, item)
    }
  }
  return result
}

items := []int{1, 2, 3, 4, 5}
items = Filter(items, func(x int) bool { return x%2 == 1 })
// [1, 3, 5]
```

### Avoid Memory Leaks with Large Slices

Be careful when keeping references to large underlying arrays.

```go
// ❌ Memory leak - entire array kept in memory
func FindFirst10(data []byte) []byte {
  // data might be 1GB, but we only want 10 bytes
  return data[:10] // Still references entire 1GB array!
}

// ✅ Copy to break reference
func FindFirst10(data []byte) []byte {
  result := make([]byte, 10)
  copy(result, data[:10])
  return result // Original data can be garbage collected
}

// ❌ Leak when removing from large slice
bigSlice := make([]BigStruct, 1000000)
// ... populate bigSlice ...
bigSlice = bigSlice[1:] // Removed first element, but array still 1M elements!

// ✅ Manually clear references for GC
for i := range bigSlice {
  bigSlice[i] = BigStruct{} // Zero out for GC
}
bigSlice = bigSlice[1:]
```

## Map Fundamentals

### Understanding Map Internals

Maps are hash tables that grow dynamically.

```go
// ✅ Map characteristics
m := make(map[string]int)

// Safe operations on nil map (reads only)
var nilMap map[string]int
_, ok := nilMap["key"] // Safe: returns zero value, false
fmt.Println(len(nilMap)) // Safe: returns 0
for k, v := range nilMap {} // Safe: iterates zero times

// ❌ Writes to nil map panic
nilMap["key"] = 1 // panic: assignment to entry in nil map

// ✅ Must initialize before writing
m := make(map[string]int)
m["key"] = 1 // Safe
```

## Map Solution Strategies

### Pre-allocate Maps with Known Size

Avoid rehashing by pre-allocating capacity.

```go
// ❌ Starts empty, grows repeatedly
func BuildMap(n int) map[int]string {
  m := make(map[int]string) // Empty map

  for i := 0; i < n; i++ {
    m[i] = fmt.Sprintf("value%d", i) // May trigger rehashing
  }

  return m
}

// ✅ Pre-allocate with expected size
func BuildMap(n int) map[int]string {
  m := make(map[int]string, n) // Pre-allocated

  for i := 0; i < n; i++ {
    m[i] = fmt.Sprintf("value%d", i) // No rehashing
  }

  return m
}
```

**Performance impact:**

```go
// Benchmark results (n=10000)
// No pre-allocation:  1.2ms, multiple rehashes
// Pre-allocated:      0.8ms, no rehashing
```

### Check Map Existence with Two-Value Assignment

Always check if key exists before using value.

```go
// ❌ Cannot distinguish "not found" from "zero value"
count := counts[word] // Returns 0 if not found or if value is 0
count++

// ✅ Check existence
if count, ok := counts[word]; ok {
  counts[word] = count + 1
} else {
  counts[word] = 1
}

// ✅ Simpler pattern with zero value
counts[word]++ // Zero value (0) works for increment

// ✅ Check before delete
if _, ok := m[key]; ok {
  delete(m, key)
}

// ✅ No need to check - delete is no-op if key doesn't exist
delete(m, key) // Safe even if key doesn't exist
```

### Maps are Not Safe for Concurrent Access

Protect maps with mutexes for concurrent access.

```go
// ❌ Race condition - concurrent map access
var cache map[string]string

func Get(key string) string {
  return cache[key] // Race!
}

func Set(key, value string) {
  cache[key] = value // Race!
}

// ✅ Use sync.RWMutex
type SafeCache struct {
  mu    sync.RWMutex
  items map[string]string
}

func (c *SafeCache) Get(key string) (string, bool) {
  c.mu.RLock()
  defer c.mu.RUnlock()
  val, ok := c.items[key]
  return val, ok
}

func (c *SafeCache) Set(key, value string) {
  c.mu.Lock()
  defer c.mu.Unlock()
  if c.items == nil {
    c.items = make(map[string]string)
  }
  c.items[key] = value
}

// ✅ Use sync.Map for specific high-concurrency scenarios
// Best when: keys are stable (write-once, read-many),
// or disjoint sets of keys accessed by different goroutines
var cache sync.Map

cache.Store("key", "value")
value, ok := cache.Load("key")
cache.Delete("key")

// Note: For most cases, sync.RWMutex with regular map is simpler
// Use sync.Map only when you have the specific access patterns above
```

### Map Iteration Order is Random

Don't depend on map iteration order.

```go
// ❌ Assuming order
m := map[string]int{"a": 1, "b": 2, "c": 3}
for k, v := range m {
  fmt.Println(k, v) // Order varies between runs!
}

// ✅ Sort keys for deterministic order
keys := make([]string, 0, len(m))
for k := range m {
  keys = append(keys, k)
}
sort.Strings(keys)

for _, k := range keys {
  fmt.Println(k, m[k]) // Always: a, b, c
}

// ✅ Use slice of pairs for ordered data
type Pair struct {
  Key   string
  Value int
}

ordered := []Pair{
  {"a", 1},
  {"b", 2},
  {"c", 3},
}

for _, p := range ordered {
  fmt.Println(p.Key, p.Value) // Guaranteed order
}
```

### Deleting Map Entries During Iteration

Safe to delete during iteration.

```go
// ✅ Safe to delete during iteration
users := map[string]*User{
  "john": {Name: "John", Active: false},
  "jane": {Name: "Jane", Active: true},
  "bob":  {Name: "Bob", Active: false},
}

for id, user := range users {
  if !user.Active {
    delete(users, id) // Safe during iteration
  }
}
// users now only contains "jane"

// ❌ Don't modify the map in unexpected ways
for k := range m {
  m[k+"_new"] = m[k] // Adds keys during iteration - undefined behavior!
}
```

### Use Map as Set

Maps work well as sets (only keys matter).

```go
// ✅ Set operations
type StringSet map[string]struct{} // struct{} uses zero memory

func NewSet(items ...string) StringSet {
  s := make(StringSet, len(items))
  for _, item := range items {
    s[item] = struct{}{}
  }
  return s
}

func (s StringSet) Add(item string) {
  s[item] = struct{}{}
}

func (s StringSet) Contains(item string) bool {
  _, ok := s[item]
  return ok
}

func (s StringSet) Remove(item string) {
  delete(s, item)
}

// Usage
set := NewSet("a", "b", "c")
set.Add("d")
if set.Contains("b") {
  fmt.Println("Found b")
}
set.Remove("a")
```

## Putting It All Together

When working with slices, understand that they're lightweight headers pointing to underlying arrays. This means slices are cheap to pass around, but modifications affect the shared underlying array. Pre-allocate slices when you know the size to avoid repeated allocations and copies. Use `make([]T, 0, capacity)` to start with zero length but reserved capacity.

Avoid modifying slice length while iterating - this leads to infinite loops or skipped elements. If you need to remove elements, either iterate backwards or filter into a new slice. When you need independent copies, use `copy()` or `append([]T(nil), slice...)` to avoid sharing the underlying array.

For maps, remember that nil maps are safe to read but panic on write. Always initialize with `make()` before adding entries. Pre-allocate maps when you know the approximate size to avoid rehashing as the map grows. Use the two-value form `value, ok := map[key]` to distinguish between "key not found" and "key has zero value".

Maps are not safe for concurrent access - protect them with sync.RWMutex or use sync.Map for high-concurrency scenarios. Never depend on map iteration order - it's deliberately randomized. Sort keys first if you need deterministic ordering. It's safe to delete map entries during iteration, but adding entries during iteration may or may not be visible in the current iteration.

## Common Mistakes to Avoid

**Don't append to a slice while iterating:**

```go
// ❌ Infinite loop
for _, item := range items {
  items = append(items, item)
}

// ✅ Copy first
original := items
for _, item := range original {
  items = append(items, process(item))
}
```

**Don't forget nil map panics on write:**

```go
// ❌ Panic
var m map[string]int
m["key"] = 1 // panic!

// ✅ Initialize first
m := make(map[string]int)
m["key"] = 1
```

**Don't assume slice append never allocates:**

```go
// ❌ May allocate if capacity exceeded
for i := 0; i < 1000; i++ {
  slice = append(slice, i) // Multiple allocations
}

// ✅ Pre-allocate
slice := make([]int, 0, 1000)
for i := 0; i < 1000; i++ {
  slice = append(slice, i) // One allocation
}
```

## Summary

Effective use of slices and maps requires understanding their internals and characteristics. Slices are lightweight headers with a pointer to an underlying array, length, and capacity. This design makes them cheap to pass around but means modifications affect the shared array. Pre-allocate slices when you know the size to avoid repeated allocations as the slice grows.

Be cautious when modifying slices during iteration - changing length while iterating leads to bugs. If you need to remove elements, iterate backwards or filter into a new slice. When you need independent copies, use copy() to avoid sharing the underlying array.

Maps are hash tables that grow dynamically through rehashing. Pre-allocate maps with expected size to minimize rehashing. Remember that nil maps are safe to read but panic on write - always initialize with make() before adding entries. Use the two-value form to distinguish between "not found" and "zero value".

Protect maps from concurrent access with mutexes - they're not safe for simultaneous reads and writes. Never depend on map iteration order, which is deliberately randomized. Sort keys first when you need deterministic ordering.

These collection types are fundamental to Go programming. Using them effectively - with appropriate pre-allocation, careful handling of nil values, and understanding of their performance characteristics - leads to code that's both correct and efficient.

## Related Content

- [Go Best Practices and Idioms](/en/learn/software-engineering/programming-language/golang/explanation/best-practices)
- [Common Go Anti-Patterns](/en/learn/software-engineering/programming-language/golang/explanation/anti-patterns)
- [How to Avoid Nil Panics](/en/learn/software-engineering/programming-language/golang/how-to/avoid-nil-panics)
- [How to Design Interfaces Properly](/en/learn/software-engineering/programming-language/golang/how-to/design-interfaces-properly)
