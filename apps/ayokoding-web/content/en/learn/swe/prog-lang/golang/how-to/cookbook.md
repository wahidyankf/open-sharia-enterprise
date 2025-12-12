---
title: "Cookbook"
date: 2025-12-09T00:00:00+07:00
draft: false
weight: 1
description: "Practical recipes and patterns for idiomatic Go programming"
tags:
  - golang
  - cookbook
  - patterns
  - best-practices
  - generics
  - concurrency
---

**Ready to level up your Go skills?** This cookbook provides practical, battle-tested recipes for solving real-world problems with idiomatic Go code. Whether you're building concurrent systems, designing APIs, or optimizing performance, you'll find proven patterns and techniques used in production by companies like Google, Uber, and Docker.

## 🎯 What You'll Learn

By working through this cookbook, you will be able to:

1. **Write Type-Safe Generic Code** - Create reusable data structures and algorithms using Go 1.18+ generics
2. **Master Concurrency Patterns** - Implement worker pools, pipelines, fan-out/fan-in, and other advanced concurrency patterns
3. **Handle Errors Idiomatically** - Use wrapping, sentinel errors, custom types, and error chains effectively
4. **Manage Context Properly** - Implement cancellation, timeouts, and value propagation in concurrent code
5. **Embed Static Assets** - Bundle configuration files, templates, and web assets into your Go binaries
6. **Write Production-Grade Tests** - Create table-driven tests, property-based tests, and benchmarks
7. **Apply Design Patterns** - Use functional options, builder pattern, and other Go-idiomatic patterns
8. **Build Web Services** - Create HTTP servers with middleware, routing, and JSON APIs

## 📋 Prerequisites

Before using this cookbook, you should:

- ✅ Complete the [Golang Beginner tutorial](beginner) - or have equivalent experience with Go fundamentals
- ✅ Understand Go syntax, types, and control flow
- ✅ Know how to work with slices, maps, and structs
- ✅ Understand functions, methods, and interfaces
- ✅ Be familiar with basic goroutines and channels
- ✅ Have Go installed (version 1.18+ recommended)

## 🎯 What's in This Cookbook

- **Generics Recipes** - Type-safe reusable code patterns
- **Advanced Concurrency** - Worker pools, pipelines, synchronization
- **Error Handling** - Wrapping, sentinel errors, custom types
- **Context Patterns** - Cancellation, timeouts, value propagation
- **File Embedding** - Static assets, templates, web servers
- **Testing Patterns** - Table-driven tests, fuzzing, benchmarks
- **Design Patterns** - Functional options, builder, and more
- **Web Development** - HTTP servers, middleware, routing
- **Best Practices** - Production-ready techniques

---

## 🔷 Generics Recipes

Go 1.18+ introduced generics for type-safe reusable code. Here are practical recipes for common use cases.

### Recipe 1: Generic Stack

**Problem**: You need a type-safe stack data structure that works with any type.

**Solution**:

```go
package main

import "fmt"

// Generic stack that works with any type
type Stack[T any] struct {
	items []T
}

func NewStack[T any]() *Stack[T] {
	return &Stack[T]{
		items: make([]T, 0),
	}
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

func (s *Stack[T]) Peek() (T, bool) {
	if len(s.items) == 0 {
		var zero T
		return zero, false
	}
	return s.items[len(s.items)-1], true
}

func (s *Stack[T]) IsEmpty() bool {
	return len(s.items) == 0
}

func (s *Stack[T]) Size() int {
	return len(s.items)
}

func main() {
	// Integer stack
	intStack := NewStack[int]()
	intStack.Push(1)
	intStack.Push(2)
	intStack.Push(3)

	for !intStack.IsEmpty() {
		val, _ := intStack.Pop()
		fmt.Println(val) // 3, 2, 1
	}

	// String stack
	strStack := NewStack[string]()
	strStack.Push("hello")
	strStack.Push("world")

	val, _ := strStack.Pop()
	fmt.Println(val) // world
}
```

**When to use**: When you need a data structure that should work with multiple types but maintain type safety.

---

### Recipe 2: Generic Cache

**Problem**: You need a simple in-memory cache that works with any key-value types.

**Solution**:

```go
package main

import (
	"fmt"
	"sync"
)

// Thread-safe generic cache
type Cache[K comparable, V any] struct {
	mu   sync.RWMutex
	data map[K]V
}

func NewCache[K comparable, V any]() *Cache[K, V] {
	return &Cache[K, V]{
		data: make(map[K]V),
	}
}

func (c *Cache[K, V]) Set(key K, value V) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.data[key] = value
}

func (c *Cache[K, V]) Get(key K) (V, bool) {
	c.mu.RLock()
	defer c.mu.RUnlock()
	value, ok := c.data[key]
	return value, ok
}

func (c *Cache[K, V]) Delete(key K) {
	c.mu.Lock()
	defer c.mu.Unlock()
	delete(c.data, key)
}

func (c *Cache[K, V]) Clear() {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.data = make(map[K]V)
}

func (c *Cache[K, V]) Size() int {
	c.mu.RLock()
	defer c.mu.RUnlock()
	return len(c.data)
}

func main() {
	// String to int cache
	cache := NewCache[string, int]()
	cache.Set("age", 30)
	cache.Set("score", 95)

	age, ok := cache.Get("age")
	if ok {
		fmt.Println("Age:", age) // Age: 30
	}

	fmt.Println("Cache size:", cache.Size()) // Cache size: 2
}
```

**When to use**: When you need a simple, thread-safe cache without external dependencies.

---

### Recipe 3: Generic Filter and Map

**Problem**: You want to filter and transform slices without writing repetitive code.

**Solution**:

```go
package main

import "fmt"

// Filter returns a new slice containing only elements that satisfy the predicate
func Filter[T any](slice []T, predicate func(T) bool) []T {
	result := make([]T, 0)
	for _, item := range slice {
		if predicate(item) {
			result = append(result, item)
		}
	}
	return result
}

// Map transforms each element in the slice using the provided function
func Map[T any, U any](slice []T, fn func(T) U) []U {
	result := make([]U, len(slice))
	for i, item := range slice {
		result[i] = fn(item)
	}
	return result
}

// Reduce combines all elements in the slice into a single value
func Reduce[T any, U any](slice []T, initial U, fn func(U, T) U) U {
	result := initial
	for _, item := range slice {
		result = fn(result, item)
	}
	return result
}

func main() {
	numbers := []int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}

	// Filter even numbers
	evens := Filter(numbers, func(n int) bool {
		return n%2 == 0
	})
	fmt.Println("Evens:", evens) // [2 4 6 8 10]

	// Map to squares
	squares := Map(numbers, func(n int) int {
		return n * n
	})
	fmt.Println("Squares:", squares) // [1 4 9 16 25 36 49 64 81 100]

	// Reduce to sum
	sum := Reduce(numbers, 0, func(acc, n int) int {
		return acc + n
	})
	fmt.Println("Sum:", sum) // 55
}
```

**When to use**: When you need functional-style operations on slices without external libraries.

---

### Recipe 4: Type Constraints for Numbers

**Problem**: You want to write generic math functions that work with all numeric types.

**Solution**:

```go
package main

import "fmt"

// Number constraint for all numeric types
type Number interface {
	int | int8 | int16 | int32 | int64 |
		uint | uint8 | uint16 | uint32 | uint64 |
		float32 | float64
}

// Min returns the smaller of two numbers
func Min[T Number](a, b T) T {
	if a < b {
		return a
	}
	return b
}

// Max returns the larger of two numbers
func Max[T Number](a, b T) T {
	if a > b {
		return a
	}
	return b
}

// Sum returns the sum of all numbers in the slice
func Sum[T Number](numbers []T) T {
	var total T
	for _, n := range numbers {
		total += n
	}
	return total
}

// Average returns the average of all numbers in the slice
func Average[T Number](numbers []T) float64 {
	if len(numbers) == 0 {
		return 0
	}
	sum := Sum(numbers)
	return float64(sum) / float64(len(numbers))
}

func main() {
	// Works with ints
	ints := []int{1, 2, 3, 4, 5}
	fmt.Println("Int sum:", Sum(ints))         // 15
	fmt.Println("Int average:", Average(ints)) // 3

	// Works with floats
	floats := []float64{1.5, 2.5, 3.5}
	fmt.Println("Float sum:", Sum(floats))         // 7.5
	fmt.Println("Float average:", Average(floats)) // 2.5

	// Works with any numeric type
	fmt.Println("Min:", Min(10, 20))       // 10
	fmt.Println("Min:", Min(3.14, 2.71))   // 2.71
}
```

**How Type Constraints Work:**

```mermaid
graph TB
    subgraph "Type Constraint Definition"
        TC[Number interface<br/>int | int8 | ... | float64]
    end

    subgraph "Generic Function"
        GF["Sum[T Number](numbers []T) T"]
    end

    subgraph "Concrete Types (Allowed)"
        T1[int]
        T2[int32]
        T3[float64]
        T4[uint]
    end

    subgraph "Non-Numeric Types (Rejected)"
        T5[string ❌]
        T6[bool ❌]
        T7[struct ❌]
    end

    TC --> GF
    T1 -.implements.-> TC
    T2 -.implements.-> TC
    T3 -.implements.-> TC
    T4 -.implements.-> TC

    T5 -.does not implement.-> TC
    T6 -.does not implement.-> TC
    T7 -.does not implement.-> TC

    style TC fill:#029E73,stroke:#000000,color:#FFFFFF
    style GF fill:#0173B2,stroke:#000000,color:#FFFFFF
    style T1 fill:#029E73,stroke:#000000,color:#FFFFFF
    style T2 fill:#029E73,stroke:#000000,color:#FFFFFF
    style T3 fill:#029E73,stroke:#000000,color:#FFFFFF
    style T4 fill:#029E73,stroke:#000000,color:#FFFFFF
    style T5 fill:#DE8F05,stroke:#000000,color:#FFFFFF
    style T6 fill:#DE8F05,stroke:#000000,color:#FFFFFF
    style T7 fill:#DE8F05,stroke:#000000,color:#FFFFFF
```

**Key Insight**: The `Number` constraint acts as a filter—only types in the union (`int | int8 | ... | float64`) can be used with functions like `Sum[T Number]`. Attempting to use `Sum([]string{...})` will fail at compile time.

**When to use**: When you need math functions that work with all numeric types.

---

## 🚀 Advanced Concurrency Patterns

Go's concurrency primitives enable powerful patterns. Here are production-ready recipes.

### Understanding Concurrent Patterns

Before diving into recipes, let's visualize how worker pools coordinate multiple goroutines:

```mermaid
graph TB
    subgraph "Main Goroutine"
        M[Main] --> JC[Jobs Channel<br/>buffered]
        M --> RC[Results Channel<br/>buffered]
    end

    subgraph "Worker Pool"
        W1[Worker 1]
        W2[Worker 2]
        W3[Worker 3]
    end

    JC --> W1
    JC --> W2
    JC --> W3

    W1 --> RC
    W2 --> RC
    W3 --> RC

    RC --> M

    style JC fill:#0173B2,stroke:#000000,color:#FFFFFF
    style RC fill:#029E73,stroke:#000000,color:#FFFFFF
    style W1 fill:#DE8F05,stroke:#000000,color:#FFFFFF
    style W2 fill:#DE8F05,stroke:#000000,color:#FFFFFF
    style W3 fill:#DE8F05,stroke:#000000,color:#FFFFFF
```

**Channel Communication Patterns:**

Channels are the fundamental communication mechanism between goroutines. Understanding buffered vs unbuffered channels is crucial:

```mermaid
sequenceDiagram
    participant S1 as Sender Goroutine
    participant UB as Unbuffered Channel
    participant R1 as Receiver Goroutine
    participant S2 as Sender Goroutine
    participant BC as Buffered Channel (size=2)
    participant R2 as Receiver Goroutine

    Note over S1,R1: Unbuffered (Synchronous)
    S1->>UB: ch <- 42 (blocks)
    Note over S1: Sender waits...
    R1->>UB: val := <-ch
    Note over S1,R1: Both synchronized

    Note over S2,R2: Buffered (Asynchronous)
    S2->>BC: ch <- 10 (no block)
    S2->>BC: ch <- 20 (no block)
    Note over BC: Buffer: [10, 20]
    S2->>BC: ch <- 30 (BLOCKS - buffer full)
    Note over S2: Sender waits...
    R2->>BC: <-ch returns 10
    Note over BC: Buffer: [20, 30]
    Note over S2: Sender unblocked
```

**Key Differences**:

- **Unbuffered**: Every send blocks until receive (tight synchronization)
- **Buffered**: Sends don't block until buffer is full (looser coupling, higher throughput)
- **Use unbuffered** for strict synchronization points
- **Use buffered** for asynchronous work distribution (like worker pools)

**Key Concepts**:

- **Jobs Channel**: Distributes work to available workers (buffered for throughput)
- **Workers**: Fixed number of goroutines processing jobs concurrently
- **Results Channel**: Collects completed work from all workers
- **WaitGroup**: Ensures all workers finish before closing results channel

### Recipe 5: Worker Pool

**Problem**: You need to process many tasks concurrently with a fixed number of workers.

**Solution**:

```go
package main

import (
	"fmt"
	"sync"
	"time"
)

type Job struct {
	ID   int
	Data string
}

type Result struct {
	JobID  int
	Output string
}

func worker(id int, jobs <-chan Job, results chan<- Result, wg *sync.WaitGroup) {
	defer wg.Done()

	for job := range jobs {
		fmt.Printf("Worker %d processing job %d\n", id, job.ID)
		// Output example: Worker 2 processing job 5
		time.Sleep(time.Second) // Simulate work

		results <- Result{
			JobID:  job.ID,
			Output: fmt.Sprintf("Processed: %s", job.Data),
		}
	}
}

func main() {
	const numWorkers = 3
	const numJobs = 10

	jobs := make(chan Job, numJobs)
	results := make(chan Result, numJobs)
	var wg sync.WaitGroup

	// Start workers
	for w := 1; w <= numWorkers; w++ {
		wg.Add(1)
		go worker(w, jobs, results, &wg)
	}

	// Send jobs
	for j := 1; j <= numJobs; j++ {
		jobs <- Job{
			ID:   j,
			Data: fmt.Sprintf("task-%d", j),
		}
	}
	close(jobs)

	// Wait for workers and close results
	go func() {
		wg.Wait()
		close(results)
	}()

	// Collect results
	for result := range results {
		fmt.Printf("Job %d: %s\n", result.JobID, result.Output)
		// Output example: Job 3: Processed: task-3
		// Note: Order is non-deterministic due to concurrent processing
	}
}
// Sample output (order varies):
// Worker 1 processing job 1
// Worker 2 processing job 2
// Worker 3 processing job 3
// Job 1: Processed: task-1
// Job 2: Processed: task-2
// ... (10 jobs total)
```

**When to use**: When you have many independent tasks and want to limit concurrent execution.

---

### Recipe 6: Pipeline Pattern

**Problem**: You need to process data through multiple stages concurrently.

**Solution**:

```go
package main

import "fmt"

// Stage 1: Generate numbers
func generate(nums ...int) <-chan int {
	out := make(chan int)
	go func() {
		defer close(out)
		for _, n := range nums {
			out <- n
		}
	}()
	return out
}

// Stage 2: Square numbers
func square(in <-chan int) <-chan int {
	out := make(chan int)
	go func() {
		defer close(out)
		for n := range in {
			out <- n * n
		}
	}()
	return out
}

// Stage 3: Filter even numbers
func filterEven(in <-chan int) <-chan int {
	out := make(chan int)
	go func() {
		defer close(out)
		for n := range in {
			if n%2 == 0 {
				out <- n
			}
		}
	}()
	return out
}

func main() {
	// Build pipeline: generate -> square -> filter
	numbers := generate(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
	squared := square(numbers)
	filtered := filterEven(squared)

	// Consume results
	for result := range filtered {
		fmt.Println(result) // 4, 16, 36, 64, 100
	}
}
```

**How Pipeline Works:**

```mermaid
graph TB
    Input[Input: 1,2,3,4,5,6,7,8,9,10] --> Gen[Stage 1: Generate<br/>goroutine]
    Gen --> Ch1[Channel<br/>1,2,3,4,5,6,7,8,9,10]
    Ch1 --> Sq[Stage 2: Square<br/>goroutine]
    Sq --> Ch2[Channel<br/>1,4,9,16,25,36,49,64,81,100]
    Ch2 --> Filt[Stage 3: Filter Even<br/>goroutine]
    Filt --> Ch3[Channel<br/>4,16,36,64,100]
    Ch3 --> Output[Output: 4,16,36,64,100]

    style Gen fill:#029E73,stroke:#000000,color:#FFFFFF
    style Sq fill:#029E73,stroke:#000000,color:#FFFFFF
    style Filt fill:#029E73,stroke:#000000,color:#FFFFFF
    style Ch1 fill:#0173B2,stroke:#000000,color:#FFFFFF
    style Ch2 fill:#0173B2,stroke:#000000,color:#FFFFFF
    style Ch3 fill:#0173B2,stroke:#000000,color:#FFFFFF
```

**Key Insight**: Each pipeline stage runs in its own goroutine and communicates via channels. Data flows left-to-right through the pipeline, with each stage processing concurrently. This enables efficient multi-core utilization where:

- `generate()` is producing numbers
- `square()` is squaring them
- `filterEven()` is filtering them
- All happening **simultaneously** for different data items

**When to use**: When data flows through multiple processing stages that can run concurrently.

---

### Recipe 7: WaitGroup Patterns

**Problem**: You need to wait for multiple goroutines to complete.

**Solution (Traditional)**:

```go
package main

import (
	"fmt"
	"sync"
	"time"
)

func worker(id int, wg *sync.WaitGroup) {
	defer wg.Done() // Decrement counter when done

	fmt.Printf("Worker %d starting\n", id)
	time.Sleep(time.Second)
	fmt.Printf("Worker %d done\n", id)
}

func main() {
	var wg sync.WaitGroup

	for i := 1; i <= 5; i++ {
		wg.Add(1) // Increment counter
		go worker(i, &wg)
	}

	wg.Wait() // Block until counter is 0
	fmt.Println("All workers done")
}
```

**Solution (Go 1.25+ Modern)**:

```go
package main

import (
	"fmt"
	"sync"
	"time"
)

func worker(id int) {
	fmt.Printf("Worker %d starting\n", id)
	time.Sleep(time.Second)
	fmt.Printf("Worker %d done\n", id)
}

func main() {
	var wg sync.WaitGroup

	// Using WaitGroup.Go() - automatically handles Add(1) and Done()
	for i := 1; i <= 5; i++ {
		id := i // Capture loop variable
		wg.Go(func() {
			worker(id)
		})
	}

	wg.Wait()
	fmt.Println("All workers done")
}
```

**When to use**: When you need to wait for a group of goroutines to complete before continuing.

---

### Recipe 8: Mutex for Shared State

**Problem**: Multiple goroutines need to safely access shared data.

**Solution**:

```go
package main

import (
	"fmt"
	"sync"
)

type SafeCounter struct {
	mu    sync.Mutex
	count int
}

func (c *SafeCounter) Increment() {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.count++
}

func (c *SafeCounter) Value() int {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.count
}

// Alternative: Using RWMutex for read-heavy workloads
type SafeCache struct {
	mu   sync.RWMutex
	data map[string]string
}

func NewSafeCache() *SafeCache {
	return &SafeCache{
		data: make(map[string]string),
	}
}

func (c *SafeCache) Set(key, value string) {
	c.mu.Lock() // Write lock
	defer c.mu.Unlock()
	c.data[key] = value
}

func (c *SafeCache) Get(key string) (string, bool) {
	c.mu.RLock() // Read lock (multiple readers allowed)
	defer c.mu.RUnlock()
	value, ok := c.data[key]
	return value, ok
}

func main() {
	counter := SafeCounter{}
	var wg sync.WaitGroup

	// 1000 goroutines incrementing
	for i := 0; i < 1000; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			counter.Increment()
		}()
	}

	wg.Wait()
	fmt.Println("Final count:", counter.Value()) // 1000
}
```

**When to use**: When multiple goroutines need to access shared mutable state.

---

### Recipe 9: Fan-Out, Fan-In

**Problem**: You need to distribute work across multiple workers and collect results.

**Solution**:

```go
package main

import (
	"fmt"
	"sync"
)

func producer(nums ...int) <-chan int {
	out := make(chan int)
	go func() {
		defer close(out)
		for _, n := range nums {
			out <- n
		}
	}()
	return out
}

func square(in <-chan int) <-chan int {
	out := make(chan int)
	go func() {
		defer close(out)
		for n := range in {
			out <- n * n
		}
	}()
	return out
}

func merge(channels ...<-chan int) <-chan int {
	var wg sync.WaitGroup
	merged := make(chan int)

	// Start goroutine for each channel
	for _, ch := range channels {
		wg.Add(1)
		go func(c <-chan int) {
			defer wg.Done()
			for n := range c {
				merged <- n
			}
		}(ch)
	}

	// Close merged when all inputs are done
	go func() {
		wg.Wait()
		close(merged)
	}()

	return merged
}

func main() {
	// Fan-out: distribute work to multiple workers
	in := producer(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

	// Start 3 workers
	c1 := square(in)
	c2 := square(in)
	c3 := square(in)

	// Fan-in: merge results from multiple workers
	for result := range merge(c1, c2, c3) {
		fmt.Println(result)
	}
}
```

**When to use**: When you need to parallelize work across multiple workers and collect all results.

---

## ⚠️ Error Handling Patterns

Idiomatic Go error handling goes beyond simple `if err != nil` checks.

### Recipe 10: Error Wrapping

**Problem**: You want to add context to errors while preserving the original error.

**Solution**:

```go
package main

import (
	"errors"
	"fmt"
	"os"
)

// Define sentinel errors
var (
	ErrNotFound     = errors.New("resource not found")
	ErrUnauthorized = errors.New("unauthorized access")
	ErrInvalidInput = errors.New("invalid input")
)

func openConfig(filename string) error {
	_, err := os.Open(filename)
	if err != nil {
		// Wrap with sentinel error
		return fmt.Errorf("%w: %s", ErrNotFound, filename)
	}
	return nil
}

func loadConfig(filename string) error {
	err := openConfig(filename)
	if err != nil {
		// Wrap with context using %w
		return fmt.Errorf("failed to load config: %w", err)
	}
	return nil
}

func main() {
	err := loadConfig("config.yaml")
	if err != nil {
		fmt.Println("Error:", err)
		// Output: Error: failed to load config: resource not found: config.yaml

		// Check for specific error
		if errors.Is(err, ErrNotFound) {
			fmt.Println("Config file not found, using defaults")
		}

		// Check for error type
		var pathErr *os.PathError
		if errors.As(err, &pathErr) {
			fmt.Printf("Path error on: %s\n", pathErr.Path)
		}
	}
}
```

**When to use**: When you want to add context to errors without losing the original error information.

---

### Recipe 11: Custom Error Types

**Problem**: You need errors with additional structured data.

**Solution**:

```go
package main

import "fmt"

// Custom error with fields
type ValidationError struct {
	Field   string
	Value   interface{}
	Message string
}

func (e *ValidationError) Error() string {
	return fmt.Sprintf("validation error on '%s': %s (got %v)",
		e.Field, e.Message, e.Value)
}

// Another custom error
type APIError struct {
	StatusCode int
	Method     string
	URL        string
	Message    string
}

func (e *APIError) Error() string {
	return fmt.Sprintf("%d %s %s: %s",
		e.StatusCode, e.Method, e.URL, e.Message)
}

func validateAge(age int) error {
	if age < 0 {
		return &ValidationError{
			Field:   "age",
			Value:   age,
			Message: "must be non-negative",
		}
	}
	if age < 18 {
		return &ValidationError{
			Field:   "age",
			Value:   age,
			Message: "must be at least 18",
		}
	}
	return nil
}

func main() {
	err := validateAge(15)
	if err != nil {
		fmt.Println(err)
		// Output: validation error on 'age': must be at least 18 (got 15)

		// Type assert to access fields
		if ve, ok := err.(*ValidationError); ok {
			fmt.Printf("Field: %s, Value: %v\n", ve.Field, ve.Value)
		}
	}
}
```

**When to use**: When errors need to carry structured data beyond a simple message.

---

### Recipe 12: Error Collection

**Problem**: You need to collect multiple errors from concurrent operations.

**Solution**:

```go
package main

import (
	"fmt"
	"strings"
	"sync"
)

// MultiError collects multiple errors
type MultiError struct {
	mu     sync.Mutex
	errors []error
}

func (m *MultiError) Add(err error) {
	if err != nil {
		m.mu.Lock()
		m.errors = append(m.errors, err)
		m.mu.Unlock()
	}
}

func (m *MultiError) Error() string {
	m.mu.Lock()
	defer m.mu.Unlock()

	if len(m.errors) == 0 {
		return ""
	}

	messages := make([]string, len(m.errors))
	for i, err := range m.errors {
		messages[i] = err.Error()
	}
	return fmt.Sprintf("%d errors occurred: %s",
		len(m.errors), strings.Join(messages, "; "))
}

func (m *MultiError) HasErrors() bool {
	m.mu.Lock()
	defer m.mu.Unlock()
	return len(m.errors) > 0
}

func (m *MultiError) Errors() []error {
	m.mu.Lock()
	defer m.mu.Unlock()
	return append([]error(nil), m.errors...)
}

func processItems(items []int) error {
	var wg sync.WaitGroup
	merr := &MultiError{}

	for _, item := range items {
		wg.Add(1)
		go func(n int) {
			defer wg.Done()
			if err := processItem(n); err != nil {
				merr.Add(err)
			}
		}(item)
	}

	wg.Wait()

	if merr.HasErrors() {
		return merr
	}
	return nil
}

func processItem(n int) error {
	if n < 0 {
		return fmt.Errorf("invalid item: %d", n)
	}
	return nil
}

func main() {
	items := []int{1, -2, 3, -4, 5}

	if err := processItems(items); err != nil {
		fmt.Println("Errors occurred:", err)

		if merr, ok := err.(*MultiError); ok {
			for i, e := range merr.Errors() {
				fmt.Printf("  Error %d: %s\n", i+1, e)
			}
		}
	}
}
```

**When to use**: When you need to collect and report multiple errors from concurrent operations.

---

## 🎯 Context Patterns

Context is essential for cancellation, timeouts, and passing request-scoped values.

### Recipe 13: Context with Timeout

**Problem**: You need to limit how long an operation can run.

**Solution**:

```go
package main

import (
	"context"
	"fmt"
	"time"
)

func doWork(ctx context.Context) error {
	for i := 0; i < 10; i++ {
		select {
		case <-ctx.Done():
			return ctx.Err() // Cancelled or timed out
		default:
			fmt.Printf("Working... %d\n", i)
			time.Sleep(500 * time.Millisecond)
		}
	}
	return nil
}

func main() {
	// Context with 2 second timeout
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Second)
	defer cancel() // Always call cancel to release resources

	if err := doWork(ctx); err != nil {
		if err == context.DeadlineExceeded {
			fmt.Println("Operation timed out")
		} else {
			fmt.Println("Error:", err)
		}
	}
}
```

**When to use**: When you need to prevent operations from running too long.

---

### Recipe 14: Context Cancellation

**Problem**: You need to cancel an operation based on external events.

**Solution**:

```go
package main

import (
	"context"
	"fmt"
	"time"
)

func monitor(ctx context.Context) {
	ticker := time.NewTicker(500 * time.Millisecond)
	defer ticker.Stop()

	for {
		select {
		case <-ctx.Done():
			fmt.Println("Monitor stopped:", ctx.Err())
			return
		case t := <-ticker.C:
			fmt.Println("Monitoring at", t.Format("15:04:05"))
		}
	}
}

func main() {
	ctx, cancel := context.WithCancel(context.Background())

	go monitor(ctx)

	// Let it run for 3 seconds
	time.Sleep(3 * time.Second)

	// Cancel the context
	fmt.Println("Cancelling...")
	cancel()

	// Give it time to clean up
	time.Sleep(time.Second)
}
```

**When to use**: When you need to stop goroutines based on external events.

---

### Recipe 15: Context with Values

**Problem**: You need to pass request-scoped data through the call chain.

**Solution**:

```go
package main

import (
	"context"
	"fmt"
)

type contextKey string

const (
	requestIDKey contextKey = "requestID"
	userIDKey    contextKey = "userID"
)

func WithRequestID(ctx context.Context, requestID string) context.Context {
	return context.WithValue(ctx, requestIDKey, requestID)
}

func GetRequestID(ctx context.Context) (string, bool) {
	requestID, ok := ctx.Value(requestIDKey).(string)
	return requestID, ok
}

func processRequest(ctx context.Context, data string) {
	if requestID, ok := GetRequestID(ctx); ok {
		fmt.Printf("[%s] Processing: %s\n", requestID, data)
	}

	// Pass context down the call chain
	saveToDatabase(ctx, data)
}

func saveToDatabase(ctx context.Context, data string) {
	if requestID, ok := GetRequestID(ctx); ok {
		fmt.Printf("[%s] Saving to database: %s\n", requestID, data)
	}
}

func main() {
	ctx := context.Background()
	ctx = WithRequestID(ctx, "req-12345")

	processRequest(ctx, "user data")
}
```

**When to use**: When you need to pass request-scoped data (IDs, tokens, etc.) through the call chain.

**⚠️ Important**: Only use context values for request-scoped data, not for passing optional parameters.

---

## 📁 File Embedding

Go 1.16+ allows embedding files directly into your binary at compile time.

### Recipe 16: Embed Static Files

**Problem**: You want to bundle static assets with your binary.

**Solution**:

```go
package main

import (
	_ "embed"
	"fmt"
)

// Embed single file as string
//go:embed config.txt
var config string

// Embed single file as bytes
//go:embed logo.png
var logo []byte

// Embed with variable initialization
//go:embed version.txt
var version string

func main() {
	fmt.Println("Config:", config)
	fmt.Println("Logo size:", len(logo), "bytes")
	fmt.Println("Version:", version)
}
```

**When to use**: When you want to distribute a single binary with all assets included.

---

### Recipe 17: Embed Directory for Web Server

**Problem**: You want to serve static web files from an embedded directory.

**Solution**:

```go
package main

import (
	"embed"
	"io/fs"
	"log"
	"net/http"
)

// Embed entire static directory
//go:embed static/*
var staticAssets embed.FS

func main() {
	// Create sub-filesystem to strip "static/" prefix
	staticFS, err := fs.Sub(staticAssets, "static")
	if err != nil {
		log.Fatal(err)
	}

	// Serve embedded files
	http.Handle("/static/",
		http.StripPrefix("/static/", http.FileServer(http.FS(staticFS))))

	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte("Hello! Static files are embedded."))
	})

	log.Println("Server starting on :8080")
	log.Fatal(http.ListenAndServe(":8080", nil))
}
```

**Project structure**:

```
myapp/
├── main.go
└── static/
    ├── css/
    │   └── style.css
    ├── js/
    │   └── app.js
    └── images/
        └── logo.png
```

**When to use**: When building web applications that need to bundle all assets.

---

### Recipe 18: Embed Templates

**Problem**: You want to embed HTML templates with your application.

**Solution**:

```go
package main

import (
	"embed"
	"html/template"
	"log"
	"net/http"
)

//go:embed templates/*.html
var templateFS embed.FS

var templates = template.Must(template.ParseFS(templateFS, "templates/*.html"))

type PageData struct {
	Title   string
	Message string
	Items   []string
}

func homeHandler(w http.ResponseWriter, r *http.Request) {
	data := PageData{
		Title:   "Welcome",
		Message: "Hello from embedded templates!",
		Items:   []string{"Go", "Rust", "Python"},
	}

	err := templates.ExecuteTemplate(w, "index.html", data)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
	}
}

func main() {
	http.HandleFunc("/", homeHandler)

	log.Println("Server starting on :8080")
	log.Fatal(http.ListenAndServe(":8080", nil))
}
```

**templates/index.html**:

```html
<!DOCTYPE html>
<html>
  <head>
    <title>{{.Title}}</title>
  </head>
  <body>
    <h1>{{.Message}}</h1>
    <ul>
      {{range .Items}}
      <li>{{.}}</li>
      {{end}}
    </ul>
  </body>
</html>
```

**When to use**: When building web applications with HTML templates.

---

## 🧪 Testing Patterns

Go's testing tools enable powerful testing strategies.

### Recipe 19: Table-Driven Tests

**Problem**: You want to test multiple cases without repeating code.

**Solution**:

```go
package math

import "testing"

func Add(a, b int) int {
	return a + b
}

func TestAdd(t *testing.T) {
	tests := []struct {
		name     string
		a, b     int
		expected int
	}{
		{"positive numbers", 2, 3, 5},
		{"negative numbers", -2, -3, -5},
		{"mixed signs", -2, 3, 1},
		{"with zero", 0, 5, 5},
		{"zero result", -5, 5, 0},
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

**When to use**: When testing functions with multiple input/output combinations.

---

### Recipe 20: Fuzz Testing

**Problem**: You want to find edge cases automatically.

**Solution**:

```go
package strings

import (
	"testing"
	"unicode/utf8"
)

func Reverse(s string) string {
	runes := []rune(s)
	for i, j := 0, len(runes)-1; i < j; i, j = i+1, j-1 {
		runes[i], runes[j] = runes[j], runes[i]
	}
	return string(runes)
}

func FuzzReverse(f *testing.F) {
	// Seed corpus
	testcases := []string{"Hello", "世界", "!12345", ""}
	for _, tc := range testcases {
		f.Add(tc)
	}

	f.Fuzz(func(t *testing.T, original string) {
		// Property 1: Reversing twice returns original
		reversed := Reverse(original)
		doubleReversed := Reverse(reversed)
		if original != doubleReversed {
			t.Errorf("Reverse twice: got %q, want %q",
				doubleReversed, original)
		}

		// Property 2: Valid UTF-8 stays valid
		if utf8.ValidString(original) && !utf8.ValidString(reversed) {
			t.Errorf("Reverse produced invalid UTF-8")
		}

		// Property 3: Length is preserved
		if len([]rune(original)) != len([]rune(reversed)) {
			t.Errorf("Length changed: %d != %d",
				len([]rune(original)), len([]rune(reversed)))
		}
	})
}
```

**Run fuzzing**:

```bash
go test -fuzz=FuzzReverse -fuzztime=30s
```

**When to use**: When you want to test invariants and find edge cases automatically.

---

### Recipe 21: Benchmarks

**Problem**: You want to measure performance and compare implementations.

**Solution**:

```go
package algorithms

import (
	"testing"
)

func LinearSearch(slice []int, target int) int {
	for i, v := range slice {
		if v == target {
			return i
		}
	}
	return -1
}

func BinarySearch(slice []int, target int) int {
	left, right := 0, len(slice)-1

	for left <= right {
		mid := left + (right-left)/2
		if slice[mid] == target {
			return mid
		}
		if slice[mid] < target {
			left = mid + 1
		} else {
			right = mid - 1
		}
	}
	return -1
}

func BenchmarkLinearSearch(b *testing.B) {
	data := make([]int, 1000)
	for i := range data {
		data[i] = i
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		LinearSearch(data, 500)
	}
}

func BenchmarkBinarySearch(b *testing.B) {
	data := make([]int, 1000)
	for i := range data {
		data[i] = i
	}

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		BinarySearch(data, 500)
	}
}
```

**Run benchmarks**:

```bash
go test -bench=. -benchmem
```

**When to use**: When you need to measure and compare performance.

---

## 🎨 Design Patterns

Idiomatic Go design patterns for clean, maintainable code.

### Recipe 22: Functional Options

**Problem**: You want flexible initialization without multiple constructors.

**Solution**:

```go
package main

import (
	"fmt"
	"time"
)

type Server struct {
	host    string
	port    int
	timeout time.Duration
	maxConn int
}

type Option func(*Server)

func WithHost(host string) Option {
	return func(s *Server) {
		s.host = host
	}
}

func WithPort(port int) Option {
	return func(s *Server) {
		s.port = port
	}
}

func WithTimeout(timeout time.Duration) Option {
	return func(s *Server) {
		s.timeout = timeout
	}
}

func WithMaxConnections(max int) Option {
	return func(s *Server) {
		s.maxConn = max
	}
}

func NewServer(opts ...Option) *Server {
	// Defaults
	s := &Server{
		host:    "localhost",
		port:    8080,
		timeout: 30 * time.Second,
		maxConn: 100,
	}

	// Apply options
	for _, opt := range opts {
		opt(s)
	}

	return s
}

func main() {
	// Use defaults
	server1 := NewServer()
	fmt.Printf("%+v\n", server1)

	// Override specific options
	server2 := NewServer(
		WithHost("0.0.0.0"),
		WithPort(9000),
		WithTimeout(60*time.Second),
	)
	fmt.Printf("%+v\n", server2)
}
```

**When to use**: When you have many configuration options with sensible defaults.

---

### Recipe 23: Builder Pattern

**Problem**: You want to construct complex objects step by step.

**Solution**:

```go
package main

import (
	"fmt"
	"strings"
)

type QueryBuilder struct {
	table  string
	fields []string
	where  string
	limit  int
}

func NewQuery(table string) *QueryBuilder {
	return &QueryBuilder{table: table}
}

func (q *QueryBuilder) Select(fields ...string) *QueryBuilder {
	q.fields = fields
	return q
}

func (q *QueryBuilder) Where(condition string) *QueryBuilder {
	q.where = condition
	return q
}

func (q *QueryBuilder) Limit(n int) *QueryBuilder {
	q.limit = n
	return q
}

func (q *QueryBuilder) Build() string {
	return fmt.Sprintf("SELECT %s FROM %s WHERE %s LIMIT %d",
		strings.Join(q.fields, ", "), q.table, q.where, q.limit)
}

func main() {
	query := NewQuery("users").
		Select("id", "name", "email").
		Where("age > 18").
		Limit(10).
		Build()

	fmt.Println(query)
	// Output: SELECT id, name, email FROM users WHERE age > 18 LIMIT 10
}
```

**When to use**: When constructing objects with many optional parameters or complex setup.

---

### Recipe 24: Singleton Pattern

**Problem**: You need exactly one instance of a type.

**Solution**:

```go
package main

import (
	"fmt"
	"sync"
)

type Database struct {
	connection string
}

var (
	instance *Database
	once     sync.Once
)

func GetDatabase() *Database {
	once.Do(func() {
		fmt.Println("Creating database instance")
		instance = &Database{
			connection: "db://localhost:5432",
		}
	})
	return instance
}

func main() {
	db1 := GetDatabase()
	db2 := GetDatabase()

	fmt.Println("Same instance:", db1 == db2) // true
	// "Creating database instance" printed only once
}
```

**When to use**: When you need exactly one instance (database connections, config, etc.).

---

## 🌐 Web Development Recipes

Practical patterns for building web services.

### Recipe 25: HTTP Middleware

**Problem**: You need to add logging, authentication, or other cross-cutting concerns.

**Solution**:

```go
package main

import (
	"fmt"
	"log"
	"net/http"
	"time"
)

// Middleware type
type Middleware func(http.HandlerFunc) http.HandlerFunc

// Logging middleware
func LoggingMiddleware(next http.HandlerFunc) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		start := time.Now()
		log.Printf("Started %s %s", r.Method, r.URL.Path)

		next(w, r)

		log.Printf("Completed in %v", time.Since(start))
	}
}

// Auth middleware
func AuthMiddleware(next http.HandlerFunc) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		token := r.Header.Get("Authorization")
		if token != "secret-token" {
			http.Error(w, "Unauthorized", http.StatusUnauthorized)
			return
		}
		next(w, r)
	}
}

// Chain multiple middleware
func Chain(f http.HandlerFunc, middlewares ...Middleware) http.HandlerFunc {
	for i := len(middlewares) - 1; i >= 0; i-- {
		f = middlewares[i](f)
	}
	return f
}

func helloHandler(w http.ResponseWriter, r *http.Request) {
	fmt.Fprintf(w, "Hello, World!")
}

func main() {
	// Apply middleware chain
	http.HandleFunc("/", Chain(helloHandler, LoggingMiddleware, AuthMiddleware))

	log.Println("Server starting on :8080")
	log.Fatal(http.ListenAndServe(":8080", nil))
}
```

**When to use**: When you need to apply common functionality across multiple handlers.

---

### Recipe 26: JSON API Handler

**Problem**: You want clean JSON API handlers with proper error handling.

**Solution**:

```go
package main

import (
	"encoding/json"
	"log"
	"net/http"
)

type User struct {
	ID    int    `json:"id"`
	Name  string `json:"name"`
	Email string `json:"email"`
}

type APIError struct {
	Error   string `json:"error"`
	Message string `json:"message"`
	Status  int    `json:"status"`
}

func writeJSON(w http.ResponseWriter, status int, data interface{}) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(status)
	json.NewEncoder(w).Encode(data)
}

func writeError(w http.ResponseWriter, status int, message string) {
	writeJSON(w, status, APIError{
		Error:   http.StatusText(status),
		Message: message,
		Status:  status,
	})
}

func getUsersHandler(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodGet {
		writeError(w, http.StatusMethodNotAllowed, "Only GET is allowed")
		return
	}

	users := []User{
		{ID: 1, Name: "Alice", Email: "alice@example.com"},
		{ID: 2, Name: "Bob", Email: "bob@example.com"},
	}

	writeJSON(w, http.StatusOK, users)
}

func createUserHandler(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		writeError(w, http.StatusMethodNotAllowed, "Only POST is allowed")
		return
	}

	var user User
	if err := json.NewDecoder(r.Body).Decode(&user); err != nil {
		writeError(w, http.StatusBadRequest, "Invalid JSON")
		return
	}

	// Validate
	if user.Name == "" || user.Email == "" {
		writeError(w, http.StatusBadRequest, "Name and email are required")
		return
	}

	// Save user (simplified)
	user.ID = 123

	writeJSON(w, http.StatusCreated, user)
}

func main() {
	http.HandleFunc("/users", getUsersHandler)
	http.HandleFunc("/users/create", createUserHandler)

	log.Println("Server starting on :8080")
	log.Fatal(http.ListenAndServe(":8080", nil))
}
```

**When to use**: When building JSON APIs with consistent error handling.

---

## 📊 Best Practices

Key principles for production-ready Go code.

### Code Organization

✅ **Keep packages focused** - Each package should have a single, clear purpose
✅ **Use interfaces** - Accept interfaces, return structs
✅ **Avoid premature abstraction** - Start simple, refactor when needed
✅ **Name clearly** - `getUserByID` not `get`, `calculateTotal` not `calc`

### Error Handling

✅ **Handle errors explicitly** - Don't ignore errors
✅ **Add context when wrapping** - Use `fmt.Errorf("context: %w", err)`
✅ **Define sentinel errors** - `var ErrNotFound = errors.New("not found")`
✅ **Use custom types for rich errors** - When you need structured error data

### Concurrency

✅ **Start goroutines explicitly** - Make concurrency visible in the code
✅ **Use channels for coordination** - Not for data sharing
✅ **Always clean up goroutines** - Use context for cancellation
✅ **Protect shared state** - Use mutexes or channels, not both

### Testing

✅ **Test behavior, not implementation** - Focus on what, not how
✅ **Use table-driven tests** - For multiple test cases
✅ **Use fuzzing for invariants** - Find edge cases automatically
✅ **Benchmark critical paths** - Measure before optimizing

### Performance

✅ **Profile before optimizing** - Use `pprof` to find bottlenecks
✅ **Reuse allocations** - Use `sync.Pool` for frequently allocated objects
✅ **Use buffered channels** - When throughput matters
✅ **Avoid premature optimization** - Clarity first, performance second

---

## 🔧 Troubleshooting Common Patterns

Advanced Go patterns can introduce subtle bugs. Here's how to identify and fix them.

### Concurrency Pitfalls

**Problem: Goroutine Leak in Worker Pool**

```go
// ❌ Wrong - Workers never exit
func badWorkerPool() {
	jobs := make(chan int)
	for i := 0; i < 5; i++ {
		go func() {
			for job := range jobs {  // Blocks forever if jobs never closes
				process(job)
			}
		}()
	}
	// Forgot to close jobs channel - workers leak!
}
```

**Fix**: Always close channels and provide exit mechanisms.

```go
// ✅ Correct - Workers can exit
func goodWorkerPool(ctx context.Context) {
	jobs := make(chan int, 10)
	var wg sync.WaitGroup

	for i := 0; i < 5; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			for {
				select {
				case job, ok := <-jobs:
					if !ok {
						return  // Channel closed, exit goroutine
					}
					process(job)
				case <-ctx.Done():
					return  // Context cancelled, exit goroutine
				}
			}
		}()
	}

	// Send jobs...
	close(jobs)  // Signal workers to exit
	wg.Wait()    // Wait for cleanup
}
```

**Problem: Data Race on Shared Counter**

```go
// ❌ Wrong - Race condition
type BadCounter struct {
	count int
}

func (c *BadCounter) Increment() {
	c.count++  // NOT atomic! Race condition
}
```

**Fix**: Use mutex or atomic operations.

```go
// ✅ Correct - Mutex protection
type GoodCounter struct {
	mu    sync.Mutex
	count int
}

func (c *GoodCounter) Increment() {
	c.mu.Lock()
	c.count++
	c.mu.Unlock()
}

// ✅ Alternative - Atomic operations
type AtomicCounter struct {
	count int64
}

func (c *AtomicCounter) Increment() {
	atomic.AddInt64(&c.count, 1)
}
```

**Problem: Deadlock from Circular Wait**

```go
// ❌ Wrong - Deadlock
func deadlockExample() {
	var mu1, mu2 sync.Mutex

	go func() {
		mu1.Lock()
		time.Sleep(10 * time.Millisecond)
		mu2.Lock()  // Deadlock: waiting for mu2
		defer mu2.Unlock()
		defer mu1.Unlock()
	}()

	go func() {
		mu2.Lock()
		time.Sleep(10 * time.Millisecond)
		mu1.Lock()  // Deadlock: waiting for mu1
		defer mu1.Unlock()
		defer mu2.Unlock()
	}()
}
```

**Fix**: Always acquire locks in the same order.

```go
// ✅ Correct - Consistent lock ordering
func fixedLocking() {
	var mu1, mu2 sync.Mutex

	lockInOrder := func() {
		mu1.Lock()  // Always lock mu1 first
		defer mu1.Unlock()
		mu2.Lock()  // Then mu2
		defer mu2.Unlock()
		// Critical section
	}

	go lockInOrder()
	go lockInOrder()
	// No deadlock - both goroutines acquire locks in same order
}
```

### Generics Pitfalls

**Problem: Type Constraint Too Restrictive**

```go
// ❌ Wrong - Only works with comparable types
func BadFind[T comparable](slice []T, target T) int {
	for i, v := range slice {
		if v == target {
			return i
		}
	}
	return -1
}

// Won't compile: slices aren't comparable
// result := BadFind([][]int{{1, 2}, {3, 4}}, []int{1, 2})
```

**Fix**: Use appropriate constraints or custom comparison functions.

```go
// ✅ Correct - Custom comparison function
func GoodFind[T any](slice []T, target T, equals func(T, T) bool) int {
	for i, v := range slice {
		if equals(v, target) {
			return i
		}
	}
	return -1
}

// Usage
result := GoodFind([][]int{{1, 2}, {3, 4}}, []int{1, 2}, func(a, b []int) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
})
```

**Problem: Zero Value Confusion**

```go
// ❌ Wrong - Returns zero value, hard to distinguish from valid result
func BadPop[T any](stack *[]T) T {
	if len(*stack) == 0 {
		var zero T
		return zero  // Ambiguous: is 0 a valid element or empty stack?
	}
	result := (*stack)[len(*stack)-1]
	*stack = (*stack)[:len(*stack)-1]
	return result
}
```

**Fix**: Return (value, bool) tuple or use error.

```go
// ✅ Correct - Boolean indicates success
func GoodPop[T any](stack *[]T) (T, bool) {
	if len(*stack) == 0 {
		var zero T
		return zero, false  // Clear signal: stack was empty
	}
	result := (*stack)[len(*stack)-1]
	*stack = (*stack)[:len(*stack)-1]
	return result, true  // Success
}

// Usage
if val, ok := GoodPop(&myStack); ok {
	fmt.Println("Popped:", val)
} else {
	fmt.Println("Stack is empty")
}
```

### Error Handling Pitfalls

**Problem: Losing Error Context**

```go
// ❌ Wrong - Lost context
func badProcessFile(filename string) error {
	data, err := os.ReadFile(filename)
	if err != nil {
		return err  // Who called this? What file? Lost!
	}
	return processData(data)
}
```

**Fix**: Wrap errors with context.

```go
// ✅ Correct - Preserves context
func goodProcessFile(filename string) error {
	data, err := os.ReadFile(filename)
	if err != nil {
		return fmt.Errorf("processFile(%q): %w", filename, err)
	}
	if err := processData(data); err != nil {
		return fmt.Errorf("processFile(%q): processing failed: %w", filename, err)
	}
	return nil
}
// Error message: processFile("data.json"): open data.json: no such file or directory
```

**Problem: Ignoring Errors in Defer**

```go
// ❌ Wrong - Ignoring Close() error
func badFileHandler() error {
	file, err := os.Create("output.txt")
	if err != nil {
		return err
	}
	defer file.Close()  // Error ignored!

	_, err = file.WriteString("data")
	return err
}
```

**Fix**: Handle defer errors properly.

```go
// ✅ Correct - Capture defer error
func goodFileHandler() (err error) {
	file, err := os.Create("output.txt")
	if err != nil {
		return err
	}
	defer func() {
		if closeErr := file.Close(); closeErr != nil && err == nil {
			err = fmt.Errorf("close file: %w", closeErr)
		}
	}()

	_, err = file.WriteString("data")
	return err
}
```

### Performance Issues

**Problem: Excessive Allocations in Hot Path**

```go
// ❌ Wrong - Allocates new slice every iteration
func badProcessLoop(items []string) {
	for i := 0; i < 1000000; i++ {
		result := make([]string, 0, len(items))  // Allocates 1M times!
		for _, item := range items {
			result = append(result, process(item))
		}
		handle(result)
	}
}
```

**Fix**: Reuse allocations or use sync.Pool.

```go
// ✅ Correct - Reuses buffer
var bufferPool = sync.Pool{
	New: func() interface{} {
		return make([]string, 0, 100)
	},
}

func goodProcessLoop(items []string) {
	for i := 0; i < 1000000; i++ {
		result := bufferPool.Get().([]string)
		result = result[:0]  // Reset length, keep capacity

		for _, item := range items {
			result = append(result, process(item))
		}
		handle(result)

		bufferPool.Put(result)  // Return to pool
	}
}
```

**Problem: Unbuffered Channel Causing Contention**

```go
// ❌ Wrong - Unbuffered channel creates synchronization points
func badPipeline() {
	ch1 := make(chan int)     // Unbuffered
	ch2 := make(chan int)     // Unbuffered

	go producer(ch1)   // Blocks on every send
	go processor(ch1, ch2)  // Blocks on send and receive
	consumer(ch2)      // Blocks on every receive
	// Stages are tightly coupled - no parallelism
}
```

**Fix**: Use buffered channels for throughput.

```go
// ✅ Correct - Buffered channels for async processing
func goodPipeline() {
	ch1 := make(chan int, 100)  // Buffered
	ch2 := make(chan int, 100)  // Buffered

	go producer(ch1)
	go processor(ch1, ch2)
	consumer(ch2)
	// Stages can work independently - better parallelism
}
```

### Debugging Tips

**1. Detect Race Conditions**

```bash
# Run with race detector
go test -race ./...
go run -race main.go

# Will report:
# WARNING: DATA RACE
# Write at 0x... by goroutine 7:
#   main.(*BadCounter).Increment()
```

**2. Find Goroutine Leaks**

```go
// Use runtime to track goroutines
import "runtime"

func detectLeaks() {
	before := runtime.NumGoroutine()

	// Run your code
	runWorkerPool()

	time.Sleep(time.Second)  // Wait for cleanup
	after := runtime.NumGoroutine()

	if after > before {
		fmt.Printf("Goroutine leak: %d -> %d (+%d)\n", before, after, after-before)
	}
}
```

**3. Profile Performance**

```go
import _ "net/http/pprof"

func main() {
	go func() {
		http.ListenAndServe("localhost:6060", nil)
	}()

	// Access http://localhost:6060/debug/pprof/
	// Or use: go tool pprof http://localhost:6060/debug/pprof/profile
}
```

**4. Use Context for Cancellation**

```go
// Always pass context for long-running operations
func worker(ctx context.Context, jobs <-chan Job) {
	for {
		select {
		case job := <-jobs:
			if err := processWithContext(ctx, job); err != nil {
				log.Printf("Error: %v", err)
			}
		case <-ctx.Done():
			log.Println("Worker cancelled")
			return
		}
	}
}
```

---

## 🎯 Practice Exercises

Apply the cookbook recipes with these hands-on challenges.

### Exercise 1: Build a Generic LRU Cache (Intermediate)

Combine the generic cache pattern with an eviction policy:

- Implement a Least Recently Used (LRU) cache
- Use generics for type-safe key-value storage
- Automatically evict least recently used items when capacity is reached
- Use a mutex for thread-safety

**Hint**: Combine a map with a doubly-linked list to track access order.

### Exercise 2: Concurrent Pipeline with Fan-Out/Fan-In (Advanced)

Build a data processing pipeline:

- Stage 1: Read numbers from a channel
- Stage 2: Square each number (fan-out to 3 workers)
- Stage 3: Collect and sum all results (fan-in)
- Use context for graceful cancellation

**Hint**: Use the pipeline pattern from the concurrency recipes section.

### Exercise 3: HTTP Middleware Chain (Intermediate)

Create a middleware system for HTTP servers:

- Implement `Logger` middleware (logs request method, path, duration)
- Implement `Auth` middleware (checks Authorization header)
- Implement `CORS` middleware (adds CORS headers)
- Chain them together using the functional options pattern

**Hint**: Each middleware should wrap an `http.Handler` and return a new `http.Handler`.

### Exercise 4: Table-Driven Test with Fuzz Testing (Advanced)

Write comprehensive tests for a `Validate Email` function:

- Implement the email validation function
- Write table-driven tests covering edge cases
- Add fuzz tests to discover unexpected inputs
- Achieve 100% code coverage

**Hint**: Use `testing.F` for fuzz tests and generate random email-like strings.

### Exercise 5: Worker Pool with Dynamic Scaling (Expert)

Implement an auto-scaling worker pool:

- Start with N workers
- Monitor job queue length
- Dynamically add workers when queue grows (max 2N)
- Remove idle workers when queue shrinks
- Track metrics (jobs processed, current workers, queue length)

**Hint**: Use a ticker to periodically check queue depth and adjust worker count.

---

## 🚀 Next Steps

You've learned practical Go patterns! Continue your journey:

### Practice Projects

- **Build a CLI tool** - Use `cobra` or `flag` package
- **Create a REST API** - Practice routing, middleware, error handling
- **Implement a cache** - Apply generics and concurrency patterns
- **Build a web scraper** - Use goroutines for concurrent scraping
- **Create a worker pool** - Implement job processing system

### Advanced Topics

- **Performance profiling** - Master `pprof` for CPU and memory profiling
- **Advanced generics** - Type constraints, type inference
- **Database patterns** - Connection pooling, transactions, migrations
- **Microservices** - gRPC, service discovery, distributed tracing
- **Testing strategies** - Integration tests, contract testing, mocks

### Resources

- **Official Documentation** - [go.dev/doc](https://go.dev/doc)
- **Effective Go** - [go.dev/doc/effective_go](https://go.dev/doc/effective_go)
- **Go Blog** - [go.dev/blog](https://go.dev/blog)
- **Go Wiki** - [go.dev/wiki](https://go.dev/wiki/)

---

**Happy Cooking! 🍳**
