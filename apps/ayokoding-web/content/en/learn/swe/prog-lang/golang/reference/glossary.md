---
title: Go Glossary
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 802
description: Comprehensive glossary of Go terminology with examples
tags: ["golang", "go", "glossary", "terminology"]
---

## C

### Channel

A typed conduit for communication between goroutines. Channels allow safe data exchange without explicit locks.

```go
// Create channel
ch := make(chan int)

// Buffered channel
ch := make(chan int, 10)

// Send and receive
ch <- 42        // Send
value := <-ch   // Receive

// Direction-specific channels
func send(ch chan<- int) {  // Send-only
    ch <- 42
}

func receive(ch <-chan int) {  // Receive-only
    value := <-ch
}
```

### Composition

Go's approach to code reuse through embedding rather than inheritance.

```go
type Engine struct {
    Power int
}

func (e Engine) Start() {
    fmt.Println("Engine starting")
}

type Car struct {
    Engine  // Embedded
    Model string
}

car := Car{
    Engine: Engine{Power: 200},
    Model:  "Sedan",
}
car.Start()  // Calls embedded Engine.Start()
```

## D

### Defer

Statement that schedules a function call to execute after the surrounding function returns.

```go
func processFile(filename string) error {
    f, err := os.Open(filename)
    if err != nil {
        return err
    }
    defer f.Close()  // Executes when processFile returns

    // Process file...
    return nil
}

// Multiple defers execute in LIFO order
func example() {
    defer fmt.Println("First")
    defer fmt.Println("Second")
    defer fmt.Println("Third")
}
// Output: Third, Second, First
```

## G

### Goroutine

Lightweight thread managed by the Go runtime. Goroutines enable concurrent execution.

```go
// Start goroutine
go func() {
    fmt.Println("Running concurrently")
}()

// With parameters
go processData(data)

// Wait for goroutines
var wg sync.WaitGroup
wg.Add(1)
go func() {
    defer wg.Done()
    // Work...
}()
wg.Wait()
```

### Go Module

Go's dependency management system. Modules group related packages with versioning.

```bash
# Initialize module
go mod init github.com/user/project

# Add dependency
go get github.com/pkg/errors

# Update dependencies
go get -u ./...

# Tidy dependencies
go mod tidy
```

## I

### Interface

Type that specifies method signatures. Any type implementing all methods satisfies the interface implicitly.

```go
type Writer interface {
    Write([]byte) (int, error)
}

type FileWriter struct {
    file *os.File
}

func (fw FileWriter) Write(data []byte) (int, error) {
    return fw.file.Write(data)
}

// FileWriter implicitly implements Writer
var w Writer = FileWriter{file: f}
```

## M

### Method

Function with a receiver argument that associates it with a type.

```go
type Rectangle struct {
    Width, Height float64
}

// Value receiver
func (r Rectangle) Area() float64 {
    return r.Width * r.Height
}

// Pointer receiver
func (r *Rectangle) Scale(factor float64) {
    r.Width *= factor
    r.Height *= factor
}
```

### Mutex

Mutual exclusion lock for protecting shared data in concurrent programs.

```go
import "sync"

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
```

## P

### Package

Collection of source files in the same directory compiled together. Unit of code organization and reuse.

```go
// Package declaration
package main

// Import packages
import (
    "fmt"
    "time"
)

// Exported identifiers (capitalized)
func PublicFunction() {}

// Unexported identifiers (lowercase)
func privateFunction() {}
```

### Panic

Built-in function that stops normal execution flow. Similar to exceptions in other languages.

```go
func mustConnect() {
    if err := connect(); err != nil {
        panic(err)  // Stop execution
    }
}

// Recover from panic
func safeCall() {
    defer func() {
        if r := recover(); r != nil {
            fmt.Println("Recovered:", r)
        }
    }()
    mayPanic()
}
```

## R

### Receiver

Parameter that associates a method with a type. Can be value or pointer receiver.

```go
type Counter struct {
    value int
}

// Value receiver (operates on copy)
func (c Counter) Value() int {
    return c.value
}

// Pointer receiver (modifies original)
func (c *Counter) Increment() {
    c.value++
}
```

### Rune

Alias for int32, represents a Unicode code point.

```go
var r rune = '世'  // Unicode character
s := "Hello, 世界"

// Iterate over runes
for i, r := range s {
    fmt.Printf("%d: %c\n", i, r)
}
```

## S

### Select

Control structure for multiplex

ing channel operations. Blocks until one case can proceed.

```go
select {
case msg := <-ch1:
    fmt.Println("Received from ch1:", msg)
case msg := <-ch2:
    fmt.Println("Received from ch2:", msg)
case <-time.After(1 * time.Second):
    fmt.Println("Timeout")
default:
    fmt.Println("No channel ready")
}
```

### Slice

Dynamic-size view into an array. Flexible alternative to fixed-size arrays.

```go
// Create slice
s := []int{1, 2, 3}
s := make([]int, 5)       // Length 5
s := make([]int, 5, 10)   // Length 5, capacity 10

// Append
s = append(s, 4, 5, 6)

// Slice operations
sub := s[1:3]   // Elements at index 1 and 2
len := len(s)   // Length
cap := cap(s)   // Capacity
```

### Struct

Composite type that groups fields together. Similar to classes in other languages but without inheritance.

```go
type Person struct {
    Name string
    Age  int
}

// Create struct
p := Person{Name: "Alice", Age: 30}
p := Person{"Alice", 30}  // Positional

// Access fields
fmt.Println(p.Name)
p.Age = 31
```

## Related Resources

**Learn more**:

- [Cheat Sheet](/en/learn/swe/prog-lang/golang/reference/cheat-sheet) - Quick syntax reference
- [Resources](/en/learn/swe/prog-lang/golang/reference/resources) - Books and documentation
