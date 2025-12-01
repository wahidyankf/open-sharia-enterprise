---
title: Golang Crash Course
description: A comprehensive quick-start guide to Go programming language
category: tutorials
tags:
  - golang
  - programming-languages
  - crash-course
  - concurrency
created: 2025-12-01
updated: 2025-12-01
---

# Golang Crash Course

Learn Go (Golang) - a modern, statically typed, compiled programming language designed for simplicity, efficiency, and excellent concurrency support.

## 🎯 What You'll Learn

- Go syntax and core language features
- Data types, functions, and control flow
- Structs, interfaces, and methods
- Goroutines and channels for concurrency
- Error handling the Go way
- Package management and project structure
- Testing in Go

## 📋 Prerequisites

- Basic programming knowledge in any language
- Familiarity with command line/terminal
- Understanding of basic computer science concepts

## 🚀 Why Go?

- **Simple syntax** - Easy to learn, minimal keywords
- **Fast compilation** - Near-instant builds
- **Built-in concurrency** - Goroutines and channels are first-class features
- **Strong standard library** - Batteries included for web, networking, crypto
- **Static typing** - Catch errors at compile time
- **Fast execution** - Compiled to native machine code
- **Great tooling** - `go fmt`, `go test`, `go vet` built-in

## 📦 Setup

### Installation

Visit [go.dev/dl](https://go.dev/dl) and download the installer for your platform.

Verify installation:

```bash
go version
# Should output: go version go1.25.x ... (or go1.24.x, go1.23.x)
```

### Your First Program

Create `hello.go`:

```go
package main

import "fmt"

func main() {
	fmt.Println("Hello, World!")
}
```

Run it:

```bash
go run hello.go
# Output: Hello, World!
```

## 🔤 Basic Syntax

### Variables

```go
package main

import "fmt"

func main() {
	// Explicit type declaration
	var name string = "Alice"
	var age int = 30

	// Type inference
	var city = "Jakarta"

	// Short declaration (inside functions only)
	country := "Indonesia"

	// Multiple declarations
	var (
		x int = 10
		y int = 20
		z     = 30  // type inferred
	)

	// Constants
	const Pi = 3.14159
	const MaxRetries = 3

	fmt.Println(name, age, city, country, x, y, z, Pi, MaxRetries)
}
```

### Basic Types

```go
// Integers
var i int = 42           // Platform dependent (32 or 64 bit)
var i8 int8 = 127        // -128 to 127
var i16 int16 = 32767    // -32768 to 32767
var i32 int32 = 2147483647
var i64 int64 = 9223372036854775807

var u uint = 42          // Unsigned, platform dependent
var u8 uint8 = 255       // 0 to 255 (also called byte)
var u16 uint16 = 65535
var u32 uint32 = 4294967295
var u64 uint64 = 18446744073709551615

// Floating point
var f32 float32 = 3.14
var f64 float64 = 3.14159265359

// Boolean
var isActive bool = true

// String
var message string = "Hello, Go!"

// Rune (Unicode code point, alias for int32)
var r rune = '€'
```

### Zero Values

Variables without explicit initialization get zero values:

```go
var i int       // 0
var f float64   // 0.0
var b bool      // false
var s string    // "" (empty string)
var p *int      // nil
```

## 🔄 Control Flow

### If/Else

```go
package main

import "fmt"

func main() {
	age := 18

	// Basic if
	if age >= 18 {
		fmt.Println("Adult")
	}

	// If with else
	if age >= 18 {
		fmt.Println("Adult")
	} else {
		fmt.Println("Minor")
	}

	// If with initialization statement
	if score := 85; score >= 90 {
		fmt.Println("Grade: A")
	} else if score >= 80 {
		fmt.Println("Grade: B")
	} else {
		fmt.Println("Grade: C")
	}
	// Note: 'score' is only in scope within the if/else block
}
```

### Switch

```go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Basic switch
	day := "Monday"
	switch day {
	case "Monday":
		fmt.Println("Start of work week")
	case "Friday":
		fmt.Println("TGIF!")
	case "Saturday", "Sunday":
		fmt.Println("Weekend!")
	default:
		fmt.Println("Midweek")
	}

	// Switch with no condition (like if-else chain)
	hour := time.Now().Hour()
	switch {
	case hour < 12:
		fmt.Println("Good morning")
	case hour < 17:
		fmt.Println("Good afternoon")
	default:
		fmt.Println("Good evening")
	}

	// Type switch
	var i interface{} = "hello"
	switch v := i.(type) {
	case int:
		fmt.Printf("Integer: %d\n", v)
	case string:
		fmt.Printf("String: %s\n", v)
	default:
		fmt.Printf("Unknown type: %T\n", v)
	}
}
```

### Loops

Go has only `for` loops (no while or do-while):

```go
package main

import "fmt"

func main() {
	// Traditional for loop
	for i := 0; i < 5; i++ {
		fmt.Println(i)
	}

	// While-style loop
	count := 0
	for count < 3 {
		fmt.Println("Count:", count)
		count++
	}

	// Infinite loop
	// for {
	//     fmt.Println("Forever")
	//     // Use break to exit
	// }

	// Range over slice
	numbers := []int{10, 20, 30, 40}
	for index, value := range numbers {
		fmt.Printf("Index: %d, Value: %d\n", index, value)
	}

	// Ignore index with _
	for _, value := range numbers {
		fmt.Println(value)
	}

	// Range over map
	scores := map[string]int{"Alice": 90, "Bob": 85}
	for name, score := range scores {
		fmt.Printf("%s scored %d\n", name, score)
	}

	// Range over string (iterates over runes)
	for index, char := range "Hello" {
		fmt.Printf("%d: %c\n", index, char)
	}
}
```

## 📊 Data Structures

### Arrays

Fixed-length, same-type collections:

```go
package main

import "fmt"

func main() {
	// Declare array
	var arr [5]int
	arr[0] = 10
	arr[1] = 20

	// Array literal
	primes := [5]int{2, 3, 5, 7, 11}

	// Let compiler count length
	days := [...]string{"Mon", "Tue", "Wed", "Thu", "Fri"}

	fmt.Println(arr)    // [10 20 0 0 0]
	fmt.Println(primes) // [2 3 5 7 11]
	fmt.Println(days)   // [Mon Tue Wed Thu Fri]
	fmt.Println(len(days)) // 5
}
```

### Slices

Dynamic-length sequences (most commonly used):

```go
package main

import "fmt"

func main() {
	// Create slice from array
	arr := [5]int{1, 2, 3, 4, 5}
	slice := arr[1:4] // [2 3 4] - from index 1 to 3

	// Slice literals
	numbers := []int{10, 20, 30, 40}

	// Make slice with make()
	s := make([]int, 5)      // length 5, capacity 5
	s2 := make([]int, 3, 10) // length 3, capacity 10

	// Append to slice
	numbers = append(numbers, 50)
	numbers = append(numbers, 60, 70, 80)

	// Copy slice
	dest := make([]int, len(numbers))
	copy(dest, numbers)

	// Slicing operations
	fmt.Println(numbers[:3])  // First 3 elements
	fmt.Println(numbers[2:])  // From index 2 to end
	fmt.Println(numbers[1:4]) // From index 1 to 3

	fmt.Println("Length:", len(numbers))
	fmt.Println("Capacity:", cap(numbers))
}
```

### Maps

Key-value pairs:

```go
package main

import "fmt"

func main() {
	// Create map with make
	ages := make(map[string]int)
	ages["Alice"] = 30
	ages["Bob"] = 25

	// Map literal
	scores := map[string]int{
		"Alice": 90,
		"Bob":   85,
		"Carol": 95,
	}

	// Access value
	fmt.Println(scores["Alice"]) // 90

	// Check if key exists
	score, exists := scores["David"]
	if exists {
		fmt.Println("David's score:", score)
	} else {
		fmt.Println("David not found")
	}

	// Delete key
	delete(scores, "Bob")

	// Iterate over map
	for name, score := range scores {
		fmt.Printf("%s: %d\n", name, score)
	}
}
```

### Structs

Custom data types:

```go
package main

import "fmt"

// Define struct
type Person struct {
	Name string
	Age  int
	City string
}

// Struct with tags (for JSON, etc.)
type User struct {
	ID       int    `json:"id"`
	Username string `json:"username"`
	Email    string `json:"email"`
}

func main() {
	// Create struct instance
	p1 := Person{
		Name: "Alice",
		Age:  30,
		City: "Jakarta",
	}

	// Positional initialization (not recommended)
	p2 := Person{"Bob", 25, "Bandung"}

	// Partial initialization
	p3 := Person{Name: "Carol"}

	// Access fields
	fmt.Println(p1.Name) // Alice

	// Modify fields
	p1.Age = 31

	// Pointer to struct
	p4 := &Person{Name: "David", Age: 35}
	p4.City = "Surabaya" // Automatic dereferencing

	// Anonymous struct
	config := struct {
		Host string
		Port int
	}{
		Host: "localhost",
		Port: 8080,
	}

	fmt.Println(p1, p2, p3, p4, config)
}
```

## 🔧 Functions

### Basic Functions

```go
package main

import "fmt"

// Simple function
func greet(name string) {
	fmt.Println("Hello,", name)
}

// Function with return value
func add(a int, b int) int {
	return a + b
}

// Shortened parameter syntax (same type)
func multiply(a, b int) int {
	return a * b
}

// Multiple return values
func divide(a, b float64) (float64, error) {
	if b == 0 {
		return 0, fmt.Errorf("division by zero")
	}
	return a / b, nil
}

// Named return values
func split(sum int) (x, y int) {
	x = sum * 4 / 9
	y = sum - x
	return // naked return
}

// Variadic function
func sum(numbers ...int) int {
	total := 0
	for _, n := range numbers {
		total += n
	}
	return total
}

func main() {
	greet("Alice")

	result := add(10, 20)
	fmt.Println("Add:", result)

	quotient, err := divide(10, 2)
	if err != nil {
		fmt.Println("Error:", err)
	} else {
		fmt.Println("Quotient:", quotient)
	}

	x, y := split(17)
	fmt.Println("Split:", x, y)

	total := sum(1, 2, 3, 4, 5)
	fmt.Println("Sum:", total)
}
```

### Defer

Execute function call after surrounding function returns:

```go
package main

import "fmt"

func main() {
	// Defer executes in LIFO order
	defer fmt.Println("World")
	defer fmt.Println("Beautiful")
	defer fmt.Println("Hello")

	fmt.Println("Start")
	// Output:
	// Start
	// Hello
	// Beautiful
	// World
}

// Common use: cleanup resources
// Required import: "os"
func readFile(filename string) error {
	file, err := os.Open(filename)
	if err != nil {
		return err
	}
	defer file.Close() // Ensures file is closed when function returns

	// Read file operations...
	return nil
}
```

### Anonymous Functions and Closures

```go
package main

import "fmt"

func main() {
	// Anonymous function
	func() {
		fmt.Println("Anonymous function")
	}()

	// Assign to variable
	add := func(a, b int) int {
		return a + b
	}
	fmt.Println(add(5, 3))

	// Closure
	counter := func() func() int {
		count := 0
		return func() int {
			count++
			return count
		}
	}()

	fmt.Println(counter()) // 1
	fmt.Println(counter()) // 2
	fmt.Println(counter()) // 3
}
```

## 🔗 Methods and Interfaces

### Methods

Functions with a receiver argument:

```go
package main

import (
	"fmt"
	"math"
)

type Rectangle struct {
	Width  float64
	Height float64
}

type Circle struct {
	Radius float64
}

// Method with value receiver
func (r Rectangle) Area() float64 {
	return r.Width * r.Height
}

// Method with pointer receiver (can modify struct)
func (r *Rectangle) Scale(factor float64) {
	r.Width *= factor
	r.Height *= factor
}

// Method on Circle
func (c Circle) Area() float64 {
	return math.Pi * c.Radius * c.Radius
}

func main() {
	rect := Rectangle{Width: 10, Height: 5}
	fmt.Println("Area:", rect.Area()) // 50

	rect.Scale(2)
	fmt.Println("Scaled area:", rect.Area()) // 200

	circle := Circle{Radius: 5}
	fmt.Println("Circle area:", circle.Area())
}
```

### Interfaces

```go
package main

import (
	"fmt"
	"math"
)

// Interface definition
type Shape interface {
	Area() float64
	Perimeter() float64
}

type Rectangle struct {
	Width, Height float64
}

type Circle struct {
	Radius float64
}

// Rectangle implements Shape interface
func (r Rectangle) Area() float64 {
	return r.Width * r.Height
}

func (r Rectangle) Perimeter() float64 {
	return 2 * (r.Width + r.Height)
}

// Circle implements Shape interface
func (c Circle) Area() float64 {
	return math.Pi * c.Radius * c.Radius
}

func (c Circle) Perimeter() float64 {
	return 2 * math.Pi * c.Radius
}

// Function that accepts any Shape
func printShapeInfo(s Shape) {
	fmt.Printf("Area: %.2f, Perimeter: %.2f\n", s.Area(), s.Perimeter())
}

func main() {
	rect := Rectangle{Width: 10, Height: 5}
	circle := Circle{Radius: 5}

	printShapeInfo(rect)
	printShapeInfo(circle)

	// Empty interface (interface{}) accepts any type
	var anything interface{}
	anything = 42
	anything = "hello"
	anything = rect

	fmt.Println(anything)
}
```

### Type Assertions and Type Switches

```go
package main

import "fmt"

func describe(i interface{}) {
	// Type assertion
	if s, ok := i.(string); ok {
		fmt.Printf("String of length %d: %s\n", len(s), s)
		return
	}

	// Type switch
	switch v := i.(type) {
	case int:
		fmt.Printf("Integer: %d\n", v)
	case float64:
		fmt.Printf("Float: %.2f\n", v)
	case bool:
		fmt.Printf("Boolean: %t\n", v)
	default:
		fmt.Printf("Unknown type: %T\n", v)
	}
}

func main() {
	describe(42)
	describe(3.14)
	describe("hello")
	describe(true)
}
```

## 🎯 Pointers

Go has pointers but no pointer arithmetic:

```go
package main

import "fmt"

func main() {
	// Declare variable
	x := 42

	// & creates pointer to variable
	p := &x

	fmt.Println("Value of x:", x)   // 42
	fmt.Println("Address of x:", p) // 0xc0000...
	fmt.Println("Value at p:", *p)  // 42 (dereferencing)

	// Modify value through pointer
	*p = 100
	fmt.Println("New value of x:", x) // 100

	// Pass by value vs pointer
	changeValue(x)
	fmt.Println("After changeValue:", x) // 100 (unchanged)

	changePointer(&x)
	fmt.Println("After changePointer:", x) // 200 (changed)
}

func changeValue(val int) {
	val = 500
}

func changePointer(ptr *int) {
	*ptr = 200
}
```

## ⚠️ Error Handling

Go doesn't have exceptions; it uses explicit error returns:

```go
package main

import (
	"errors"
	"fmt"
)

// Return error
func divide(a, b float64) (float64, error) {
	if b == 0 {
		return 0, errors.New("division by zero")
	}
	return a / b, nil
}

// Custom error with formatting
func validateAge(age int) error {
	if age < 0 {
		return fmt.Errorf("invalid age: %d (must be non-negative)", age)
	}
	if age < 18 {
		return fmt.Errorf("age %d is below minimum (18)", age)
	}
	return nil
}

// Custom error type
type ValidationError struct {
	Field   string
	Message string
}

func (e *ValidationError) Error() string {
	return fmt.Sprintf("validation error on %s: %s", e.Field, e.Message)
}

func validateUser(username string) error {
	if len(username) < 3 {
		return &ValidationError{
			Field:   "username",
			Message: "must be at least 3 characters",
		}
	}
	return nil
}

func main() {
	// Handle errors with if
	result, err := divide(10, 0)
	if err != nil {
		fmt.Println("Error:", err)
	} else {
		fmt.Println("Result:", result)
	}

	// Check and use error
	if err := validateAge(15); err != nil {
		fmt.Println(err)
	}

	// Type assertion on custom error
	if err := validateUser("ab"); err != nil {
		if ve, ok := err.(*ValidationError); ok {
			fmt.Printf("Field: %s, Message: %s\n", ve.Field, ve.Message)
		}
	}

	// Panic and recover (use sparingly!)
	safeDivide(10, 0)
}

func safeDivide(a, b int) {
	defer func() {
		if r := recover(); r != nil {
			fmt.Println("Recovered from panic:", r)
		}
	}()

	if b == 0 {
		panic("division by zero!")
	}
	fmt.Println(a / b)
}
```

## 🚀 Concurrency

Go's killer feature - goroutines and channels:

### Goroutines

Lightweight threads managed by Go runtime:

```go
package main

import (
	"fmt"
	"time"
)

func say(s string) {
	for i := 0; i < 3; i++ {
		time.Sleep(100 * time.Millisecond)
		fmt.Println(s)
	}
}

func main() {
	// Run in goroutine
	go say("world")

	// Run in main goroutine
	say("hello")

	// Without sleep/wait, main might exit before goroutines finish
	time.Sleep(1 * time.Second)
}
```

### Channels

Communication between goroutines:

```go
package main

import "fmt"

func main() {
	// Create channel
	ch := make(chan int)

	// Send and receive in different goroutines
	go func() {
		ch <- 42 // Send to channel
	}()

	value := <-ch // Receive from channel
	fmt.Println(value)

	// Buffered channel
	buffered := make(chan string, 2)
	buffered <- "hello"
	buffered <- "world"
	// No blocking until buffer is full

	fmt.Println(<-buffered)
	fmt.Println(<-buffered)
}
```

### Channel Patterns

```go
package main

import (
	"fmt"
	"time"
)

// Worker pattern
func worker(id int, jobs <-chan int, results chan<- int) {
	for j := range jobs {
		fmt.Printf("Worker %d processing job %d\n", id, j)
		time.Sleep(time.Second)
		results <- j * 2
	}
}

func main() {
	jobs := make(chan int, 5)
	results := make(chan int, 5)

	// Start 3 workers
	for w := 1; w <= 3; w++ {
		go worker(w, jobs, results)
	}

	// Send 5 jobs
	for j := 1; j <= 5; j++ {
		jobs <- j
	}
	close(jobs)

	// Collect results
	for a := 1; a <= 5; a++ {
		fmt.Println("Result:", <-results)
	}
}
```

### Select Statement

Multiplex channel operations:

```go
package main

import (
	"fmt"
	"time"
)

func main() {
	ch1 := make(chan string)
	ch2 := make(chan string)

	go func() {
		time.Sleep(1 * time.Second)
		ch1 <- "one"
	}()

	go func() {
		time.Sleep(2 * time.Second)
		ch2 <- "two"
	}()

	// Wait for both channels
	for i := 0; i < 2; i++ {
		select {
		case msg1 := <-ch1:
			fmt.Println("Received from ch1:", msg1)
		case msg2 := <-ch2:
			fmt.Println("Received from ch2:", msg2)
		case <-time.After(3 * time.Second):
			fmt.Println("Timeout!")
		}
	}
}
```

### WaitGroup

Wait for goroutines to finish:

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

### Mutex

Protect shared data:

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

## 📦 Packages and Modules

### Package Basics

Every Go file belongs to a package:

```go
// math/calculator.go
package math

// Exported (public) - starts with capital letter
func Add(a, b int) int {
	return a + b
}

// Unexported (private) - starts with lowercase
func multiply(a, b int) int {
	return a * b
}
```

### Importing Packages

```go
package main

import (
	"fmt"           // Standard library
	"math/rand"     // Standard library

	// Custom package
	"github.com/user/project/math"
)

func main() {
	fmt.Println(math.Add(5, 3))
	fmt.Println(rand.Intn(100))
}
```

### Go Modules

Initialize a new module:

```bash
go mod init github.com/username/projectname
```

This creates `go.mod`:

```
module github.com/username/projectname

go 1.25
```

Add dependencies:

```bash
go get github.com/gin-gonic/gin
```

Tidy up dependencies:

```bash
go mod tidy
```

### Project Structure

```
myproject/
├── go.mod
├── go.sum
├── main.go
├── internal/          # Private packages
│   └── auth/
│       └── auth.go
├── pkg/              # Public packages
│   └── models/
│       └── user.go
└── cmd/              # Multiple executables
    ├── server/
    │   └── main.go
    └── worker/
        └── main.go
```

## 🧪 Testing

Go has built-in testing support:

### Basic Tests

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

func TestAddNegative(t *testing.T) {
	result := Add(-2, -3)
	expected := -5

	if result != expected {
		t.Errorf("Add(-2, -3) = %d; want %d", result, expected)
	}
}

func TestDivide(t *testing.T) {
	result, err := Divide(10, 2)
	if err != nil {
		t.Errorf("Unexpected error: %v", err)
	}

	expected := 5.0
	if result != expected {
		t.Errorf("Divide(10, 2) = %.2f; want %.2f", result, expected)
	}
}

func TestDivideByZero(t *testing.T) {
	_, err := Divide(10, 0)
	if err == nil {
		t.Error("Expected error for division by zero, got nil")
	}
}
```

Run tests:

```bash
go test              # Run tests in current package
go test ./...        # Run tests in all packages
go test -v           # Verbose output
go test -cover       # Show coverage
go test -run TestAdd # Run specific test
```

### Table-Driven Tests

```go
func TestAddTableDriven(t *testing.T) {
	tests := []struct {
		name     string
		a, b     int
		expected int
	}{
		{"positive numbers", 2, 3, 5},
		{"negative numbers", -2, -3, -5},
		{"mixed signs", -2, 3, 1},
		{"with zero", 0, 5, 5},
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

### Benchmarks

```go
func BenchmarkAdd(b *testing.B) {
	for i := 0; i < b.N; i++ {
		Add(2, 3)
	}
}
```

Run benchmarks:

```bash
go test -bench=.
go test -bench=BenchmarkAdd
```

## 🎨 Common Patterns

### Error Wrapping (Go 1.13+)

```go
import (
	"fmt"
	"errors"
)

func readConfig() error {
	err := openFile()
	if err != nil {
		return fmt.Errorf("failed to read config: %w", err)
	}
	return nil
}

// Check wrapped error
if errors.Is(err, ErrNotFound) {
	// Handle not found
}

// Unwrap to check type
var pathErr *os.PathError
if errors.As(err, &pathErr) {
	// Handle path error
}
```

### Context for Cancellation

```go
import (
	"context"
	"fmt"
	"time"
)

func doWork(ctx context.Context) error {
	for {
		select {
		case <-ctx.Done():
			return ctx.Err() // Cancelled or timed out
		default:
			// Do work
			time.Sleep(100 * time.Millisecond)
		}
	}
}

func main() {
	// Context with timeout
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Second)
	defer cancel()

	if err := doWork(ctx); err != nil {
		fmt.Println("Error:", err)
	}
}
```

### Functional Options Pattern

```go
type Server struct {
	host string
	port int
	timeout time.Duration
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

func NewServer(opts ...Option) *Server {
	// Defaults
	s := &Server{
		host: "localhost",
		port: 8080,
		timeout: 30 * time.Second,
	}

	// Apply options
	for _, opt := range opts {
		opt(s)
	}

	return s
}

// Usage
server := NewServer(
	WithHost("0.0.0.0"),
	WithPort(9000),
)
```

### Builder Pattern

```go
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
	// Build SQL query
	return fmt.Sprintf("SELECT %s FROM %s WHERE %s LIMIT %d",
		strings.Join(q.fields, ", "), q.table, q.where, q.limit)
}

// Usage
query := NewQuery("users").
	Select("id", "name", "email").
	Where("age > 18").
	Limit(10).
	Build()
```

## 📚 Next Steps

### Essential Reading

- **Official Tour**: [tour.golang.org](https://tour.golang.org)
- **Effective Go**: [go.dev/doc/effective_go](https://go.dev/doc/effective_go)
- **Go by Example**: [gobyexample.com](https://gobyexample.com)

### Practice Projects

- **CLI Tool** - Build a command-line utility with `flag` or `cobra`
- **REST API** - Create an API with `net/http` or `gin`
- **Concurrent Worker Pool** - Process jobs using goroutines and channels
- **Web Scraper** - Parse HTML with `goquery`, use goroutines for concurrency

### Advanced Topics

- **Reflection** - Runtime type inspection with `reflect` package
- **Generics** - Type parameters (Go 1.18+)
- **Performance** - Profiling with pprof, optimization techniques
- **CGo** - Calling C code from Go
- **Assembly** - Understanding Go's assembly output

### Popular Packages

- **Web Frameworks**: Gin, Echo, Fiber
- **Database**: GORM, sqlx, pgx
- **Testing**: Testify, GoMock
- **CLI**: Cobra, urfave/cli
- **HTTP Client**: resty
- **Validation**: validator
- **Configuration**: Viper
- **Logging**: Zap, Logrus

## 🎓 Summary

You've learned:

- ✅ Go syntax, types, and variables
- ✅ Control flow (if, switch, for, range)
- ✅ Data structures (arrays, slices, maps, structs)
- ✅ Functions, methods, and interfaces
- ✅ Pointers and memory
- ✅ Error handling patterns
- ✅ Concurrency with goroutines and channels
- ✅ Packages, modules, and project structure
- ✅ Testing and benchmarking
- ✅ Common Go idioms and patterns

Go is designed for building **simple, reliable, and efficient software**. The language is intentionally minimal, forcing you to write explicit, readable code.

**Key Go Philosophy**:

- Simplicity over cleverness
- Explicit over implicit
- Composition over inheritance
- Concurrency as a first-class feature

Now go build something! 🚀
