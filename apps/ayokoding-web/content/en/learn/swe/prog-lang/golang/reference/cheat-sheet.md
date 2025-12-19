---
title: Go Cheat Sheet
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 801
description: Quick reference for Go syntax, patterns, and common operations
tags: ["golang", "go", "cheat-sheet", "reference"]
---

## Basic Syntax

### Variables and Types

```go
// Variable declaration
var name string = "Alice"
var age int = 30
var isActive bool = true

// Short declaration (type inference)
name := "Alice"
age := 30

// Multiple declarations
var (
    firstName string = "John"
    lastName  string = "Doe"
    age       int    = 25
)

// Constants
const Pi = 3.14159
const (
    StatusOK    = 200
    StatusError = 500
)

// iota (auto-increment)
const (
    Sunday = iota  // 0
    Monday         // 1
    Tuesday        // 2
)
```

### Basic Types

```go
// Numeric types
var i int = 42           // Platform-dependent size
var i8 int8 = 127        // 8-bit
var i16 int16 = 32767    // 16-bit
var i32 int32 = 2147483647  // 32-bit (rune)
var i64 int64 = 9223372036854775807  // 64-bit

var u uint = 42          // Unsigned int
var u8 uint8 = 255       // 8-bit (byte)

var f32 float32 = 3.14
var f64 float64 = 3.14159265359

// String and rune
var s string = "Hello, 世界"
var r rune = '世'  // int32 alias

// Boolean
var b bool = true
```

## Control Flow

### If-Else

```go
// Basic if
if x > 0 {
    fmt.Println("Positive")
}

// If with initialization
if err := doSomething(); err != nil {
    return err
}

// If-else
if x > 0 {
    fmt.Println("Positive")
} else if x < 0 {
    fmt.Println("Negative")
} else {
    fmt.Println("Zero")
}
```

### Switch

```go
// Basic switch
switch day {
case "Monday":
    fmt.Println("Start of week")
case "Friday":
    fmt.Println("End of week")
default:
    fmt.Println("Midweek")
}

// Switch without expression
switch {
case age < 18:
    fmt.Println("Minor")
case age < 65:
    fmt.Println("Adult")
default:
    fmt.Println("Senior")
}

// Type switch
switch v := value.(type) {
case string:
    fmt.Printf("String: %s\n", v)
case int:
    fmt.Printf("Int: %d\n", v)
default:
    fmt.Printf("Unknown: %v\n", v)
}
```

### Loops

```go
// For loop (only loop in Go)
for i := 0; i < 10; i++ {
    fmt.Println(i)
}

// While-style loop
i := 0
for i < 10 {
    fmt.Println(i)
    i++
}

// Infinite loop
for {
    // Break with condition
    if done {
        break
    }
}

// Range over slice
for i, v := range slice {
    fmt.Printf("Index: %d, Value: %v\n", i, v)
}

// Range over map
for key, value := range myMap {
    fmt.Printf("Key: %s, Value: %v\n", key, value)
}

// Ignore index/key with _
for _, v := range slice {
    fmt.Println(v)
}
```

## Data Structures

### Arrays and Slices

```go
// Array (fixed size)
var arr [5]int
arr[0] = 1

// Array literal
arr := [3]string{"a", "b", "c"}
arr := [...]int{1, 2, 3, 4, 5}  // Size inferred

// Slice (dynamic size)
var slice []int
slice = []int{1, 2, 3}

// Make slice
slice := make([]int, 5)      // Length 5, capacity 5
slice := make([]int, 5, 10)  // Length 5, capacity 10

// Append
slice = append(slice, 6)
slice = append(slice, 7, 8, 9)

// Slicing
sub := slice[1:4]  // Elements 1, 2, 3
sub := slice[:3]   // First 3 elements
sub := slice[2:]   // From index 2 to end
sub := slice[:]    // All elements

// Copy
dst := make([]int, len(src))
copy(dst, src)
```

### Maps

```go
// Declare map
var m map[string]int

// Initialize with make
m = make(map[string]int)

// Map literal
m := map[string]int{
    "Alice": 25,
    "Bob":   30,
}

// Access
age := m["Alice"]

// Check existence
age, ok := m["Alice"]
if ok {
    fmt.Println("Found:", age)
}

// Insert/Update
m["Charlie"] = 35

// Delete
delete(m, "Bob")

// Iterate
for key, value := range m {
    fmt.Printf("%s: %d\n", key, value)
}
```

## Functions

### Basic Functions

```go
// Function declaration
func add(x int, y int) int {
    return x + y
}

// Shortened parameter types
func add(x, y int) int {
    return x + y
}

// Multiple return values
func divide(x, y float64) (float64, error) {
    if y == 0 {
        return 0, errors.New("division by zero")
    }
    return x / y, nil
}

// Named return values
func split(sum int) (x, y int) {
    x = sum * 4 / 9
    y = sum - x
    return  // Naked return
}

// Variadic function
func sum(numbers ...int) int {
    total := 0
    for _, n := range numbers {
        total += n
    }
    return total
}

// Usage
result := sum(1, 2, 3, 4, 5)
```

### Methods

```go
// Method on struct
type Rectangle struct {
    Width, Height float64
}

// Value receiver
func (r Rectangle) Area() float64 {
    return r.Width * r.Height
}

// Pointer receiver (modifies receiver)
func (r *Rectangle) Scale(factor float64) {
    r.Width *= factor
    r.Height *= factor
}

// Usage
rect := Rectangle{Width: 10, Height: 5}
area := rect.Area()
rect.Scale(2)
```

## Structs and Interfaces

### Structs

```go
// Define struct
type Person struct {
    Name string
    Age  int
}

// Create struct
p := Person{Name: "Alice", Age: 30}
p := Person{"Alice", 30}  // Positional

// Anonymous struct
point := struct {
    X, Y int
}{10, 20}

// Embedded struct
type Employee struct {
    Person  // Embedded
    EmpID int
}

emp := Employee{
    Person: Person{"Alice", 30},
    EmpID:  12345,
}
fmt.Println(emp.Name)  // Access embedded field
```

### Interfaces

```go
// Define interface
type Shape interface {
    Area() float64
    Perimeter() float64
}

// Implement interface (implicit)
type Rectangle struct {
    Width, Height float64
}

func (r Rectangle) Area() float64 {
    return r.Width * r.Height
}

func (r Rectangle) Perimeter() float64 {
    return 2 * (r.Width + r.Height)
}

// Use interface
func printArea(s Shape) {
    fmt.Println("Area:", s.Area())
}

// Empty interface
var i interface{}
i = 42
i = "hello"
i = []int{1, 2, 3}

// Type assertion
var i interface{} = "hello"
s := i.(string)

// Type assertion with check
s, ok := i.(string)
if ok {
    fmt.Println(s)
}
```

## Error Handling

```go
// Define error
var ErrNotFound = errors.New("not found")

// Return error
func findUser(id string) (*User, error) {
    if id == "" {
        return nil, errors.New("empty id")
    }
    // ...
    return user, nil
}

// Check error
user, err := findUser("123")
if err != nil {
    return err
}

// Wrap error (Go 1.13+)
if err != nil {
    return fmt.Errorf("failed to find user: %w", err)
}

// Custom error type
type ValidationError struct {
    Field   string
    Message string
}

func (e *ValidationError) Error() string {
    return fmt.Sprintf("%s: %s", e.Field, e.Message)
}
```

## Goroutines and Channels

### Goroutines

```go
// Start goroutine
go func() {
    fmt.Println("Running in goroutine")
}()

// With parameters
go printMessage("Hello")

func printMessage(msg string) {
    fmt.Println(msg)
}
```

### Channels

```go
// Create channel
ch := make(chan int)

// Buffered channel
ch := make(chan int, 10)

// Send to channel
ch <- 42

// Receive from channel
value := <-ch

// Close channel
close(ch)

// Range over channel
for value := range ch {
    fmt.Println(value)
}

// Select (multiplex channels)
select {
case msg := <-ch1:
    fmt.Println("Received from ch1:", msg)
case msg := <-ch2:
    fmt.Println("Received from ch2:", msg)
case <-time.After(1 * time.Second):
    fmt.Println("Timeout")
}
```

## Defer, Panic, Recover

```go
// Defer (executes after function returns)
func readFile(filename string) error {
    f, err := os.Open(filename)
    if err != nil {
        return err
    }
    defer f.Close()  // Closes when function returns

    // Read file...
    return nil
}

// Panic (stop execution)
func mustConnect() {
    if err := connect(); err != nil {
        panic("connection failed")
    }
}

// Recover (catch panic)
func safeCall() {
    defer func() {
        if r := recover(); r != nil {
            fmt.Println("Recovered from panic:", r)
        }
    }()

    // Code that might panic
    mayPanic()
}
```

## Common Patterns

### Constructor Pattern

```go
type Server struct {
    host string
    port int
}

func NewServer(host string, port int) *Server {
    return &Server{
        host: host,
        port: port,
    }
}

// Usage
server := NewServer("localhost", 8080)
```

### Functional Options

```go
type Server struct {
    host    string
    port    int
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
    s := &Server{
        host: "localhost",
        port: 8080,
    }
    for _, opt := range opts {
        opt(s)
    }
    return s
}

// Usage
server := NewServer(WithHost("0.0.0.0"), WithPort(9000))
```

## Related Resources

**Learn more**:

- [Glossary](/en/learn/swe/prog-lang/golang/reference/glossary) - Go terminology
- [Resources](/en/learn/swe/prog-lang/golang/reference/resources) - Books and docs
- [Cookbook](/en/learn/swe/prog-lang/golang/how-to/cookbook) - Practical recipes
