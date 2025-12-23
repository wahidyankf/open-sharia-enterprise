---
title: Go Glossary
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 1000040
description: Go-specific terminology and definitions for learners and developers
---

**Comprehensive glossary** of Go-specific terms, concepts, and language features. Organized alphabetically for quick reference.

## B

### Buffered Channel

**Definition**: Channel with capacity to hold values before blocking. Send operations block only when buffer is full.

**Example**:

```go
ch := make(chan int, 3)  // Buffer size 3
ch <- 1  // Non-blocking
ch <- 2  // Non-blocking
ch <- 3  // Non-blocking
ch <- 4  // Blocks until space available
```

**See Also**: Channel, Unbuffered Channel, Select

### Build Constraint (Build Tag)

**Definition**: Special comment that controls when a file is compiled based on conditions like OS, architecture, or custom tags.

**Example**:

```go
//go:build linux && amd64
// +build linux,amd64

package mypackage
```

**See Also**: Conditional Compilation, Package

## C

### Cap (Capacity)

**Definition**: Built-in function that returns the capacity of a slice, array, or channel.

**Example**:

```go
slice := make([]int, 3, 5)  // len=3, cap=5
fmt.Println(cap(slice))      // 5

ch := make(chan int, 10)
fmt.Println(cap(ch))         // 10
```

**See Also**: Len, Slice, Make

### Channel

**Definition**: Typed conduit for communication between goroutines. Provides synchronization and message passing.

**Example**:

```go
ch := make(chan int)

// Sender goroutine
go func() {
    ch <- 42
}()

// Receiver
value := <-ch
fmt.Println(value)  // 42
```

**See Also**: Goroutine, Select, Buffered Channel, Close

### Close

**Definition**: Built-in function that closes a channel, signaling no more values will be sent.

**Example**:

```go
ch := make(chan int)

go func() {
    ch <- 1
    ch <- 2
    close(ch)  // Signal completion
}()

for value := range ch {
    fmt.Println(value)
}
```

**See Also**: Channel, Range, Select

### Closure

**Definition**: Function value that references variables from outside its body. The function "closes over" these variables.

**Example**:

```go
func counter() func() int {
    count := 0
    return func() int {
        count++
        return count
    }
}

c := counter()
fmt.Println(c())  // 1
fmt.Println(c())  // 2
```

**See Also**: Anonymous Function, Function Value

### Composite Literal

**Definition**: Syntax for creating instances of structs, arrays, slices, and maps in one expression.

**Example**:

```go
// Struct literal
person := Person{Name: "Alice", Age: 30}

// Slice literal
numbers := []int{1, 2, 3, 4, 5}

// Map literal
config := map[string]int{
    "timeout": 30,
    "retries": 3,
}
```

**See Also**: Struct, Slice, Map, Array

### Concurrency

**Definition**: Composition of independently executing computations. In Go, managed through goroutines and channels.

**Example**:

```go
func fetchData(url string, ch chan<- string) {
    // Simulate fetch
    ch <- "Data from " + url
}

ch := make(chan string)
go fetchData("api.example.com", ch)
go fetchData("api2.example.com", ch)

fmt.Println(<-ch)
fmt.Println(<-ch)
```

**See Also**: Goroutine, Channel, Parallelism, Select

### Context

**Definition**: Package and type for carrying deadlines, cancellation signals, and request-scoped values across API boundaries.

**Example**:

```go
ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
defer cancel()

select {
case <-time.After(3 * time.Second):
    fmt.Println("operation completed")
case <-ctx.Done():
    fmt.Println("timeout:", ctx.Err())
}
```

**See Also**: Context Package, Cancellation, Deadline

## D

### Defer

**Definition**: Statement that schedules a function call to run after the surrounding function returns.

**Example**:

```go
func readFile(filename string) error {
    file, err := os.Open(filename)
    if err != nil {
        return err
    }
    defer file.Close()  // Executes when function returns

    // Read file operations
    return nil
}
```

**See Also**: Panic, Recover, LIFO Order

### Defer Statement Order

**Definition**: Deferred functions execute in LIFO (Last In, First Out) order when function returns.

**Example**:

```go
func example() {
    defer fmt.Println("First")
    defer fmt.Println("Second")
    defer fmt.Println("Third")
}
// Output: Third, Second, First
```

**See Also**: Defer

## E

### Embedding

**Definition**: Composition mechanism where a type includes another type without explicit field name, promoting its methods.

**Example**:

```go
type Engine struct {
    Power int
}

func (e Engine) Start() {
    fmt.Println("Engine started")
}

type Car struct {
    Engine  // Embedded struct
    Model string
}

car := Car{Engine: Engine{Power: 200}, Model: "Sedan"}
car.Start()  // Promoted method from Engine
```

**See Also**: Composition, Interface, Method Promotion

### Empty Interface

**Definition**: Interface type with zero methods (`interface{}`), can hold values of any type. Modern Go uses `any` as alias.

**Example**:

```go
var anything any
anything = 42
anything = "hello"
anything = []int{1, 2, 3}

// Type assertion needed to use
if str, ok := anything.(string); ok {
    fmt.Println(str)
}
```

**See Also**: Interface, Type Assertion, Any

### Error

**Definition**: Built-in interface type for representing error conditions. Conventional way to handle failures in Go.

**Example**:

```go
func divide(a, b float64) (float64, error) {
    if b == 0 {
        return 0, errors.New("division by zero")
    }
    return a / b, nil
}

result, err := divide(10, 0)
if err != nil {
    fmt.Println("Error:", err)
    return
}
```

**See Also**: Error Interface, Errors Package, Error Wrapping

### Error Wrapping

**Definition**: Technique for adding context to errors while preserving the original error chain using `%w` verb.

**Example**:

```go
func readConfig(path string) error {
    file, err := os.Open(path)
    if err != nil {
        return fmt.Errorf("failed to open config: %w", err)
    }
    defer file.Close()
    return nil
}

// Unwrap to check original error
err := readConfig("config.yaml")
if errors.Is(err, os.ErrNotExist) {
    fmt.Println("Config file not found")
}
```

**See Also**: Error, Errors.Is, Errors.As

### Exported

**Definition**: Identifier (variable, function, type, etc.) that is accessible from other packages. Must start with uppercase letter.

**Example**:

```go
package mypackage

var ExportedVar = "accessible"  // Exported
var unexportedVar = "private"   // Unexported

func ExportedFunc() {}  // Exported
func unexportedFunc() {} // Unexported

type ExportedType struct {
    ExportedField   string  // Exported field
    unexportedField int     // Unexported field
}
```

**See Also**: Unexported, Package, Visibility

## F

### Function Value

**Definition**: Functions in Go are first-class values that can be assigned to variables, passed as arguments, and returned.

**Example**:

```go
// Assign to variable
var add func(int, int) int = func(a, b int) int {
    return a + b
}

// Pass as argument
func apply(f func(int) int, value int) int {
    return f(value)
}

result := apply(func(x int) int { return x * 2 }, 5)  // 10
```

**See Also**: Closure, Anonymous Function, Higher-Order Function

## G

### Go Module

**Definition**: Collection of related Go packages versioned together. Modern dependency management system since Go 1.11.

**Example**:

```go
// go.mod file
module github.com/username/myproject

go 1.21

require (
    github.com/gorilla/mux v1.8.0
    github.com/stretchr/testify v1.8.4
)
```

**See Also**: Package, Go.mod, Go.sum, Module Path

### Goroutine

**Definition**: Lightweight thread managed by Go runtime. Function or method executed concurrently.

**Example**:

```go
func sayHello(name string) {
    fmt.Printf("Hello, %s!\n", name)
}

// Launch goroutines
go sayHello("Alice")
go sayHello("Bob")

// Anonymous function goroutine
go func() {
    fmt.Println("Anonymous goroutine")
}()

time.Sleep(time.Second)  // Wait for goroutines
```

**See Also**: Concurrency, Channel, WaitGroup, Go Statement

## I

### Init Function

**Definition**: Special function `init()` that runs automatically before `main()`. Used for package initialization.

**Example**:

```go
package database

var db *sql.DB

func init() {
    var err error
    db, err = sql.Open("postgres", "connection-string")
    if err != nil {
        log.Fatal(err)
    }
}

// Multiple init functions execute in order
func init() {
    fmt.Println("Second init")
}
```

**See Also**: Package Initialization, Main Function

### Interface

**Definition**: Type that specifies method signatures. Types implicitly satisfy interfaces by implementing methods.

**Example**:

```go
type Writer interface {
    Write([]byte) (int, error)
}

type FileWriter struct {
    filename string
}

func (fw FileWriter) Write(data []byte) (int, error) {
    // Implementation
    return len(data), nil
}

// FileWriter implicitly implements Writer
var w Writer = FileWriter{filename: "data.txt"}
```

**See Also**: Method, Type, Duck Typing, Empty Interface

### Interface Satisfaction

**Definition**: Type satisfies an interface by implementing all its methods. No explicit declaration needed (duck typing).

**Example**:

```go
type Animal interface {
    Speak() string
}

type Dog struct{}

func (d Dog) Speak() string {
    return "Woof!"
}

// Dog satisfies Animal interface implicitly
var animal Animal = Dog{}
fmt.Println(animal.Speak())
```

**See Also**: Interface, Duck Typing, Method

## L

### Len (Length)

**Definition**: Built-in function that returns the length of strings, arrays, slices, maps, and channels.

**Example**:

```go
str := "hello"
fmt.Println(len(str))  // 5

slice := []int{1, 2, 3}
fmt.Println(len(slice))  // 3

m := map[string]int{"a": 1}
fmt.Println(len(m))  // 1
```

**See Also**: Cap, Slice, Map, String

## M

### Make

**Definition**: Built-in function for creating slices, maps, and channels. Allocates and initializes internal data structures.

**Example**:

```go
// Create slice with length and capacity
slice := make([]int, 3, 5)  // len=3, cap=5

// Create map
m := make(map[string]int)

// Create channel
ch := make(chan int, 10)  // Buffered channel
```

**See Also**: New, Slice, Map, Channel

### Map

**Definition**: Built-in hash table type mapping keys to values. Reference type.

**Example**:

```go
// Create map
ages := make(map[string]int)
ages["Alice"] = 30
ages["Bob"] = 25

// Map literal
config := map[string]string{
    "host": "localhost",
    "port": "8080",
}

// Check existence
if age, exists := ages["Alice"]; exists {
    fmt.Println(age)
}
```

**See Also**: Make, Slice, Key-Value, Reference Type

### Method

**Definition**: Function with a receiver argument that appears between `func` keyword and method name.

**Example**:

```go
type Rectangle struct {
    Width, Height float64
}

// Method with value receiver
func (r Rectangle) Area() float64 {
    return r.Width * r.Height
}

// Method with pointer receiver
func (r *Rectangle) Scale(factor float64) {
    r.Width *= factor
    r.Height *= factor
}

rect := Rectangle{Width: 10, Height: 5}
fmt.Println(rect.Area())  // 50
rect.Scale(2)
fmt.Println(rect.Area())  // 200
```

**See Also**: Receiver, Pointer Receiver, Value Receiver

### Method Receiver (Pointer)

**Definition**: Method receiver using pointer type (`*T`). Can modify the receiver and avoids copying.

**Example**:

```go
type Counter struct {
    count int
}

// Pointer receiver - can modify
func (c *Counter) Increment() {
    c.count++
}

// Pointer receiver - avoids large copy
func (c *Counter) Reset() {
    c.count = 0
}

counter := &Counter{}
counter.Increment()
fmt.Println(counter.count)  // 1
```

**See Also**: Method, Receiver, Value Receiver, Pointer

### Method Receiver (Value)

**Definition**: Method receiver using value type (`T`). Operates on a copy of the receiver.

**Example**:

```go
type Point struct {
    X, Y float64
}

// Value receiver - cannot modify original
func (p Point) Distance() float64 {
    return math.Sqrt(p.X*p.X + p.Y*p.Y)
}

// Value receiver gets a copy
func (p Point) Move(dx, dy float64) Point {
    return Point{X: p.X + dx, Y: p.Y + dy}
}
```

**See Also**: Method, Receiver, Pointer Receiver

### Mutex

**Definition**: Mutual exclusion lock from `sync` package. Protects shared data in concurrent access.

**Example**:

```go
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

**See Also**: RWMutex, Sync Package, Concurrency, Race Condition

## N

### New

**Definition**: Built-in function that allocates memory for a type, initializes to zero value, and returns pointer.

**Example**:

```go
// Allocate and get pointer
ptr := new(int)
*ptr = 42

// Equivalent to
var value int
ptr := &value

// Common with structs
person := new(Person)
person.Name = "Alice"
```

**See Also**: Make, Pointer, Zero Value

### Nil

**Definition**: Zero value for pointers, interfaces, maps, slices, channels, and function types.

**Example**:

```go
var ptr *int            // nil
var slice []int         // nil slice (not same as empty)
var m map[string]int    // nil map
var ch chan int         // nil channel
var err error           // nil error

if ptr == nil {
    fmt.Println("Pointer is nil")
}

// Nil slice vs empty slice
var nilSlice []int           // nil, len=0, cap=0
emptySlice := []int{}        // not nil, len=0, cap=0
```

**See Also**: Zero Value, Pointer, Slice, Map

## P

### Package

**Definition**: Namespace for organizing related Go code. Unit of compilation and code distribution.

**Example**:

```go
package main  // Executable package

import (
    "fmt"
    "time"
    "github.com/user/mylib"  // External package
)

func main() {
    fmt.Println("Hello")
}
```

**See Also**: Import, Module, Exported, Package Path

### Panic

**Definition**: Built-in function that stops normal execution and begins panicking. Runs deferred functions before crashing.

**Example**:

```go
func riskyOperation(value int) {
    if value < 0 {
        panic("negative value not allowed")
    }
    fmt.Println("Processing:", value)
}

func safeCall() {
    defer func() {
        if r := recover(); r != nil {
            fmt.Println("Recovered from:", r)
        }
    }()

    riskyOperation(-1)  // Panics
    fmt.Println("Never reached")
}
```

**See Also**: Recover, Defer, Error

### Pointer

**Definition**: Variable that stores memory address of another variable. Denoted with `*` for type and `&` for address.

**Example**:

```go
x := 42
ptr := &x  // Pointer to x

fmt.Println(*ptr)  // Dereference: 42
*ptr = 100         // Modify through pointer
fmt.Println(x)     // 100

// Pointer in function
func increment(val *int) {
    *val++
}

increment(&x)
fmt.Println(x)  // 101
```

**See Also**: Reference, Dereference, Address Operator, Method Receiver

## R

### Range

**Definition**: Keyword used in `for` loops to iterate over arrays, slices, maps, strings, and channels.

**Example**:

```go
// Iterate slice
numbers := []int{1, 2, 3, 4, 5}
for index, value := range numbers {
    fmt.Printf("%d: %d\n", index, value)
}

// Iterate map
ages := map[string]int{"Alice": 30, "Bob": 25}
for name, age := range ages {
    fmt.Printf("%s is %d years old\n", name, age)
}

// Iterate string (runes)
for index, char := range "Hello" {
    fmt.Printf("%d: %c\n", index, char)
}

// Iterate channel
ch := make(chan int)
go func() {
    ch <- 1
    ch <- 2
    close(ch)
}()
for value := range ch {
    fmt.Println(value)
}
```

**See Also**: For Loop, Slice, Map, Channel

### Receiver

**Definition**: Parameter that appears before method name, binding the method to a type.

**Example**:

```go
type Person struct {
    Name string
    Age  int
}

// 'p Person' is the receiver
func (p Person) Greet() string {
    return "Hello, I'm " + p.Name
}

// '*p Person' is pointer receiver
func (p *Person) Birthday() {
    p.Age++
}
```

**See Also**: Method, Pointer Receiver, Value Receiver

### Recover

**Definition**: Built-in function that regains control of panicking goroutine. Must be called in deferred function.

**Example**:

```go
func protected() {
    defer func() {
        if r := recover(); r != nil {
            fmt.Println("Recovered from panic:", r)
        }
    }()

    panic("something went wrong")
    fmt.Println("Never executed")
}

protected()
fmt.Println("Program continues")
```

**See Also**: Panic, Defer, Error Handling

### Rune

**Definition**: Alias for `int32`, represents a Unicode code point. Used for character operations.

**Example**:

```go
var char rune = 'A'  // Single quotes for rune literal
fmt.Printf("%c = %d\n", char, char)  // A = 65

// String to runes
str := "Hello, 世界"
runes := []rune(str)
fmt.Println(len(str))    // 13 bytes
fmt.Println(len(runes))  // 9 characters

// Iterate string as runes
for index, r := range "Hello" {
    fmt.Printf("%d: %c\n", index, r)
}
```

**See Also**: String, Byte, Unicode, Range

## S

### Select

**Definition**: Statement for choosing among multiple channel operations. Blocks until one case can proceed.

**Example**:

```go
ch1 := make(chan string)
ch2 := make(chan string)

go func() {
    time.Sleep(100 * time.Millisecond)
    ch1 <- "Channel 1"
}()

go func() {
    time.Sleep(200 * time.Millisecond)
    ch2 <- "Channel 2"
}()

select {
case msg1 := <-ch1:
    fmt.Println(msg1)
case msg2 := <-ch2:
    fmt.Println(msg2)
case <-time.After(500 * time.Millisecond):
    fmt.Println("Timeout")
}
```

**See Also**: Channel, Goroutine, Switch, Multiplexing

### Slice

**Definition**: Dynamically-sized, flexible view into arrays. Reference type with length and capacity.

**Example**:

```go
// Create slice
numbers := []int{1, 2, 3, 4, 5}

// Slicing operation
subset := numbers[1:4]  // [2, 3, 4]

// Append elements
numbers = append(numbers, 6, 7)

// Make slice with capacity
slice := make([]int, 3, 5)  // len=3, cap=5
fmt.Println(len(slice))     // 3
fmt.Println(cap(slice))     // 5
```

**See Also**: Array, Append, Cap, Len, Make

### Struct

**Definition**: Composite data type that groups together variables (fields) under a single name.

**Example**:

```go
type Person struct {
    Name    string
    Age     int
    Email   string
}

// Create struct
person := Person{
    Name:  "Alice",
    Age:   30,
    Email: "alice@example.com",
}

// Anonymous struct
config := struct {
    Host string
    Port int
}{
    Host: "localhost",
    Port: 8080,
}
```

**See Also**: Type, Composite Literal, Embedding

### Struct Tag

**Definition**: Metadata string attached to struct field, commonly used for serialization and reflection.

**Example**:

```go
type User struct {
    ID        int    `json:"id" db:"user_id"`
    Name      string `json:"name" db:"user_name"`
    Email     string `json:"email,omitempty" validate:"email"`
    Password  string `json:"-" db:"password_hash"`
}

// Tags used by encoding/json
data, _ := json.Marshal(user)
// {"id":1,"name":"Alice","email":"alice@example.com"}
```

**See Also**: Struct, JSON, Reflection

## T

### Type Assertion

**Definition**: Operation to extract concrete value from interface. Syntax: `value.(Type)`.

**Example**:

```go
var i interface{} = "hello"

// Type assertion
str := i.(string)
fmt.Println(str)  // "hello"

// Type assertion with check
str, ok := i.(string)
if ok {
    fmt.Println("String:", str)
} else {
    fmt.Println("Not a string")
}

// Panics if wrong type
num := i.(int)  // panic: interface conversion
```

**See Also**: Interface, Type Switch, Empty Interface

### Type Declaration

**Definition**: Statement that creates a new named type based on existing type.

**Example**:

```go
// Type declaration
type UserID int
type Handler func(http.ResponseWriter, *http.Request)
type StringSlice []string

// Methods can be attached to named types
func (id UserID) String() string {
    return fmt.Sprintf("User-%d", id)
}

var uid UserID = 123
fmt.Println(uid)  // User-123
```

**See Also**: Type Alias, Struct, Interface

### Type Switch

**Definition**: Switch statement that tests type of interface value instead of regular values.

**Example**:

```go
func describe(i interface{}) {
    switch v := i.(type) {
    case int:
        fmt.Printf("Integer: %d\n", v)
    case string:
        fmt.Printf("String: %s\n", v)
    case bool:
        fmt.Printf("Boolean: %t\n", v)
    case []int:
        fmt.Printf("Slice of ints: %v\n", v)
    default:
        fmt.Printf("Unknown type: %T\n", v)
    }
}

describe(42)        // Integer: 42
describe("hello")   // String: hello
describe(true)      // Boolean: true
```

**See Also**: Type Assertion, Interface, Switch

## U

### Unbuffered Channel

**Definition**: Channel with zero capacity. Send blocks until receiver is ready; receive blocks until sender is ready.

**Example**:

```go
ch := make(chan int)  // Unbuffered channel

// This would deadlock
// ch <- 42  // Blocks forever if no receiver

// Correct usage with goroutine
go func() {
    ch <- 42  // Blocks until receiver ready
}()

value := <-ch  // Blocks until sender ready
fmt.Println(value)
```

**See Also**: Channel, Buffered Channel, Goroutine, Synchronization

### Unexported

**Definition**: Identifier starting with lowercase letter. Accessible only within its own package.

**Example**:

```go
package mypackage

var exportedVar = "public"     // Accessible from other packages
var unexportedVar = "private"  // Only within mypackage

func ExportedFunc() {}   // Accessible from other packages
func unexportedFunc() {} // Only within mypackage

type ExportedType struct {
    ExportedField   string  // Accessible from other packages
    unexportedField int     // Only within mypackage
}
```

**See Also**: Exported, Package, Visibility, Encapsulation

## V

### Variadic Function

**Definition**: Function that accepts variable number of arguments of same type. Uses `...` syntax.

**Example**:

```go
func sum(numbers ...int) int {
    total := 0
    for _, num := range numbers {
        total += num
    }
    return total
}

result := sum(1, 2, 3)         // 6
result = sum(1, 2, 3, 4, 5)    // 15

// Spread slice into variadic function
numbers := []int{1, 2, 3, 4}
result = sum(numbers...)        // 10
```

**See Also**: Function, Slice, Printf

## W

### WaitGroup

**Definition**: Type from `sync` package for waiting for collection of goroutines to complete.

**Example**:

```go
var wg sync.WaitGroup

func worker(id int) {
    defer wg.Done()  // Decrement counter
    fmt.Printf("Worker %d starting\n", id)
    time.Sleep(time.Second)
    fmt.Printf("Worker %d done\n", id)
}

// Launch goroutines
for i := 1; i <= 3; i++ {
    wg.Add(1)  // Increment counter
    go worker(i)
}

wg.Wait()  // Block until counter is zero
fmt.Println("All workers completed")
```

**See Also**: Goroutine, Sync Package, Concurrency

## Z

### Zero Value

**Definition**: Default value assigned to variables when declared without explicit initialization.

**Example**:

```go
var i int           // 0
var f float64       // 0.0
var b bool          // false
var s string        // "" (empty string)
var ptr *int        // nil
var slice []int     // nil
var m map[string]int // nil
var ch chan int     // nil
var fn func()       // nil

// Struct zero value
type Person struct {
    Name string
    Age  int
}
var p Person  // Person{"", 0}
```

**See Also**: Nil, Variable, Declaration, Initialization

## Learn More

**Comprehensive Documentation**:

- [Beginner Tutorial](/en/learn/software-engineering/programming-language/golang/tutorials/beginner) - Detailed explanations of concepts
- [Quick Start](/en/learn/software-engineering/programming-language/golang/tutorials/quick-start) - Overview of key features
- [How-To Guides](/en/learn/software-engineering/programming-language/golang/how-to) - Practical usage examples
- [Cheat Sheet](/en/learn/software-engineering/programming-language/golang/reference/cheat-sheet) - Quick syntax reference

**Official Resources**:

- [Go Language Specification](https://go.dev/ref/spec) - Official language reference
- [Effective Go](https://go.dev/doc/effective_go) - Best practices guide
- [Go by Example](https://gobyexample.com/) - Hands-on examples
