---
title: Go Cheat Sheet
date: 2025-12-19T00:00:00+07:00
draft: false
weight: 803
description: Quick reference for Go syntax, operators, and common patterns
---

**Quick reference guide** for essential Go syntax and patterns. Copy-paste ready snippets for daily development.

## Variables and Types

### Variable Declaration

```go
// var declaration
var name string = "Alice"
var age int = 25
var price float64 = 19.99

// Type inference
var count = 10              // int
var rate = 4.5              // float64
var message = "Hello"       // string

// Short declaration (inside functions only)
name := "Alice"
age := 25
isActive := true

// Multiple declarations
var (
    x int    = 10
    y string = "test"
    z bool   = true
)

// Multiple assignment
a, b := 1, 2
x, y, z := "a", "b", "c"

// Constants
const Pi = 3.14159
const Greeting = "Hello"

// Typed constants
const MaxRetries int = 3
const Timeout float64 = 30.0

// Constant block
const (
    StatusOK       = 200
    StatusNotFound = 404
    StatusError    = 500
)

// iota (auto-incrementing constant)
const (
    Sunday = iota    // 0
    Monday           // 1
    Tuesday          // 2
    Wednesday        // 3
)
```

### Basic Types

```go
// Integers
var i8 int8 = 127                    // -128 to 127
var i16 int16 = 32767                // -32768 to 32767
var i32 int32 = 2147483647           // -2^31 to 2^31-1
var i64 int64 = 9223372036854775807  // -2^63 to 2^63-1
var i int = 100                      // Platform dependent (32 or 64 bit)

var ui8 uint8 = 255                  // 0 to 255
var ui16 uint16 = 65535              // 0 to 65535
var ui32 uint32 = 4294967295         // 0 to 2^32-1
var ui64 uint64                      // 0 to 2^64-1
var ui uint                          // Platform dependent

// Type aliases
var b byte = 255                     // alias for uint8
var r rune = 'A'                     // alias for int32 (Unicode code point)

// Floating point
var f32 float32 = 3.14
var f64 float64 = 3.14159265359

// Complex numbers
var c64 complex64 = 1 + 2i
var c128 complex128 = complex(1.5, 2.5)

// Boolean
var flag bool = true
var enabled bool = false

// String
var text string = "Hello, 世界"

// Type conversion (explicit only)
var x int = 100
var y int64 = int64(x)
var z float64 = float64(x)
var s string = string(x)  // "d" - Converts to Unicode rune (code point 100), NOT "100"!
                           // To get "100" as string, use: strconv.Itoa(x)
```

### Zero Values

```go
// Every type has a zero value
var i int          // 0
var f float64      // 0.0
var b bool         // false
var s string       // "" (empty string)
var p *int         // nil
var arr [3]int     // [0, 0, 0]
var slice []int    // nil
var m map[string]int  // nil
```

## Functions

### Function Declaration

```go
// Basic function
func greet(name string) string {
    return "Hello, " + name
}

// Multiple parameters
func add(a int, b int) int {
    return a + b
}

// Consecutive parameters of same type
func multiply(a, b, c int) int {
    return a * b * c
}

// Multiple return values
func divide(a, b float64) (float64, error) {
    if b == 0 {
        return 0, errors.New("division by zero")
    }
    return a / b, nil
}

// Named return values
func swap(a, b string) (x, y string) {
    x = b
    y = a
    return  // naked return
}

// No return value
func logMessage(msg string) {
    fmt.Println(msg)
}
```

### Variadic Functions

```go
// Variadic parameter (must be last)
func sum(numbers ...int) int {
    total := 0
    for _, num := range numbers {
        total += num
    }
    return total
}

sum(1, 2, 3, 4, 5)  // 15

// Spread slice into variadic function
nums := []int{1, 2, 3}
sum(nums...)        // 6

// Mixed parameters
func greetAll(greeting string, names ...string) {
    for _, name := range names {
        fmt.Printf("%s, %s\n", greeting, name)
    }
}
```

### Anonymous Functions and Closures

```go
// Anonymous function
multiply := func(a, b int) int {
    return a * b
}
result := multiply(3, 4)  // 12

// Immediately invoked
result := func(x int) int {
    return x * 2
}(5)  // 10

// Closure (captures outer variables)
func counter() func() int {
    count := 0
    return func() int {
        count++
        return count
    }
}

next := counter()
fmt.Println(next())  // 1
fmt.Println(next())  // 2
fmt.Println(next())  // 3
```

### Defer

```go
// Defer executes at function return
func readFile(filename string) error {
    f, err := os.Open(filename)
    if err != nil {
        return err
    }
    defer f.Close()  // Ensures file is closed

    // Read file...
    return nil
}

// Multiple defers (LIFO order)
func example() {
    defer fmt.Println("Third")   // Executes third
    defer fmt.Println("Second")  // Executes second
    defer fmt.Println("First")   // Executes first
    fmt.Println("Main")          // Executes immediately
}
// Output: Main, First, Second, Third

// Defer with parameters (evaluated immediately)
func deferExample() {
    i := 1
    defer fmt.Println(i)  // Prints 1 (not 2)
    i++
    fmt.Println(i)        // Prints 2
}
```

## Control Flow

### If Statement

```go
// Basic if
if x > 0 {
    fmt.Println("Positive")
}

// If-else
if x > 0 {
    fmt.Println("Positive")
} else {
    fmt.Println("Non-positive")
}

// If-else if-else
if x > 0 {
    fmt.Println("Positive")
} else if x < 0 {
    fmt.Println("Negative")
} else {
    fmt.Println("Zero")
}

// If with initialization (variable scoped to if block)
if err := doSomething(); err != nil {
    return err
}

// Common pattern: error checking
if val, err := getValue(); err != nil {
    return err
} else {
    fmt.Println(val)
}
```

### For Loop

```go
// Traditional for loop
for i := 0; i < 10; i++ {
    fmt.Println(i)
}

// While-style loop (no while keyword in Go)
i := 0
for i < 10 {
    fmt.Println(i)
    i++
}

// Infinite loop
for {
    // Loop forever (use break to exit)
}

// Range over slice
nums := []int{1, 2, 3, 4, 5}
for index, value := range nums {
    fmt.Printf("Index: %d, Value: %d\n", index, value)
}

// Range without index (use underscore)
for _, value := range nums {
    fmt.Println(value)
}

// Range without value (just index)
for index := range nums {
    fmt.Println(index)
}

// Range over map
m := map[string]int{"a": 1, "b": 2}
for key, value := range m {
    fmt.Printf("%s: %d\n", key, value)
}

// Range over string (runes)
for index, char := range "Hello" {
    fmt.Printf("%d: %c\n", index, char)
}

// Break and continue
for i := 0; i < 10; i++ {
    if i == 5 {
        break     // Exit loop
    }
    if i%2 == 0 {
        continue  // Skip to next iteration
    }
    fmt.Println(i)
}
```

### Switch Statement

```go
// Basic switch
switch day {
case "Monday":
    fmt.Println("Start of week")
case "Friday":
    fmt.Println("End of week")
case "Saturday", "Sunday":
    fmt.Println("Weekend")
default:
    fmt.Println("Midweek")
}

// Switch with initialization
switch day := time.Now().Weekday(); day {
case time.Saturday, time.Sunday:
    fmt.Println("Weekend")
default:
    fmt.Println("Weekday")
}

// Switch without expression (like if-else chain)
x := 10
switch {
case x < 0:
    fmt.Println("Negative")
case x == 0:
    fmt.Println("Zero")
case x > 0:
    fmt.Println("Positive")
}

// Type switch
func describe(i interface{}) {
    switch v := i.(type) {
    case int:
        fmt.Printf("Integer: %d\n", v)
    case string:
        fmt.Printf("String: %s\n", v)
    case bool:
        fmt.Printf("Boolean: %t\n", v)
    default:
        fmt.Printf("Unknown type: %T\n", v)
    }
}

// Fallthrough (explicit, unlike C/Java)
switch x {
case 1:
    fmt.Println("One")
    fallthrough
case 2:
    fmt.Println("One or Two")  // Executes for both 1 and 2
default:
    fmt.Println("Other")
}
```

## Data Structures

### Arrays

```go
// Array declaration (fixed size)
var arr [5]int                    // [0, 0, 0, 0, 0]
var names [3]string               // ["", "", ""]

// Array literal
arr := [5]int{1, 2, 3, 4, 5}
names := [3]string{"Alice", "Bob", "Charlie"}

// Infer length
arr := [...]int{1, 2, 3, 4, 5}   // Length is 5

// Partial initialization
arr := [5]int{1, 2}               // [1, 2, 0, 0, 0]

// Initialize specific indices
arr := [5]int{0: 10, 4: 50}       // [10, 0, 0, 0, 50]

// Access elements
first := arr[0]
last := arr[len(arr)-1]

// Arrays are values (copying creates new array)
a := [3]int{1, 2, 3}
b := a          // b is a copy, not a reference
b[0] = 10
// a is still [1, 2, 3], b is [10, 2, 3]
```

### Slices

```go
// Slice declaration (dynamic size)
var slice []int                   // nil slice
slice := []int{}                  // empty slice (not nil)
slice := []int{1, 2, 3, 4, 5}     // slice literal

// Make slice with length and capacity
slice := make([]int, 5)           // [0, 0, 0, 0, 0], len=5, cap=5
slice := make([]int, 3, 10)       // [0, 0, 0], len=3, cap=10

// Slicing (subslice)
arr := [5]int{1, 2, 3, 4, 5}
slice := arr[1:4]                 // [2, 3, 4] (indices 1, 2, 3)
slice := arr[:3]                  // [1, 2, 3] (from start to index 3)
slice := arr[2:]                  // [3, 4, 5] (from index 2 to end)
slice := arr[:]                   // [1, 2, 3, 4, 5] (entire array)

// Append (returns new slice if needed)
slice := []int{1, 2, 3}
slice = append(slice, 4)          // [1, 2, 3, 4]
slice = append(slice, 5, 6, 7)    // [1, 2, 3, 4, 5, 6, 7]

// Append slice to slice
a := []int{1, 2}
b := []int{3, 4}
a = append(a, b...)               // [1, 2, 3, 4]

// Copy slice
src := []int{1, 2, 3}
dst := make([]int, len(src))
copy(dst, src)                    // dst is [1, 2, 3]

// Length and capacity
slice := make([]int, 3, 10)
len(slice)                        // 3
cap(slice)                        // 10

// Remove element (no built-in method)
slice := []int{1, 2, 3, 4, 5}
i := 2  // Remove index 2
slice = append(slice[:i], slice[i+1:]...)  // [1, 2, 4, 5]
```

### Maps

```go
// Map declaration
var m map[string]int              // nil map (cannot add to nil map!)
m := map[string]int{}             // empty map
m := make(map[string]int)         // empty map

// Map literal
ages := map[string]int{
    "Alice": 25,
    "Bob":   30,
    "Charlie": 35,
}

// Add/update elements
ages["David"] = 40
ages["Alice"] = 26  // Update

// Get element
age := ages["Alice"]              // 26
age := ages["Unknown"]            // 0 (zero value, not an error)

// Check if key exists (two-value assignment)
age, exists := ages["Alice"]      // age=26, exists=true
age, exists := ages["Unknown"]    // age=0, exists=false

// Delete element
delete(ages, "Bob")

// Iterate over map (order is random!)
for name, age := range ages {
    fmt.Printf("%s is %d years old\n", name, age)
}

// Map length
len(ages)                         // Number of key-value pairs

// Maps are reference types
m1 := map[string]int{"a": 1}
m2 := m1        // m2 refers to same map
m2["a"] = 2     // m1["a"] is now 2
```

### Structs

```go
// Struct definition
type Person struct {
    Name string
    Age  int
    Email string
}

// Create struct
p1 := Person{Name: "Alice", Age: 25, Email: "alice@example.com"}
p2 := Person{"Bob", 30, "bob@example.com"}  // Must match field order
p3 := Person{Name: "Charlie"}  // Unspecified fields get zero values

// Zero value struct
var p4 Person  // {Name: "", Age: 0, Email: ""}

// Pointer to struct
p := &Person{Name: "Alice", Age: 25}

// Access fields
fmt.Println(p1.Name)  // "Alice"
p1.Age = 26           // Update field

// Access via pointer (automatic dereferencing)
p := &Person{Name: "Alice"}
p.Age = 25            // Equivalent to (*p).Age = 25

// Anonymous struct
person := struct {
    Name string
    Age  int
}{
    Name: "Alice",
    Age:  25,
}

// Embedded structs (composition)
type Address struct {
    Street string
    City   string
}

type Employee struct {
    Person   // Embedded Person struct
    Address  // Embedded Address struct
    Salary int
}

emp := Employee{
    Person:  Person{Name: "Alice", Age: 25},
    Address: Address{Street: "123 Main St", City: "NYC"},
    Salary:  50000,
}

// Access embedded fields directly
fmt.Println(emp.Name)    // "Alice" (from Person)
fmt.Println(emp.City)    // "NYC" (from Address)
```

## Pointers

```go
// Pointer declaration
var p *int                       // nil pointer
var x int = 10
p = &x                           // p points to x

// Dereferencing
fmt.Println(*p)                  // 10 (value at address)
*p = 20                          // Change value through pointer
fmt.Println(x)                   // 20

// New function (allocates memory, returns pointer)
p := new(int)                    // p is *int, points to zero value (0)
*p = 100

// Pointer to struct
type Person struct {
    Name string
    Age  int
}

p := &Person{Name: "Alice", Age: 25}
p.Age = 26                       // Automatic dereferencing

// Function parameters (pass by value vs pointer)
func increment(x int) {
    x++  // Does not affect caller
}

func incrementPtr(x *int) {
    *x++  // Modifies caller's variable
}

num := 10
increment(num)                   // num is still 10
incrementPtr(&num)               // num is now 11

// Nil pointer check
var p *int
if p == nil {
    fmt.Println("Pointer is nil")
}
```

## Methods and Interfaces

### Methods

```go
// Method with value receiver
type Rectangle struct {
    Width, Height float64
}

func (r Rectangle) Area() float64 {
    return r.Width * r.Height
}

rect := Rectangle{Width: 10, Height: 5}
area := rect.Area()  // 50.0

// Method with pointer receiver (can modify receiver)
func (r *Rectangle) Scale(factor float64) {
    r.Width *= factor
    r.Height *= factor
}

rect := Rectangle{Width: 10, Height: 5}
rect.Scale(2)  // rect is now {Width: 20, Height: 10}

// Methods on any type
type Celsius float64

func (c Celsius) ToFahrenheit() float64 {
    return float64(c)*9/5 + 32
}

temp := Celsius(100)
f := temp.ToFahrenheit()  // 212.0
```

### Interfaces

```go
// Interface definition
type Shape interface {
    Area() float64
    Perimeter() float64
}

// Implement interface (implicit, no "implements" keyword)
type Rectangle struct {
    Width, Height float64
}

func (r Rectangle) Area() float64 {
    return r.Width * r.Height
}

func (r Rectangle) Perimeter() float64 {
    return 2 * (r.Width + r.Height)
}

type Circle struct {
    Radius float64
}

func (c Circle) Area() float64 {
    return math.Pi * c.Radius * c.Radius
}

func (c Circle) Perimeter() float64 {
    return 2 * math.Pi * c.Radius
}

// Use interface
func printArea(s Shape) {
    fmt.Printf("Area: %.2f\n", s.Area())
}

rect := Rectangle{Width: 10, Height: 5}
circle := Circle{Radius: 3}
printArea(rect)    // Area: 50.00
printArea(circle)  // Area: 28.27

// Empty interface (any type)
var i interface{}
i = 42
i = "hello"
i = true

// Type assertion
var i interface{} = "hello"
s := i.(string)             // s is "hello"
s, ok := i.(string)         // s is "hello", ok is true
n, ok := i.(int)            // n is 0, ok is false (safe)

// Type assertion panic (if wrong type)
s := i.(int)  // Panic! i is string, not int

// Type switch
func describe(i interface{}) {
    switch v := i.(type) {
    case int:
        fmt.Printf("Integer: %d\n", v)
    case string:
        fmt.Printf("String: %s\n", v)
    default:
        fmt.Printf("Unknown: %T\n", v)
    }
}
```

## Goroutines and Channels

### Goroutines

```go
// Launch goroutine (lightweight thread)
go func() {
    fmt.Println("Hello from goroutine")
}()

// Goroutine with function
func printMessage(msg string) {
    fmt.Println(msg)
}

go printMessage("Hello")

// Wait for goroutines (using WaitGroup)
var wg sync.WaitGroup

for i := 0; i < 5; i++ {
    wg.Add(1)
    go func(id int) {
        defer wg.Done()
        fmt.Printf("Goroutine %d\n", id)
    }(i)
}

wg.Wait()  // Wait for all goroutines to finish
```

### Channels

```go
// Create channel
ch := make(chan int)              // Unbuffered channel
ch := make(chan int, 10)          // Buffered channel (capacity 10)

// Send to channel (blocks if unbuffered and no receiver)
ch <- 42

// Receive from channel (blocks until value available)
value := <-ch

// Close channel (only sender should close)
close(ch)

// Check if channel is closed
value, ok := <-ch  // ok is false if channel is closed

// Range over channel (receives until closed)
for value := range ch {
    fmt.Println(value)
}

// Example: producer-consumer
func producer(ch chan int) {
    for i := 0; i < 5; i++ {
        ch <- i
    }
    close(ch)
}

func consumer(ch chan int) {
    for value := range ch {
        fmt.Println(value)
    }
}

ch := make(chan int)
go producer(ch)
consumer(ch)  // Prints 0, 1, 2, 3, 4

// Buffered channel (non-blocking until full)
ch := make(chan int, 2)
ch <- 1  // Non-blocking
ch <- 2  // Non-blocking
ch <- 3  // Blocks until a value is received

// Direction-specific channels
func send(ch chan<- int) {  // Send-only channel
    ch <- 42
}

func receive(ch <-chan int) {  // Receive-only channel
    value := <-ch
}
```

### Select Statement

```go
// Select waits on multiple channels
select {
case msg := <-ch1:
    fmt.Println("Received from ch1:", msg)
case msg := <-ch2:
    fmt.Println("Received from ch2:", msg)
case ch3 <- 100:
    fmt.Println("Sent to ch3")
default:
    fmt.Println("No communication")
}

// Timeout pattern
select {
case msg := <-ch:
    fmt.Println(msg)
case <-time.After(1 * time.Second):
    fmt.Println("Timeout")
}

// Quit channel pattern
func worker(ch chan int, quit chan bool) {
    for {
        select {
        case job := <-ch:
            fmt.Println("Processing:", job)
        case <-quit:
            fmt.Println("Quitting")
            return
        }
    }
}

// Fan-in pattern (merge multiple channels)
func fanIn(ch1, ch2 chan int) chan int {
    out := make(chan int)
    go func() {
        for {
            select {
            case val := <-ch1:
                out <- val
            case val := <-ch2:
                out <- val
            }
        }
    }()
    return out
}
```

### Common Concurrency Patterns

```go
// Mutex (mutual exclusion)
var mu sync.Mutex
var count int

func increment() {
    mu.Lock()
    defer mu.Unlock()
    count++
}

// RWMutex (read-write mutex)
var rwmu sync.RWMutex
var data map[string]string

func read(key string) string {
    rwmu.RLock()
    defer rwmu.RUnlock()
    return data[key]
}

func write(key, value string) {
    rwmu.Lock()
    defer rwmu.Unlock()
    data[key] = value
}

// Once (execute only once)
var once sync.Once

func initialize() {
    once.Do(func() {
        fmt.Println("Initialized")  // Executes only once
    })
}
```

## Error Handling

### Error Type

```go
// error is an interface
type error interface {
    Error() string
}

// Create error
err := errors.New("something went wrong")
err := fmt.Errorf("value must be between %d and %d", min, max)

// Return error
func divide(a, b float64) (float64, error) {
    if b == 0 {
        return 0, errors.New("division by zero")
    }
    return a / b, nil
}

// Check error
result, err := divide(10, 0)
if err != nil {
    fmt.Println("Error:", err)
    return
}
fmt.Println("Result:", result)

// Custom error type
type ValidationError struct {
    Field string
    Message string
}

func (e *ValidationError) Error() string {
    return fmt.Sprintf("%s: %s", e.Field, e.Message)
}

// Error wrapping (Go 1.13+)
err := doSomething()
if err != nil {
    return fmt.Errorf("failed to do something: %w", err)
}

// Error unwrapping
err := someOperation()
if errors.Is(err, ErrNotFound) {
    // Handle specific error
}

var validationErr *ValidationError
if errors.As(err, &validationErr) {
    // Handle validation error
}
```

### Panic and Recover

```go
// Panic (stops normal execution)
func mustConnect(url string) *Connection {
    conn, err := connect(url)
    if err != nil {
        panic(err)  // Use sparingly!
    }
    return conn
}

// Recover (catch panic, must be in defer)
func safeOperation() {
    defer func() {
        if r := recover(); r != nil {
            fmt.Println("Recovered from panic:", r)
        }
    }()

    // Code that might panic
    riskyOperation()
}

// Panic with value
panic("critical error")
panic(42)
panic(CustomError{})
```

## Packages and Imports

### Package Declaration

```go
// Every Go file starts with package declaration
package main  // Executable package

package mypackage  // Library package
```

### Imports

```go
// Single import
import "fmt"
import "math"

// Import block
import (
    "fmt"
    "math"
    "net/http"
)

// Alias import
import (
    f "fmt"
    m "math"
)

f.Println("Hello")
m.Sqrt(16)

// Blank import (run init() function only)
import _ "github.com/lib/pq"

// Dot import (import into current namespace, avoid!)
import . "fmt"
Println("No prefix needed")  // Don't do this!
```

### Package Initialization

```go
// init() runs before main()
func init() {
    fmt.Println("Initializing package")
}

// Multiple init() functions execute in order
func init() {
    setupDatabase()
}

func init() {
    loadConfig()
}

// main() is the entry point
func main() {
    fmt.Println("Starting application")
}
```

### Exported vs Unexported

```go
// Exported (capitalized, public)
type Person struct {
    Name string  // Exported field
    age  int     // Unexported field
}

func PublicFunction() {}    // Exported
func privateFunction() {}   // Unexported

// Only exported names are accessible from other packages
import "mypackage"

p := mypackage.Person{Name: "Alice"}  // OK
p.Name                                 // OK
p.age                                  // Error: unexported
mypackage.PublicFunction()            // OK
mypackage.privateFunction()           // Error: unexported
```

## String Operations

```go
// String concatenation
s := "Hello" + " " + "World"

// String formatting
name := "Alice"
age := 25
msg := fmt.Sprintf("Name: %s, Age: %d", name, age)

// Common format verbs
fmt.Printf("%s", "string")      // string
fmt.Printf("%d", 42)            // integer
fmt.Printf("%f", 3.14)          // float
fmt.Printf("%.2f", 3.14159)     // float with precision (3.14)
fmt.Printf("%t", true)          // boolean
fmt.Printf("%v", value)         // default format
fmt.Printf("%+v", struct{})     // struct with field names
fmt.Printf("%#v", value)        // Go syntax representation
fmt.Printf("%T", value)         // type

// String comparison
s1 := "hello"
s2 := "hello"
s1 == s2                        // true

// String length (bytes, not runes!)
len("hello")                    // 5
len("世界")                     // 6 (not 2!)

// Rune count (Unicode characters)
utf8.RuneCountInString("世界")  // 2

// String iteration
for i := 0; i < len(s); i++ {
    fmt.Printf("%c ", s[i])     // Byte iteration
}

for i, r := range "hello" {
    fmt.Printf("%d: %c\n", i, r) // Rune iteration
}

// String conversion
s := string(97)                  // "a" (from ASCII)
s := string([]byte{'h', 'i'})    // "hi"
s := string([]rune{'世', '界'})  // "世界"
b := []byte("hello")             // [104 101 108 108 111]
r := []rune("世界")              // [19990 30028]

// Strings package
import "strings"

strings.Contains("hello", "ll")           // true
strings.HasPrefix("hello", "he")          // true
strings.HasSuffix("hello", "lo")          // true
strings.Index("hello", "ll")              // 2
strings.ToUpper("hello")                  // "HELLO"
strings.ToLower("HELLO")                  // "hello"
strings.TrimSpace("  hello  ")            // "hello"
strings.Split("a,b,c", ",")               // ["a", "b", "c"]
strings.Join([]string{"a", "b"}, ",")     // "a,b"
strings.Replace("hello", "l", "L", 1)     // "heLlo"
strings.ReplaceAll("hello", "l", "L")     // "heLLo"
strings.Repeat("a", 5)                    // "aaaaa"
```

## Common Patterns

### Options Pattern

```go
// Configuration with options
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

func WithTimeout(timeout time.Duration) Option {
    return func(s *Server) {
        s.timeout = timeout
    }
}

func NewServer(opts ...Option) *Server {
    s := &Server{
        host:    "localhost",  // defaults
        port:    8080,
        timeout: 30 * time.Second,
    }
    for _, opt := range opts {
        opt(s)
    }
    return s
}

// Usage
server := NewServer(
    WithHost("example.com"),
    WithPort(9000),
)
```

### Builder Pattern

```go
type Query struct {
    table   string
    columns []string
    where   string
    limit   int
}

type QueryBuilder struct {
    query Query
}

func NewQueryBuilder() *QueryBuilder {
    return &QueryBuilder{}
}

func (qb *QueryBuilder) Table(table string) *QueryBuilder {
    qb.query.table = table
    return qb
}

func (qb *QueryBuilder) Select(columns ...string) *QueryBuilder {
    qb.query.columns = columns
    return qb
}

func (qb *QueryBuilder) Where(condition string) *QueryBuilder {
    qb.query.where = condition
    return qb
}

func (qb *QueryBuilder) Limit(limit int) *QueryBuilder {
    qb.query.limit = limit
    return qb
}

func (qb *QueryBuilder) Build() Query {
    return qb.query
}

// Usage
query := NewQueryBuilder().
    Table("users").
    Select("id", "name", "email").
    Where("age > 18").
    Limit(10).
    Build()
```

### Error Handling Patterns

```go
// Early return pattern
func processUser(id int) error {
    user, err := getUser(id)
    if err != nil {
        return err
    }

    if err := validateUser(user); err != nil {
        return err
    }

    if err := saveUser(user); err != nil {
        return err
    }

    return nil
}

// Error wrapping for context
func loadConfig(path string) error {
    data, err := os.ReadFile(path)
    if err != nil {
        return fmt.Errorf("failed to load config from %s: %w", path, err)
    }
    // Process data...
    return nil
}

// Sentinel errors
var (
    ErrNotFound     = errors.New("not found")
    ErrUnauthorized = errors.New("unauthorized")
    ErrInvalidInput = errors.New("invalid input")
)

func getUser(id int) (*User, error) {
    // ...
    return nil, ErrNotFound
}

// Check with errors.Is
err := getUser(42)
if errors.Is(err, ErrNotFound) {
    // Handle not found
}
```

## Testing Quick Reference

```go
// Test file: mypackage_test.go
package mypackage

import "testing"

// Test function (starts with Test)
func TestAdd(t *testing.T) {
    result := Add(2, 3)
    if result != 5 {
        t.Errorf("Add(2, 3) = %d; want 5", result)
    }
}

// Table-driven tests
func TestAddTable(t *testing.T) {
    tests := []struct {
        name     string
        a, b     int
        expected int
    }{
        {"positive", 2, 3, 5},
        {"negative", -1, -2, -3},
        {"zero", 0, 5, 5},
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

// Benchmark (starts with Benchmark)
func BenchmarkAdd(b *testing.B) {
    for i := 0; i < b.N; i++ {
        Add(2, 3)
    }
}

// Example (starts with Example, appears in documentation)
func ExampleAdd() {
    result := Add(2, 3)
    fmt.Println(result)
    // Output: 5
}

// Setup and teardown
func TestMain(m *testing.M) {
    // Setup
    setup()

    // Run tests
    code := m.Run()

    // Teardown
    teardown()

    os.Exit(code)
}
```

## Learn More

**Detailed Documentation**:

- [Quick Start Tutorial](/en/learn/swe/prog-lang/golang/tutorials/quick-start) - Essential Go touchpoints
- [Beginner Tutorial](/en/learn/swe/prog-lang/golang/tutorials/beginner) - Comprehensive fundamentals
- [Cookbook](/en/learn/swe/prog-lang/golang/how-to/cookbook) - Practical recipes
- [How-To Guides](/en/learn/swe/prog-lang/golang/how-to) - Problem-solving guides

**Official Resources**:

- [Go Language Specification](https://go.dev/ref/spec) - Complete language reference
- [Go Standard Library](https://pkg.go.dev/std) - Standard library documentation
- [Effective Go](https://go.dev/doc/effective_go) - Best practices guide
