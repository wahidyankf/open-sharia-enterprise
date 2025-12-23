---
title: "Intermediate Examples - Go By Example"
date: 2025-12-23T00:00:00+07:00
draft: false
weight: 10000002
description: "Examples 16-35: Advanced types, concurrency fundamentals, I/O, HTTP, standard library, production patterns, and testing (40-70% coverage)"
tags:
  [
    "golang",
    "go",
    "tutorial",
    "by-example",
    "intermediate",
    "concurrency",
    "http",
    "testing",
  ]
---

## Group 1: Advanced Interfaces and Types

### Example 16: Type Embedding and Composition

Embedding allows structs to inherit fields and methods from other types without inheritance chains. This composition-over-inheritance pattern is Go's philosophy - simpler and more flexible than traditional OOP inheritance.

```mermaid
graph TB
    A["struct Person<br/>Name, Age"]
    B["embedded"]
    C["struct Employee<br/>Person, Title<br/>Has all Person fields"]

    A ---|embed| C
    C -->|"has access to"| A

    style A fill:#0173B2,stroke:#000,color:#fff
    style B fill:#DE8F05,stroke:#000,color:#fff
    style C fill:#029E73,stroke:#000,color:#fff
```

**Code**:

```go
package main

import "fmt"

func main() {
    emp := Employee{
        Person: Person{Name: "Alice", Age: 30}, // => Explicit Person initialization
        Title:  "Engineer",
    }

    // Access embedded fields directly
    fmt.Println(emp.Name)    // => Alice (direct access to Person fields)
    fmt.Println(emp.Title)   // => Engineer

    // Call embedded methods
    emp.Describe()           // => Embedded method callable directly

    // Explicit access to embedded type
    fmt.Println(emp.Person.Name) // => Alice
}

type Person struct {
    Name string
    Age  int
}

func (p Person) Describe() { // => Person method
    fmt.Printf("%s is %d years old\n", p.Name, p.Age)
    // => Output: Alice is 30 years old
}

// Embed Person - Employee has all Person fields and methods
type Employee struct {
    Person // => Embedded field (no name) - fields and methods promoted
    Title  string
}
```

**Key Takeaway**: Embedding promotes fields and methods of embedded types to the outer type. This composition pattern is more flexible than inheritance - a type can embed multiple types, and you can override methods by defining them on the outer type.

### Example 17: Custom Error Types

While `error` interface only needs an `Error()` method, custom error types let you attach extra information. Implementing the `error` interface and using error wrapping enables sophisticated error handling patterns.

**Code**:

```go
package main

import (
    "errors"
    "fmt"
)

func main() {
    // Custom error with additional context
    err := performOperation()
    if err != nil {
        // Check if it's our custom error type
        var opErr *OperationError
        if errors.As(err, &opErr) { // => Extract custom error type
            fmt.Printf("Operation failed: %s (Code: %d)\n", opErr.Message, opErr.Code)
            // => Output: Operation failed: invalid input (Code: 400)
        }
    }

    // Error wrapping preserves error chain
    err = divideWithWrapping(10, 0)
    if err != nil {
        fmt.Println("Wrapped error:", err)
        // => Output: Wrapped error: division failed: cannot divide by zero

        original := errors.Unwrap(err)
        fmt.Println("Original:", original)
        // => Output: Original: cannot divide by zero
    }
}

// Custom error type implementing error interface
type OperationError struct {
    Message string
    Code    int
}

// Error() method satisfies error interface
func (e *OperationError) Error() string { // => Must be pointer receiver for this pattern
    return fmt.Sprintf("operation error: %s", e.Message)
}

func performOperation() error {
    return &OperationError{
        Message: "invalid input",
        Code:    400,
    }
}

var ErrDivisionByZero = errors.New("cannot divide by zero")

func divideWithWrapping(a, b int) error {
    if b == 0 {
        return fmt.Errorf("division failed: %w", ErrDivisionByZero) // => Wrap error
    }
    return nil
}
```

**Key Takeaway**: Implement `Error()` method to create custom error types. Use `errors.As()` to check error type and extract additional context. `fmt.Errorf` with `%w` wraps errors, preserving the chain for `errors.Unwrap()`.

### Example 18: JSON Handling

JSON is ubiquitous in Go APIs. The `encoding/json` package marshals (structs to JSON) and unmarshals (JSON to structs). Struct tags control JSON field mapping - critical for API compatibility when Go field names don't match JSON field names.

**Code**:

```go
package main

import (
    "encoding/json"
    "fmt"
)

func main() {
    // Marshal - Go struct to JSON string
    user := User{
        Name: "Alice",
        Age:  30,
        Email: "alice@example.com",
    }

    jsonBytes, err := json.Marshal(user) // => Convert struct to JSON
    if err != nil {
        fmt.Println("Marshal error:", err)
        return
    }
    fmt.Println(string(jsonBytes))
    // => Output: {"Name":"Alice","Age":30,"Email":"alice@example.com"}

    // Unmarshal - JSON string to Go struct
    jsonStr := `{"name":"Bob","age":25,"email":"bob@example.com"}` // => Lowercase JSON fields
    var person Person
    err = json.Unmarshal([]byte(jsonStr), &person) // => Unmarshal into pointer
    if err != nil {
        fmt.Println("Unmarshal error:", err)
        return
    }
    fmt.Println(person)
    // => Output: {Bob 25 bob@example.com}

    // Custom type with different JSON representation
    apiResponse := APIResponse{
        Status: 200,
        Data:   person,
    }
    responseJSON, _ := json.MarshalIndent(apiResponse, "", "  ") // => Pretty print JSON
    fmt.Println(string(responseJSON))
    // => Output: {
    //            "status": 200,
    //            "data": {
    //              "name": "Bob",
    //              "age": 25,
    //              "email": "bob@example.com"
    //            }
    //          }
}

// User - field names must be capitalized for json.Marshal to see them
type User struct {
    Name  string
    Age   int
    Email string
}

// Person - struct tags control JSON mapping
type Person struct {
    Name  string `json:"name"`          // => Map to "name" in JSON
    Age   int    `json:"age"`           // => Map to "age" in JSON
    Email string `json:"email"`         // => Map to "email" in JSON
}

type APIResponse struct {
    Status int         `json:"status"`            // => Customize field name
    Data   Person      `json:"data"`
}
```

**Key Takeaway**: Struct tags control JSON field mapping - essential when Go names differ from JSON names. Struct field names must be capitalized for JSON encoding. Use `json.Marshal()` for compact JSON and `json.MarshalIndent()` for pretty-printed JSON.

## Group 2: Concurrency Basics

### Example 19: Goroutines

Goroutines are lightweight threads managed by the Go runtime. Unlike OS threads, thousands of goroutines can run concurrently without overwhelming system resources. The `go` keyword starts a goroutine that runs concurrently with code that follows.

```mermaid
graph TB
    A["func main"]
    B["go someFunc"]
    C["continue in main"]
    D["someFunc runs<br/>concurrently"]

    A --> B
    B -->|spawns goroutine| D
    A --> C

    style A fill:#0173B2,stroke:#000,color:#fff
    style B fill:#DE8F05,stroke:#000,color:#fff
    style C fill:#029E73,stroke:#000,color:#fff
    style D fill:#CC78BC,stroke:#000,color:#fff
```

**Code**:

```go
package main

import (
    "fmt"
    "time"
)

func main() {
    // Start goroutine
    go printNumbers()        // => go keyword spawns goroutine - execution continues

    // Main continues while goroutine runs
    fmt.Println("Main continues immediately")

    // Wait for goroutine to finish
    time.Sleep(1 * time.Second) // => Goroutine needs time to complete
    fmt.Println("Main done")

    // => Output:
    // => Main continues immediately
    // => Number: 0
    // => Number: 1
    // => Number: 2
    // => Main done

    // Multiple goroutines
    for i := 0; i < 3; i++ {
        go func(id int) { // => Anonymous function in goroutine
            fmt.Printf("Goroutine %d running\n", id)
        }(i)              // => Note: must pass i as argument (see common pitfall)
    }

    time.Sleep(100 * time.Millisecond) // => Let goroutines complete
}

func printNumbers() {
    for i := 0; i < 3; i++ {
        fmt.Printf("Number: %d\n", i)
    }
}
```

**Key Takeaway**: `go func()` spawns a goroutine that runs concurrently. The main function doesn't wait for goroutines - you must synchronize with `time.Sleep()`, channels, or `sync.WaitGroup`. When passing loop variables to goroutines, pass as arguments to avoid closure pitfalls.

### Example 20: Channels

Channels enable safe communication between goroutines. Send data on one end, receive on the other. Unbuffered channels synchronize goroutines - a send blocks until a receive happens. Buffered channels decouple send and receive.

```mermaid
graph LR
    A["Goroutine 1<br/>send on channel"]
    B["Channel<br/>message"]
    C["Goroutine 2<br/>receive from channel"]

    A -->|data flow| B
    B -->|data flow| C

    style A fill:#0173B2,stroke:#000,color:#fff
    style B fill:#DE8F05,stroke:#000,color:#fff
    style C fill:#029E73,stroke:#000,color:#fff
```

**Code**:

```go
package main

import (
    "fmt"
)

func main() {
    // Unbuffered channel - send blocks until receive
    messages := make(chan string) // => Create channel for strings

    go func() {
        messages <- "Hello from goroutine" // => Send on channel
    }()

    msg := <-messages                      // => Receive from channel (blocks until send)
    fmt.Println(msg)
    // => Output: Hello from goroutine

    // Buffered channel - send blocks only when buffer full
    buffered := make(chan int, 2) // => Buffer capacity 2

    buffered <- 1                  // => Send (doesn't block, buffer has space)
    buffered <- 2                  // => Send (buffer full now)
    // buffered <- 3               // => This would block (buffer full)

    val1 := <-buffered            // => Receive (now buffer has space)
    fmt.Println(val1)             // => 1
    val2 := <-buffered
    fmt.Println(val2)             // => 2

    // Range over channel
    results := make(chan int)
    go func() {
        results <- 10
        results <- 20
        results <- 30
        close(results)            // => Close channel to signal no more sends
    }()

    for value := range results { // => Range blocks until channel closed
        fmt.Println(value)
    }
    // => Output: 10 20 30
}
```

**Key Takeaway**: Unbuffered channels synchronize goroutines - sends block until receives. Buffered channels decouple send/receive by buffering values. Always `close()` channels when done to signal completion. Use `range` to iterate until channel closes.

### Example 21: Channel Select

The `select` statement lets a goroutine wait on multiple channel operations. It's like a `switch` for channels - whichever channel is ready executes first. This pattern enables timeouts and handling multiple concurrent operations.

**Code**:

```go
package main

import (
    "fmt"
    "time"
)

func main() {
    // Select between two channels
    ch1 := make(chan string)
    ch2 := make(chan string)

    go func() {
        time.Sleep(100 * time.Millisecond)
        ch1 <- "from channel 1" // => Send after delay
    }()

    go func() {
        time.Sleep(200 * time.Millisecond)
        ch2 <- "from channel 2" // => Send after longer delay
    }()

    // Wait for either channel
    for i := 0; i < 2; i++ {
        select {                // => Wait for any channel to be ready
        case msg1 := <-ch1:     // => If ch1 is ready, execute
            fmt.Println("Received:", msg1)
        case msg2 := <-ch2:     // => If ch2 is ready, execute
            fmt.Println("Received:", msg2)
        }
    }

    // Select with timeout
    timeoutCh := make(chan string)
    select {
    case msg := <-timeoutCh: // => Wait up to 100ms for message
        fmt.Println("Got message:", msg)
    case <-time.After(100 * time.Millisecond): // => Timeout fires if no message in 100ms
        fmt.Println("Timeout - no message received")
    }
    // => Output: Timeout - no message received

    // Default case - non-blocking receive
    results := make(chan int)
    select {
    case result := <-results:
        fmt.Println("Got result:", result)
    default:                 // => Executes if no other case ready
        fmt.Println("No result available")
    }
    // => Output: No result available
}
```

**Key Takeaway**: `select` waits for multiple channels. Use with `time.After()` for timeouts. The `default` case makes select non-blocking - useful for checking if work is available without blocking.

### Example 22: WaitGroups and Sync

Coordinating multiple goroutines requires synchronization. `sync.WaitGroup` is the standard pattern - increment a counter for each goroutine, decrement when done, and wait for all to finish. Mutexes protect shared data from race conditions.

**Code**:

```go
package main

import (
    "fmt"
    "sync"
)

func main() {
    // WaitGroup - coordinate multiple goroutines
    var wg sync.WaitGroup // => WaitGroup tracks active goroutines

    for i := 0; i < 3; i++ {
        wg.Add(1)          // => Increment counter - one more goroutine starting
        go func(id int) {
            defer wg.Done() // => Decrement counter when goroutine completes
            fmt.Printf("Worker %d processing\n", id)
        }(i)
    }

    wg.Wait()              // => Block until all goroutines call Done()
    fmt.Println("All workers complete")
    // => Output:
    // => Worker 0 processing
    // => Worker 1 processing
    // => Worker 2 processing
    // => All workers complete

    // Mutex - protect shared data
    var mu sync.Mutex      // => Mutex protects counter
    var counter int
    var wg2 sync.WaitGroup

    for i := 0; i < 5; i++ {
        wg2.Add(1)
        go func() {
            defer wg2.Done()
            mu.Lock()       // => Acquire lock - only one goroutine at a time
            counter++       // => Safely modify shared counter
            mu.Unlock()     // => Release lock - other goroutines can proceed
        }()
    }

    wg2.Wait()
    fmt.Println("Counter:", counter) // => 5 (safe increment)

    // RWMutex - multiple readers, exclusive writer
    var rwmu sync.RWMutex
    var data = "initial"

    // Multiple readers can run concurrently
    var wg3 sync.WaitGroup
    for i := 0; i < 3; i++ {
        wg3.Add(1)
        go func(id int) {
            defer wg3.Done()
            rwmu.RLock()    // => Read lock - multiple goroutines allowed
            fmt.Printf("Reader %d: %s\n", id, data)
            rwmu.RUnlock()
        }(i)
    }

    wg3.Wait()
}
```

**Key Takeaway**: Use `sync.WaitGroup` to wait for multiple goroutines. Call `Add(1)` before spawning, `Done()` when complete, and `Wait()` to block until all finish. Use `sync.Mutex` to protect shared data - `Lock()` before accessing, `Unlock()` after. Use `sync.RWMutex` when you have many readers and few writers.

## Group 3: I/O and File Handling

### Example 23: File I/O

File operations are fundamental. Go provides multiple layers: low-level (`os` package), buffered (`bufio`), and convenience functions. The `defer file.Close()` pattern ensures files close even if errors occur.

**Code**:

```go
package main

import (
    "bufio"
    "fmt"
    "os"
)

func main() {
    // Write to file
    filename := "/tmp/test.txt"
    file, err := os.Create(filename) // => Create or truncate file
    if err != nil {
        fmt.Println("Error creating file:", err)
        return
    }
    defer file.Close()              // => Ensure file closes

    file.WriteString("Line 1\n")    // => Write data
    file.WriteString("Line 2\n")

    // Read entire file
    data, err := os.ReadFile(filename) // => Read all content at once
    if err != nil {
        fmt.Println("Error reading file:", err)
        return
    }
    fmt.Println("File contents:")
    fmt.Println(string(data))
    // => Output:
    // => File contents:
    // => Line 1
    // => Line 2

    // Read line by line (buffered)
    file, err = os.Open(filename)
    if err != nil {
        fmt.Println("Error opening file:", err)
        return
    }
    defer file.Close()

    scanner := bufio.NewScanner(file) // => Buffer reads for efficiency
    for scanner.Scan() {              // => Read until EOF
        line := scanner.Text()        // => Get current line
        fmt.Println("Read:", line)
    }
    // => Output:
    // => Read: Line 1
    // => Read: Line 2

    // File info
    info, err := os.Stat(filename)
    if err != nil {
        fmt.Println("Error getting info:", err)
        return
    }
    fmt.Printf("File size: %d bytes\n", info.Size()) // => File metadata
    fmt.Printf("Modified: %v\n", info.ModTime())
}
```

**Key Takeaway**: Use `os.Create()` to write, `os.ReadFile()` to read entire file, `os.Open()` with `bufio.Scanner` for line-by-line reading. Always `defer file.Close()` to ensure cleanup.

### Example 24: HTTP Client

Making HTTP requests is essential for API integration. The `net/http` package provides client functionality. Customize requests with headers, timeouts, and query parameters. Always check response status codes.

**Code**:

```go
package main

import (
    "fmt"
    "io"
    "net/http"
    "time"
)

func main() {
    // Simple GET request
    resp, err := http.Get("https://api.example.com/users") // => GET request
    if err != nil {
        fmt.Println("Error:", err)
        return
    }
    defer resp.Body.Close()  // => Ensure response body closes

    body, _ := io.ReadAll(resp.Body) // => Read response body
    fmt.Println("Status:", resp.Status) // => Check status
    fmt.Println("Body:", string(body))

    // Custom client with timeout
    client := &http.Client{
        Timeout: 5 * time.Second, // => Request timeout
    }

    // Create custom request
    req, _ := http.NewRequest("GET", "https://api.example.com/users", nil)
    req.Header.Add("Authorization", "Bearer token123") // => Add headers

    resp, err = client.Do(req) // => Execute request
    if err != nil {
        fmt.Println("Error:", err)
        return
    }
    defer resp.Body.Close()

    if resp.StatusCode == http.StatusOK { // => Check status code
        fmt.Println("Request successful")
    }
}
```

**Key Takeaway**: Use `http.Get()` for simple requests. Use `http.Client` with custom `Timeout` for control. Always `defer resp.Body.Close()` to avoid resource leaks. Check response status codes - successful responses are 200-299.

### Example 25: HTTP Server

Go's standard library includes HTTP server capabilities. Handler functions or types can handle requests. Multiplexing routes maps URL paths to handlers. Understanding request/response flow is essential for building APIs.

**Code**:

```go
package main

import (
    "fmt"
    "io"
    "net/http"
)

func main() {
    // Register handlers
    mux := http.NewServeMux() // => Create router

    mux.HandleFunc("/", homeHandler)       // => Map URL path to handler function
    mux.HandleFunc("/users", usersHandler)
    mux.Handle("/data", &DataHandler{})    // => Use handler type

    // Start server
    fmt.Println("Server listening on :8080")
    err := http.ListenAndServe(":8080", mux) // => Start server on port 8080
    if err != nil {
        fmt.Println("Error:", err)
    }
}

// Handler function
func homeHandler(w http.ResponseWriter, r *http.Request) { // => w: write response, r: request
    body, _ := io.ReadAll(r.Body) // => Read request body
    fmt.Fprintf(w, "Hello from home! Body: %s\n", string(body)) // => Write response
}

// Handler function
func usersHandler(w http.ResponseWriter, r *http.Request) {
    w.Header().Set("Content-Type", "application/json") // => Set response header
    w.WriteHeader(http.StatusOK)                       // => Set status code

    if r.Method == http.MethodPost {                   // => Check request method
        fmt.Fprint(w, `{"status": "user created"}`)
    } else {
        fmt.Fprint(w, `{"users": []}`)
    }
}

// Handler type
type DataHandler struct{}

func (h *DataHandler) ServeHTTP(w http.ResponseWriter, r *http.Request) { // => ServeHTTP satisfies Handler interface
    fmt.Fprint(w, "Data response")
}
```

**Key Takeaway**: Register handlers with `HandleFunc` (for functions) or `Handle` (for types). Handler functions receive `ResponseWriter` and `*Request`. Use `w.Write()` or `w.WriteString()` to send responses. Set headers and status codes before writing the body.

## Group 4: Standard Library Deep Dive

### Example 26: Time and Duration

Time handling is complex across programming languages. Go's `time` package makes it straightforward with `time.Time` values, `time.Duration` for intervals, and format strings for parsing/formatting.

**Code**:

```go
package main

import (
    "fmt"
    "time"
)

func main() {
    // Current time
    now := time.Now()               // => Get current time
    fmt.Println("Now:", now)        // => Output: 2025-12-23 10:30:45.123456789 +0000 UTC

    // Time arithmetic
    tomorrow := now.Add(24 * time.Hour) // => Add duration
    fmt.Println("Tomorrow:", tomorrow)

    // Parse time
    layout := "2006-01-02"          // => Go uses specific reference time
    parsed, _ := time.Parse(layout, "2025-12-23") // => Parse string to time
    fmt.Println("Parsed:", parsed)

    // Format time
    formatted := now.Format("January 2, 2006") // => Format to string
    fmt.Println("Formatted:", formatted)

    // Duration
    start := time.Now()
    time.Sleep(100 * time.Millisecond)
    elapsed := time.Since(start)    // => Calculate duration
    fmt.Printf("Elapsed: %v\n", elapsed) // => ~100ms

    // Timer and Ticker
    timer := time.NewTimer(200 * time.Millisecond) // => One-shot timer
    <-timer.C                       // => Wait for timer to fire
    fmt.Println("Timer fired")

    ticker := time.NewTicker(100 * time.Millisecond) // => Repeating ticker
    go func() {
        for i := 0; i < 3; i++ {
            <-ticker.C             // => Fire every 100ms
            fmt.Println("Tick", i)
        }
        ticker.Stop()              // => Stop ticker
    }()

    time.Sleep(400 * time.Millisecond) // => Wait for ticks
}
```

**Key Takeaway**: `time.Now()` gets current time. Use `time.Duration` for intervals. Format strings use the reference time "Mon Jan 2 15:04:05 MST 2006" (remember as "2006-01-02 15:04:05"). `time.Timer` fires once, `time.Ticker` fires repeatedly.

### Example 27: Regular Expressions

Regular expressions enable pattern matching. Go's `regexp` package compiles patterns and provides matching/replacing functions. Precompile expensive patterns to avoid recompilation.

**Code**:

```go
package main

import (
    "fmt"
    "regexp"
)

func main() {
    // Compile pattern
    pattern, _ := regexp.Compile(`^[a-z]+@[a-z]+\.[a-z]+$`) // => Precompile pattern

    // Test if string matches
    if pattern.MatchString("alice@example.com") {
        fmt.Println("Valid email")
    }

    // Find matches
    re := regexp.MustCompile(`\d+`)  // => Find all numbers
    matches := re.FindAllString("abc 123 def 456", -1) // => -1 means find all
    fmt.Println("Numbers:", matches) // => [123 456]

    // Replace matches
    text := "Hello World"
    replaced := regexp.MustCompile(`\w+`).ReplaceAllString(text, "[word]")
    fmt.Println("Replaced:", replaced) // => [word] [word]

    // Extract capture groups
    re = regexp.MustCompile(`(\w+)@(\w+)\.(\w+)`) // => Three capture groups
    matches = re.FindStringSubmatch("alice@example.com")
    if len(matches) > 0 {
        fmt.Println("Full:", matches[0])   // => alice@example.com
        fmt.Println("User:", matches[1])   // => alice
        fmt.Println("Domain:", matches[2]) // => example
        fmt.Println("TLD:", matches[3])    // => com
    }
}
```

**Key Takeaway**: Use `regexp.MustCompile()` for patterns known at compile-time. Use `regexp.Compile()` for runtime patterns (returns error). Precompile patterns used repeatedly. Use capture groups `()` to extract parts of matches.

### Example 28: Context Package

The `context` package manages deadlines, cancellation, and request-scoped values. It's essential for building responsive systems that can be cancelled and respect timeouts. Context flows through function calls to coordinate cancellation.

```mermaid
graph TB
    A["func main<br/>ctx, cancel := context.WithTimeout"]
    B["func doWork<br/>receives ctx"]
    C["func subTask<br/>respects ctx"]
    D["context.Done()<br/>receives cancellation"]

    A -->|passes| B
    B -->|passes| C
    C -->|listens to| D

    style A fill:#0173B2,stroke:#000,color:#fff
    style B fill:#DE8F05,stroke:#000,color:#fff
    style C fill:#029E73,stroke:#000,color:#fff
    style D fill:#CC78BC,stroke:#000,color:#fff
```

**Code**:

```go
package main

import (
    "context"
    "fmt"
    "time"
)

func main() {
    // Context with timeout
    ctx, cancel := context.WithTimeout(context.Background(), 2*time.Second)
    defer cancel()                  // => Clean up context

    result := doWork(ctx)           // => Pass context
    fmt.Println("Result:", result)

    // Context with cancellation
    ctx2, cancel2 := context.WithCancel(context.Background())

    go func() {
        time.Sleep(500 * time.Millisecond)
        cancel2()                   // => Cancel context
    }()

    doWork(ctx2)
    fmt.Println("After cancellation")

    // Context values (use sparingly)
    ctx3 := context.WithValue(context.Background(), "user_id", 123)
    userID := ctx3.Value("user_id") // => Retrieve value
    fmt.Println("User ID:", userID)
}

func doWork(ctx context.Context) string {
    for {
        select {
        case <-ctx.Done():          // => Context cancelled or deadline exceeded
            fmt.Println("Work cancelled:", ctx.Err())
            return "cancelled"
        case <-time.After(500 * time.Millisecond):
            fmt.Println("Working...")
        }
    }
}
```

**Key Takeaway**: `context.Background()` is the root context. `WithTimeout()` creates a context with deadline. `WithCancel()` creates a cancellable context. Always `defer cancel()` to avoid leaking goroutines. Listen to `ctx.Done()` to receive cancellation signals.

### Example 29: Flag Parsing

Command-line flags enable configurable programs. Go's `flag` package parses flags automatically, extracting values into variables. Useful for scripts and tools that need configuration from command-line arguments.

**Code**:

```go
package main

import (
    "flag"
    "fmt"
)

func main() {
    // Define flags
    name := flag.String("name", "World", "Name to greet") // => String flag with default
    count := flag.Int("count", 1, "Number of greetings")  // => Integer flag
    verbose := flag.Bool("verbose", false, "Verbose mode") // => Boolean flag

    flag.Parse()                   // => Parse command-line flags

    // Use parsed values
    for i := 0; i < *count; i++ { // => Flags are pointers to values
        greeting := fmt.Sprintf("Hello, %s!", *name)
        fmt.Println(greeting)
    }

    if *verbose {
        fmt.Println("Verbose mode enabled")
    }

    // Remaining arguments after flags
    args := flag.Args()            // => Non-flag arguments
    if len(args) > 0 {
        fmt.Println("Extra arguments:", args)
    }
}

// Usage: go run main.go -name Alice -count 3 -verbose
// Output:
// Hello, Alice!
// Hello, Alice!
// Hello, Alice!
// Verbose mode enabled
```

**Key Takeaway**: Define flags with `flag.String()`, `flag.Int()`, `flag.Bool()`. Call `flag.Parse()` to extract values. Flag values are pointers - dereference with `*`. Use `flag.Args()` to get remaining non-flag arguments.

## Group 5: Production Patterns

### Example 30: HTTP Middleware Pattern

Middleware intercepts requests and responses, enabling cross-cutting concerns like logging, authentication, and error handling. Middleware wraps handlers with decorator pattern - each middleware can inspect requests and responses.

**Code**:

```go
package main

import (
    "fmt"
    "log"
    "net/http"
    "time"
)

func main() {
    mux := http.NewServeMux()

    // Register handler with middleware chain
    handler := http.HandlerFunc(homeHandler)
    handler = loggingMiddleware(handler)      // => Wrap with logging
    handler = authMiddleware(handler)         // => Wrap with auth
    handler = recoveryMiddleware(handler)     // => Wrap with recovery

    mux.Handle("/", handler)
    http.ListenAndServe(":8080", mux)
}

// Middleware function - returns handler that wraps original
func loggingMiddleware(next http.Handler) http.Handler {
    return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
        start := time.Now()
        log.Printf("Started %s %s", r.Method, r.RequestURI)

        next.ServeHTTP(w, r)        // => Call next handler

        log.Printf("Completed in %v", time.Since(start))
    })
}

// Authentication middleware
func authMiddleware(next http.Handler) http.Handler {
    return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
        token := r.Header.Get("Authorization") // => Check request header

        if token == "" {
            http.Error(w, "Unauthorized", http.StatusUnauthorized) // => Reject if no token
            return
        }

        next.ServeHTTP(w, r)        // => Proceed if authorized
    })
}

// Recovery middleware - catch panics
func recoveryMiddleware(next http.Handler) http.Handler {
    return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
        defer func() {
            if err := recover(); err != nil { // => Catch panic
                log.Printf("Panic: %v", err)
                http.Error(w, "Internal Server Error", http.StatusInternalServerError)
            }
        }()

        next.ServeHTTP(w, r)
    })
}

func homeHandler(w http.ResponseWriter, r *http.Request) {
    fmt.Fprint(w, "Hello, Authenticated User!")
}
```

**Key Takeaway**: Middleware wraps handlers, executing code before and after the handler. Create middleware by returning a handler that calls `next.ServeHTTP()`. Chain multiple middleware for cross-cutting concerns like logging, auth, and error handling.

### Example 31: Graceful Shutdown

Production servers need graceful shutdown - stop accepting new requests, finish in-flight requests, clean up resources. Signal handling with `os/signal` enables responding to termination signals like SIGTERM.

**Code**:

```go
package main

import (
    "context"
    "fmt"
    "net/http"
    "os"
    "os/signal"
    "syscall"
    "time"
)

func main() {
    // Create server
    server := &http.Server{
        Addr:           ":8080",
        Handler:        http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
            time.Sleep(2 * time.Second) // => Simulate work
            fmt.Fprint(w, "Response")
        }),
        ReadTimeout:    10 * time.Second,
        WriteTimeout:   10 * time.Second,
        MaxHeaderBytes: 1 << 20,
    }

    // Start server in goroutine
    go func() {
        fmt.Println("Starting server on :8080")
        if err := server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
            fmt.Printf("Server error: %v\n", err)
        }
    }()

    // Wait for interrupt signal
    sigChan := make(chan os.Signal, 1)
    signal.Notify(sigChan, syscall.SIGTERM, syscall.SIGINT) // => Listen for signals

    <-sigChan                        // => Block until signal received
    fmt.Println("\nShutdown signal received, gracefully stopping...")

    // Graceful shutdown with timeout
    ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
    defer cancel()

    if err := server.Shutdown(ctx); err != nil { // => Shutdown gracefully
        fmt.Printf("Shutdown error: %v\n", err)
    }

    fmt.Println("Server stopped")
}
```

**Key Takeaway**: Use `signal.Notify()` to receive OS signals. Call `server.Shutdown(ctx)` with context timeout to gracefully stop. Graceful shutdown waits for in-flight requests to complete before stopping.

### Example 32: Worker Pool Pattern

Worker pools limit concurrent work and improve resource efficiency. Fixed number of workers process jobs from a queue. Useful for API calls, database operations, or any bounded-resource work.

**Code**:

```go
package main

import (
    "fmt"
    "sync"
)

func main() {
    // Create worker pool
    jobChan := make(chan int, 10)  // => Job queue (buffered)
    var wg sync.WaitGroup

    // Start workers
    numWorkers := 3
    for i := 0; i < numWorkers; i++ {
        go worker(i, jobChan, &wg) // => Start worker
    }

    // Submit jobs
    for j := 1; j <= 10; j++ {
        wg.Add(1)                  // => One more job
        jobChan <- j               // => Send job to queue
    }

    wg.Wait()                      // => Wait for all jobs complete
    close(jobChan)                 // => Signal no more jobs
    fmt.Println("All jobs complete")
}

func worker(id int, jobs <-chan int, wg *sync.WaitGroup) {
    for job := range jobs {        // => Receive jobs
        fmt.Printf("Worker %d processing job %d\n", id, job)
        wg.Done()                  // => Mark job complete
    }
}
```

**Key Takeaway**: Create a channel for jobs. Start fixed number of workers that receive from the channel. Send jobs to the channel. Workers process jobs concurrently, bounded by number of workers.

## Group 6: Advanced Testing

### Example 33: Benchmarking

Benchmarking measures performance. Go's `testing.B` type runs test functions multiple times to measure speed. Use benchmarks to catch performance regressions and optimize bottlenecks.

**Code**:

```go
package main

import "testing"

// Run benchmarks with: go test -bench=.
// Memory allocation: go test -bench=. -benchmem

func BenchmarkSliceAppend(b *testing.B) {
    for i := 0; i < b.N; i++ { // => b.N iterations (determined by testing framework)
        s := make([]int, 0)
        for j := 0; j < 100; j++ {
            s = append(s, j)    // => Append to slice
        }
    }
}

func BenchmarkSlicePrealloc(b *testing.B) {
    for i := 0; i < b.N; i++ {
        s := make([]int, 0, 100) // => Preallocate capacity
        for j := 0; j < 100; j++ {
            s = append(s, j)
        }
    }
}

func BenchmarkMapAccess(b *testing.B) {
    m := make(map[string]int)
    for i := 0; i < 1000; i++ {
        m[fmt.Sprintf("key%d", i)] = i
    }

    b.ResetTimer()               // => Reset timer (exclude setup)

    for i := 0; i < b.N; i++ {
        _ = m["key500"]          // => Access operation to benchmark
    }
}

// Memory allocation tracking
func BenchmarkAllocation(b *testing.B) {
    b.ReportAllocs()            // => Report memory allocations

    for i := 0; i < b.N; i++ {
        s := make([]int, 100)    // => Allocation
        _ = s
    }
}
```

**Key Takeaway**: Benchmark functions named `BenchmarkXxx`. Loop from `0` to `b.N` - the testing framework adjusts N to get meaningful results. Use `b.ResetTimer()` to exclude setup time. Use `b.ReportAllocs()` to track allocations.

### Example 34: Examples as Tests

Example functions test code while also serving as documentation. When you run `go test`, examples execute. Output should match comments starting with `// Output:`. Go generates docs from examples.

**Code**:

```go
package main

import "fmt"

// Example functions must be in _test.go file
func ExampleAdd() {
    result := add(2, 3)
    fmt.Println(result)
    // Output: 5
}

func ExampleGreet() {
    name := greet("Alice")
    fmt.Println(name)
    // Output: Hello, Alice!
}

// Unordered output when order varies
func ExampleMapIteration() {
    m := make(map[string]int)
    m["x"] = 1
    m["y"] = 2
    for k, v := range m {
        fmt.Printf("%s:%d ", k, v)
    }
    // Unordered output: x:1 y:2 OR y:2 x:1 OR x:1 y:2 OR ...
}

func add(a, b int) int {
    return a + b
}

func greet(name string) string {
    return fmt.Sprintf("Hello, %s!", name)
}
```

**Key Takeaway**: Example functions start with `Example` and must have `// Output:` comments. The output after `// Output:` must match the function's output exactly. Use `// Unordered output:` when order is non-deterministic.

### Example 35: Test Coverage

Test coverage measures what percentage of code is executed by tests. While 100% coverage doesn't guarantee correctness, measuring coverage reveals untested code. Use `go test -cover` to see coverage percentage.

**Code**:

```go
package main

import (
    "testing"
)

func TestCoverage(t *testing.T) {
    // Test normal case
    result := processValue(10)
    if result != 20 {
        t.Errorf("Expected 20, got %d", result)
    }

    // Test zero case
    result = processValue(0)
    if result != 0 {
        t.Errorf("Expected 0, got %d", result)
    }

    // Test negative case
    result = processValue(-5)
    if result != -10 {
        t.Errorf("Expected -10, got %d", result)
    }
}

func processValue(x int) int {
    if x == 0 {
        return 0                  // => Covered by test
    }
    if x < 0 {
        return x * 2             // => Covered by test
    }
    return x * 2                 // => Covered by test
}

// Run: go test -cover
// Output: coverage: 100.0% of statements
```

**Key Takeaway**: Run `go test -cover` to see coverage percentage. Use `go test -coverprofile=coverage.out` to generate detailed reports. High coverage is good but doesn't replace thoughtful tests.
