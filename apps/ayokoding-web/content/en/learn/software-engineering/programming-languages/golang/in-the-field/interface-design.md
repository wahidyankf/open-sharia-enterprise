---
title: "Interface Design"
date: 2026-02-04T00:00:00+07:00
draft: false
description: "io.Reader/Writer patterns, interface segregation, and small interfaces in Go"
weight: 10000028
tags: ["golang", "interface-design", "io", "patterns", "production"]
---

## Why Interface Design Matters

Go's interface design philosophy centers on small, focused interfaces that compose into larger behaviors. Understanding io.Reader/Writer patterns, interface segregation, and when to define custom interfaces prevents over-abstraction and enables flexible, testable production code.

**Core benefits**:

- **Decoupling**: Code depends on behavior, not concrete types
- **Testability**: Easy to mock interfaces in tests
- **Composability**: Small interfaces combine into complex behaviors
- **Flexibility**: Swap implementations without changing consumers

**Problem**: Developers define large interfaces upfront (like Java/C#), creating tight coupling and difficult testing. Interfaces should be discovered from usage, not designed speculatively.

**Solution**: Learn io.Reader/Writer patterns from standard library, understand implicit interface satisfaction, then define small, focused interfaces at point of use.

## Standard Library: io.Reader and io.Writer

The io package defines fundamental interfaces for reading and writing data.

**Pattern: io.Reader Interface**:

```go
package main

import (
    "fmt"
    // => Standard library for output
    "io"
    // => Standard library I/O interfaces
    "strings"
    // => Standard library string utilities
)

// IO.READER INTERFACE (from standard library):
// type Reader interface {
//     Read(p []byte) (n int, err error)
// }
// => Read fills p with data
// => Returns number of bytes read (n) and error
// => Single method interface (focused responsibility)

func processData(r io.Reader) error {
    // => r is any type implementing Read
    // => Accepts files, buffers, networks, strings
    // => Interface enables polymorphism

    buffer := make([]byte, 1024)
    // => 1KB buffer for reading
    // => Allocated on heap

    n, err := r.Read(buffer)
    // => Read up to 1024 bytes into buffer
    // => n is actual bytes read (≤ 1024)
    // => err is io.EOF when done, or other error

    if err != nil && err != io.EOF {
        // => Error occurred (not EOF)
        // => io.EOF is expected "error" signaling end

        return fmt.Errorf("read error: %w", err)
        // => Wrap error with context
    }

    fmt.Printf("Read %d bytes: %s\n", n, string(buffer[:n]))
    // => buffer[:n] slices to actual data read
    // => Output: Read 11 bytes: hello world

    return nil
}

func main() {
    // strings.Reader implements io.Reader
    reader := strings.NewReader("hello world")
    // => Wraps string as io.Reader
    // => Read method reads from string

    processData(reader)
    // => Works with any io.Reader
    // => Could be *os.File, *bytes.Buffer, *net.Conn, etc.
}
```

**Pattern: io.Writer Interface**:

```go
package main

import (
    "bytes"
    // => Standard library byte buffer
    "fmt"
    "io"
    // => Standard library I/O interfaces
    "os"
    // => Standard library file operations
)

// IO.WRITER INTERFACE (from standard library):
// type Writer interface {
//     Write(p []byte) (n int, err error)
// }
// => Write writes p to underlying data stream
// => Returns number of bytes written (n) and error
// => Single method interface

func writeData(w io.Writer, data string) error {
    // => w is any type implementing Write
    // => Accepts files, buffers, networks
    // => Interface decouples from concrete types

    n, err := w.Write([]byte(data))
    // => []byte(data) converts string to byte slice
    // => Write returns bytes written and error

    if err != nil {
        // => Write failed

        return fmt.Errorf("write error: %w", err)
    }

    fmt.Printf("Wrote %d bytes\n", n)
    // => Output: Wrote 11 bytes

    return nil
}

func main() {
    // Write to bytes.Buffer (in-memory)
    var buf bytes.Buffer
    // => bytes.Buffer implements io.Writer
    // => Stores data in memory

    writeData(&buf, "hello world")
    // => &buf is io.Writer
    // => Data written to buffer

    fmt.Println("Buffer contents:", buf.String())
    // => Output: Buffer contents: hello world

    // Write to os.Stdout (terminal)
    writeData(os.Stdout, "hello stdout\n")
    // => os.Stdout is *os.File (implements io.Writer)
    // => Output: hello stdout
    // => Same function works with different writers
}
```

**Pattern: Composing Readers and Writers**:

```go
package main

import (
    "compress/gzip"
    // => Standard library gzip compression
    "io"
    // => Standard library I/O utilities
    "os"
    // => Standard library file operations
)

func compressFile(inputPath, outputPath string) error {
    // => Compress file using gzip
    // => Demonstrates reader/writer composition

    // Open input file
    input, err := os.Open(inputPath)
    // => input is *os.File (implements io.Reader)
    if err != nil {
        return fmt.Errorf("open input: %w", err)
    }
    defer input.Close()
    // => Ensure file closed

    // Create output file
    output, err := os.Create(outputPath)
    // => output is *os.File (implements io.Writer)
    if err != nil {
        return fmt.Errorf("create output: %w", err)
    }
    defer output.Close()

    // Create gzip writer wrapping output file
    gzipWriter := gzip.NewWriter(output)
    // => gzipWriter implements io.Writer
    // => Wraps output (writes compressed data)
    // => COMPOSITION: gzipWriter → output → disk

    defer gzipWriter.Close()
    // => Flush compressed data on close

    // Copy input to gzip writer
    _, err = io.Copy(gzipWriter, input)
    // => io.Copy reads from input (io.Reader)
    // => Writes to gzipWriter (io.Writer)
    // => Compresses data automatically
    // => Returns bytes copied and error

    if err != nil {
        return fmt.Errorf("copy: %w", err)
    }

    return nil
    // => File compressed successfully
}
```

**Why io.Reader/Writer matter**:

- Standard abstraction across Go ecosystem
- Compose readers/writers for complex behavior (encryption, compression, buffering)
- Easy to test (mock with bytes.Buffer)
- Single method enables any type to implement

**Limitations**:

- No built-in buffering (use bufio.Reader/Writer)
- No seek/position operations (use io.ReadSeeker)
- Error handling manual (no automatic retry)

## Production Pattern: Interface Segregation

Define small interfaces at point of use rather than large interfaces upfront.

**Pattern: Interface Discovery from Usage**:

```go
package main

import "fmt"

// DON'T START WITH THIS (large interface):
// type UserService interface {
//     GetUser(id string) (*User, error)
//     CreateUser(user *User) error
//     UpdateUser(user *User) error
//     DeleteUser(id string) error
//     ListUsers() ([]*User, error)
// }
// => Large interface hard to implement
// => Forces all implementations to provide all methods
// => Tight coupling

// INSTEAD: Define small interfaces at point of use

// UserGetter is defined where needed (consumer defines interface)
type UserGetter interface {
    GetUser(id string) (*User, error)
    // => Single method interface
    // => Focused responsibility
}

// UserCreator defined separately
type UserCreator interface {
    CreateUser(user *User) error
}

// Compose when both needed
type UserRepository interface {
    UserGetter
    UserCreator
    // => Embeds smaller interfaces
    // => Composed from focused interfaces
}

type User struct {
    ID   string
    Name string
}

// Function depends on minimal interface (only GetUser needed)
func displayUser(getter UserGetter, id string) error {
    // => getter is any type with GetUser method
    // => Doesn't require full UserService
    // => Decoupled from concrete implementation

    user, err := getter.GetUser(id)
    if err != nil {
        return fmt.Errorf("get user: %w", err)
    }

    fmt.Printf("User: %s\n", user.Name)
    // => Output: User: Alice

    return nil
}

// Concrete implementation provides all methods
type DatabaseUserService struct {
    // db connection, etc.
}

func (s *DatabaseUserService) GetUser(id string) (*User, error) {
    // => Implements UserGetter
    return &User{ID: id, Name: "Alice"}, nil
}

func (s *DatabaseUserService) CreateUser(user *User) error {
    // => Implements UserCreator
    return nil
}

func main() {
    service := &DatabaseUserService{}
    // => Concrete implementation

    displayUser(service, "user-123")
    // => service satisfies UserGetter (has GetUser method)
    // => Implicit interface satisfaction (no declaration)
    // => Output: User: Alice
}
```

**Pattern: Accept Interfaces, Return Structs**:

```go
package main

import "fmt"

// Logger interface (small, focused)
type Logger interface {
    Log(message string)
    // => Single method interface
}

// ConsoleLogger is concrete struct
type ConsoleLogger struct{}

func (l *ConsoleLogger) Log(message string) {
    fmt.Println(message)
}

// GOOD: Accept interface (flexible)
func ProcessData(logger Logger, data string) {
    // => logger is interface (any Logger implementation)
    // => Easy to test (mock logger)
    // => Decoupled from ConsoleLogger

    logger.Log("Processing: " + data)
    // => Calls Log method (polymorphic)
}

// GOOD: Return struct (concrete)
func NewConsoleLogger() *ConsoleLogger {
    // => Returns concrete type (*ConsoleLogger)
    // => Caller can access all methods
    // => Not limited to interface methods

    return &ConsoleLogger{}
}

func main() {
    // Caller converts concrete to interface as needed
    logger := NewConsoleLogger()
    // => logger is *ConsoleLogger (concrete)

    ProcessData(logger, "test")
    // => Converts *ConsoleLogger to Logger (implicit)
    // => Output: Processing: test
}
```

**Pattern: Empty Interface for True Polymorphism**:

```go
package main

import "fmt"

// Empty interface accepts any type
func printAny(val interface{}) {
    // => interface{} is empty interface (no methods)
    // => Accepts any type (universal interface)
    // => Use sparingly (loses type safety)

    fmt.Printf("Value: %v, Type: %T\n", val, val)
    // => %v prints value, %T prints type
}

func main() {
    printAny(42)
    // => Output: Value: 42, Type: int

    printAny("hello")
    // => Output: Value: hello, Type: string

    printAny(true)
    // => Output: Value: true, Type: bool
}
```

**Go 1.18+: any is alias for interface{}**:

```go
func printAny(val any) {
    // => any is interface{} (built-in alias Go 1.18+)
    // => More readable than interface{}

    fmt.Printf("%v\n", val)
}
```

## Trade-offs: When to Use Each

**Comparison table**:

| Approach             | Type Safety  | Flexibility | Use Case                 |
| -------------------- | ------------ | ----------- | ------------------------ |
| **Concrete types**   | Compile-time | Low         | Known implementation     |
| **Small interfaces** | Compile-time | Medium      | Minimal dependencies     |
| **Large interfaces** | Compile-time | Low         | Legacy code (avoid)      |
| **interface{}**      | Runtime      | High        | Truly heterogeneous data |

**When to use concrete types**:

- Internal functions (not APIs)
- Only one implementation exists
- Performance critical (avoid interface overhead)
- Private helpers

**When to use small interfaces**:

- Public APIs (accept behavior, not types)
- Testing (easy to mock)
- Multiple implementations likely
- Decoupling concerns

**When to define custom interfaces**:

- Discover from usage (consumer defines interface)
- Define at point of use (not upfront)
- Keep interfaces small (1-3 methods)
- Compose larger interfaces from smaller ones

**When to use empty interface (interface{}/any)**:

- JSON unmarshaling (unknown structure)
- Generic containers (before Go 1.18 generics)
- Truly heterogeneous collections
- Last resort (prefer generics Go 1.18+)

## Production Best Practices

**Define interfaces at point of use**:

```go
// GOOD: consumer defines interface
package handler

type UserGetter interface {
    GetUser(id string) (*User, error)
}

func HandleGetUser(getter UserGetter) http.Handler {
    // Uses minimal interface
}

// BAD: provider defines large interface
package service

type UserService interface {
    GetUser(id string) (*User, error)
    CreateUser(...) error
    // ... many methods
}
// Forces implementations to provide all methods
```

**Keep interfaces small (1-3 methods)**:

```go
// GOOD: focused interface
type Notifier interface {
    Notify(message string) error
}

// BAD: large interface
type MessageService interface {
    Notify(msg string) error
    Log(msg string)
    Metrics() Stats
    // Too many responsibilities
}
```

**Accept interfaces, return structs**:

```go
// GOOD
func NewLogger() *ConsoleLogger { }  // Return struct
func Process(logger Logger) { }      // Accept interface

// BAD
func NewLogger() Logger { }          // Return interface (inflexible)
func Process(logger *ConsoleLogger) { }  // Accept struct (coupled)
```

**Use io.Reader/Writer when appropriate**:

```go
// GOOD: use standard interfaces
func processData(r io.Reader) error { }

// BAD: reinvent reader interface
type DataReader interface {
    ReadData() ([]byte, error)
}
func processData(r DataReader) error { }
```

## Summary

Go's interface design prioritizes small, focused interfaces discovered from usage. Standard library io.Reader/Writer demonstrate single-method interfaces that compose into complex behaviors. Define interfaces at point of use (consumer-defined), accept interfaces in functions (flexibility), return structs from constructors (concrete), and keep interfaces small (1-3 methods) for testable, decoupled production code.

**Key takeaways**:

- io.Reader/Writer are fundamental single-method interfaces
- Interfaces satisfied implicitly (no declaration needed)
- Define interfaces at point of use (not upfront)
- Accept interfaces, return structs (flexibility)
- Keep interfaces small (1-3 methods maximum)
- Compose small interfaces into larger ones
- Use interface{}/any sparingly (loses type safety)
