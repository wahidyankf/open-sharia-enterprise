---
title: Go Interfaces and Composition
description: Interface design and composition over inheritance in Go
category: explanation
subcategory: stack-lang-golang
tags:
  - golang
  - interfaces
  - composition
  - embedding
  - polymorphism
---

# Go Interfaces and Composition

**Quick Reference**: [Overview](#overview) | [Interfaces Fundamentals](#interfaces-fundamentals) | [Interface Design](#interface-design) | [Struct Embedding](#struct-embedding) | [Composition Patterns](#composition-patterns) | [Interface vs Concrete Types](#interface-vs-concrete-types) | [Empty Interface](#empty-interface) | [Type Assertions](#type-assertions) | [Type Switches](#type-switches) | [Common Interfaces](#common-interfaces) | [Interface Best Practices](#interface-best-practices) | [Related Documentation](#related-documentation)
**Understanding-oriented documentation** for Go's interface-based polymorphism and struct composition.

## Overview

Go uses interfaces and composition instead of class-based inheritance. This guide covers interface design, struct embedding, and composition patterns.

**Key Concepts**:

- **Interfaces**: Implicit implementation through method sets
- **Composition**: Struct embedding for code reuse
- **Polymorphism**: Interface-based abstraction
- **Duck typing**: "If it walks like a duck and quacks like a duck, it's a duck"

**Philosophy**: "Composition over inheritance", "Accept interfaces, return structs"

## Interfaces Fundamentals

### What is an Interface?

An interface is a set of method signatures. A type implements an interface by implementing all its methods:

- **Implicit implementation**: No "implements" keyword
- **Duck typing**: "If it walks like a duck..."
- **Zero-cost abstraction**: Implemented via method dispatch tables

```go
// Interface definition
type Reader interface {
    Read(p []byte) (n int, err error)
}

// Any type with Read([]byte) (int, error) implements Reader
type FileReader struct {
    path string
}

func (f *FileReader) Read(p []byte) (int, error) {
    // Implementation
    return len(p), nil
}

// No explicit "implements" needed!
var r Reader = &FileReader{path: "file.txt"}
```

### Why Interfaces?

**Decoupling**: Depend on abstractions, not concrete types

```go
// Bad: Depends on concrete type
func ProcessFile(f *os.File) {
    // Can only work with *os.File
}

// Good: Depends on interface
func ProcessReader(r io.Reader) {
    // Works with any Reader implementation
}
```

**Testing**: Easy to mock with interfaces

```go
// Production code
type Database interface {
    Query(sql string) ([]Row, error)
}

// Test mock
type MockDB struct {
    QueryFunc func(string) ([]Row, error)
}

func (m *MockDB) Query(sql string) ([]Row, error) {
    return m.QueryFunc(sql)
}
```

**Flexibility**: Multiple implementations

```go
type Storage interface {
    Save(key string, value []byte) error
    Load(key string) ([]byte, error)
}

// Different implementations
type FileStorage struct{}
type S3Storage struct{}
type RedisStorage struct{}

// All implement Storage interface
```

## Interface Design

### Keep Interfaces Small

Go philosophy: "The bigger the interface, the weaker the abstraction"

```go
// Good: Small, focused interface
type Reader interface {
    Read(p []byte) (n int, err error)
}

// Good: Compose small interfaces
type ReadWriter interface {
    Reader
    Writer
}

// Bad: Large interface with many methods
type DataStore interface {
    Create(data interface{}) error
    Read(id string) (interface{}, error)
    Update(id string, data interface{}) error
    Delete(id string) error
    List() ([]interface{}, error)
    Count() (int, error)
    // Too many methods!
}
```

### Interface Segregation Principle

Define interfaces per use case, not per implementation:

```go
// Define interfaces where they're used (consumer side)

// package beneficiary
type UserStore interface {
    FindByID(id string) (*Beneficiary, error)
    Save(u *Beneficiary) error
}

// package admin
type AdminStore interface {
    FindByID(id string) (*Beneficiary, error)
    Save(u *Beneficiary) error
    Delete(id string) error
    List() ([]*Beneficiary, error)
}

// package database implements both
type PostgresDB struct{}

func (db *PostgresDB) FindByID(id string) (*Beneficiary, error) { return nil, nil }
func (db *PostgresDB) Save(u *Beneficiary) error                { return nil }
func (db *PostgresDB) Delete(id string) error            { return nil }
func (db *PostgresDB) List() ([]*Beneficiary, error)            { return nil, nil }
```

**Benefits**:

- Each package defines what it needs
- Implementations can satisfy multiple interfaces
- Minimal dependencies

### Accept Interfaces, Return Structs

```go
// Good: Accept interface (flexible)
func ProcessData(r io.Reader) (*Result, error) {
    data, err := io.ReadAll(r)
    if err != nil {
        return nil, err
    }
    return &Result{Data: data}, nil
}

// Good: Return concrete type (explicit)
func NewDatabase() *PostgresDB {
    return &PostgresDB{}
}

// Bad: Return interface (hides implementation)
func NewDatabase() Database {
    return &PostgresDB{} // Loses type information
}
```

**Rationale**:

- Accepting interfaces: Callers pass any compatible type
- Returning structs: Callers know exactly what they get
- Exceptions: Factory pattern, when hiding implementation is intentional

### Single-Method Interfaces

Many Go interfaces have single methods:

```go
type Reader interface {
    Read(p []byte) (n int, err error)
}

type Writer interface {
    Write(p []byte) (n int, err error)
}

type Closer interface {
    Close() error
}

// Compose into larger interfaces
type ReadCloser interface {
    Reader
    Closer
}

type WriteCloser interface {
    Writer
    Closer
}

type ReadWriteCloser interface {
    Reader
    Writer
    Closer
}
```

## Struct Embedding

### What is Embedding?

Embedding promotes methods from embedded type to outer type:

```go
type Engine struct {
    Power int
}

func (e Engine) Start() {
    fmt.Println("Engine starting...")
}

// Car embeds Engine
type Car struct {
    Engine // Embedded field (no name)
    Brand  string
}

func main() {
    car := Car{
        Engine: Engine{Power: 200},
        Brand:  "Tesla",
    }

    car.Start() // Promoted method from Engine
    fmt.Println(car.Power) // Promoted field from Engine
}
```

### Embedding vs Inheritance

```go
// NOT inheritance! No polymorphism
type Animal struct {
    Name string
}

func (a Animal) Speak() {
    fmt.Println("...")
}

type Dog struct {
    Animal
}

// Method override (NOT override, just shadowing!)
func (d Dog) Speak() {
    fmt.Println("Woof!")
}

func main() {
    dog := Dog{Animal: Animal{Name: "Rex"}}
    dog.Speak() // "Woof!"

    // But...
    var animal Animal = dog.Animal // Type conversion needed
    animal.Speak() // "..." (calls Animal's method, not Dog's!)

    // No polymorphism!
}
```

**Key difference from inheritance**:

- Embedding is **composition**, not inheritance
- No automatic polymorphism
- Embedded type doesn't know about outer type

### Embedding Multiple Types

```go
type Reader interface {
    Read(p []byte) (int, error)
}

type Writer interface {
    Write(p []byte) (int, error)
}

type Closer interface {
    Close() error
}

// Embed multiple types
type File struct {
    io.Reader // Embedded interface
    io.Writer // Embedded interface
    io.Closer // Embedded interface

    name string
}

// File automatically implements io.ReadWriteCloser
var _ io.ReadWriteCloser = (*File)(nil)
```

### Name Conflicts

```go
type A struct {
    Value int
}

func (a A) Method() {
    fmt.Println("A")
}

type B struct {
    Value int
}

func (b B) Method() {
    fmt.Println("B")
}

type C struct {
    A
    B
}

func main() {
    c := C{}

    // Ambiguous: which Value?
    // c.Value // Compile error!

    // Must be explicit
    c.A.Value = 1
    c.B.Value = 2

    // Ambiguous: which Method?
    // c.Method() // Compile error!

    // Must be explicit
    c.A.Method() // "A"
    c.B.Method() // "B"
}
```

**Resolution**: Use explicit field names to avoid ambiguity

## Composition Patterns

### Decorator Pattern

```go
type Handler interface {
    Handle(req Request) Response
}

// Base handler
type BaseHandler struct{}

func (h BaseHandler) Handle(req Request) Response {
    return Response{Body: "OK"}
}

// Logging decorator
type LoggingHandler struct {
    Handler // Embedded
    logger  *Logger
}

func (h LoggingHandler) Handle(req Request) Response {
    h.logger.Log("Handling request")
    resp := h.Handler.Handle(req) // Delegate
    h.logger.Log("Request handled")
    return resp
}

// Authentication decorator
type AuthHandler struct {
    Handler
    auth *Authenticator
}

func (h AuthHandler) Handle(req Request) Response {
    if !h.auth.Verify(req.Token) {
        return Response{Status: 401}
    }
    return h.Handler.Handle(req)
}

// Usage: Compose decorators
func main() {
    handler := AuthHandler{
        Handler: LoggingHandler{
            Handler: BaseHandler{},
            logger:  NewLogger(),
        },
        auth: NewAuthenticator(),
    }

    response := handler.Handle(request)
}
```

### Adapter Pattern

```go
// External library interface
type LegacyPrinter interface {
    PrintDocument(doc string)
}

// Our interface
type Printer interface {
    Print(content []byte) error
}

// Adapter
type LegacyAdapter struct {
    legacy LegacyPrinter
}

func (a *LegacyAdapter) Print(content []byte) error {
    a.legacy.PrintDocument(string(content))
    return nil
}

// Usage
func main() {
    legacyPrinter := &OldPrinter{}
    modernPrinter := &LegacyAdapter{legacy: legacyPrinter}

    var printer Printer = modernPrinter
    printer.Print([]byte("Hello"))
}
```

### Strategy Pattern

```go
// Strategy interface
type SortStrategy interface {
    Sort(data []int) []int
}

// Concrete strategies
type BubbleSort struct{}

func (s BubbleSort) Sort(data []int) []int {
    // Bubble sort implementation
    return data
}

type QuickSort struct{}

func (s QuickSort) Sort(data []int) []int {
    // Quick sort implementation
    return data
}

// Context
type Sorter struct {
    strategy SortStrategy
}

func (s *Sorter) SetStrategy(strategy SortStrategy) {
    s.strategy = strategy
}

func (s *Sorter) Sort(data []int) []int {
    return s.strategy.Sort(data)
}

// Usage
func main() {
    sorter := &Sorter{}

    // Use bubble sort for small arrays
    sorter.SetStrategy(BubbleSort{})
    result1 := sorter.Sort([]int{3, 1, 2})

    // Use quick sort for large arrays
    sorter.SetStrategy(QuickSort{})
    result2 := sorter.Sort(largeArray)
}
```

### Functional Options Pattern

```go
type Server struct {
    host    string
    port    int
    timeout time.Duration
    tls     *tls.Config
}

// Option function type
type Option func(*Server)

// Option constructors
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

func WithTLS(config *tls.Config) Option {
    return func(s *Server) {
        s.tls = config
    }
}

// Constructor with options
func NewServer(opts ...Option) *Server {
    // Default values
    s := &Server{
        host:    "localhost",
        port:    8080,
        timeout: 30 * time.Second,
    }

    // Apply options
    for _, opt := range opts {
        opt(s)
    }

    return s
}

// Usage
func main() {
    // Default server
    server1 := NewServer()

    // Custom server
    server2 := NewServer(
        WithHost("0.0.0.0"),
        WithPort(9000),
        WithTimeout(60*time.Second),
        WithTLS(tlsConfig),
    )
}
```

## Interface vs Concrete Types

### When to Use Interfaces

```go
// Good: Interface for abstraction
func SaveUser(store UserStore, beneficiary *Beneficiary) error {
    return store.Save(beneficiary)
}

// Good: Interface for testing
func ProcessPayment(gateway PaymentGateway, amount int) error {
    return gateway.Charge(amount)
}

// Good: Interface for multiple implementations
func Notify(notifier Notifier, message string) {
    notifier.Send(message)
}
```

### When to Use Concrete Types

```go
// Good: Concrete type for data
type Beneficiary struct {
    ID    string
    Email string
    Name  string
}

// Good: Concrete type when no abstraction needed
type Config struct {
    Database DatabaseConfig
    Server   ServerConfig
}

// Good: Concrete type for simple operations
func FormatDate(t time.Time) string {
    return t.Format("2006-01-02")
}
```

### Interface Pollution

```go
// Bad: Unnecessary interface
type UserGetter interface {
    GetUser(id string) (*Beneficiary, error)
}

type UserService struct{}

func (s *UserService) GetUser(id string) (*Beneficiary, error) {
    // Implementation
    return &Beneficiary{}, nil
}

// If there's only one implementation and no need for mocking,
// just use the concrete type!

// Good: Use concrete type directly
type UserService struct{}

func (s *UserService) GetUser(id string) (*Beneficiary, error) {
    return &Beneficiary{}, nil
}
```

**Rule**: Define interfaces when you need them, not "just in case"

## Empty Interface

### interface{} (any in Go 1.18+)

Empty interface can hold any value:

```go
// Before Go 1.18
var x interface{}
x = 42
x = "hello"
x = []int{1, 2, 3}

// Go 1.18+: Use 'any' alias
var y any
y = 42
y = "hello"
y = []int{1, 2, 3}
```

### Use Cases

```go
// JSON marshaling
func Marshal(v any) ([]byte, error)

// fmt.Println accepts any type
func Println(a ...any) (n int, err error)

// Container types (before generics)
type Cache struct {
    data map[string]any
}
```

### Problems with any

```go
// Type safety lost
func process(data any) {
    // Must use type assertion
    if s, ok := data.(string); ok {
        fmt.Println(strings.ToUpper(s))
    }
}

// Better: Use generics (Go 1.18+)
func process[T any](data T) {
    // Type is known at compile time
    fmt.Printf("%v\n", data)
}
```

## Type Assertions

### Syntax

```go
// Type assertion
value, ok := interfaceValue.(ConcreteType)
if !ok {
    // Type assertion failed
}

// Without ok (panics if wrong type)
value := interfaceValue.(ConcreteType)
```

### Example

```go
var i interface{} = "hello"

// Safe type assertion
s, ok := i.(string)
if ok {
    fmt.Println(strings.ToUpper(s)) // "HELLO"
}

// Unsafe (panics if wrong type)
s := i.(string)
fmt.Println(s) // "hello"

// Wrong type (panics!)
// n := i.(int) // panic: interface conversion
```

### Type Assertion Best Practices

```go
// Good: Check ok value
func processValue(v interface{}) {
    if s, ok := v.(string); ok {
        fmt.Println("String:", s)
        return
    }
    if n, ok := v.(int); ok {
        fmt.Println("Int:", n)
        return
    }
    fmt.Println("Unknown type")
}

// Better: Use type switch (see next section)
```

## Type Switches

### Syntax

```go
switch v := value.(type) {
case TypeA:
    // v is TypeA
case TypeB:
    // v is TypeB
default:
    // Unknown type
}
```

### Example

```go
func describe(i interface{}) {
    switch v := i.(type) {
    case int:
        fmt.Printf("Integer: %d\n", v)
    case string:
        fmt.Printf("String: %s\n", v)
    case bool:
        fmt.Printf("Boolean: %t\n", v)
    case nil:
        fmt.Println("Nil value")
    default:
        fmt.Printf("Unknown type: %T\n", v)
    }
}

func main() {
    describe(42)       // "Integer: 42"
    describe("hello")  // "String: hello"
    describe(true)     // "Boolean: true"
    describe(nil)      // "Nil value"
    describe([]int{})  // "Unknown type: []int"
}
```

### Type Switch with Interfaces

```go
type Shape interface {
    Area() float64
}

type Circle struct {
    Radius float64
}

func (c Circle) Area() float64 {
    return math.Pi * c.Radius * c.Radius
}

type Rectangle struct {
    Width, Height float64
}

func (r Rectangle) Area() float64 {
    return r.Width * r.Height
}

func processShape(s Shape) {
    // Type switch on interface
    switch shape := s.(type) {
    case Circle:
        fmt.Printf("Circle with radius %.2f\n", shape.Radius)
    case Rectangle:
        fmt.Printf("Rectangle %.2f x %.2f\n", shape.Width, shape.Height)
    default:
        fmt.Println("Unknown shape")
    }

    // Call interface method
    fmt.Printf("Area: %.2f\n", s.Area())
}
```

## Common Interfaces

### io.Reader and io.Writer

```go
// Fundamental I/O interfaces
type Reader interface {
    Read(p []byte) (n int, err error)
}

type Writer interface {
    Write(p []byte) (n int, err error)
}

// Many types implement these
var _ io.Reader = (*os.File)(nil)
var _ io.Reader = (*bytes.Buffer)(nil)
var _ io.Reader = (*strings.Reader)(nil)

// Example implementation
type UppercaseReader struct {
    r io.Reader
}

func (u *UppercaseReader) Read(p []byte) (int, error) {
    n, err := u.r.Read(p)
    for i := 0; i < n; i++ {
        if 'a' <= p[i] && p[i] <= 'z' {
            p[i] -= 32 // Convert to uppercase
        }
    }
    return n, err
}
```

### error Interface

```go
type error interface {
    Error() string
}

// Custom error type
type ValidationError struct {
    Field   string
    Message string
}

func (e ValidationError) Error() string {
    return fmt.Sprintf("%s: %s", e.Field, e.Message)
}

// Usage
func validateEmail(email string) error {
    if !strings.Contains(email, "@") {
        return ValidationError{
            Field:   "email",
            Message: "must contain @",
        }
    }
    return nil
}
```

### fmt.Stringer

```go
type Stringer interface {
    String() string
}

// Custom formatting
type Point struct {
    X, Y int
}

func (p Point) String() string {
    return fmt.Sprintf("(%d, %d)", p.X, p.Y)
}

func main() {
    p := Point{10, 20}
    fmt.Println(p) // "(10, 20)"
}
```

### sort.Interface

```go
type Interface interface {
    Len() int
    Less(i, j int) bool
    Swap(i, j int)
}

// Custom sorting
type ByAge []Person

func (a ByAge) Len() int           { return len(a) }
func (a ByAge) Less(i, j int) bool { return a[i].Age < a[j].Age }
func (a ByAge) Swap(i, j int)      { a[i], a[j] = a[j], a[i] }

func main() {
    people := []Person{
        {"Alice", 30},
        {"Bob", 25},
        {"Charlie", 35},
    }

    sort.Sort(ByAge(people))
    // Sorted by age: Bob (25), Alice (30), Charlie (35)
}
```

## Interface Best Practices

### 1. Keep Interfaces Small

```go
// Good: Small interface
type Saver interface {
    Save() error
}

// Bad: Large interface
type Repository interface {
    Save() error
    Update() error
    Delete() error
    Find() error
    List() error
    Count() error
}
```

### 2. Define Interfaces at Use Site

```go
// package beneficiary (consumer)
type UserStore interface {
    FindByID(id string) (*Beneficiary, error)
}

// package postgres (implementation)
type DB struct{}

func (db *DB) FindByID(id string) (*Beneficiary, error) {
    // Implementation
    return nil, nil
}

// No explicit declaration needed!
```

### 3. Return Concrete Types

```go
// Good: Return concrete type
func NewCache() *Cache {
    return &Cache{}
}

// Avoid: Return interface (usually)
func NewCache() Cacher {
    return &Cache{} // Hides implementation
}
```

### 4. Accept Interfaces

```go
// Good: Accept interface
func ProcessData(r io.Reader) error {
    // Works with any Reader
    return nil
}

// Avoid: Accept concrete type (less flexible)
func ProcessData(f *os.File) error {
    // Only works with *os.File
    return nil
}
```

### 5. Avoid Interface Pollution

Don't create interfaces "just in case":

```go
// Bad: Unnecessary interface
type EmailSender interface {
    SendEmail(to, subject, body string) error
}

type SMTPSender struct{}

func (s *SMTPSender) SendEmail(to, subject, body string) error {
    return nil
}

// If only one implementation exists, skip the interface!
```

### 6. Compose Interfaces

```go
// Small interfaces
type Reader interface {
    Read(p []byte) (int, error)
}

type Closer interface {
    Close() error
}

// Compose them
type ReadCloser interface {
    Reader
    Closer
}
```

## Related Documentation

- [Go Best Practices](./ex-so-stla-go__best-practices.md) - Interface design best practices
- [Go Idioms](./ex-so-stla-go__idioms.md) - Interface usage patterns
- [Go Type Safety](./ex-so-stla-go__type-safety.md) - Type system and interfaces
- [Go Anti-Patterns](./ex-so-stla-go__anti-patterns.md) - Common interface mistakes
- [Go Domain-Driven Design](./ex-so-stla-go__domain-driven-design.md) - Interfaces in DDD

---

**Last Updated**: 2026-01-23
**Go Version**: 1.21+ (baseline), 1.22+ (recommended), 1.23 (latest)
**Maintainers**: Platform Documentation Team
