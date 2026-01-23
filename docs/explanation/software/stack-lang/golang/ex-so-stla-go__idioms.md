---
title: Go Idioms
description: Comprehensive guide to Go-specific patterns, idiomatic constructs, and common programming conventions
category: explanation
subcategory: stack-lang
tags:
  - golang
  - idioms
  - patterns
  - defer
  - panic
  - recover
  - struct-tags
  - functional-options
  - zero-values
  - comma-ok
  - go-1.18
  - go-1.21
  - go-1.22
  - go-1.23
  - go-1.24
  - go-1.25
---

# Go Idioms

**Quick Reference**: [Overview](#overview) | [Defer, Panic, and Recover](#defer-panic-and-recover) | [Zero Values](#zero-values) | [Comma-Ok Idiom](#comma-ok-idiom) | [Blank Identifier](#blank-identifier) | [Struct Tags](#struct-tags) | [Functional Options Pattern](#functional-options-pattern) | [Builder Pattern](#builder-pattern) | [Slice Idioms](#slice-idioms) | [Map Idioms](#map-idioms) | [String Idioms](#string-idioms) | [Interface Idioms](#interface-idioms) | [Error Handling Idioms](#error-handling-idioms) | [Concurrency Idioms](#concurrency-idioms) | [init() Functions](#init-functions) | [Package Organization Idioms](#package-organization-idioms) | [Testing Idioms](#testing-idioms) | [Performance Idioms](#performance-idioms) | [Summary](#summary) | [Additional Resources](#additional-resources)
**Understanding-oriented guide** to Go-specific idioms - patterns, conventions, and constructs that make Go code idiomatic and effective.

## Overview

Go idioms are recurring patterns and conventions that Go developers use to write clear, efficient, and maintainable code. These patterns leverage Go's features effectively and follow the community's established best practices.

### What Makes Code Idiomatic?

```go
// IDIOMATIC: Simple, clear, follows Go conventions
func GetUser(id int) (*Beneficiary, error) {
    if id <= 0 {
        return nil, errors.New("invalid beneficiary id")
    }

    beneficiary, err := db.QueryUser(id)
    if err != nil {
        return nil, fmt.Errorf("query beneficiary: %w", err)
    }

    return beneficiary, nil
}

// NON-IDIOMATIC: Overly complex, doesn't follow conventions
func getUserFromDatabase(userId int) (beneficiary *Beneficiary, errorMessage error) {
    if userId <= 0 {
        errorMessage = errors.New("Invalid Beneficiary ID")
        beneficiary = nil
        return
    }

    beneficiary, errorMessage = db.QueryUser(userId)
    if errorMessage != nil {
        errorMessage = fmt.Errorf("Query Beneficiary: %w", errorMessage)
        beneficiary = nil
        return
    }

    return beneficiary, errorMessage
}
```

**Key Principles**:

1. **Clarity over cleverness**: Prefer clear code over clever tricks
2. **Explicit over implicit**: Make behavior obvious
3. **Simple over complex**: Use simplest solution that works
4. **Consistent with community**: Follow established patterns
5. **Leverage zero values**: Design types to work with zero values

### Go Version Context

This guide covers Go 1.18-1.25 with emphasis on:

- **Go 1.18+**: Generic idioms, workspace patterns
- **Go 1.21+**: min/max/clear built-ins, PGO idioms
- **Go 1.22+**: Loop variable scoping, enhanced routing patterns
- **Go 1.23+**: Iterator idioms, range over func
- **Go 1.24+**: Swiss Tables map idioms, runtime.AddCleanup
- **Go 1.25**: Current stable release

## Defer, Panic, and Recover

Defer, panic, and recover are Go's unique control flow mechanisms.

### Defer Statement

Defer schedules a function call to run after the surrounding function returns:

```go
// Basic defer usage
func ReadFile(path string) ([]byte, error) {
    f, err := os.Open(path)
    if err != nil {
        return nil, err
    }
    defer f.Close() // Guaranteed to run when function returns

    return io.ReadAll(f)
}

// Multiple defers (LIFO order)
func ProcessData() {
    defer fmt.Println("1") // Runs last
    defer fmt.Println("2") // Runs second
    defer fmt.Println("3") // Runs first
    fmt.Println("Processing")
    // Output:
    // Processing
    // 3
    // 2
    // 1
}
```

### Defer for Cleanup

Common defer patterns for resource cleanup:

```go
// File cleanup
func WriteFile(path string, data []byte) error {
    f, err := os.Create(path)
    if err != nil {
        return err
    }
    defer f.Close()

    _, err = f.Write(data)
    return err
}

// Lock cleanup
func (s *Service) UpdateCounter() {
    s.mu.Lock()
    defer s.mu.Unlock()

    s.counter++
}

// DonationTransaction cleanup
func UpdateUser(beneficiary *Beneficiary) error {
    tx, err := db.Begin()
    if err != nil {
        return err
    }
    defer tx.Rollback() // Rollback if not committed

    if err := tx.UpdateUser(beneficiary); err != nil {
        return err
    }

    return tx.Commit() // Commit cancels rollback
}

// Context cancellation cleanup
func ProcessWithTimeout(timeout time.Duration) error {
    ctx, cancel := context.WithTimeout(context.Background(), timeout)
    defer cancel() // Release resources

    return process(ctx)
}
```

### Defer with Named Returns

Defer can modify named return values:

```go
func ProcessFile(path string) (err error) {
    f, err := os.Open(path)
    if err != nil {
        return err
    }

    // Defer can modify err
    defer func() {
        closeErr := f.Close()
        if closeErr != nil && err == nil {
            err = fmt.Errorf("close file: %w", closeErr)
        }
    }()

    // Process file...
    return nil
}

// Tracking function duration
func SlowFunction() (duration time.Duration) {
    start := time.Now()
    defer func() {
        duration = time.Since(start)
    }()

    // Do work...
    time.Sleep(time.Second)
    return // duration will be set by defer
}
```

### Defer Gotchas

```go
// INCORRECT: Defer in loop
for _, file := range files {
    f, err := os.Open(file)
    if err != nil {
        continue
    }
    defer f.Close() // Defers accumulate, files stay open!
    process(f)
}

// CORRECT: Close immediately or use separate function
for _, file := range files {
    if err := processFile(file); err != nil {
        log.Printf("error: %v", err)
    }
}

func processFile(path string) error {
    f, err := os.Open(path)
    if err != nil {
        return err
    }
    defer f.Close() // Closes after this function returns

    return process(f)
}

// INCORRECT: Defer arguments evaluated immediately
func PrintNumbers() {
    x := 1
    defer fmt.Println(x) // Prints 1 (not 2)
    x = 2
}

// CORRECT: Use closure to capture final value
func PrintNumbers() {
    x := 1
    defer func() { fmt.Println(x) }() // Prints 2
    x = 2
}
```

### Panic

Use panic sparingly, only for unrecoverable errors:

```go
// CORRECT: Panic for impossible situations
func unreachable() {
    panic("this code should be unreachable")
}

// CORRECT: Panic in initialization
func init() {
    if config == nil {
        panic("config not initialized")
    }
}

// Must functions (panic on error)
var (
    configRegex = regexp.MustCompile(`^[a-z]+$`)
    templates   = template.Must(template.ParseGlob("*.html"))
)

// Custom Must function
func MustLoadConfig(path string) *Config {
    cfg, err := LoadConfig(path)
    if err != nil {
        panic(fmt.Sprintf("failed to load config: %v", err))
    }
    return cfg
}

// INCORRECT: Panic for expected errors
func ReadFile(path string) []byte {
    data, err := os.ReadFile(path)
    if err != nil {
        panic(err) // Wrong! Return error instead
    }
    return data
}
```

### Recover

Recover catches panics within defer:

```go
// Basic recover
func SafeCall(fn func()) (err error) {
    defer func() {
        if r := recover(); r != nil {
            err = fmt.Errorf("panic: %v", r)
        }
    }()

    fn()
    return nil
}

// HTTP handler with panic recovery
func SafeHandler(h http.HandlerFunc) http.HandlerFunc {
    return func(w http.ResponseWriter, r *http.Request) {
        defer func() {
            if err := recover(); err != nil {
                log.Printf("panic in handler: %v", err)
                http.Error(w, "Internal server error", http.StatusInternalServerError)
            }
        }()

        h(w, r)
    }
}

// Recover with stack trace
func RecoverWithStack() {
    if r := recover(); r != nil {
        buf := make([]byte, 4096)
        n := runtime.Stack(buf, false)
        log.Printf("panic: %v\n%s", r, buf[:n])
    }
}
```

## Zero Values

Go's zero values enable useful patterns where variables are ready to use without explicit initialization.

### Zero Value Examples

```go
// Basic types
var i int          // 0
var f float64      // 0.0
var s string       // ""
var b bool         // false
var p *int         // nil
var sl []int       // nil
var m map[string]int // nil
var fn func()      // nil
var ch chan int    // nil

// Struct zero value
type Counter struct {
    count int // 0
    name  string // ""
}

var c Counter // {count: 0, name: ""}
```

### Designing for Zero Values

```go
// CORRECT: Useful zero value
type Buffer struct {
    data []byte
}

// Works with zero value
var buf Buffer
buf.data = append(buf.data, 'a') // No initialization needed!

// CORRECT: sync.Mutex works with zero value
type SafeCounter struct {
    mu    sync.Mutex // Zero value is unlocked mutex
    count int
}

func (s *SafeCounter) Inc() {
    s.mu.Lock() // Works immediately
    defer s.mu.Unlock()
    s.count++
}

// INCORRECT: Requires initialization
type Cache struct {
    data map[string]string // nil map, needs make()
}

func (c *Cache) Set(key, val string) {
    c.data[key] = val // Panic! nil map
}

// CORRECT: Use sync.Map or initialize lazily
type Cache struct {
    mu   sync.RWMutex
    data map[string]string
}

func (c *Cache) Set(key, val string) {
    c.mu.Lock()
    defer c.mu.Unlock()

    if c.data == nil {
        c.data = make(map[string]string)
    }
    c.data[key] = val
}
```

### Zero Value Patterns

```go
// Constructor pattern
type Config struct {
    Host string
    Port int
    Timeout time.Duration
}

// NewConfig with defaults
func NewConfig() *Config {
    return &Config{
        Host:    "localhost", // Override zero value
        Port:    8080,
        Timeout: 30 * time.Second,
    }
}

// Or use functional options (see later section)
func NewConfig(opts ...Option) *Config {
    cfg := &Config{
        Host:    "localhost",
        Port:    8080,
        Timeout: 30 * time.Second,
    }
    for _, opt := range opts {
        opt(cfg)
    }
    return cfg
}

// Check nil slices and maps
func Process(items []string, config map[string]string) {
    // Nil slices are safe to range over
    for _, item := range items { // Works even if items is nil
        fmt.Println(item)
    }

    // Nil maps are safe to read (return zero value)
    value := config["key"] // Returns "" if config is nil

    // But NOT safe to write
    if config != nil {
        config["key"] = "value"
    }
}
```

## Comma-Ok Idiom

The comma-ok idiom tests success of operations that can fail.

### Map Access

```go
// Check if key exists in map
m := map[string]int{
    "a": 1,
    "b": 2,
}

// Without comma-ok (can't distinguish missing from zero)
value := m["c"] // 0 (but is "c" missing or is value 0?)

// With comma-ok
value, ok := m["c"]
if !ok {
    fmt.Println("key not found")
} else {
    fmt.Printf("value: %d\n", value)
}

// Common pattern: check and use
if value, ok := m["a"]; ok {
    fmt.Printf("found: %d\n", value)
}
```

### Type Assertions

```go
var i interface{} = "hello"

// Without comma-ok (panics if wrong type)
s := i.(string) // OK
n := i.(int)    // Panics!

// With comma-ok (safe)
s, ok := i.(string)
if ok {
    fmt.Printf("string: %s\n", s)
}

n, ok := i.(int)
if ok {
    fmt.Printf("int: %d\n", n)
} else {
    fmt.Println("not an int")
}
```

### Channel Receives

```go
ch := make(chan int, 1)
ch <- 42
close(ch)

// Distinguish value from channel close
value, ok := <-ch
if ok {
    fmt.Printf("received: %d\n", value) // 42
}

value, ok = <-ch
if !ok {
    fmt.Println("channel closed") // Channel is closed
}

// Common pattern: loop until closed
for value := range ch {
    fmt.Println(value)
} // Exits when channel closes
```

### Other Comma-Ok Uses

```go
// Type switch doesn't use comma-ok (different pattern)
func describe(i interface{}) {
    switch v := i.(type) {
    case int:
        fmt.Printf("int: %d\n", v)
    case string:
        fmt.Printf("string: %s\n", v)
    default:
        fmt.Printf("unknown: %T\n", v)
    }
}

// Context deadline
deadline, ok := ctx.Deadline()
if ok {
    fmt.Printf("deadline: %v\n", deadline)
} else {
    fmt.Println("no deadline")
}

// Load from sync.Map
var sm sync.Map
sm.Store("key", "value")

value, ok := sm.Load("key")
if ok {
    fmt.Printf("value: %v\n", value)
}
```

## Blank Identifier

The blank identifier `_` discards values you don't need.

### Ignoring Return Values

```go
// Ignore error (document why!)
_ = conn.Close() // Ignore close error, already handling main error

// Ignore values from multiple returns
value, _ := m["key"] // Ignore ok
_, err := io.Copy(dst, src) // Ignore bytes written

// INCORRECT: Ignoring errors you should handle
_ = json.Unmarshal(data, &v) // DON'T ignore errors!

// CORRECT: Handle errors
if err := json.Unmarshal(data, &v); err != nil {
    return fmt.Errorf("unmarshal: %w", err)
}
```

### Import for Side Effects

```go
// Import for init() side effects
import _ "github.com/lib/pq" // PostgreSQL driver registration

// Database driver pattern
import (
    "database/sql"
    _ "github.com/lib/pq" // Driver registers itself in init()
)

func main() {
    db, err := sql.Open("postgres", "connection-string")
    // ...
}
```

### Interface Implementation Check

```go
// Compile-time check that type implements interface
var _ io.Reader = (*MyReader)(nil)
var _ http.Handler = (*MyHandler)(nil)

type MyReader struct{}

func (m *MyReader) Read(p []byte) (int, error) {
    return 0, io.EOF
}

// If MyReader doesn't implement io.Reader, compilation fails
```

### Blank Fields in Structs

```go
// Force named fields (prevent positional initialization)
type Config struct {
    Host string
    Port int
    _    struct{} // Force named field initialization
}

// CORRECT: Must use named fields
cfg := Config{
    Host: "localhost",
    Port: 8080,
}

// INCORRECT: Positional initialization fails
// cfg := Config{"localhost", 8080} // Compile error!
```

### Loop Variables

```go
// Ignore loop index
for _, value := range slice {
    fmt.Println(value)
}

// Ignore loop value
for key := range map {
    fmt.Println(key)
}

// Just check if slice is non-empty
for range slice {
    // Just iterating, don't need index or value
}
```

## Struct Tags

Struct tags provide metadata for struct fields.

### JSON Tags

```go
type Beneficiary struct {
    ID        int       `json:"id"`
    Name      string    `json:"name"`
    Email     string    `json:"email,omitempty"` // Omit if empty
    Password  string    `json:"-"`               // Never serialize
    CreatedAt time.Time `json:"created_at"`

    // Embedded struct with inline fields
    Address `json:"address"`
}

type Address struct {
    Street  string `json:"street"`
    City    string `json:"city"`
    Country string `json:"country"`
}

// Marshal to JSON
beneficiary := Beneficiary{
    ID:    1,
    Name:  "Alice",
    Email: "alice@example.com",
}

data, _ := json.Marshal(beneficiary)
// {"id":1,"name":"Alice","email":"alice@example.com","created_at":"...","address":{...}}
```

### Common JSON Tag Options

```go
type Example struct {
    // Basic mapping
    Field1 string `json:"field1"`

    // Omit if zero value
    Field2 string `json:"field2,omitempty"`

    // Omit field entirely (never marshal/unmarshal)
    Field3 string `json:"-"`

    // Use "-" as field name (escape with comma)
    Field4 string `json:"-,"`

    // Use field name as-is
    FieldName string `json:"FieldName"`

    // Convert to string
    Field5 int `json:"field5,string"`
}
```

### XML Tags

```go
type Book struct {
    XMLName xml.Name `xml:"book"`
    Title   string   `xml:"title"`
    Author  string   `xml:"author"`
    ISBN    string   `xml:"isbn,attr"` // XML attribute
    Year    int      `xml:"year,omitempty"`
}

// <book isbn="123-456">
//   <title>Go Programming</title>
//   <author>John Doe</author>
//   <year>2024</year>
// </book>
```

### Validation Tags

```go
import "github.com/go-playground/validator/v10"

type Beneficiary struct {
    Email    string `validate:"required,email"`
    Age      int    `validate:"gte=0,lte=150"`
    Password string `validate:"required,min=8"`
    Website  string `validate:"omitempty,url"`
}

func ValidateUser(beneficiary *Beneficiary) error {
    validate := validator.New()
    return validate.Struct(beneficiary)
}
```

### Database Tags (GORM)

```go
type Beneficiary struct {
    ID        uint      `gorm:"primaryKey"`
    Name      string    `gorm:"type:varchar(100);not null"`
    Email     string    `gorm:"uniqueIndex"`
    Age       int       `gorm:"check:age >= 0"`
    CreatedAt time.Time `gorm:"autoCreateTime"`
    UpdatedAt time.Time `gorm:"autoUpdateTime"`
}
```

### Custom Tags

```go
import "reflect"

type Config struct {
    Host string `env:"HOST" default:"localhost"`
    Port int    `env:"PORT" default:"8080"`
}

func LoadFromEnv(cfg interface{}) error {
    v := reflect.ValueOf(cfg).Elem()
    t := v.Type()

    for i := 0; i < t.NumField(); i++ {
        field := t.Field(i)
        envKey := field.Tag.Get("env")
        defaultValue := field.Tag.Get("default")

        value := os.Getenv(envKey)
        if value == "" {
            value = defaultValue
        }

        // Set field value...
    }

    return nil
}
```

### Multiple Tags

```go
type Beneficiary struct {
    Name  string `json:"name" xml:"name" db:"name" validate:"required"`
    Email string `json:"email,omitempty" xml:"email" db:"email" validate:"email"`
}

// Access tags
field, _ := reflect.TypeOf(Beneficiary{}).FieldByName("Name")
jsonTag := field.Tag.Get("json")     // "name"
validateTag := field.Tag.Get("validate") // "required"
```

## Functional Options Pattern

The functional options pattern provides flexible configuration for constructors.

### Basic Pattern

```go
type Server struct {
    host    string
    port    int
    timeout time.Duration
    maxConn int
}

// Option is a function that modifies Server
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

func WithMaxConnections(n int) Option {
    return func(s *Server) {
        s.maxConn = n
    }
}

// Constructor with defaults
func NewServer(opts ...Option) *Server {
    // Default configuration
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

// Usage: clean and flexible
server := NewServer(
    WithHost("0.0.0.0"),
    WithPort(9000),
    WithMaxConnections(200),
)

// Or use defaults
server := NewServer()
```

### Validation in Options

```go
func WithPort(port int) Option {
    return func(s *Server) {
        if port < 1 || port > 65535 {
            // Option can't return error, so panic or use defaults
            log.Printf("invalid port %d, using default", port)
            return
        }
        s.port = port
    }
}

// Alternative: Return error from constructor
func NewServer(opts ...Option) (*Server, error) {
    s := &Server{
        host:    "localhost",
        port:    8080,
        timeout: 30 * time.Second,
    }

    for _, opt := range opts {
        if err := opt(s); err != nil {
            return nil, err
        }
    }

    return s, nil
}

type Option func(*Server) error

func WithPort(port int) Option {
    return func(s *Server) error {
        if port < 1 || port > 65535 {
            return fmt.Errorf("invalid port: %d", port)
        }
        s.port = port
        return nil
    }
}
```

### Option Groups

```go
// Combine related options
func WithDatabase(host string, port int, name string) Option {
    return func(s *Server) {
        s.dbHost = host
        s.dbPort = port
        s.dbName = name
    }
}

// Composite options
func WithProduction() Option {
    return func(s *Server) {
        WithHost("0.0.0.0")(s)
        WithPort(443)(s)
        WithTimeout(60 * time.Second)(s)
        WithMaxConnections(1000)(s)
    }
}

// Usage
server := NewServer(WithProduction())
```

### Generic Options (Go 1.18+)

```go
type Config[T any] struct {
    value T
}

type Option[T any] func(*Config[T])

func WithValue[T any](v T) Option[T] {
    return func(c *Config[T]) {
        c.value = v
    }
}

func NewConfig[T any](opts ...Option[T]) *Config[T] {
    c := &Config[T]{}
    for _, opt := range opts {
        opt(c)
    }
    return c
}

// Usage
intConfig := NewConfig(WithValue(42))
strConfig := NewConfig(WithValue("hello"))
```

## Builder Pattern

The builder pattern constructs complex objects step by step.

### Basic Builder

```go
type Query struct {
    table   string
    columns []string
    where   []string
    limit   int
    offset  int
}

type QueryBuilder struct {
    query Query
}

func NewQueryBuilder(table string) *QueryBuilder {
    return &QueryBuilder{
        query: Query{table: table},
    }
}

func (b *QueryBuilder) Select(columns ...string) *QueryBuilder {
    b.query.columns = columns
    return b
}

func (b *QueryBuilder) Where(condition string) *QueryBuilder {
    b.query.where = append(b.query.where, condition)
    return b
}

func (b *QueryBuilder) Limit(n int) *QueryBuilder {
    b.query.limit = n
    return b
}

func (b *QueryBuilder) Offset(n int) *QueryBuilder {
    b.query.offset = n
    return b
}

func (b *QueryBuilder) Build() Query {
    return b.query
}

// Usage: fluent interface
query := NewQueryBuilder("users").
    Select("id", "name", "email").
    Where("age > 18").
    Where("active = true").
    Limit(10).
    Offset(20).
    Build()
```

### Builder with Validation

```go
func (b *QueryBuilder) Build() (Query, error) {
    if b.query.table == "" {
        return Query{}, errors.New("table name required")
    }
    if len(b.query.columns) == 0 {
        return Query{}, errors.New("columns required")
    }
    return b.query, nil
}

// Usage
query, err := NewQueryBuilder("users").
    Select("id", "name").
    Build()
if err != nil {
    log.Fatal(err)
}
```

### Builder vs Functional Options

```go
// Builder: better for complex objects with many optional fields
query := NewQueryBuilder("users").
    Select("id", "name").
    Where("age > 18").
    OrderBy("name").
    Limit(10).
    Build()

// Functional Options: better for configuration
server := NewServer(
    WithHost("localhost"),
    WithPort(8080),
    WithTimeout(30*time.Second),
)

// Use Builder when:
// - Building complex queries/documents
// - Many interdependent options
// - Order of operations matters
// - Need to validate before building

// Use Functional Options when:
// - Simple configuration
// - Independent options
// - Order doesn't matter
// - Want to provide good defaults
```

## Slice Idioms

Common patterns for working with slices.

### Creating Slices

```go
// Literal
s := []int{1, 2, 3}

// Make with length
s := make([]int, 5) // [0, 0, 0, 0, 0]

// Make with length and capacity
s := make([]int, 5, 10) // len=5, cap=10

// From array
arr := [5]int{1, 2, 3, 4, 5}
s := arr[1:4] // [2, 3, 4]

// Empty slice (preferred over nil for JSON)
var s []int     // nil
s := []int{}    // empty but not nil
s := make([]int, 0) // empty but not nil
```

### Appending

```go
// Append single element
s := []int{1, 2}
s = append(s, 3) // [1, 2, 3]

// Append multiple elements
s = append(s, 4, 5, 6) // [1, 2, 3, 4, 5, 6]

// Append slice
other := []int{7, 8}
s = append(s, other...) // [1, 2, 3, 4, 5, 6, 7, 8]

// Append to nil slice (works!)
var s []int
s = append(s, 1) // [1]
```

### Pre-allocating

```go
// INCORRECT: Growing incrementally (slow)
var result []int
for i := 0; i < 1000; i++ {
    result = append(result, i) // Multiple allocations
}

// CORRECT: Pre-allocate with known size
result := make([]int, 0, 1000) // No reallocations
for i := 0; i < 1000; i++ {
    result = append(result, i)
}

// Alternative: Make with length, fill by index
result := make([]int, 1000)
for i := 0; i < 1000; i++ {
    result[i] = i
}
```

### Filtering

```go
// Filter in place (reuse slice)
func Filter(s []int, predicate func(int) bool) []int {
    n := 0
    for _, v := range s {
        if predicate(v) {
            s[n] = v
            n++
        }
    }
    return s[:n]
}

// Filter with new slice
func Filter(s []int, predicate func(int) bool) []int {
    result := make([]int, 0, len(s))
    for _, v := range s {
        if predicate(v) {
            result = append(result, v)
        }
    }
    return result
}

// Usage
even := Filter([]int{1, 2, 3, 4, 5}, func(n int) bool {
    return n%2 == 0
}) // [2, 4]
```

### Removing Elements

```go
// Remove element at index i
func Remove(s []int, i int) []int {
    return append(s[:i], s[i+1:]...)
}

// Remove without preserving order (faster)
func RemoveFast(s []int, i int) []int {
    s[i] = s[len(s)-1]
    return s[:len(s)-1]
}

// Remove all occurrences of value
func RemoveValue(s []int, value int) []int {
    n := 0
    for _, v := range s {
        if v != value {
            s[n] = v
            n++
        }
    }
    return s[:n]
}
```

### Reversing

```go
func Reverse(s []int) {
    for i, j := 0, len(s)-1; i < j; i, j = i+1, j-1 {
        s[i], s[j] = s[j], s[i]
    }
}

// Or create reversed copy
func ReverseCopy(s []int) []int {
    result := make([]int, len(s))
    for i, v := range s {
        result[len(s)-1-i] = v
    }
    return result
}
```

### Copying

```go
// Copy slice
s := []int{1, 2, 3}
c := make([]int, len(s))
copy(c, s)

// Shallow copy (shares underlying array)
s := []int{1, 2, 3}
c := s[:] // c and s share array!

// Append to copy (doesn't affect original if capacity allows)
s := []int{1, 2, 3}
c := append([]int{}, s...) // Copy via append
```

### Checking Membership

```go
// Check if slice contains value
func Contains(s []int, value int) bool {
    for _, v := range s {
        if v == value {
            return true
        }
    }
    return false
}

// Find index of value
func IndexOf(s []int, value int) int {
    for i, v := range s {
        if v == value {
            return i
        }
    }
    return -1
}
```

### Deduplication

```go
// Remove duplicates (preserves order)
func Unique(s []int) []int {
    seen := make(map[int]bool)
    result := make([]int, 0, len(s))

    for _, v := range s {
        if !seen[v] {
            seen[v] = true
            result = append(result, v)
        }
    }

    return result
}

// With generics (Go 1.18+)
func Unique[T comparable](s []T) []T {
    seen := make(map[T]bool)
    result := make([]T, 0, len(s))

    for _, v := range s {
        if !seen[v] {
            seen[v] = true
            result = append(result, v)
        }
    }

    return result
}
```

## Map Idioms

Common patterns for working with maps.

### Creating Maps

```go
// Literal
m := map[string]int{
    "a": 1,
    "b": 2,
}

// Make
m := make(map[string]int)

// Make with capacity hint
m := make(map[string]int, 100)

// Nil map (read-only)
var m map[string]int // Can read (returns zero), can't write
value := m["key"]     // OK: returns 0
// m["key"] = 1       // Panic!
```

### Safe Map Access

```go
// Check if key exists
m := map[string]int{"a": 1}

value, ok := m["a"]
if ok {
    fmt.Printf("found: %d\n", value)
}

// Use default if missing
value, ok := m["b"]
if !ok {
    value = 0 // Default
}

// Or more concisely
value := m["b"] // Returns 0 if missing
```

### Deleting Keys

```go
m := map[string]int{
    "a": 1,
    "b": 2,
}

// Delete key
delete(m, "a")

// Delete is safe with missing keys
delete(m, "nonexistent") // No panic

// Clear all keys (Go 1.21+)
clear(m)

// Before Go 1.21: create new map
m = make(map[string]int)
```

### Iteration

```go
// Iterate over keys and values
for key, value := range m {
    fmt.Printf("%s: %d\n", key, value)
}

// Iterate over keys only
for key := range m {
    fmt.Println(key)
}

// Iteration order is random!
m := map[string]int{"a": 1, "b": 2, "c": 3}
for k := range m {
    fmt.Println(k) // Random order each time!
}

// Sorted iteration
keys := make([]string, 0, len(m))
for k := range m {
    keys = append(keys, k)
}
sort.Strings(keys)
for _, k := range keys {
    fmt.Printf("%s: %d\n", k, m[k])
}
```

### Map of Slices

```go
// Group items by category
type Item struct {
    Category string
    Name     string
}

items := []Item{
    {Category: "fruit", Name: "apple"},
    {Category: "fruit", Name: "banana"},
    {Category: "vegetable", Name: "carrot"},
}

grouped := make(map[string][]string)
for _, item := range items {
    grouped[item.Category] = append(grouped[item.Category], item.Name)
}
// map[fruit:[apple banana] vegetable:[carrot]]
```

### Map as Set

```go
// Use map[T]bool or map[T]struct{} as set
type Set map[string]struct{}

func NewSet() Set {
    return make(Set)
}

func (s Set) Add(key string) {
    s[key] = struct{}{}
}

func (s Set) Remove(key string) {
    delete(s, key)
}

func (s Set) Contains(key string) bool {
    _, ok := s[key]
    return ok
}

func (s Set) Len() int {
    return len(s)
}

// Usage
set := NewSet()
set.Add("a")
set.Add("b")
fmt.Println(set.Contains("a")) // true
```

### Merging Maps

```go
func Merge(m1, m2 map[string]int) map[string]int {
    result := make(map[string]int, len(m1)+len(m2))

    for k, v := range m1 {
        result[k] = v
    }

    for k, v := range m2 {
        result[k] = v // Overwrites if key exists
    }

    return result
}

// In place merge
func MergeInto(dst, src map[string]int) {
    for k, v := range src {
        dst[k] = v
    }
}
```

### Inverting Maps

```go
// Invert map (swap keys and values)
func Invert(m map[string]int) map[int]string {
    result := make(map[int]string, len(m))
    for k, v := range m {
        result[v] = k // Warning: loses data if values aren't unique!
    }
    return result
}

// Invert with multiple values per key
func InvertMulti(m map[string]int) map[int][]string {
    result := make(map[int][]string)
    for k, v := range m {
        result[v] = append(result[v], k)
    }
    return result
}
```

### Concurrent Map Access

```go
// INCORRECT: Concurrent reads and writes race
var m = make(map[string]int)

go func() {
    m["key"] = 1 // Write
}()

go func() {
    _ = m["key"] // Read (races with write!)
}()

// CORRECT: Use sync.RWMutex
type SafeMap struct {
    mu sync.RWMutex
    m  map[string]int
}

func (sm *SafeMap) Set(key string, value int) {
    sm.mu.Lock()
    defer sm.mu.Unlock()
    sm.m[key] = value
}

func (sm *SafeMap) Get(key string) (int, bool) {
    sm.mu.RLock()
    defer sm.mu.RUnlock()
    value, ok := sm.m[key]
    return value, ok
}

// Or use sync.Map for concurrent access
var m sync.Map

m.Store("key", 1)
value, ok := m.Load("key")
m.Delete("key")
```

## String Idioms

Common patterns for working with strings.

### String Building

```go
// INCORRECT: Concatenation in loop (inefficient)
var result string
for i := 0; i < 1000; i++ {
    result += fmt.Sprintf("%d ", i) // Creates new string each time
}

// CORRECT: Use strings.Builder
var builder strings.Builder
for i := 0; i < 1000; i++ {
    fmt.Fprintf(&builder, "%d ", i)
}
result := builder.String()

// Or pre-allocate capacity
var builder strings.Builder
builder.Grow(1000 * 10) // Estimate size
for i := 0; i < 1000; i++ {
    fmt.Fprintf(&builder, "%d ", i)
}
result := builder.String()
```

### String vs []byte

```go
// String is immutable
s := "hello"
// s[0] = 'H' // Compile error!

// Convert to []byte to modify
b := []byte(s)
b[0] = 'H'
s = string(b) // "Hello"

// Avoid unnecessary conversions
var buf bytes.Buffer
buf.WriteString("hello") // Use WriteString, not Write([]byte("hello"))

// Zero-copy conversion (unsafe, read-only)
import "unsafe"

func StringToBytes(s string) []byte {
    return unsafe.Slice(unsafe.StringData(s), len(s))
}

func BytesToString(b []byte) string {
    return unsafe.String(unsafe.SliceData(b), len(b))
}
```

### String Formatting

```go
// Common format verbs
s := "hello"
i := 42
f := 3.14

fmt.Printf("%s\n", s)    // hello
fmt.Printf("%q\n", s)    // "hello" (quoted)
fmt.Printf("%d\n", i)    // 42
fmt.Printf("%f\n", f)    // 3.140000
fmt.Printf("%.2f\n", f)  // 3.14
fmt.Printf("%v\n", s)    // hello (default)
fmt.Printf("%+v\n", beneficiary) // {Name:Alice Age:30} (with field names)
fmt.Printf("%#v\n", beneficiary) // main.Beneficiary{Name:"Alice", Age:30} (Go syntax)
fmt.Printf("%T\n", beneficiary)  // main.Beneficiary (type)

// Build formatted string
result := fmt.Sprintf("beneficiary %d: %s", id, name)
```

### String Checks

```go
// Check prefix/suffix
strings.HasPrefix("hello world", "hello") // true
strings.HasSuffix("hello world", "world") // true

// Check contains
strings.Contains("hello world", "lo wo") // true

// Count occurrences
strings.Count("cheese", "e") // 3

// Index
strings.Index("hello", "ll")      // 2
strings.LastIndex("hello", "l")   // 3
strings.IndexByte("hello", 'l')   // 2 (faster for single byte)
```

### String Manipulation

```go
// Split
parts := strings.Split("a,b,c", ",") // ["a", "b", "c"]
parts := strings.SplitN("a,b,c", ",", 2) // ["a", "b,c"]

// Join
s := strings.Join([]string{"a", "b", "c"}, ",") // "a,b,c"

// Replace
s := strings.Replace("hello hello", "he", "je", 1) // "jello hello"
s := strings.ReplaceAll("hello hello", "he", "je") // "jello jello"

// Trim
s := strings.TrimSpace("  hello  ") // "hello"
s := strings.Trim("!!!hello!!!", "!") // "hello"
s := strings.TrimPrefix("hello world", "hello ") // "world"
s := strings.TrimSuffix("hello world", " world") // "hello"

// Case
s := strings.ToUpper("hello") // "HELLO"
s := strings.ToLower("HELLO") // "hello"
s := strings.Title("hello world") // "Hello World" (deprecated)
```

### String Iteration

```go
s := "hello 世界"

// Iterate bytes
for i := 0; i < len(s); i++ {
    fmt.Printf("%c ", s[i])
}
// h e l l o   ä ¸ - ç • Œ (incorrect for UTF-8!)

// Iterate runes (correct for UTF-8)
for i, r := range s {
    fmt.Printf("%d: %c\n", i, r)
}
// 0: h
// 1: e
// 2: l
// 3: l
// 4: o
// 5:
// 6: 世
// 9: 界

// Get rune count
runeCount := utf8.RuneCountInString(s) // 8
byteCount := len(s)                    // 12
```

### String Comparison

```go
// Equality
s1 == s2 // Case-sensitive

// Case-insensitive
strings.EqualFold("hello", "HELLO") // true

// Ordering
strings.Compare("a", "b") // -1
strings.Compare("b", "a") // 1
strings.Compare("a", "a") // 0

// Use directly in if
if s1 < s2 {
    // s1 comes before s2 lexicographically
}
```

## Interface Idioms

Common interface patterns.

### Accept Interfaces, Return Structs

```go
// CORRECT: Accept interface (flexible)
func SaveUser(w io.Writer, beneficiary *Beneficiary) error {
    data, err := json.Marshal(beneficiary)
    if err != nil {
        return err
    }
    _, err = w.Write(data)
    return err
}

// Can now use with any Writer
SaveUser(file, beneficiary)
SaveUser(buffer, beneficiary)
SaveUser(httpResponse, beneficiary)

// CORRECT: Return struct (specific)
func LoadConfig(path string) (*Config, error) {
    // Returns concrete type, caller knows exact type
    return &Config{}, nil
}

// INCORRECT: Return interface (limits future changes)
func LoadConfig(path string) (Configurer, error) {
    // Forces all configs to implement Configurer
    // Hard to add new methods to Configurer
    return &Config{}, nil
}
```

### Small Interfaces

```go
// CORRECT: Small, focused interfaces (io package style)
type Reader interface {
    Read(p []byte) (n int, err error)
}

type Writer interface {
    Write(p []byte) (n int, err error)
}

type Closer interface {
    Close() error
}

// Compose when needed
type ReadWriteCloser interface {
    Reader
    Writer
    Closer
}

// INCORRECT: Large interface (hard to implement)
type FileSystem interface {
    Open() error
    Close() error
    Read() ([]byte, error)
    Write([]byte) error
    Seek(int64) error
    Stat() (FileInfo, error)
    // ... many more methods
}
```

### Interface Composition

```go
// Build complex interfaces from simple ones
type ReadWriter interface {
    io.Reader
    io.Writer
}

type ReadWriteCloser interface {
    io.Reader
    io.Writer
    io.Closer
}

// Your own composition
type DataStore interface {
    Reader
    Writer
    Closer
}

func ProcessData(store DataStore) error {
    // Can read, write, and close
    return nil
}
```

### Type Switches

```go
func describe(i interface{}) {
    switch v := i.(type) {
    case int:
        fmt.Printf("Integer: %d\n", v)
    case string:
        fmt.Printf("String: %s\n", v)
    case nil:
        fmt.Println("Nil")
    default:
        fmt.Printf("Unknown type: %T\n", v)
    }
}

// Check multiple types
func size(i interface{}) int {
    switch v := i.(type) {
    case string:
        return len(v)
    case []int:
        return len(v)
    case map[string]int:
        return len(v)
    default:
        return 0
    }
}
```

### Interface Checks at Compile Time

```go
// Ensure type implements interface at compile time
var _ io.Reader = (*MyReader)(nil)
var _ io.Writer = (*MyWriter)(nil)
var _ http.Handler = (*MyHandler)(nil)

// Fails at compile time if interface not satisfied
type MyReader struct{}

func (m *MyReader) Read(p []byte) (int, error) {
    return 0, io.EOF
}

// If Read method is missing or has wrong signature, compilation fails
```

## Error Handling Idioms

See [Error Handling Documentation](./ex-so-stla-go__error-handling.md) for comprehensive error handling patterns.

### Quick Reference

```go
// Check errors immediately
result, err := DoSomething()
if err != nil {
    return fmt.Errorf("do something: %w", err)
}

// Early return pattern
func ProcessUser(id int) error {
    beneficiary, err := FetchUser(id)
    if err != nil {
        return fmt.Errorf("fetch beneficiary: %w", err)
    }

    if err := ValidateUser(beneficiary); err != nil {
        return fmt.Errorf("validate beneficiary: %w", err)
    }

    return SaveUser(beneficiary)
}

// Sentinel errors
var ErrNotFound = errors.New("not found")

// Check with errors.Is
if errors.Is(err, ErrNotFound) {
    // Handle not found
}

// Custom error types with errors.As
var validationErr *ValidationError
if errors.As(err, &validationErr) {
    // Handle validation error
}
```

## Concurrency Idioms

See [Concurrency and Parallelism Documentation](./ex-so-stla-go__concurrency-and-parallelism.md) for comprehensive concurrency patterns.

### Quick Reference

```go
// Worker pool
func WorkerPool(jobs <-chan int, results chan<- int) {
    var wg sync.WaitGroup
    for i := 0; i < numWorkers; i++ {
        wg.Add(1)
        go func() {
            defer wg.Done()
            for job := range jobs {
                results <- process(job)
            }
        }()
    }
    wg.Wait()
    close(results)
}

// Pipeline pattern
func pipeline() <-chan int {
    out := make(chan int)
    go func() {
        defer close(out)
        for i := 0; i < 10; i++ {
            out <- i
        }
    }()
    return out
}

// Select with timeout
select {
case result := <-ch:
    fmt.Println(result)
case <-time.After(time.Second):
    fmt.Println("timeout")
}
```

## init() Functions

The init() function runs automatically before main().

### Basic Usage

```go
var config *Config

func init() {
    // Initialize package-level variables
    config = loadConfig()
}

func main() {
    // config is already initialized
    fmt.Println(config)
}
```

### Multiple init() Functions

```go
// Multiple init() in same file (run in order)
func init() {
    fmt.Println("init 1")
}

func init() {
    fmt.Println("init 2")
}

// Output:
// init 1
// init 2
```

### init() Execution Order

```go
// Package-level variables initialized first
var config = loadConfig() // Runs first

func init() {
    // Then init() functions run
    validateConfig(config)
}

func main() {
    // Finally main() runs
}

// Imports initialize before current package
import (
    "database/sql"
    _ "github.com/lib/pq" // init() registers driver
)

func init() {
    // Database driver is already registered
    db, _ := sql.Open("postgres", "...")
}
```

### Common init() Uses

```go
// Register drivers
func init() {
    sql.Register("postgres", &PostgresDriver{})
}

// Validate configuration
func init() {
    if os.Getenv("API_KEY") == "" {
        panic("API_KEY required")
    }
}

// Pre-compile regex
var emailRegex *regexp.Regexp

func init() {
    emailRegex = regexp.MustCompile(`^[a-z0-9._%+\-]+@[a-z0-9.\-]+\.[a-z]{2,}$`)
}

// Initialize random seed (before Go 1.20)
func init() {
    rand.Seed(time.Now().UnixNano())
}
```

### init() Best Practices

```go
// CORRECT: Simple initialization
func init() {
    config = loadDefaultConfig()
}

// CORRECT: Registration
func init() {
    http.HandleFunc("/", handler)
}

// AVOID: Complex logic
func init() {
    // Don't do expensive operations
    // Don't make network calls
    // Don't start goroutines (usually)
}

// AVOID: Side effects
func init() {
    // Don't write to global state
    // Don't modify external resources
}
```

## Package Organization Idioms

### Package Naming

```go
// CORRECT: Short, lowercase, single word
package http
package json
package beneficiary
package auth

// INCORRECT: Mixed case, underscores, plurals
package HTTP
package user_service
package users
```

### Internal Packages

```go
// Internal packages can only be imported by parent and sibling packages
myapp/
  internal/
    database/  // Only myapp/* can import
      conn.go
  api/
    handler.go // Can import myapp/internal/database

otherapp/
  main.go // CANNOT import myapp/internal/database
```

### Package Structure

```go
// Flat is better than nested
myapp/
  beneficiary/
    beneficiary.go
    service.go
    repository.go
  order/
    order.go
    service.go
  main.go

// Avoid deep nesting
myapp/
  internal/
    domain/
      beneficiary/
        model/
          beneficiary.go // Too deep!
```

## Testing Idioms

### Table-Driven Tests

```go
func TestAdd(t *testing.T) {
    tests := []struct {
        name string
        a, b int
        want int
    }{
        {"positive", 2, 3, 5},
        {"negative", -1, -2, -3},
        {"mixed", -1, 2, 1},
        {"zero", 0, 0, 0},
    }

    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            got := Add(tt.a, tt.b)
            if got != tt.want {
                t.Errorf("Add(%d, %d) = %d, want %d", tt.a, tt.b, got, tt.want)
            }
        })
    }
}
```

### Test Helpers

```go
// Helper function (doesn't count in stack traces)
func assertEqual(t *testing.T, got, want int) {
    t.Helper() // Mark as helper
    if got != want {
        t.Errorf("got %d, want %d", got, want)
    }
}

func TestSomething(t *testing.T) {
    result := DoSomething()
    assertEqual(t, result, 42) // Stack trace shows TestSomething, not assertEqual
}
```

### Setup and Teardown

```go
func TestMain(m *testing.M) {
    // Setup
    setup()

    // Run tests
    code := m.Run()

    // Teardown
    teardown()

    os.Exit(code)
}

// Or per-test setup
func TestSomething(t *testing.T) {
    // Setup
    db := setupTestDB(t)
    defer db.Close() // Teardown

    // Test
    // ...
}
```

### Mocking

```go
// Interface for mocking
type UserStore interface {
    GetUser(id int) (*Beneficiary, error)
}

// Real implementation
type DBUserStore struct {
    db *sql.DB
}

func (s *DBUserStore) GetUser(id int) (*Beneficiary, error) {
    // Real database query
}

// Mock implementation
type MockUserStore struct {
    Users map[int]*Beneficiary
}

func (s *MockUserStore) GetUser(id int) (*Beneficiary, error) {
    beneficiary, ok := s.Users[id]
    if !ok {
        return nil, ErrNotFound
    }
    return beneficiary, nil
}

// Test with mock
func TestGetUser(t *testing.T) {
    store := &MockUserStore{
        Users: map[int]*Beneficiary{
            1: {ID: 1, Name: "Alice"},
        },
    }

    beneficiary, err := store.GetUser(1)
    if err != nil {
        t.Fatal(err)
    }
    if beneficiary.Name != "Alice" {
        t.Errorf("got %s, want Alice", beneficiary.Name)
    }
}
```

## Performance Idioms

### Avoiding Allocations

```go
// SLOW: String concatenation
result := ""
for i := 0; i < 1000; i++ {
    result += strconv.Itoa(i) // Allocates new string each time
}

// FAST: strings.Builder
var builder strings.Builder
builder.Grow(1000 * 10) // Pre-allocate
for i := 0; i < 1000; i++ {
    builder.WriteString(strconv.Itoa(i))
}
result := builder.String()

// SLOW: Slice growth
var result []int
for i := 0; i < 1000; i++ {
    result = append(result, i) // May reallocate
}

// FAST: Pre-allocated slice
result := make([]int, 0, 1000)
for i := 0; i < 1000; i++ {
    result = append(result, i) // No reallocation
}
```

### Struct Layout

```go
// INEFFICIENT: Poor field alignment (24 bytes)
type Bad struct {
    flag  bool    // 1 byte + 7 padding
    count int64   // 8 bytes
    value int32   // 4 bytes + 4 padding
}

// EFFICIENT: Optimal field alignment (16 bytes)
type Good struct {
    count int64   // 8 bytes
    value int32   // 4 bytes
    flag  bool    // 1 byte + 3 padding
}
```

### Benchmarking

```go
func BenchmarkSomething(b *testing.B) {
    // Setup
    data := setupData()

    b.ResetTimer() // Reset timer after setup

    for i := 0; i < b.N; i++ {
        DoSomething(data)
    }
}

// Run: go test -bench=. -benchmem
// Output shows allocations and time per operation
```

## Summary

### Key Idioms by Category

**Control Flow**:

- defer for cleanup
- Early return for error handling
- Comma-ok for safe operations
- Blank identifier for unused values

**Data Structures**:

- Zero values for simple defaults
- Pre-allocation for known sizes
- Map as set with struct{} values
- strings.Builder for concatenation

**Patterns**:

- Functional options for configuration
- Builder for complex construction
- Small interfaces composed together
- Accept interfaces, return structs

**Concurrency**:

- Goroutines for parallelism
- Channels for communication
- Select for multiplexing
- sync package for synchronization

**Organization**:

- init() for registration
- Flat package structure
- Internal packages for encapsulation
- Table-driven tests

### Idiomatic Go Checklist

- [ ] Use defer for cleanup
- [ ] Check errors explicitly
- [ ] Return early on errors
- [ ] Leverage zero values
- [ ] Use comma-ok for safety
- [ ] Accept interfaces, return structs
- [ ] Keep interfaces small
- [ ] Pre-allocate slices/maps when size known
- [ ] Use strings.Builder for concatenation
- [ ] Avoid deep package nesting
- [ ] Write table-driven tests
- [ ] Use meaningful variable names
- [ ] Format with gofmt
- [ ] Run golangci-lint

### Quick Reference

```go
// Defer cleanup
defer f.Close()
defer mu.Unlock()
defer cancel()

// Error handling
if err != nil {
    return fmt.Errorf("context: %w", err)
}

// Comma-ok
value, ok := m["key"]
if !ok {
    // Handle missing key
}

// Pre-allocate
s := make([]int, 0, size)
m := make(map[string]int, size)

// String building
var b strings.Builder
fmt.Fprintf(&b, "formatted %d", value)

// Zero values
var mu sync.Mutex // Ready to use
var buf bytes.Buffer // Ready to use
```

## Additional Resources

### Official Documentation

- [Effective Go](https://go.dev/doc/effective_go)
- [Go Code Review Comments](https://github.com/golang/go/wiki/CodeReviewComments)
- [Go Proverbs](https://go-proverbs.github.io/)

### Style Guides

- [Uber Go Style Guide](https://github.com/uber-go/guide/blob/master/style.md)
- [Google Go Style Guide](https://google.github.io/styleguide/go/)
- [Go Standard Project Layout](https://github.com/golang-standards/project-layout)

### Tools

- [gofmt](https://pkg.go.dev/cmd/gofmt) - Format Go code
- [golangci-lint](https://golangci-lint.run/) - Linter aggregator
- [staticcheck](https://staticcheck.io/) - Advanced static analysis
- [go vet](https://pkg.go.dev/cmd/vet) - Examines Go source code

### Best Practices Resources

- [Go Wiki](https://github.com/golang/go/wiki)
- [Awesome Go](https://github.com/avelino/awesome-go)
- [Go by Example](https://gobyexample.com/)

---

**Related Documentation**:

- [Concurrency and Parallelism](./ex-so-stla-go__concurrency-and-parallelism.md)
- [Interfaces and Composition](./ex-so-stla-go__interfaces-and-composition.md)
- [Error Handling](./ex-so-stla-go__error-handling.md)

**Navigation**: [← Back to Golang Overview](./README.md)

---

**Last Updated**: 2026-01-23
**Go Version**: 1.21+ (baseline), 1.22+ (recommended), 1.23 (latest)
**Maintainers**: Platform Documentation Team
