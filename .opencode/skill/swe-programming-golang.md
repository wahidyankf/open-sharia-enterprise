---
name: swe-programming-golang
description: Go coding standards from authoritative docs/explanation/software-engineering/programming-languages/golang/ documentation
---

# Go Coding Standards

## Purpose

Progressive disclosure of Go coding standards for agents writing Go code.

**Authoritative Source**: [docs/explanation/software-engineering/programming-languages/golang/README.md](../../docs/explanation/software-engineering/programming-languages/golang/README.md)

**Usage**: Auto-loaded for agents when writing Go code. Provides quick reference to idioms, best practices, and antipatterns.

## Quick Standards Reference

### Naming Conventions

**Packages**: lowercase, single word

- `http`, `json`, `user`, `payment`
- Avoid underscores

**Types and Functions**: MixedCaps

- Exported: `UserAccount`, `CalculateTotal()`
- Unexported: `userAccount`, `calculateTotal()`

**Variables**: Short names in limited scope

- `i`, `j` for loop counters
- `r` for reader, `w` for writer
- Descriptive names for package-level: `defaultTimeout`

**Constants**: MixedCaps (not UPPER_CASE)

- `MaxRetries`, `DefaultTimeout`

### Modern Go Features (Go 1.18+)

**Generics**: Use for type-safe data structures

```go
func Map[T, U any](slice []T, f func(T) U) []U {
    result := make([]U, len(slice))
    for i, v := range slice {
        result[i] = f(v)
    }
    return result
}
```

**Error Wrapping**: Use `fmt.Errorf` with `%w`

```go
if err != nil {
    return fmt.Errorf("failed to process user: %w", err)
}
```

**Struct Embedding**: Use for composition

```go
type User struct {
    BaseModel
    Name string
}
```

### Error Handling

**Explicit Error Returns**: Always check errors

```go
result, err := doSomething()
if err != nil {
    return fmt.Errorf("operation failed: %w", err)
}
```

**Custom Error Types**: Define for specific cases

```go
type ValidationError struct {
    Field string
    Err   error
}

func (e *ValidationError) Error() string {
    return fmt.Sprintf("validation failed for %s: %v", e.Field, e.Err)
}
```

**Error Wrapping**: Preserve error chain

```go
return fmt.Errorf("processing user %s: %w", userID, err)
```

### Concurrency

**Goroutines**: Use for concurrent operations

```go
go func() {
    // Concurrent work
}()
```

**Channels**: Use for communication

```go
ch := make(chan Result, 10) // Buffered
ch <- result                // Send
result := <-ch              // Receive
```

**Context**: Use for cancellation and timeouts

```go
ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
defer cancel()
```

### Testing Standards

**Table-Driven Tests**: Preferred testing pattern

```go
tests := []struct {
    name     string
    input    int
    expected int
}{
    {"positive", 5, 10},
    {"zero", 0, 0},
    {"negative", -5, -10},
}

for _, tt := range tests {
    t.Run(tt.name, func(t *testing.T) {
        result := double(tt.input)
        if result != tt.expected {
            t.Errorf("got %d, want %d", result, tt.expected)
        }
    })
}
```

**Test Helpers**: Use `t.Helper()` for helper functions

```go
func assertEqual(t *testing.T, got, want any) {
    t.Helper()
    if got != want {
        t.Errorf("got %v, want %v", got, want)
    }
}
```

### Security Practices

**Input Validation**: Validate all external input

- Check bounds, formats, and types
- Reject invalid input early

**SQL Injection**: Use parameterized queries

```go
rows, err := db.Query("SELECT * FROM users WHERE id = ?", userID)
```

**Context Timeouts**: Always set timeouts

```go
ctx, cancel := context.WithTimeout(ctx, 30*time.Second)
defer cancel()
```

## Comprehensive Documentation

For detailed guidance, refer to:

- **[Idioms](../../docs/explanation/software-engineering/programming-languages/golang/ex-soen-prla-go__idioms.md)** - Go-specific patterns
- **[Best Practices](../../docs/explanation/software-engineering/programming-languages/golang/ex-soen-prla-go__best-practices.md)** - Clean code standards
- **[Anti-Patterns](../../docs/explanation/software-engineering/programming-languages/golang/ex-soen-prla-go__anti-patterns.md)** - Common mistakes

## Related Skills

- docs-applying-content-quality
- repo-practicing-trunk-based-development

## References

- [Go README](../../docs/explanation/software-engineering/programming-languages/golang/README.md)
- [Functional Programming](../../governance/development/pattern/functional-programming.md)
