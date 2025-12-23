---
title: "Handle Errors Effectively"
date: 2025-12-17T10:00:00+07:00
draft: false
weight: 1000002
description: "Practical patterns for Go error handling with wrapping, sentinel errors, and custom error types"
tags: ["golang", "errors", "error-handling", "best-practices"]
categories: ["learn"]
---

## Problem

Go's explicit error handling requires returning and checking errors at every call site. Poor error handling leads to lost context, silent failures, or errors that are difficult to debug.

```go
// ❌ Lost context - what failed and why?
data, err := process()
if err != nil {
  return err // No context about what failed
}
```

This guide shows patterns for handling errors effectively in Go.

## Solution Strategies

### Wrap Errors with Context

Use `fmt.Errorf` with `%w` to add context while preserving the original error.

**When to use**: Any time you're returning an error from a dependency.

```go
// ❌ Returns error without context
func LoadUser(id string) (*User, error) {
  user, err := db.Query(id)
  if err != nil {
    return nil, err // Lost: what operation failed, which ID
  }
  return user, nil
}

// ✅ Wrap error with context
func LoadUser(id string) (*User, error) {
  user, err := db.Query(id)
  if err != nil {
    return nil, fmt.Errorf("loading user %s: %w", id, err)
  }
  return user, nil
}

// Error chain shows full context:
// "loading user 123: querying database: connection refused"
```

**Multiple layers of context:**

```go
// ✅ Each layer adds context
func HandleRequest(w http.ResponseWriter, r *http.Request) {
  id := r.URL.Query().Get("id")

  user, err := LoadUser(id)
  if err != nil {
    // Wrap with handler context
    http.Error(w, fmt.Sprintf("request failed: %v", err), 500)
    return
  }
  // Use user...
}

func LoadUser(id string) (*User, error) {
  user, err := fetchUserFromDB(id)
  if err != nil {
    // Wrap with service context
    return nil, fmt.Errorf("loading user: %w", err)
  }
  return user, nil
}

func fetchUserFromDB(id string) (*User, error) {
  result, err := db.Query("SELECT * FROM users WHERE id = ?", id)
  if err != nil {
    // Wrap with database context
    return nil, fmt.Errorf("querying user %s: %w", id, err)
  }
  return result, nil
}

// Full error chain:
// "request failed: loading user: querying user 123: connection refused"
```

**Benefits:**

- Preserves original error for `errors.Is` and `errors.As`
- Shows execution path in error message
- Makes debugging faster
- Adds context without losing information

### Use errors.Is for Sentinel Error Checking

Check for specific errors using `errors.Is` instead of equality comparison.

**When to use**: Checking if an error matches a sentinel error, regardless of wrapping.

```go
// Define sentinel errors
var (
  ErrNotFound      = errors.New("not found")
  ErrUnauthorized  = errors.New("unauthorized")
  ErrInvalidInput  = errors.New("invalid input")
)

func FindUser(id string) (*User, error) {
  user, err := db.Query(id)
  if err != nil {
    return nil, err
  }
  if user == nil {
    return nil, ErrNotFound
  }
  return user, nil
}

// ❌ Direct comparison fails with wrapped errors
user, err := FindUser("123")
if err == ErrNotFound {
  // Won't match if error was wrapped!
}

// ✅ errors.Is works with wrapped errors
user, err := FindUser("123")
if errors.Is(err, ErrNotFound) {
  // Matches even if wrapped: fmt.Errorf("loading user: %w", ErrNotFound)
  http.Error(w, "user not found", 404)
  return
}
if err != nil {
  // Other error
  http.Error(w, "internal error", 500)
  return
}
```

**Multiple error conditions:**

```go
func ProcessPayment(payment *Payment) error {
  if payment == nil {
    return ErrInvalidInput
  }

  user, err := FindUser(payment.UserID)
  if errors.Is(err, ErrNotFound) {
    return fmt.Errorf("user not found: %w", ErrInvalidInput)
  }
  if errors.Is(err, ErrUnauthorized) {
    return fmt.Errorf("unauthorized user: %w", ErrUnauthorized)
  }
  if err != nil {
    return fmt.Errorf("finding user: %w", err)
  }

  // Process payment...
  return nil
}
```

### Use errors.As for Custom Error Types

Extract structured error information using `errors.As`.

**When to use**: Working with custom error types that have additional fields or methods.

```go
// ✅ Custom error type with structured data
type ValidationError struct {
  Field   string
  Message string
  Value   interface{}
}

func (e *ValidationError) Error() string {
  return fmt.Sprintf("validation failed for %s: %s (value: %v)",
    e.Field, e.Message, e.Value)
}

func ValidateUser(user *User) error {
  if user.Email == "" {
    return &ValidationError{
      Field:   "email",
      Message: "email is required",
      Value:   user.Email,
    }
  }

  if !strings.Contains(user.Email, "@") {
    return &ValidationError{
      Field:   "email",
      Message: "invalid email format",
      Value:   user.Email,
    }
  }

  return nil
}

// ✅ Extract structured error data
err := ValidateUser(user)
var validErr *ValidationError
if errors.As(err, &validErr) {
  // Access structured fields
  fmt.Printf("Field: %s\n", validErr.Field)
  fmt.Printf("Message: %s\n", validErr.Message)
  fmt.Printf("Value: %v\n", validErr.Value)

  // HTTP handler can return detailed response
  http.Error(w, fmt.Sprintf("Invalid %s: %s", validErr.Field, validErr.Message), 400)
  return
}
```

**Multiple error types:**

```go
type NetworkError struct {
  Op  string
  URL string
  Err error
}

func (e *NetworkError) Error() string {
  return fmt.Sprintf("network error during %s to %s: %v", e.Op, e.URL, e.Err)
}

type TimeoutError struct {
  Operation string
  Duration  time.Duration
}

func (e *TimeoutError) Error() string {
  return fmt.Sprintf("operation %s timed out after %v", e.Operation, e.Duration)
}

// Check for specific error types
err := FetchData(url)

var netErr *NetworkError
if errors.As(err, &netErr) {
  log.Printf("Network error calling %s: %v", netErr.URL, netErr.Err)
  return
}

var timeoutErr *TimeoutError
if errors.As(err, &timeoutErr) {
  log.Printf("Timeout after %v during %s", timeoutErr.Duration, timeoutErr.Operation)
  return
}
```

### Return Errors from Goroutines

Capture and handle errors from concurrent operations.

**When to use**: Any goroutine that can fail.

```go
// ❌ Error lost in goroutine
func ProcessItems(items []Item) {
  for _, item := range items {
    go func(i Item) {
      err := process(i)
      if err != nil {
        log.Println(err) // Error logged but not returned!
      }
    }(item)
  }
}

// ✅ Collect errors from goroutines
func ProcessItems(items []Item) error {
  errs := make(chan error, len(items))

  for _, item := range items {
    go func(i Item) {
      errs <- process(i) // Send error (or nil)
    }(item)
  }

  var firstErr error
  for i := 0; i < len(items); i++ {
    if err := <-errs; err != nil && firstErr == nil {
      firstErr = err // Capture first error
    }
  }

  return firstErr
}

// ✅ Use errgroup for coordinated error handling
import "golang.org/x/sync/errgroup"

func ProcessItems(ctx context.Context, items []Item) error {
  g, ctx := errgroup.WithContext(ctx)

  for _, item := range items {
    item := item // Capture for goroutine
    g.Go(func() error {
      return process(item) // Returns error to errgroup
    })
  }

  // Wait returns first error encountered (if any)
  return g.Wait()
}

// ✅ Collect all errors
func ProcessItems(items []Item) []error {
  errs := make(chan error, len(items))

  for _, item := range items {
    go func(i Item) {
      errs <- process(i)
    }(item)
  }

  var errors []error
  for i := 0; i < len(items); i++ {
    if err := <-errs; err != nil {
      errors = append(errors, err)
    }
  }

  return errors // Returns all errors
}
```

### Handle Errors in Defer Statements

Properly handle errors from deferred operations.

**When to use**: Cleanup operations that can fail (closing files, transactions).

```go
// ❌ Ignores error from Close
func ProcessFile(path string) error {
  file, err := os.Open(path)
  if err != nil {
    return err
  }
  defer file.Close() // Error ignored

  return processData(file)
}

// ✅ Capture and check defer error
func ProcessFile(path string) (err error) {
  file, err := os.Open(path)
  if err != nil {
    return err
  }

  defer func() {
    closeErr := file.Close()
    if err == nil {
      err = closeErr // Preserve close error if no other error
    }
  }()

  return processData(file)
}

// ✅ Log defer errors when you can't return them
func ProcessFile(path string) error {
  file, err := os.Open(path)
  if err != nil {
    return err
  }

  defer func() {
    if closeErr := file.Close(); closeErr != nil {
      log.Printf("failed to close %s: %v", path, closeErr)
    }
  }()

  return processData(file)
}
```

**Transaction rollback pattern:**

```go
func TransferFunds(from, to string, amount int) (err error) {
  tx, err := db.Begin()
  if err != nil {
    return fmt.Errorf("starting transaction: %w", err)
  }

  defer func() {
    if err != nil {
      // Rollback on error
      if rbErr := tx.Rollback(); rbErr != nil {
        log.Printf("failed to rollback: %v", rbErr)
      }
      return
    }

    // Commit on success
    err = tx.Commit()
  }()

  if err = withdraw(tx, from, amount); err != nil {
    return fmt.Errorf("withdraw failed: %w", err)
  }

  if err = deposit(tx, to, amount); err != nil {
    return fmt.Errorf("deposit failed: %w", err)
  }

  return nil
}
```

### Create Domain-Specific Error Types

Define error types that represent your domain concepts.

```go
// ✅ Domain-specific errors
type UserError struct {
  UserID string
  Op     string
  Err    error
}

func (e *UserError) Error() string {
  return fmt.Sprintf("user operation %s failed for user %s: %v", e.Op, e.UserID, e.Err)
}

func (e *UserError) Unwrap() error {
  return e.Err // Enable errors.Is/As to check wrapped error
}

type PaymentError struct {
  PaymentID string
  Amount    int
  Reason    string
}

func (e *PaymentError) Error() string {
  return fmt.Sprintf("payment %s for amount %d failed: %s", e.PaymentID, e.Amount, e.Reason)
}

// Usage
func ProcessPayment(paymentID string, amount int) error {
  if amount <= 0 {
    return &PaymentError{
      PaymentID: paymentID,
      Amount:    amount,
      Reason:    "amount must be positive",
    }
  }

  user, err := FindUser(paymentID)
  if err != nil {
    return &UserError{
      UserID: paymentID,
      Op:     "find",
      Err:    err,
    }
  }

  // Process...
  return nil
}

// Caller can handle domain-specific errors
err := ProcessPayment("pay123", -100)

var payErr *PaymentError
if errors.As(err, &payErr) {
  log.Printf("Payment error: ID=%s, Amount=%d, Reason=%s",
    payErr.PaymentID, payErr.Amount, payErr.Reason)
}

var userErr *UserError
if errors.As(err, &userErr) {
  log.Printf("User error: ID=%s, Op=%s, Err=%v",
    userErr.UserID, userErr.Op, userErr.Err)
}
```

### Multi-Error Collection

Collect and return multiple errors when appropriate.

```go
// ✅ Collect validation errors
type ValidationErrors struct {
  Errors []error
}

func (e *ValidationErrors) Error() string {
  var msgs []string
  for _, err := range e.Errors {
    msgs = append(msgs, err.Error())
  }
  return strings.Join(msgs, "; ")
}

func (e *ValidationErrors) Add(err error) {
  if err != nil {
    e.Errors = append(e.Errors, err)
  }
}

func ValidateUser(user *User) error {
  var errs ValidationErrors

  if user.Email == "" {
    errs.Add(errors.New("email is required"))
  }

  if !strings.Contains(user.Email, "@") {
    errs.Add(errors.New("email must contain @"))
  }

  if user.Age < 18 {
    errs.Add(errors.New("user must be 18 or older"))
  }

  if len(errs.Errors) > 0 {
    return &errs
  }

  return nil
}

// Caller can handle all validation errors
err := ValidateUser(user)
var validErrs *ValidationErrors
if errors.As(err, &validErrs) {
  for _, e := range validErrs.Errors {
    fmt.Println("Validation error:", e)
  }
}
```

## Putting It All Together

When you're ready to improve your error handling, start by auditing your error returns. Every time you return an error from a function that calls something else, wrap it with `fmt.Errorf` and `%w`. Add enough context to understand what operation failed and what parameters were involved. This single practice makes debugging infinitely easier.

Define sentinel errors as package-level variables for common error conditions like "not found", "unauthorized", or "invalid input". Use these consistently throughout your package so callers can check for specific conditions with `errors.Is`. This creates a vocabulary of errors that both you and your callers understand.

When you need to attach structured data to errors - like which field failed validation or what network operation timed out - create custom error types. Implement the `Error()` method and optionally `Unwrap()` to participate in error wrapping. Callers can then use `errors.As` to extract your custom type and access the structured fields.

For concurrent operations, never let errors disappear into goroutines. Use channels to collect errors, or better yet, use `errgroup` from `golang.org/x/sync/errgroup` to coordinate goroutines and automatically capture the first error. This makes concurrent error handling as straightforward as sequential code.

Pay attention to deferred cleanup operations. File closes, transaction rollbacks, and other cleanup can fail. Decide whether to return these errors, log them, or both. For transaction-style operations, use named return values so defer can modify the error return based on whether the function succeeds or fails.

## Common Mistakes to Avoid

**Don't use %v when you want wrapping:**

```go
// ❌ Breaks error chain - %v doesn't wrap
return fmt.Errorf("loading user: %v", err)

// ✅ Use %w to preserve error chain
return fmt.Errorf("loading user: %w", err)
```

**Don't ignore errors with underscore:**

```go
// ❌ Silent failure
data, _ := os.ReadFile(path)

// ✅ Handle the error
data, err := os.ReadFile(path)
if err != nil {
  return fmt.Errorf("reading file: %w", err)
}
```

**Don't create errors with fmt.Sprintf:**

```go
// ❌ Returns string, not error
func BadFunc() error {
  return fmt.Sprintf("operation failed") // Compile error!
}

// ✅ Use errors.New or fmt.Errorf
func GoodFunc() error {
  return errors.New("operation failed")
}
```

**Don't panic for recoverable errors:**

```go
// ❌ Panic for normal error
func FindUser(id string) *User {
  user, err := db.Query(id)
  if err != nil {
    panic(err) // Don't panic!
  }
  return user
}

// ✅ Return error
func FindUser(id string) (*User, error) {
  user, err := db.Query(id)
  if err != nil {
    return nil, fmt.Errorf("querying user: %w", err)
  }
  return user, nil
}
```

## Summary

Effective error handling in Go means treating errors as first-class values that carry context through your program. Wrap every error you return with `fmt.Errorf` and `%w`, adding enough information to understand what failed and why. This creates error chains that show the full execution path when problems occur.

Use sentinel errors defined as package-level variables for common conditions like "not found" or "unauthorized". Callers can check for these with `errors.Is`, which works correctly even when errors have been wrapped multiple times. This creates a stable API where error conditions are discoverable and reliable.

Create custom error types when you need to attach structured data - validation field names, network URLs, timeout durations, or any other context that helps debugging or recovery. Implement `Unwrap()` to participate in error chains and enable callers to use `errors.As` to extract your custom type and access the detailed fields.

Never let errors disappear into goroutines. Use channels or `errgroup` to capture errors from concurrent operations and return them to callers. This makes concurrent code as reliable as sequential code for error propagation.

Handle errors from deferred operations appropriately. Decide whether to return them, log them, or both based on whether callers need to know about cleanup failures. Use named return values and closures in defer to combine cleanup errors with function errors intelligently.

These patterns work together to create error handling that's explicit, informative, and actionable. Errors become helpful diagnostic tools rather than mysterious failures, making your code more reliable and easier to debug.

## Related Content

- [Go Best Practices and Idioms](/en/learn/software-engineering/programming-language/golang/explanation/best-practices)
- [Common Go Anti-Patterns](/en/learn/software-engineering/programming-language/golang/explanation/anti-patterns)
- [How to Avoid Nil Panics](/en/learn/software-engineering/programming-language/golang/how-to/avoid-nil-panics)
- [How to Use Goroutines and Channels](/en/learn/software-engineering/programming-language/golang/how-to/use-goroutines-and-channels)
