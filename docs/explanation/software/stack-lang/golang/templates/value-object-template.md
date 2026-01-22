---
title: Go Value Object Template
description: Template for creating Domain-Driven Design value objects in Go with immutability patterns and value-based equality
category: template
tags:
  - golang
  - ddd
  - domain-model
  - value-object
  - immutability
  - go-1.18
  - go-1.21
  - go-1.22
  - go-1.23
  - go-1.24
  - go-1.25
related:
  - ex-so-stla-go__best-practices.md
  - ex-so-stla-go__idioms.md
principles:
  - simplicity-over-complexity
  - explicit-over-implicit
created: 2026-01-22
updated: 2026-01-22
---

# Go Value Object Template

This template provides a standardized structure for creating Domain-Driven Design (DDD) value objects in Go. Value objects are immutable (by convention), defined by their attributes, and have value-based equality.

## Table of Contents

1. [Overview](#overview)
2. [Template Structure](#template-structure)
3. [Immutability in Go](#immutability-in-go)
4. [Value Equality](#value-equality)
5. [Operations Pattern](#operations-pattern)
6. [Validation Strategies](#validation-strategies)
7. [Financial Domain Examples](#financial-domain-examples)
8. [Usage Guidelines](#usage-guidelines)
9. [Testing Value Objects](#testing-value-objects)
10. [Related Documentation](#related-documentation)

## Overview

Value objects in Go differ from Java implementations due to Go's lack of explicit immutability keywords (`final`) and different approach to encapsulation. Go value objects follow these principles:

**Key Characteristics**:

- **Immutability by convention**: No setters, operations return new instances
- **Value-based equality**: Implement `Equals` method comparing all fields
- **Self-validating**: Constructor validates invariants
- **Side-effect free**: Methods don't modify receiver state
- **Unexported fields**: Encapsulation through package visibility

**Go vs Java Differences**:

```go
// Go: Manual immutability (no final keyword)
type Money struct {
 amount   int64  // Unexported field
 currency string // Unexported field
}

// Operations return new instances
func (m Money) Add(other Money) (Money, error) {
 // Returns NEW Money, doesn't modify m
}
```

```java
// Java: Explicit immutability with final
public final class Money {
    private final BigDecimal amount; // final keyword
    private final Currency currency; // final keyword

    // Operations return new instances
    public Money add(Money other) {
        // Returns NEW Money, this is final
    }
}
```

## Template Structure

```go
package model

import (
 "errors"
 "fmt"
)

// ValueObjectName represents [brief description of what this value object models].
//
// Characteristics:
//   - Immutable: Cannot be modified after creation (by convention)
//   - Value-based equality: Two instances with same values are equal
//   - Self-validating: Ensures invariants at construction
//   - Side-effect free: Methods don't modify state
//
// Invariants:
//   - [Invariant 1]
//   - [Invariant 2]
//   - [Invariant 3]
//
// Example:
//
// vo := NewValueObjectName("value1", 42, NestedValueObject{})
// updated := vo.WithAttribute1("newValue") // Returns new instance
type ValueObjectName struct {
 // Unexported fields for encapsulation
 attribute1 string
 attribute2 int
 attribute3 NestedValueObject
}

// ========================================
// Constructor (Factory Function)
// ========================================

// NewValueObjectName creates a new ValueObjectName with validation.
//
// Returns error if any invariant is violated.
func NewValueObjectName(attribute1 string, attribute2 int, attribute3 NestedValueObject) (ValueObjectName, error) {
 // Validate all invariants
 if err := validateAttribute1(attribute1); err != nil {
  return ValueObjectName{}, fmt.Errorf("invalid attribute1: %w", err)
 }

 if err := validateAttribute2(attribute2); err != nil {
  return ValueObjectName{}, fmt.Errorf("invalid attribute2: %w", err)
 }

 if err := attribute3.Validate(); err != nil {
  return ValueObjectName{}, fmt.Errorf("invalid attribute3: %w", err)
 }

 return ValueObjectName{
  attribute1: attribute1,
  attribute2: attribute2,
  attribute3: attribute3,
 }, nil
}

// MustNewValueObjectName creates a ValueObjectName, panicking on invalid input.
//
// Use only when input is guaranteed valid (e.g., hard-coded values, config).
func MustNewValueObjectName(attribute1 string, attribute2 int, attribute3 NestedValueObject) ValueObjectName {
 vo, err := NewValueObjectName(attribute1, attribute2, attribute3)
 if err != nil {
  panic(fmt.Sprintf("invalid ValueObjectName: %v", err))
 }
 return vo
}

// ParseValueObjectName parses a ValueObjectName from string representation.
//
// Expected format: "attr1:attr2:attr3"
func ParseValueObjectName(s string) (ValueObjectName, error) {
 if s == "" {
  return ValueObjectName{}, errors.New("empty string")
 }

 // Parse logic...
 // parts := strings.Split(s, ":")
 // ...

 return ValueObjectName{}, errors.New("not implemented")
}

// ========================================
// Validation (Private Functions)
// ========================================

func validateAttribute1(attr string) error {
 if attr == "" {
  return errors.New("must not be empty")
 }

 if len(attr) < 3 || len(attr) > 50 {
  return errors.New("must be between 3 and 50 characters")
 }

 return nil
}

func validateAttribute2(attr int) error {
 if attr < 0 {
  return errors.New("must not be negative")
 }

 if attr > 1000 {
  return errors.New("must not exceed 1000")
 }

 return nil
}

// ========================================
// Operations (Return New Instances)
// ========================================

// WithAttribute1 returns a new ValueObjectName with updated attribute1.
func (v ValueObjectName) WithAttribute1(newAttr1 string) (ValueObjectName, error) {
 return NewValueObjectName(newAttr1, v.attribute2, v.attribute3)
}

// WithAttribute2 returns a new ValueObjectName with updated attribute2.
func (v ValueObjectName) WithAttribute2(newAttr2 int) (ValueObjectName, error) {
 return NewValueObjectName(v.attribute1, newAttr2, v.attribute3)
}

// PerformOperation performs a domain operation returning a new instance.
func (v ValueObjectName) PerformOperation(other ValueObjectName) (ValueObjectName, error) {
 // Business logic producing new value object
 result1 := v.attribute1 + other.attribute1
 result2 := v.attribute2 + other.attribute2
 result3 := v.attribute3.Combine(other.attribute3)

 return NewValueObjectName(result1, result2, result3)
}

// ========================================
// Query Methods (Getters)
// ========================================

// Attribute1 returns the first attribute value.
func (v ValueObjectName) Attribute1() string {
 return v.attribute1
}

// Attribute2 returns the second attribute value.
func (v ValueObjectName) Attribute2() int {
 return v.attribute2
}

// Attribute3 returns the third attribute value (copy if mutable).
func (v ValueObjectName) Attribute3() NestedValueObject {
 // If NestedValueObject is mutable, return defensive copy
 return v.attribute3
}

// SatisfiesCondition checks if this value object meets a specific condition.
func (v ValueObjectName) SatisfiesCondition() bool {
 return v.attribute2 > 100 && len(v.attribute1) > 5
}

// ========================================
// Equality (Value-Based)
// ========================================

// Equals compares this ValueObjectName with another for value equality.
func (v ValueObjectName) Equals(other ValueObjectName) bool {
 return v.attribute1 == other.attribute1 &&
  v.attribute2 == other.attribute2 &&
  v.attribute3.Equals(other.attribute3)
}

// ========================================
// String Representation
// ========================================

// String returns a string representation of the value object.
func (v ValueObjectName) String() string {
 return fmt.Sprintf("%s:%d:%v", v.attribute1, v.attribute2, v.attribute3)
}
```

## Immutability in Go

Go doesn't have Java's `final` keyword. Immutability is achieved through **convention and patterns**:

### No Setters Pattern

```go
// CORRECT: Immutable value object (no setters)
type EmailAddress struct {
 value string
}

func NewEmailAddress(email string) (EmailAddress, error) {
 if err := validateEmail(email); err != nil {
  return EmailAddress{}, err
 }
 return EmailAddress{value: email}, nil
}

// No setter! Use WithValue instead
func (e EmailAddress) WithValue(newEmail string) (EmailAddress, error) {
 return NewEmailAddress(newEmail) // Returns NEW instance
}

func (e EmailAddress) Value() string {
 return e.value
}

// INCORRECT: Mutable value object (has setter)
type MutableEmailAddress struct {
 Value string // Exported field (can be modified)
}

func (e *MutableEmailAddress) SetValue(email string) {
 e.Value = email // Mutates receiver!
}
```

### Operations Return New Instances

```go
// CORRECT: Operations return new instances
type Rectangle struct {
 width  int
 height int
}

func (r Rectangle) Scale(factor int) Rectangle {
 return Rectangle{
  width:  r.width * factor,
  height: r.height * factor,
 }
}

// Usage: Chaining returns new instances
rect := NewRectangle(10, 20)
scaled := rect.Scale(2)          // rect unchanged, scaled is new
doubled := scaled.Scale(2)       // scaled unchanged, doubled is new

// INCORRECT: Mutation with pointer receiver
func (r *Rectangle) MutatingScale(factor int) {
 r.width *= factor   // Mutates original!
 r.height *= factor
}
```

### Defensive Copying for Mutable Fields

```go
// CORRECT: Defensive copy of mutable slice
type UserPermissions struct {
 roles []string // Mutable slice
}

func NewUserPermissions(roles []string) UserPermissions {
 // Defensive copy on construction
 rolesCopy := make([]string, len(roles))
 copy(rolesCopy, roles)

 return UserPermissions{roles: rolesCopy}
}

func (u UserPermissions) Roles() []string {
 // Defensive copy on access
 rolesCopy := make([]string, len(u.roles))
 copy(rolesCopy, u.roles)
 return rolesCopy
}

// INCORRECT: No defensive copy (mutable!)
func (u UserPermissions) RolesNoCopy() []string {
 return u.roles // Caller can modify internal slice!
}
```

## Value Equality

Go value objects implement custom `Equals` method instead of overriding built-in `==`:

### Basic Equality

```go
type Point struct {
 x, y int
}

// Equals implements value-based equality
func (p Point) Equals(other Point) bool {
 return p.x == other.x && p.y == other.y
}

// Usage
p1 := Point{x: 10, y: 20}
p2 := Point{x: 10, y: 20}

// Struct equality (works for simple cases)
fmt.Println(p1 == p2) // true (Go compares fields)

// Custom equality (explicit, preferred for clarity)
fmt.Println(p1.Equals(p2)) // true
```

### Complex Equality (Nested Values)

```go
type Address struct {
 street  string
 city    string
 country string
}

func (a Address) Equals(other Address) bool {
 return a.street == other.street &&
  a.city == other.city &&
  a.country == other.country
}

type Person struct {
 name    string
 age     int
 address Address
}

func (p Person) Equals(other Person) bool {
 return p.name == other.name &&
  p.age == other.age &&
  p.address.Equals(other.address) // Delegate to nested Equals
}
```

### Equality with Slices

```go
type Tags struct {
 values []string
}

// Equals compares slice contents
func (t Tags) Equals(other Tags) bool {
 if len(t.values) != len(other.values) {
  return false
 }

 for i := range t.values {
  if t.values[i] != other.values[i] {
   return false
  }
 }

 return true
}

// Alternative: Use slices.Equal (Go 1.21+)
import "slices"

func (t Tags) Equals(other Tags) bool {
 return slices.Equal(t.values, other.values)
}
```

## Operations Pattern

All operations on value objects return **new instances**:

### Arithmetic Operations

```go
type Quantity struct {
 value int
}

func NewQuantity(value int) (Quantity, error) {
 if value < 0 {
  return Quantity{}, errors.New("quantity must not be negative")
 }
 return Quantity{value: value}, nil
}

// Add returns a new Quantity with sum
func (q Quantity) Add(other Quantity) (Quantity, error) {
 return NewQuantity(q.value + other.value)
}

// Subtract returns a new Quantity with difference
func (q Quantity) Subtract(other Quantity) (Quantity, error) {
 return NewQuantity(q.value - other.value)
}

// Multiply returns a new Quantity with product
func (q Quantity) Multiply(factor int) (Quantity, error) {
 return NewQuantity(q.value * factor)
}

func (q Quantity) Value() int {
 return q.value
}
```

### Transformation Operations

```go
type Name struct {
 value string
}

// ToUpperCase returns new Name with uppercase value
func (n Name) ToUpperCase() Name {
 return Name{value: strings.ToUpper(n.value)}
}

// ToLowerCase returns new Name with lowercase value
func (n Name) ToLowerCase() Name {
 return Name{value: strings.ToLower(n.value)}
}

// Append returns new Name with appended suffix
func (n Name) Append(suffix string) (Name, error) {
 newValue := n.value + suffix
 if len(newValue) > 100 {
  return Name{}, errors.New("name too long")
 }
 return Name{value: newValue}, nil
}
```

### Chaining Operations

```go
// Operations can be chained
name := NewName("john")
result := name.ToUpperCase().Append(" DOE") // Each returns new instance

// With error handling
name, err := NewName("john")
if err != nil {
 return err
}

upper := name.ToUpperCase()
final, err := upper.Append(" DOE")
if err != nil {
 return err
}
```

## Validation Strategies

### Constructor Validation (Preferred)

```go
type TaxID struct {
 value string
}

// NewTaxID validates on construction
func NewTaxID(value string) (TaxID, error) {
 if value == "" {
  return TaxID{}, errors.New("tax ID must not be empty")
 }

 // Remove non-digits
 digits := removeNonDigits(value)

 if len(digits) != 9 {
  return TaxID{}, errors.New("tax ID must have 9 digits")
 }

 // Format: XXX-XX-XXXX
 formatted := fmt.Sprintf("%s-%s-%s", digits[:3], digits[3:5], digits[5:])

 return TaxID{value: formatted}, nil
}

func (t TaxID) Value() string {
 return t.value
}

func removeNonDigits(s string) string {
 var result strings.Builder
 for _, ch := range s {
  if ch >= '0' && ch <= '9' {
   result.WriteRune(ch)
  }
 }
 return result.String()
}
```

### Panic vs Error Return

```go
// Pattern 1: Error return (preferred for user input)
func NewEmail(email string) (Email, error) {
 if !isValidEmail(email) {
  return Email{}, errors.New("invalid email format")
 }
 return Email{value: email}, nil
}

// Pattern 2: Panic (only for programmer errors)
func MustNewEmail(email string) Email {
 e, err := NewEmail(email)
 if err != nil {
  panic(fmt.Sprintf("invalid email: %v", err))
 }
 return e
}

// Usage: Error return for runtime validation
email, err := NewEmail(userInput)
if err != nil {
 return fmt.Errorf("invalid email: %w", err)
}

// Usage: Panic for compile-time constants
var adminEmail = MustNewEmail("admin@example.com")
```

### Multi-Field Validation

```go
type DateRange struct {
 start time.Time
 end   time.Time
}

func NewDateRange(start, end time.Time) (DateRange, error) {
 // Validate individual fields
 if start.IsZero() {
  return DateRange{}, errors.New("start date required")
 }

 if end.IsZero() {
  return DateRange{}, errors.New("end date required")
 }

 // Validate relationship between fields
 if end.Before(start) {
  return DateRange{}, errors.New("end date must be after start date")
 }

 return DateRange{start: start, end: end}, nil
}

func (d DateRange) Start() time.Time {
 return d.start
}

func (d DateRange) End() time.Time {
 return d.end
}

func (d DateRange) Duration() time.Duration {
 return d.end.Sub(d.start)
}
```

## Financial Domain Examples

### Money Value Object

Complete implementation with precise decimal arithmetic:

```go
package model

import (
 "errors"
 "fmt"
 "math/big"
 "strings"
)

// Money represents an amount of money in a specific currency.
//
// Characteristics:
//   - Immutable: Cannot be modified after creation
//   - Value-based equality: Two Money instances with same amount and currency are equal
//   - Precise decimal arithmetic using big.Rat for accuracy
//   - Currency-aware operations prevent mixing currencies
//
// Invariants:
//   - Amount must not be nil
//   - Currency must be valid ISO-4217 code (3 uppercase letters)
//   - Amount precision matches currency's decimal places
//
// Example:
//
// usd := MustNewMoney(10000, "USD")  // $100.00 (stored as cents)
// doubled := usd.Multiply(2)         // $200.00
// total := usd.Add(MustNewMoney(5000, "USD")) // $150.00
type Money struct {
 amount   int64  // Amount in smallest currency unit (e.g., cents)
 currency string // ISO-4217 currency code
}

// Common currencies with their decimal places
var currencyDecimalPlaces = map[string]int{
 "USD": 2, // US Dollar
 "EUR": 2, // Euro
 "GBP": 2, // British Pound
 "JPY": 0, // Japanese Yen (no decimal places)
 "IDR": 2, // Indonesian Rupiah
}

// ========================================
// Constructors
// ========================================

// NewMoney creates Money with the given amount (in smallest unit) and currency.
//
// Amount should be in smallest currency unit (e.g., cents for USD).
// Returns error if currency is invalid.
func NewMoney(amount int64, currencyCode string) (Money, error) {
 if err := validateCurrency(currencyCode); err != nil {
  return Money{}, fmt.Errorf("invalid currency: %w", err)
 }

 return Money{
  amount:   amount,
  currency: strings.ToUpper(currencyCode),
 }, nil
}

// MustNewMoney creates Money, panicking on invalid input.
//
// Use only for compile-time constants or guaranteed valid input.
func MustNewMoney(amount int64, currencyCode string) Money {
 m, err := NewMoney(amount, currencyCode)
 if err != nil {
  panic(fmt.Sprintf("invalid money: %v", err))
 }
 return m
}

// NewMoneyFromFloat creates Money from floating-point amount.
//
// WARNING: Float arithmetic is imprecise. Prefer NewMoney with integer cents.
// Example: NewMoneyFromFloat(100.50, "USD") creates $100.50
func NewMoneyFromFloat(amount float64, currencyCode string) (Money, error) {
 if err := validateCurrency(currencyCode); err != nil {
  return Money{}, fmt.Errorf("invalid currency: %w", err)
 }

 decimalPlaces := getCurrencyDecimalPlaces(currencyCode)
 multiplier := int64(1)
 for i := 0; i < decimalPlaces; i++ {
  multiplier *= 10
 }

 // Convert to smallest unit (e.g., cents)
 cents := int64(amount * float64(multiplier))

 return Money{
  amount:   cents,
  currency: strings.ToUpper(currencyCode),
 }, nil
}

// Zero creates zero Money in the specified currency.
func Zero(currencyCode string) (Money, error) {
 return NewMoney(0, currencyCode)
}

// MustZero creates zero Money, panicking on invalid currency.
func MustZero(currencyCode string) Money {
 return MustNewMoney(0, currencyCode)
}

// ParseMoney parses Money from string format "100.50 USD".
func ParseMoney(s string) (Money, error) {
 parts := strings.Fields(s)
 if len(parts) != 2 {
  return Money{}, errors.New("invalid format, expected 'AMOUNT CURRENCY'")
 }

 // Parse amount as float, then convert
 var amount float64
 _, err := fmt.Sscanf(parts[0], "%f", &amount)
 if err != nil {
  return Money{}, fmt.Errorf("invalid amount: %w", err)
 }

 return NewMoneyFromFloat(amount, parts[1])
}

// ========================================
// Arithmetic Operations
// ========================================

// Add returns a new Money with the sum.
//
// Returns error if currencies don't match.
func (m Money) Add(other Money) (Money, error) {
 if err := m.validateSameCurrency(other); err != nil {
  return Money{}, err
 }

 return Money{
  amount:   m.amount + other.amount,
  currency: m.currency,
 }, nil
}

// Subtract returns a new Money with the difference.
//
// Returns error if currencies don't match.
func (m Money) Subtract(other Money) (Money, error) {
 if err := m.validateSameCurrency(other); err != nil {
  return Money{}, err
 }

 return Money{
  amount:   m.amount - other.amount,
  currency: m.currency,
 }, nil
}

// Multiply returns a new Money multiplied by factor.
func (m Money) Multiply(factor int64) Money {
 return Money{
  amount:   m.amount * factor,
  currency: m.currency,
 }
}

// Divide returns a new Money divided by divisor.
//
// Returns error if divisor is zero.
func (m Money) Divide(divisor int64) (Money, error) {
 if divisor == 0 {
  return Money{}, errors.New("division by zero")
 }

 return Money{
  amount:   m.amount / divisor,
  currency: m.currency,
 }, nil
}

// Abs returns the absolute value of this Money.
func (m Money) Abs() Money {
 if m.amount < 0 {
  return Money{amount: -m.amount, currency: m.currency}
 }
 return m
}

// Negate returns the negated value of this Money.
func (m Money) Negate() Money {
 return Money{amount: -m.amount, currency: m.currency}
}

// ========================================
// Comparison Operations
// ========================================

// IsGreaterThan checks if this Money is greater than other.
func (m Money) IsGreaterThan(other Money) (bool, error) {
 if err := m.validateSameCurrency(other); err != nil {
  return false, err
 }
 return m.amount > other.amount, nil
}

// IsGreaterThanOrEqual checks if this Money is greater than or equal to other.
func (m Money) IsGreaterThanOrEqual(other Money) (bool, error) {
 if err := m.validateSameCurrency(other); err != nil {
  return false, err
 }
 return m.amount >= other.amount, nil
}

// IsLessThan checks if this Money is less than other.
func (m Money) IsLessThan(other Money) (bool, error) {
 if err := m.validateSameCurrency(other); err != nil {
  return false, err
 }
 return m.amount < other.amount, nil
}

// IsLessThanOrEqual checks if this Money is less than or equal to other.
func (m Money) IsLessThanOrEqual(other Money) (bool, error) {
 if err := m.validateSameCurrency(other); err != nil {
  return false, err
 }
 return m.amount <= other.amount, nil
}

// IsZero checks if this Money is zero.
func (m Money) IsZero() bool {
 return m.amount == 0
}

// IsPositive checks if this Money is positive.
func (m Money) IsPositive() bool {
 return m.amount > 0
}

// IsNegative checks if this Money is negative.
func (m Money) IsNegative() bool {
 return m.amount < 0
}

// IsNegativeOrZero checks if this Money is negative or zero.
func (m Money) IsNegativeOrZero() bool {
 return m.amount <= 0
}

// ========================================
// Validation
// ========================================

func validateCurrency(code string) error {
 if len(code) != 3 {
  return errors.New("currency code must be 3 characters")
 }

 upper := strings.ToUpper(code)
 if _, ok := currencyDecimalPlaces[upper]; !ok {
  return fmt.Errorf("unsupported currency: %s", code)
 }

 return nil
}

func (m Money) validateSameCurrency(other Money) error {
 if m.currency != other.currency {
  return fmt.Errorf("currency mismatch: %s vs %s", m.currency, other.currency)
 }
 return nil
}

func getCurrencyDecimalPlaces(code string) int {
 upper := strings.ToUpper(code)
 if places, ok := currencyDecimalPlaces[upper]; ok {
  return places
 }
 return 2 // Default to 2 decimal places
}

// ========================================
// Getters
// ========================================

// Amount returns the amount in smallest currency unit (e.g., cents).
func (m Money) Amount() int64 {
 return m.amount
}

// Currency returns the ISO-4217 currency code.
func (m Money) Currency() string {
 return m.currency
}

// AmountAsFloat returns the amount as floating-point (e.g., 100.50).
//
// WARNING: Floating-point arithmetic is imprecise. Use for display only.
func (m Money) AmountAsFloat() float64 {
 decimalPlaces := getCurrencyDecimalPlaces(m.currency)
 divisor := float64(1)
 for i := 0; i < decimalPlaces; i++ {
  divisor *= 10
 }
 return float64(m.amount) / divisor
}

// ========================================
// Equality
// ========================================

// Equals compares this Money with another for value equality.
func (m Money) Equals(other Money) bool {
 return m.amount == other.amount && m.currency == other.currency
}

// ========================================
// String Representation
// ========================================

// String returns a string representation (e.g., "100.50 USD").
func (m Money) String() string {
 decimalPlaces := getCurrencyDecimalPlaces(m.currency)

 if decimalPlaces == 0 {
  return fmt.Sprintf("%d %s", m.amount, m.currency)
 }

 divisor := int64(1)
 for i := 0; i < decimalPlaces; i++ {
  divisor *= 10
 }

 wholePart := m.amount / divisor
 fractionalPart := m.amount % divisor
 if fractionalPart < 0 {
  fractionalPart = -fractionalPart
 }

 formatStr := fmt.Sprintf("%%d.%%0%dd %%s", decimalPlaces)
 return fmt.Sprintf(formatStr, wholePart, fractionalPart, m.currency)
}
```

### Account Number Value Object

```go
package model

import (
 "errors"
 "fmt"
 "regexp"
)

// AccountNumber represents a bank account number.
//
// Invariants:
//   - Must be 10-16 digits
//   - Only numeric characters allowed
//   - Must pass Luhn checksum validation
type AccountNumber struct {
 value string
}

var accountNumberRegex = regexp.MustCompile(`^\d{10,16}$`)

// NewAccountNumber creates a validated AccountNumber.
func NewAccountNumber(value string) (AccountNumber, error) {
 if !accountNumberRegex.MatchString(value) {
  return AccountNumber{}, errors.New("account number must be 10-16 digits")
 }

 if !luhnCheck(value) {
  return AccountNumber{}, errors.New("invalid account number checksum")
 }

 return AccountNumber{value: value}, nil
}

// Value returns the account number as string.
func (a AccountNumber) Value() string {
 return a.value
}

// Masked returns masked account number (e.g., "******1234").
func (a AccountNumber) Masked() string {
 if len(a.value) <= 4 {
  return a.value
 }
 return "******" + a.value[len(a.value)-4:]
}

// Equals compares account numbers for equality.
func (a AccountNumber) Equals(other AccountNumber) bool {
 return a.value == other.value
}

func (a AccountNumber) String() string {
 return a.Masked()
}

// luhnCheck validates checksum using Luhn algorithm.
func luhnCheck(number string) bool {
 sum := 0
 alternate := false

 for i := len(number) - 1; i >= 0; i-- {
  digit := int(number[i] - '0')

  if alternate {
   digit *= 2
   if digit > 9 {
    digit -= 9
   }
  }

  sum += digit
  alternate = !alternate
 }

 return sum%10 == 0
}
```

## Usage Guidelines

### Immutability Checklist

- [ ] All fields are unexported (lowercase first letter)
- [ ] No setter methods (no `SetX()` functions)
- [ ] Constructor validates all invariants
- [ ] All operations return new instances
- [ ] Defensive copying for mutable fields (slices, maps)
- [ ] Value receiver for all methods (not pointer `*`)

### Constructor Patterns

```go
// Pattern 1: Error-returning constructor (preferred)
func NewEmail(value string) (Email, error) {
 if !isValidEmail(value) {
  return Email{}, errors.New("invalid email")
 }
 return Email{value: value}, nil
}

// Pattern 2: Panic constructor (for constants)
func MustNewEmail(value string) Email {
 e, err := NewEmail(value)
 if err != nil {
  panic(err)
 }
 return e
}

// Pattern 3: Zero value with validation method
type Email struct {
 value string
}

func (e Email) Validate() error {
 if !isValidEmail(e.value) {
  return errors.New("invalid email")
 }
 return nil
}
```

### Operation Patterns

```go
// CORRECT: Pure function, returns new instance
func (m Money) Add(other Money) (Money, error) {
 if err := m.validateSameCurrency(other); err != nil {
  return Money{}, err
 }
 return Money{amount: m.amount + other.amount, currency: m.currency}, nil
}

// INCORRECT: Mutates receiver
func (m *Money) MutatingAdd(other Money) error {
 m.amount += other.amount // Don't do this!
 return nil
}
```

### Before/After Comparison

```go
// BEFORE: Mutable, identity-based
type MutableMoney struct {
 Amount   float64 // Exported, can be modified
 Currency string  // Exported, can be modified
}

func (m *MutableMoney) Add(other *MutableMoney) {
 m.Amount += other.Amount // Mutates!
}

// Problems:
// - Can be modified externally: money.Amount = -1000
// - Operations mutate: money.Add(other) changes money
// - Identity-based: money1 != money2 even if same values

// AFTER: Immutable, value-based
type Money struct {
 amount   int64  // Unexported, encapsulated
 currency string // Unexported, encapsulated
}

func (m Money) Add(other Money) (Money, error) {
 if err := m.validateSameCurrency(other); err != nil {
  return Money{}, err
 }
 return Money{amount: m.amount + other.amount, currency: m.currency}, nil
}

// Benefits:
// - Cannot be modified: money.amount not accessible
// - Operations pure: money.Add(other) returns new Money
// - Value-based: money1.Equals(money2) compares values
```

## Testing Value Objects

### Basic Tests

```go
package model_test

import (
 "testing"

 "myapp/model"
)

func TestMoney_NewMoney(t *testing.T) {
 tests := []struct {
  name          string
  amount        int64
  currency      string
  wantErr       bool
  expectedError string
 }{
  {"valid USD", 10000, "USD", false, ""},
  {"valid EUR", 5000, "EUR", false, ""},
  {"invalid currency", 100, "XXX", true, "unsupported currency"},
  {"zero amount", 0, "USD", false, ""},
  {"negative amount", -1000, "USD", false, ""}, // Negative allowed
 }

 for _, tt := range tests {
  t.Run(tt.name, func(t *testing.T) {
   got, err := model.NewMoney(tt.amount, tt.currency)

   if (err != nil) != tt.wantErr {
    t.Errorf("NewMoney() error = %v, wantErr %v", err, tt.wantErr)
    return
   }

   if tt.wantErr && err != nil {
    if !contains(err.Error(), tt.expectedError) {
     t.Errorf("error message = %v, want substring %v", err.Error(), tt.expectedError)
    }
    return
   }

   if got.Amount() != tt.amount {
    t.Errorf("Amount() = %v, want %v", got.Amount(), tt.amount)
   }

   if got.Currency() != tt.currency {
    t.Errorf("Currency() = %v, want %v", got.Currency(), tt.currency)
   }
  })
 }
}

func TestMoney_Add(t *testing.T) {
 tests := []struct {
  name     string
  m1       model.Money
  m2       model.Money
  want     int64
  wantErr  bool
 }{
  {
   "same currency",
   model.MustNewMoney(10000, "USD"),
   model.MustNewMoney(5000, "USD"),
   15000,
   false,
  },
  {
   "different currency",
   model.MustNewMoney(10000, "USD"),
   model.MustNewMoney(5000, "EUR"),
   0,
   true,
  },
  {
   "zero values",
   model.MustNewMoney(0, "USD"),
   model.MustNewMoney(0, "USD"),
   0,
   false,
  },
 }

 for _, tt := range tests {
  t.Run(tt.name, func(t *testing.T) {
   got, err := tt.m1.Add(tt.m2)

   if (err != nil) != tt.wantErr {
    t.Errorf("Add() error = %v, wantErr %v", err, tt.wantErr)
    return
   }

   if !tt.wantErr && got.Amount() != tt.want {
    t.Errorf("Add() = %v, want %v", got.Amount(), tt.want)
   }
  })
 }
}

func TestMoney_Equals(t *testing.T) {
 m1 := model.MustNewMoney(10000, "USD")
 m2 := model.MustNewMoney(10000, "USD")
 m3 := model.MustNewMoney(5000, "USD")
 m4 := model.MustNewMoney(10000, "EUR")

 if !m1.Equals(m2) {
  t.Error("same amount and currency should be equal")
 }

 if m1.Equals(m3) {
  t.Error("different amounts should not be equal")
 }

 if m1.Equals(m4) {
  t.Error("different currencies should not be equal")
 }
}

func TestMoney_Immutability(t *testing.T) {
 original := model.MustNewMoney(10000, "USD")
 other := model.MustNewMoney(5000, "USD")

 // Perform operation
 result, err := original.Add(other)
 if err != nil {
  t.Fatalf("Add() error = %v", err)
 }

 // Original should be unchanged
 if original.Amount() != 10000 {
  t.Errorf("original modified! Amount = %v, want 10000", original.Amount())
 }

 // Result should have sum
 if result.Amount() != 15000 {
  t.Errorf("result = %v, want 15000", result.Amount())
 }
}

func contains(s, substr string) bool {
 return len(s) >= len(substr) && (s == substr || len(s) > len(substr) && (s[:len(substr)] == substr || s[len(s)-len(substr):] == substr || len(s) > 2*len(substr)))
}
```

## Related Documentation

**Go Language Documentation**:

- [Best Practices](../ex-so-stla-go__best-practices.md) - Go coding standards
- [Idioms](../ex-so-stla-go__idioms.md) - Go-specific patterns
- [Interfaces and Composition](../ex-so-stla-go__interfaces-and-composition.md) - Interface design

**DDD Concepts**:

- Entity pattern (different from value objects - has identity)
- Aggregate pattern (combines entities and value objects)
- Repository pattern (persistence)

**Principles**:

- [Simplicity Over Complexity](../../../../../../governance/principles/general/simplicity-over-complexity.md)
- [Explicit Over Implicit](../../../../../../governance/principles/software-engineering/explicit-over-implicit.md)

---

**Principles Applied**: Simplicity Over Complexity, Explicit Over Implicit
