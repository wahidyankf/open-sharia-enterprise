---
title: TypeScript Value Object Template
description: Template for creating Domain-Driven Design value objects in TypeScript with immutability using readonly, Object.freeze, and functional operations
category: template
tags:
  - typescript
  - ddd
  - domain-model
  - value-object
  - immutability
  - readonly
  - ts-5.0
  - ts-5.1
  - ts-5.2
  - ts-5.3
  - ts-5.4
  - ts-5.5
  - ts-5.6
  - ts-5.9
related:
  - entity-template.md
  - aggregate-template.md
  - ex-so-stla-ts__best-practices.md
  - ex-so-stla-ts__type-safety.md
principles:
  - simplicity-over-complexity
  - explicit-over-implicit
---

# TypeScript Value Object Template

This template provides a standardized structure for creating Domain-Driven Design (DDD) value objects in TypeScript. Value objects are immutable, defined by their attributes, and have value-based equality.

## Table of Contents

1. [Overview](#overview)
2. [Template Structure](#template-structure)
3. [Immutability Patterns](#immutability-patterns)
4. [Value Equality](#value-equality)
5. [Factory Functions](#factory-functions)
6. [Operations Returning New Instances](#operations-returning-new-instances)
7. [Validation Strategies](#validation-strategies)
8. [Complete Example: Money Value Object](#complete-example-money-value-object)
9. [Complete Example: Email Value Object](#complete-example-email-value-object)
10. [TypeScript vs Java/Go Comparison](#typescript-vs-javago-comparison)
11. [Testing Value Objects](#testing-value-objects)
12. [Related Documentation](#related-documentation)

## Overview

Value objects in TypeScript use **readonly properties**, **private constructors**, and **factory functions** to ensure immutability and validation. TypeScript's type system provides compile-time safety superior to Java's runtime enforcement and more ergonomic than Go's manual patterns.

**Key Characteristics**:

- **Immutable state**: readonly properties prevent reassignment
- **Value-based equality**: Two objects with same values are equal
- **Self-validating**: Factory functions enforce invariants
- **No identity**: Interchangeable objects with same values
- **Side-effect free**: Operations return new instances
- **Private fields**: Encapsulation through TypeScript private

**TypeScript vs Java/Go Comparison**:

```typescript
// TypeScript: Class with readonly + private
class Money {
  // Readonly prevents reassignment
  private readonly amount: number;
  private readonly currency: string;

  private constructor(amount: number, currency: string) {
    this.amount = amount;
    this.currency = currency;
  }

  static create(amount: number, currency: string): Result<Money, ValidationError> {
    // Validation logic
    return Ok(new Money(amount, currency));
  }

  add(other: Money): Result<Money, DomainError> {
    // Returns NEW Money instance
    return Money.create(this.amount + other.amount, this.currency);
  }

  equals(other: Money): boolean {
    return this.amount === other.amount && this.currency === other.currency;
  }
}
```

```java
// Java: Class with final keyword
public final class Money {
    private final BigDecimal amount; // final = immutable
    private final Currency currency;

    private Money(BigDecimal amount, Currency currency) {
        this.amount = amount;
        this.currency = currency;
    }

    public static Money create(BigDecimal amount, Currency currency) {
        // Validation logic
        return new Money(amount, currency);
    }

    public Money add(Money other) {
        return new Money(this.amount.add(other.amount), this.currency);
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof Money)) return false;
        Money other = (Money) obj;
        return this.amount.equals(other.amount) && this.currency.equals(other.currency);
    }
}
```

```go
// Go: Struct with unexported fields
type Money struct {
 amount   int64  // unexported = encapsulated
 currency string // no readonly keyword, convention only
}

func NewMoney(amount int64, currency string) (Money, error) {
 // Validation logic
 return Money{amount: amount, currency: currency}, nil
}

func (m Money) Add(other Money) (Money, error) {
 // Returns NEW Money (value receiver prevents mutation)
 return NewMoney(m.amount+other.amount, m.currency)
}

func (m Money) Equals(other Money) bool {
 return m.amount == other.amount && m.currency == other.currency
}
```

**Critical Differences**:

- TypeScript uses **readonly** for immutability (compile-time enforced)
- TypeScript uses **private** keyword instead of Go's unexported convention
- TypeScript uses **Result<T, E>** pattern instead of Go's `(value, error)` tuples
- TypeScript has **stricter null handling** with `strictNullChecks`
- TypeScript value objects use **class-based design** (not Go's struct-based approach)

## Template Structure

````typescript
import { Result, Ok, Err } from "./result";
import { ValidationError, DomainError } from "./errors";

// ========================================
// Value Object Class
// ========================================

/**
 * ValueObjectName represents [brief description].
 *
 * Characteristics:
 * - Immutable: Cannot be modified after creation
 * - Value-based equality: Objects with same values are equal
 * - Self-validating: Ensures invariants at construction
 * - Side-effect free: Methods don't modify state
 *
 * Invariants:
 * - [Invariant 1]
 * - [Invariant 2]
 * - [Invariant 3]
 *
 * @example
 * ```typescript
 * const result = ValueObjectName.create("value", 42);
 * if (result.isOk()) {
 *   const vo = result.value;
 *   const updated = vo.withAttribute1("newValue");
 * }
 * ```
 */
export class ValueObjectName {
  // ========================================
  // Private Readonly Fields
  // ========================================

  /**
   * Primary attribute (immutable after construction).
   */
  private readonly attribute1: string;

  /**
   * Secondary attribute (immutable after construction).
   */
  private readonly attribute2: number;

  /**
   * Nested value object (immutable after construction).
   */
  private readonly attribute3: NestedValueObject;

  // ========================================
  // Private Constructor
  // ========================================

  /**
   * Private constructor prevents direct instantiation.
   * Use ValueObjectName.create() instead.
   *
   * @param attribute1 - First attribute
   * @param attribute2 - Second attribute
   * @param attribute3 - Nested value object
   */
  private constructor(attribute1: string, attribute2: number, attribute3: NestedValueObject) {
    this.attribute1 = attribute1;
    this.attribute2 = attribute2;
    this.attribute3 = attribute3;

    // Optional: Deep freeze for runtime immutability guarantee
    Object.freeze(this);
  }

  // ========================================
  // Factory Functions
  // ========================================

  /**
   * Creates a new ValueObjectName with validation.
   *
   * Validates all invariants before creating instance.
   * Returns Result monad for functional error handling.
   *
   * @param attribute1 - First attribute value
   * @param attribute2 - Second attribute value
   * @param attribute3 - Nested value object
   * @returns Result containing value object or validation error
   *
   * @example
   * ```typescript
   * const result = ValueObjectName.create("test", 42, nested);
   * if (result.isErr()) {
   *   console.error(result.error);
   *   return;
   * }
   * const vo = result.value;
   * ```
   */
  static create(
    attribute1: string,
    attribute2: number,
    attribute3: NestedValueObject,
  ): Result<ValueObjectName, ValidationError> {
    // Validate all invariants
    const attr1Validation = this.validateAttribute1(attribute1);
    if (attr1Validation.isErr()) {
      return Err(new ValidationError(`Invalid attribute1: ${attr1Validation.error}`));
    }

    const attr2Validation = this.validateAttribute2(attribute2);
    if (attr2Validation.isErr()) {
      return Err(new ValidationError(`Invalid attribute2: ${attr2Validation.error}`));
    }

    const attr3Validation = attribute3.validate();
    if (attr3Validation.isErr()) {
      return Err(new ValidationError(`Invalid attribute3: ${attr3Validation.error}`));
    }

    return Ok(new ValueObjectName(attribute1, attribute2, attribute3));
  }

  /**
   * Creates ValueObjectName, throwing on invalid input.
   *
   * Use ONLY when input is guaranteed valid (e.g., hard-coded values).
   *
   * @param attribute1 - First attribute value
   * @param attribute2 - Second attribute value
   * @param attribute3 - Nested value object
   * @returns ValueObjectName instance
   * @throws ValidationError if any invariant is violated
   */
  static mustCreate(attribute1: string, attribute2: number, attribute3: NestedValueObject): ValueObjectName {
    const result = this.create(attribute1, attribute2, attribute3);
    if (result.isErr()) {
      throw result.error;
    }
    return result.value;
  }

  /**
   * Parses ValueObjectName from string representation.
   *
   * Expected format: "attr1:attr2:attr3"
   *
   * @param input - String to parse
   * @returns Result containing parsed value object or error
   */
  static parse(input: string): Result<ValueObjectName, ValidationError> {
    if (!input || input.trim().length === 0) {
      return Err(new ValidationError("Input string required"));
    }

    const parts = input.split(":");
    if (parts.length !== 3) {
      return Err(new ValidationError("Invalid format, expected 'attr1:attr2:attr3'"));
    }

    const attr2 = parseInt(parts[1], 10);
    if (isNaN(attr2)) {
      return Err(new ValidationError("Attribute2 must be a number"));
    }

    const nestedResult = NestedValueObject.parse(parts[2]);
    if (nestedResult.isErr()) {
      return Err(new ValidationError(`Invalid nested value object: ${nestedResult.error}`));
    }

    return this.create(parts[0], attr2, nestedResult.value);
  }

  // ========================================
  // Validation (Private Static Methods)
  // ========================================

  private static validateAttribute1(value: string): Result<void, string> {
    if (!value || value.trim().length === 0) {
      return Err("Attribute1 must not be empty");
    }

    if (value.length < 3 || value.length > 50) {
      return Err("Attribute1 must be between 3 and 50 characters");
    }

    return Ok(undefined);
  }

  private static validateAttribute2(value: number): Result<void, string> {
    if (value < 0) {
      return Err("Attribute2 must not be negative");
    }

    if (value > 1000) {
      return Err("Attribute2 must not exceed 1000");
    }

    return Ok(undefined);
  }

  // ========================================
  // Operations (Return New Instances)
  // ========================================

  /**
   * Returns new ValueObjectName with updated attribute1.
   *
   * Original instance is unchanged (immutability).
   *
   * @param newAttr1 - New attribute1 value
   * @returns Result containing new instance or validation error
   */
  withAttribute1(newAttr1: string): Result<ValueObjectName, ValidationError> {
    return ValueObjectName.create(newAttr1, this.attribute2, this.attribute3);
  }

  /**
   * Returns new ValueObjectName with updated attribute2.
   *
   * @param newAttr2 - New attribute2 value
   * @returns Result containing new instance or validation error
   */
  withAttribute2(newAttr2: number): Result<ValueObjectName, ValidationError> {
    return ValueObjectName.create(this.attribute1, newAttr2, this.attribute3);
  }

  /**
   * Performs domain operation returning new instance.
   *
   * @param other - Other value object to combine with
   * @returns Result containing combined instance or domain error
   */
  combine(other: ValueObjectName): Result<ValueObjectName, DomainError> {
    const combined1 = this.attribute1 + other.attribute1;
    const combined2 = this.attribute2 + other.attribute2;

    const nestedResult = this.attribute3.combine(other.attribute3);
    if (nestedResult.isErr()) {
      return Err(new DomainError(`Cannot combine nested values: ${nestedResult.error}`));
    }

    const result = ValueObjectName.create(combined1, combined2, nestedResult.value);
    if (result.isErr()) {
      return Err(new DomainError(`Invalid combined value: ${result.error}`));
    }

    return Ok(result.value);
  }

  // ========================================
  // Query Methods (Getters)
  // ========================================

  /**
   * Returns the first attribute value.
   */
  getAttribute1(): string {
    return this.attribute1;
  }

  /**
   * Returns the second attribute value.
   */
  getAttribute2(): number {
    return this.attribute2;
  }

  /**
   * Returns the nested value object.
   */
  getAttribute3(): NestedValueObject {
    return this.attribute3;
  }

  /**
   * Checks if this value object satisfies a specific condition.
   */
  satisfiesCondition(): boolean {
    return this.attribute2 > 100 && this.attribute1.length > 5;
  }

  /**
   * Validates this value object's current state.
   *
   * Returns Ok if valid, Err with reason if invalid.
   */
  validate(): Result<void, string> {
    const attr1Result = ValueObjectName.validateAttribute1(this.attribute1);
    if (attr1Result.isErr()) {
      return attr1Result;
    }

    const attr2Result = ValueObjectName.validateAttribute2(this.attribute2);
    if (attr2Result.isErr()) {
      return attr2Result;
    }

    return Ok(undefined);
  }

  // ========================================
  // Equality (Value-Based)
  // ========================================

  /**
   * Compares this value object with another for value equality.
   *
   * Two value objects are equal if all their attributes match.
   *
   * @param other - Value object to compare with
   * @returns true if values are equal
   *
   * @example
   * ```typescript
   * const vo1 = ValueObjectName.mustCreate("test", 42, nested);
   * const vo2 = ValueObjectName.mustCreate("test", 42, nested);
   * console.log(vo1.equals(vo2)); // true (same values)
   * console.log(vo1 === vo2); // false (different objects)
   * ```
   */
  equals(other: ValueObjectName): boolean {
    if (!(other instanceof ValueObjectName)) {
      return false;
    }

    return (
      this.attribute1 === other.attribute1 &&
      this.attribute2 === other.attribute2 &&
      this.attribute3.equals(other.attribute3)
    );
  }

  // ========================================
  // String Representation
  // ========================================

  /**
   * Returns string representation of the value object.
   *
   * @returns String in format "attr1:attr2:attr3"
   */
  toString(): string {
    return `${this.attribute1}:${this.attribute2}:${this.attribute3.toString()}`;
  }

  /**
   * Returns JSON representation for serialization.
   *
   * @returns Plain object representation
   */
  toJSON(): ValueObjectNameJSON {
    return {
      attribute1: this.attribute1,
      attribute2: this.attribute2,
      attribute3: this.attribute3.toJSON(),
    };
  }
}

// ========================================
// Supporting Types
// ========================================

/**
 * JSON representation of ValueObjectName.
 */
interface ValueObjectNameJSON {
  attribute1: string;
  attribute2: number;
  attribute3: NestedValueObjectJSON;
}
````

## Immutability Patterns

TypeScript provides multiple mechanisms to enforce immutability:

### Pattern 1: readonly Properties (Compile-Time)

```typescript
// CORRECT: readonly prevents reassignment at compile time
class Email {
  private readonly value: string;

  private constructor(value: string) {
    this.value = value;
  }

  static create(email: string): Result<Email, ValidationError> {
    // Validation
    return Ok(new Email(email));
  }

  getValue(): string {
    return this.value;
  }

  // No setter method!
  // Operations return new instances
  withValue(newEmail: string): Result<Email, ValidationError> {
    return Email.create(newEmail);
  }
}

// INCORRECT: Mutable value object
class MutableEmail {
  private value: string; // NOT readonly!

  setValue(email: string): void {
    this.value = email; // Mutation violates immutability!
  }
}
```

### Pattern 2: Object.freeze (Runtime Enforcement)

```typescript
// CORRECT: Object.freeze prevents runtime mutations
class Money {
  private readonly amount: number;
  private readonly currency: string;

  private constructor(amount: number, currency: string) {
    this.amount = amount;
    this.currency = currency;

    // Runtime immutability guarantee
    Object.freeze(this);
  }

  static create(amount: number, currency: string): Result<Money, ValidationError> {
    // Validation
    return Ok(new Money(amount, currency));
  }
}

// Usage: Attempting mutation throws error in strict mode
const money = Money.mustCreate(100, "USD");
// @ts-expect-error
money.amount = 200; // TypeError: Cannot assign to read only property 'amount'
```

### Pattern 3: Readonly Array/Object Types

```typescript
class Tags {
  // Readonly array prevents reassignment AND mutation
  private readonly values: readonly string[];

  private constructor(values: readonly string[]) {
    // Defensive copy to prevent external mutation
    this.values = Object.freeze([...values]);
  }

  static create(values: string[]): Result<Tags, ValidationError> {
    if (values.length === 0) {
      return Err(new ValidationError("At least one tag required"));
    }
    return Ok(new Tags(values));
  }

  getValues(): readonly string[] {
    // Return readonly reference (no defensive copy needed)
    return this.values;
  }

  addTag(tag: string): Result<Tags, ValidationError> {
    // Returns NEW instance
    return Tags.create([...this.values, tag]);
  }
}
```

## Value Equality

Value objects compare by value, not identity:

### Basic Value Equality

```typescript
class Point {
  private readonly x: number;
  private readonly y: number;

  private constructor(x: number, y: number) {
    this.x = x;
    this.y = y;
  }

  static create(x: number, y: number): Point {
    return new Point(x, y);
  }

  equals(other: Point): boolean {
    if (!(other instanceof Point)) {
      return false;
    }
    return this.x === other.x && this.y === other.y;
  }
}

// Usage
const p1 = Point.create(10, 20);
const p2 = Point.create(10, 20);

console.log(p1 === p2); // false (different object references)
console.log(p1.equals(p2)); // true (same values)
```

### Complex Equality with Nested Values

```typescript
class Address {
  private readonly street: string;
  private readonly city: string;
  private readonly country: string;

  equals(other: Address): boolean {
    return this.street === other.street && this.city === other.city && this.country === other.country;
  }
}

class Person {
  private readonly name: string;
  private readonly age: number;
  private readonly address: Address;

  equals(other: Person): boolean {
    return (
      this.name === other.name && this.age === other.age && this.address.equals(other.address) // Delegate to nested equals
    );
  }
}
```

### Equality with Collections

```typescript
class Permissions {
  private readonly roles: readonly string[];

  equals(other: Permissions): boolean {
    if (!(other instanceof Permissions)) {
      return false;
    }

    if (this.roles.length !== other.roles.length) {
      return false;
    }

    // Compare array contents
    return this.roles.every((role, index) => role === other.roles[index]);
  }
}
```

## Factory Functions

TypeScript value objects use static factory methods with private constructors:

### Basic Factory Pattern

```typescript
class Email {
  private readonly value: string;

  private constructor(value: string) {
    this.value = value;
  }

  // Pattern 1: Result-returning factory (preferred)
  static create(email: string): Result<Email, ValidationError> {
    if (!this.isValidEmail(email)) {
      return Err(new ValidationError("Invalid email format"));
    }
    return Ok(new Email(email.toLowerCase()));
  }

  // Pattern 2: Throwing factory (for guaranteed valid input)
  static mustCreate(email: string): Email {
    const result = this.create(email);
    if (result.isErr()) {
      throw result.error;
    }
    return result.value;
  }

  private static isValidEmail(email: string): boolean {
    const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
    return emailRegex.test(email);
  }
}

// Usage with Result
const emailResult = Email.create("user@example.com");
if (emailResult.isErr()) {
  console.error(emailResult.error);
  return;
}
const email = emailResult.value;

// Usage with throw (constants only)
const adminEmail = Email.mustCreate("admin@example.com");
```

### Multi-Step Construction

```typescript
class PhoneNumber {
  private readonly countryCode: string;
  private readonly number: string;

  private constructor(countryCode: string, number: string) {
    this.countryCode = countryCode;
    this.number = number;
  }

  static create(countryCode: string, number: string): Result<PhoneNumber, ValidationError> {
    // Normalize inputs
    const normalizedCode = countryCode.replace(/[^\d+]/g, "");
    const normalizedNumber = number.replace(/[^\d]/g, "");

    // Validate country code
    if (!normalizedCode.startsWith("+")) {
      return Err(new ValidationError("Country code must start with +"));
    }

    // Validate number length
    if (normalizedNumber.length < 7 || normalizedNumber.length > 15) {
      return Err(new ValidationError("Number must be 7-15 digits"));
    }

    return Ok(new PhoneNumber(normalizedCode, normalizedNumber));
  }

  static parse(input: string): Result<PhoneNumber, ValidationError> {
    // Expected format: "+1-555-123-4567" or "+15551234567"
    const cleaned = input.replace(/[^\d+]/g, "");

    if (!cleaned.startsWith("+")) {
      return Err(new ValidationError("Must include country code"));
    }

    // Split country code and number
    const match = cleaned.match(/^(\+\d{1,3})(\d{7,15})$/);
    if (!match) {
      return Err(new ValidationError("Invalid phone number format"));
    }

    return this.create(match[1], match[2]);
  }
}
```

## Operations Returning New Instances

All operations return new instances, preserving immutability:

### Arithmetic Operations

```typescript
class Quantity {
  private readonly value: number;

  private constructor(value: number) {
    this.value = value;
  }

  static create(value: number): Result<Quantity, ValidationError> {
    if (value < 0) {
      return Err(new ValidationError("Quantity must not be negative"));
    }
    return Ok(new Quantity(value));
  }

  add(other: Quantity): Result<Quantity, DomainError> {
    return Quantity.create(this.value + other.value).mapErr(
      (err) => new DomainError(`Cannot add quantities: ${err.message}`),
    );
  }

  subtract(other: Quantity): Result<Quantity, DomainError> {
    return Quantity.create(this.value - other.value).mapErr(
      (err) => new DomainError(`Cannot subtract quantities: ${err.message}`),
    );
  }

  multiply(factor: number): Result<Quantity, DomainError> {
    return Quantity.create(this.value * factor).mapErr(
      (err) => new DomainError(`Cannot multiply quantity: ${err.message}`),
    );
  }
}
```

### Transformation Operations

```typescript
class Name {
  private readonly value: string;

  private constructor(value: string) {
    this.value = value;
  }

  static create(name: string): Result<Name, ValidationError> {
    const trimmed = name.trim();
    if (trimmed.length === 0) {
      return Err(new ValidationError("Name required"));
    }
    return Ok(new Name(trimmed));
  }

  toUpperCase(): Name {
    return new Name(this.value.toUpperCase());
  }

  toLowerCase(): Name {
    return new Name(this.value.toLowerCase());
  }

  append(suffix: string): Result<Name, ValidationError> {
    return Name.create(this.value + suffix);
  }

  prepend(prefix: string): Result<Name, ValidationError> {
    return Name.create(prefix + this.value);
  }
}

// Usage: Method chaining
const name = Name.mustCreate("john");
const result = name.toUpperCase().append(" DOE");
// Each operation returns new instance
```

## Validation Strategies

### Strategy 1: Constructor Validation (Preferred)

```typescript
class ZakatNisab {
  private readonly amountInGoldGrams: number;

  private constructor(amountInGoldGrams: number) {
    this.amountInGoldGrams = amountInGoldGrams;
  }

  static create(amountInGoldGrams: number): Result<ZakatNisab, ValidationError> {
    if (amountInGoldGrams < 0) {
      return Err(new ValidationError("Nisab amount must not be negative"));
    }

    // Islamic nisab: 85 grams of gold
    const MIN_NISAB_GRAMS = 85;
    if (amountInGoldGrams < MIN_NISAB_GRAMS) {
      return Err(new ValidationError(`Nisab must be at least ${MIN_NISAB_GRAMS} grams of gold`));
    }

    return Ok(new ZakatNisab(amountInGoldGrams));
  }

  getAmountInGoldGrams(): number {
    return this.amountInGoldGrams;
  }

  meetsNisab(wealth: Money): boolean {
    // Complex business logic for nisab calculation
    return wealth.convertToGoldEquivalent() >= this.amountInGoldGrams;
  }
}
```

### Strategy 2: Multi-Field Validation

```typescript
class DateRange {
  private readonly start: Date;
  private readonly end: Date;

  private constructor(start: Date, end: Date) {
    this.start = start;
    this.end = end;
  }

  static create(start: Date, end: Date): Result<DateRange, ValidationError> {
    // Validate individual fields
    if (!(start instanceof Date) || isNaN(start.getTime())) {
      return Err(new ValidationError("Start date must be valid"));
    }

    if (!(end instanceof Date) || isNaN(end.getTime())) {
      return Err(new ValidationError("End date must be valid"));
    }

    // Validate relationship between fields
    if (end < start) {
      return Err(new ValidationError("End date must be after start date"));
    }

    return Ok(new DateRange(start, end));
  }

  getStart(): Date {
    return new Date(this.start); // Defensive copy
  }

  getEnd(): Date {
    return new Date(this.end); // Defensive copy
  }

  getDurationInDays(): number {
    const diffMs = this.end.getTime() - this.start.getTime();
    return Math.floor(diffMs / (1000 * 60 * 60 * 24));
  }
}
```

## Complete Example: Money Value Object

Complete implementation with precise arithmetic and currency handling:

````typescript
import { Result, Ok, Err } from "../shared/result";
import { ValidationError, DomainError } from "../shared/errors";

// ========================================
// Money Value Object
// ========================================

/**
 * Money represents an amount of money in a specific currency.
 *
 * Characteristics:
 * - Immutable: Cannot be modified after creation
 * - Value-based equality: Two Money instances with same amount and currency are equal
 * - Precise decimal arithmetic using number (cents/smallest unit)
 * - Currency-aware operations prevent mixing currencies
 *
 * Invariants:
 * - Amount stored in smallest currency unit (cents)
 * - Currency must be valid ISO-4217 code
 * - Operations maintain precision
 *
 * @example
 * ```typescript
 * const usd = Money.create(10000, "USD"); // $100.00
 * const doubled = usd.value.multiply(2);
 * const total = usd.value.add(Money.create(5000, "USD").value);
 * ```
 */
export class Money {
  private readonly amount: number; // Amount in smallest unit (cents)
  private readonly currency: string; // ISO-4217 code

  // Common currencies with decimal places
  private static readonly CURRENCY_DECIMAL_PLACES: Record<string, number> = {
    USD: 2, // US Dollar
    EUR: 2, // Euro
    GBP: 2, // British Pound
    JPY: 0, // Japanese Yen (no decimals)
    IDR: 2, // Indonesian Rupiah
  };

  private constructor(amount: number, currency: string) {
    this.amount = amount;
    this.currency = currency;
    Object.freeze(this);
  }

  // ========================================
  // Factory Functions
  // ========================================

  static create(amount: number, currencyCode: string): Result<Money, ValidationError> {
    const validation = this.validateCurrency(currencyCode);
    if (validation.isErr()) {
      return Err(new ValidationError(`Invalid currency: ${validation.error}`));
    }

    return Ok(new Money(amount, currencyCode.toUpperCase()));
  }

  static createFromFloat(amount: number, currencyCode: string): Result<Money, ValidationError> {
    const validation = this.validateCurrency(currencyCode);
    if (validation.isErr()) {
      return Err(new ValidationError(`Invalid currency: ${validation.error}`));
    }

    const decimalPlaces = this.getCurrencyDecimalPlaces(currencyCode);
    const multiplier = Math.pow(10, decimalPlaces);
    const cents = Math.round(amount * multiplier);

    return Ok(new Money(cents, currencyCode.toUpperCase()));
  }

  static zero(currencyCode: string): Result<Money, ValidationError> {
    return this.create(0, currencyCode);
  }

  static mustZero(currencyCode: string): Money {
    const result = this.zero(currencyCode);
    if (result.isErr()) {
      throw result.error;
    }
    return result.value;
  }

  static parse(input: string): Result<Money, ValidationError> {
    // Expected format: "100.50 USD"
    const parts = input.trim().split(/\s+/);
    if (parts.length !== 2) {
      return Err(new ValidationError("Invalid format, expected 'AMOUNT CURRENCY'"));
    }

    const amount = parseFloat(parts[0]);
    if (isNaN(amount)) {
      return Err(new ValidationError("Invalid amount"));
    }

    return this.createFromFloat(amount, parts[1]);
  }

  // ========================================
  // Arithmetic Operations
  // ========================================

  add(other: Money): Result<Money, DomainError> {
    const currencyCheck = this.validateSameCurrency(other);
    if (currencyCheck.isErr()) {
      return Err(currencyCheck.error);
    }

    return Ok(new Money(this.amount + other.amount, this.currency));
  }

  subtract(other: Money): Result<Money, DomainError> {
    const currencyCheck = this.validateSameCurrency(other);
    if (currencyCheck.isErr()) {
      return Err(currencyCheck.error);
    }

    return Ok(new Money(this.amount - other.amount, this.currency));
  }

  multiply(factor: number): Money {
    return new Money(Math.round(this.amount * factor), this.currency);
  }

  divide(divisor: number): Result<Money, DomainError> {
    if (divisor === 0) {
      return Err(new DomainError("Division by zero"));
    }

    return Ok(new Money(Math.round(this.amount / divisor), this.currency));
  }

  abs(): Money {
    return new Money(Math.abs(this.amount), this.currency);
  }

  negate(): Money {
    return new Money(-this.amount, this.currency);
  }

  // ========================================
  // Comparison Operations
  // ========================================

  isGreaterThan(other: Money): Result<boolean, DomainError> {
    const currencyCheck = this.validateSameCurrency(other);
    if (currencyCheck.isErr()) {
      return Err(currencyCheck.error);
    }
    return Ok(this.amount > other.amount);
  }

  isGreaterThanOrEqual(other: Money): Result<boolean, DomainError> {
    const currencyCheck = this.validateSameCurrency(other);
    if (currencyCheck.isErr()) {
      return Err(currencyCheck.error);
    }
    return Ok(this.amount >= other.amount);
  }

  isLessThan(other: Money): Result<boolean, DomainError> {
    const currencyCheck = this.validateSameCurrency(other);
    if (currencyCheck.isErr()) {
      return Err(currencyCheck.error);
    }
    return Ok(this.amount < other.amount);
  }

  isLessThanOrEqual(other: Money): Result<boolean, DomainError> {
    const currencyCheck = this.validateSameCurrency(other);
    if (currencyCheck.isErr()) {
      return Err(currencyCheck.error);
    }
    return Ok(this.amount <= other.amount);
  }

  isZero(): boolean {
    return this.amount === 0;
  }

  isPositive(): boolean {
    return this.amount > 0;
  }

  isNegative(): boolean {
    return this.amount < 0;
  }

  isNegativeOrZero(): boolean {
    return this.amount <= 0;
  }

  // ========================================
  // Validation
  // ========================================

  private static validateCurrency(code: string): Result<void, string> {
    if (!code || code.length !== 3) {
      return Err("Currency code must be 3 characters");
    }

    const upper = code.toUpperCase();
    if (!(upper in this.CURRENCY_DECIMAL_PLACES)) {
      return Err(`Unsupported currency: ${code}`);
    }

    return Ok(undefined);
  }

  private validateSameCurrency(other: Money): Result<void, DomainError> {
    if (this.currency !== other.currency) {
      return Err(new DomainError(`Currency mismatch: ${this.currency} vs ${other.currency}`));
    }
    return Ok(undefined);
  }

  private static getCurrencyDecimalPlaces(code: string): number {
    const upper = code.toUpperCase();
    return this.CURRENCY_DECIMAL_PLACES[upper] ?? 2;
  }

  // ========================================
  // Getters
  // ========================================

  getAmount(): number {
    return this.amount;
  }

  getCurrency(): string {
    return this.currency;
  }

  getAmountAsFloat(): number {
    const decimalPlaces = Money.getCurrencyDecimalPlaces(this.currency);
    const divisor = Math.pow(10, decimalPlaces);
    return this.amount / divisor;
  }

  // ========================================
  // Equality
  // ========================================

  equals(other: Money): boolean {
    if (!(other instanceof Money)) {
      return false;
    }
    return this.amount === other.amount && this.currency === other.currency;
  }

  // ========================================
  // String Representation
  // ========================================

  toString(): string {
    const decimalPlaces = Money.getCurrencyDecimalPlaces(this.currency);

    if (decimalPlaces === 0) {
      return `${this.amount} ${this.currency}`;
    }

    const divisor = Math.pow(10, decimalPlaces);
    const wholePart = Math.floor(this.amount / divisor);
    const fractionalPart = Math.abs(this.amount % divisor);

    const paddedFractional = fractionalPart.toString().padStart(decimalPlaces, "0");
    return `${wholePart}.${paddedFractional} ${this.currency}`;
  }

  toJSON(): MoneyJSON {
    return {
      amount: this.amount,
      currency: this.currency,
    };
  }
}

// ========================================
// Supporting Types
// ========================================

interface MoneyJSON {
  amount: number;
  currency: string;
}
````

## Complete Example: Email Value Object

````typescript
import { Result, Ok, Err } from "../shared/result";
import { ValidationError } from "../shared/errors";

/**
 * Email represents a validated email address.
 *
 * Characteristics:
 * - Immutable: Cannot be modified after creation
 * - Self-validating: Ensures valid email format
 * - Normalized: Stored in lowercase
 *
 * Invariants:
 * - Must match email format (RFC 5322 simplified)
 * - Maximum 254 characters
 * - Normalized to lowercase
 *
 * @example
 * ```typescript
 * const result = Email.create("user@example.com");
 * if (result.isOk()) {
 *   const email = result.value;
 *   console.log(email.getValue()); // "user@example.com"
 * }
 * ```
 */
export class Email {
  private readonly value: string;

  // Simplified RFC 5322 email regex
  private static readonly EMAIL_REGEX = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
  private static readonly MAX_LENGTH = 254;

  private constructor(value: string) {
    this.value = value;
    Object.freeze(this);
  }

  static create(email: string): Result<Email, ValidationError> {
    if (!email || email.trim().length === 0) {
      return Err(new ValidationError("Email required"));
    }

    const trimmed = email.trim();

    if (trimmed.length > this.MAX_LENGTH) {
      return Err(new ValidationError(`Email must not exceed ${this.MAX_LENGTH} characters`));
    }

    if (!this.EMAIL_REGEX.test(trimmed)) {
      return Err(new ValidationError("Invalid email format"));
    }

    // Normalize to lowercase
    const normalized = trimmed.toLowerCase();

    return Ok(new Email(normalized));
  }

  static mustCreate(email: string): Email {
    const result = this.create(email);
    if (result.isErr()) {
      throw result.error;
    }
    return result.value;
  }

  getValue(): string {
    return this.value;
  }

  getDomain(): string {
    const atIndex = this.value.indexOf("@");
    return this.value.substring(atIndex + 1);
  }

  getLocalPart(): string {
    const atIndex = this.value.indexOf("@");
    return this.value.substring(0, atIndex);
  }

  isSameDomain(other: Email): boolean {
    return this.getDomain() === other.getDomain();
  }

  equals(other: Email): boolean {
    if (!(other instanceof Email)) {
      return false;
    }
    return this.value === other.value;
  }

  toString(): string {
    return this.value;
  }

  toJSON(): string {
    return this.value;
  }
}
````

## TypeScript vs Java/Go Comparison

### Key Differences Summary

| Aspect               | TypeScript                          | Java                                   | Go                                  |
| -------------------- | ----------------------------------- | -------------------------------------- | ----------------------------------- |
| **Immutability**     | `readonly` keyword                  | `final` keyword                        | Convention (no keyword)             |
| **Encapsulation**    | `private` keyword                   | `private` modifier                     | Unexported fields (lowercase)       |
| **Error Handling**   | `Result<T, E>` monad                | Exceptions                             | `(value, error)` tuples             |
| **Factory**          | Static method + private constructor | Static method or builder               | Function returning `(value, error)` |
| **Null Safety**      | `undefined`, strict null checks     | `null`, `Optional<T>`                  | `nil` pointers                      |
| **Collections**      | Arrays, `readonly` arrays           | `List<T>`, `Collections.unmodifiable*` | Slices (convention for immutability |
| **Defensive Copies** | Spread operator `[...arr]`          | `.clone()`, copy constructors          | Manual slicing `arr[:]`             |

### Side-by-Side Implementation

```typescript
// TypeScript: Class-based with readonly
class ZakatNisab {
  private readonly goldGrams: number;

  private constructor(goldGrams: number) {
    this.goldGrams = goldGrams;
    Object.freeze(this);
  }

  static create(goldGrams: number): Result<ZakatNisab, ValidationError> {
    if (goldGrams < 85) {
      return Err(new ValidationError("Nisab must be at least 85 grams"));
    }
    return Ok(new ZakatNisab(goldGrams));
  }

  meetsNisab(wealth: Money): boolean {
    return wealth.convertToGold() >= this.goldGrams;
  }

  equals(other: ZakatNisab): boolean {
    return this.goldGrams === other.goldGrams;
  }
}
```

```java
// Java: Class with final fields
public final class ZakatNisab {
    private final BigDecimal goldGrams;

    private ZakatNisab(BigDecimal goldGrams) {
        this.goldGrams = goldGrams;
    }

    public static ZakatNisab create(BigDecimal goldGrams) {
        if (goldGrams.compareTo(new BigDecimal("85")) < 0) {
            throw new IllegalArgumentException("Nisab must be at least 85 grams");
        }
        return new ZakatNisab(goldGrams);
    }

    public boolean meetsNisab(Money wealth) {
        return wealth.convertToGold().compareTo(goldGrams) >= 0;
    }

    @Override
    public boolean equals(Object obj) {
        if (!(obj instanceof ZakatNisab)) return false;
        ZakatNisab other = (ZakatNisab) obj;
        return goldGrams.equals(other.goldGrams);
    }
}
```

```go
// Go: Struct with unexported fields
type ZakatNisab struct {
 goldGrams float64 // unexported, immutable by convention
}

func NewZakatNisab(goldGrams float64) (ZakatNisab, error) {
 if goldGrams < 85 {
  return ZakatNisab{}, errors.New("nisab must be at least 85 grams")
 }
 return ZakatNisab{goldGrams: goldGrams}, nil
}

func (z ZakatNisab) MeetsNisab(wealth Money) bool {
 return wealth.ConvertToGold() >= z.goldGrams
}

func (z ZakatNisab) Equals(other ZakatNisab) bool {
 return z.goldGrams == other.goldGrams
}
```

## Testing Value Objects

### Basic Value Object Tests

```typescript
import { describe, it, expect } from "vitest";
import { Money } from "./money";

describe("Money Value Object", () => {
  describe("create", () => {
    it("should create valid money", () => {
      const result = Money.create(10000, "USD");

      expect(result.isOk()).toBe(true);
      expect(result.value.getAmount()).toBe(10000);
      expect(result.value.getCurrency()).toBe("USD");
    });

    it("should reject invalid currency", () => {
      const result = Money.create(10000, "XXX");

      expect(result.isErr()).toBe(true);
      expect(result.error.message).toContain("Unsupported currency");
    });

    it("should normalize currency to uppercase", () => {
      const result = Money.create(10000, "usd");

      expect(result.isOk()).toBe(true);
      expect(result.value.getCurrency()).toBe("USD");
    });
  });

  describe("add", () => {
    it("should add money with same currency", () => {
      const m1 = Money.create(10000, "USD").value;
      const m2 = Money.create(5000, "USD").value;

      const result = m1.add(m2);

      expect(result.isOk()).toBe(true);
      expect(result.value.getAmount()).toBe(15000);
    });

    it("should reject adding different currencies", () => {
      const m1 = Money.create(10000, "USD").value;
      const m2 = Money.create(5000, "EUR").value;

      const result = m1.add(m2);

      expect(result.isErr()).toBe(true);
      expect(result.error.message).toContain("Currency mismatch");
    });

    it("should handle negative amounts", () => {
      const m1 = Money.create(-10000, "USD").value;
      const m2 = Money.create(5000, "USD").value;

      const result = m1.add(m2);

      expect(result.isOk()).toBe(true);
      expect(result.value.getAmount()).toBe(-5000);
    });
  });

  describe("equals", () => {
    it("should be equal with same values", () => {
      const m1 = Money.create(10000, "USD").value;
      const m2 = Money.create(10000, "USD").value;

      expect(m1.equals(m2)).toBe(true);
    });

    it("should not be equal with different amounts", () => {
      const m1 = Money.create(10000, "USD").value;
      const m2 = Money.create(5000, "USD").value;

      expect(m1.equals(m2)).toBe(false);
    });

    it("should not be equal with different currencies", () => {
      const m1 = Money.create(10000, "USD").value;
      const m2 = Money.create(10000, "EUR").value;

      expect(m1.equals(m2)).toBe(false);
    });
  });

  describe("immutability", () => {
    it("should not mutate original on operations", () => {
      const original = Money.create(10000, "USD").value;
      const other = Money.create(5000, "USD").value;

      const result = original.add(other);

      // Original unchanged
      expect(original.getAmount()).toBe(10000);

      // Result has sum
      expect(result.value.getAmount()).toBe(15000);
    });

    it("should prevent mutation via Object.freeze", () => {
      const money = Money.create(10000, "USD").value;

      // TypeScript compiler prevents this at compile time
      // Runtime also prevents it via Object.freeze
      expect(() => {
        // @ts-expect-error - Testing runtime immutability
        money.amount = 20000;
      }).toThrow();
    });
  });

  describe("toString", () => {
    it("should format USD with 2 decimals", () => {
      const money = Money.create(10050, "USD").value;

      expect(money.toString()).toBe("100.50 USD");
    });

    it("should format JPY with 0 decimals", () => {
      const money = Money.create(1000, "JPY").value;

      expect(money.toString()).toBe("1000 JPY");
    });

    it("should handle negative amounts", () => {
      const money = Money.create(-10050, "USD").value;

      expect(money.toString()).toBe("-100.50 USD");
    });
  });
});
```

### Email Value Object Tests

```typescript
import { describe, it, expect } from "vitest";
import { Email } from "./email";

describe("Email Value Object", () => {
  describe("create", () => {
    it("should create valid email", () => {
      const result = Email.create("user@example.com");

      expect(result.isOk()).toBe(true);
      expect(result.value.getValue()).toBe("user@example.com");
    });

    it("should normalize to lowercase", () => {
      const result = Email.create("USER@EXAMPLE.COM");

      expect(result.isOk()).toBe(true);
      expect(result.value.getValue()).toBe("user@example.com");
    });

    it("should reject empty email", () => {
      const result = Email.create("");

      expect(result.isErr()).toBe(true);
      expect(result.error.message).toContain("required");
    });

    it("should reject invalid format", () => {
      const testCases = ["notanemail", "@example.com", "user@", "user @example.com"];

      testCases.forEach((invalid) => {
        const result = Email.create(invalid);
        expect(result.isErr()).toBe(true);
      });
    });

    it("should reject email exceeding max length", () => {
      const longEmail = "a".repeat(250) + "@example.com";
      const result = Email.create(longEmail);

      expect(result.isErr()).toBe(true);
      expect(result.error.message).toContain("must not exceed");
    });
  });

  describe("getDomain", () => {
    it("should extract domain", () => {
      const email = Email.mustCreate("user@example.com");

      expect(email.getDomain()).toBe("example.com");
    });
  });

  describe("getLocalPart", () => {
    it("should extract local part", () => {
      const email = Email.mustCreate("user@example.com");

      expect(email.getLocalPart()).toBe("user");
    });
  });

  describe("isSameDomain", () => {
    it("should return true for same domain", () => {
      const email1 = Email.mustCreate("user1@example.com");
      const email2 = Email.mustCreate("user2@example.com");

      expect(email1.isSameDomain(email2)).toBe(true);
    });

    it("should return false for different domains", () => {
      const email1 = Email.mustCreate("user@example.com");
      const email2 = Email.mustCreate("user@other.com");

      expect(email1.isSameDomain(email2)).toBe(false);
    });
  });

  describe("equals", () => {
    it("should be equal with same normalized value", () => {
      const email1 = Email.mustCreate("USER@example.com");
      const email2 = Email.mustCreate("user@EXAMPLE.com");

      expect(email1.equals(email2)).toBe(true);
    });

    it("should not be equal with different values", () => {
      const email1 = Email.mustCreate("user1@example.com");
      const email2 = Email.mustCreate("user2@example.com");

      expect(email1.equals(email2)).toBe(false);
    });
  });

  describe("immutability", () => {
    it("should prevent mutation via Object.freeze", () => {
      const email = Email.mustCreate("user@example.com");

      expect(() => {
        // @ts-expect-error - Testing runtime immutability
        email.value = "hacker@evil.com";
      }).toThrow();
    });
  });
});
```

## Related Documentation

**TypeScript Language Documentation**:

- [TypeScript Best Practices](../ex-so-stla-ts__best-practices.md) - TypeScript coding standards
- [Type Safety in TypeScript](../ex-so-stla-ts__type-safety.md) - Advanced type patterns
- [Error Handling with Result](../ex-so-stla-ts__error-handling.md) - Result monad pattern

**Template Documentation**:

- [Entity Template](./entity-template.md) - Entities with identity and lifecycle
- [Aggregate Template](./aggregate-template.md) - Aggregate root pattern

**DDD Concepts**:

- Entity pattern (identity-based, mutable)
- Aggregate pattern (transactional boundaries)
- Repository pattern (persistence)

**Principles**:

- [Simplicity Over Complexity](../../../../../../governance/principles/general/simplicity-over-complexity.md)
- [Explicit Over Implicit](../../../../../../governance/principles/software-engineering/explicit-over-implicit.md)

---

**Principles Applied**: Simplicity Over Complexity, Explicit Over Implicit

---

**Last Updated**: 2026-01-24
**TypeScript Version**: 5.0+
