---
title: Java Value Object Template
description: Template for creating Domain-Driven Design value objects in Java with immutability and value-based equality
category: template
tags:
  - java
  - ddd
  - domain-model
  - value-object
  - immutability
related:
  - ex-so-stla-ja-te__entity-template.md
principles:
  - simplicity-over-complexity
  - explicit-over-implicit
last_updated: 2026-01-21
---

# Java Value Object Template

This template provides a standardized structure for creating Domain-Driven Design (DDD) value objects in Java. Value objects are immutable, defined by their attributes, and have value-based equality.

## Template Structure

```java
package com.openshariaenterprise.{domain}.model;

import java.util.Objects;

/**
 * {ValueObjectName} represents {brief description of what this value object models}.
 *
 * <p>Characteristics:
 * <ul>
 *   <li>Immutable - Cannot be modified after creation</li>
 *   <li>Value-based equality - Two instances with same values are equal</li>
 *   <li>Self-validating - Ensures invariants at construction</li>
 *   <li>Side-effect free - Methods don't modify state</li>
 * </ul>
 *
 * <p>Invariants:
 * <ul>
 *   <li>{Invariant 1}</li>
 *   <li>{Invariant 2}</li>
 *   <li>{Invariant 3}</li>
 * </ul>
 *
 * @see RelatedValueObject
 * @see RelatedEntity
 */
public final class ValueObjectName {

    // ========================================
    // Attributes (All final for immutability)
    // ========================================

    private final String attribute1;
    private final int attribute2;
    private final NestedValueObject attribute3;

    // ========================================
    // Constructor (Private - Use Factory Methods)
    // ========================================

    private ValueObjectName(String attribute1, int attribute2, NestedValueObject attribute3) {
        // Validate all invariants
        validateAttribute1(attribute1);
        validateAttribute2(attribute2);
        Objects.requireNonNull(attribute3, "Attribute3 must not be null");

        // Assign (no defensive copies needed if inputs are immutable)
        this.attribute1 = attribute1;
        this.attribute2 = attribute2;
        this.attribute3 = attribute3;
    }

    // ========================================
    // Factory Methods
    // ========================================

    /**
     * Creates a new {ValueObjectName} with the given attributes.
     *
     * @param attribute1 the first attribute
     * @param attribute2 the second attribute
     * @param attribute3 the third attribute
     * @return newly created value object
     * @throws IllegalArgumentException if any invariant is violated
     */
    public static ValueObjectName of(
        String attribute1,
        int attribute2,
        NestedValueObject attribute3
    ) {
        return new ValueObjectName(attribute1, attribute2, attribute3);
    }

    /**
     * Creates a {ValueObjectName} from a string representation.
     *
     * @param representation string representation (e.g., "attr1:attr2:attr3")
     * @return parsed value object
     * @throws IllegalArgumentException if format is invalid
     */
    public static ValueObjectName parse(String representation) {
        Objects.requireNonNull(representation, "Representation must not be null");

        var parts = representation.split(":");
        if (parts.length != 3) {
            throw new IllegalArgumentException("Invalid format");
        }

        return new ValueObjectName(
            parts[0],
            Integer.parseInt(parts[1]),
            NestedValueObject.parse(parts[2])
        );
    }

    // ========================================
    // Business Methods (All return new instances)
    // ========================================

    /**
     * Returns a new instance with the specified attribute1.
     *
     * @param newAttribute1 the new value for attribute1
     * @return new value object with updated attribute1
     * @throws IllegalArgumentException if attribute1 is invalid
     */
    public ValueObjectName withAttribute1(String newAttribute1) {
        return new ValueObjectName(newAttribute1, this.attribute2, this.attribute3);
    }

    /**
     * Returns a new instance with the specified attribute2.
     *
     * @param newAttribute2 the new value for attribute2
     * @return new value object with updated attribute2
     * @throws IllegalArgumentException if attribute2 is invalid
     */
    public ValueObjectName withAttribute2(int newAttribute2) {
        return new ValueObjectName(this.attribute1, newAttribute2, this.attribute3);
    }

    /**
     * Performs a domain operation on this value object.
     *
     * @param operand the operand for the operation
     * @return result of the operation as a new value object
     */
    public ValueObjectName performOperation(ValueObjectName operand) {
        Objects.requireNonNull(operand, "Operand must not be null");

        // Business logic that produces new value object
        var result1 = this.attribute1 + operand.attribute1;
        var result2 = this.attribute2 + operand.attribute2;
        var result3 = this.attribute3.combine(operand.attribute3);

        return new ValueObjectName(result1, result2, result3);
    }

    // ========================================
    // Validation (Private)
    // ========================================

    private static void validateAttribute1(String attribute1) {
        if (attribute1 == null || attribute1.isBlank()) {
            throw new IllegalArgumentException("Attribute1 must not be blank");
        }

        if (attribute1.length() < 3 || attribute1.length() > 50) {
            throw new IllegalArgumentException(
                "Attribute1 must be between 3 and 50 characters"
            );
        }
    }

    private static void validateAttribute2(int attribute2) {
        if (attribute2 < 0) {
            throw new IllegalArgumentException("Attribute2 must not be negative");
        }

        if (attribute2 > 1000) {
            throw new IllegalArgumentException("Attribute2 must not exceed 1000");
        }
    }

    // ========================================
    // Query Methods
    // ========================================

    public String getAttribute1() {
        return attribute1;
    }

    public int getAttribute2() {
        return attribute2;
    }

    public NestedValueObject getAttribute3() {
        return attribute3;
    }

    /**
     * Checks if this value object satisfies a specific condition.
     *
     * @return true if condition is met
     */
    public boolean satisfiesCondition() {
        return attribute2 > 100 && attribute1.startsWith("valid");
    }

    // ========================================
    // Equality & Hash Code (Value-based)
    // ========================================

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        ValueObjectName that = (ValueObjectName) o;
        return attribute2 == that.attribute2 &&
               Objects.equals(attribute1, that.attribute1) &&
               Objects.equals(attribute3, that.attribute3);
    }

    @Override
    public int hashCode() {
        return Objects.hash(attribute1, attribute2, attribute3);
    }

    @Override
    public String toString() {
        return String.format("%s:%d:%s", attribute1, attribute2, attribute3);
    }
}
```

## Financial Domain Example: Money Value Object

```java
package com.openshariaenterprise.common.model;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Currency;
import java.util.Objects;

/**
 * Money represents an amount of money in a specific currency.
 *
 * <p>Characteristics:
 * <ul>
 *   <li>Immutable - Cannot be modified after creation</li>
 *   <li>Value-based equality - Two Money instances with same amount and currency are equal</li>
 *   <li>Precise decimal arithmetic using BigDecimal</li>
 *   <li>Currency-aware operations</li>
 * </ul>
 *
 * <p>Invariants:
 * <ul>
 *   <li>Amount must not be null</li>
 *   <li>Currency must be valid ISO-4217 code</li>
 *   <li>Amount scale must match currency's fraction digits</li>
 * </ul>
 *
 * <p>Usage Examples:
 * <pre>
 * var amount = Money.of(new BigDecimal("100.00"), "USD");
 * var doubled = amount.multiply(2);
 * var total = amount.add(Money.of(new BigDecimal("50.00"), "USD"));
 * </pre>
 */
public final class Money {

    private static final int DEFAULT_SCALE = 2;
    private static final RoundingMode DEFAULT_ROUNDING = RoundingMode.HALF_UP;

    private final BigDecimal amount;
    private final Currency currency;

    // Private Constructor
    private Money(BigDecimal amount, Currency currency) {
        Objects.requireNonNull(amount, "Amount must not be null");
        Objects.requireNonNull(currency, "Currency must not be null");

        // Set scale to currency's fraction digits
        var scale = currency.getDefaultFractionDigits();
        this.amount = amount.setScale(scale, DEFAULT_ROUNDING);
        this.currency = currency;
    }

    // Factory Methods

    /**
     * Creates Money with the given amount and currency.
     *
     * @param amount the monetary amount
     * @param currencyCode ISO-4217 currency code (e.g., "USD", "EUR")
     * @return new Money instance
     * @throws IllegalArgumentException if currency code is invalid
     */
    public static Money of(BigDecimal amount, String currencyCode) {
        Objects.requireNonNull(currencyCode, "Currency code must not be null");

        try {
            var currency = Currency.getInstance(currencyCode);
            return new Money(amount, currency);
        } catch (IllegalArgumentException e) {
            throw new IllegalArgumentException("Invalid currency code: " + currencyCode, e);
        }
    }

    /**
     * Creates Money from a numeric amount and currency code.
     *
     * @param amount the monetary amount
     * @param currencyCode ISO-4217 currency code
     * @return new Money instance
     */
    public static Money of(long amount, String currencyCode) {
        return of(BigDecimal.valueOf(amount), currencyCode);
    }

    /**
     * Creates Money from a double amount and currency code.
     *
     * @param amount the monetary amount
     * @param currencyCode ISO-4217 currency code
     * @return new Money instance
     */
    public static Money of(double amount, String currencyCode) {
        return of(BigDecimal.valueOf(amount), currencyCode);
    }

    /**
     * Creates zero Money in the specified currency.
     *
     * @param currencyCode ISO-4217 currency code
     * @return Money with zero amount
     */
    public static Money zero(String currencyCode) {
        return of(BigDecimal.ZERO, currencyCode);
    }

    /**
     * Parses Money from string format "AMOUNT CURRENCY" (e.g., "100.00 USD").
     *
     * @param representation string representation
     * @return parsed Money instance
     * @throws IllegalArgumentException if format is invalid
     */
    public static Money parse(String representation) {
        Objects.requireNonNull(representation, "Representation must not be null");

        var parts = representation.trim().split("\\s+");
        if (parts.length != 2) {
            throw new IllegalArgumentException("Invalid format. Expected: 'AMOUNT CURRENCY'");
        }

        try {
            var amount = new BigDecimal(parts[0]);
            return of(amount, parts[1]);
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("Invalid amount: " + parts[0], e);
        }
    }

    // Arithmetic Operations (All return new instances)

    /**
     * Adds another Money amount to this Money.
     *
     * @param other the Money to add
     * @return new Money with sum
     * @throws IllegalArgumentException if currencies don't match
     */
    public Money add(Money other) {
        validateSameCurrency(other);
        return new Money(this.amount.add(other.amount), this.currency);
    }

    /**
     * Subtracts another Money amount from this Money.
     *
     * @param other the Money to subtract
     * @return new Money with difference
     * @throws IllegalArgumentException if currencies don't match
     */
    public Money subtract(Money other) {
        validateSameCurrency(other);
        return new Money(this.amount.subtract(other.amount), this.currency);
    }

    /**
     * Multiplies this Money by a factor.
     *
     * @param multiplier the multiplication factor
     * @return new Money with product
     */
    public Money multiply(BigDecimal multiplier) {
        Objects.requireNonNull(multiplier, "Multiplier must not be null");
        return new Money(this.amount.multiply(multiplier), this.currency);
    }

    /**
     * Multiplies this Money by an integer factor.
     *
     * @param multiplier the multiplication factor
     * @return new Money with product
     */
    public Money multiply(long multiplier) {
        return multiply(BigDecimal.valueOf(multiplier));
    }

    /**
     * Divides this Money by a divisor.
     *
     * @param divisor the division divisor
     * @return new Money with quotient
     * @throws ArithmeticException if divisor is zero
     */
    public Money divide(BigDecimal divisor) {
        Objects.requireNonNull(divisor, "Divisor must not be null");

        if (divisor.compareTo(BigDecimal.ZERO) == 0) {
            throw new ArithmeticException("Cannot divide by zero");
        }

        return new Money(
            this.amount.divide(divisor, currency.getDefaultFractionDigits(), DEFAULT_ROUNDING),
            this.currency
        );
    }

    /**
     * Divides this Money by an integer divisor.
     *
     * @param divisor the division divisor
     * @return new Money with quotient
     * @throws ArithmeticException if divisor is zero
     */
    public Money divide(long divisor) {
        return divide(BigDecimal.valueOf(divisor));
    }

    /**
     * Returns the absolute value of this Money.
     *
     * @return new Money with absolute amount
     */
    public Money abs() {
        return new Money(this.amount.abs(), this.currency);
    }

    /**
     * Returns the negated value of this Money.
     *
     * @return new Money with negated amount
     */
    public Money negate() {
        return new Money(this.amount.negate(), this.currency);
    }

    // Comparison Operations

    /**
     * Checks if this Money is greater than another Money.
     *
     * @param other the Money to compare
     * @return true if this is greater
     * @throws IllegalArgumentException if currencies don't match
     */
    public boolean isGreaterThan(Money other) {
        validateSameCurrency(other);
        return this.amount.compareTo(other.amount) > 0;
    }

    /**
     * Checks if this Money is greater than or equal to another Money.
     *
     * @param other the Money to compare
     * @return true if this is greater than or equal
     * @throws IllegalArgumentException if currencies don't match
     */
    public boolean isGreaterThanOrEqualTo(Money other) {
        validateSameCurrency(other);
        return this.amount.compareTo(other.amount) >= 0;
    }

    /**
     * Checks if this Money is less than another Money.
     *
     * @param other the Money to compare
     * @return true if this is less
     * @throws IllegalArgumentException if currencies don't match
     */
    public boolean isLessThan(Money other) {
        validateSameCurrency(other);
        return this.amount.compareTo(other.amount) < 0;
    }

    /**
     * Checks if this Money is less than or equal to another Money.
     *
     * @param other the Money to compare
     * @return true if this is less than or equal
     * @throws IllegalArgumentException if currencies don't match
     */
    public boolean isLessThanOrEqualTo(Money other) {
        validateSameCurrency(other);
        return this.amount.compareTo(other.amount) <= 0;
    }

    /**
     * Checks if this Money is zero.
     *
     * @return true if amount is zero
     */
    public boolean isZero() {
        return this.amount.compareTo(BigDecimal.ZERO) == 0;
    }

    /**
     * Checks if this Money is positive.
     *
     * @return true if amount is greater than zero
     */
    public boolean isPositive() {
        return this.amount.compareTo(BigDecimal.ZERO) > 0;
    }

    /**
     * Checks if this Money is negative.
     *
     * @return true if amount is less than zero
     */
    public boolean isNegative() {
        return this.amount.compareTo(BigDecimal.ZERO) < 0;
    }

    /**
     * Checks if this Money is negative or zero.
     *
     * @return true if amount is less than or equal to zero
     */
    public boolean isNegativeOrZero() {
        return this.amount.compareTo(BigDecimal.ZERO) <= 0;
    }

    // Validation

    private void validateSameCurrency(Money other) {
        Objects.requireNonNull(other, "Other money must not be null");

        if (!this.currency.equals(other.currency)) {
            throw new IllegalArgumentException(
                String.format("Currency mismatch: %s vs %s",
                    this.currency.getCurrencyCode(),
                    other.currency.getCurrencyCode())
            );
        }
    }

    // Getters

    public BigDecimal getAmount() {
        return amount;
    }

    public Currency getCurrency() {
        return currency;
    }

    public String getCurrencyCode() {
        return currency.getCurrencyCode();
    }

    // Equality & Hash Code (Value-based)

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Money money = (Money) o;
        return amount.compareTo(money.amount) == 0 &&
               Objects.equals(currency, money.currency);
    }

    @Override
    public int hashCode() {
        return Objects.hash(amount, currency);
    }

    @Override
    public String toString() {
        return String.format("%s %s", amount.toPlainString(), currency.getCurrencyCode());
    }
}
```

## Usage Guidelines

1. **Immutability**: All fields must be `final`, no setters allowed
2. **Validation**: Validate all invariants in constructor
3. **Factory Methods**: Use static factory methods for clarity
4. **Operations**: All operations return new instances, never modify existing
5. **Equality**: Implement value-based equality using all attributes
6. **Null Safety**: Validate all inputs, reject nulls early
7. **Final Class**: Declare class as `final` to prevent subclassing
8. **Self-contained**: Value objects should be self-contained with no external dependencies

## Related Templates

- [Entity Template](./ex-so-stla-ja-te__entity-template.md)
- [Aggregate Template](./ex-so-stla-ja-te__aggregate-template.md)

## See Also

- [Immutability Best Practices](../ex-so-stla-ja__functional-programming.md)

---

**Principles Applied**: Simplicity Over Complexity, Explicit Over Implicit

---

**Last Updated**: 2025-01-23
**Java Version**: 17+
