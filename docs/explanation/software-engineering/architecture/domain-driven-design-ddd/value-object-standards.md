---
title: "DDD Value Object Standards"
description: OSE Platform standards for immutable value objects in Islamic finance domains
category: explanation
subcategory: architecture
tags:
  - ddd
  - value-objects
  - immutability
principles:
  - immutability
  - explicit-over-implicit
created: 2026-02-09
---

# DDD Value Object Standards

## Prerequisite Knowledge

**REQUIRED**: Complete [AyoKoding DDD Value Objects](../../../../../apps/ayokoding-web/content/en/learn/software-engineering/software-architecture/patterns-and-principles/) before using these standards.

## Purpose

OSE Platform value object standards for domain primitives.

## REQUIRED: Use Immutable Value Objects

**REQUIRED**: All domain primitives MUST be implemented as immutable value objects.

**Implementation**:

- **Java**: Use `record` (Java 17+)
- **TypeScript**: Use `readonly` properties
- **Go**: Use immutable structs

## OSE Platform Value Objects

### Money

**REQUIRED for all financial amounts**:

#### `Java`

```java
public record Money(BigDecimal amount, Currency currency) {
    public Money {
        if (amount.compareTo(BigDecimal.ZERO) < 0) {
            throw new IllegalArgumentException("Amount cannot be negative");
        }
    }

    public Money add(Money other) {
        assertSameCurrency(other);
        return new Money(amount.add(other.amount), currency);
    }

    public Money multiply(double factor) {
        return new Money(amount.multiply(BigDecimal.valueOf(factor)), currency);
    }
}
```

#### `Kotlin`

```kotlin
data class Money(val amount: BigDecimal, val currency: Currency) {
    init {
        require(amount >= BigDecimal.ZERO) { "Amount cannot be negative" }
    }

    fun add(other: Money): Money {
        assertSameCurrency(other)
        return copy(amount = amount.add(other.amount))
    }

    operator fun times(factor: Double): Money =
        copy(amount = amount.multiply(BigDecimal.valueOf(factor)))
}
```

#### `C#`

```csharp
namespace Ose.Zakat.Domain;

public sealed record Money(decimal Amount, string Currency)
{
    public Money
    {
        if (Amount < 0)
            throw new ArgumentException("Amount cannot be negative", nameof(Amount));
    }

    public Money Add(Money other)
    {
        AssertSameCurrency(other);
        return this with { Amount = Amount + other.Amount };
    }

    public Money Multiply(decimal factor) => this with { Amount = Amount * factor };
}
```

### FiscalDate

**REQUIRED for Zakat calculations (Islamic calendar)**:

#### `Java`

```java
public record FiscalDate(int hijriYear, int hijriMonth, int hijriDay) {
    // Validation, conversion methods
}
```

#### `Kotlin`

```kotlin
data class FiscalDate(
    val hijriYear: Int,
    val hijriMonth: Int,
    val hijriDay: Int,
) {
    // Validation, conversion methods
}
```

#### `C#`

```csharp
namespace Ose.Zakat.Domain;

public sealed record FiscalDate(int HijriYear, int HijriMonth, int HijriDay)
{
    // Validation, conversion methods
}
```

### NisabThreshold

**REQUIRED for Zakat obligation checks**:

#### `Java`

```java
public record NisabThreshold(Money goldEquivalent) {
    private static final BigDecimal GOLD_GRAMS = BigDecimal.valueOf(87.48);

    public boolean exceeds(Money wealth) {
        return wealth.isGreaterThan(goldEquivalent);
    }
}
```

#### `Kotlin`

```kotlin
data class NisabThreshold(val goldEquivalent: Money) {
    fun exceeds(wealth: Money): Boolean = wealth.isGreaterThan(goldEquivalent)

    companion object {
        val GOLD_GRAMS: BigDecimal = BigDecimal.valueOf(87.48)
    }
}
```

#### `C#`

```csharp
namespace Ose.Zakat.Domain;

public sealed record NisabThreshold(Money GoldEquivalent)
{
    private static readonly decimal GoldGrams = 87.48m;

    public bool Exceeds(Money wealth) => wealth.IsGreaterThan(GoldEquivalent);
}
```

## Validation

**REQUIRED**: All value objects MUST validate invariants in constructor.

**PROHIBITED**: Setters (value objects are immutable).
