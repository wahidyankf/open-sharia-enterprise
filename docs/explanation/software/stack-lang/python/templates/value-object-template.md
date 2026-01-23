---
title: Value Object Template
description: Copy-paste ready Python value object template with immutability, equality by value, type hints, and validation for domain-driven design in Sharia-compliant financial applications
category: explanation
subcategory: stack-lang
tags:
  - python
  - template
  - value-object
  - ddd
  - domain-driven-design
  - immutability
related:
  - ../ex-so-stla-py__domain-driven-design.md
  - ../ex-so-stla-py__type-safety.md
  - ./entity-template.md
principles:
  - explicit-over-implicit
  - simplicity-over-complexity
last_updated: 2025-01-23
---

# Value Object Template

## Overview

Value objects are immutable objects described by their values, not identity. Two value objects with same values are interchangeable. This template provides a complete value object implementation with immutability, validation, and value-based equality.

**Use this template when**:

- Object described by its values (no identity)
- Object immutable (never changes after creation)
- Two objects with same values are interchangeable
- Object serves as attribute of entities

**Examples**: `Money`, `Address`, `DateRange`, `EmailAddress`, `PhoneNumber`

## Complete Code Template

```python
"""Value object template for domain-driven design."""

from dataclasses import dataclass
from decimal import Decimal
from typing import Any


@dataclass(frozen=True)
class SampleValueObject:
    """Sample immutable value object.

    Value objects:
    - Immutable (frozen=True)
    - Equality by value
    - No identity
    - Serve as entity attributes

    Attributes:
        # Add domain-specific attributes here
    """

    # DOMAIN ATTRIBUTES (add your specific fields here)
    # Example: value: Decimal
    # Example: currency: str = "USD"

    def __post_init__(self) -> None:
        """Validate value object invariants.

        Use object.__setattr__ for frozen dataclasses.

        Raises:
            ValueError: If invariants violated
        """
        self._validate()

    def _validate(self) -> None:
        """Validate value object invariants.

        Override this method to add domain-specific validation.

        Raises:
            ValueError: If validation fails
        """
        # Add validation rules here
        # Example:
        # if self.value < 0:
        #     raise ValueError("Value cannot be negative")
        pass

    # Equality inherited from dataclass (compares all fields)
    # Hash inherited from dataclass (frozen=True makes it hashable)


# ============================================================================
# FINANCIAL DOMAIN EXAMPLE: Money Value Object
# ============================================================================


@dataclass(frozen=True)
class Money:
    """Immutable money value object.

    Represents monetary value with currency.

    Attributes:
        amount: Monetary amount (precise Decimal)
        currency: Currency code (ISO 4217)
    """

    amount: Decimal
    currency: str = "USD"

    def __post_init__(self) -> None:
        """Validate money invariants."""
        # Validate precision (max 2 decimal places)
        if self.amount.as_tuple().exponent < -2:
            raise ValueError("Amount must have max 2 decimal places")

        # Validate currency code
        valid_currencies = ["USD", "SAR", "EUR", "GBP", "AED"]
        if self.currency not in valid_currencies:
            raise ValueError(f"Invalid currency: {self.currency}")

    def __str__(self) -> str:
        """Human-readable representation.

        Returns:
            Formatted money string
        """
        return f"{self.currency} {self.amount:,.2f}"

    def __add__(self, other: "Money") -> "Money":
        """Add two money values (same currency).

        Args:
            other: Money to add

        Returns:
            New Money with sum

        Raises:
            ValueError: If different currencies
        """
        if self.currency != other.currency:
            raise ValueError(
                f"Cannot add different currencies: {self.currency} and {other.currency}"
            )
        return Money(self.amount + other.amount, self.currency)

    def __sub__(self, other: "Money") -> "Money":
        """Subtract two money values (same currency).

        Args:
            other: Money to subtract

        Returns:
            New Money with difference

        Raises:
            ValueError: If different currencies
        """
        if self.currency != other.currency:
            raise ValueError(
                f"Cannot subtract different currencies: {self.currency} and {other.currency}"
            )
        return Money(self.amount - other.amount, self.currency)

    def __mul__(self, multiplier: Decimal) -> "Money":
        """Multiply money by scalar.

        Args:
            multiplier: Decimal multiplier

        Returns:
            New Money with product
        """
        return Money(self.amount * multiplier, self.currency)

    def __truediv__(self, divisor: Decimal) -> "Money":
        """Divide money by scalar.

        Args:
            divisor: Decimal divisor

        Returns:
            New Money with quotient

        Raises:
            ValueError: If division by zero
        """
        if divisor == 0:
            raise ValueError("Cannot divide by zero")
        return Money(self.amount / divisor, self.currency)

    def is_positive(self) -> bool:
        """Check if amount is positive.

        Returns:
            True if amount > 0
        """
        return self.amount > 0

    def is_zero(self) -> bool:
        """Check if amount is zero.

        Returns:
            True if amount == 0
        """
        return self.amount == 0

    def negate(self) -> "Money":
        """Negate money value.

        Returns:
            New Money with negated amount
        """
        return Money(-self.amount, self.currency)
```

## Usage Example

```python
from decimal import Decimal


# Create money values
zakat = Money(Decimal("2500.00"), "USD")
donation = Money(Decimal("5000.00"), "USD")

print(zakat)  # USD 2,500.00
print(donation)  # USD 5,000.00

# Arithmetic operations
total = zakat + donation
print(total)  # USD 7,500.00

# Multiplication
zakat_doubled = zakat * Decimal("2")
print(zakat_doubled)  # USD 5,000.00

# Division
half_zakat = zakat / Decimal("2")
print(half_zakat)  # USD 1,250.00

# Immutability - operations create NEW objects
print(f"Original zakat unchanged: {zakat}")  # USD 2,500.00

# Value equality
zakat2 = Money(Decimal("2500.00"), "USD")
print(f"Same value? {zakat == zakat2}")  # True

# Hashable (can use in sets, dict keys)
money_set = {zakat, donation, zakat2}
print(f"Unique values: {len(money_set)}")  # 2 (zakat and zakat2 are same)
```

## Customization Guide

### Step 1: Replace Names

```python
# Change class name to your domain value object
class Money:  # Was: SampleValueObject
    """Your domain value object."""
```

### Step 2: Add Domain Attributes

```python
@dataclass(frozen=True)  # IMPORTANT: Keep frozen=True for immutability
class Money:
    # ADD YOUR ATTRIBUTES HERE
    amount: Decimal
    currency: str = "USD"
```

### Step 3: Add Validation

```python
def __post_init__(self) -> None:
    """Add your validation rules."""
    # Validate precision
    if self.amount.as_tuple().exponent < -2:
        raise ValueError("Amount must have max 2 decimal places")

    # Validate currency
    if self.currency not in ["USD", "SAR", "EUR"]:
        raise ValueError(f"Invalid currency: {self.currency}")
```

### Step 4: Add Operations

```python
def __add__(self, other: "Money") -> "Money":
    """Add domain-specific operations."""
    if self.currency != other.currency:
        raise ValueError("Cannot add different currencies")
    return Money(self.amount + other.amount, self.currency)
```

## Best Practices

### Do: Use frozen=True

```python
# GOOD: Immutable value object
@dataclass(frozen=True)
class Money:
    amount: Decimal
```

### Don't: Mutate After Creation

```python
# BAD: Trying to mutate frozen dataclass
money = Money(Decimal("100"), "USD")
# money.amount = Decimal("200")  # Error: frozen dataclass

# GOOD: Create new instance
new_money = Money(Decimal("200"), "USD")
```

### Do: Return New Instances

```python
# GOOD: Operations return NEW instances
def __add__(self, other: "Money") -> "Money":
    return Money(self.amount + other.amount, self.currency)  # New instance
```

### Do: Use Decimal for Money

```python
# GOOD: Decimal for financial calculations
amount: Decimal = Decimal("2500.00")

# BAD: Float for money (precision issues)
# amount: float = 2500.00  # NEVER use float for money!
```

## Related Patterns

- **Entity**: Use for objects with identity → [entity-template.md](./entity-template.md)
- **Aggregate**: Value objects as aggregate parts → [aggregate-template.md](./aggregate-template.md)

## References

- [Domain-Driven Design](../ex-so-stla-py__domain-driven-design.md)
- [Type Safety](../ex-so-stla-py__type-safety.md)

---

**Last Updated**: 2025-01-23
**Python Version**: 3.11+ (baseline), 3.12+ (stable maintenance), 3.13.x (latest stable)
**Maintainers**: OSE Platform Documentation Team
