---
title: Python Anti-Patterns
description: Common Python mistakes and anti-patterns to avoid including mutable defaults, global state, exception swallowing, import anti-patterns, and financial domain pitfalls
category: explanation
subcategory: stack-lang
tags:
  - python
  - anti-patterns
  - code-quality
  - common-mistakes
  - pitfalls
related:
  - ./ex-so-stla-py__best-practices.md
  - ./ex-so-stla-py__idioms.md
  - ./ex-so-stla-py__error-handling.md
principles:
  - simplicity-over-complexity
  - explicit-over-implicit
last_updated: 2025-01-23
---

# Python Anti-Patterns

**Quick Reference**: [Overview](#overview) | [Mutable Defaults](#mutable-default-arguments) | [Global State](#global-state) | [Exception Swallowing](#exception-swallowing) | [Import Anti-Patterns](#import-anti-patterns) | [Circular Imports](#circular-imports) | [Type Confusion](#type-confusion) | [Premature Optimization](#premature-optimization) | [Financial Domain](#financial-domain-anti-patterns) | [References](#references)

## Overview

Anti-patterns are common but flawed solutions that create bugs, reduce maintainability, or harm performance. For financial applications, avoiding these patterns prevents calculation errors and data integrity issues.

## Mutable Default Arguments

Mutable defaults (`[]`, `{}`) are shared across function calls.

### The Problem

```python
# BAD: Mutable default argument
from decimal import Decimal
from typing import List


def record_donations(
    donor_id: str,
    donations: List[Decimal] = [],  # BAD: Shared across calls
) -> List[Decimal]:
    """BAD: Mutable default creates shared state."""
    donations.append(Decimal("100.00"))
    return donations


# Demonstrates problem
result1 = record_donations("D001")  # [Decimal('100.00')]
result2 = record_donations("D002")  # [Decimal('100.00'), Decimal('100.00')] - UNEXPECTED!
result3 = record_donations("D003")  # [Decimal('100.00'), Decimal('100.00'), Decimal('100.00')] - BUG!
```

### The Solution

```python
# GOOD: None as default, create mutable in function
from typing import Optional, List
from decimal import Decimal


def record_donations(
    donor_id: str,
    donations: Optional[List[Decimal]] = None,
) -> List[Decimal]:
    """GOOD: Create new list each call."""
    if donations is None:
        donations = []  # New list each call
    donations.append(Decimal("100.00"))
    return donations


# Correct behavior
result1 = record_donations("D001")  # [Decimal('100.00')]
result2 = record_donations("D002")  # [Decimal('100.00')] - Correct!
```

**Why this matters**: Mutable defaults evaluated once at function definition. All calls share same object. Use `None` as default, create mutable in function body.

## Global State

Global variables create hidden dependencies and make testing difficult.

### The Problem

```python
# BAD: Global state for accumulation
from decimal import Decimal

total_zakat = Decimal("0")  # BAD: Global state


def calculate_and_accumulate_zakat(wealth: Decimal) -> Decimal:
    """BAD: Modifies global state."""
    global total_zakat
    zakat = wealth * Decimal("0.025")
    total_zakat += zakat  # BAD: Side effect
    return zakat


# Problems: Not thread-safe, hard to test, hidden dependency
zakat1 = calculate_and_accumulate_zakat(Decimal("100000"))
zakat2 = calculate_and_accumulate_zakat(Decimal("150000"))
print(total_zakat)  # State accumulated invisibly
```

### The Solution

```python
# GOOD: Explicit state management
from dataclasses import dataclass
from decimal import Decimal
from typing import List


@dataclass
class ZakatAccumulator:
    """Explicit state container."""

    total: Decimal = Decimal("0")

    def add_calculation(self, wealth: Decimal) -> Decimal:
        """Calculate and accumulate explicitly."""
        zakat = wealth * Decimal("0.025")
        self.total += zakat
        return zakat


# Usage: Explicit, testable, thread-safe per instance
accumulator = ZakatAccumulator()
zakat1 = accumulator.add_calculation(Decimal("100000"))
zakat2 = accumulator.add_calculation(Decimal("150000"))
print(accumulator.total)  # State visible and controlled
```

**Why this matters**: Global state creates hidden dependencies. Not thread-safe. Testing requires state cleanup. Explicit state containers clearer.

## Exception Swallowing

Silent failures hide bugs.

### The Problem

```python
# BAD: Bare except and silent failure
from decimal import Decimal, InvalidOperation


def parse_zakat_amount(amount_str: str) -> Decimal:
    """BAD: Swallows exceptions silently."""
    try:
        return Decimal(amount_str)
    except:  # BAD: Catches everything
        return Decimal("0")  # BAD: Silent failure, no logging


# Usage: Errors hidden, debugging impossible
result = parse_zakat_amount("invalid")  # Returns 0 - BUG HIDDEN!
```

### The Solution

```python
# GOOD: Specific exceptions with logging
from decimal import Decimal, InvalidOperation
import logging

logger = logging.getLogger(__name__)


def parse_zakat_amount(amount_str: str) -> Decimal:
    """GOOD: Explicit error handling."""
    try:
        return Decimal(amount_str)
    except InvalidOperation as e:
        logger.error(f"Invalid Zakat amount: {amount_str}, error: {e}")
        raise ValueError(f"Cannot parse Zakat amount: {amount_str}") from e


# Usage: Errors visible and logged
try:
    result = parse_zakat_amount("invalid")
except ValueError as e:
    print(f"Error: {e}")  # Clear error message
```

**Why this matters**: Bare `except:` catches system exceptions (KeyboardInterrupt, SystemExit). Silent failures hide bugs. Log and re-raise for observability.

## Import Anti-Patterns

### Import \* (Wildcard Imports)

```python
# BAD: Wildcard import
from decimal import *  # BAD: Pollutes namespace

# Which Decimal? Where did InvalidOperation come from?
amount = Decimal("100.00")
```

### The Solution

```python
# GOOD: Explicit imports
from decimal import Decimal, InvalidOperation

# Clear origin of each name
amount = Decimal("100.00")
```

**Why this matters**: Wildcard imports pollute namespace. Unclear name origins. Name conflicts. Explicit imports document dependencies.

## Circular Imports

Circular dependencies cause import errors.

### The Problem

```python
# BAD: Circular import
# File: models.py
from calculators import StandardZakatCalculator  # Imports calculators


class ZakatRecord:
    def calculate(self):
        calc = StandardZakatCalculator()
        # ...


# File: calculators.py
from models import ZakatRecord  # Imports models - CIRCULAR!


class StandardZakatCalculator:
    def process_record(self, record: ZakatRecord):
        # ...
```

### The Solution

```python
# GOOD: Break circular dependency with protocols
# File: models.py
from typing import Protocol


class Calculator(Protocol):
    """Protocol breaks circular dependency."""

    def calculate(self, amount: Decimal) -> Decimal:
        ...


class ZakatRecord:
    def calculate(self, calculator: Calculator):  # Protocol type
        # ...


# File: calculators.py
# No import of models needed!


class StandardZakatCalculator:
    """Satisfies Calculator protocol."""

    def calculate(self, amount: Decimal) -> Decimal:
        return amount * Decimal("0.025")
```

**Why this matters**: Circular imports cause `ImportError`. Protocols or dependency injection break cycles. Clear dependency direction (domain ← application ← infrastructure).

## Type Confusion

Mixing types causes unexpected behavior.

### The Problem

```python
# BAD: Mixing float and Decimal
from decimal import Decimal

zakat_rate = 0.025  # BAD: float
wealth = Decimal("100000.00")  # Decimal

# Type mismatch causes float result
zakat = wealth * zakat_rate  # Returns float, not Decimal!
print(type(zakat))  # <class 'float'> - UNEXPECTED!
```

### The Solution

```python
# GOOD: Consistent Decimal usage
from decimal import Decimal

zakat_rate = Decimal("0.025")  # Decimal
wealth = Decimal("100000.00")  # Decimal

# Consistent types
zakat = wealth * zakat_rate  # Returns Decimal
print(type(zakat))  # <class 'decimal.Decimal'> - Correct!
```

**Why this matters**: Float/Decimal mixing returns float (loses precision). Financial calculations require Decimal throughout. Type hints catch mismatches.

## Premature Optimization

Optimizing before profiling wastes effort.

### The Problem

```python
# BAD: Premature optimization
from decimal import Decimal
from typing import List


def calculate_all_zakat_optimized(wealth_items: List[Decimal]) -> Decimal:
    """BAD: Complex optimization without profiling."""
    # Assumption: list comprehension faster (unverified)
    # Assumption: single multiplication faster than loop (unverified)
    return sum([w * Decimal("0.025") for w in wealth_items])


# Versus simple version (probably same speed for small lists)
def calculate_all_zakat_simple(wealth_items: List[Decimal]) -> Decimal:
    """Simple, readable version."""
    total = Decimal("0")
    for wealth in wealth_items:
        total += wealth * Decimal("0.025")
    return total
```

### The Solution

```python
# GOOD: Profile first, optimize second
import cProfile
from decimal import Decimal
from typing import List


def calculate_all_zakat(wealth_items: List[Decimal]) -> Decimal:
    """Start with simple, readable code."""
    return sum(wealth * Decimal("0.025") for wealth in wealth_items)


# Profile before optimizing
def profile_calculation():
    """Profile to identify actual bottlenecks."""
    wealth_data = [Decimal("100000")] * 10000

    profiler = cProfile.Profile()
    profiler.enable()

    result = calculate_all_zakat(wealth_data)

    profiler.disable()
    profiler.print_stats()


# Optimize only proven bottlenecks
```

**Why this matters**: Premature optimization complicates code without benefit. Profile identifies real bottlenecks. Readable code easier to optimize later.

## Financial Domain Anti-Patterns

### Using Float for Money

```python
# BAD: Float for financial calculations
total = 0.1 + 0.2  # 0.30000000000000004 - PRECISION ERROR!


# GOOD: Decimal for financial calculations
from decimal import Decimal

total = Decimal("0.1") + Decimal("0.2")  # Decimal('0.3') - Exact!
```

### Not Validating Financial Invariants

```python
# BAD: No validation
class QardHasanLoan:
    def __init__(self, principal, repaid):
        self.principal = principal  # BAD: No validation
        self.repaid = repaid  # BAD: Could exceed principal


# GOOD: Validate invariants
from pydantic import BaseModel, Field, field_validator


class QardHasanLoan(BaseModel):
    principal: Decimal = Field(gt=0)
    repaid: Decimal = Field(ge=0)

    @field_validator("repaid")
    @classmethod
    def validate_repaid(cls, v: Decimal, values: dict) -> Decimal:
        """Ensure repaid doesn't exceed principal."""
        if "principal" in values and v > values["principal"]:
            raise ValueError("Repaid amount exceeds principal")
        return v
```

### Ignoring Rounding in Calculations

```python
# BAD: No explicit rounding
from decimal import Decimal

zakat = Decimal("100000.00") * Decimal("0.025")  # Exact: 2500.00
# But what about: 100000.00 / 3 * 0.025?


# GOOD: Explicit rounding for currency
from decimal import Decimal, ROUND_HALF_UP

wealth = Decimal("100000.00")
zakat = wealth * Decimal("0.025")
zakat_rounded = zakat.quantize(Decimal("0.01"), rounding=ROUND_HALF_UP)
```

**Why this matters**: Float precision errors unacceptable for money. Validation prevents invalid financial states. Explicit rounding ensures consistent currency representation.

## References

### Official Documentation

- [PEP 8 - Style Guide](https://peps.python.org/pep-0008/)
- [Python Common Pitfalls](https://docs.python.org/3/faq/programming.html)

### Related Documentation

- [Best Practices](./ex-so-stla-py__best-practices.md) - Coding standards
- [Idioms](./ex-so-stla-py__idioms.md) - Pythonic patterns
- [Error Handling](./ex-so-stla-py__error-handling.md) - Exception patterns

### Books

- "Effective Python" by Brett Slatkin - Anti-patterns and solutions

---

**Last Updated**: 2025-01-23
**Python Version**: 3.11+ (baseline), 3.12+ (stable maintenance), 3.13.x (latest stable)
**Maintainers**: OSE Platform Documentation Team
