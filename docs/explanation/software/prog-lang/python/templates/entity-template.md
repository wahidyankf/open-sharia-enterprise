---
title: Entity Template
description: Copy-paste ready Python entity template with identity, lifecycle, type hints, and validation for domain-driven design in Sharia-compliant financial applications
category: explanation
subcategory: stack-lang
tags:
  - python
  - template
  - entity
  - ddd
  - domain-driven-design
related:
  - ../ex-so-stla-py__domain-driven-design.md
  - ../ex-so-stla-py__type-safety.md
  - ./value-object-template.md
  - ./aggregate-template.md
principles:
  - explicit-over-implicit
  - simplicity-over-complexity
last_updated: 2025-01-23
---

# Entity Template

## Overview

Entities are objects with unique identity and lifecycle. Unlike value objects, entities are mutable and tracked by ID rather than values. This template provides a complete entity implementation with type hints, validation, and lifecycle methods.

**Use this template when**:

- Object has unique identifier (ID)
- Object changes over time (mutable state)
- Object lifecycle matters (created, updated, deleted)
- Object must be tracked individually

**Examples**: `ZakatRecord`, `DonationCampaign`, `QardHasanLoan`, `PayerAccount`

## Complete Code Template

```python
"""Entity template for domain-driven design."""

from dataclasses import dataclass, field
from decimal import Decimal
from datetime import datetime
from typing import Optional
from uuid import uuid4, UUID


@dataclass
class SampleEntity:
    """Sample entity with identity and lifecycle.

    Entities have unique identity and mutable state.
    Two entities with same ID are considered equal.

    Attributes:
        id: Unique identifier (immutable)
        created_at: Creation timestamp (immutable)
        updated_at: Last update timestamp
        version: Version number for optimistic locking
        # Add domain-specific attributes here
    """

    # IDENTITY (immutable)
    id: str = field(default_factory=lambda: str(uuid4()))
    created_at: datetime = field(default_factory=datetime.utcnow)

    # LIFECYCLE (mutable)
    updated_at: datetime = field(default_factory=datetime.utcnow)
    version: int = 1

    # DOMAIN ATTRIBUTES (add your specific fields here)
    # Example: name: str
    # Example: amount: Decimal
    # Example: status: str = "active"

    def __post_init__(self) -> None:
        """Validate entity invariants after initialization.

        Raises:
            ValueError: If invariants violated
        """
        self._validate()

    def _validate(self) -> None:
        """Validate entity invariants.

        Override this method to add domain-specific validation.

        Raises:
            ValueError: If validation fails
        """
        # Add validation rules here
        # Example:
        # if self.amount < 0:
        #     raise ValueError("Amount cannot be negative")
        pass

    def __eq__(self, other: object) -> bool:
        """Entities equal if same ID.

        Args:
            other: Object to compare

        Returns:
            True if same entity (same ID)
        """
        if not isinstance(other, SampleEntity):
            return False
        return self.id == other.id

    def __hash__(self) -> int:
        """Hash based on ID.

        Returns:
            Hash of entity ID
        """
        return hash(self.id)

    def update(self) -> None:
        """Mark entity as updated.

        Updates timestamp and version for optimistic locking.
        """
        self.updated_at = datetime.utcnow()
        self.version += 1


# ============================================================================
# FINANCIAL DOMAIN EXAMPLE: ZakatRecord Entity
# ============================================================================


@dataclass
class ZakatRecord:
    """Zakat calculation record entity.

    Tracks Zakat calculation and payment lifecycle.

    Attributes:
        id: Unique record identifier
        payer_id: Payer identifier
        wealth_amount: Total wealth subject to Zakat
        nisab_threshold: Nisab threshold at calculation time
        zakat_amount: Calculated Zakat amount (2.5% of qualifying wealth)
        calculation_date: When Zakat was calculated
        payment_status: Payment status (pending, completed, cancelled)
        payment_date: When payment was made (if completed)
        created_at: Record creation timestamp
        updated_at: Last update timestamp
        version: Version for optimistic locking
    """

    # IDENTITY
    id: str = field(default_factory=lambda: f"ZKT-{str(uuid4())[:8]}")
    payer_id: str = ""

    # FINANCIAL ATTRIBUTES
    wealth_amount: Decimal = Decimal("0")
    nisab_threshold: Decimal = Decimal("85000")
    zakat_amount: Decimal = Decimal("0")

    # LIFECYCLE
    calculation_date: datetime = field(default_factory=datetime.utcnow)
    payment_status: str = "pending"
    payment_date: Optional[datetime] = None
    created_at: datetime = field(default_factory=datetime.utcnow)
    updated_at: datetime = field(default_factory=datetime.utcnow)
    version: int = 1

    def __post_init__(self) -> None:
        """Validate Zakat record after initialization."""
        self._validate()

    def _validate(self) -> None:
        """Validate Zakat record invariants.

        Raises:
            ValueError: If invariants violated
        """
        if not self.payer_id:
            raise ValueError("Payer ID required")

        if self.wealth_amount < 0:
            raise ValueError("Wealth amount cannot be negative")

        if self.nisab_threshold <= 0:
            raise ValueError("Nisab threshold must be positive")

        if self.zakat_amount < 0:
            raise ValueError("Zakat amount cannot be negative")

        if self.payment_status not in ["pending", "completed", "cancelled"]:
            raise ValueError(f"Invalid payment status: {self.payment_status}")

    def __eq__(self, other: object) -> bool:
        """Entities equal if same ID."""
        if not isinstance(other, ZakatRecord):
            return False
        return self.id == other.id

    def __hash__(self) -> int:
        """Hash based on ID."""
        return hash(self.id)

    @property
    def is_obligated(self) -> bool:
        """Check if Zakat is obligated.

        Returns:
            True if wealth exceeds nisab
        """
        return self.wealth_amount >= self.nisab_threshold

    @property
    def is_paid(self) -> bool:
        """Check if payment completed.

        Returns:
            True if payment status is completed
        """
        return self.payment_status == "completed"

    def mark_paid(self, payment_date: Optional[datetime] = None) -> None:
        """Mark Zakat payment as completed.

        Args:
            payment_date: Payment date (defaults to now)

        Raises:
            ValueError: If already paid or cancelled
        """
        if self.payment_status == "completed":
            raise ValueError("Payment already completed")

        if self.payment_status == "cancelled":
            raise ValueError("Cannot mark cancelled payment as paid")

        self.payment_status = "completed"
        self.payment_date = payment_date or datetime.utcnow()
        self.update()

    def cancel(self) -> None:
        """Cancel Zakat payment.

        Raises:
            ValueError: If already completed
        """
        if self.payment_status == "completed":
            raise ValueError("Cannot cancel completed payment")

        self.payment_status = "cancelled"
        self.update()

    def update(self) -> None:
        """Mark entity as updated."""
        self.updated_at = datetime.utcnow()
        self.version += 1
```

## Usage Example

```python
from decimal import Decimal
from datetime import datetime


# Create new Zakat record
record = ZakatRecord(
    payer_id="PAYER-12345",
    wealth_amount=Decimal("100000"),
    nisab_threshold=Decimal("85000"),
    zakat_amount=Decimal("2500"),
)

print(f"Record ID: {record.id}")  # ZKT-abc12345
print(f"Is obligated: {record.is_obligated}")  # True
print(f"Is paid: {record.is_paid}")  # False

# Mark payment as completed
record.mark_paid()
print(f"Payment status: {record.payment_status}")  # completed
print(f"Payment date: {record.payment_date}")  # 2025-01-23 ...

# Version incremented after update
print(f"Version: {record.version}")  # 2


# Entities equal by ID
record2 = ZakatRecord(
    payer_id="PAYER-67890",
    wealth_amount=Decimal("150000"),
    nisab_threshold=Decimal("85000"),
    zakat_amount=Decimal("3750"),
)

print(f"Same record? {record == record2}")  # False (different IDs)
```

## Customization Guide

### Step 1: Replace Names

```python
# Change class name to your domain entity
class ZakatRecord:  # Was: SampleEntity
    """Your domain entity."""
```

### Step 2: Add Domain Attributes

```python
@dataclass
class ZakatRecord:
    # IDENTITY (keep these)
    id: str = field(default_factory=lambda: f"ZKT-{str(uuid4())[:8]}")
    created_at: datetime = field(default_factory=datetime.utcnow)

    # ADD YOUR ATTRIBUTES HERE
    payer_id: str = ""
    wealth_amount: Decimal = Decimal("0")
    zakat_amount: Decimal = Decimal("2500")
    payment_status: str = "pending"
```

### Step 3: Add Validation

```python
def _validate(self) -> None:
    """Add your validation rules."""
    if self.wealth_amount < 0:
        raise ValueError("Wealth cannot be negative")

    if self.payment_status not in ["pending", "completed"]:
        raise ValueError(f"Invalid status: {self.payment_status}")
```

### Step 4: Add Business Methods

```python
def mark_paid(self) -> None:
    """Add domain-specific behavior."""
    if self.payment_status == "completed":
        raise ValueError("Already paid")

    self.payment_status = "completed"
    self.update()  # Update timestamp and version
```

## Best Practices

### Do: Use Unique IDs

```python
# GOOD: Generate unique ID
id: str = field(default_factory=lambda: str(uuid4()))

# GOOD: Domain-specific ID format
id: str = field(default_factory=lambda: f"ZKT-{str(uuid4())[:8]}")
```

### Do: Validate in **post_init**

```python
# GOOD: Validate immediately
def __post_init__(self) -> None:
    self._validate()
```

### Don't: Compare by Attributes

```python
# BAD: Comparing by attributes
def __eq__(self, other):
    return self.name == other.name and self.amount == other.amount

# GOOD: Compare by ID only
def __eq__(self, other):
    return isinstance(other, ZakatRecord) and self.id == other.id
```

### Do: Track Versions

```python
# GOOD: Optimistic locking support
def update(self) -> None:
    self.updated_at = datetime.utcnow()
    self.version += 1  # Increment version
```

## Related Patterns

- **Value Object**: Use for objects without identity → [value-object-template.md](./value-object-template.md)
- **Aggregate**: Use for entity clusters → [aggregate-template.md](./aggregate-template.md)
- **Repository**: Persist entities → [repository-template.md](./repository-template.md)

## References

- [Domain-Driven Design](../ex-so-stla-py__domain-driven-design.md)
- [Type Safety](../ex-so-stla-py__type-safety.md)

---

**Last Updated**: 2025-01-23
**Python Version**: 3.11+ (baseline), 3.12+ (stable maintenance), 3.14.x (latest stable)
**Maintainers**: OSE Platform Documentation Team
