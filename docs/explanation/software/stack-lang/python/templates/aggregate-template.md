---
title: Aggregate Template
description: Copy-paste ready Python aggregate template with root entity, consistency boundaries, invariants, and domain events for domain-driven design in Sharia-compliant financial applications
category: explanation
subcategory: stack-lang
tags:
  - python
  - template
  - aggregate
  - ddd
  - domain-driven-design
  - consistency
related:
  - ../ex-so-stla-py__domain-driven-design.md
  - ./entity-template.md
  - ./domain-event-template.md
principles:
  - explicit-over-implicit
  - simplicity-over-complexity
last_updated: 2025-01-23
---

# Aggregate Template

## Overview

Aggregates are clusters of entities and value objects treated as a single unit for data changes. The aggregate root is the only member accessible from outside. Use aggregates to enforce consistency boundaries and invariants across multiple objects.

**Use this template when**:

- Multiple objects must maintain consistency together
- Transaction boundaries needed
- Business rules span multiple entities
- Need to enforce invariants across related objects

**Examples**: `DonationCampaign` (with Donations), `QardHasanLoan` (with Payments)

## Complete Code Template

```python
"""Aggregate template for domain-driven design."""

from dataclasses import dataclass, field
from decimal import Decimal
from datetime import datetime
from typing import List
from uuid import uuid4


# ============================================================================
# FINANCIAL DOMAIN EXAMPLE: DonationCampaign Aggregate
# ============================================================================


@dataclass
class Donation:
    """Donation entity (aggregate member).

    Managed by DonationCampaign aggregate root.
    """

    id: str = field(default_factory=lambda: f"DON-{str(uuid4())[:8]}")
    donor_id: str = ""
    amount: Decimal = Decimal("0")
    message: str = ""
    donated_at: datetime = field(default_factory=datetime.utcnow)

    def __post_init__(self) -> None:
        """Validate donation."""
        if not self.donor_id:
            raise ValueError("Donor ID required")
        if self.amount <= 0:
            raise ValueError("Donation amount must be positive")


@dataclass
class DonationCampaign:
    """Donation campaign aggregate root.

    Enforces consistency for campaign and its donations.
    All donations accessed through campaign (aggregate root).

    Invariants:
    - Current amount must equal sum of all donations
    - Cannot accept donations after campaign completed
    - Cannot complete campaign before end date
    """

    # ROOT ENTITY ATTRIBUTES
    id: str = field(default_factory=lambda: f"CAMP-{str(uuid4())[:8]}")
    name: str = ""
    target_amount: Decimal = Decimal("0")
    current_amount: Decimal = Decimal("0")
    status: str = "active"  # active, paused, completed, cancelled

    # AGGREGATE MEMBERS (private, accessed only through root)
    _donations: List[Donation] = field(default_factory=list, repr=False)

    # LIFECYCLE
    created_at: datetime = field(default_factory=datetime.utcnow)
    updated_at: datetime = field(default_factory=datetime.utcnow)
    version: int = 1

    def __post_init__(self) -> None:
        """Validate aggregate invariants."""
        self._validate()

    def _validate(self) -> None:
        """Validate aggregate invariants.

        Raises:
            ValueError: If invariants violated
        """
        if not self.name:
            raise ValueError("Campaign name required")

        if self.target_amount <= 0:
            raise ValueError("Target amount must be positive")

        if self.current_amount < 0:
            raise ValueError("Current amount cannot be negative")

        if self.status not in ["active", "paused", "completed", "cancelled"]:
            raise ValueError(f"Invalid status: {self.status}")

        # Invariant: current_amount == sum of donations
        total_donations = sum(d.amount for d in self._donations)
        if self.current_amount != total_donations:
            raise ValueError(
                f"Invariant violation: current_amount ({self.current_amount}) "
                f"!= sum of donations ({total_donations})"
            )

    @property
    def donations(self) -> List[Donation]:
        """Get read-only view of donations.

        Returns:
            Copy of donations list (prevent external modification)
        """
        return self._donations.copy()

    @property
    def donor_count(self) -> int:
        """Get number of unique donors.

        Returns:
            Count of unique donor IDs
        """
        return len({d.donor_id for d in self._donations})

    @property
    def progress_percentage(self) -> Decimal:
        """Calculate campaign progress.

        Returns:
            Progress as percentage (0-100)
        """
        if self.target_amount == 0:
            return Decimal("0")
        return (self.current_amount / self.target_amount) * Decimal("100")

    @property
    def is_completed(self) -> bool:
        """Check if campaign completed.

        Returns:
            True if status is completed
        """
        return self.status == "completed"

    def add_donation(
        self, donor_id: str, amount: Decimal, message: str = ""
    ) -> Donation:
        """Add donation to campaign.

        Enforces invariants:
        - Campaign must be active
        - Donation amount must be positive
        - Updates current_amount

        Args:
            donor_id: Donor identifier
            amount: Donation amount
            message: Optional donation message

        Returns:
            Created donation

        Raises:
            ValueError: If campaign not active or amount invalid
        """
        # Enforce invariant: only accept donations when active
        if self.status != "active":
            raise ValueError(f"Cannot add donation to {self.status} campaign")

        # Create donation
        donation = Donation(
            donor_id=donor_id, amount=amount, message=message
        )

        # Add to aggregate
        self._donations.append(donation)

        # Update aggregate state (maintain invariant)
        self.current_amount += amount
        self._update()

        return donation

    def complete(self) -> None:
        """Complete campaign.

        Enforces invariants:
        - Campaign must be active
        - Must have reached target

        Raises:
            ValueError: If invariants violated
        """
        if self.status != "active":
            raise ValueError(f"Cannot complete {self.status} campaign")

        if self.current_amount < self.target_amount:
            raise ValueError(
                f"Cannot complete: current ${self.current_amount} < "
                f"target ${self.target_amount}"
            )

        self.status = "completed"
        self._update()

    def pause(self) -> None:
        """Pause campaign.

        Raises:
            ValueError: If campaign not active
        """
        if self.status != "active":
            raise ValueError(f"Cannot pause {self.status} campaign")

        self.status = "paused"
        self._update()

    def resume(self) -> None:
        """Resume paused campaign.

        Raises:
            ValueError: If campaign not paused
        """
        if self.status != "paused":
            raise ValueError(f"Cannot resume {self.status} campaign")

        self.status = "active"
        self._update()

    def cancel(self) -> None:
        """Cancel campaign.

        Raises:
            ValueError: If campaign already completed
        """
        if self.status == "completed":
            raise ValueError("Cannot cancel completed campaign")

        self.status = "cancelled"
        self._update()

    def _update(self) -> None:
        """Update aggregate timestamp and version."""
        self.updated_at = datetime.utcnow()
        self.version += 1
        self._validate()  # Re-validate after state change
```

## Usage Example

```python
from decimal import Decimal

# Create campaign
campaign = DonationCampaign(
    name="Ramadan Relief",
    target_amount=Decimal("500000"),
)

print(f"Campaign: {campaign.name}")
print(f"Target: ${campaign.target_amount:,.2f}")
print(f"Current: ${campaign.current_amount:,.2f}")

# Add donations (through aggregate root)
donation1 = campaign.add_donation(
    donor_id="DONOR-001",
    amount=Decimal("10000"),
    message="May Allah accept it",
)

donation2 = campaign.add_donation(
    donor_id="DONOR-002",
    amount=Decimal("25000"),
)

print(f"Donations: {len(campaign.donations)}")
print(f"Donors: {campaign.donor_count}")
print(f"Current: ${campaign.current_amount:,.2f}")
print(f"Progress: {campaign.progress_percentage:.1f}%")

# Aggregate enforces invariants
# current_amount == sum of donations (maintained automatically)
assert campaign.current_amount == Decimal("35000")

# Cannot add donations to non-active campaign
campaign.complete()  # Would fail: target not reached
# ValueError: Cannot complete: current $35,000 < target $500,000
```

## Customization Guide

### Step 1: Define Aggregate Members

```python
# Define entities/value objects that belong to aggregate
@dataclass
class Donation:  # Aggregate member
    id: str
    donor_id: str
    amount: Decimal
```

### Step 2: Define Aggregate Root

```python
@dataclass
class DonationCampaign:  # Aggregate root
    id: str
    name: str
    _donations: List[Donation] = field(default_factory=list)  # Private
```

### Step 3: Enforce Invariants

```python
def _validate(self) -> None:
    """Enforce business rules."""
    # Invariant: current_amount must equal sum of donations
    total = sum(d.amount for d in self._donations)
    if self.current_amount != total:
        raise ValueError("Invariant violation")
```

### Step 4: Add Domain Methods

```python
def add_donation(self, donor_id: str, amount: Decimal) -> Donation:
    """Add donation (maintains invariants)."""
    donation = Donation(donor_id=donor_id, amount=amount)
    self._donations.append(donation)
    self.current_amount += amount  # Maintain invariant
    self._update()
    return donation
```

## Best Practices

### Do: Enforce Invariants

```python
# GOOD: Validate after every state change
def add_donation(self, ...):
    self._donations.append(donation)
    self.current_amount += donation.amount
    self._update()  # Validates invariants
```

### Do: Keep Members Private

```python
# GOOD: Private members, public read-only property
_donations: List[Donation] = field(default_factory=list)

@property
def donations(self) -> List[Donation]:
    return self._donations.copy()  # Read-only
```

### Don't: Bypass Aggregate Root

```python
# BAD: Directly modifying aggregate members
campaign._donations.append(donation)  # BAD: Breaks invariants

# GOOD: Use aggregate root methods
campaign.add_donation(...)  # GOOD: Maintains invariants
```

## Related Patterns

- **Entity**: Aggregate root is an entity → [entity-template.md](./entity-template.md)
- **Domain Event**: Emit events on state changes → [domain-event-template.md](./domain-event-template.md)
- **Repository**: Persist aggregates → [repository-template.md](./repository-template.md)

## References

- [Domain-Driven Design](../ex-so-stla-py__domain-driven-design.md)

---

**Last Updated**: 2025-01-23
**Python Version**: 3.11+ (baseline), 3.12+ (stable maintenance), 3.13.x (latest stable)
**Maintainers**: OSE Platform Documentation Team
