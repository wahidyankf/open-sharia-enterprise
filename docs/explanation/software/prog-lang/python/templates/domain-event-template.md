---
title: Domain Event Template
description: Copy-paste ready Python domain event template with immutability, type safety, and event handling for event-driven architecture in Sharia-compliant financial applications
category: explanation
subcategory: prog-lang
tags:
  - python
  - template
  - domain-event
  - event-driven
  - ddd
related:
  - ../ex-so-prla-py__domain-driven-design.md
  - ./aggregate-template.md
principles:
  - explicit-over-implicit
last_updated: 2025-01-23
---

# Domain Event Template

## Overview

Domain events capture significant business occurrences. Use events for audit trails, triggering side effects, and decoupling domains. Events are immutable records of things that happened.

**Use this template when**:

- Important state change occurred
- Other parts of system need notification
- Event sourcing or audit trail required
- Decoupling domains

**Examples**: `ZakatCalculated`, `DonationReceived`, `LoanDisbursed`

## Complete Code Template

```python
"""Domain event template."""

from dataclasses import dataclass
from decimal import Decimal
from datetime import datetime
from uuid import uuid4


@dataclass(frozen=True)
class DomainEvent:
    """Base domain event (immutable).

    Attributes:
        event_id: Unique event identifier
        occurred_at: When event occurred
        aggregate_id: ID of aggregate that emitted event
    """

    event_id: str = ""
    occurred_at: datetime = datetime.utcnow()
    aggregate_id: str = ""

    def __post_init__(self) -> None:
        """Set defaults if not provided."""
        if not self.event_id:
            object.__setattr__(self, "event_id", str(uuid4()))
        if not self.occurred_at:
            object.__setattr__(self, "occurred_at", datetime.utcnow())


# ============================================================================
# FINANCIAL DOMAIN EXAMPLES
# ============================================================================


@dataclass(frozen=True)
class ZakatCalculated(DomainEvent):
    """Zakat calculation completed event."""

    payer_id: str = ""
    wealth_amount: Decimal = Decimal("0")
    zakat_amount: Decimal = Decimal("0")


@dataclass(frozen=True)
class DonationReceived(DomainEvent):
    """Donation received event."""

    campaign_id: str = ""
    donor_id: str = ""
    amount: Decimal = Decimal("0")
    message: str = ""


@dataclass(frozen=True)
class LoanDisbursed(DomainEvent):
    """QardHasan loan disbursed event."""

    loan_id: str = ""
    borrower_id: str = ""
    principal: Decimal = Decimal("0")
    disbursement_date: datetime = datetime.utcnow()


# Event Handler Example
class EventBus:
    """Simple event bus for dispatching events."""

    def __init__(self):
        self._handlers = {}

    def subscribe(self, event_type, handler):
        """Subscribe handler to event type."""
        if event_type not in self._handlers:
            self._handlers[event_type] = []
        self._handlers[event_type].append(handler)

    def publish(self, event: DomainEvent):
        """Publish event to all subscribers."""
        event_type = type(event)
        if event_type in self._handlers:
            for handler in self._handlers[event_type]:
                handler(event)


# Usage Example
event_bus = EventBus()


def send_zakat_receipt(event: ZakatCalculated):
    """Handle Zakat calculation by sending receipt."""
    print(f"Sending receipt for Zakat ${event.zakat_amount} to {event.payer_id}")


event_bus.subscribe(ZakatCalculated, send_zakat_receipt)

# Emit event
event = ZakatCalculated(
    payer_id="PAYER-123",
    wealth_amount=Decimal("100000"),
    zakat_amount=Decimal("2500"),
    aggregate_id="ZKT-REC-001",
)

event_bus.publish(event)
```

## Usage Example

```python
# Create and publish events
donation_event = DonationReceived(
    campaign_id="CAMP-001",
    donor_id="DONOR-456",
    amount=Decimal("10000"),
    message="For orphans",
    aggregate_id="CAMP-001",
)

# Event is immutable
print(f"Event ID: {donation_event.event_id}")
print(f"Occurred: {donation_event.occurred_at}")

# Cannot modify (frozen dataclass)
# donation_event.amount = Decimal("20000")  # Error!
```

## Best Practices

### Do: Use Past Tense

```python
# GOOD: Past tense (event already happened)
class DonationReceived(DomainEvent): ...


# BAD: Present tense
class ReceiveDonation(DomainEvent): ...
```

### Do: Keep Events Immutable

```python
# GOOD: Frozen dataclass
@dataclass(frozen=True)
class DonationReceived(DomainEvent): ...
```

## Related Patterns

- **Aggregate**: Aggregates emit events → [aggregate-template.md](./aggregate-template.md)

## References

- [Domain-Driven Design](../ex-so-prla-py__domain-driven-design.md)

---

**Last Updated**: 2025-01-23
**Python Version**: 3.11+ (baseline), 3.12+ (stable maintenance), 3.14.x (latest stable)
**Maintainers**: OSE Platform Documentation Team
