---
title: Service Layer Template
description: Copy-paste ready Python service layer template with transaction management, orchestration, and application logic for domain-driven design in Sharia-compliant financial applications
category: explanation
subcategory: stack-lang
tags:
  - python
  - template
  - service-layer
  - application-service
  - orchestration
related:
  - ../ex-so-stla-py__domain-driven-design.md
  - ./repository-template.md
principles:
  - explicit-over-implicit
last_updated: 2025-01-23
---

# Service Layer Template

## Overview

Service layer coordinates application logic, orchestrates domain operations, and manages transactions. Use for operations spanning multiple aggregates or requiring external services.

**Use this template when**:

- Operation involves multiple aggregates
- Need transaction management
- Coordinating external services
- Application workflow orchestration

**Examples**: `ZakatCalculationService`, `DonationProcessingService`

## Complete Code Template

```python
"""Service layer template."""

from decimal import Decimal
from typing import Optional
from dataclasses import dataclass


@dataclass
class ZakatCalculationRequest:
    """Service request DTO."""

    payer_id: str
    wealth_amount: Decimal
    nisab_threshold: Decimal


@dataclass
class ZakatCalculationResult:
    """Service result DTO."""

    record_id: str
    payer_id: str
    zakat_amount: Decimal
    is_obligated: bool


class ZakatCalculationService:
    """Application service for Zakat calculation.

    Orchestrates domain logic and persistence.
    """

    def __init__(self, repository, event_bus):
        self._repository = repository
        self._event_bus = event_bus

    async def calculate_zakat(
        self, request: ZakatCalculationRequest
    ) -> ZakatCalculationResult:
        """Calculate and save Zakat.

        Args:
            request: Calculation request

        Returns:
            Calculation result

        Raises:
            ValueError: If validation fails
        """
        # Validate
        if request.wealth_amount < 0:
            raise ValueError("Wealth cannot be negative")

        # Domain logic
        is_obligated = request.wealth_amount >= request.nisab_threshold
        zakat_amount = (
            request.wealth_amount * Decimal("0.025") if is_obligated else Decimal("0")
        )

        # Create entity
        record = ZakatRecord(
            id=str(uuid4()),
            payer_id=request.payer_id,
            wealth_amount=request.wealth_amount,
            zakat_amount=zakat_amount,
        )

        # Persist
        await self._repository.save(record)

        # Emit event
        event = ZakatCalculated(
            payer_id=request.payer_id,
            wealth_amount=request.wealth_amount,
            zakat_amount=zakat_amount,
        )
        self._event_bus.publish(event)

        # Return result
        return ZakatCalculationResult(
            record_id=record.id,
            payer_id=record.payer_id,
            zakat_amount=zakat_amount,
            is_obligated=is_obligated,
        )
```

## Usage Example

```python
async def use_service_example(session):
    """Example service usage."""
    # Setup service
    repository = ZakatRecordRepository(session)
    event_bus = EventBus()
    service = ZakatCalculationService(repository, event_bus)

    # Use service
    request = ZakatCalculationRequest(
        payer_id="PAYER-123",
        wealth_amount=Decimal("100000"),
        nisab_threshold=Decimal("85000"),
    )

    result = await service.calculate_zakat(request)
    print(f"Zakat: ${result.zakat_amount}")
```

## Best Practices

### Do: Use DTOs for Input/Output

```python
# GOOD: Service uses DTOs (not domain entities directly)
@dataclass
class ZakatCalculationRequest:  # DTO
    payer_id: str
    wealth_amount: Decimal


async def calculate_zakat(self, request: ZakatCalculationRequest):
    ...
```

### Do: Coordinate Transactions

```python
# GOOD: Service manages transaction
async def calculate_zakat(self, request):
    async with self._session.begin():  # Transaction
        await self._repository.save(record)
        self._event_bus.publish(event)
```

## References

- [Domain-Driven Design](../ex-so-stla-py__domain-driven-design.md)

---

**Last Updated**: 2025-01-23
**Python Version**: 3.11+ (baseline), 3.12+ (stable maintenance), 3.14.x (latest stable)
**Maintainers**: OSE Platform Documentation Team
