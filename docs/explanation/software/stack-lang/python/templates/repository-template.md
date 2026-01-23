---
title: Repository Template
description: Copy-paste ready Python repository template with async database access, SQLAlchemy ORM, and persistence patterns for domain-driven design in Sharia-compliant financial applications
category: explanation
subcategory: stack-lang
tags:
  - python
  - template
  - repository
  - persistence
  - sqlalchemy
  - async
related:
  - ../ex-so-stla-py__domain-driven-design.md
  - ./entity-template.md
  - ./aggregate-template.md
principles:
  - explicit-over-implicit
last_updated: 2025-01-23
---

# Repository Template

## Overview

Repositories abstract persistence layer, providing collection-like interface for aggregates/entities. Use repositories to hide database details and enable testing with mocks.

**Use this template when**:

- Need data access abstraction
- Want to hide persistence implementation
- Testing requires mocking data access

**Examples**: `ZakatRecordRepository`, `CampaignRepository`

## Complete Code Template

```python
"""Repository template with async SQLAlchemy."""

from abc import ABC, abstractmethod
from typing import List, Optional
from decimal import Decimal
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy import select
from sqlalchemy.orm import DeclarativeBase, Mapped, mapped_column


# Domain Model (Entity)
class ZakatRecord:
    """Domain entity."""

    def __init__(self, id: str, payer_id: str, zakat_amount: Decimal):
        self.id = id
        self.payer_id = payer_id
        self.zakat_amount = zakat_amount


# SQLAlchemy ORM Model
class Base(DeclarativeBase):
    pass


class ZakatRecordModel(Base):
    """SQLAlchemy ORM model."""

    __tablename__ = "zakat_records"

    id: Mapped[str] = mapped_column(primary_key=True)
    payer_id: Mapped[str]
    wealth_amount: Mapped[Decimal]
    zakat_amount: Mapped[Decimal]
    payment_status: Mapped[str]


# Repository Interface
class IZakatRecordRepository(ABC):
    """Repository interface."""

    @abstractmethod
    async def save(self, record: ZakatRecord) -> None:
        pass

    @abstractmethod
    async def find_by_id(self, record_id: str) -> Optional[ZakatRecord]:
        pass

    @abstractmethod
    async def find_by_payer(self, payer_id: str) -> List[ZakatRecord]:
        pass

    @abstractmethod
    async def delete(self, record_id: str) -> None:
        pass


# Repository Implementation
class ZakatRecordRepository(IZakatRecordRepository):
    """SQLAlchemy repository implementation."""

    def __init__(self, session: AsyncSession):
        self._session = session

    async def save(self, record: ZakatRecord) -> None:
        """Save or update record."""
        model = await self._session.get(ZakatRecordModel, record.id)

        if model is None:
            # Create new
            model = ZakatRecordModel(
                id=record.id,
                payer_id=record.payer_id,
                wealth_amount=record.wealth_amount,
                zakat_amount=record.zakat_amount,
                payment_status=record.payment_status,
            )
            self._session.add(model)
        else:
            # Update existing
            model.wealth_amount = record.wealth_amount
            model.zakat_amount = record.zakat_amount
            model.payment_status = record.payment_status

        await self._session.commit()

    async def find_by_id(self, record_id: str) -> Optional[ZakatRecord]:
        """Find record by ID."""
        model = await self._session.get(ZakatRecordModel, record_id)

        if model is None:
            return None

        return self._to_entity(model)

    async def find_by_payer(self, payer_id: str) -> List[ZakatRecord]:
        """Find all records for payer."""
        stmt = select(ZakatRecordModel).where(ZakatRecordModel.payer_id == payer_id)
        result = await self._session.execute(stmt)
        models = result.scalars().all()

        return [self._to_entity(model) for model in models]

    async def delete(self, record_id: str) -> None:
        """Delete record by ID."""
        model = await self._session.get(ZakatRecordModel, record_id)
        if model:
            await self._session.delete(model)
            await self._session.commit()

    def _to_entity(self, model: ZakatRecordModel) -> ZakatRecord:
        """Convert ORM model to domain entity."""
        return ZakatRecord(
            id=model.id, payer_id=model.payer_id, zakat_amount=model.zakat_amount
        )
```

## Usage Example

```python
from sqlalchemy.ext.asyncio import create_async_engine, AsyncSession
from sqlalchemy.orm import sessionmaker

# Setup database
engine = create_async_engine("sqlite+aiosqlite:///zakat.db")
async_session = sessionmaker(engine, class_=AsyncSession, expire_on_commit=False)


async def example():
    async with async_session() as session:
        repo = ZakatRecordRepository(session)

        # Save record
        record = ZakatRecord(id="ZKT-001", payer_id="PAYER-123", zakat_amount=Decimal("2500"))
        await repo.save(record)

        # Find by ID
        found = await repo.find_by_id("ZKT-001")
        print(f"Found: {found.payer_id}")

        # Find by payer
        payer_records = await repo.find_by_payer("PAYER-123")
        print(f"Records: {len(payer_records)}")
```

## Best Practices

### Do: Use Async for I/O

```python
# GOOD: Async repository methods
async def save(self, record: ZakatRecord) -> None:
    await self._session.commit()
```

### Do: Separate Domain and Persistence

```python
# GOOD: Domain entity separate from ORM model
class ZakatRecord:  # Domain
    pass


class ZakatRecordModel(Base):  # ORM
    pass
```

## Related Patterns

- **Entity**: Repositories persist entities → [entity-template.md](./entity-template.md)
- **Aggregate**: Repositories persist aggregates → [aggregate-template.md](./aggregate-template.md)

## References

- [Domain-Driven Design](../ex-so-stla-py__domain-driven-design.md)

---

**Last Updated**: 2025-01-23
**Python Version**: 3.11+ (baseline), 3.12+ (stable maintenance), 3.13.x (latest stable)
**Maintainers**: OSE Platform Documentation Team
