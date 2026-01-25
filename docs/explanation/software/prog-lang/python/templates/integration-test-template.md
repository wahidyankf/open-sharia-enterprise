---
title: Integration Test Template
description: Copy-paste ready pytest integration test template with database fixtures, async tests, and API testing for Python applications in Sharia-compliant financial services
category: explanation
subcategory: prog-lang
tags:
  - python
  - template
  - integration-test
  - pytest
  - testing
  - database
related:
  - ../ex-so-prla-py__test-driven-development.md
  - ./unit-test-template.md
principles:
  - test-first
last_updated: 2025-01-23
---

# Integration Test Template

## Overview

Integration tests verify components working together (database, API, external services). Use real database connections and async fixtures.

**Use this template when**:

- Testing database interactions
- Testing API endpoints
- Testing with external services

**Examples**: Test Zakat record persistence, test donation API endpoint

## Complete Code Template

```python
"""Integration test template with pytest and async."""

import pytest
import pytest_asyncio
from decimal import Decimal
from sqlalchemy.ext.asyncio import create_async_engine, AsyncSession
from sqlalchemy.orm import sessionmaker
from httpx import AsyncClient


# ============================================================================
# DATABASE FIXTURES
# ============================================================================


@pytest_asyncio.fixture
async def db_engine():
    """Create test database engine."""
    engine = create_async_engine("sqlite+aiosqlite:///:memory:")

    # Create tables
    async with engine.begin() as conn:
        await conn.run_sync(Base.metadata.create_all)

    yield engine

    # Cleanup
    await engine.dispose()


@pytest_asyncio.fixture
async def db_session(db_engine):
    """Create test database session."""
    async_session = sessionmaker(
        db_engine, class_=AsyncSession, expire_on_commit=False
    )

    async with async_session() as session:
        yield session


# ============================================================================
# REPOSITORY TESTS
# ============================================================================


@pytest.mark.asyncio
async def test_save_zakat_record(db_session):
    """Test saving Zakat record to database."""
    # Arrange
    repo = ZakatRecordRepository(db_session)
    record = ZakatRecord(
        id="ZKT-001", payer_id="PAYER-123", zakat_amount=Decimal("2500")
    )

    # Act
    await repo.save(record)

    # Assert
    found = await repo.find_by_id("ZKT-001")
    assert found is not None
    assert found.payer_id == "PAYER-123"
    assert found.zakat_amount == Decimal("2500")


@pytest.mark.asyncio
async def test_find_by_payer(db_session):
    """Test finding records by payer ID."""
    # Arrange
    repo = ZakatRecordRepository(db_session)
    record1 = ZakatRecord(id="ZKT-001", payer_id="PAYER-123", zakat_amount=Decimal("2500"))
    record2 = ZakatRecord(id="ZKT-002", payer_id="PAYER-123", zakat_amount=Decimal("3000"))

    await repo.save(record1)
    await repo.save(record2)

    # Act
    records = await repo.find_by_payer("PAYER-123")

    # Assert
    assert len(records) == 2


# ============================================================================
# API TESTS
# ============================================================================


@pytest_asyncio.fixture
async def test_client():
    """Create test HTTP client."""
    async with AsyncClient(app=app, base_url="http://test") as client:
        yield client


@pytest.mark.asyncio
async def test_calculate_zakat_endpoint(test_client):
    """Test Zakat calculation API endpoint."""
    # Arrange
    request_data = {
        "payer_id": "PAYER-123",
        "wealth_amount": "100000.00",
        "nisab_threshold": "85000.00",
    }

    # Act
    response = await test_client.post("/api/v1/zakat/calculate", json=request_data)

    # Assert
    assert response.status_code == 200
    data = response.json()
    assert data["zakat_amount"] == "2500.00"
    assert data["is_obligated"] is True


@pytest.mark.asyncio
async def test_create_donation_endpoint(test_client, db_session):
    """Test donation creation endpoint."""
    # Arrange
    request_data = {
        "campaign_id": "CAMP-001",
        "donor_id": "DONOR-456",
        "amount": "10000.00",
        "message": "For orphans",
    }

    # Act
    response = await test_client.post("/api/v1/donations", json=request_data)

    # Assert
    assert response.status_code == 201
    data = response.json()
    assert "id" in data
    assert data["amount"] == "10000.00"
```

## Best Practices

### Do: Use Test Database

```python
# GOOD: In-memory test database
@pytest_asyncio.fixture
async def db_engine():
    engine = create_async_engine("sqlite+aiosqlite:///:memory:")
    ...
```

### Do: Clean Up After Tests

```python
# GOOD: Cleanup in fixture
@pytest_asyncio.fixture
async def db_engine():
    engine = ...
    yield engine
    await engine.dispose()  # Cleanup
```

## References

- [Test-Driven Development](../ex-so-prla-py__test-driven-development.md)

---

**Last Updated**: 2025-01-23
**Python Version**: 3.11+ (baseline), 3.12+ (stable maintenance), 3.14.x (latest stable)
**Maintainers**: OSE Platform Documentation Team
