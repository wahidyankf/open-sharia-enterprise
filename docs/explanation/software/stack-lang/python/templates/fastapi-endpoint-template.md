---
title: FastAPI Endpoint Template
description: Copy-paste ready FastAPI endpoint template with Pydantic validation, dependency injection, authentication, and error handling for Sharia-compliant financial APIs
category: explanation
subcategory: stack-lang
tags:
  - python
  - template
  - fastapi
  - api
  - rest
  - pydantic
related:
  - ../ex-so-stla-py__web-services.md
  - ../ex-so-stla-py__type-safety.md
principles:
  - explicit-over-implicit
last_updated: 2025-01-23
---

# FastAPI Endpoint Template

## Overview

FastAPI endpoints with automatic validation, OpenAPI documentation, and dependency injection. Use for building type-safe REST APIs.

**Use this template when**:

- Building REST API
- Need automatic validation
- Want OpenAPI documentation
- Type-safe API development

**Examples**: Zakat calculation API, donation processing API

## Complete Code Template

```python
"""FastAPI endpoint template."""

from fastapi import FastAPI, HTTPException, status, Depends
from fastapi.security import HTTPBearer, HTTPAuthorizationCredentials
from pydantic import BaseModel, Field
from decimal import Decimal
from typing import Annotated


app = FastAPI(title="OSE Platform API", version="1.0.0")
security = HTTPBearer()


# ============================================================================
# REQUEST/RESPONSE MODELS
# ============================================================================


class ZakatCalculationRequest(BaseModel):
    """Zakat calculation request."""

    payer_id: str = Field(min_length=5, max_length=50)
    wealth_amount: Decimal = Field(gt=0, description="Total wealth in USD")
    nisab_threshold: Decimal = Field(gt=0, description="Nisab threshold in USD")


class ZakatCalculationResponse(BaseModel):
    """Zakat calculation response."""

    record_id: str
    payer_id: str
    wealth_amount: Decimal
    zakat_amount: Decimal
    is_obligated: bool


# ============================================================================
# DEPENDENCIES
# ============================================================================


async def verify_token(
    credentials: HTTPAuthorizationCredentials = Depends(security),
) -> str:
    """Verify JWT token and extract user ID."""
    # Implement JWT verification
    # For template: simplified example
    return "user-123"


CurrentUser = Annotated[str, Depends(verify_token)]


# ============================================================================
# ENDPOINTS
# ============================================================================


@app.post(
    "/api/v1/zakat/calculate",
    response_model=ZakatCalculationResponse,
    status_code=status.HTTP_200_OK,
    summary="Calculate Zakat obligation",
    tags=["Zakat"],
)
async def calculate_zakat(
    request: ZakatCalculationRequest,
    user_id: CurrentUser,
) -> ZakatCalculationResponse:
    """Calculate Zakat (2.5% of wealth exceeding nisab).

    Args:
        request: Calculation request with payer ID and wealth info
        user_id: Authenticated user ID from token

    Returns:
        Calculation result with Zakat amount

    Raises:
        HTTPException: If calculation fails
    """
    try:
        # Validate payer access
        if request.payer_id != user_id:
            raise HTTPException(
                status_code=status.HTTP_403_FORBIDDEN,
                detail="Cannot calculate Zakat for other payers",
            )

        # Calculate Zakat
        is_obligated = request.wealth_amount >= request.nisab_threshold
        zakat_amount = (
            request.wealth_amount * Decimal("0.025")
            if is_obligated
            else Decimal("0")
        )

        # Save to repository (simplified)
        record_id = f"ZKT-{uuid4().hex[:8]}"

        return ZakatCalculationResponse(
            record_id=record_id,
            payer_id=request.payer_id,
            wealth_amount=request.wealth_amount,
            zakat_amount=zakat_amount,
            is_obligated=is_obligated,
        )

    except ValueError as e:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST, detail=str(e)
        )


@app.get(
    "/api/v1/zakat/records/{record_id}",
    response_model=ZakatCalculationResponse,
    summary="Get Zakat record",
    tags=["Zakat"],
)
async def get_zakat_record(
    record_id: str,
    user_id: CurrentUser,
) -> ZakatCalculationResponse:
    """Get Zakat record by ID.

    Args:
        record_id: Record identifier
        user_id: Authenticated user ID

    Returns:
        Zakat record

    Raises:
        HTTPException: If record not found or access denied
    """
    # Fetch from repository (simplified)
    record = await fetch_record(record_id)

    if not record:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail=f"Record {record_id} not found",
        )

    # Authorization check
    if record.payer_id != user_id:
        raise HTTPException(
            status_code=status.HTTP_403_FORBIDDEN,
            detail="Access denied",
        )

    return record
```

## Usage Example

```python
# Client usage with httpx
import httpx


async def call_zakat_api():
    """Example API client call."""
    async with httpx.AsyncClient() as client:
        response = await client.post(
            "http://api.oseplatform.com/api/v1/zakat/calculate",
            json={
                "payer_id": "PAYER-123",
                "wealth_amount": "100000.00",
                "nisab_threshold": "85000.00",
            },
            headers={"Authorization": "Bearer YOUR_JWT_TOKEN"},
        )

        data = response.json()
        print(f"Zakat: ${data['zakat_amount']}")
```

## Best Practices

### Do: Use Pydantic Models

```python
# GOOD: Pydantic validates automatically
class ZakatCalculationRequest(BaseModel):
    wealth_amount: Decimal = Field(gt=0)  # Must be positive
```

### Do: Use Dependency Injection

```python
# GOOD: Inject dependencies
async def calculate_zakat(
    request: ZakatCalculationRequest,
    user_id: CurrentUser,  # Injected dependency
):
    ...
```

## References

- [Web Services](../ex-so-stla-py__web-services.md)

---

**Last Updated**: 2025-01-23
**Python Version**: 3.11+ (baseline), 3.12+ (stable maintenance), 3.14.x (latest stable)
**Maintainers**: OSE Platform Documentation Team
