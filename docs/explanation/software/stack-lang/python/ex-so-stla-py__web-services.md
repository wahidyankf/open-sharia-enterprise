---
title: Python Web Services
description: Web service development in Python using FastAPI async framework, Django batteries-included framework, Flask minimalist framework, httpx client, REST API design, JWT authentication, and OpenAPI documentation for financial applications
category: explanation
subcategory: stack-lang
tags:
  - python
  - web-services
  - fastapi
  - django
  - flask
  - httpx
  - rest-api
  - authentication
  - openapi
related:
  - ./ex-so-stla-py__security.md
  - ./ex-so-stla-py__concurrency-and-parallelism.md
  - ./ex-so-stla-py__type-safety.md
principles:
  - explicit-over-implicit
  - security-first
last_updated: 2025-01-23
---

# Python Web Services

**Quick Reference**: [Overview](#overview) | [FastAPI](#fastapi-async-framework) | [Django](#django-batteries-included) | [Flask](#flask-minimalist-framework) | [httpx](#httpx-async-http-client) | [REST API Design](#rest-api-design) | [Authentication](#authentication) | [API Versioning](#api-versioning) | [OpenAPI](#openapi-documentation) | [Financial Domain APIs](#financial-domain-api-examples) | [References](#references)

## Overview

Python offers three major web frameworks, each with distinct philosophies. For OSE Platform financial services, framework choice depends on project requirements: FastAPI for modern async APIs, Django for full-featured web apps, Flask for microservices.

```mermaid
graph TB
    subgraph "Framework Selection"
        A[Project Requirements] --> B{Primary Need?}
        B -->|High Performance API| C[FastAPI]
        B -->|Full Web App| D[Django]
        B -->|Microservice| E[Flask]

        C --> F[Async/Await]
        C --> G[Type Safety]
        C --> H[Auto Docs]

        D --> I[Admin Panel]
        D --> J[ORM]
        D --> K[Auth System]

        E --> L[Minimal Core]
        E --> M[Extensions]
        E --> N[Flexibility]
    end

    style C fill:#0173B2
    style D fill:#DE8F05
    style E fill:#029E73
```

### Framework Comparison

| Feature            | FastAPI               | Django             | Flask                   |
| ------------------ | --------------------- | ------------------ | ----------------------- |
| **Philosophy**     | Modern async API      | Batteries-included | Minimalist              |
| **Performance**    | Very High             | Medium             | Medium-High             |
| **Type Safety**    | Built-in (Pydantic)   | Optional           | Optional                |
| **Learning Curve** | Medium                | Steep              | Easy                    |
| **Async Support**  | Native                | Limited            | Via extensions          |
| **Admin Panel**    | No                    | Yes                | No                      |
| **ORM**            | None (use SQLAlchemy) | Django ORM         | None (use SQLAlchemy)   |
| **Best For**       | APIs, microservices   | Full web apps      | Simple APIs, prototypes |

## FastAPI (Async Framework)

FastAPI is a modern async web framework with automatic validation and documentation.

### Installing FastAPI

```bash
# Install FastAPI with dependencies
pip install fastapi[all]

# Or minimal install
pip install fastapi uvicorn[standard]
```

### Basic FastAPI Application

```python
# GOOD: FastAPI with Pydantic validation
from fastapi import FastAPI, HTTPException, status
from pydantic import BaseModel, Field
from decimal import Decimal
from typing import Optional
from datetime import datetime

app = FastAPI(
    title="OSE Platform API",
    description="Sharia-compliant financial services",
    version="1.0.0",
)


class ZakatCalculationRequest(BaseModel):
    """Request model for Zakat calculation."""

    payer_id: str = Field(min_length=5, max_length=50)
    wealth_amount: Decimal = Field(gt=0, description="Total wealth in USD")
    nisab_threshold: Decimal = Field(gt=0, description="Nisab threshold in USD")


class ZakatCalculationResponse(BaseModel):
    """Response model for Zakat calculation."""

    payer_id: str
    wealth_amount: Decimal
    nisab_threshold: Decimal
    zakat_amount: Decimal
    is_obligated: bool
    calculation_date: datetime


@app.post(
    "/api/v1/zakat/calculate",
    response_model=ZakatCalculationResponse,
    status_code=status.HTTP_200_OK,
)
async def calculate_zakat(request: ZakatCalculationRequest) -> ZakatCalculationResponse:
    """Calculate Zakat obligation (2.5% of wealth exceeding nisab)."""
    # Validation automatic via Pydantic
    is_obligated = request.wealth_amount >= request.nisab_threshold

    if is_obligated:
        zakat_amount = request.wealth_amount * Decimal("0.025")
    else:
        zakat_amount = Decimal("0")

    return ZakatCalculationResponse(
        payer_id=request.payer_id,
        wealth_amount=request.wealth_amount,
        nisab_threshold=request.nisab_threshold,
        zakat_amount=zakat_amount,
        is_obligated=is_obligated,
        calculation_date=datetime.utcnow(),
    )
```

**Why this matters**: FastAPI auto-validates requests with Pydantic. Type hints generate OpenAPI docs. Async/await for high concurrency.

### FastAPI Dependency Injection

```python
# GOOD: Dependency injection pattern
from fastapi import Depends
from typing import Annotated
from sqlalchemy.orm import Session
from database import SessionLocal


def get_db() -> Session:
    """Database session dependency."""
    db = SessionLocal()
    try:
        yield db
    finally:
        db.close()


DbSession = Annotated[Session, Depends(get_db)]


@app.get("/api/v1/donations/{campaign_id}")
async def get_campaign_donations(
    campaign_id: str,
    db: DbSession,
) -> list[DonationResponse]:
    """Get all donations for campaign."""
    donations = db.query(Donation).filter(
        Donation.campaign_id == campaign_id
    ).all()

    return [DonationResponse.from_orm(d) for d in donations]
```

**Why this matters**: Dependencies injected automatically. Database sessions managed correctly. Testable (can mock dependencies).

### FastAPI Background Tasks

```python
# GOOD: Background tasks for async operations
from fastapi import BackgroundTasks


async def send_zakat_receipt(payer_id: str, zakat_amount: Decimal) -> None:
    """Send Zakat receipt email (background task)."""
    # Simulate email sending
    await send_email(
        to=get_payer_email(payer_id),
        subject="Zakat Payment Receipt",
        body=f"Your Zakat payment of ${zakat_amount} has been recorded.",
    )


@app.post("/api/v1/zakat/payment")
async def record_zakat_payment(
    payment: ZakatPaymentRequest,
    background_tasks: BackgroundTasks,
) -> ZakatPaymentResponse:
    """Record Zakat payment and send receipt."""
    # Process payment synchronously
    result = await process_payment(payment)

    # Send receipt in background (non-blocking)
    background_tasks.add_task(
        send_zakat_receipt,
        payment.payer_id,
        payment.amount,
    )

    return result
```

**Why this matters**: Background tasks don't block response. Useful for emails, notifications, logging. Fire-and-forget pattern.

### FastAPI Exception Handling

```python
# GOOD: Custom exception handlers
from fastapi import Request
from fastapi.responses import JSONResponse


class InsufficientFundsError(Exception):
    """Raised when account has insufficient funds."""

    def __init__(self, available: Decimal, required: Decimal):
        self.available = available
        self.required = required
        super().__init__(f"Insufficient funds: {available} < {required}")


@app.exception_handler(InsufficientFundsError)
async def insufficient_funds_handler(
    request: Request,
    exc: InsufficientFundsError,
) -> JSONResponse:
    """Handle insufficient funds error."""
    return JSONResponse(
        status_code=status.HTTP_402_PAYMENT_REQUIRED,
        content={
            "error": "insufficient_funds",
            "message": str(exc),
            "available_amount": str(exc.available),
            "required_amount": str(exc.required),
        },
    )


@app.post("/api/v1/donations/process")
async def process_donation(donation: DonationRequest) -> DonationResponse:
    """Process donation payment."""
    balance = await get_account_balance(donation.donor_id)

    if balance < donation.amount:
        raise InsufficientFundsError(balance, donation.amount)

    return await charge_donation(donation)
```

**Why this matters**: Custom exception handlers provide consistent error responses. Centralized error handling. Client-friendly error messages.

## Django (Batteries-Included)

Django provides complete web framework with ORM, admin panel, and authentication.

### Django Project Structure

```
zakat_platform/
├── manage.py
├── zakat_platform/
│   ├── __init__.py
│   ├── settings.py
│   ├── urls.py
│   └── wsgi.py
├── zakat/
│   ├── __init__.py
│   ├── models.py
│   ├── views.py
│   ├── serializers.py
│   ├── urls.py
│   └── admin.py
└── requirements.txt
```

### Django Models

```python
# GOOD: Django ORM models
from django.db import models
from decimal import Decimal


class ZakatRecord(models.Model):
    """Zakat calculation and payment record."""

    payer_id = models.CharField(max_length=50, db_index=True)
    wealth_amount = models.DecimalField(max_digits=15, decimal_places=2)
    nisab_threshold = models.DecimalField(max_digits=15, decimal_places=2)
    zakat_amount = models.DecimalField(max_digits=15, decimal_places=2)
    calculation_date = models.DateTimeField(auto_now_add=True)
    payment_status = models.CharField(
        max_length=20,
        choices=[
            ("pending", "Pending"),
            ("completed", "Completed"),
            ("failed", "Failed"),
        ],
        default="pending",
    )

    class Meta:
        ordering = ["-calculation_date"]
        indexes = [
            models.Index(fields=["payer_id", "-calculation_date"]),
        ]

    def __str__(self) -> str:
        return f"Zakat {self.payer_id}: ${self.zakat_amount}"

    @property
    def is_obligated(self) -> bool:
        """Check if Zakat is obligated."""
        return self.wealth_amount >= self.nisab_threshold
```

**Why this matters**: Django ORM handles migrations, relationships, queries. Admin panel auto-generated. Model validation built-in.

### Django REST Framework

```python
# GOOD: DRF serializers and views
from rest_framework import serializers, viewsets, status
from rest_framework.decorators import action
from rest_framework.response import Response
from decimal import Decimal


class ZakatRecordSerializer(serializers.ModelSerializer):
    """Serializer for Zakat records."""

    is_obligated = serializers.BooleanField(read_only=True)

    class Meta:
        model = ZakatRecord
        fields = [
            "id",
            "payer_id",
            "wealth_amount",
            "nisab_threshold",
            "zakat_amount",
            "is_obligated",
            "calculation_date",
            "payment_status",
        ]
        read_only_fields = ["zakat_amount", "calculation_date"]

    def validate(self, data):
        """Validate wealth and nisab amounts."""
        if data["wealth_amount"] <= 0:
            raise serializers.ValidationError("Wealth must be positive")
        if data["nisab_threshold"] <= 0:
            raise serializers.ValidationError("Nisab must be positive")
        return data

    def create(self, validated_data):
        """Calculate Zakat before creating record."""
        wealth = validated_data["wealth_amount"]
        nisab = validated_data["nisab_threshold"]

        if wealth >= nisab:
            validated_data["zakat_amount"] = wealth * Decimal("0.025")
        else:
            validated_data["zakat_amount"] = Decimal("0")

        return super().create(validated_data)


class ZakatRecordViewSet(viewsets.ModelViewSet):
    """ViewSet for Zakat records."""

    queryset = ZakatRecord.objects.all()
    serializer_class = ZakatRecordSerializer

    @action(detail=False, methods=["post"])
    def calculate(self, request):
        """Calculate Zakat without saving."""
        serializer = self.get_serializer(data=request.data)
        serializer.is_valid(raise_exception=True)

        wealth = serializer.validated_data["wealth_amount"]
        nisab = serializer.validated_data["nisab_threshold"]

        if wealth >= nisab:
            zakat = wealth * Decimal("0.025")
        else:
            zakat = Decimal("0")

        return Response({
            "zakat_amount": zakat,
            "is_obligated": wealth >= nisab,
        })
```

**Why this matters**: DRF provides REST API framework on Django. ViewSets reduce boilerplate. Serializers handle validation and transformation.

### Django Admin Panel

```python
# GOOD: Custom admin interface
from django.contrib import admin
from .models import ZakatRecord


@admin.register(ZakatRecord)
class ZakatRecordAdmin(admin.ModelAdmin):
    """Admin interface for Zakat records."""

    list_display = [
        "payer_id",
        "wealth_amount",
        "zakat_amount",
        "payment_status",
        "calculation_date",
    ]
    list_filter = ["payment_status", "calculation_date"]
    search_fields = ["payer_id"]
    readonly_fields = ["zakat_amount", "calculation_date"]

    fieldsets = [
        (
            "Payer Information",
            {
                "fields": ["payer_id"],
            },
        ),
        (
            "Financial Details",
            {
                "fields": [
                    "wealth_amount",
                    "nisab_threshold",
                    "zakat_amount",
                ],
            },
        ),
        (
            "Status",
            {
                "fields": ["payment_status", "calculation_date"],
            },
        ),
    ]
```

**Why this matters**: Admin panel auto-generated from models. Customizable list views and filters. Quick data management interface.

## Flask (Minimalist Framework)

Flask provides minimal core with extensions for features.

### Basic Flask Application

```python
# GOOD: Flask with Blueprint organization
from flask import Flask, request, jsonify
from flask_sqlalchemy import SQLAlchemy
from pydantic import BaseModel, Field, ValidationError
from decimal import Decimal

app = Flask(__name__)
app.config["SQLALCHEMY_DATABASE_URI"] = "sqlite:///zakat.db"
db = SQLAlchemy(app)


class ZakatCalculationRequest(BaseModel):
    """Request validation with Pydantic."""

    payer_id: str = Field(min_length=5, max_length=50)
    wealth_amount: Decimal = Field(gt=0)
    nisab_threshold: Decimal = Field(gt=0)


@app.route("/api/v1/zakat/calculate", methods=["POST"])
def calculate_zakat():
    """Calculate Zakat endpoint."""
    try:
        # Validate request with Pydantic
        data = ZakatCalculationRequest(**request.json)
    except ValidationError as e:
        return jsonify({"error": "validation_error", "details": e.errors()}), 400

    # Calculate Zakat
    if data.wealth_amount >= data.nisab_threshold:
        zakat_amount = data.wealth_amount * Decimal("0.025")
    else:
        zakat_amount = Decimal("0")

    return jsonify({
        "payer_id": data.payer_id,
        "wealth_amount": str(data.wealth_amount),
        "nisab_threshold": str(data.nisab_threshold),
        "zakat_amount": str(zakat_amount),
        "is_obligated": data.wealth_amount >= data.nisab_threshold,
    })


if __name__ == "__main__":
    app.run(debug=True)
```

**Why this matters**: Flask minimal and flexible. Pydantic adds validation (Flask doesn't have built-in). Explicit routing.

### Flask Blueprints

```python
# GOOD: Flask Blueprints for organization
from flask import Blueprint

zakat_bp = Blueprint("zakat", __name__, url_prefix="/api/v1/zakat")


@zakat_bp.route("/calculate", methods=["POST"])
def calculate():
    """Calculate Zakat."""
    # Implementation here
    pass


@zakat_bp.route("/records", methods=["GET"])
def get_records():
    """Get Zakat records."""
    # Implementation here
    pass


# Register blueprint in main app
app.register_blueprint(zakat_bp)
```

**Why this matters**: Blueprints organize routes into modules. Reusable across apps. Clear URL prefixes.

## httpx (Async HTTP Client)

httpx is async HTTP client for Python (like requests but async).

### Basic httpx Usage

```python
# GOOD: httpx async client
import httpx
from decimal import Decimal
from pydantic import BaseModel


class ExchangeRateResponse(BaseModel):
    """Exchange rate API response."""

    base: str
    date: str
    rates: dict[str, Decimal]


async def get_gold_price_usd() -> Decimal:
    """Fetch current gold price in USD per gram."""
    async with httpx.AsyncClient() as client:
        response = await client.get(
            "https://api.metals.live/v1/spot/gold",
            timeout=10.0,
        )
        response.raise_for_status()

        data = response.json()
        return Decimal(str(data["price"])) / Decimal("31.1035")  # Troy oz to gram


async def calculate_nisab_threshold() -> Decimal:
    """Calculate nisab threshold based on current gold price."""
    gold_price_per_gram = await get_gold_price_usd()
    nisab_grams = Decimal("85")  # 85 grams of gold
    return gold_price_per_gram * nisab_grams
```

**Why this matters**: httpx supports async/await. Timeout configuration prevents hanging. Automatically follows redirects.

### httpx Error Handling

```python
# GOOD: Robust error handling with httpx
from httpx import HTTPStatusError, RequestError, TimeoutException


async def fetch_with_retry(url: str, max_retries: int = 3) -> dict:
    """Fetch URL with retry logic."""
    async with httpx.AsyncClient() as client:
        for attempt in range(max_retries):
            try:
                response = await client.get(url, timeout=10.0)
                response.raise_for_status()
                return response.json()

            except TimeoutException:
                if attempt == max_retries - 1:
                    raise
                await asyncio.sleep(2 ** attempt)  # Exponential backoff

            except HTTPStatusError as e:
                if e.response.status_code >= 500:
                    # Retry server errors
                    if attempt == max_retries - 1:
                        raise
                    await asyncio.sleep(2 ** attempt)
                else:
                    # Don't retry client errors
                    raise

            except RequestError:
                if attempt == max_retries - 1:
                    raise
                await asyncio.sleep(2 ** attempt)
```

**Why this matters**: Exponential backoff for retries. Distinguish server vs client errors. Timeout prevents hanging indefinitely.

## REST API Design

REST API principles for financial services.

### Resource Naming

```python
# GOOD: RESTful resource naming
"""
Correct REST endpoints:

GET    /api/v1/zakat/records           # List all Zakat records
POST   /api/v1/zakat/records           # Create new record
GET    /api/v1/zakat/records/{id}      # Get specific record
PUT    /api/v1/zakat/records/{id}      # Update record
DELETE /api/v1/zakat/records/{id}      # Delete record

GET    /api/v1/campaigns/{id}/donations  # Nested resource
POST   /api/v1/campaigns/{id}/donations  # Create donation

POST   /api/v1/zakat/calculate         # RPC-style action (OK for calculations)
"""


# BAD: Non-RESTful naming
"""
Incorrect endpoints:

GET  /api/v1/getZakatRecords          # BAD: verb in URL
POST /api/v1/createZakat              # BAD: verb in URL
GET  /api/v1/zakat_records            # BAD: snake_case in URL
GET  /api/v1/ZakatRecords             # BAD: PascalCase in URL
"""
```

### HTTP Status Codes

```python
# GOOD: Proper HTTP status codes
from fastapi import status

@app.post(
    "/api/v1/donations",
    status_code=status.HTTP_201_CREATED,  # Created
)
async def create_donation(donation: DonationRequest) -> DonationResponse:
    """Create donation."""
    pass


@app.get(
    "/api/v1/donations/{donation_id}",
    status_code=status.HTTP_200_OK,  # Success
)
async def get_donation(donation_id: str) -> DonationResponse:
    """Get donation."""
    donation = await fetch_donation(donation_id)
    if not donation:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND)
    return donation


@app.put(
    "/api/v1/donations/{donation_id}",
    status_code=status.HTTP_200_OK,  # Success
)
async def update_donation(
    donation_id: str,
    donation: DonationRequest,
) -> DonationResponse:
    """Update donation."""
    pass


@app.delete(
    "/api/v1/donations/{donation_id}",
    status_code=status.HTTP_204_NO_CONTENT,  # No content
)
async def delete_donation(donation_id: str) -> None:
    """Delete donation."""
    pass
```

**Why this matters**: Correct status codes communicate intent. 2xx for success, 4xx for client errors, 5xx for server errors.

### Pagination

```python
# GOOD: Cursor-based pagination
from fastapi import Query
from typing import Optional


class PaginatedResponse(BaseModel):
    """Paginated response model."""

    items: list[DonationResponse]
    next_cursor: Optional[str] = None
    has_more: bool


@app.get("/api/v1/donations")
async def list_donations(
    limit: int = Query(default=20, le=100),
    cursor: Optional[str] = None,
) -> PaginatedResponse:
    """List donations with cursor pagination."""
    # Fetch limit + 1 to check if more exist
    donations = await fetch_donations(cursor=cursor, limit=limit + 1)

    has_more = len(donations) > limit
    items = donations[:limit]

    next_cursor = None
    if has_more:
        next_cursor = encode_cursor(items[-1].id)

    return PaginatedResponse(
        items=items,
        next_cursor=next_cursor,
        has_more=has_more,
    )
```

**Why this matters**: Cursor pagination scales better than offset. Handles inserts/deletes during pagination. More efficient for large datasets.

## Authentication

JWT authentication for stateless APIs.

### JWT Token Generation

```python
# GOOD: JWT authentication
from fastapi import Depends, HTTPException, status
from fastapi.security import HTTPBearer, HTTPAuthorizationCredentials
from jose import JWTError, jwt
from datetime import datetime, timedelta
from passlib.context import CryptContext

SECRET_KEY = "your-secret-key-from-env"
ALGORITHM = "HS256"
ACCESS_TOKEN_EXPIRE_MINUTES = 30

security = HTTPBearer()
pwd_context = CryptContext(schemes=["bcrypt"], deprecated="auto")


def create_access_token(user_id: str) -> str:
    """Create JWT access token."""
    expire = datetime.utcnow() + timedelta(minutes=ACCESS_TOKEN_EXPIRE_MINUTES)
    to_encode = {"sub": user_id, "exp": expire}
    return jwt.encode(to_encode, SECRET_KEY, algorithm=ALGORITHM)


def verify_password(plain_password: str, hashed_password: str) -> bool:
    """Verify password against hash."""
    return pwd_context.verify(plain_password, hashed_password)


def hash_password(password: str) -> str:
    """Hash password with bcrypt."""
    return pwd_context.hash(password)


async def get_current_user(
    credentials: HTTPAuthorizationCredentials = Depends(security),
) -> str:
    """Extract user ID from JWT token."""
    try:
        payload = jwt.decode(
            credentials.credentials,
            SECRET_KEY,
            algorithms=[ALGORITHM],
        )
        user_id: str = payload.get("sub")
        if user_id is None:
            raise HTTPException(status_code=status.HTTP_401_UNAUTHORIZED)
        return user_id
    except JWTError:
        raise HTTPException(status_code=status.HTTP_401_UNAUTHORIZED)
```

### Protected Endpoints

```python
# GOOD: Protected endpoints with dependency injection
from typing import Annotated

CurrentUser = Annotated[str, Depends(get_current_user)]


@app.post("/api/v1/auth/login")
async def login(credentials: LoginRequest) -> TokenResponse:
    """Login and get access token."""
    user = await authenticate_user(credentials.username, credentials.password)
    if not user:
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Invalid credentials",
        )

    access_token = create_access_token(user.id)
    return TokenResponse(access_token=access_token, token_type="bearer")


@app.get("/api/v1/zakat/my-records")
async def get_my_zakat_records(user_id: CurrentUser) -> list[ZakatRecordResponse]:
    """Get authenticated user's Zakat records."""
    records = await fetch_user_zakat_records(user_id)
    return records


@app.post("/api/v1/donations")
async def create_donation(
    donation: DonationRequest,
    user_id: CurrentUser,
) -> DonationResponse:
    """Create donation (authenticated)."""
    return await process_donation(donation, donor_id=user_id)
```

**Why this matters**: JWT enables stateless authentication. No session storage needed. Tokens self-contained. Dependency injection makes testing easy.

## API Versioning

Version APIs for backward compatibility.

### URL Versioning

```python
# GOOD: URL-based versioning (recommended)
from fastapi import APIRouter

# Version 1 API
v1_router = APIRouter(prefix="/api/v1")


@v1_router.post("/zakat/calculate")
async def calculate_zakat_v1(request: ZakatRequestV1) -> ZakatResponseV1:
    """V1: Basic Zakat calculation."""
    # Simplified calculation
    pass


# Version 2 API (with breaking changes)
v2_router = APIRouter(prefix="/api/v2")


@v2_router.post("/zakat/calculate")
async def calculate_zakat_v2(request: ZakatRequestV2) -> ZakatResponseV2:
    """V2: Enhanced Zakat calculation with nisab auto-detection."""
    # Enhanced calculation with gold price lookup
    pass


# Register both versions
app.include_router(v1_router)
app.include_router(v2_router)
```

```mermaid
graph LR
    A[Client] --> B{API Version?}
    B -->|/api/v1/zakat/calculate| C[V1 Handler<br/>Basic Calculation]
    B -->|/api/v2/zakat/calculate| D[V2 Handler<br/>Enhanced Calculation]

    C --> E[V1 Response]
    D --> F[V2 Response]

    style C fill:#0173B2
    style D fill:#DE8F05
```

**Why this matters**: URL versioning explicit and clear. Multiple versions run simultaneously. Clients upgrade at their own pace.

## OpenAPI Documentation

FastAPI auto-generates OpenAPI (Swagger) documentation.

### Customizing OpenAPI

```python
# GOOD: Custom OpenAPI metadata
from fastapi import FastAPI
from fastapi.openapi.utils import get_openapi


def custom_openapi():
    """Customize OpenAPI schema."""
    if app.openapi_schema:
        return app.openapi_schema

    openapi_schema = get_openapi(
        title="OSE Platform API",
        version="1.0.0",
        description="""
        ## Sharia-Compliant Financial Services API

        This API provides endpoints for:
        - Zakat calculation and tracking
        - Donation campaign management
        - QardHasan (interest-free) loan processing
        - Murabaha contract management

        ### Authentication
        All endpoints require Bearer token authentication except `/auth/login`.

        ### Rate Limits
        - 100 requests per minute per user
        - 1000 requests per hour per user
        """,
        routes=app.routes,
    )

    # Add custom fields
    openapi_schema["info"]["x-logo"] = {
        "url": "https://oseplatform.com/logo.png"
    }

    app.openapi_schema = openapi_schema
    return app.openapi_schema


app = FastAPI()
app.openapi = custom_openapi
```

### Documenting Endpoints

````python
# GOOD: Comprehensive endpoint documentation
@app.post(
    "/api/v1/zakat/calculate",
    response_model=ZakatResponse,
    status_code=status.HTTP_200_OK,
    summary="Calculate Zakat obligation",
    description="""
    Calculate Zakat (Islamic alms) based on wealth and nisab threshold.

    Zakat is calculated as 2.5% of qualifying wealth that exceeds the nisab.
    The nisab threshold is equivalent to 85 grams of gold or 595 grams of silver.

    **Business Rules:**
    - Wealth must exceed nisab threshold for Zakat to be obligated
    - Zakat rate is fixed at 2.5% (1/40th of wealth)
    - Calculation uses Gregorian calendar year

    **Example Request:**
    ```json
    {
        "payer_id": "PAYER-12345",
        "wealth_amount": 100000.00,
        "nisab_threshold": 85000.00
    }
    ```

    **Example Response:**
    ```json
    {
        "payer_id": "PAYER-12345",
        "wealth_amount": 100000.00,
        "nisab_threshold": 85000.00,
        "zakat_amount": 2500.00,
        "is_obligated": true
    }
    ```
    """,
    response_description="Zakat calculation result with obligation status",
    tags=["Zakat"],
)
async def calculate_zakat(request: ZakatRequest) -> ZakatResponse:
    """Calculate Zakat obligation."""
    pass
````

**Why this matters**: Good documentation reduces support burden. Examples show correct usage. Auto-generated from code (stays in sync).

## Financial Domain API Examples

### Complete Donation Campaign API

```python
# GOOD: Complete donation campaign API
from enum import Enum
from datetime import date


class CampaignStatus(str, Enum):
    """Campaign status enumeration."""

    DRAFT = "draft"
    ACTIVE = "active"
    COMPLETED = "completed"
    CANCELLED = "cancelled"


class CampaignCreateRequest(BaseModel):
    """Request to create campaign."""

    name: str = Field(min_length=5, max_length=200)
    description: str
    target_amount: Decimal = Field(gt=0)
    start_date: date
    end_date: date


class CampaignResponse(BaseModel):
    """Campaign response model."""

    id: str
    name: str
    description: str
    target_amount: Decimal
    current_amount: Decimal
    donor_count: int
    status: CampaignStatus
    start_date: date
    end_date: date
    progress_percentage: Decimal


class DonationCreateRequest(BaseModel):
    """Request to create donation."""

    campaign_id: str
    donor_id: str
    amount: Decimal = Field(gt=0)
    message: Optional[str] = None


# Campaign endpoints
@app.post("/api/v1/campaigns", status_code=status.HTTP_201_CREATED)
async def create_campaign(
    campaign: CampaignCreateRequest,
    user_id: CurrentUser,
) -> CampaignResponse:
    """Create new donation campaign."""
    if campaign.end_date <= campaign.start_date:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="End date must be after start date",
        )

    # Create campaign
    campaign_record = await save_campaign(campaign, created_by=user_id)
    return campaign_record


@app.get("/api/v1/campaigns/{campaign_id}")
async def get_campaign(campaign_id: str) -> CampaignResponse:
    """Get campaign details."""
    campaign = await fetch_campaign(campaign_id)
    if not campaign:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND)
    return campaign


@app.get("/api/v1/campaigns")
async def list_campaigns(
    status: Optional[CampaignStatus] = None,
    limit: int = Query(default=20, le=100),
) -> list[CampaignResponse]:
    """List donation campaigns."""
    return await fetch_campaigns(status=status, limit=limit)


# Donation endpoints
@app.post("/api/v1/donations", status_code=status.HTTP_201_CREATED)
async def create_donation(
    donation: DonationCreateRequest,
    user_id: CurrentUser,
) -> DonationResponse:
    """Create donation to campaign."""
    campaign = await fetch_campaign(donation.campaign_id)
    if not campaign:
        raise HTTPException(
            status_code=status.HTTP_404_NOT_FOUND,
            detail="Campaign not found",
        )

    if campaign.status != CampaignStatus.ACTIVE:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Campaign is not active",
        )

    # Process donation
    donation_record = await process_donation(donation, user_id=user_id)
    return donation_record


@app.get("/api/v1/campaigns/{campaign_id}/donations")
async def get_campaign_donations(
    campaign_id: str,
    limit: int = Query(default=20, le=100),
) -> list[DonationResponse]:
    """Get donations for campaign."""
    return await fetch_campaign_donations(campaign_id, limit=limit)
```

```mermaid
sequenceDiagram
    participant Client
    participant API
    participant Auth
    participant DB
    participant PaymentGateway

    Client->>API: POST /api/v1/donations
    API->>Auth: Verify JWT Token
    Auth-->>API: User ID

    API->>DB: Fetch Campaign
    DB-->>API: Campaign Data

    alt Campaign Not Active
        API-->>Client: 400 Bad Request
    else Campaign Active
        API->>PaymentGateway: Process Payment
        PaymentGateway-->>API: Payment Success

        API->>DB: Save Donation
        API->>DB: Update Campaign Total
        DB-->>API: Donation Record

        API-->>Client: 201 Created
    end
```

### QardHasan Loan API

```python
# GOOD: Interest-free loan (QardHasan) API
class LoanStatus(str, Enum):
    """Loan status enumeration."""

    PENDING = "pending"
    APPROVED = "approved"
    ACTIVE = "active"
    FULLY_REPAID = "fully_repaid"
    DEFAULTED = "defaulted"


class QardHasanLoanRequest(BaseModel):
    """Request for QardHasan loan."""

    borrower_id: str
    principal_amount: Decimal = Field(gt=0)
    purpose: str
    repayment_months: int = Field(gt=0, le=60)


class LoanRepaymentRequest(BaseModel):
    """Loan repayment request."""

    loan_id: str
    payment_amount: Decimal = Field(gt=0)


class QardHasanLoanResponse(BaseModel):
    """QardHasan loan response."""

    id: str
    borrower_id: str
    principal_amount: Decimal
    repaid_amount: Decimal
    remaining_balance: Decimal
    status: LoanStatus
    disbursement_date: Optional[date] = None
    repayment_months: int


@app.post("/api/v1/qard-hasan/loans", status_code=status.HTTP_201_CREATED)
async def create_loan_request(
    loan: QardHasanLoanRequest,
    user_id: CurrentUser,
) -> QardHasanLoanResponse:
    """Create QardHasan (interest-free) loan request."""
    # Validate: No interest allowed in QardHasan
    loan_record = await save_loan_request(loan, requester_id=user_id)
    return loan_record


@app.post("/api/v1/qard-hasan/loans/{loan_id}/repayment")
async def record_repayment(
    loan_id: str,
    repayment: LoanRepaymentRequest,
    user_id: CurrentUser,
) -> QardHasanLoanResponse:
    """Record loan repayment (principal only, no interest)."""
    loan = await fetch_loan(loan_id)
    if not loan:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND)

    if loan.status != LoanStatus.ACTIVE:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="Loan is not active",
        )

    # Record payment (principal only)
    updated_loan = await record_loan_payment(loan_id, repayment.payment_amount)

    # Mark as fully repaid if balance is zero
    if updated_loan.remaining_balance == 0:
        updated_loan = await update_loan_status(loan_id, LoanStatus.FULLY_REPAID)

    return updated_loan


@app.get("/api/v1/qard-hasan/loans/{loan_id}")
async def get_loan(loan_id: str, user_id: CurrentUser) -> QardHasanLoanResponse:
    """Get loan details."""
    loan = await fetch_loan(loan_id)
    if not loan:
        raise HTTPException(status_code=status.HTTP_404_NOT_FOUND)

    # Authorization: only borrower or admin can view
    if loan.borrower_id != user_id and not await is_admin(user_id):
        raise HTTPException(status_code=status.HTTP_403_FORBIDDEN)

    return loan
```

**Why this matters**: Financial domain APIs require precise validation. Decimal for money (never float). Status enums for type safety. Authorization checks for sensitive data.

## References

### Official Documentation

- [FastAPI Documentation](https://fastapi.tiangolo.com/)
- [Django Documentation](https://docs.djangoproject.com/)
- [Flask Documentation](https://flask.palletsprojects.com/)
- [httpx Documentation](https://www.python-httpx.org/)
- [Pydantic Documentation](https://docs.pydantic.dev/)
- [Django REST Framework](https://www.django-rest-framework.org/)

### Related Documentation

- [Security](./ex-so-stla-py__security.md) - Authentication and security
- [Concurrency and Parallelism](./ex-so-stla-py__concurrency-and-parallelism.md) - Async patterns
- [Type Safety](./ex-so-stla-py__type-safety.md) - Type hints and validation

---

**Last Updated**: 2025-01-23
**Python Version**: 3.11+ (baseline), 3.12+ (stable maintenance), 3.13.x (latest stable)
**Maintainers**: OSE Platform Documentation Team
