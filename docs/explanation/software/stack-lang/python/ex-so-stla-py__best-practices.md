---
title: Python Best Practices
description: Comprehensive guide to Python coding standards including PEP 8 style, PEP 20 philosophy, type hints, code organization, and Sharia-compliant financial domain patterns
category: explanation
subcategory: stack-lang
tags:
  - python
  - best-practices
  - pep-8
  - pep-20
  - code-quality
  - type-hints
  - documentation
related:
  - ./ex-so-stla-py__idioms.md
  - ./ex-so-stla-py__type-safety.md
  - ./ex-so-stla-py__error-handling.md
  - ./ex-so-stla-py__linting-and-formatting.md
  - ./ex-so-stla-py__test-driven-development.md
principles:
  - simplicity-over-complexity
  - explicit-over-implicit
  - documentation-first
  - accessibility-first
last_updated: 2025-01-23
---

# Python Best Practices

**Quick Reference**: [Overview](#overview) | [PEP 8 Style](#pep-8-style-guide) | [PEP 20 Philosophy](#pep-20-zen-of-python) | [Type Hints](#type-hints-and-annotations) | [Code Organization](#code-organization) | [Error Handling](#error-handling-patterns) | [Testing](#testing-practices) | [Documentation](#documentation-standards) | [Security](#security-practices) | [Anti-Patterns](#anti-patterns) | [References](#references)

## Overview

Python best practices establish coding standards ensuring consistency, maintainability, and production quality across the Open Sharia Enterprise platform. These practices build on Python Enhancement Proposals (PEPs), community conventions, and platform-specific requirements for financial domain applications.

### Why Best Practices Matter

**Consistency**: Uniform code style reduces cognitive load and review friction.

**Maintainability**: Clear patterns enable easier updates and refactoring.

**Quality**: Automated tools (Ruff, Black, mypy) enforce standards programmatically.

**Safety**: Type hints and validation prevent runtime errors in financial calculations.

**Collaboration**: Shared conventions enable efficient team development.

## PEP 8 Style Guide

[PEP 8](https://peps.python.org/pep-0008/) defines Python's official style guide. The platform enforces PEP 8 via Ruff linter.

### Naming Conventions

```python
# GOOD: Follow PEP 8 naming conventions
from decimal import Decimal
from datetime import date


class ZakatCalculator:
    """PEP 8: Classes use CapWords (PascalCase)."""

    NISAB_GOLD_GRAMS = 85  # PEP 8: Constants use UPPER_CASE_WITH_UNDERSCORES

    def __init__(self, zakat_rate: Decimal = Decimal("0.025")) -> None:
        # PEP 8: Functions and variables use snake_case
        self._zakat_rate = zakat_rate  # PEP 8: Private attributes start with single underscore

    def calculate_obligation(self, wealth_amount: Decimal) -> Decimal:
        """PEP 8: Methods use snake_case with descriptive names."""
        return wealth_amount * self._zakat_rate


# BAD: Violates PEP 8 naming conventions
class zakatCalculator:  # BAD: Class name should be CapWords
    nisabGoldGrams = 85  # BAD: Constant should be UPPER_CASE

    def CalculateObligation(self, WealthAmount):  # BAD: Method should be snake_case
        return WealthAmount * 0.025
```

**Why this matters**: Consistent naming enables developers to recognize types at a glance. Classes use CapWords, functions use snake_case, constants use UPPER_CASE. The naming convention itself documents intent.

### Indentation and Whitespace

```python
# GOOD: PEP 8 indentation (4 spaces)
from pydantic import BaseModel, Field
from typing import Optional


class DonationCampaign(BaseModel):
    """PEP 8: Use 4 spaces for indentation."""

    campaign_id: str
    target_amount: Decimal
    current_amount: Decimal = Field(default=Decimal("0"))

    def add_donation(self, amount: Decimal) -> None:
        """PEP 8: Blank line after method docstring."""
        if amount <= 0:
            raise ValueError("Donation must be positive")

        self.current_amount += amount  # PEP 8: No whitespace before operators

    def is_target_met(self) -> bool:
        """Check if campaign reached target."""
        return self.current_amount >= self.target_amount


# BAD: Inconsistent indentation and whitespace
class DonationCampaign:
   def add_donation(self,amount):  # BAD: 3-space indent, missing spaces after comma
       if amount<=0:  # BAD: No spaces around operators
           raise ValueError("Donation must be positive")
       self.current_amount+=amount  # BAD: No spaces around +=
```

**Why this matters**: Consistent indentation (4 spaces) aligns with Python community standards. Spaces around operators improve readability. Blank lines separate logical sections.

### Line Length and Breaking

```python
# GOOD: PEP 8 line length (≤88 characters for Black compatibility)
from typing import Protocol


class PaymentProcessor(Protocol):
    """Process various payment types for Islamic finance transactions."""

    def process_zakat_payment(
        self,
        payer_id: str,
        amount: Decimal,
        payment_method: str,
    ) -> str:
        """Process Zakat payment and return transaction ID.

        Args:
            payer_id: Unique identifier for the payer
            amount: Zakat amount in base currency
            payment_method: Payment method (card, bank, cash)

        Returns:
            Transaction ID for the processed payment
        """
        ...


# GOOD: Break long expressions naturally
total_zakat = (
    calculate_wealth_zakat(gold_holdings, gold_nisab)
    + calculate_wealth_zakat(silver_holdings, silver_nisab)
    + calculate_business_zakat(business_assets, business_liabilities)
)

# BAD: Overly long line (exceeds 88 characters)
total_zakat = calculate_wealth_zakat(gold_holdings, gold_nisab) + calculate_wealth_zakat(silver_holdings, silver_nisab) + calculate_business_zakat(business_assets, business_liabilities)
```

**Why this matters**: Line length limits (79-88 characters) enable side-by-side code review. Black formatter standardizes at 88 characters. Natural breaks at operators and commas improve readability.

### Import Organization

```python
# GOOD: PEP 8 import order (use isort for automatic sorting)
# 1. Standard library imports
import sys
from datetime import date, datetime
from decimal import Decimal
from typing import Optional, Protocol

# 2. Third-party imports
from pydantic import BaseModel, Field, field_validator
import httpx

# 3. Local application imports
from ose_platform.domain.zakat import ZakatCalculator
from ose_platform.domain.donation import DonationCampaign


# BAD: Disorganized imports
from ose_platform.domain.zakat import ZakatCalculator  # BAD: Mix of orders
from decimal import Decimal
import httpx
from datetime import date
import sys
from pydantic import BaseModel
```

**Why this matters**: Organized imports (standard library → third-party → local) improve navigation. isort automates this ordering. Alphabetical sorting within groups aids findability.

## PEP 20 Zen of Python

[PEP 20](https://peps.python.org/pep-0020/) defines Python's guiding philosophy (`import this`):

### Explicit Over Implicit

```python
# GOOD: Explicit type hints and clear intent
from decimal import Decimal
from typing import Optional


def calculate_murabaha_profit(
    asset_cost: Decimal,
    profit_margin_rate: Decimal,
) -> Decimal:
    """Calculate Murabaha profit explicitly.

    Args:
        asset_cost: Original cost of the asset
        profit_margin_rate: Profit margin as decimal (0.15 = 15%)

    Returns:
        Calculated profit amount
    """
    return asset_cost * profit_margin_rate


# BAD: Implicit types and unclear intent
def calc_profit(cost, rate):  # BAD: No type hints, unclear what rate represents
    return cost * rate  # BAD: Is this profit, markup, or something else?
```

**Why this matters**: Explicit type hints document expectations. Clear names prevent ambiguity. Docstrings explain business context. Readers understand intent without guessing.

### Simple Over Complex

```python
# GOOD: Simple, direct implementation
from dataclasses import dataclass
from decimal import Decimal


@dataclass
class QardHasanLoan:
    """Simple interest-free loan model."""

    loan_id: str
    principal: Decimal
    repaid: Decimal = Decimal("0")

    @property
    def remaining(self) -> Decimal:
        """Calculate remaining balance."""
        return self.principal - self.repaid


# BAD: Unnecessary complexity
class QardHasanLoanComplex:
    """Over-engineered loan model."""

    def __init__(self, loan_id: str, principal: Decimal):
        self._data = {"id": loan_id, "principal": principal, "repaid": Decimal("0")}
        self._calculator = lambda: self._data["principal"] - self._data["repaid"]

    def get_remaining(self):  # BAD: Complex for no benefit
        return self._calculator()
```

**Why this matters**: Simple solutions reduce bugs and maintenance. Dataclasses eliminate boilerplate. Direct calculation beats indirection. Complexity should solve actual problems, not create elegance.

### Readability Counts

```python
# GOOD: Readable comprehension with clear filtering
from typing import List


def calculate_total_zakat(wealth_items: List[Decimal], nisab: Decimal) -> Decimal:
    """Calculate total Zakat for wealth items exceeding nisab."""
    qualifying_wealth = [
        amount for amount in wealth_items if amount >= nisab
    ]
    return sum(qualifying_wealth) * Decimal("0.025")


# BAD: Unreadable one-liner
def calculate_total_zakat(w: List[Decimal], n: Decimal) -> Decimal:
    return sum([a for a in w if a >= n]) * Decimal("0.025")  # BAD: Cryptic names
```

**Why this matters**: Descriptive names document purpose. Multi-line comprehensions with clear filtering improve comprehension. Saving keystrokes costs understanding.

## Type Hints and Annotations

Type hints (PEP 484) provide static analysis and documentation. The platform requires type hints for all public APIs.

### Function Signatures

```python
# GOOD: Complete type hints with Pydantic validation
from decimal import Decimal
from pydantic import BaseModel, Field, field_validator


class WaqfEndowment(BaseModel):
    """Waqf (Islamic endowment) with validation."""

    endowment_id: str
    principal_amount: Decimal = Field(gt=0, description="Initial endowment")
    annual_return_rate: Decimal = Field(ge=0, le=1, description="Expected return rate")

    @field_validator("endowment_id")
    @classmethod
    def validate_id_format(cls, v: str) -> str:
        """Ensure endowment ID follows format."""
        if not v.startswith("WQF-"):
            raise ValueError("Endowment ID must start with WQF-")
        return v

    def calculate_annual_return(self) -> Decimal:
        """Calculate expected annual return from endowment."""
        return self.principal_amount * self.annual_return_rate


# BAD: Missing type hints
def calculate_annual_return(endowment):  # BAD: No type hints
    return endowment["principal"] * endowment["rate"]  # BAD: Dictionary-based, no validation
```

**Why this matters**: Type hints enable mypy static analysis. Pydantic validates at runtime. Field constraints (`gt=0`, `ge=0`, `le=1`) enforce business rules. Validators ensure data integrity.

### Generic Types

```python
# GOOD: Use generics for collection types
from typing import List, Dict, Optional, TypeVar
from decimal import Decimal


T = TypeVar("T")


def find_by_id(items: List[T], item_id: str, get_id: callable) -> Optional[T]:
    """Find item by ID in a list.

    Args:
        items: List of items to search
        item_id: ID to search for
        get_id: Function to extract ID from item

    Returns:
        Found item or None
    """
    for item in items:
        if get_id(item) == item_id:
            return item
    return None


# Example usage with donation campaigns
campaigns: List[DonationCampaign] = get_all_campaigns()
target_campaign = find_by_id(
    campaigns,
    "CAMP-2025-001",
    lambda c: c.campaign_id
)


# BAD: No generics, any type accepted
def find_by_id(items, item_id):  # BAD: No type information
    for item in items:
        if item["id"] == item_id:  # BAD: Assumes dict structure
            return item
    return None
```

**Why this matters**: Generic types (`TypeVar`, `List[T]`) preserve type information. Type checkers verify correct usage. Explicit types prevent runtime errors.

### Protocol Types (Structural Subtyping)

```python
# GOOD: Protocol for structural subtyping (duck typing with types)
from typing import Protocol
from decimal import Decimal


class FinancialCalculator(Protocol):
    """Protocol for financial calculation services."""

    def calculate_obligation(self, base_amount: Decimal) -> Decimal:
        """Calculate financial obligation from base amount."""
        ...


class ZakatCalculator:
    """Calculate Zakat obligation (2.5% of qualifying wealth)."""

    def calculate_obligation(self, wealth_amount: Decimal) -> Decimal:
        """Calculate Zakat owed."""
        return wealth_amount * Decimal("0.025")


class TaxCalculator:
    """Calculate tax obligation (variable rate)."""

    def __init__(self, tax_rate: Decimal):
        self.tax_rate = tax_rate

    def calculate_obligation(self, income_amount: Decimal) -> Decimal:
        """Calculate tax owed."""
        return income_amount * self.tax_rate


def process_obligations(
    calculator: FinancialCalculator,
    amounts: List[Decimal]
) -> Decimal:
    """Process obligations using any compatible calculator."""
    return sum(calculator.calculate_obligation(amount) for amount in amounts)


# Usage: Both calculators satisfy Protocol without explicit inheritance
zakat_calc = ZakatCalculator()
tax_calc = TaxCalculator(Decimal("0.20"))

zakat_total = process_obligations(zakat_calc, wealth_amounts)
tax_total = process_obligations(tax_calc, income_amounts)


# BAD: Abstract base class with explicit inheritance requirement
from abc import ABC, abstractmethod


class FinancialCalculatorABC(ABC):  # BAD: Requires explicit inheritance
    @abstractmethod
    def calculate_obligation(self, amount: Decimal) -> Decimal:
        pass


class ZakatCalculator(FinancialCalculatorABC):  # BAD: Forced to inherit
    def calculate_obligation(self, wealth_amount: Decimal) -> Decimal:
        return wealth_amount * Decimal("0.025")
```

**Why this matters**: Protocols enable structural subtyping (duck typing with type checking). Classes satisfy protocols without explicit inheritance. Looser coupling, stronger typing.

## Code Organization

### Module Structure

```python
# GOOD: Well-organized module structure
# File: ose_platform/domain/zakat/calculator.py

"""Zakat calculation services for wealth-based obligations.

This module provides calculation services for various types of Zakat
including wealth-based (gold, silver, cash) and business asset Zakat.
"""

from decimal import Decimal
from typing import Protocol

# Constants at module level
ZAKAT_RATE = Decimal("0.025")  # 2.5%
GOLD_NISAB_GRAMS = 85
SILVER_NISAB_GRAMS = 595


class ZakatCalculationStrategy(Protocol):
    """Protocol for different Zakat calculation strategies."""

    def calculate(self, base_amount: Decimal) -> Decimal:
        """Calculate Zakat for given amount."""
        ...


class StandardZakatCalculator:
    """Standard 2.5% Zakat calculation."""

    def calculate(self, wealth_amount: Decimal) -> Decimal:
        """Calculate standard Zakat obligation."""
        return wealth_amount * ZAKAT_RATE


class GoldZakatCalculator:
    """Zakat calculation for gold holdings."""

    def __init__(self, gold_price_per_gram: Decimal):
        self.gold_price_per_gram = gold_price_per_gram
        self.nisab_value = gold_price_per_gram * GOLD_NISAB_GRAMS

    def calculate(self, gold_grams: Decimal) -> Decimal:
        """Calculate Zakat for gold holdings if exceeding nisab."""
        gold_value = gold_grams * self.gold_price_per_gram
        if gold_value >= self.nisab_value:
            return gold_value * ZAKAT_RATE
        return Decimal("0")


# BAD: Disorganized module
# File: calc.py

def calc_zakat(amt):  # BAD: Unclear function name, no docstring
    return amt * 0.025

RATE = 0.025  # BAD: Constant after function
gn = 85  # BAD: Cryptic name

class Z:  # BAD: Non-descriptive class name
    def c(self, a):  # BAD: Single-letter method and parameter
        return a * RATE
```

**Why this matters**: Module docstrings explain purpose. Constants defined at top. Clear class names document intent. Protocols separate interface from implementation.

### Package Organization

```mermaid
%%{init: {'theme': 'base', 'themeVariables': { 'primaryColor': '#0173B2', 'primaryTextColor': '#000', 'primaryBorderColor': '#0173B2', 'lineColor': '#DE8F05', 'secondaryColor': '#029E73', 'tertiaryColor': '#CC78BC', 'background': '#fff', 'mainBkg': '#fff'}}}%%
graph TD
    A[ose_platform/] --> B[domain/]
    A --> C[application/]
    A --> D[infrastructure/]

    B --> B1[zakat/]
    B --> B2[donation/]
    B --> B3[qard_hasan/]

    B1 --> B1A[__init__.py]
    B1 --> B1B[models.py]
    B1 --> B1C[calculator.py]
    B1 --> B1D[repository.py]

    C --> C1[services/]
    C --> C2[commands/]
    C --> C3[queries/]

    D --> D1[database/]
    D --> D2[api/]
    D --> D3[messaging/]

    style A fill:#0173B2,stroke:#000,color:#fff
    style B fill:#DE8F05,stroke:#000,color:#000
    style C fill:#029E73,stroke:#000,color:#fff
    style D fill:#CC78BC,stroke:#000,color:#000
```

**Package structure follows domain-driven design**:

- **domain/**: Core business logic (entities, value objects, domain services)
- **application/**: Use cases and application services
- **infrastructure/**: Technical implementations (database, API, messaging)

### Dependency Direction

```python
# GOOD: Dependencies point inward (infrastructure → application → domain)

# domain/zakat/models.py (no external dependencies)
from dataclasses import dataclass
from decimal import Decimal


@dataclass(frozen=True)
class ZakatObligation:
    """Domain model with no infrastructure dependencies."""

    payer_id: str
    wealth_amount: Decimal
    zakat_amount: Decimal


# application/services/zakat_service.py (depends on domain)
from domain.zakat.models import ZakatObligation
from domain.zakat.calculator import StandardZakatCalculator


class ZakatService:
    """Application service orchestrating Zakat calculation."""

    def __init__(self, calculator: StandardZakatCalculator):
        self._calculator = calculator

    def calculate_obligation(self, payer_id: str, wealth: Decimal) -> ZakatObligation:
        """Calculate Zakat obligation for payer."""
        zakat_amount = self._calculator.calculate(wealth)
        return ZakatObligation(payer_id, wealth, zakat_amount)


# infrastructure/api/zakat_controller.py (depends on application)
from fastapi import APIRouter
from application.services.zakat_service import ZakatService

router = APIRouter()


@router.post("/zakat/calculate")
async def calculate_zakat(payer_id: str, wealth: Decimal):
    """API endpoint for Zakat calculation."""
    service = ZakatService(StandardZakatCalculator())
    obligation = service.calculate_obligation(payer_id, wealth)
    return {"zakat_amount": str(obligation.zakat_amount)}


# BAD: Domain depends on infrastructure
# domain/zakat/models.py
from infrastructure.database import db  # BAD: Domain depends on infrastructure


class ZakatObligation:
    def save(self):  # BAD: Persistence logic in domain
        db.save(self)
```

**Why this matters**: Inward dependency direction (infrastructure → application → domain) keeps domain pure. Domain models contain business logic without technical concerns. Easier testing and portability.

## Error Handling Patterns

### Explicit Exception Handling

```python
# GOOD: Explicit exception handling with context
from decimal import Decimal, InvalidOperation


class InvalidZakatAmount(ValueError):
    """Raised when Zakat amount calculation is invalid."""

    pass


def calculate_zakat_obligation(wealth_amount: Decimal, nisab: Decimal) -> Decimal:
    """Calculate Zakat obligation with explicit error handling.

    Args:
        wealth_amount: Total wealth subject to Zakat
        nisab: Minimum threshold for Zakat obligation

    Returns:
        Calculated Zakat amount (0 if below nisab)

    Raises:
        InvalidZakatAmount: If wealth_amount or nisab is negative
    """
    try:
        if wealth_amount < 0 or nisab < 0:
            raise InvalidZakatAmount("Wealth and nisab must be non-negative")

        if wealth_amount >= nisab:
            return wealth_amount * Decimal("0.025")
        return Decimal("0")

    except InvalidOperation as e:
        raise InvalidZakatAmount(f"Invalid decimal operation: {e}") from e


# BAD: Silent failure
def calculate_zakat_obligation(wealth_amount, nisab):
    try:
        if wealth_amount >= nisab:
            return wealth_amount * 0.025
    except:  # BAD: Bare except catches everything
        pass  # BAD: Silent failure hides errors
    return 0
```

**Why this matters**: Explicit exceptions document failure modes. Custom exceptions provide context. Chaining exceptions (`from e`) preserves stack traces. Silent failures hide bugs.

### Validation at Boundaries

```python
# GOOD: Validate at system boundaries using Pydantic
from pydantic import BaseModel, Field, field_validator
from decimal import Decimal


class ZakatCalculationRequest(BaseModel):
    """Validate Zakat calculation requests at API boundary."""

    payer_id: str = Field(min_length=1, description="Unique payer identifier")
    wealth_amount: Decimal = Field(gt=0, description="Total qualifying wealth")
    nisab_threshold: Decimal = Field(gt=0, description="Nisab threshold")

    @field_validator("payer_id")
    @classmethod
    def validate_payer_id_format(cls, v: str) -> str:
        """Ensure payer ID follows expected format."""
        if not v.startswith("PAYER-"):
            raise ValueError("Payer ID must start with PAYER-")
        return v


# Usage in API endpoint
from fastapi import APIRouter

router = APIRouter()


@router.post("/zakat/calculate")
async def calculate_zakat(request: ZakatCalculationRequest):
    """Pydantic validates request automatically at boundary."""
    # Internal code trusts validated data
    zakat_amount = request.wealth_amount * Decimal("0.025")
    return {"payer_id": request.payer_id, "zakat_amount": str(zakat_amount)}


# BAD: No validation or validation scattered throughout
@router.post("/zakat/calculate")
async def calculate_zakat(payer_id: str, wealth_amount: float):  # BAD: No validation
    # BAD: Validation scattered in business logic
    if not payer_id:
        return {"error": "Invalid payer"}
    if wealth_amount <= 0:
        return {"error": "Invalid amount"}
    # More code...
```

**Why this matters**: Validate at boundaries (API, database, external services). Internal code trusts validated data. Pydantic centralizes validation. Reduces defensive programming clutter.

## Testing Practices

### Test Organization

```python
# GOOD: Organized test structure with clear naming
# tests/domain/zakat/test_calculator.py

import pytest
from decimal import Decimal
from ose_platform.domain.zakat.calculator import StandardZakatCalculator


class TestStandardZakatCalculator:
    """Test suite for standard Zakat calculation."""

    @pytest.fixture
    def calculator(self) -> StandardZakatCalculator:
        """Provide calculator instance for tests."""
        return StandardZakatCalculator()

    def test_calculate_returns_2_5_percent_of_wealth(
        self, calculator: StandardZakatCalculator
    ):
        """Verify Zakat is exactly 2.5% of wealth amount."""
        wealth = Decimal("100000.00")
        expected_zakat = Decimal("2500.00")

        result = calculator.calculate(wealth)

        assert result == expected_zakat

    def test_calculate_with_zero_wealth_returns_zero(
        self, calculator: StandardZakatCalculator
    ):
        """Verify zero wealth produces zero Zakat."""
        result = calculator.calculate(Decimal("0"))
        assert result == Decimal("0")

    @pytest.mark.parametrize(
        "wealth,expected_zakat",
        [
            (Decimal("100000.00"), Decimal("2500.00")),
            (Decimal("50000.00"), Decimal("1250.00")),
            (Decimal("1000.00"), Decimal("25.00")),
        ],
    )
    def test_calculate_with_various_amounts(
        self,
        calculator: StandardZakatCalculator,
        wealth: Decimal,
        expected_zakat: Decimal,
    ):
        """Verify Zakat calculation across various wealth amounts."""
        assert calculator.calculate(wealth) == expected_zakat


# BAD: Disorganized tests
def test1():  # BAD: Unclear test name
    c = StandardZakatCalculator()
    assert c.calculate(Decimal("100000")) == Decimal("2500")  # BAD: Magic numbers


def test2():  # BAD: Non-descriptive
    assert StandardZakatCalculator().calculate(Decimal("0")) == 0  # BAD: Mixed types
```

**Why this matters**: Organized test classes group related tests. Fixtures reduce duplication. Descriptive names document intent. Parameterized tests cover multiple cases concisely.

## Documentation Standards

### Docstring Format

```python
# GOOD: Complete docstrings with Google style
from decimal import Decimal
from typing import Optional


def calculate_murabaha_total(
    asset_cost: Decimal,
    profit_margin_rate: Decimal,
    down_payment: Optional[Decimal] = None,
) -> Decimal:
    """Calculate total Murabaha cost-plus financing amount.

    Murabaha is Islamic financing where the seller discloses the cost of
    the asset and adds a profit margin agreed upon by both parties.

    Args:
        asset_cost: Original acquisition cost of the asset
        profit_margin_rate: Profit margin as decimal (0.15 = 15%)
        down_payment: Optional initial down payment to subtract from total

    Returns:
        Total amount to be financed (cost + profit - down payment)

    Raises:
        ValueError: If asset_cost or profit_margin_rate is negative
        ValueError: If down_payment exceeds total selling price

    Examples:
        >>> calculate_murabaha_total(Decimal("200000"), Decimal("0.15"))
        Decimal('230000.00')

        >>> calculate_murabaha_total(
        ...     Decimal("200000"),
        ...     Decimal("0.15"),
        ...     Decimal("50000")
        ... )
        Decimal('180000.00')
    """
    if asset_cost < 0:
        raise ValueError("Asset cost must be non-negative")
    if profit_margin_rate < 0:
        raise ValueError("Profit margin must be non-negative")

    total_selling_price = asset_cost + (asset_cost * profit_margin_rate)

    if down_payment is not None:
        if down_payment > total_selling_price:
            raise ValueError("Down payment exceeds total selling price")
        return total_selling_price - down_payment

    return total_selling_price


# BAD: Minimal or missing docstring
def calc_murabaha(c, r, d=None):  # BAD: No docstring, unclear parameters
    return c + (c * r) - (d or 0)
```

**Why this matters**: Complete docstrings document parameters, returns, exceptions, and usage. Examples demonstrate correct usage. Docstrings enable auto-generated documentation (Sphinx, MkDocs).

## Security Practices

### Input Validation

```python
# GOOD: Validate and sanitize inputs
from pydantic import BaseModel, Field, field_validator
from decimal import Decimal
import re


class DonationPayment(BaseModel):
    """Donation payment with strict validation."""

    donor_id: str = Field(min_length=5, max_length=50)
    amount: Decimal = Field(gt=0, le=Decimal("1000000"))
    payment_reference: str = Field(min_length=1, max_length=100)

    @field_validator("donor_id")
    @classmethod
    def validate_donor_id_format(cls, v: str) -> str:
        """Ensure donor ID contains only alphanumeric and hyphens."""
        if not re.match(r"^[A-Z0-9-]+$", v):
            raise ValueError("Donor ID must be alphanumeric with hyphens")
        return v

    @field_validator("payment_reference")
    @classmethod
    def validate_payment_reference(cls, v: str) -> str:
        """Sanitize payment reference to prevent injection."""
        # Remove potentially dangerous characters
        sanitized = re.sub(r"[^\w\s-]", "", v)
        if not sanitized:
            raise ValueError("Payment reference invalid after sanitization")
        return sanitized


# BAD: No validation, SQL injection risk
def record_donation(donor_id: str, amount: float):
    query = f"INSERT INTO donations VALUES ('{donor_id}', {amount})"  # BAD: SQL injection
    db.execute(query)
```

**Why this matters**: Input validation prevents injection attacks. Pydantic validates types and constraints. Regular expressions sanitize strings. Never trust user input.

### Secrets Management

```python
# GOOD: Use environment variables and secrets management
import os
from pydantic_settings import BaseSettings


class Settings(BaseSettings):
    """Application settings loaded from environment."""

    database_url: str
    api_key: str
    jwt_secret: str

    class Config:
        env_file = ".env"
        env_file_encoding = "utf-8"


# Usage
settings = Settings()
# Secrets loaded from environment, never hardcoded


# BAD: Hardcoded secrets
DATABASE_URL = "postgresql://user:password@localhost/db"  # BAD: Hardcoded credentials
API_KEY = "sk-1234567890abcdef"  # BAD: Exposed secret
```

**Why this matters**: Environment variables separate configuration from code. Never commit secrets to version control. Use `.env` files (excluded from git) for local development. Production uses secret managers (AWS Secrets Manager, HashiCorp Vault).

## Anti-Patterns

### Avoid Mutable Default Arguments

```python
# GOOD: Use None as default, create mutable in function
from typing import List, Optional


def process_donations(
    donations: Optional[List[Decimal]] = None,
) -> Decimal:
    """Process list of donations and return total."""
    if donations is None:
        donations = []  # Create new list each call

    return sum(donations)


# BAD: Mutable default argument
def process_donations(donations: List[Decimal] = []) -> Decimal:  # BAD: Mutable default
    """BAD: Default list is shared across calls!"""
    donations.append(Decimal("10.00"))  # BAD: Modifies shared default
    return sum(donations)


# Demonstrates problem:
# >>> process_donations()
# Decimal('10.00')
# >>> process_donations()  # BAD: Retains previous call's data
# Decimal('20.00')
```

**Why this matters**: Mutable defaults (`[]`, `{}`) are evaluated once at function definition. All calls share the same instance. Use `None` as default and create new instances in function body.

### Avoid Bare Except

```python
# GOOD: Catch specific exceptions
from decimal import Decimal, InvalidOperation


def parse_zakat_amount(amount_str: str) -> Decimal:
    """Parse Zakat amount from string with specific exception handling."""
    try:
        return Decimal(amount_str)
    except InvalidOperation as e:
        raise ValueError(f"Invalid Zakat amount format: {amount_str}") from e


# BAD: Bare except catches everything
def parse_zakat_amount(amount_str: str) -> Decimal:
    try:
        return Decimal(amount_str)
    except:  # BAD: Catches KeyboardInterrupt, SystemExit, etc.
        return Decimal("0")  # BAD: Silent failure
```

**Why this matters**: Bare `except:` catches all exceptions including system exits. Specific exceptions document expected failures. Exception chaining preserves debugging information.

## References

### Official Documentation

- [PEP 8 - Style Guide for Python Code](https://peps.python.org/pep-0008/)
- [PEP 20 - The Zen of Python](https://peps.python.org/pep-0020/)
- [PEP 484 - Type Hints](https://peps.python.org/pep-0484/)
- [PEP 544 - Protocols](https://peps.python.org/pep-0544/)
- [Python Documentation](https://docs.python.org/3/)

### Related Documentation

- [Python Idioms](./ex-so-stla-py__idioms.md) - Pythonic patterns
- [Type Safety](./ex-so-stla-py__type-safety.md) - Type hints and mypy
- [Error Handling](./ex-so-stla-py__error-handling.md) - Exception patterns
- [Linting and Formatting](./ex-so-stla-py__linting-and-formatting.md) - Automated tools
- [Test-Driven Development](./ex-so-stla-py__test-driven-development.md) - Testing practices

### Tools

- [Ruff](https://docs.astral.sh/ruff/) - Fast Python linter and formatter
- [Black](https://black.readthedocs.io/) - Opinionated code formatter
- [mypy](https://mypy.readthedocs.io/) - Static type checker
- [pytest](https://docs.pytest.org/) - Testing framework
- [Pydantic](https://docs.pydantic.dev/) - Data validation

---

**Last Updated**: 2025-01-23
**Python Version**: 3.11+ (baseline), 3.12+ (stable maintenance), 3.13.x (latest stable)
**Maintainers**: OSE Platform Documentation Team
