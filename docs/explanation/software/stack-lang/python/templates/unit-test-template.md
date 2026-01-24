---
title: Unit Test Template
description: Copy-paste ready pytest unit test template with fixtures, parameterization, and mocking for testing Python domain logic in Sharia-compliant financial applications
category: explanation
subcategory: stack-lang
tags:
  - python
  - template
  - unit-test
  - pytest
  - testing
related:
  - ../ex-so-stla-py__test-driven-development.md
  - ./integration-test-template.md
principles:
  - test-first
last_updated: 2025-01-23
---

# Unit Test Template

## Overview

Unit tests verify individual components in isolation. Use pytest for assertions, fixtures, and parameterized tests.

**Use this template when**:

- Testing pure functions
- Testing domain logic
- Testing class methods in isolation

**Examples**: Test Zakat calculation, test Money value object

## Complete Code Template

```python
"""Unit test template with pytest."""

import pytest
from decimal import Decimal
from zakat_calculator import calculate_zakat, Money


# ============================================================================
# BASIC TESTS
# ============================================================================


def test_calculate_zakat_above_nisab():
    """Test Zakat calculation for wealth above nisab."""
    wealth = Decimal("100000")
    nisab = Decimal("85000")

    result = calculate_zakat(wealth, nisab)

    assert result == Decimal("2500")  # 2.5% of 100,000


def test_calculate_zakat_below_nisab():
    """Test Zakat calculation for wealth below nisab."""
    wealth = Decimal("50000")
    nisab = Decimal("85000")

    result = calculate_zakat(wealth, nisab)

    assert result == Decimal("0")  # No Zakat required


# ============================================================================
# PARAMETERIZED TESTS
# ============================================================================


@pytest.mark.parametrize(
    "wealth,nisab,expected_zakat",
    [
        (Decimal("100000"), Decimal("85000"), Decimal("2500")),
        (Decimal("150000"), Decimal("85000"), Decimal("3750")),
        (Decimal("84999"), Decimal("85000"), Decimal("0")),
        (Decimal("85000"), Decimal("85000"), Decimal("2125")),
    ],
)
def test_calculate_zakat_parameterized(wealth, nisab, expected_zakat):
    """Test Zakat calculation with multiple cases."""
    result = calculate_zakat(wealth, nisab)
    assert result == expected_zakat


# ============================================================================
# FIXTURES
# ============================================================================


@pytest.fixture
def sample_wealth():
    """Fixture for sample wealth amount."""
    return Decimal("100000")


@pytest.fixture
def sample_nisab():
    """Fixture for nisab threshold."""
    return Decimal("85000")


def test_calculate_zakat_with_fixtures(sample_wealth, sample_nisab):
    """Test using fixtures."""
    result = calculate_zakat(sample_wealth, sample_nisab)
    assert result == Decimal("2500")


# ============================================================================
# VALUE OBJECT TESTS
# ============================================================================


def test_money_creation():
    """Test Money value object creation."""
    money = Money(Decimal("2500.00"), "USD")

    assert money.amount == Decimal("2500.00")
    assert money.currency == "USD"


def test_money_addition():
    """Test Money addition."""
    money1 = Money(Decimal("1000.00"), "USD")
    money2 = Money(Decimal("1500.00"), "USD")

    result = money1 + money2

    assert result.amount == Decimal("2500.00")
    assert result.currency == "USD"


def test_money_different_currency_raises():
    """Test Money addition with different currencies raises error."""
    money1 = Money(Decimal("1000.00"), "USD")
    money2 = Money(Decimal("1500.00"), "SAR")

    with pytest.raises(ValueError, match="different currencies"):
        money1 + money2


# ============================================================================
# MOCK EXAMPLE
# ============================================================================


def test_service_with_mock(mocker):
    """Test service with mocked repository."""
    # Mock repository
    mock_repo = mocker.Mock()
    mock_repo.save = mocker.AsyncMock()

    # Create service
    service = ZakatCalculationService(mock_repo)

    # Execute
    result = await service.calculate_zakat(...)

    # Verify
    mock_repo.save.assert_called_once()
```

## Best Practices

### Do: Use Descriptive Names

```python
# GOOD: Clear test name
def test_calculate_zakat_above_nisab():
    ...


# BAD: Unclear name
def test_zakat():
    ...
```

### Do: Arrange-Act-Assert

```python
# GOOD: Clear test structure
def test_calculate_zakat():
    # Arrange
    wealth = Decimal("100000")
    nisab = Decimal("85000")

    # Act
    result = calculate_zakat(wealth, nisab)

    # Assert
    assert result == Decimal("2500")
```

## References

- [Test-Driven Development](../ex-so-stla-py__test-driven-development.md)

---

**Last Updated**: 2025-01-23
**Python Version**: 3.11+ (baseline), 3.12+ (stable maintenance), 3.14.x (latest stable)
**Maintainers**: OSE Platform Documentation Team
