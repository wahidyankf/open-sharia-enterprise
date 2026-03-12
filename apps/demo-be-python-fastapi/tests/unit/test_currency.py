"""Unit tests for currency/amount validation."""

from decimal import Decimal

import pytest

from demo_be_python_fastapi.domain.errors import ValidationError
from demo_be_python_fastapi.domain.expense import validate_amount, validate_currency


@pytest.mark.unit
class TestCurrencyValidation:
    """Tests for currency validation."""

    def test_valid_usd(self) -> None:
        assert validate_currency("USD") == "USD"

    def test_valid_idr(self) -> None:
        assert validate_currency("IDR") == "IDR"

    def test_unsupported_currency(self) -> None:
        with pytest.raises(ValidationError) as exc_info:
            validate_currency("EUR")
        assert exc_info.value.field == "currency"
        assert "Unsupported" in exc_info.value.message

    def test_malformed_short_currency(self) -> None:
        with pytest.raises(ValidationError) as exc_info:
            validate_currency("US")
        assert exc_info.value.field == "currency"
        assert "Malformed" in exc_info.value.message

    def test_malformed_long_currency(self) -> None:
        with pytest.raises(ValidationError) as exc_info:
            validate_currency("USDD")
        assert exc_info.value.field == "currency"

    def test_lowercase_currency_normalised(self) -> None:
        assert validate_currency("usd") == "USD"


@pytest.mark.unit
class TestAmountValidation:
    """Tests for amount validation."""

    def test_usd_two_decimal_places(self) -> None:
        result = validate_amount("USD", "10.50")
        assert result == Decimal("10.50")

    def test_idr_zero_decimal_places(self) -> None:
        result = validate_amount("IDR", "150000")
        assert result == Decimal("150000")

    def test_negative_amount_rejected(self) -> None:
        with pytest.raises(ValidationError) as exc_info:
            validate_amount("USD", "-10.00")
        assert exc_info.value.field == "amount"
        assert "negative" in exc_info.value.message

    def test_usd_wrong_precision_rejected(self) -> None:
        with pytest.raises(ValidationError) as exc_info:
            validate_amount("USD", "10.555")
        assert exc_info.value.field == "amount"

    def test_idr_decimal_rejected(self) -> None:
        with pytest.raises(ValidationError) as exc_info:
            validate_amount("IDR", "150000.50")
        assert exc_info.value.field == "amount"

    def test_invalid_amount_string(self) -> None:
        with pytest.raises(ValidationError) as exc_info:
            validate_amount("USD", "not-a-number")
        assert exc_info.value.field == "amount"

    def test_zero_amount_accepted(self) -> None:
        result = validate_amount("USD", "0.00")
        assert result == Decimal("0.00")
