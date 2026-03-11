"""Expense domain validation functions."""

from decimal import Decimal, InvalidOperation

from demo_be_pyfa.domain.errors import ValidationError
from demo_be_pyfa.domain.types import SUPPORTED_UNITS

CURRENCY_DECIMALS: dict[str, int] = {
    "USD": 2,
    "IDR": 0,
}

SUPPORTED_CURRENCIES = set(CURRENCY_DECIMALS.keys())


def validate_currency(currency: str) -> str:
    """Validate currency code and return normalised uppercase form."""
    if len(currency) != 3:
        raise ValidationError("currency", f"Malformed currency code: {currency}")
    upper = currency.upper()
    if upper not in SUPPORTED_CURRENCIES:
        raise ValidationError("currency", f"Unsupported currency: {currency}")
    return upper


def validate_amount(currency: str, amount_str: str) -> Decimal:
    """Validate amount string for the given currency.

    Returns the validated Decimal amount.
    """
    try:
        amount = Decimal(amount_str)
    except InvalidOperation as err:
        raise ValidationError("amount", f"Invalid amount: {amount_str}") from err

    if amount < 0:
        raise ValidationError("amount", "Amount must not be negative")

    places = CURRENCY_DECIMALS[currency]
    quantizer = Decimal(10) ** -places
    quantized = amount.quantize(quantizer)
    if quantized != amount:
        raise ValidationError(
            "amount",
            f"{currency} requires {places} decimal place(s)",
        )
    return amount


def validate_unit(unit: str) -> str:
    """Validate unit of measure."""
    if unit not in SUPPORTED_UNITS:
        raise ValidationError("unit", f"Unsupported unit: {unit}")
    return unit
