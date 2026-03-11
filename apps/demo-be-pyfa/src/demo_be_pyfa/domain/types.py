"""Domain enumerations."""

from enum import StrEnum


class Currency(StrEnum):
    """Supported currencies."""

    USD = "USD"
    IDR = "IDR"


class Role(StrEnum):
    """User roles."""

    USER = "USER"
    ADMIN = "ADMIN"


class UserStatus(StrEnum):
    """User account statuses."""

    ACTIVE = "ACTIVE"
    INACTIVE = "INACTIVE"
    DISABLED = "DISABLED"
    LOCKED = "LOCKED"


class EntryType(StrEnum):
    """Financial entry types."""

    EXPENSE = "expense"
    INCOME = "income"


SUPPORTED_UNITS = {
    "liter",
    "ml",
    "kg",
    "g",
    "km",
    "meter",
    "gallon",
    "lb",
    "oz",
    "mile",
    "piece",
    "hour",
}
