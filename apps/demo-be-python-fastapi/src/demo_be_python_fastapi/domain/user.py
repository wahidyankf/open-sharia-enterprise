"""User domain validation functions."""

import re

from demo_be_python_fastapi.domain.errors import ValidationError


def validate_password_strength(password: str) -> None:
    """Validate password meets complexity requirements.

    Requirements:
    - At least 12 characters
    - At least one uppercase letter
    - At least one special character
    """
    if len(password) < 12:
        raise ValidationError("password", "Password must be at least 12 characters long")
    if not re.search(r"[A-Z]", password):
        raise ValidationError("password", "Password must contain at least one uppercase letter")
    if not re.search(r"[^a-zA-Z0-9]", password):
        raise ValidationError("password", "Password must contain at least one special character")


def validate_email_format(email: str) -> None:
    """Validate email format."""
    pattern = r"^[a-zA-Z0-9._%+\-]+@[a-zA-Z0-9.\-]+\.[a-zA-Z]{2,}$"
    if not re.match(pattern, email):
        raise ValidationError("email", f"Invalid email format: {email}")


def validate_username(username: str) -> None:
    """Validate username is non-empty."""
    if not username or not username.strip():
        raise ValidationError("username", "Username must not be empty")
