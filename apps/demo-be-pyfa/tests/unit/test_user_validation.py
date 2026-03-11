"""Unit tests for user domain validation functions."""

import pytest

from demo_be_pyfa.domain.errors import ValidationError
from demo_be_pyfa.domain.user import (
    validate_email_format,
    validate_password_strength,
    validate_username,
)


@pytest.mark.unit
class TestPasswordStrength:
    """Tests for password strength validation."""

    def test_valid_password_passes(self) -> None:
        validate_password_strength("Str0ng#Pass1")

    def test_password_too_short(self) -> None:
        with pytest.raises(ValidationError) as exc_info:
            validate_password_strength("Short1!")
        assert exc_info.value.field == "password"
        assert "12 characters" in exc_info.value.message

    def test_password_no_uppercase(self) -> None:
        with pytest.raises(ValidationError) as exc_info:
            validate_password_strength("str0ng#pass1word")
        assert exc_info.value.field == "password"
        assert "uppercase" in exc_info.value.message

    def test_password_no_special_char(self) -> None:
        with pytest.raises(ValidationError) as exc_info:
            validate_password_strength("AllUpperCase1234")
        assert exc_info.value.field == "password"
        assert "special character" in exc_info.value.message

    def test_exactly_12_characters_with_requirements(self) -> None:
        validate_password_strength("Str0ngPass1!!")

    def test_empty_password_fails_length_check(self) -> None:
        with pytest.raises(ValidationError) as exc_info:
            validate_password_strength("")
        assert exc_info.value.field == "password"


@pytest.mark.unit
class TestEmailValidation:
    """Tests for email format validation."""

    def test_valid_email(self) -> None:
        validate_email_format("alice@example.com")

    def test_invalid_email_no_at(self) -> None:
        with pytest.raises(ValidationError) as exc_info:
            validate_email_format("not-an-email")
        assert exc_info.value.field == "email"

    def test_invalid_email_no_domain(self) -> None:
        with pytest.raises(ValidationError) as exc_info:
            validate_email_format("alice@")
        assert exc_info.value.field == "email"


@pytest.mark.unit
class TestUsernameValidation:
    """Tests for username validation."""

    def test_valid_username(self) -> None:
        validate_username("alice")

    def test_empty_username(self) -> None:
        with pytest.raises(ValidationError) as exc_info:
            validate_username("")
        assert exc_info.value.field == "username"

    def test_whitespace_username(self) -> None:
        with pytest.raises(ValidationError) as exc_info:
            validate_username("   ")
        assert exc_info.value.field == "username"
