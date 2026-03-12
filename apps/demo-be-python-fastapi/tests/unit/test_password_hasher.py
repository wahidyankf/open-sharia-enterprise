"""Unit tests for password hasher."""

import pytest

from demo_be_python_fastapi.infrastructure.password_hasher import hash_password, verify_password


@pytest.mark.unit
class TestPasswordHasher:
    """Tests for bcrypt password hashing."""

    def test_hash_returns_string(self) -> None:
        result = hash_password("Str0ng#Pass1")
        assert isinstance(result, str)
        assert len(result) > 0

    def test_verify_correct_password(self) -> None:
        hashed = hash_password("Str0ng#Pass1")
        assert verify_password("Str0ng#Pass1", hashed) is True

    def test_verify_wrong_password(self) -> None:
        hashed = hash_password("Str0ng#Pass1")
        assert verify_password("WrongPassword!", hashed) is False

    def test_hash_is_not_plain_text(self) -> None:
        plain = "Str0ng#Pass1"
        hashed = hash_password(plain)
        assert hashed != plain

    def test_different_hashes_for_same_password(self) -> None:
        hashed1 = hash_password("Str0ng#Pass1")
        hashed2 = hash_password("Str0ng#Pass1")
        # bcrypt uses random salt, so hashes differ
        assert hashed1 != hashed2
