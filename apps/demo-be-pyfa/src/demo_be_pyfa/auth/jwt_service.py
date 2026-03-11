"""JWT token generation and validation."""

from datetime import UTC, datetime, timedelta
from typing import Any
from uuid import uuid4

import jwt

from demo_be_pyfa.config import settings
from demo_be_pyfa.domain.errors import UnauthorizedError


def create_access_token(user_id: str, username: str, role: str) -> str:
    """Create a signed JWT access token."""
    now = datetime.now(UTC)
    payload: dict[str, Any] = {
        "sub": user_id,
        "username": username,
        "role": role,
        "iss": settings.app_jwt_issuer,
        "iat": now,
        "exp": now + timedelta(minutes=15),
        "jti": str(uuid4()),
        "type": "access",
    }
    return jwt.encode(payload, settings.app_jwt_secret, algorithm="HS256")


def create_refresh_token(user_id: str) -> str:
    """Create a signed JWT refresh token."""
    now = datetime.now(UTC)
    payload: dict[str, Any] = {
        "sub": user_id,
        "iss": settings.app_jwt_issuer,
        "iat": now,
        "exp": now + timedelta(days=7),
        "jti": str(uuid4()),
        "type": "refresh",
    }
    return jwt.encode(payload, settings.app_jwt_secret, algorithm="HS256")


def create_expired_refresh_token(user_id: str) -> str:
    """Create an already-expired refresh token (for testing only)."""
    now = datetime.now(UTC)
    payload: dict[str, Any] = {
        "sub": user_id,
        "iss": settings.app_jwt_issuer,
        "iat": now - timedelta(days=8),
        "exp": now - timedelta(days=1),
        "jti": str(uuid4()),
        "type": "refresh",
    }
    return jwt.encode(payload, settings.app_jwt_secret, algorithm="HS256")


def decode_token(token: str) -> dict[str, Any]:
    """Decode and verify a JWT token.

    Raises UnauthorizedError on invalid or expired tokens.
    """
    try:
        return jwt.decode(token, settings.app_jwt_secret, algorithms=["HS256"])
    except jwt.ExpiredSignatureError as err:
        raise UnauthorizedError("Token has expired") from err
    except jwt.InvalidTokenError as err:
        raise UnauthorizedError("Invalid token") from err


def decode_token_unverified(token: str) -> dict[str, Any]:
    """Decode a JWT token without verifying expiry (used for logout)."""
    try:
        return jwt.decode(
            token,
            settings.app_jwt_secret,
            algorithms=["HS256"],
            options={"verify_exp": False},
        )
    except jwt.InvalidTokenError as err:
        raise UnauthorizedError("Invalid token") from err


def get_jwks() -> dict[str, Any]:
    """Return JWKS endpoint response (symmetric key info for HS256)."""
    return {
        "keys": [
            {
                "kty": "oct",
                "alg": "HS256",
                "use": "sig",
                "kid": "demo-be-pyfa-key-1",
            }
        ]
    }
