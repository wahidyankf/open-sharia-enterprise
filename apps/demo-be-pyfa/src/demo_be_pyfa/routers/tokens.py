"""Tokens router: claims, JWKS."""

from typing import Any

from fastapi import APIRouter, Depends
from pydantic import BaseModel

from demo_be_pyfa.auth.dependencies import get_current_user
from demo_be_pyfa.infrastructure.models import UserModel

router = APIRouter()


class ClaimsResponse(BaseModel):
    """JWT claims response."""

    claims: dict[str, Any]


@router.get("/claims", response_model=ClaimsResponse)
def get_claims(
    current_user: UserModel = Depends(get_current_user),
) -> ClaimsResponse:
    """Return the claims from the current user's access token."""
    return ClaimsResponse(
        claims={
            "sub": current_user.id,
            "username": current_user.username,
            "role": current_user.role,
        }
    )
