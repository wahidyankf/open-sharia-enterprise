"""Tokens router: claims, JWKS."""

from fastapi import APIRouter, Depends
from fastapi.security import OAuth2PasswordBearer
from generated_contracts import TokenClaims

from demo_be_python_fastapi.auth.dependencies import get_current_user
from demo_be_python_fastapi.auth.jwt_service import decode_token
from demo_be_python_fastapi.infrastructure.models import UserModel

router = APIRouter()

_oauth2_scheme = OAuth2PasswordBearer(tokenUrl="/api/v1/auth/login")


@router.get("/claims", response_model=TokenClaims)
def get_claims(
    token: str = Depends(_oauth2_scheme),
    current_user: UserModel = Depends(get_current_user),
) -> TokenClaims:
    """Return the standard JWT claims from the current user's access token."""
    payload = decode_token(token)
    return TokenClaims(
        sub=str(payload.get("sub", "")),
        iss=str(payload.get("iss", "")),
        exp=int(payload.get("exp", 0)),
        iat=int(payload.get("iat", 0)),
        roles=[current_user.role],
    )
