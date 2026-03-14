"""User account router."""

from fastapi import APIRouter, Depends
from pydantic import BaseModel
from sqlalchemy.orm import Session

from demo_be_python_fastapi.auth.dependencies import get_current_user
from demo_be_python_fastapi.dependencies import get_db, get_revoked_token_repo, get_user_repo
from demo_be_python_fastapi.domain.errors import UnauthorizedError
from demo_be_python_fastapi.infrastructure.models import UserModel
from demo_be_python_fastapi.infrastructure.password_hasher import hash_password, verify_password

router = APIRouter()


class UserProfileResponse(BaseModel):
    """User profile response model."""

    id: str
    username: str
    email: str
    displayName: str | None
    status: str


class UpdateProfileRequest(BaseModel):
    """Update profile request model."""

    displayName: str


class ChangePasswordRequest(BaseModel):
    """Change password request model."""

    oldPassword: str
    newPassword: str


@router.get("/me", response_model=UserProfileResponse)
def get_profile(
    current_user: UserModel = Depends(get_current_user),
) -> UserProfileResponse:
    """Get current user profile."""
    return UserProfileResponse(
        id=current_user.id,
        username=current_user.username,
        email=current_user.email,
        displayName=current_user.display_name,
        status=current_user.status,
    )


@router.patch("/me", response_model=UserProfileResponse)
def update_profile(
    body: UpdateProfileRequest,
    db: Session = Depends(get_db),
    current_user: UserModel = Depends(get_current_user),
) -> UserProfileResponse:
    """Update current user display name."""
    user_repo = get_user_repo(db)
    user = user_repo.update_display_name(current_user.id, body.displayName)
    if user is None:
        raise UnauthorizedError("User not found")
    return UserProfileResponse(
        id=user.id,
        username=user.username,
        email=user.email,
        displayName=user.display_name,
        status=user.status,
    )


@router.post("/me/password")
def change_password(
    body: ChangePasswordRequest,
    db: Session = Depends(get_db),
    current_user: UserModel = Depends(get_current_user),
) -> dict:
    """Change password for the current user."""
    if not verify_password(body.oldPassword, current_user.password_hash):
        raise UnauthorizedError("Invalid credentials")
    new_hash = hash_password(body.newPassword)
    user_repo = get_user_repo(db)
    user_repo.update_password(current_user.id, new_hash)
    return {"message": "Password changed"}


@router.post("/me/deactivate")
def deactivate(
    db: Session = Depends(get_db),
    current_user: UserModel = Depends(get_current_user),
) -> dict:
    """Self-deactivate the current user's account."""
    user_repo = get_user_repo(db)
    user_repo.update_status(current_user.id, "INACTIVE")
    revoked_repo = get_revoked_token_repo(db)
    revoked_repo.revoke_all_for_user(current_user.id)
    return {"message": "Account deactivated"}
