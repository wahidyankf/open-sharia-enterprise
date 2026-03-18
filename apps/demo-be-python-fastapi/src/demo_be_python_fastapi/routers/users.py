"""User account router."""

from datetime import UTC, datetime

from fastapi import APIRouter, Depends
from sqlalchemy.orm import Session

from demo_be_python_fastapi.auth.dependencies import get_current_user
from demo_be_python_fastapi.dependencies import get_db, get_revoked_token_repo, get_user_repo
from demo_be_python_fastapi.domain.errors import UnauthorizedError
from demo_be_python_fastapi.infrastructure.models import UserModel
from demo_be_python_fastapi.infrastructure.password_hasher import hash_password, verify_password
from generated_contracts import ChangePasswordRequest, UpdateProfileRequest, User

router = APIRouter()


def _ensure_utc(dt: datetime) -> datetime:
    """Attach UTC timezone to a naive datetime (SQLite strips timezone info in tests)."""
    if dt.tzinfo is None:
        return dt.replace(tzinfo=UTC)
    return dt


def _user_to_contract(user: UserModel) -> User:
    """Map a UserModel ORM instance to the generated User contract type."""
    return User(
        id=user.id,
        username=user.username,
        email=user.email,
        displayName=user.display_name or "",
        status=user.status,
        roles=[user.role],
        createdAt=_ensure_utc(user.created_at),
        updatedAt=_ensure_utc(user.updated_at),
    )


@router.get("/me", response_model=User)
def get_profile(
    current_user: UserModel = Depends(get_current_user),
) -> User:
    """Get current user profile."""
    return _user_to_contract(current_user)


@router.patch("/me", response_model=User)
def update_profile(
    body: UpdateProfileRequest,
    db: Session = Depends(get_db),
    current_user: UserModel = Depends(get_current_user),
) -> User:
    """Update current user display name."""
    user_repo = get_user_repo(db)
    user = user_repo.update_display_name(current_user.id, body.displayName)
    if user is None:
        raise UnauthorizedError("User not found")
    return _user_to_contract(user)


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
