"""Admin router for user management."""

from fastapi import APIRouter, Depends, Query
from pydantic import BaseModel
from sqlalchemy.orm import Session

from demo_be_python_fastapi.auth.dependencies import require_admin
from demo_be_python_fastapi.dependencies import get_db, get_user_repo
from demo_be_python_fastapi.domain.errors import NotFoundError
from demo_be_python_fastapi.infrastructure.models import UserModel

router = APIRouter()


class UserSummary(BaseModel):
    """User summary for admin list."""

    id: str
    username: str
    email: str
    status: str
    role: str


class UserListResponse(BaseModel):
    """Paginated user list response."""

    content: list[UserSummary]
    totalElements: int
    page: int
    size: int


class DisableRequest(BaseModel):
    """Disable user request."""

    reason: str | None = None


class PasswordResetResponse(BaseModel):
    """Password reset token response."""

    token: str


@router.get("/users", response_model=UserListResponse)
def list_users(
    page: int = Query(default=1, ge=0),
    size: int = Query(default=20, ge=1, le=100),
    search: str | None = Query(default=None),
    db: Session = Depends(get_db),
    _admin: UserModel = Depends(require_admin),
) -> UserListResponse:
    """List all users (admin only)."""
    user_repo = get_user_repo(db)
    page = max(1, page)
    users, total = user_repo.list_users(page, size, search)
    return UserListResponse(
        content=[
            UserSummary(
                id=u.id,
                username=u.username,
                email=u.email,
                status=u.status,
                role=u.role,
            )
            for u in users
        ],
        totalElements=total,
        page=page,
        size=size,
    )


@router.post("/users/{user_id}/disable")
def disable_user(
    user_id: str,
    body: DisableRequest | None = None,
    db: Session = Depends(get_db),
    _admin: UserModel = Depends(require_admin),
) -> dict:
    """Disable a user account (admin only)."""
    user_repo = get_user_repo(db)
    user = user_repo.update_status(user_id, "DISABLED")
    if user is None:
        raise NotFoundError(f"User {user_id} not found")
    # Revoke all tokens for the disabled user
    from demo_be_python_fastapi.dependencies import get_revoked_token_repo

    revoked_repo = get_revoked_token_repo(db)
    revoked_repo.revoke_all_for_user(user_id)
    return {"message": "User disabled"}


@router.post("/users/{user_id}/enable")
def enable_user(
    user_id: str,
    db: Session = Depends(get_db),
    _admin: UserModel = Depends(require_admin),
) -> dict:
    """Enable a user account (admin only)."""
    user_repo = get_user_repo(db)
    user = user_repo.update_status(user_id, "ACTIVE")
    if user is None:
        raise NotFoundError(f"User {user_id} not found")
    return {"message": "User enabled"}


@router.post("/users/{user_id}/unlock")
def unlock_user(
    user_id: str,
    db: Session = Depends(get_db),
    _admin: UserModel = Depends(require_admin),
) -> dict:
    """Unlock a locked user account (admin only)."""
    user_repo = get_user_repo(db)
    user = user_repo.unlock(user_id)
    if user is None:
        raise NotFoundError(f"User {user_id} not found")
    return {"message": "User unlocked"}


@router.post("/users/{user_id}/force-password-reset", response_model=PasswordResetResponse)
def force_password_reset(
    user_id: str,
    db: Session = Depends(get_db),
    _admin: UserModel = Depends(require_admin),
) -> PasswordResetResponse:
    """Generate a one-time password reset token (admin only)."""
    user_repo = get_user_repo(db)
    user = user_repo.find_by_id(user_id)
    if user is None:
        raise NotFoundError(f"User {user_id} not found")
    reset_token = user_repo.generate_password_reset_token(user_id)
    return PasswordResetResponse(token=reset_token)
