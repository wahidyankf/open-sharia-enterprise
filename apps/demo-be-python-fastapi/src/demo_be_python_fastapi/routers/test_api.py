"""Test API router: database reset and admin promotion endpoints.

These endpoints are only available when ENABLE_TEST_API=true.
They exist to support FE E2E test setup and teardown.
"""

import os

from fastapi import APIRouter, Depends
from pydantic import BaseModel
from sqlalchemy.orm import Session

from demo_be_python_fastapi.dependencies import get_db
from demo_be_python_fastapi.domain.errors import NotFoundError
from demo_be_python_fastapi.infrastructure.models import (
    AttachmentModel,
    ExpenseModel,
    RevokedTokenModel,
    UserModel,
)

router = APIRouter()


class PromoteAdminRequest(BaseModel):
    """Request body for promote-admin endpoint."""

    username: str


def _is_test_api_enabled() -> bool:
    return os.getenv("ENABLE_TEST_API") == "true"


@router.post("/reset-db")
def reset_db(db: Session = Depends(get_db)) -> dict[str, str]:
    """Delete all data in dependency order: attachments → expenses → revoked_tokens → users."""
    db.query(AttachmentModel).delete()
    db.query(ExpenseModel).delete()
    db.query(RevokedTokenModel).delete()
    db.query(UserModel).delete()
    db.commit()
    return {"message": "Database reset successful"}


@router.post("/promote-admin")
def promote_admin(
    body: PromoteAdminRequest,
    db: Session = Depends(get_db),
) -> dict[str, str]:
    """Set the given user's role to ADMIN."""
    from sqlalchemy import select

    user = db.execute(
        select(UserModel).where(UserModel.username == body.username)
    ).scalar_one_or_none()
    if user is None:
        raise NotFoundError(f"User {body.username} not found")
    user.role = "ADMIN"
    db.commit()
    return {"message": f"User {body.username} promoted to ADMIN"}
