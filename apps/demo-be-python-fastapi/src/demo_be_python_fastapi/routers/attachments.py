"""Attachments router: upload, list, delete."""

from datetime import UTC, datetime

from fastapi import APIRouter, Depends, File, UploadFile
from pydantic import BaseModel
from sqlalchemy.orm import Session

from demo_be_python_fastapi.auth.dependencies import get_current_user
from demo_be_python_fastapi.dependencies import get_attachment_repo, get_db, get_expense_repo
from demo_be_python_fastapi.domain.attachment import ALLOWED_CONTENT_TYPES, MAX_ATTACHMENT_SIZE
from demo_be_python_fastapi.domain.errors import (
    FileTooLargeError,
    ForbiddenError,
    NotFoundError,
    UnsupportedMediaTypeError,
)
from demo_be_python_fastapi.infrastructure.models import UserModel

router = APIRouter()


def _ensure_utc(dt: datetime) -> datetime:
    """Attach UTC timezone to a naive datetime (SQLite strips timezone info in tests)."""
    if dt.tzinfo is None:
        return dt.replace(tzinfo=UTC)
    return dt


# AttachmentResponse is kept local: returns metadata including a virtual download URL.
# Binary data is stored as BYTEA in the database; the url field provides a download path.
class AttachmentResponse(BaseModel):
    """Attachment metadata response."""

    id: str
    filename: str
    contentType: str
    size: int
    url: str


# AttachmentListResponse is kept local: not defined in the OpenAPI spec.
class AttachmentListResponse(BaseModel):
    """List of attachments."""

    attachments: list[AttachmentResponse]


def _check_expense_owner(expense_id: str, current_user: UserModel, db: Session) -> None:
    """Verify the expense belongs to the current user."""
    expense_repo = get_expense_repo(db)
    expense = expense_repo.find_by_id(expense_id)
    if expense is None:
        raise NotFoundError(f"Expense {expense_id} not found")
    if str(expense.user_id) != str(current_user.id):
        raise ForbiddenError("Access denied")


@router.post("/{expense_id}/attachments", status_code=201, response_model=AttachmentResponse)
async def upload_attachment(
    expense_id: str,
    file: UploadFile = File(...),
    db: Session = Depends(get_db),
    current_user: UserModel = Depends(get_current_user),
) -> AttachmentResponse:
    """Upload a file attachment to an expense entry."""
    _check_expense_owner(expense_id, current_user, db)

    content_type = file.content_type or ""
    if content_type not in ALLOWED_CONTENT_TYPES:
        raise UnsupportedMediaTypeError(f"Unsupported file type: {content_type}")

    contents = await file.read()
    if len(contents) > MAX_ATTACHMENT_SIZE:
        raise FileTooLargeError("File exceeds maximum size limit")

    filename = file.filename or "upload"

    attachment_repo = get_attachment_repo(db)
    attachment = attachment_repo.create(
        expense_id=expense_id,
        filename=filename,
        content_type=content_type,
        size=len(contents),
        data=contents,
    )
    attachment_id_str = str(attachment.id)
    return AttachmentResponse(
        id=attachment_id_str,
        filename=attachment.filename,
        contentType=attachment.content_type,
        size=attachment.size,
        url=f"/attachments/{attachment_id_str}/{attachment.filename}",
    )


@router.get("/{expense_id}/attachments", response_model=AttachmentListResponse)
def list_attachments(
    expense_id: str,
    db: Session = Depends(get_db),
    current_user: UserModel = Depends(get_current_user),
) -> AttachmentListResponse:
    """List attachments for an expense entry."""
    _check_expense_owner(expense_id, current_user, db)
    attachment_repo = get_attachment_repo(db)
    attachments = attachment_repo.list_by_expense(expense_id)
    return AttachmentListResponse(
        attachments=[
            AttachmentResponse(
                id=str(a.id),
                filename=a.filename,
                contentType=a.content_type,
                size=a.size,
                url=f"/attachments/{a.id}/{a.filename}",
            )
            for a in attachments
        ]
    )


@router.delete("/{expense_id}/attachments/{attachment_id}", status_code=204)
def delete_attachment(
    expense_id: str,
    attachment_id: str,
    db: Session = Depends(get_db),
    current_user: UserModel = Depends(get_current_user),
) -> None:
    """Delete an attachment from an expense entry."""
    _check_expense_owner(expense_id, current_user, db)
    attachment_repo = get_attachment_repo(db)
    attachment = attachment_repo.find_by_id(attachment_id)
    if attachment is None:
        raise NotFoundError(f"Attachment {attachment_id} not found")
    if str(attachment.expense_id) != expense_id:
        raise ForbiddenError("Access denied")
    attachment_repo.delete(attachment_id)
