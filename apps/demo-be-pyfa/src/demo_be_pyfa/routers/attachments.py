"""Attachments router: upload, list, delete."""

import uuid

from fastapi import APIRouter, Depends, File, UploadFile
from pydantic import BaseModel
from sqlalchemy.orm import Session

from demo_be_pyfa.auth.dependencies import get_current_user
from demo_be_pyfa.dependencies import get_attachment_repo, get_db, get_expense_repo
from demo_be_pyfa.domain.attachment import ALLOWED_CONTENT_TYPES, MAX_ATTACHMENT_SIZE
from demo_be_pyfa.domain.errors import (
    FileTooLargeError,
    ForbiddenError,
    NotFoundError,
    UnsupportedMediaTypeError,
)
from demo_be_pyfa.infrastructure.models import UserModel

router = APIRouter()


class AttachmentResponse(BaseModel):
    """Attachment metadata response."""

    id: str
    filename: str
    content_type: str
    size: int
    url: str


class AttachmentListResponse(BaseModel):
    """List of attachments."""

    attachments: list[AttachmentResponse]


def _check_expense_owner(expense_id: str, current_user: UserModel, db: Session) -> None:
    """Verify the expense belongs to the current user."""
    expense_repo = get_expense_repo(db)
    expense = expense_repo.find_by_id(expense_id)
    if expense is None:
        raise NotFoundError(f"Expense {expense_id} not found")
    if expense.user_id != current_user.id:
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

    attachment_id = str(uuid.uuid4())
    filename = file.filename or "upload"
    url = f"/attachments/{attachment_id}/{filename}"

    attachment_repo = get_attachment_repo(db)
    attachment = attachment_repo.create(
        expense_id=expense_id,
        filename=filename,
        content_type=content_type,
        size=len(contents),
        url=url,
    )
    return AttachmentResponse(
        id=attachment.id,
        filename=attachment.filename,
        content_type=attachment.content_type,
        size=attachment.size,
        url=attachment.url,
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
                id=a.id,
                filename=a.filename,
                content_type=a.content_type,
                size=a.size,
                url=a.url,
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
    if attachment.expense_id != expense_id:
        raise ForbiddenError("Access denied")
    attachment_repo.delete(attachment_id)
