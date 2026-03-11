"""FastAPI dependency providers."""

from collections.abc import Generator

from sqlalchemy.orm import Session

from demo_be_pyfa.database import SessionLocal
from demo_be_pyfa.infrastructure.repositories import (
    AttachmentRepository,
    ExpenseRepository,
    RevokedTokenRepository,
    UserRepository,
)


def get_db() -> Generator[Session]:
    """Provide a database session."""
    db = SessionLocal()
    try:
        yield db
    finally:
        db.close()


def get_user_repo(db: Session) -> UserRepository:
    """Provide a UserRepository instance."""
    return UserRepository(db)


def get_revoked_token_repo(db: Session) -> RevokedTokenRepository:
    """Provide a RevokedTokenRepository instance."""
    return RevokedTokenRepository(db)


def get_expense_repo(db: Session) -> ExpenseRepository:
    """Provide an ExpenseRepository instance."""
    return ExpenseRepository(db)


def get_attachment_repo(db: Session) -> AttachmentRepository:
    """Provide an AttachmentRepository instance."""
    return AttachmentRepository(db)
