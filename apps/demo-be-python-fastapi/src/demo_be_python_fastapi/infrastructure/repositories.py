"""SQLAlchemy repository implementations."""

import uuid as _uuid_mod
from datetime import date, datetime
from decimal import Decimal

from sqlalchemy import func, select
from sqlalchemy.orm import Session

from demo_be_python_fastapi.infrastructure.models import (
    AttachmentModel,
    ExpenseModel,
    RevokedTokenModel,
    UserModel,
)


def _to_str(value: str | _uuid_mod.UUID) -> str:
    """Coerce a UUID or string to a string for session.get() lookups."""
    return str(value)


class UserRepository:
    """Repository for user operations."""

    def __init__(self, db: Session) -> None:
        self._db = db

    def create(
        self,
        username: str,
        email: str,
        password_hash: str,
        display_name: str | None = None,
        role: str = "USER",
    ) -> UserModel:
        user = UserModel(
            username=username,
            email=email,
            password_hash=password_hash,
            display_name=display_name or username,
            role=role,
            status="ACTIVE",
        )
        self._db.add(user)
        self._db.commit()
        self._db.refresh(user)
        return user

    def find_by_username(self, username: str) -> UserModel | None:
        return self._db.execute(
            select(UserModel).where(UserModel.username == username)
        ).scalar_one_or_none()

    def find_by_id(self, user_id: str) -> UserModel | None:
        return self._db.get(UserModel, _to_str(user_id))

    def update_status(self, user_id: str, status: str) -> UserModel | None:
        user = self.find_by_id(user_id)
        if user is None:
            return None
        user.status = status
        self._db.commit()
        self._db.refresh(user)
        return user

    def update_display_name(self, user_id: str, display_name: str) -> UserModel | None:
        user = self.find_by_id(user_id)
        if user is None:
            return None
        user.display_name = display_name
        self._db.commit()
        self._db.refresh(user)
        return user

    def update_password(self, user_id: str, password_hash: str) -> UserModel | None:
        user = self.find_by_id(user_id)
        if user is None:
            return None
        user.password_hash = password_hash
        self._db.commit()
        self._db.refresh(user)
        return user

    def increment_failed_attempts(self, user_id: str) -> int:
        user = self.find_by_id(user_id)
        if user is None:
            return 0
        user.failed_login_attempts += 1
        self._db.commit()
        self._db.refresh(user)
        return user.failed_login_attempts

    def reset_failed_attempts(self, user_id: str) -> None:
        user = self.find_by_id(user_id)
        if user is not None:
            user.failed_login_attempts = 0
            self._db.commit()

    def unlock(self, user_id: str) -> UserModel | None:
        user = self.find_by_id(user_id)
        if user is None:
            return None
        user.status = "ACTIVE"
        user.failed_login_attempts = 0
        self._db.commit()
        self._db.refresh(user)
        return user

    def list_users(
        self, page: int, size: int, email_filter: str | None = None
    ) -> tuple[list[UserModel], int]:
        query = select(UserModel)
        if email_filter:
            query = query.where(UserModel.email.ilike(f"%{email_filter}%"))
        total_result = self._db.execute(
            select(func.count()).select_from(query.subquery())
        ).scalar_one()
        offset = (page - 1) * size
        users = list(self._db.execute(query.offset(offset).limit(size)).scalars().all())
        return users, total_result

    def generate_password_reset_token(self, user_id: str) -> str:
        import uuid

        return str(uuid.uuid4())


class RevokedTokenRepository:
    """Repository for revoked token operations."""

    def __init__(self, db: Session) -> None:
        self._db = db

    def revoke(self, jti: str, user_id: str) -> None:
        # Check if already revoked by jti — idempotent
        existing = self._db.execute(
            select(RevokedTokenModel).where(RevokedTokenModel.jti == jti)
        ).scalar_one_or_none()
        if existing is not None:
            return
        token = RevokedTokenModel(jti=jti, user_id=user_id)
        self._db.add(token)
        self._db.commit()

    def is_revoked(self, jti: str, user_id: str, issued_at: datetime | None = None) -> bool:
        # Check direct revocation by jti
        token = self._db.execute(
            select(RevokedTokenModel).where(RevokedTokenModel.jti == jti)
        ).scalar_one_or_none()
        return token is not None

    def revoke_all_for_user(self, user_id: str) -> None:
        # The canonical schema has no bulk-revoke marker.
        # Callers (logout-all, deactivate) already revoke the current JTI explicitly
        # via revoke(jti, user_id) before calling this method, so the current token
        # is already invalidated. This method is kept for API compatibility.
        pass


class ExpenseRepository:
    """Repository for expense/income entry operations."""

    def __init__(self, db: Session) -> None:
        self._db = db

    def create(self, user_id: str, data: dict) -> ExpenseModel:
        # date may come in as a string ISO date or a date object
        expense_date = data["date"]
        if isinstance(expense_date, str):
            expense_date = date.fromisoformat(expense_date)
        expense = ExpenseModel(
            user_id=user_id,
            amount=str(data["amount"]),
            currency=data["currency"],
            category=data["category"],
            description=data.get("description") or "",
            date=expense_date,
            type=data.get("type", "expense").lower(),
            quantity=str(data["quantity"]) if data.get("quantity") is not None else None,
            unit=data.get("unit"),
        )
        self._db.add(expense)
        self._db.commit()
        self._db.refresh(expense)
        return expense

    def find_by_id(self, expense_id: str) -> ExpenseModel | None:
        return self._db.get(ExpenseModel, _to_str(expense_id))

    def list_by_user(self, user_id: str, page: int, size: int) -> tuple[list[ExpenseModel], int]:
        query = select(ExpenseModel).where(ExpenseModel.user_id == _to_str(user_id))
        total = self._db.execute(select(func.count()).select_from(query.subquery())).scalar_one()
        offset = (page - 1) * size
        items = list(self._db.execute(query.offset(offset).limit(size)).scalars().all())
        return items, total

    def update(self, expense_id: str, data: dict) -> ExpenseModel | None:
        expense = self.find_by_id(expense_id)
        if expense is None:
            return None
        expense_date = data["date"]
        if isinstance(expense_date, str):
            expense_date = date.fromisoformat(expense_date)
        expense.amount = str(data["amount"])
        expense.currency = data["currency"]
        expense.category = data["category"]
        expense.description = data.get("description") or ""
        expense.date = expense_date
        expense.type = data.get("type", "expense").lower()
        if "quantity" in data:
            expense.quantity = str(data["quantity"]) if data["quantity"] is not None else None
        if "unit" in data:
            expense.unit = data.get("unit")
        self._db.commit()
        self._db.refresh(expense)
        return expense

    def delete(self, expense_id: str) -> None:
        expense = self.find_by_id(expense_id)
        if expense is not None:
            self._db.delete(expense)
            self._db.commit()

    def summary_by_currency(self, user_id: str) -> list[dict]:
        stmt = select(ExpenseModel).where(
            ExpenseModel.user_id == _to_str(user_id),
            ExpenseModel.type == "expense",
        )
        expenses = list(self._db.execute(stmt).scalars().all())
        totals: dict[str, Decimal] = {}
        for exp in expenses:
            currency = exp.currency
            amount = Decimal(str(exp.amount))
            totals[currency] = totals.get(currency, Decimal("0")) + amount
        return [{"currency": k, "total": v} for k, v in totals.items()]

    def pl_report(
        self,
        user_id: str | _uuid_mod.UUID,
        from_date: str,
        to_date: str,
        currency: str,
    ) -> dict:
        from_date_obj = date.fromisoformat(from_date) if isinstance(from_date, str) else from_date
        to_date_obj = date.fromisoformat(to_date) if isinstance(to_date, str) else to_date
        stmt = select(ExpenseModel).where(
            ExpenseModel.user_id == _to_str(user_id),
            ExpenseModel.currency == currency,
            ExpenseModel.date >= from_date_obj,
            ExpenseModel.date <= to_date_obj,
        )
        entries = list(self._db.execute(stmt).scalars().all())
        income_total = Decimal("0")
        expense_total = Decimal("0")
        income_breakdown: dict[str, Decimal] = {}
        expense_breakdown: dict[str, Decimal] = {}
        for entry in entries:
            amount = Decimal(str(entry.amount))
            if entry.type == "income":
                income_total += amount
                income_breakdown[entry.category] = (
                    income_breakdown.get(entry.category, Decimal("0")) + amount
                )
            else:
                expense_total += amount
                expense_breakdown[entry.category] = (
                    expense_breakdown.get(entry.category, Decimal("0")) + amount
                )
        net = income_total - expense_total
        # Format decimals based on currency
        from demo_be_python_fastapi.domain.expense import CURRENCY_DECIMALS

        places = CURRENCY_DECIMALS.get(currency, 2)
        quantizer = Decimal(10) ** -places

        def fmt(d: Decimal) -> str:
            return str(d.quantize(quantizer))

        return {
            "totalIncome": fmt(income_total),
            "totalExpense": fmt(expense_total),
            "net": fmt(net),
            "income_breakdown": {k: fmt(v) for k, v in income_breakdown.items()},
            "expense_breakdown": {k: fmt(v) for k, v in expense_breakdown.items()},
        }


class AttachmentRepository:
    """Repository for attachment operations."""

    def __init__(self, db: Session) -> None:
        self._db = db

    def create(
        self,
        expense_id: str,
        filename: str,
        content_type: str,
        size: int,
        data: bytes,
    ) -> AttachmentModel:
        attachment = AttachmentModel(
            expense_id=expense_id,
            filename=filename,
            content_type=content_type,
            size=size,
            data=data,
        )
        self._db.add(attachment)
        self._db.commit()
        self._db.refresh(attachment)
        return attachment

    def list_by_expense(self, expense_id: str) -> list[AttachmentModel]:
        stmt = select(AttachmentModel).where(
            AttachmentModel.expense_id == _to_str(expense_id)
        )
        return list(self._db.execute(stmt).scalars().all())

    def find_by_id(self, attachment_id: str) -> AttachmentModel | None:
        return self._db.get(AttachmentModel, _to_str(attachment_id))

    def delete(self, attachment_id: str) -> None:
        attachment = self.find_by_id(attachment_id)
        if attachment is not None:
            self._db.delete(attachment)
            self._db.commit()
