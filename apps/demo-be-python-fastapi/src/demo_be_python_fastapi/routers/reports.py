"""Reports router: P&L report."""

from fastapi import APIRouter, Depends, Query
from pydantic import BaseModel
from sqlalchemy.orm import Session

from demo_be_python_fastapi.auth.dependencies import get_current_user
from demo_be_python_fastapi.dependencies import get_db, get_expense_repo
from demo_be_python_fastapi.domain.expense import validate_currency
from demo_be_python_fastapi.infrastructure.models import UserModel

router = APIRouter()


class PLResponse(BaseModel):
    """Profit and loss report response."""

    income_total: str
    expense_total: str
    net: str
    income_breakdown: dict[str, str]
    expense_breakdown: dict[str, str]


@router.get("/pl", response_model=PLResponse)
def get_pl_report(
    from_: str = Query(alias="from"),
    to: str = Query(),
    currency: str = Query(),
    db: Session = Depends(get_db),
    current_user: UserModel = Depends(get_current_user),
) -> PLResponse:
    """Generate profit and loss report for a date range."""
    validated_currency = validate_currency(currency)
    expense_repo = get_expense_repo(db)
    report = expense_repo.pl_report(current_user.id, from_, to, validated_currency)
    return PLResponse(**report)
