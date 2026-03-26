"""create expenses table

Revision ID: 004
Revises: 003
Create Date: 2026-03-26 00:00:03.000000

"""

from collections.abc import Sequence

import sqlalchemy as sa
from alembic import op

# revision identifiers, used by Alembic.
revision: str = "004"
down_revision: str | None = "003"
branch_labels: str | Sequence[str] | None = None
depends_on: str | Sequence[str] | None = None


def upgrade() -> None:
    op.create_table(
        "expenses",
        sa.Column("id", sa.String(36), primary_key=True, nullable=False),
        sa.Column(
            "user_id",
            sa.String(36),
            sa.ForeignKey("users.id"),
            nullable=False,
        ),
        sa.Column("amount", sa.String(50), nullable=False),
        sa.Column("currency", sa.String(10), nullable=False),
        sa.Column("category", sa.String(100), nullable=False),
        sa.Column("description", sa.Text, nullable=True),
        sa.Column("date", sa.String(20), nullable=False),
        sa.Column("entry_type", sa.String(20), nullable=False, server_default="expense"),
        sa.Column("quantity", sa.String(50), nullable=True),
        sa.Column("unit", sa.String(50), nullable=True),
        sa.Column(
            "created_at",
            sa.DateTime(timezone=True),
            nullable=False,
            server_default=sa.text("now()"),
        ),
        sa.Column(
            "updated_at",
            sa.DateTime(timezone=True),
            nullable=False,
            server_default=sa.text("now()"),
        ),
    )
    op.create_index("ix_expenses_user_id", "expenses", ["user_id"], unique=False)


def downgrade() -> None:
    op.drop_index("ix_expenses_user_id", table_name="expenses")
    op.drop_table("expenses")
