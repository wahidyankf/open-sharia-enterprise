"""create attachments table

Revision ID: 005
Revises: 004
Create Date: 2026-03-26 00:00:04.000000

"""

from collections.abc import Sequence

import sqlalchemy as sa
from alembic import op

# revision identifiers, used by Alembic.
revision: str = "005"
down_revision: str | None = "004"
branch_labels: str | Sequence[str] | None = None
depends_on: str | Sequence[str] | None = None


def upgrade() -> None:
    op.create_table(
        "attachments",
        sa.Column("id", sa.String(36), primary_key=True, nullable=False),
        sa.Column(
            "expense_id",
            sa.String(36),
            sa.ForeignKey("expenses.id"),
            nullable=False,
        ),
        sa.Column("filename", sa.String(255), nullable=False),
        sa.Column("content_type", sa.String(100), nullable=False),
        sa.Column("size", sa.Integer, nullable=False),
        sa.Column("url", sa.String(512), nullable=False),
        sa.Column(
            "created_at",
            sa.DateTime(timezone=True),
            nullable=False,
            server_default=sa.text("now()"),
        ),
    )
    op.create_index("ix_attachments_expense_id", "attachments", ["expense_id"], unique=False)


def downgrade() -> None:
    op.drop_index("ix_attachments_expense_id", table_name="attachments")
    op.drop_table("attachments")
