"""Alembic environment configuration."""

import os
import sys
from logging.config import fileConfig
from pathlib import Path

from alembic import context
from sqlalchemy import engine_from_config, pool

# Add the src directory to sys.path so the models can be imported.
_src_path = Path(__file__).parents[2] / "src"
if str(_src_path) not in sys.path:
    sys.path.insert(0, str(_src_path))

# Import the metadata from the ORM models.
from demo_be_python_fastapi.infrastructure.models import Base  # noqa: E402

# Alembic Config object, providing access to values in alembic.ini.
config = context.config

# Interpret the alembic.ini logging config.
if config.config_file_name is not None:
    fileConfig(config.config_file_name)

# Set the target metadata for autogenerate support.
target_metadata = Base.metadata

# Read the database URL from the environment, falling back to the ini value.
_database_url = os.environ.get("DATABASE_URL") or config.get_main_option("sqlalchemy.url")
if _database_url:
    config.set_main_option("sqlalchemy.url", _database_url)


def run_migrations_offline() -> None:
    """Run migrations in offline mode (emit SQL without a live connection)."""
    url = config.get_main_option("sqlalchemy.url")
    context.configure(
        url=url,
        target_metadata=target_metadata,
        literal_binds=True,
        dialect_opts={"paramstyle": "named"},
    )
    with context.begin_transaction():
        context.run_migrations()


def run_migrations_online() -> None:
    """Run migrations in online mode (with a live database connection)."""
    connectable = engine_from_config(
        config.get_section(config.config_ini_section, {}),
        prefix="sqlalchemy.",
        poolclass=pool.NullPool,
    )
    with connectable.connect() as connection:
        context.configure(
            connection=connection,
            target_metadata=target_metadata,
        )
        with context.begin_transaction():
            context.run_migrations()


if context.is_offline_mode():
    run_migrations_offline()
else:
    run_migrations_online()
