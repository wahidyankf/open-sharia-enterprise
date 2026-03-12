"""Root test configuration and fixtures."""

from collections.abc import Generator

import pytest
from fastapi.testclient import TestClient
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker

from demo_be_python_fastapi.dependencies import get_db
from demo_be_python_fastapi.infrastructure.models import Base
from demo_be_python_fastapi.main import create_app


@pytest.fixture
def test_client() -> Generator[TestClient]:
    """Provide a FastAPI TestClient backed by SQLite in-memory database.

    Uses a shared-cache in-memory SQLite DB so all connections see the same tables.
    """
    # Use shared cache so all connections within same process see same DB
    engine = create_engine(
        "sqlite:///file:testdb?mode=memory&cache=shared&uri=true",
        connect_args={"check_same_thread": False},
    )
    Base.metadata.create_all(engine)
    testing_session_local = sessionmaker(autocommit=False, autoflush=False, bind=engine)

    def override_get_db():  # type: ignore[no-untyped-def]
        db = testing_session_local()
        try:
            yield db
        finally:
            db.close()

    application = create_app()
    application.dependency_overrides[get_db] = override_get_db
    with TestClient(application) as client:
        yield client
    Base.metadata.drop_all(engine)
    engine.dispose()
