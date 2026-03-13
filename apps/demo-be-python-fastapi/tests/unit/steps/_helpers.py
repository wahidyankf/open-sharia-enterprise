"""Shared helper functions for unit BDD step definitions.

This module contains no pytest-bdd step definitions or scenario registrations.
It provides pure helper functions that multiple step files can import safely.
"""

from fastapi.testclient import TestClient

_ADMIN_PASSWORD = "Admin#Str0ng1"


def register_and_promote_admin(client: TestClient, username: str, password: str) -> dict:
    """Register a user and immediately set their role to ADMIN.

    Uses the app's dependency override to access the in-memory SQLite DB
    and update the user role directly without a separate admin endpoint.
    """
    resp = client.post(
        "/api/v1/auth/register",
        json={"username": username, "email": f"{username}@example.com", "password": password},
    )
    assert resp.status_code == 201, f"Registration failed: {resp.text}"
    user_data = resp.json()

    from sqlalchemy.orm import Session

    app = client.app  # type: ignore[attr-defined]
    override = app.dependency_overrides.get(  # type: ignore[union-attr]
        __import__("demo_be_python_fastapi.dependencies", fromlist=["get_db"]).get_db
    )
    if override:
        db_gen = override()
        db: Session = next(db_gen)
        try:
            from demo_be_python_fastapi.infrastructure.models import UserModel

            user = db.get(UserModel, user_data["id"])
            if user:
                user.role = "ADMIN"
                db.commit()
        finally:
            try:
                next(db_gen)
            except StopIteration:
                pass

    return user_data
