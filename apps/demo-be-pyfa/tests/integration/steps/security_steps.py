"""BDD step definitions for security feature."""

from fastapi.testclient import TestClient
from pytest_bdd import given, parsers, scenarios, then, when

from demo_be_pyfa.config import settings
from tests.integration.conftest import GHERKIN_ROOT

scenarios(str(GHERKIN_ROOT / "security" / "security.feature"))

_PASSWORD = "Str0ng#Pass1"
_ADMIN_PASSWORD = "Admin#Str0ng1"


def _promote_to_admin(client: TestClient, username: str) -> None:
    """Promote a user to ADMIN by using the internal SQLAlchemy DB directly.

    Since we don't have a separate admin-creation endpoint, we use a special
    registration endpoint pattern or directly mutate via the test DB.
    Instead, we register via a special fixture that creates an ADMIN user directly.
    """
    pass  # handled via _register_admin_user below


def _register_and_promote_admin(client: TestClient, username: str, password: str) -> dict:
    """Register a user and immediately set their role to ADMIN.

    This uses the app's internal DI to get the DB session and update the role.
    We achieve this by accessing the app's dependency override to get a session.
    """
    # Register as normal user first
    resp = client.post(
        "/api/v1/auth/register",
        json={"username": username, "email": f"{username}@example.com", "password": password},
    )
    assert resp.status_code == 201, f"Registration failed: {resp.text}"
    user_data = resp.json()

    # Directly update role using the app's session factory via the test client's app
    from sqlalchemy.orm import Session

    app = client.app  # type: ignore[attr-defined]
    override = app.dependency_overrides.get(  # type: ignore[union-attr]
        __import__("demo_be_pyfa.dependencies", fromlist=["get_db"]).get_db
    )
    if override:
        db_gen = override()
        db: Session = next(db_gen)
        try:
            from demo_be_pyfa.infrastructure.models import UserModel

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


@given(
    parsers.parse('a user "{username}" is registered and locked after too many failed logins'),
    target_fixture="locked_user",
)
def register_and_lock_user(client: TestClient, username: str) -> dict:
    # Register
    resp = client.post(
        "/api/v1/auth/register",
        json={
            "username": username,
            "email": f"{username}@example.com",
            "password": _PASSWORD,
        },
    )
    assert resp.status_code == 201
    user_data = resp.json()

    # Fail logins to lock the account
    for _ in range(settings.max_failed_login_attempts):
        client.post(
            "/api/v1/auth/login",
            json={"username": username, "password": "WrongPass#1234"},
        )
    return user_data


@given(
    parsers.parse('an admin user "{username}" is registered and logged in'),
    target_fixture="admin_tokens",
)
def register_admin_and_login(client: TestClient, username: str) -> dict:
    user_data = _register_and_promote_admin(client, username, _ADMIN_PASSWORD)
    resp = client.post(
        "/api/v1/auth/login",
        json={"username": username, "password": _ADMIN_PASSWORD},
    )
    assert resp.status_code == 200, f"Admin login failed: {resp.text}"
    return {**resp.json(), "id": user_data["id"]}


@given(
    parsers.parse('"alice" has had the maximum number of failed login attempts'),
)
def alice_max_failed_attempts(client: TestClient, registered_user: dict) -> None:
    for _ in range(settings.max_failed_login_attempts):
        client.post(
            "/api/v1/auth/login",
            json={"username": "alice", "password": "WrongPass#1234"},
        )


@given("an admin has unlocked alice's account")
def admin_unlocks_alice(client: TestClient, locked_user: dict) -> None:
    # Create a temporary admin inline for this step
    _register_and_promote_admin(client, "tmpadmin", _ADMIN_PASSWORD)
    login_resp = client.post(
        "/api/v1/auth/login",
        json={"username": "tmpadmin", "password": _ADMIN_PASSWORD},
    )
    assert login_resp.status_code == 200, f"Admin login failed: {login_resp.text}"
    tmp_tokens = login_resp.json()
    resp = client.post(
        f"/api/v1/admin/users/{locked_user['id']}/unlock",
        headers={"Authorization": f"Bearer {tmp_tokens['access_token']}"},
    )
    assert resp.status_code == 200, f"Unlock failed: {resp.text}"


# --- When steps ---


@when(
    parsers.parse("the admin sends POST /api/v1/admin/users/{{alice_id}}/unlock"),
    target_fixture="response",
)
def admin_unlock_alice(client: TestClient, locked_user: dict, admin_tokens: dict):  # type: ignore[no-untyped-def]
    return client.post(
        f"/api/v1/admin/users/{locked_user['id']}/unlock",
        headers={"Authorization": f"Bearer {admin_tokens['access_token']}"},
    )


# --- Then steps ---


@then('alice\'s account status should be "locked"')
def check_alice_locked(client: TestClient, registered_user: dict) -> None:
    # Verify the account is locked by attempting login — locked accounts return 401
    # The previous When step already confirmed the 401; attempt another login to verify lock
    resp = client.post(
        "/api/v1/auth/login",
        json={"username": "alice", "password": _PASSWORD},
    )
    assert resp.status_code == 401, (
        f"Expected 401 for locked account, got {resp.status_code}: {resp.text}"
    )
