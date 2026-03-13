"""Unit BDD step definitions for security feature."""

import pytest
from fastapi.testclient import TestClient
from pytest_bdd import given, parsers, scenarios, then, when

from demo_be_python_fastapi.config import settings
from tests.unit.conftest import GHERKIN_ROOT
from tests.unit.steps._helpers import _ADMIN_PASSWORD, register_and_promote_admin

pytestmark = pytest.mark.unit

scenarios(str(GHERKIN_ROOT / "security" / "security.feature"))

_PASSWORD = "Str0ng#Pass1"


@given(
    parsers.parse('a user "{username}" is registered and locked after too many failed logins'),
    target_fixture="locked_user",
)
def register_and_lock_user(client: TestClient, username: str) -> dict:
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
    user_data = register_and_promote_admin(client, username, _ADMIN_PASSWORD)
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
    register_and_promote_admin(client, "tmpadmin", _ADMIN_PASSWORD)
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
    resp = client.post(
        "/api/v1/auth/login",
        json={"username": "alice", "password": _PASSWORD},
    )
    assert resp.status_code == 401, (
        f"Expected 401 for locked account, got {resp.status_code}: {resp.text}"
    )
