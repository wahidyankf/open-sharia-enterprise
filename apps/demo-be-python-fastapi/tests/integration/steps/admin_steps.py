"""BDD step definitions for admin feature."""

from fastapi.testclient import TestClient
from pytest_bdd import given, parsers, scenarios, then, when

from tests.integration.conftest import GHERKIN_ROOT
from tests.integration.steps.security_steps import _ADMIN_PASSWORD, _register_and_promote_admin

scenarios(str(GHERKIN_ROOT / "admin" / "admin.feature"))

_PASSWORD = "Str0ng#Pass1"


@given(
    parsers.parse('an admin user "{username}" is registered and logged in'),
    target_fixture="admin_tokens",
)
def admin_login(client: TestClient, username: str) -> dict:
    user_data = _register_and_promote_admin(client, username, _ADMIN_PASSWORD)
    resp = client.post(
        "/api/v1/auth/login",
        json={"username": username, "password": _ADMIN_PASSWORD},
    )
    assert resp.status_code == 200
    return {**resp.json(), "admin_id": user_data["id"]}


@given(
    parsers.parse('users "{a}", "{b}", and "{c}" are registered'),
    target_fixture="registered_users",
)
def register_multiple_users(client: TestClient, a: str, b: str, c: str) -> list:
    users = []
    for username in [a, b, c]:
        resp = client.post(
            "/api/v1/auth/register",
            json={
                "username": username,
                "email": f"{username}@example.com",
                "password": _PASSWORD,
            },
        )
        assert resp.status_code == 201
        users.append(resp.json())
    return users


@given(
    parsers.parse('"{username}" has logged in and stored the access token'),
    target_fixture="alice_tokens",
)
def alice_login_for_admin(client: TestClient, username: str) -> dict:
    resp = client.post(
        "/api/v1/auth/login",
        json={"username": username, "password": _PASSWORD},
    )
    assert resp.status_code == 200
    return resp.json()


@given("alice's account has been disabled by the admin")
def disable_alice_by_admin(client: TestClient, registered_users: list, admin_tokens: dict) -> None:
    alice = next((u for u in registered_users if u["username"] == "alice"), None)
    assert alice is not None
    resp = client.post(
        f"/api/v1/admin/users/{alice['id']}/disable",
        json={"reason": "Test disable"},
        headers={"Authorization": f"Bearer {admin_tokens['access_token']}"},
    )
    assert resp.status_code == 200


@given("alice's account has been disabled")
def alice_account_disabled(client: TestClient, registered_users: list, admin_tokens: dict) -> None:
    alice = next((u for u in registered_users if u["username"] == "alice"), None)
    assert alice is not None
    resp = client.post(
        f"/api/v1/admin/users/{alice['id']}/disable",
        json={"reason": "Initial disable"},
        headers={"Authorization": f"Bearer {admin_tokens['access_token']}"},
    )
    assert resp.status_code == 200


# --- When steps ---


@when("the admin sends GET /api/v1/admin/users", target_fixture="response")
def admin_list_users(client: TestClient, admin_tokens: dict):  # type: ignore[no-untyped-def]
    return client.get(
        "/api/v1/admin/users",
        headers={"Authorization": f"Bearer {admin_tokens['access_token']}"},
    )


@when(
    "the admin sends GET /api/v1/admin/users?email=alice@example.com",
    target_fixture="response",
)
def admin_search_users_by_email(client: TestClient, admin_tokens: dict):  # type: ignore[no-untyped-def]
    return client.get(
        "/api/v1/admin/users",
        params={"email": "alice@example.com"},
        headers={"Authorization": f"Bearer {admin_tokens['access_token']}"},
    )


@when(
    parsers.parse("the admin sends POST /api/v1/admin/users/{{alice_id}}/disable with body {body}"),
    target_fixture="response",
)
def admin_disable_alice(client: TestClient, registered_users: list, admin_tokens: dict, body: str):  # type: ignore[no-untyped-def]
    import json

    alice = next((u for u in registered_users if u["username"] == "alice"), None)
    assert alice is not None
    data = json.loads(body)
    return client.post(
        f"/api/v1/admin/users/{alice['id']}/disable",
        json=data,
        headers={"Authorization": f"Bearer {admin_tokens['access_token']}"},
    )


@when(
    "the client sends GET /api/v1/users/me with alice's access token",
    target_fixture="response",
)
def get_me_alice_token(client: TestClient, alice_tokens: dict):  # type: ignore[no-untyped-def]
    return client.get(
        "/api/v1/users/me",
        headers={"Authorization": f"Bearer {alice_tokens['access_token']}"},
    )


@when(
    parsers.parse("the admin sends POST /api/v1/admin/users/{{alice_id}}/enable"),
    target_fixture="response",
)
def admin_enable_alice(client: TestClient, registered_users: list, admin_tokens: dict):  # type: ignore[no-untyped-def]
    alice = next((u for u in registered_users if u["username"] == "alice"), None)
    assert alice is not None
    return client.post(
        f"/api/v1/admin/users/{alice['id']}/enable",
        headers={"Authorization": f"Bearer {admin_tokens['access_token']}"},
    )


@when(
    parsers.parse("the admin sends POST /api/v1/admin/users/{{alice_id}}/force-password-reset"),
    target_fixture="response",
)
def admin_force_password_reset(client: TestClient, registered_users: list, admin_tokens: dict):  # type: ignore[no-untyped-def]
    alice = next((u for u in registered_users if u["username"] == "alice"), None)
    assert alice is not None
    return client.post(
        f"/api/v1/admin/users/{alice['id']}/force-password-reset",
        headers={"Authorization": f"Bearer {admin_tokens['access_token']}"},
    )


# --- Then steps ---


@then(
    'the response body should contain at least one user with "email" equal to "alice@example.com"'
)  # noqa: E501
def check_alice_in_results(response) -> None:  # type: ignore[no-untyped-def]
    body = response.json()
    users = body.get("data", [])
    assert any(u.get("email") == "alice@example.com" for u in users), (
        f"alice@example.com not found in users: {users}"
    )


@then('alice\'s account status should be "disabled"')
def check_alice_disabled(client: TestClient, registered_users: list, admin_tokens: dict) -> None:
    alice = next((u for u in registered_users if u["username"] == "alice"), None)
    assert alice is not None
    resp = client.get(
        "/api/v1/admin/users",
        params={"email": "alice@example.com"},
        headers={"Authorization": f"Bearer {admin_tokens['access_token']}"},
    )
    body = resp.json()
    users = body.get("data", [])
    alice_info = next((u for u in users if u["username"] == "alice"), None)
    assert alice_info is not None
    assert alice_info["status"].upper() == "DISABLED"


@then('alice\'s account status should be "active"')
def check_alice_active(client: TestClient, registered_users: list, admin_tokens: dict) -> None:
    alice = next((u for u in registered_users if u["username"] == "alice"), None)
    assert alice is not None
    resp = client.get(
        "/api/v1/admin/users",
        params={"email": "alice@example.com"},
        headers={"Authorization": f"Bearer {admin_tokens['access_token']}"},
    )
    body = resp.json()
    users = body.get("data", [])
    alice_info = next((u for u in users if u["username"] == "alice"), None)
    assert alice_info is not None
    assert alice_info["status"].upper() == "ACTIVE"
