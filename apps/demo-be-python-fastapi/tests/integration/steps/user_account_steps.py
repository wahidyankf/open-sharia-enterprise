"""BDD step definitions for user account feature."""

import json

from fastapi.testclient import TestClient
from pytest_bdd import given, parsers, scenarios, when

from tests.integration.conftest import GHERKIN_ROOT

scenarios(str(GHERKIN_ROOT / "user-lifecycle" / "user-account.feature"))

_PASSWORD = "Str0ng#Pass1"


@given(
    '"alice" has logged in and stored the access token',
    target_fixture="alice_tokens",
)
def alice_login(client: TestClient, registered_user: dict) -> dict:
    resp = client.post(
        "/api/v1/auth/login",
        json={"username": "alice", "password": _PASSWORD},
    )
    assert resp.status_code == 200, f"Login failed: {resp.text}"
    return resp.json()


@given("alice has deactivated her own account via POST /api/v1/users/me/deactivate")
def alice_self_deactivate(client: TestClient, alice_tokens: dict) -> None:
    resp = client.post(
        "/api/v1/users/me/deactivate",
        headers={"Authorization": f"Bearer {alice_tokens['access_token']}"},
    )
    assert resp.status_code == 200


# --- When steps ---


@when("alice sends GET /api/v1/users/me", target_fixture="response")
def alice_get_me(client: TestClient, alice_tokens: dict):  # type: ignore[no-untyped-def]
    return client.get(
        "/api/v1/users/me",
        headers={"Authorization": f"Bearer {alice_tokens['access_token']}"},
    )


@when(
    parsers.parse("alice sends PATCH /api/v1/users/me with body {body}"),
    target_fixture="response",
)
def alice_patch_me(client: TestClient, alice_tokens: dict, body: str):  # type: ignore[no-untyped-def]
    data = json.loads(body)
    return client.patch(
        "/api/v1/users/me",
        json=data,
        headers={"Authorization": f"Bearer {alice_tokens['access_token']}"},
    )


@when(
    parsers.parse("alice sends POST /api/v1/users/me/password with body {body}"),
    target_fixture="response",
)
def alice_change_password(client: TestClient, alice_tokens: dict, body: str):  # type: ignore[no-untyped-def]
    data = json.loads(body)
    return client.post(
        "/api/v1/users/me/password",
        json=data,
        headers={"Authorization": f"Bearer {alice_tokens['access_token']}"},
    )


@when("alice sends POST /api/v1/users/me/deactivate", target_fixture="response")
def alice_deactivate(client: TestClient, alice_tokens: dict):  # type: ignore[no-untyped-def]
    return client.post(
        "/api/v1/users/me/deactivate",
        headers={"Authorization": f"Bearer {alice_tokens['access_token']}"},
    )
