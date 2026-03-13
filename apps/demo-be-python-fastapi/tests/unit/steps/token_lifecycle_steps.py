"""Unit BDD step definitions for token lifecycle feature."""

import pytest
from fastapi.testclient import TestClient
from pytest_bdd import given, scenarios, then, when

from demo_be_python_fastapi.auth.jwt_service import create_expired_refresh_token
from tests.unit.conftest import GHERKIN_ROOT

pytestmark = pytest.mark.unit

scenarios(str(GHERKIN_ROOT / "authentication" / "token-lifecycle.feature"))

_PASSWORD = "Str0ng#Pass1"


@given(
    '"alice" has logged in and stored the access token and refresh token',
    target_fixture="alice_tokens",
)
def alice_login_tokens(client: TestClient, registered_user: dict) -> dict:
    resp = client.post(
        "/api/v1/auth/login",
        json={"username": "alice", "password": _PASSWORD},
    )
    assert resp.status_code == 200, f"Login failed: {resp.text}"
    return resp.json()


@given("alice's refresh token has expired", target_fixture="alice_tokens")
def alice_expired_refresh_token(client: TestClient, registered_user: dict) -> dict:
    resp = client.post(
        "/api/v1/auth/login",
        json={"username": "alice", "password": _PASSWORD},
    )
    assert resp.status_code == 200
    tokens = resp.json()
    tokens["refresh_token"] = create_expired_refresh_token(registered_user["id"])
    return tokens


@given(
    "alice has used her refresh token to get a new token pair",
    target_fixture="alice_tokens",
)
def alice_used_refresh_token(client: TestClient, registered_user: dict) -> dict:
    resp = client.post(
        "/api/v1/auth/login",
        json={"username": "alice", "password": _PASSWORD},
    )
    assert resp.status_code == 200
    original_tokens = resp.json()
    refresh_resp = client.post(
        "/api/v1/auth/refresh",
        json={"refresh_token": original_tokens["refresh_token"]},
    )
    assert refresh_resp.status_code == 200
    return {**original_tokens, "_original_refresh": original_tokens["refresh_token"]}


@given('the user "alice" has been deactivated', target_fixture="deactivated_alice")
def deactivate_alice(client: TestClient, registered_user: dict, alice_tokens: dict) -> dict:
    resp = client.post(
        "/api/v1/users/me/deactivate",
        headers={"Authorization": f"Bearer {alice_tokens['access_token']}"},
    )
    assert resp.status_code == 200
    return registered_user


@given("alice has already logged out once")
def alice_already_logged_out(client: TestClient, registered_user: dict, alice_tokens: dict) -> None:
    client.post(
        "/api/v1/auth/logout",
        headers={"Authorization": f"Bearer {alice_tokens['access_token']}"},
    )


# --- When steps ---


@when("alice sends POST /api/v1/auth/refresh with her refresh token", target_fixture="response")
def alice_refresh(client: TestClient, alice_tokens: dict):  # type: ignore[no-untyped-def]
    return client.post(
        "/api/v1/auth/refresh",
        json={"refresh_token": alice_tokens["refresh_token"]},
    )


_STEP_REFRESH_ORIGINAL = "alice sends POST /api/v1/auth/refresh with her original refresh token"


@when(_STEP_REFRESH_ORIGINAL, target_fixture="response")
def alice_refresh_with_original(client: TestClient, alice_tokens: dict):  # type: ignore[no-untyped-def]
    original = alice_tokens.get("_original_refresh", alice_tokens["refresh_token"])
    return client.post(
        "/api/v1/auth/refresh",
        json={"refresh_token": original},
    )


@when("alice sends POST /api/v1/auth/logout with her access token", target_fixture="response")
def alice_logout(client: TestClient, alice_tokens: dict):  # type: ignore[no-untyped-def]
    return client.post(
        "/api/v1/auth/logout",
        headers={"Authorization": f"Bearer {alice_tokens['access_token']}"},
    )


@when("alice sends POST /api/v1/auth/logout-all with her access token", target_fixture="response")
def alice_logout_all(client: TestClient, alice_tokens: dict):  # type: ignore[no-untyped-def]
    return client.post(
        "/api/v1/auth/logout-all",
        headers={"Authorization": f"Bearer {alice_tokens['access_token']}"},
    )


# --- Then steps ---


@then("alice's access token should be invalidated")
def check_access_token_invalidated(client: TestClient, alice_tokens: dict) -> None:
    resp = client.get(
        "/api/v1/users/me",
        headers={"Authorization": f"Bearer {alice_tokens['access_token']}"},
    )
    assert resp.status_code == 401, f"Expected 401 after logout, got {resp.status_code}"
